
#ISPE 2025
#Introduction to R Shiny for Pharmacoepidemiologists: Interactive Visualization in Real-World Evidence Studies

# Install packages if they are not installed
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("survival")
# install.packages("survminer")

#Load the packages
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(survival)
library(survminer)

# Function to generate synthetic data
generate_data <- function() {
  set.seed(123)
  
  # Parameters
  n_patients  <- 1000
  patient_ids <- sprintf("P%04d", seq_len(n_patients))
  
  # Demographics (only the two upper age groups)
  demographics <- data.frame(
    person_id = patient_ids,
    sex       = sample(c("Male", "Female"), n_patients, replace = TRUE),
    age_group = sample(c("45-64", "65+"),     # <-- only these two
                       n_patients,
                       replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Base persistence rates and adjustments
  base_lambda <- c(A = 8, B = 4)
  sex_adj     <- c(Male = -1, Female = +1)
  age_adj     <- c("45-64" = +1,                # <-- only these two
                   "65+"   = +2)
  
  # Compute individual lambdas per patient and drug
  lambda_matrix <- with(demographics, {
    lam_A <- base_lambda["A"] + sex_adj[sex] + age_adj[age_group]
    lam_B <- base_lambda["B"] + sex_adj[sex] + age_adj[age_group]
    data.frame(lam_A, lam_B)
  })
  
  # Simulate number of dispensations
  n_disp_A <- rpois(n_patients, lambda_matrix$lam_A) + 1
  n_disp_B <- rpois(n_patients, lambda_matrix$lam_B) + 1
  
  # Build long dispensation table
  ids_A       <- rep(patient_ids, times = n_disp_A)
  ids_B       <- rep(patient_ids, times = n_disp_B)
  drugs_vec   <- c(rep("A", sum(n_disp_A)), rep("B", sum(n_disp_B)))
  total_recs  <- length(drugs_vec)
  
  # Sample dispensation dates and pack details
  dates_pool <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "day")
  disp_dates <- sample(dates_pool, size = total_recs, replace = TRUE)
  
  num_packs <- sample(1:3, total_recs, replace = TRUE)
  units_p   <- sample(c(168, 60, 100), total_recs, replace = TRUE)
  ddd_p     <- sample(c(42, 15, 50, 84), total_recs, replace = TRUE)
  
  data <- data.frame(
    person_id  = c(ids_A, ids_B),
    disp_date  = as.Date(c(
      disp_dates[seq_along(ids_A)],
      disp_dates[(length(ids_A) + 1):length(disp_dates)]
    )),
    drug       = drugs_vec,
    num_pack   = c(num_packs[seq_along(ids_A)],   num_packs[(length(ids_A) + 1):length(num_packs)]),
    units_pack = c(units_p[seq_along(ids_A)],     units_p[(length(ids_A) + 1):length(units_p)]),
    ddd_pack   = c(ddd_p[seq_along(ids_A)],       ddd_p[(length(ids_A) + 1):length(ddd_p)]),
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(demographics, by = "person_id") |>
    dplyr::arrange(person_id, drug, disp_date)
  
  data
}


ui <- shiny::fluidPage(
  
  titlePanel("Time-to-First Discontinuation Analysis"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
        shiny::numericInput(
          inputId = "gracePeriod",
          label   = "Grace period (days):",
          value   = 30,
          min     = 0
        )
    ),
    shiny::mainPanel(
      shiny::plotOutput("survPlot")
    )
  )
)

server <- function(input, output, session) {
  #Generate data once
  data <- generate_data()
  
  # Reactive preparation of treatment episode data
  prep_data <- reactive({
    
    #aggregate by dispensation
    agg <- data |>
      dplyr::arrange(person_id, drug, disp_date) |>
      dplyr::group_by(person_id, drug, disp_date) |>
      dplyr::summarize(
        total_ddd = base::sum(ddd_pack * num_pack),
        total_tab = base::sum(units_pack * num_pack),
        sex       = dplyr::first(sex),
        age_group = dplyr::first(age_group),
        .groups   = "drop"
      ) |>
      dplyr::mutate(
        days_supply = total_tab / 2,
        end_date    = disp_date + base::ceiling(days_supply) + lubridate::days(input$gracePeriod)
      )
    
    # build episodes
    episodes <- agg |>
      dplyr::group_by(person_id, drug) |>
      dplyr::arrange(disp_date) |>
      dplyr::mutate(
        overlap = disp_date <= dplyr::lag(end_date),
        epi_id  = base::cumsum(dplyr::coalesce(!overlap, TRUE))
      ) |>
      dplyr::group_by(person_id, drug, epi_id) |>
      dplyr::summarize(
        start_date = base::min(disp_date),
        end_date   = base::max(end_date),
        sex        = dplyr::first(sex),
        age_group  = dplyr::first(age_group),
        .groups    = "drop"
      )
    
    # select first episodes within the date range and compute time-to-event
    study_end <- as.Date("2024-12-31")
    
    episodes <- episodes |>
      dplyr::filter(start_date >= as.Date("2020-01-01")) |>
      dplyr::group_by(person_id) |>
      dplyr::arrange(start_date) |>
      dplyr::filter(start_date == dplyr::first(start_date)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        censor_date = base::pmin(end_date, study_end),
        disc        = dplyr::if_else(end_date < study_end, 1L, 0L),
        time_months = round(lubridate::time_length(lubridate::interval(start_date, censor_date), unit = "month"), 1)
      )
    
    return(episodes)
  })
  
  
  #Kaplan-Meier plot 
  output$survPlot <- renderPlot({
    df <- prep_data()
    
    #Surv formula + fit
    fit <- "Surv(time_months, disc) ~ drug" |>
      stats::as.formula() |>
      survminer::surv_fit(data = df)
    
    # generate survival plot 
    survminer::ggsurvplot(
      fit,
      data       = df,
      risk.table = TRUE,
      pval       = TRUE,
      conf.int   = TRUE,
      ggtheme    = ggplot2::theme_minimal(),
      xlab       = "Months since start",
      ylab       = "Persistence (%)"
    )
    
  })
  
}

shinyApp(ui, server)