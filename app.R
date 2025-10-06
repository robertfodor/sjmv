# Load libraries
library(shiny)
library(shinydashboard)
library(sjlabelled)
library(jmvReadWrite)
library(DT)
library(shinyWidgets)


# Load dashboard tabs
source("modules/getting_started.R")
source("modules/descriptive.R")
source("modules/correlation.R")
source("modules/plots.R")
source("modules/regression.R")
source("modules/variance.R")
source("R/diagnostics.R")
source("R/variance_helpers.R")
source("R/themes.R")

# Define UI for application
ui <- dashboardPage(
  header = dashboardHeader(title = "SJMV"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Getting started", tabName = "getting_started", icon = icon("home")),
      menuItem(text = "Descriptive Statistics", icon = icon(name = "table"), tabName = "descriptive"),
      menuItem(text = "Plots", icon = icon(name = "chart-pie"), tabName = "plots"),
      menuItem(text = "Correlation", icon = icon(name = "th"), tabName = "correlation"),
      menuItem(text = "Regression Analysis", icon = icon(name = "line-chart"), tabName = "regression"),
      menuItem(text = "Variance Analysis", icon = icon(name = "bar-chart"), tabName = "variance"),
      menuItem(text = "About", icon = icon(name = "info-circle"), tabName = "about"),
      menuItem(
        text = "Settings", icon = icon(name = "cog"),
        sliderTextInput(inputId = "digits", label = "Decimal places for numbers:", choices = seq(2, 6, 1), grid = TRUE, selected = 3),
        sliderTextInput(inputId = "p_value_digits", label = "Decimal places for significance:", choices = seq(2, 6, 1), grid = TRUE, selected = 3)
      )
    )
  ),
  body = dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "customsmjv.css")),
    tabItems(
      tabItem(tabName = "getting_started", getting_started_ui("getting_started")),
      tabItem(tabName = "descriptive", descriptive_ui("descriptive")),
      tabItem(tabName = "plots", plots_ui("plots")),
      tabItem(tabName = "correlation", correlation_ui("correlation")),
      tabItem(tabName = "regression", regression_ui("regression")),
      tabItem(tabName = "variance", variance_ui("variance"))
    )
  ),
  skin = "black"
)

# Define server logic
server <- function(input, output, session) {
  file_input <- callModule(module = getting_started_server, id = "getting_started")
  data_to_pass <- reactive({
    req(file_input$df_filtered)
    list(df = file_input$df_filtered)
  })
  settings <- reactive({
    list(digits = as.numeric(input$digits), p_digits = as.numeric(input$p_value_digits))
  })

  non_factor_variables <- reactive({
    req(data_to_pass()$df)
    names(data_to_pass()$df)[!sapply(data_to_pass()$df, is.factor)]
  })

  # --- MODULE SERVER LOGIC ---

  callModule(
    module = descriptive_server, id = "descriptive", file_input = data_to_pass(),
    non_factor_variables = non_factor_variables, # FIX: Pass reactive expression, not its value
    settings = settings
  )

  callModule(
    module = plots_server, id = "plots", file_input = data_to_pass(),
    non_factor_variables = non_factor_variables
  ) # FIX: Pass reactive expression, not its value

  callModule(module = correlation_server, id = "correlation", file_input = data_to_pass(), settings = settings)
  callModule(module = regression_server, id = "regression", file_input = data_to_pass(), settings = settings)
  callModule(module = variance_server, id = "variance", file_input = data_to_pass(), settings = settings)
}

# Run the application
shinyApp(ui = ui, server = server)
