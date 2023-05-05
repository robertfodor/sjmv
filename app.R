# Load libraries
library(shiny)
library(shinydashboard)
library(sjlabelled)
library(jmvReadWrite)
library(DT)
library(shinyWidgets)


# Load dashboard tabs
source("modules/getting_started.R") # Welcome and data import
source("modules/descriptive.R") # Descriptive statistics
source("modules/regression.R") # Regression analysis
source("modules/variance.R") # Variance analysis

# Define UI for application
ui <- dashboardPage(
  header = dashboardHeader(
    title = "ShinyStat"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      # This allows to show which tabName is selected
      id = "tabs",
      menuItem(
        text = "Getting started",
        tabName = "getting_started",
        icon = icon(name = "home")
      ),
      menuItem(
        text = "Descriptive Statistics",
        icon = icon(name = "table"),
        tabName = "descriptive"
      ),
      menuItem(
        text = "Regression Analysis",
        icon = icon(name = "line-chart"),
        tabName = "regression"
      ),
      menuItem(
        text = "Variance Analysis",
        icon = icon(name = "bar-chart"),
        tabName = "variance"
      ),
      menuItem(
        text = "About",
        icon = icon(name = "info-circle"),
        tabName = "about"
      ),
      menuItem(
        text = "Settings",
        icon = icon(name = "cog"),
        sliderTextInput(
          inputId = "digits",
          label = "Decimal places for numbers:",
          choices = seq(2, 6, 1),
          grid = TRUE,
          selected = 3
        ),
        sliderTextInput(
          inputId = "p_value_digits",
          label = "Decimal places for significance:",
          choices = seq(2, 6, 1),
          grid = TRUE,
          selected = 3
        ),
        numericInput(
          inputId = "ci",
          label = "Confidence interval (%):",
          value = 95,
          min = 0,
          max = 100,
          step = 1
        )
      )
    )
  ),
  body = dashboardBody(
    # Custom CSS
    tags$head(tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "shinystat.css"
    )),
    tabItems(
      tabItem(
        tabName = "getting_started",
        getting_started_ui("getting_started")
      ),
      tabItem(
        tabName = "descriptive",
        descriptive_ui("descriptive")
      ),
      tabItem(
        tabName = "regression",
        regression_ui("regression")
      ),
      tabItem(
        tabName = "variance",
        variance_ui("variance")
      )
    )
  ),
  skin = "black"
)

# Define server logic
server <- function(input, output, session) {
  # Call modules
  #  Input: Getting Started
  file_input <- callModule(
    module = getting_started_server,
    id = "getting_started"
  )

  # Non-factor variables
  non_factor_variables <- reactive({
    if (length(file_input$df) > 0) {
      return(
        names(file_input$df)[sapply(
          file_input$df,
          function(x) !inherits(x, "factor")
        )]
      )
    }
  })

  ##  Output: Descriptive statistics
  observe(
    if (substr(input$tabs, 1, 5) == "descr") {
      callModule(
        module = descriptive_server,
        id = input$tabs,
        file_input = file_input,
        non_factor_variables = non_factor_variables(),
        digits = input$digits,
        p_value_digits = input$p_value_digits,
        ci_level = (input$ci / 100)
      )
    }
  )

  ##  Output: Regression analysis
  observe(
    if (substr(input$tabs, 1, 5) == "regre") {
      callModule(
        module = regression_server,
        id = input$tabs,
        file_input = file_input,
        digits = input$digits
      )
    }
  )

  ##  Output: Variance analysis
  observe(
    if (substr(input$tabs, 1, 5) == "varia") {
      callModule(
        module = variance_server,
        id = input$tabs,
        file_input = file_input,
        digits = input$digits
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
