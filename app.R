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
          label = "Decimal places:",
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
        ),
        # dropdown for bootstrap method
        pickerInput(
          inputId = "bootstrap_method",
          label = "Bootstrap method:",
          choices = list(
            "Do not perform" = c("None"),
            "Select bootstrap method:" = c(
              "Percentile",
              "Bias corrected accelerated (BCa)"
            )
          )
        ),
        uiOutput("bootstrap")
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
  # Render base UI
  ##    Render bootstrap input
  observeEvent(input$bootstrap_method, {
    if (input$bootstrap_method != "None") {
      output$bootstrap <- renderUI({
        numericInput(
          inputId = "bootstrap_sample_size",
          label = "Bootstrap samples:",
          value = 1000,
          min = 20,
          max = 20000,
          step = 1
        )
      })
    } else {
      return(NULL)
    }
  })


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

  # Settings to be passed to modules
  settings <- reactiveValues(
    digits = 3,
    ci = 0.95,
    bootstrap_method_code = "none",
    bootstrap_sample_size = 0
  )

  observe({
    settings$digits <- input$digits
  })

  observe({
    settings$ci_level <- input$ci / 100
  })

  observe({
    settings$bootstrap_method_code <- if (input$bootstrap_method == "Percentile") {
      "perc"
    } else if (input$bootstrap_method == "Bias corrected accelerated (BCa)") {
      "bca"
    } else {
      "none"
    }
  })

  observe({
    settings$bootstrap_sample_size <- # if input doesn't exist, set to 0
      if (exists("input$bootstrap_sample_size")) {
        input$bootstrap_sample_size
      } else {
        0
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
        digits = settings$digits,
        ci_level = settings$ci,
        bootstrap_method = settings$bootstrap_method_code,
        bootstrap_sample_size = settings$bootstrap_sample_size
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
