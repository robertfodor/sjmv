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
        icon = icon(name = "bar-chart"),
        tabName = "descriptive"
      ),
      menuItem("Settings",
        icon = icon("cog"),
        startExpanded = FALSE,
        sliderTextInput(
          inputId = "digits",
          label = "Number of decimals:",
          choices = seq(2, 6, 1),
          grid = TRUE,
          selected = 3
        )
      ),
      verbatimTextOutput("debug")
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

  # Create a list of the variable classes
  column_classes <- reactive({
    if (length(file_input$df) > 0) {
      return(
        sapply(file_input$df, function(x) {
          class(x)
        })
      )
    }
  })

  # Non-factor variables
  non_factor_variables <- reactive({
    if (length(file_input$df) > 0) {
      return(
        names(file_input$df)[column_classes() != "factor"]
      )
    }
  })

  #  Output: Descriptive statistics
  observe(
    if (substr(input$tabs, 1, 5) == "descr") {
      callModule(
        module = descriptive_server,
        id = input$tabs,
        file_input = file_input,
        non_factor_variables = non_factor_variables()
      )
    }
  )

  # Debug
  output$debug <- renderPrint({
    if (length(file_input$df) > 0) {
      list(
        # This works for SPSS files:
        # as.numeric(levels(file_input$df[, "X1_Smoke"]))
        # For jamovi files
        # column_classes(),
        # paste("Column number 3 is a:", column_classes()[3]),
        # file_input$df[, "Country"],
        # sjlabelled::as_numeric(file_input$df[, "Country"])
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
