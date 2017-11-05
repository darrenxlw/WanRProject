library(shiny)
library(plotly)


shinyUI(fluidPage(
  theme="main.css",

  # Application title
  titlePanel("Stock Forecasting using ARIMA"),

  sidebarLayout(
    sidebarPanel(
      textInput("ticker", label="Enter a Stock Symbol"),
      actionButton("action","SEARCH"),
      span("Configurations", class="config", tags$span("+", class="button")),
      tags$div(class="config_toggle",
        radioButtons("radio", h6("Data frequency"),
                     choices = list("Daily" = 1, "Weekly" = 2, 
                                    "Monthly" = 3),selected = 2),
        h6("Confidence Levels"),
        sliderInput("fc_confidence_1", label = "",
                    min = 0, max = 99, value = 95),
        sliderInput("fc_confidence_2", label = "",
                    min = 0, max = 99, value = 0),
        numericInput("months_trailing", h6("Data trailing n months"), value=24)
      )
    ),

    mainPanel(
      plotlyOutput("graph"),
      tags$head(tags$script(src="main.js"))
    )
  )
))
