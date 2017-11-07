library(shiny)
library(plotly)


shinyUI(fluidPage(
  theme="main.css",

  # Application title
  titlePanel("Stock Price Forecasting using ARIMA"),
  actionButton("modal", "?", class="toggle_info"),

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
        numericInput("months_trailing", h6("Trailing period (months)"), value=24),
        numericInput("fc_period", h6("Forecast period (weeks)"), value=26)
      )
    ),

    mainPanel(
      div(class="mainpanel",
        plotlyOutput("graph"),
        textOutput("stock_name"),
        uiOutput("weblink"),
        div(class="flexbox",
          tableOutput("summary"),
          textOutput("description")
        )
      ),
      tags$head(tags$script(src="main.js"))
    )
  )
))
