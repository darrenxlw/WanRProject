library(shiny)
library(plotly)


shinyUI(fluidPage(
  theme="main.css",

  # Application title
  titlePanel("Stock Forecasting using ARIMA"),

  sidebarLayout(
    sidebarPanel(
      textInput("ticker", label="Enter Stock Symbol"),
      actionButton("action","SEARCH"),
      span("Configurations", class="config")
    ),

    mainPanel(
      plotlyOutput("graph")
    )
  )
))
