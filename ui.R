library(shiny)
library(plotly)


shinyUI(fluidPage(

  # Application title
  titlePanel("Stock Forecasting using ARIMA"),

  sidebarLayout(
    sidebarPanel(
      textInput("ticker", label="Enter Stock Symbol")
    ),

    mainPanel(
      plotlyOutput("graph")
    )
  )
))
