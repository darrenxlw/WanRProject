library(plotly)
library(quantmod)
library(TTR)
library(tseries)
library(forecast)
library(glue)
library(dplyr)

library(shiny)

shinyServer(function(input, output) {
  
  plot_ticker <- function(t){
    #load timeseries of stock using quantmod
    eq_ts <- to.weekly(getSymbols(t,auto.assign = FALSE))
    eq_df <- data.frame(Date=index(eq_ts),coredata(eq_ts))
    #clean up column names
    tmpnames <- unlist(strsplit(colnames(eq_df),"\\."))
    colnames(eq_df) <- tmpnames[tmpnames!="eq_ts"]
    #Create Bollinger Bands from TTR Package
    bbands <- BBands(eq_df[,c("High", "Low", "Close")])
  }

  output$distPlot <- renderPlot({
    plot_ticker("PYPL")
    #plot_ticker(input$ticker)
  })

})
