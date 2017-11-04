library(plotly)
library(quantmod)
library(TTR)
library(tseries)
library(forecast)
library(glue)
library(dplyr)

library(shiny)

shinyServer(function(input, output) {
  
  clean_data <- function(t){
    #load timeseries of stock using quantmod
    eq_ts <- getSymbols(t,auto.assign = FALSE)
    eq_ts <- to.weekly(eq_ts)
    eq_df <- data.frame(Date=index(eq_ts),coredata(eq_ts))
    #clean up column names
    tmpnames <- unlist(strsplit(colnames(eq_df),"\\."))
    colnames(eq_df) <- tmpnames[tmpnames!="eq_ts"]
    #Create Bollinger Bands from TTR Package
    bbands <- BBands(eq_df[,c("High", "Low", "Close")])
    #Ignore Data older than 2 years
    cdate <- as.POSIXlt(Sys.Date())
    cdate$year <- cdate$year-2
    eq_df <- subset(cbind(eq_df, data.frame(bbands[,1:3])), Date >= as.Date(cdate))
    #Create univariate timeseries for arima modeling
    uv_ts <- eq_ts[,"eq_ts.Close"]
    #Use forecast package to create auto.arima and forecast development at different confidence levels
    eq_arima <- auto.arima(uv_ts)
    fc <- forecast(eq_arima, h=26, level=c(50,80,95))
    fc_df <- data.frame(fc)
    fc_df$Date <- c(Date=seq(Sys.Date(), by="weeks", length=27)[-1])
    eq_out <- merge(eq_df, fc_df,all=TRUE)
    return(eq_out)
  }
  
  plot_ticker <- function(df){
    #plot the price
    plt_price <- df %>% 
      plot_ly(x=~Date, type="candlestick",
              open=~Open, close=~Close, 
              high=~High, low=~Low, name=input$ticker,
              decreasing=list(color="#d66a60"), 
              increasing=list(color="#9bf2a2")) %>% 
      add_lines(y=~up, name="Bollinger Bands", line=list(width=0.25, color="#29C37B"), hoverinfo="none", showlegend=FALSE) %>% 
      add_lines(y=~dn, name="Bollinger Bands", line=list(width=0.25, color="#29C37B"), hoverinfo="none", showlegend=FALSE) %>% 
      add_lines(y=~mavg, name="Moving Avg", line=list(width=0.5, color="#29C37B"), hoverinfo="none") %>% 
      add_ribbons(ymin=~Lo.95, ymax=~Hi.95,   fillcolor="#d5e8e5", line=list(width=0.5, color="#b9c9c6")) %>% 
      add_ribbons(ymin=~Lo.80, ymax=~Hi.80,   fillcolor="#a0dbd0", line=list(width=0.5, color="#80b2a9")) %>% 
      add_ribbons(ymin=~Lo.50, ymax=~Hi.50,   fillcolor="#7dd1c2", line=list(width=0.5, color="#5fb7a7")) %>% 
      add_lines(y=~Point.Forecast, name="Forecast", line=list(width=1, color="FF5733")) %>% 
      layout(yaxis=list(title="price"))
    
    #plot volume
    plt_vol <- df %>% 
      plot_ly(x=~Date, y=~Volume, name="Volume", type="bar", marker=list(color="#10acb5")) %>% 
      layout(yaxis=list(title="vol", tickfont=list(size=8)))
    
    #subplot
    plt_ts <- subplot(plt_price, plt_vol, heights=c(0.8,0.2), nrows=2, shareX=TRUE, titleY=TRUE) %>% 
      layout(title=paste(input$ticker, " - ", Sys.Date()),
             legend=list(orientation="h",
                         xanchor="center", x = 0.5, y=1.5,
                         font = list(size = 5),
                         bgcolor = 'white'), showlegend=FALSE) %>% 
      config(displayModeBar = FALSE)
    return(plt_ts)
  }

  output$graph <- renderPlotly({
    plot_ticker(clean_data(input$ticker))
    #plot_ticker(input$ticker)
  })

})
