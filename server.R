library(plotly)
library(quantmod)
library(TTR)
library(tseries)
library(forecast)
library(glue)
library(dplyr)
library(rvest)

library(shiny)

shinyServer(function(input, output) {
  
  stock_ticker <- eventReactive(input$action, {
    return(input$ticker)
  })
  webdata <- reactive({
    read_html(glue("http://www.nasdaq.com/symbol/{tolower(stock_ticker())}"))
  })
  validate_input <- function(t){
    tmp <- tryCatch(
      getSymbols(t),
      error=function(e){return("error")},
      warning=function(e){return("warning")}
    )
    validate(
      need(input$ticker != "" && length(input$ticker) <= 4 && tmp != "warning" && tmp != "error", "Please enter a valid stock symbol"),
      {if(input$ticker!="") need(length(input$ticker) <= 4 && tmp != "warning", paste(toupper(input$ticker), "is not a valid symbol."))}
    ) 
  }
  
  observeEvent(input$modal, {
    showModal(modalDialog(
      title = "ARIMA Forecasting",
      p("This Shiny App uses the ARIMA Model to forecast stock prices. The Autoregressive Integrated Moving Average (ARIMA) model combines Auto Regression, Moving Average, and Differencing to fit the time series as well as possible."),
      p("ARIMA models are modified by 3 parameters: the order of the Auto Regression, the degree of differencing and the order of the Moving Average. Instead of determining the parameters manually, auto.arima of the forecast package returns the optimal ARIMA Model using AIC/BIC."),
      p("Using the forecasting function we can determine future prices at different confidence levels with regard to our fitted model.")
    ))
  })
  
  scrape_info <- function(t){
    tmp <- webdata() %>% 
      html_nodes("#quotes_content_left_InfoQuotesResults .genTable table")
    df <- html_table(tmp, fill=TRUE)[[1]]
    out <- data.frame(c("1Y Target","50D avg vol","Market cap","P/E ratio","EPS","Beta"), c(df$X2[2],df$X2[5],df$X2[8],df$X2[9],df$X2[11],df$X2[16]))
    colnames(out)=c("name", "value")
    return(out)
  }
  
  scrape_name <- function(t){
    tmp <- webdata() %>% 
      html_nodes("#qwidget_pageheader h1")
    return(html_text(tmp))
  }
  scrape_text <- function(t){
    tmp <- webdata() %>% 
      html_nodes("#left-column-div div p")
    return(html_text(tmp)[2])
  }
  
  clean_data <- function(t){
    #load timeseries of stock using quantmod
    eq_ts <- getSymbols(t,auto.assign = FALSE)
    if(input$radio==2){
      eq_ts <- to.weekly(eq_ts)
    }else if(input$radio==3){
      eq_ts <- to.monthly(eq_ts)
    }else{
      eq_ts <- to.daily(eq_ts)
    }
    
    eq_df <- data.frame(Date=as.Date(index(eq_ts)),coredata(eq_ts))
    #clean up column names
    tmpnames <- unlist(strsplit(colnames(eq_df),"\\."))
    colnames(eq_df) <- tmpnames[tmpnames!="eq_ts"]
    #Create Bollinger Bands from TTR Package
    bbands <- BBands(eq_df[,c("High", "Low", "Close")])
    #Ignore Data older than 2 years
    cdate <- as.POSIXlt(Sys.Date())
    cdate$mon <- cdate$mon-input$months_trailing
    eq_df <- subset(cbind(eq_df, data.frame(bbands[,1:3])), Date >= as.Date(cdate))
    #Create univariate timeseries for arima modeling
    uv_ts <- eq_ts[,"eq_ts.Close"]
    #Use forecast package to create auto.arima and forecast development at different confidence levels
    eq_arima <- auto.arima(uv_ts)
    fc <- forecast(eq_arima, h=input$fc_period, level=c(input$fc_confidence_1,input$fc_confidence_2))
    fc_df <- data.frame(fc)
    fc_df$Date <- c(Date=seq(Sys.Date(), by="weeks", length=input$fc_period+1)[-1])
    eq_out <- merge(eq_df, fc_df,all=TRUE)
    return(eq_out)
  }
  
  plot_ticker <- function(df,t){
    #plot the price
    plt_price <- df %>% 
      plot_ly(x=~Date, type="candlestick",
              open=~Open, close=~Close, 
              high=~High, low=~Low, name=t,
              decreasing=list(color="#d66a60"), 
              increasing=list(color="#9bf2a2")) %>% 
      add_lines(y=~up, name="Bollinger Bands", line=list(width=0.25, color="#29C37B"), hoverinfo="none", showlegend=FALSE) %>% 
      add_lines(y=~dn, name="Bollinger Bands", line=list(width=0.25, color="#29C37B"), hoverinfo="none", showlegend=FALSE) %>% 
      add_lines(y=~mavg, name="Moving Avg", line=list(width=0.5, color="#29C37B"), hoverinfo="none") %>% 
      add_ribbons(ymin=eval(parse(text=paste("~Lo.", toString(max(input$fc_confidence_1,input$fc_confidence_2)), sep=""))),
                  ymax=eval(parse(text=paste("~Hi.", toString(max(input$fc_confidence_1,input$fc_confidence_2)), sep=""))),
                  fillcolor="#d5e8e5", line=list(width=0.5, color="#b9c9c6")) %>% 
      add_ribbons(ymin=eval(parse(text=paste("~Lo.", toString(min(input$fc_confidence_1,input$fc_confidence_2)), sep=""))), 
                  ymax=eval(parse(text=paste("~Hi.", toString(min(input$fc_confidence_1,input$fc_confidence_2)), sep=""))),
                  fillcolor="#a0dbd0", line=list(width=0.5, color="#5fb7a7")) %>% 
      add_lines(y=~Point.Forecast, name="Forecast", line=list(width=1, color="FF5733")) %>% 
      layout(yaxis=list(title="price usd"))
    
    #plot volume
    plt_vol <- df %>% 
      plot_ly(x=~Date, y=~Volume, name="Volume", type="bar", marker=list(color="#10acb5")) %>% 
      layout(yaxis=list(title="vol", tickfont=list(size=8)))
    
    #subplot
    plt_ts <- subplot(plt_price, plt_vol, heights=c(0.8,0.2), nrows=2, shareX=TRUE, titleY=TRUE) %>% 
      layout(title=paste(t, " - ", Sys.Date()), xaxis=list(title=""),
             legend=list(orientation="h",
                         xanchor="center", x = 0.5, y=1.5,
                         font = list(size = 5),
                         bgcolor = 'white'), showlegend=FALSE) %>% 
      config(displayModeBar = FALSE)
    return(plt_ts)
  }

  output$graph <- renderPlotly({
    validate_input(stock_ticker())
    plot_ticker(clean_data(toupper(stock_ticker())), toupper(stock_ticker()))
  })
  output$stock_name <- renderText({
    scrape_name(stock_ticker())
  })
  output$summary <- renderTable({
    scrape_info(stock_ticker())
  }, include.colnames=FALSE)
  output$description <- renderText({
    scrape_text(stock_ticker())
  })
})
