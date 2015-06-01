
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# server.R

library(quantmod)
library(ggplot2)
library(dplyr)
library(grid)
library(TTR)
library(scales)
library(lubridate)
library(shiny)
library(DT)
library(ggvis)

shinyServer(function(input, output,session){
        input_data <- reactive({
                x <- getSymbols.yahoo(paste(input$Ticker,".AX",sep = ""),
                                      env = .GlobalEnv,return.class = "data.frame",
                                      auto.assign=FALSE)
                #the below is done redundantly for ease of maintenance later on
                #First, strip OHLC data (need to vectorize)
                date <- as.Date(rownames(x))
                open <- as.vector(Op(x))
                high <- as.vector(Hi(x))
                low <- as.vector(Lo(x))
                close <- as.vector(Cl(x))
                #Then build the data frame
                xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
                xSubset})
        
#         addma <- reactive({
#                 x <- cutdata()
#                 x$MA <- SMA(x = data$SharePrice,n=input$smaval)
#                 x
#         })
        
                
        cutdata <- reactive({
                x <- input_data()
                startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
                enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))
                if(any(input$upperindicators == 1)){x$MA <- SMA(x = x$close,n=input$smaval)}
                cutdata <- x[(x$date >= startdate) & (x$date <= enddate),]              
        })
        
        
        
        cutdata%>%
                ggvis(x = ~date,y = ~close) %>%
                layer_lines(stroke := "darkorange", strokeWidth := 1)%>%
                if(!is.null(cutdata$MA)){layer_lines(x = ~date,y = ~ MA)}
                add_axis("x",title = "",title_offset = -10 ,properties = axis_props(labels = list(angle = -90, align = "right")))%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Share Price",expand = c(0.01,0.1),)%>%
                bind_shiny("ggvis","ggvis_ui")
}
)

 