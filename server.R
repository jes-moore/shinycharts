
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
                volume <- as.vector(Vo(x))
                #Then build the data frame
                xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close,"volume"=volume)
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
                x$MA <- SMA(x = x$close,n=input$smaval)
                x$EMA <- EMA(x = x$close,n=input$emaval)
                
                #######add Bollinger Bolds####
                boll <- BBands(HLC = x[,c(3,4,5)],n = input$bollval)
                x <- cbind(x,boll)
                ##############################
                
                #######add Elder Rays#########
                x$elder <- EMA(x = x$close,n = 13)
                x$Bull <- x$high - x$elder
                x$Bear <- x$low - x$elder
                ##############################
                
                cutdata <- x[(x$date >= startdate) & (x$date <= enddate),]
                
        })

        
        cutdata%>%
                ggvis(x = ~date,y = ~close) %>%
                ######Add Share ,MA, EMA#######
                layer_lines(x = ~date,y = ~ MA, stroke = "MA")%>%
                layer_lines(x = ~date,y = ~ EMA,stroke = "EMA")%>%
                layer_lines(stroke := "grey", strokeWidth := 2)%>%
                ###############################
                #######Add Bollinger Lines######
                layer_lines(y = ~ up, stroke := "red", strokeWidth := 1.5)%>%
                layer_lines(y = ~ dn, stroke := "red", strokeWidth := 1.5)%>%
                ################################
                ######Add Normal Y Axis#########
                add_axis("y",orient = "left",title = "Share Price")%>%        
                scale_numeric("y",expand = c(0.01,0.1),)%>%
                ################################
                ######Add Normal X Axis#########
                add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%        
                ################################
                ######Add Secondary y Axis#########
                #add_axis("y",'y2', orient = "right", title= "Volume",grid=F) %>% 
                #scale_numeric("y","y2", domain = c(0, -1), nice = FALSE) %>%
                #layer_rects(~volume,prop('y',scale='y2'))%>%
                set_options(height = 300, width = 700,resizable = F)%>%
                bind_shiny("ggvis","ggvis_ui")



        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ Bear, stroke = "Elder Bear")%>%
                layer_lines(y = ~ Bull,stroke = "Elder Bull")%>%
                layer_paths(y=0,stroke := "black")%>%
                #add_axis("x",title = "",,title_offset = -10 ,properties = axis_props(labels = list(angle = -90, align = "right")))%>%
                hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Differential",expand = c(0.01,0.1))%>%
                set_options(height = 150, width = 700,,resizable = F)%>%
                bind_shiny("ggvis1","ggvis_ui1")
        

}
)

 