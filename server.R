
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
                
                ###########Bollinger Bolds####
                boll <- BBands(HLC = x[,c(3,4,5)],n = input$bollval)
                x <- cbind(x,boll)
                ##############################
                
                ###########Elder Rays#########
                x$elder <- EMA(x = x$close,n = 13)
                x$Bull <- x$high - x$elder
                x$Bear <- x$low - x$elder
                ##############################
                
                ##################RSI#########
                x$RSI <- RSI(x$close)
                ##############################
                
                #######MACD###################
                x$EMA12 <- EMA(x = x$close,n = 12)
                x$EMA26 <- EMA(x = x$close,n = 26)
                x$MACD <- x$EMA12 - x$EMA26
                x$SIGNAL <- EMA(x = x$MACD,n = 9)
                ##############################
                
                #############MACD#############
                mfi <- MFI(HLC = x[,c("high","low","close")],x[,"volume"],n=14)
                x <- cbind(x,mfi)
                ##############################
                
#                 ##########Short Data##########
#                 shorthistory <- read.csv("http://asic.gov.au/Reports/YTD/2015/RR20150511-001-SSDailyYTD.csv",skip=1,fileEncoding = "UTF-16",sep = "\t")
#                 shorthistory <- shorthistory[-(1:2),]
#                 shorthistory <- cbind(Row.Names = rownames(shorthistory), shorthistory)
#                 rownames(shorthistory) <- NULL
#                 colnames(shorthistory) <- substr(colnames(shorthistory),2,11)
#                 colnames(shorthistory)[1] <- "Company"
#                 colnames(shorthistory)[2] <- "Ticker"
#                 shorthist1 <- shorthistory[,1:2]
#                 i=3 ##start at first volume column with short data
#                 while(i<=length(colnames(shorthistory))){
#                         if(i%%2 == 0){
#                                 shorthist1 <- cbind(shorthist1,shorthistory[i])
#                                 i <- i+1
#                         }
#                         else{
#                                 i <- i+1
#                         }
#                 }
#                 melted <- melt(data = shorthist1,id = c("Ticker","Company"))
#                 melted$variable <- as.POSIXlt(x = melted$variable,format = "%Y.%m.%d")
#                 melted$value[melted$value==""] <- 0.00
#                 melted$value <- as.numeric(melted$value)
#                 a <- melted[grep(pattern = input$Ticker,x = melted$Ticker),]
                
                cutdata <- x[(x$date >= startdate) & (x$date <= enddate),]
                
        })

        ##Create Share price plot
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
                layer_lines(y= ~mavg,stroke := "red",strokeWidth :=1.2 )%>%
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
                set_options(height = 250, width = 700,resizable = F)%>%
                bind_shiny("ggvis","ggvis_ui")


        ##Create Elder Rays Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ Bear, stroke = "Elder Bear")%>%
                layer_lines(y = ~ Bull,stroke = "Elder Bull")%>%
                layer_paths(y=0,stroke := "black")%>%
                #add_axis("x",title = "",,title_offset = -10 ,properties = axis_props(labels = list(angle = -90, align = "right")))%>%
                hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Differential",expand = c(0.01,0.1))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvis1","ggvis_ui1")
        
        #Create RSI Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ RSI, stroke = "RSI")%>%
                layer_lines(y = 70,stroke := "red")%>%
                layer_lines(y = 50,stroke := "black")%>%
                layer_lines(y = 30,stroke := "green")%>%
                add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%        
                #hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Indicator",expand = c(0,0),c(100,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvisrsi","ggvisrsi_ui")

        #Create MACD Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ MACD, stroke = "MACD")%>%
                layer_lines(y = ~SIGNAL,stroke = "Signal")%>%
                layer_lines(y = 0,stroke := "black")%>%
                hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Indicator",expand = c(0.01,0.1))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvismacd","ggvismacd_ui")

        #Create MFI Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ mfi, stroke = "MFI")%>%
                layer_lines(y = 80,stroke := "red")%>%
                layer_lines(y = 20,stroke := "green")%>%
                layer_lines(y = 50,stroke := "black")%>%
                hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "Indicator",expand = c(0,0),c(100,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvismfi","ggvismfi_ui")

        #Create Volume plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_rects(y = ~volume, y2 = 0 , width := 5)%>%
                hide_axis("x")%>%
                add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
                scale_datetime("x",expand = c(0,0))%>%
                scale_numeric("y",label = "Indicator",expand = c(0,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvisvol","ggvisvol_ui")

        output$tabs <- renderText({
                return(input$tab)})



}
)

 