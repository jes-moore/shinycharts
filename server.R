###Add Share Price Comparer??

##change to select box for dates

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
library(reshape2)
library(xts)
library(googleVis)

shinyServer(function(input, output,session){
        input_data <- reactive({
                withProgress(message = 'Downloading ASX Stock Data', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.1)
                        }
                })
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
        
        cutdata <- reactive({
                x <- input_data()
                withProgress(message = 'Computing Indicators', value = 0, {
                        for (i in 1:15) {
                                incProgress(1/15)
                                Sys.sleep(0.1)
                        }
                })
                startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
                enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))
                x$MA <- SMA(x = x$close,n=input$smaval)
                x$MA <- round(x$MA,2)
                x$EMA <- EMA(x = x$close,n=input$emaval)
                x$EMA <- round(x$EMA,2)
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
                
                ############Chaikan and ADL###########
                x$MFM <- ((x$close-x$low)-(x$high-x$close))/(x$high-x$low)
                x$MFV <- x$MFM *x$volume
                x$MFV[is.nan(x$MFV)] <- 0
                x$ADL <- NA
                x$ADL[1] <- x$MFV[1]
                i <- 2
                while(i <= nrow(x)){
                        x$ADL[i] <- x$ADL[i-1] + x$MFV[i]
                        i <- i+1
                }
                x$chaiEMA3 <- EMA(x = x$ADL,n=3)
                x$chaiEMA10 <- EMA(x = x$ADL,n = 10)
                x$Chai <-(x$chaiEMA3-x$chaiEMA10)/1000
                x$chaiSMA <- SMA(x = x$Chai,10)
                x$ADL <- x$ADL/1000
                x$volume <- x$volume/1000
                #################################
                
                ######Aroon Trendline########
                aroontrend <- aroon(x[,c("high","low")],n=input$aroon)
                x <- cbind( x,aroontrend)
                #############################
                        
                ######Add SAR########
                sar <- SAR(x[,c("high","low")])
                x$sar <- x$close - sar
                x$sar1 <- sar
                        
                #############################
                
                cutdata <- x[(x$date >= startdate) & (x$date <= enddate),]
        
        
        })

        output$sharePrice1<- renderChart2({
                source('sharePricePlot.R')
                data <- cutdata()
                chart <- sharePricePlot(data)
                return(chart)
        })
        
        output$sharePrice2<- renderChart2({
                source('sharePricePlot2.R')
                data <- cutdata()
                chart <- sharePricePlot2(data)
                return(chart)
        })
        
        
        ##Create a thirdSP plot
        cutdata%>%
                ggvis(x = ~date,y = ~close) %>%
                ######Add Share ,MA, EMA#######
                layer_lines(stroke = "Share Price", strokeWidth := 2)%>%
                layer_points(y = ~sar1,fill := "darkred",size := 10)%>%
                ###############################
                ######Add Normal Y Axis#########
                add_axis("y",orient = "left",title = "",tick_padding = -40)%>%        
                scale_numeric("y",expand = c(0.01,0.1),)%>%
                ################################
                ######Add Normal X Axis#########
                add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%        
                set_options(height = 250, width = 700,resizable = F)%>%
                bind_shiny("ggvissp3","ggvissp3_ui")
        
        



        
        #ADL Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ ADL, stroke = "ADL/1000")%>%
                hide_axis("x")%>%
                add_axis("y",tick_padding = -40)%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "",expand = c(0,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvisadl","ggvisadl_ui")

        #Create Volume plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_rects(y = ~volume, y2 = 0, fill = "Volume/1000" , width := 5)%>%
                hide_axis("x")%>%
                add_axis("y",tick_padding = -40)%>%
                #add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
                scale_datetime("x",expand = c(0,0))%>%
                scale_numeric("y",label = "",expand = c(0,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvisvol","ggvisvol_ui")
        
        #SAR Plot
        cutdata%>%
                ggvis(x = ~date) %>%
                layer_lines(y = ~ sar, stroke = "SAR")%>%
                layer_lines(y = 0,stroke := "black")%>%
                add_axis("y",tick_padding = -40)%>%
                hide_axis("x")%>%
                scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
                scale_numeric("y",label = "",expand = c(0,0))%>%
                set_options(height = 125, width = 700,resizable = F)%>%
                bind_shiny("ggvissar","ggvissar_ui")
        
        

#############################Economic Calendar Data##########################################
calendar <-reactive({
        withProgress(message = 'Gathering Worldwide Economic Calendar', value = 0, {
                for (i in 1:15) {
                        incProgress(1/15)
                        Sys.sleep(0.25)
                }
        })
        calendar <- read.csv("http://www.myfxbook.com/calendar_statement.csv?start=2015-06-02%2000:00&end=2015-06-04%2000:00&filter=0-1-2-3_ANG-ARS-AUD-BRL-CAD-CHF-CLP-CNY-COP-CZK-DKK-EEK-EUR-GBP-HKD-HUF-IDR-INR-ISK-JPY-KPW-KRW-MXN-NOK-NZD-PEI-PLN-QAR-ROL-RUB-SEK-SGD-TRY-USD-ZAR&calPeriod=10")
        calendar$Date <- as.POSIXct(strptime(x = calendar$Date,format = "%Y, %B %d,%k",tz = "gmt"),tz = "gmt")
        calendar$Date <- format(calendar$Date, tz=Sys.timezone(),usetz=TRUE)     
        calendar
})
output$table <- renderDataTable(DT::datatable(calendar()))
#############################################################################################
       
# ##################################Currency Pain 1##############################################
# readcurrency_data1 <-reactive({
#         currency_data1 <- getSymbols(paste(input$base,"/",input$comp1,sep = ""),src="oanda",env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
#         currency_data1 <- data.frame(date = as.Date(rownames(currency_data1)),exchange = currency_data1)
#         rownames(currency_data1) <- NULL
#         colnames(currency_data1) <- c("date","exchange")
#         currency_data1
# })
# 
# currency_data1 <-reactive({
#         currency_data1 <- getSymbols(paste(input$base,"/",input$comp1,sep = ""),src="oanda",env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
#         currency_data1 <- data.frame(date = as.Date(rownames(currency_data1)),exchange = currency_data1)
#         rownames(currency_data1) <- NULL
#         colnames(currency_data1) <- c("date","exchange")
#         startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
#         enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))
#         currency_data1 <- currency_data1[(currency_data1$date >= startdate) & (currency_data1$date <= enddate),]
# })
# 
# output$ct1 <- renderText({
#         paste(input$base,"/",input$comp1,"")
# })
# 
#         ##plot currency data
#         currency_data1%>%
#         ggvis(x = ~date,y = ~exchange ) %>%
#         layer_lines(stroke := "red", strokeWidth := 2)%>%
#         ###############################
#         ######Add Normal Y Axis#########
#         add_axis("y",orient = "left",title = "")%>%        
#         scale_numeric("y",expand = c(0.01,0.1),)%>%
#         ################################
#         ######Add Normal X Axis#########
#         add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
#         scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%        
#         set_options(height = 250, width = 700,resizable = F)%>%
#         bind_shiny("ggviscomp1","ggviscomp1_ui")
#######################################################################################################        
 
# ##################################Currency Pain 2##############################################
# readcurrency_data2 <-reactive({
#         currency_data2 <- getSymbols(paste(input$base,"/",input$comp2,sep = ""),src="oanda",env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
#         currency_data2 <- data.frame(date = as.Date(rownames(currency_data2)),exchange = currency_data2)
#         rownames(currency_data2) <- NULL
#         colnames(currency_data2) <- c("date","exchange")
#         currency_data2
# })
# 
# currency_data2 <-reactive({
#         currency_data2 <- getSymbols(paste(input$base,"/",input$comp2,sep = ""),src="oanda",env = .GlobalEnv,return.class = "data.frame",auto.assign=FALSE)
#         currency_data2 <- data.frame(date = as.Date(rownames(currency_data2)),exchange = currency_data2)
#         rownames(currency_data2) <- NULL
#         colnames(currency_data2) <- c("date","exchange")
#         startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
#         enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))
#         currency_data2 <- currency_data2[(currency_data2$date >= startdate) & (currency_data2$date <= enddate),]
# })
# 
# output$ct2 <- renderText({
#         paste(input$base,"/",input$comp2,"")
# })
# 
# ##plot currency data
# currency_data2%>%
#         ggvis(x = ~date,y = ~exchange ) %>%
#         layer_lines(stroke := "red", strokeWidth := 2)%>%
#         ###############################
# ######Add Normal Y Axis#########
# add_axis("y",orient = "left",title = "")%>%        
#         scale_numeric("y",expand = c(0.01,0.1),)%>%
#         ################################
# ######Add Normal X Axis#########
# add_axis("x",title = "",orient = "top",title_offset = -10 ,properties = axis_props(labels = list(angle = -90,align = "left")))%>%
#         scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%        
#         set_options(height = 250, width = 700,resizable = F)%>%
#         bind_shiny("ggviscomp2","ggviscomp2_ui")
#######################################################################################################  

# currency_data3 <-reactive({
#         currency_data3 <- currency_data2()
#         currency_data3<- xts(x = currency_data3$exchange,order.by = currency_data3$date)
#         currency_data3
# })

#####################################Short History and Shorting Information#############################
short_general <- reactive({
        shorthistory <- read.csv("http://asic.gov.au/Reports/YTD/2015/RR20150720-001-SSDailyYTD.csv",skip=1,fileEncoding = "UTF-16",sep = "\t",row.names=NULL)
        shorthistory <- shorthistory[-(1:2),]
        shorthistory <- cbind(Row.Names = rownames(shorthistory), shorthistory)
        rownames(shorthistory) <- NULL
        colnames(shorthistory) <- substr(colnames(shorthistory),2,11)
        colnames(shorthistory)[2] <- "Company"
        colnames(shorthistory)[3] <- "Ticker"
        shorthist1 <- shorthistory[,2:3]
        shorthistory <- shorthistory[,-1]
        i=3 ##start at first volume column with short data
        while(i<=length(colnames(shorthistory))){
                if(i%%2 == 0){
                        shorthist1 <- cbind(shorthist1,shorthistory[i])
                        i <- i+1
                }
                else{
                        i <- i+1
                }
        }
        shorthist1
})

short_specific <- reactive ({
        startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
        enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))
        shorthist1 <- short_general()
        melted <- melt(data = shorthist1,id = c("Ticker","Company"))
        melted$variable <- as.POSIXlt(x = melted$variable,format = "%Y.%m.%d")
        melted$value[melted$value==""] <- 0.00
        melted$value <- as.numeric(melted$value)
        data <- melted
        data <- data[grep(pattern = input$Ticker,x = data$Ticker),]
        data$variable <- as.Date(data$variable)
        cutdata <- data[(data$variable >= startdate) & (data$variable <= enddate),]
        
})

#Short Plot
short_specific%>%
        ggvis(x = ~variable) %>%
        layer_lines(y = ~ value, stroke = "Short % of Stock Volume")%>%
        add_axis("y",tick_padding = -40)%>%
        hide_axis("x")%>%
        scale_datetime("x",round = TRUE,expand = c(0,0),label = NULL,clamp = TRUE)%>%
        scale_numeric("y",label = "",expand = c(0,0))%>%
        set_options(height = 125, width = 700,resizable = F)%>%
        add_tooltip(all_values,"hover")%>%
        #add_tooltip(all_values,"click")%>%
        bind_shiny("ggvisshort","ggvisshort_ui")

all_values <- function(x) {
        if(is.null(x)) return(NULL)
        paste0(names(x), ": ", format(x), collapse = "<br />")
}

short_table <-reactive({
        data <- short_general()
        data <- data[,1:7]
        data
})
output$shorttable <- renderDataTable({
        withProgress(message = 'Gathering Shorting Data', value = 0, {
                for (i in 1:10) {
                        incProgress(1/10)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(short_table())
        })


##############################Market Density Plots#####################
#############################################################################################


output$profile1 <- renderPlot({
        source("marketprofile.R")
        withProgress(message = 'Generating Closing Profile', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        marketprofile(input$Ticker)
})

output$profile2 <- renderPlot({
        source("marketprofile.R")
        withProgress(message = 'Generating Volume Profile', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        marketprofilevol(input$Ticker)
})

#############################################################################################
#############################Announcement Data##########################################
announce <-reactive({
        source('announcements_1.R')
        announce <- announcement_data(input$Ticker)
        announce
})
output$announce <- renderDataTable({
        withProgress(message = 'Gathering Announcements', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
                     })
        DT::datatable(announce())
        })
#############################################################################################
#############################################################################################



#############################################################################################
#############################Stock Ratio Data##########################################
ratios <-reactive({
        source('stockratios.R')
        ratios <- stock_ratios()
        ratios
})
output$ratios <- renderDataTable({
        withProgress(message = 'Calculating Ratios', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(ratios())
        })
#############################################################################################
#############################################################################################

#############################################################################################
#############################Stock Data##########################################
stats_ratio <-reactive({
        source('stockratios.R')
        stats <- stock_ratios()
        stats
})

asx_stats <-reactive({
        source('stockratios.R')
        stats <- tab_stats(input$Ticker)
        stats
})

output$stats <- renderDataTable({
        withProgress(message = 'Collating Stats', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(asx_stats())
})
#############################################################################################
#############################################################################################

#############################################################################################
#############################Sector Analysis##########################################

output$sector1 <- renderGvis({
        source('sectoranalysis.R')
        withProgress(message = 'Rendering Plot 1', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        c <- plotsector(sector = input$sector,data = stats_ratio(),ratio = input$sector_ratio1)
})

output$sector2 <- renderGvis({
        source('sectoranalysis.R')
        withProgress(message = 'Rendering Plot 2', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        c <- plotsector(sector = input$sector,data = stats_ratio(),ratio = input$sector_ratio2)
})
#############################################################################################
#############################################################################################

output$floats <- renderDataTable({
        source('upcoming_floats.R')
        withProgress(message = 'Getting Upcoming Floats', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(upcoming_floats())
})

output$recent_floats <- renderDataTable({
        source('upcoming_floats.R')
        withProgress(message = 'Getting Upcoming Floats', value = 0, {
                for (i in 1:5) {
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(recent_floats())
})

output$dividends <- renderDataTable({
        source('dividends.R')
        withProgress(message = 'Getting Dividend Data', value = 0, {
                for (i in 1:5) {##Open
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(dividends())
})

###############Section for evaluating indices#########################

source("compare_indices.R")
##Gather indices together And
##Create Chart JS and HTML with rCharts
# IndicesPlots <- compare_indices()



##Render Plots for output
output$hc1 <- renderChart2({
        charts <- compare_indices()
        hc1 <- charts[[1]]
        return(hc1)
})
# output$hc2 <- renderChart({
#         hc2 <- IndicesPlots[[2]]
#         return(hc2)
# })
# output$hc3 <- renderChart({
#         hc3 <- IndicesPlots[[3]]
#         return(hc3)
# })





##########################Get Options from the ASX#######################

output$options <- renderDataTable({
        source('derivatives.R')
        withProgress(message = 'Getting Options Data', value = 0, {
                for (i in 1:5) {##Open
                        incProgress(1/5)
                        Sys.sleep(time = 0.1)
                }
        })
        DT::datatable(readDerivatives())
})
#########################################################################

########################HotCopper Data###################################
source("readHotCopper.R")

##Render Plots for output
output$hot <- renderChart2({
        charts <- plotHotCopper(input$Ticker)
        return(charts)
})


output$warrants <- renderDataTable({
        source("downloadWarrants.R")
        warrants <- downloadWarrants()
        DT::datatable(warrants)
})



})







