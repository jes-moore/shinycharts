# startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
# enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))

compare_stocks <- function(Stock1 = "KAR", Stock2 = "AWE", Stock3 = "ZIP", start_date = Sys.Date()-months(6),end_date = Sys.Date()){
        library(lubridate)
        library(rCharts)
        ###Gather stock data for various stocks
        stock1_data <- get_data(Stock1,start_date,end_date)    
        stock2_data <- get_data(Stock2,start_date,end_date)  
        stock3_data <- get_data(Stock3,start_date,end_date)  
        data <- merge(stock1_data,stock2_data,"date")
        data <- merge(data,stock3_data,"date")
        data <- transform(data, date = as.character(date))
        data <- melt(data = data,id.vars = "date")
        closeData <- data[grep(pattern = "close",x = data$variable,ignore.case = TRUE),]
        volumeData <- data[grep(pattern = "volume",x = data$variable,ignore.case = TRUE),] 
        m1 <- mPlot(x = "date", y = "value",group = 'variable', type = "Line", data = closeData)
        m1$set(pointSize = 0, lineWidth = 2)
        m1$set(hideHover = "auto")
        m1
         
}
        

get_data <- function(stock,start_date,end_date){
        library(quantmod)
        library(dplyr)
        library(TTR)
        library(scales)
        library(lubridate)
        library(reshape2)
        library(xts)
        library(rCharts)
        x <- getSymbols.yahoo(paste(stock,".AX",sep = ""),from = start_date,to = end_date,
                              env = .GlobalEnv,return.class = "data.frame",
                              auto.assign=FALSE)
        date <- as.Date(rownames(x))
        close <- as.vector(Cl(x))
        volume <- as.vector(Vo(x))
        data <-data.frame('date'=date,'close'=close,"volume"=volume)
        colnames(data) <- c("date",paste(stock," Close",""),paste(stock," Volume",""))
        data      
        
}







