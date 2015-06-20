# startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
# enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))

indices_codes <- function(){
        
        library(rvest)
        library(plyr)
        library(XML)
        
        ##Get ASX Sector Indices
        url <- "https://au.finance.yahoo.com/indices?e=asx_gic"
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data <- html_text(htmlnodes, trim = TRUE)
        data <- data[-(1:9)]
        data <- data[-(61:62)]
        data1 <- as.data.frame(matrix(data = data,ncol = 5,byrow = TRUE),stringsAsFactors = FALSE)
        
        ##Get ASX General Indices
        url <- "https://au.finance.yahoo.com/indices?e=asx"
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data2 <- html_text(htmlnodes, trim = TRUE)
        data2 <- data2[-(1:9)]
        data3 <- as.data.frame(matrix(data = data,ncol = 5,byrow = TRUE),stringsAsFactors = FALSE)
        
        ##Combind the two datasets and set column names
        indices <- rbind(data3,data1)
        colnames(indices) <- c("Symbol","Name","LastTrade","Change","Related") 
        indices$Change <- gsub( " .*$", "", indices$Change)
        indices$LastTrade <- gsub( " .*$", "", indices$LastTrade)
        indices$LastTrade <- gsub(pattern = ",",replacement = "",x = indices$LastTrade)
        indices$LastTrade <- as.numeric(indices$LastTrade)
        indices$Change <- as.numeric(indices$Change)
        indices$PercentChange <- indices$Change/indices$LastTrade 
        indices <- indices[,c(1:4,6)]
        indices
}


compare_indices<- function(start_date = Sys.Date()-months(36),end_date = Sys.Date()){
        library(lubridate)
        library(rCharts)
        library(plyr)
        ###Gather stock data for various stocks
        indices <- indices_codes()
        tickers <- indices$Symbol
        tickers <- as.character(tickers)
        data1 <- get_data(tickers[1],start_date,end_date)
        i <- 2
        while(i <= length(tickers)){
                data <- get_data(tickers[i],start_date,end_date)
                data1 <- merge(x = data,y = data1,by = "date")
                i <- i+1
        }
        
        data <- data1
        data <- transform(data, date = as.numeric(date) * 86400000)

        data <- melt(data = data,id.vars = "date")
        closeData <- data[grep(pattern = "return",x = data$variable,ignore.case = TRUE),]
        colnames(closeData) <- c("date","Stock","%Return")
        closeData$Stock <- as.character(closeData$Stock)
        closeData$Stock <- substr(closeData$Stock,3,6)
        m1 <- hPlot(data = closeData,x ="date",y = "%Return",group = 'Stock', type = "line")
        m1$plotOptions(line=list(marker=list(enabled = F)))
        m1$set(pointSize = 0, lineWidth = 2,width = 750, height = 400)
        m1$tooltip(shared = FALSE,
                   formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   $.each(this.points,function(){
                   d = '<b>' + d + '</b>';
                   d= d + '<br/>' + this.series.name + ' :' + this.y + ' %';                     
                   });
                   return d;
}!#")
        m1$xAxis(type='datetime')
        m1$yAxis(title = list(text = "Return"),labels = list(format = '{value} %'))
        m1$chart(zoomType = 'x')
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
        x <- getSymbols.yahoo(stock,from = start_date,to = end_date,
                              env = .GlobalEnv,return.class = "data.frame",
                              auto.assign=FALSE)
        date <- as.Date(rownames(x))
        close <- as.vector(Cl(x))
        data <-data.frame('date'=date,'close'=close)
        data$Return <- NA
        i <- 1
        while(i <= nrow(data)){
                data$Return[i] <- round(((data$close[i]-data$close[1])/data$close[1])*100,1)
                i <- i+1
        }
        colnames(data) <- c("date",paste(stock," Close",""),paste(stock," Return",""))
        
        data      
        
}