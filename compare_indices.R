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


compare_indices<- function(start_date = Sys.Date()-months(24),end_date = Sys.Date()){
        library(lubridate)
        library(rCharts)
        library(plyr)
        ###Gather stock data for various stocks
        indices <- indices_codes()
        tickers <- indices$Symbol
        tickers <- as.character(tickers)
        data1 <- get_data(tickers[1],start_date,end_date)
        i <- 2
        while(i <= 8){
                data <- get_data(tickers[i],start_date,end_date)
                data1 <- merge(x = data,y = data1,by = "date")
                i <- i+1
        }
        
        data2 <- get_data(tickers[1],start_date,end_date)
        while(i <= 16){
                data <- get_data(tickers[i],start_date,end_date)
                data2 <- merge(x = data,y = data2,by = "date")
                i <- i+1
        }
        
        data3 <- get_data(tickers[1],start_date,end_date)
        while(i <= 24){
                data <- get_data(tickers[i],start_date,end_date)
                data3 <- merge(x = data,y = data3,by = "date")
                i <- i+1
        }
        
        ##First Data Set Transform
        data1 <- transform(data1, date = as.numeric(date) * 86400000)
        data1 <- melt(data = data1,id.vars = "date")
        data1 <- data1[grep(pattern = "return",x = data1$variable,ignore.case = TRUE),]
        colnames(data1) <- c("date","Stock","%Return")
        data1$Stock <- as.character(data1$Stock)
        data1$Stock <- substr(data1$Stock,3,6)
        ###First Plot
        m1 <- hPlot(data = data1,x ="date",y = "%Return",group = 'Stock', type = "line")
        m1$plotOptions(line=list(marker=list(enabled = F)))
        m1$set(pointSize = 0, lineWidth = 2,width = 750, height = 600)
        m1$tooltip(shared = TRUE,
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
        
        ##Second Data Set Transform
        data2 <- transform(data2, date = as.numeric(date) * 86400000)
        data2 <- melt(data = data2,id.vars = "date")
        data2 <- data2[grep(pattern = "return",x = data2$variable,ignore.case = TRUE),]
        colnames(data2) <- c("date","Stock","%Return")
        data2$Stock <- as.character(data2$Stock)
        data2$Stock <- substr(data2$Stock,3,6)
        ###Second Plot
        m2 <- hPlot(data = data2,x ="date",y = "%Return",group = 'Stock', type = "line")
        m2$plotOptions(line=list(marker=list(enabled = F)))
        m2$set(pointSize = 0, lineWidth = 2,width = 750, height = 600)
        m2$tooltip(shared = TRUE,
                   formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   $.each(this.points,function(){
                   d = '<b>' + d + '</b>';
                   d= d + '<br/>' + this.series.name + ' :' + this.y + ' %';                     
                   });
                   return d;
}!#")
        m2$xAxis(type='datetime')
        m2$yAxis(title = list(text = "Return"),labels = list(format = '{value} %'))
        m2$chart(zoomType = 'x')
        
        ##Third Data Set Transform
        data3 <- transform(data3, date = as.numeric(date) * 86400000)
        data3 <- melt(data = data3,id.vars = "date")
        data3 <- data3[grep(pattern = "return",x = data3$variable,ignore.case = TRUE),]
        colnames(data3) <- c("date","Stock","%Return")
        data3$Stock <- as.character(data3$Stock)
        data3$Stock <- substr(data3$Stock,3,6)
        ###Third Plot
        m3 <- hPlot(data = data3,x ="date",y = "%Return",group = 'Stock', type = "line")
        m3$plotOptions(line=list(marker=list(enabled = F)))
        m3$set(pointSize = 0, lineWidth = 2,width = 750, height = 600)
        m3$tooltip(shared = TRUE,
                   crosshairs = list(T, F),
                   formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   $.each(this.points,function(){
                   d = '<b>' + d + '</b>';
                   d= d + '<br/>' + this.series.name + ' :' + this.y + ' %';                     
                   });
                   return d;
}!#")
        m3$xAxis(type='datetime')
        m3$yAxis(title = list(text = "Return"),labels = list(format = '{value} %'))
        m3$chart(zoomType = 'x')
        m3        
#         output <- list(m1 = m1,m2 = m2, m3 = m3)
        
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