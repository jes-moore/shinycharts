# startdate <- as.Date(as.Date("1970-01-01") + days(input$dates[1]))
# enddate <- as.Date(as.Date("1970-01-01") + days(input$dates[2]))

compare_stocks <- function(Stock1 = "KAR", Stock2 = "AWE", Stock3 = "SXY",commodity = "gold", start_date = Sys.Date()-months(36),end_date = Sys.Date()){
        library(lubridate)
        library(rCharts)
        ###Gather stock data for various stocks
        stock1_data <- get_data(Stock1,start_date,end_date)    
        stock2_data <- get_data(Stock2,start_date,end_date)  
        stock3_data <- get_data(Stock3,start_date,end_date)  
#         commodity_data <- get_data(commodity,start_date,end_date) 
        data <- merge(stock1_data,stock2_data,"date")
        data <- merge(data,stock3_data,"date")
        data <- transform(data, date = as.numeric(date) * 86400000)
        data <- melt(data = data,id.vars = "date")
        closeData <- data[grep(pattern = "return",x = data$variable,ignore.case = TRUE),]
        colnames(closeData) <- c("date","Stock","%Return")
        closeData$Stock <- as.character(closeData$Stock)
        closeData$Stock <- substr(closeData$Stock,1,3)

#         i <- 1
#         while (i <= nrow(closeData)){
#                 closeData$Stock[i] <- substr(x = closeData$stock[i],start = 1,stop = 3)
#                 i <- i + 1              
#         }

        volumeData <- data[grep(pattern = "volume",x = data$variable,ignore.case = TRUE),] 
        m1 <- hPlot(data = closeData,x ="date",y = "%Return",group = 'Stock', type = "line")
        m1$plotOptions(line=list(marker=list(enabled = F)))
        m1$set(pointSize = 0, lineWidth = 2,width = 750, height = 400)
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
        data$Return <- NA
        i <- 1
        while(i <= nrow(data)){
                data$Return[i] <- round(((data$close[i]-data$close[1])/data$close[1])*100,1)
                i <- i+1
        }
        colnames(data) <- c("date",paste(stock," Close",""),paste(stock," Volume",""),paste(stock," Return",""))
        
        data      
        
}







