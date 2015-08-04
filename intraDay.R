intraDay <- function(Ticker = "ZIP",Days = 1){
        library(dplyr)
        library(quantmod)
        data<-read.csv(paste("http://chartapi.finance.yahoo.com/instrument/1.0/",Ticker,".AX/chartdata;type=quote;range=",Days,"d/csv",sep = ""),skip=19,header = FALSE)
        data[,1] <- as.POSIXct(data$V1,origin = "1970-01-01")
        data[,1] <- data[,1] + hours(10)
        colnames(data) <- c("Datetime","Close","High","Low","Open","Volume")
        data$Datetime <- as.numeric(data$Datetime)
        data$Datetime <- data$Datetime*1000
        data
        data <- mutate(data,CumulativeVolume = cumsum(data$Volume))
        data <- na.exclude(data)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(
                list(title = list(text = "Share Price"),max = max(data$Close)*1.05,min = min(data$Close)*0.95,opposite = TRUE,height = 300,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "Volume"),opposite = FALSE, 
                     top = 325,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "Cumulative Volume"),opposite = TRUE, 
                     top = 425,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                                )
                     )
                )
        )
        
        
        a$xAxis(type = "datetime")
        
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data[,c("Datetime","Close")],json = F,names = F),
                 color = "orange"
        )
        
        a$series(type = "line",name = "Volume",
                 data = toJSONArray2(data[,c("Datetime","Volume")],json = F,names = F),yAxis = 1              
        )
        
        a$series(type = "line",name = "Cumulative Volume",
                 data = toJSONArray2(data[,c("Datetime","CumulativeVolume")],json = F,names = F) , yAxis =2             
        )
        
        a$tooltip(crosshairs = list(T,F),
                  shared=TRUE,
                  formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   d = '<strong>' + d + '</strong>';
                   $.each(this.points,function(){
                   d += '<br>' + this.series.name + ': ' + '<strong>' + Highcharts.numberFormat(this.y,2,'.') + '</strong>';
                   })
                   return (d);
                   }!#"
        )       
        
        
        a$legend(align = "right",verticalAlign = "center", 
                 layout = "vertical",itemMarginTop = 10,x = 10,
                 floating = FALSE,backgroundColor = "white")
        
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0,marker = list(enabled=FALSE)))
        a$chart(zoomType = 'x',width = 1000, height = 550,plotBorderWidth = 0.5,plotBorderColor="black")
        a
        
        
        
}