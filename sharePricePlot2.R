sharePricePlot2 <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(
                list(title = list(text = "Share Price"),max = max(data$close)*1.05,opposite = TRUE,height = 300,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "MACD"),max = max(data$MACD)*1.05,min = min(data$MACD)*0.95,opposite = FALSE, 
                     top = 325,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "CHAI"),max = max(data$Chai)*1.05,min = min(data$Chai)*0.95,
                     opposite = TRUE, plotBands = list(color = "black",from = 100, to = 110), 
                     top = 425,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "Elder"),opposite = FALSE, 
                     top = 525,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                )
        )
        )
        
        a$xAxis(type = "datetime")
        
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data[,c("date","close")],json = F,names = F),
                 color = "orange"
        )
        
        a$series(type = "line",name = "MA",
                 data = toJSONArray2(data[,c("date","MA")],json = F,names = F)              
        )
        
        a$series(type = "line",name = "EMA",
                 data = toJSONArray2(data[,c("date","EMA")],json = F,names = F)              
        )
        a$series(type = "line",name = "BollUp",
                 data = toJSONArray2(data[,c("date","up")],json = F,names = F),
                 color = "red"              
        )
        a$series(type = "line",name = "BollDn",
                 data = toJSONArray2(data[,c("date","dn")],json = F,names = F),
                 color = "red"              
        )
        
        a$series(type = "line",name = "Bollavg",
                 data = toJSONArray2(data[,c("date","mavg")],json = F,names = F),
                 color = "red"
        )
        
        
        a$series(type = "line",name = "MACD",
                 data = toJSONArray2(data[,c("date","MACD")],json = F,names = F),
                 yAxis = 1
        )
        
        a$series(type = "line",name = "SIGNAL",
                 data = toJSONArray2(data[,c("date","SIGNAL")],json = F,names = F),
                 yAxis = 1
        )
        
        a$series(type = "line",name = "CHAI",
                 data = toJSONArray2(data[,c("date","Chai")],json = F,names = F),
                 yAxis = 2
        )
        
        a$series(type = "line",name = "CHAI SMA",
                 data = toJSONArray2(data[,c("date","chaiSMA")],json = F,names = F),
                 yAxis = 2
        )
        
        a$series(type = "line",name = "Elder Bear",
                 data = toJSONArray2(data[,c("date","Bear")],json = F,names = F),
                 yAxis = 3             
        )
        
        a$series(type = "line",name = "ElderBull",
                 data = toJSONArray2(data[,c("date","Bull")],json = F,names = F),
                 yAxis = 3              
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
                 floating = FALSE,backgroundColor = "white",
                 labelFormatter="#! function () {return this.name;}!#")
        
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(zoomType = 'x',width = 1000, height = 650,plotBorderWidth = 0.5,plotBorderColor="black")
        a
        
}