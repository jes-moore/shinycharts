sharePricePlot <- function(data){
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
                list(title = list(text = "RSI"),max = 100,min = 0,opposite = FALSE, 
                     top = 325,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "MFI"),max = 100,min = 0,opposite = TRUE, plotBands = list(color = "black",from = 100, to = 110), 
                     top = 425,offset = 0,height = 75,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                     )
                ),
                list(title = list(text = "Aroon"),max = 100,min = 0,opposite = FALSE, 
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
        
        
        a$series(type = "line",name = "RSI",
                 data = toJSONArray2(data[,c("date","RSI")],json = F,names = F),
                 yAxis = 1
        )
        
        a$series(type = "line",name = "MFI",
                 data = toJSONArray2(data[,c("date","mfi")],json = F,names = F),
                 yAxis = 2
        )
        
        a$series(type = "line",name = "AroonUp",
                 data = toJSONArray2(data[,c("date","aroonUp")],json = F,names = F),
                 yAxis = 3             
        )
        
        a$series(type = "line",name = "AroonDn",
                 data = toJSONArray2(data[,c("date","aroonDn")],json = F,names = F),
                 yAxis = 3              
        )
        
        
        a$tooltip(crosshairs = list(T,F),
                  shared=TRUE,
                  formatter = "#! function() { 
                   var d = new Date(this.x);
                   d = d.toLocaleDateString();
                   d = '<strong>' + d + '</strong>';
                   $.each(this.points,function(){
                   d += '<br>' + this.series.name + ': ' + '<strong>' + this.y + '</strong>';
                   })
                   return (d);
                   }!#"
        )       
        
        
        a$legend(align = "right",verticalAlign = "center", 
                 layout = "vertical",itemMarginTop = 10,x = 10,
                 floating = FALSE,backgroundColor = "white")

        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(width = 1000, height = 650,plotBorderWidth = 0.5,plotBorderColor="black")
        a
        
}