sharePricePlot <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(list(title = list(text = "Share Price"),max = max (data$close)*1.05,opposite = TRUE)
                )
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
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data[,c("date","close")],json = F,names = F),
                 color = "orange"
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
        
        
        a$legend(align = "center left",verticalAlign = "top", 
                 layout = "horizontal",itemMarginTop = 10,x = 20,
                 floating = TRUE,backgroundColor = "white")
        a$xAxis(type = "datetime",opposite = TRUE)
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(width = 900, height = 300)
        a
        
}