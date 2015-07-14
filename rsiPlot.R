rsiPlot <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(list(title = list(text = "Indicator"),min=0,max=100,opposite = TRUE)
        )
        )
        
        
        a$series(type = "line",name = "RSI",
                 data = toJSONArray2(data[,c("date","RSI")],json = F,names = F)              
        )
        a$series(type = "line",name = "MFI",
                 data = toJSONArray2(data[,c("date","mfi")],json = F,names = F)              
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
        a$xAxis(type = "datetime")
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(width = 900, height = 150)
        a
        
}