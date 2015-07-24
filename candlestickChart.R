candlestickChart <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        data <- data[,c("date","open","high","low","close")]
        colnames(data) <- c("x", "open", "high", "low", "close")
        rownames(data) <- NULL
        a <- rCharts::Highcharts$new()
        a$yAxis(list(
                list(title = list(text = "Share Price"),max = max(data$close)*1.05,opposite = TRUE,height = 300,
                     labels = list(
                             style = list(
                                     fontWeight = "bold")
                             )
                     )
                )
        )
        a$setTemplate(script="highstock.html")
        a$xAxis(type = "datetime")
        
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data,json = F,names = F),
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
        
        
        a$legend(align = "right",verticalAlign = "center", 
                 layout = "vertical",itemMarginTop = 10,x = 10,
                 floating = FALSE,backgroundColor = "white")
        
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$chart(type = "candlestick",width = 1000, height = 650,plotBorderWidth = 0.5,plotBorderColor="black")
        a
        
}