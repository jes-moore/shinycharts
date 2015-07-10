sharePricePlot <- function(data){
        data <- na.exclude(data)
        data <- transform(data, date = as.numeric(as.Date(date)) * 86400000)
        a <- rCharts::Highcharts$new()
        a$yAxis(list(list(title = list(text = "Share Price"),max = max (data$close)*1.05,opposite = TRUE)
                )
        )
#         layer_lines(x = ~date,y = ~ MA, stroke = "MA")%>%
#         layer_lines(x = ~date,y = ~ EMA,stroke = "EMA")%>%
#         layer_lines(stroke := "grey", strokeWidth := 2)%>%
#         ###############################
#         #######Add Bollinger Lines######
#         layer_lines(y = ~ up, stroke := "red", strokeWidth := 1.5)%>%
#         layer_lines(y = ~ dn, stroke := "red", strokeWidth := 1.5)%>%
#         layer_lines(y= ~mavg,stroke := "red",strokeWidth :=1.2 )%>%
                
        a$series(type = "line",name = "MA",
                 data = toJSONArray2(data[,c("date","MA")],json = F,names = F)              
        )

        a$series(type = "line",name = "EMA",
                 data = toJSONArray2(data[,c("date","EMA")],json = F,names = F)              
        )
        a$series(type = "line",name = "AroonUp",
                 data = toJSONArray2(data[,c("date","up")],json = F,names = F),
                 color = "red"              
        )
        a$series(type = "line",name = "AroonDn",
                 data = toJSONArray2(data[,c("date","dn")],json = F,names = F),
                 color = "red"              
        )

        a$series(type = "line",name = "Aroonavg",
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
        a$xAxis(type = "datetime")
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2,marker = list(enabled=FALSE)))
        a$set(width = 900, height = 500)
        a
        
}