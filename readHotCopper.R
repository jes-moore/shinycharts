readHotCopper <- function(Ticker = "ZIP"){
        ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        library(XML)
        library(lubridate)
        
        ##Write URL based on Ticker
        url <- paste(sep = "","http://hotcopper.com.au/asx/",Ticker)
        ##Read in HTML and get Nodes for post dates
        html <- rvest::html(url)
        dateNodes <- html_nodes(x = html, css = ".medium-only")    
        authorNodes <- html_nodes (x = html, css = ".author a" )
        dateVector <- html_text(dateNodes, trim = TRUE)
        authorVector <- html_text(authorNodes,trim=TRUE)
        postDates <- data.frame(datetime = dateVector,author = authorVector)
        i=2
        while(i<= 20){
                url <- paste(sep = "","http://hotcopper.com.au/asx/",Ticker,"/page-",i)
                html <- rvest::html(url)
                dateNodes <- html_nodes(x = html, css = ".medium-only") 
                authorNodes <- html_nodes (x = html, css = ".author a" )
                dateVector <- html_text(dateNodes, trim = TRUE)
                authorVector <- html_text(authorNodes,trim=TRUE)
                dateDataTable <- data.frame(datetime = dateVector,author = authorVector)
                postDates <- rbind(postDates,dateDataTable)
                i = i+1
        }
        postDates$Stripped <- strptime(x = postDates$datetime,format = "%d/%m/%y %H:%M")
        postDates$Date <- as.Date(postDates$Stripped)
 
        unique <- postDates[,c(4,2)]
        unique <- as.data.frame(table(unique))
        unique$Freq <- ifelse(unique$Freq > 0,1,0)
        final <- count(unique[,c(1,3)])
        final <- final[final$Freq==1,c(1,3)]
        colnames(final) <- c("date","unique")
        countDay <- postDates[,4]
        frequency <- count(countDay)
        colnames(frequency) <- c("date","count")
        frequency$date <- as.character(frequency$date)
        final$date <- as.character(final$date)
        output <- merge(x = frequency,y = final,by = "date")
        output$date <- as.Date(output$date)
        output
}


plotHotCopper <- function(Ticker = "ZIP"){
        library(rCharts)
        library(quantmod)
        library(plyr)
        library(lubridate)
        data <- readHotCopper(Ticker)
        range <- range(data$date)
        dataSP <- getSymbols.yahoo(paste(Ticker,".AX",sep = ""),
                              env = .GlobalEnv,return.class = "data.frame",
                              auto.assign=FALSE,from = range[1],to = range[2])
        dataSP <- data.frame(date = as.Date(rownames(dataSP)),close = dataSP[,4],volume = dataSP[,5])
        data <- merge(x = data,y = dataSP,by = "date",all = TRUE)
        i = 1 
        while(i<= nrow(data)){
                if(is.na(data$close[i])) {
                        data$close[i]=data$close[i-1]
                }
                if(is.na(data$count[i])){
                        data$count[i] = 0
                }
                if(is.na(data$volume[i])){
                        data$volume[i] = 0
                }
                if(is.na(data$unique[i])){
                        data$unique[i] = 0
                }
                i = i+1
        }
        data <- transform(data, date = as.numeric(date) * 86400000)
        # a <- hPlot(data=data,freq~x,type = "column")
        a <- rCharts::Highcharts$new()
        a$yAxis(list(list(title = list(text = "Post Count"),min = 0,max = max(data$count)*1.05),
                     list(title = list(text = "Share Price"),min = 0,max = max (data$close)*1.05,opposite = TRUE),
                     list(title = list(text = "Volume"),min = 0,max = max (data$volume)*1.05,opposite = TRUE),
                     list(title = list(text = "Unique Posters"),min = 0,max = max (data$unique)*1.05,opposite = FALSE)
                     )
                )
        
        a$series(type = "column",name = "Volume",
                 data = toJSONArray2(data[,c(1,5)],json = F,names = F),
                 yAxis =2                 
        )
        a$series(type = "column",name = "Number of Posts",
                 data = toJSONArray2(data[,c(1,2)],json = F,names = F)
        )
        
        
        a$series(type = "column",name = "Unique Posters",
                 data = toJSONArray2(data[,c(1,3)],json = F,names = F),
                 yAxis =3                 
        )
        
        a$series(type = "line",name = "Share Price",
                 data = toJSONArray2(data[,c(1,4)],json = F,names = F),
                 yAxis =1                 
        )
        a$colors('#2f7ed8', '#0d233a',"#9d233a",'darkorange')
        
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
                 layout = "vertical",itemMarginTop = 10,x = 120,
                 floating = TRUE,backgroundColor = "white")
        a$xAxis(type = "datetime")
        a$plotOptions(series = list(pointPadding = 0,groupPadding = 0.2))
        a$chart(zoomType = 'x')
        a$set(width = 1000, height = 600)
        a
        
}





