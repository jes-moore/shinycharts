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
        dateVector <- html_text(dateNodes, trim = TRUE)
        postDates <- data.frame(datetime = dateVector)
        i=2
        while(i<= 50){
                url <- paste(sep = "","http://hotcopper.com.au/asx/",Ticker,"/page-",i)
                html <- rvest::html(url)
                dateNodes <- html_nodes(x = html, css = ".medium-only")       
                dateVector <- html_text(dateNodes, trim = TRUE)
                dateDataTable <- data.frame(datetime = dateVector)
                postDates <- rbind(postDates,dateDataTable)
                i = i+1
        }
        postDates$Stripped <- strptime(x = postDates$datetime,format = "%d/%m/%y %H:%M")
        postDates$Date <- as.Date(postDates$Stripped)
        postDates <- postDates[,3]
        frequency <- count(postDates)
        frequency
}
