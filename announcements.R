announcement_data <- function(Ticker = "ZIP"){
                ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        url <- paste(sep = "","http://www.asx.com.au/asx/statistics/announcements.do?by=asxCode&asxCode=","ZIP","&timeframe=D&period=M6")
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data <- html_text(htmlnodes, trim = TRUE)
        data1 <- as.data.frame(matrix(data = data,ncol = 6,byrow = TRUE),stringsAsFactors = FALSE)
        columns <- htmlnodes <- html_nodes(x = html, css = "th")
        columns <- html_text(columns, trim = TRUE)
        columns <- as.data.frame(matrix(data = columns,ncol = 6,byrow = TRUE))
        colnames(data1) <- unlist(columns[1,])
        doc <- htmlParse(url)
        links <- xpathSApply(doc, "//a/@href")
        free(doc)
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        #http://www.asx.com.au /asx/statistics/displayAnnouncement.do?display=pdf&idsId=01627401
        
        links <- links[grepl(pattern = "displayAnnouncement",x = links[,1],ignore.case = TRUE)==TRUE,]
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        links$Link <- NA
        i <- 1
        while(i<=nrow(links)){
                links$Link[i] <- paste("http://www.asx.com.au",links$V1[i],sep = "")
                i <- i+1        
        }##End While
        announcements <- cbind(data1[,c(1,3,4,5)],Links = links[,2])
        announcements$Date <- as.Date(strptime(x = announcements$Date,format = "%d/%m/%Y"))
        
        ##################################Get SP Data#########################################
        x <- getSymbols.yahoo(paste("ZIP",".AX",sep = ""),
                              env = .GlobalEnv,return.class = "data.frame",
                              auto.assign=FALSE)
        #the below is done redundantly for ease of maintenance later on
        #First, strip OHLC data (need to vectorize)
        date <- as.Date(rownames(x))
        open <- as.vector(Op(x))
        high <- as.vector(Hi(x))
        low <- as.vector(Lo(x))
        close <- as.vector(Cl(x))
        volume <- as.vector(Vo(x))
        #Then build the data frame
        xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close,"volume"=volume)
        xSubset
        announcements$Close <- NA
        i <- 1
        while(i<=nrow(announcements)){
                announcements$Close[i] <- xSubset[grep(pattern = announcements$Date[i],x = xSubset$date),5]
                i <- i+1        
        }##End While
        
        
        list(a = announcements,b = xSubset)
}

announcement_plot <- function(Ticker = "ZIP"){
        

        
        
        
        
}
