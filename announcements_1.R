announcement_data <- function(Ticker = "ZIP"){
                ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        url <- paste(sep = "","http://www.asx.com.au/asx/statistics/announcements.do?by=asxCode&asxCode=",Ticker,"&timeframe=D&period=M6")
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

        announcements
}

