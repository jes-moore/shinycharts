upcoming_floats <- function(){
        ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        library(XML)
        url <- "http://www.asx.com.au/prices/upcoming.htm"
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data <- html_text(htmlnodes, trim = TRUE)
        data1 <- as.data.frame(matrix(data = data,ncol = 3,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(data1) <- c("Company","Proposed ASX Code","Proposed Listing Date/Time")
        doc <- htmlParse(url)
        links <- xpathSApply(doc, "//a/@href")
        free(doc)
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        links <- links[grepl(pattern = "upcomingFloat",x = links[,1],ignore.case = TRUE)==TRUE,]
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        links$Link <- NA
        i <- 1
        while(i<=nrow(links)){
                links$Link[i] <- paste("http://www.asx.com.au",links$V1[i],sep = "")
                i <- i+1        
        }##End While
        announcements <- cbind(data1,Links = links[,2])
        announcements
}

recent_floats <- function(){
        ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        library(XML)
        url <- "http://www.asx.com.au/asx/research/recentFloats.do"
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data <- html_text(htmlnodes, trim = TRUE)
        data1 <- as.data.frame(matrix(data = data,ncol = 6,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(data1) <- c("Company","ASX Code","Listing Date","Open","Close","")
        doc <- htmlParse(url)
        links <- xpathSApply(doc, "//a/@href")
        free(doc)
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        links <- links[grepl(pattern = "company.do",x = links[,1],ignore.case = TRUE)==TRUE,]
        links <- as.data.frame(matrix(data = links,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        links$Link <- NA
        i <- 1
        while(i<=nrow(links)){
                links$Link[i] <- paste("http://www.asx.com.au",links$V1[i],sep = "")
                i <- i+1        
        }##End While
        announcements <- cbind(data1,Links = links[,2])
        announcements
}