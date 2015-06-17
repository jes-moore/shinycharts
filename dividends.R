dividends <- function(){
        ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        library(XML)
        url <- "http://www.morningstar.com.au/Stocks/UpcomingDividends"
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "td")
        data <- html_text(htmlnodes, trim = TRUE)
        data1 <- as.data.frame(matrix(data = data,ncol = 6,byrow = TRUE),stringsAsFactors = FALSE)
        colnames(data1) <- c("ASX Code","Company Name","Ex Dividend Date","Dividend Pay Date","Amount(cents)","Franking %")
        data1
}
