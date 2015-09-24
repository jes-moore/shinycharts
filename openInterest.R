openInterest <- function(Ticker="ORG"){
        ##Gather company names, ticker, and market cap for a metal
        library(rvest)
        library(plyr)
        library(XML)
        url <- paste("http://www.asx.com.au/asx/markets/optionPrices.do?by=underlyingCode&optionType=B&underlyingCode=",Ticker,sep = "")
        html <- rvest::html(url)
        htmlnodes <- html_nodes(x = html, css = "#optionstable td")
        htmlnodes2 <- html_nodes(x=html,css = "#optionstable .row")
        datanodes1 <- html_text(htmlnodes, trim = TRUE)
        datanodes2 <- html_text(htmlnodes2,trim=TRUE)
        data1 <- as.data.frame(matrix(data = datanodes1,ncol = 9,byrow = TRUE),stringsAsFactors = FALSE)
        data2 <- as.data.frame(matrix(data = datanodes2,ncol = 1,byrow = TRUE),stringsAsFactors = FALSE)
        output <- cbind(data2,data1)
        colnames(output) <- c("ASX Code","Expiry","P/C","Exercise","Bid","Offer","Last","Volume","Open Interest","Margin Price")
        output
}
