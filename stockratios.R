stock_ratios <- function(){
        data <- read.csv("asx.csv",stringsAsFactors = FALSE)
        data        
}


stock_stats <- function(symbol) {
        ##To call use:
        ##tickers <- c("ZIP.AX","KAR.AX")
        ##stats <- ldply(tickers,getKeyStats_xpath)      
        require(xml2)
        require(plyr)
        yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
        html_text <- read_html(paste(yahoo.URL, symbol,".AX", sep = ""), encoding="UTF-8")
        
        #search for <td> nodes anywhere that have class 'yfnc_tablehead1'
        nodes <- xml_find_all(html_text, "/*//td[@class='yfnc_tablehead1']")
        
        if(length(nodes) > 0 ) {
                measures <- sapply(nodes, xml2::xml_text)
                
                #Clean up the column name
                measures <- gsub(" *[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))   
                
                #Remove dups
                dups <- which(duplicated(measures))
                #print(dups) 
                for(i in 1:length(dups)) 
                        measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")
                
                #use siblings function to get value
                values <- sapply(nodes, function(x)  xml2::xml_text(xml_siblings(x)))
                
                df <- data.frame(t(values),stringsAsFactors = FALSE)
                colnames(df) <- measures
                df$Ticker <- symbol
                return(df)
        } else {
                return()
        }
}

tab_stats <- function(symbol){
        data <- stock_stats(symbol)
        data$Ticker <- symbol
        data <- melt(data = data,id.vars = "Ticker")
        data <- data[,2:3]
        colnames(data) <- c("Datapoint","Value")
        data
}


