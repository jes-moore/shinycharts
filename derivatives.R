readDerivatives <- function(){
        data <- read.csv("http://www.asx.com.au/data/ASXCLDerivativesMasterList.csv",stringsAsFactors = FALSE)
        data
}

analyzeDerivatives <- function(data){
        library(dplyr)
        data <- readDerivatives()
        data <- group_by_(.data = data,"DerivativeProduct","OptType")  
        df1 <- ddply(data, .(DerivativeProduct, Underlying), summarise, newvar = sum(ContractSize))
        df1
}

derivativeSpecific <- function(){
        data <- readDerivatives()
        tickers <- unique(data$Underlying)
        tickers
        
}