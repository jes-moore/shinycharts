downloadWarrants <- function(){
        data <- read.csv("http://www.asx.com.au/data/Warrants.csv",sep = ",",skip = 1,stringsAsFactors = FALSE,header = TRUE)
        data <- as.data.frame(data[,-11])
        return(data)
}