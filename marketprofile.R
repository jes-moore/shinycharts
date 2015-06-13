marketprofile <- function(Ticker){
        ###############Market Profile#########################
        stockdata<-read.csv(paste("http://chartapi.finance.yahoo.com/instrument/1.0/",Ticker,".AX/chartdata;type=quote;range=",5,"d/csv",sep = ""),
                            skip=22,
                            header = FALSE,
                            stringsAsFactors = FALSE)
        stockdata[,1] <- as.POSIXct(stockdata$V1,origin = "1970-01-01")
        colnames(stockdata) <- c("Timestamp","Close","High","Low","Open","Volume")
        stockdata$Date <- as.Date(stockdata$Timestamp)
        stockdata <- dplyr::group_by(.data = stockdata,Date)
        melted <- melt(data = stockdata,id.vars = c("Timestamp","Date","Volume"),measure.vars = "Close")
        melted$Time <- cut(melted$Timestamp, breaks="hour")
        melted$Time <- strftime(melted$Time, format="%H:%M:%S") 
        
        cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
        
        a <- ggplot(data = melted) + 
                geom_histogram(aes(x = value,fill = Time),binwidth = (range(melted$value)[2] - range(melted$value)[1])/30)+
                facet_grid(. ~ Date,scales = "free_y") +
                scale_fill_manual(values=cbPalette) + 
                ylab("Close Interval Count")+
                xlab("Share Price")+
                scale_x_continuous(breaks = round(seq(min(melted$value), max(melted$value),length.out = 10,),digits = 3)) +       
                coord_flip()
        
        a
        
}

marketprofilevol <- function(Ticker){
        ###############Market Profile#########################
        stockdata<-read.csv(paste("http://chartapi.finance.yahoo.com/instrument/1.0/",Ticker,".AX/chartdata;type=quote;range=",5,"d/csv",sep = ""),
                            skip=22,
                            header = FALSE,
                            stringsAsFactors = FALSE)
        stockdata[,1] <- as.POSIXct(stockdata$V1,origin = "1970-01-01")
        colnames(stockdata) <- c("Timestamp","Close","High","Low","Open","Volume")
        stockdata$Date <- as.Date(stockdata$Timestamp)
        stockdata <- dplyr::group_by(.data = stockdata,Date)
        melted <- melt(data = stockdata,id.vars = c("Timestamp","Date","Volume"),measure.vars = "Close")
        melted$Time <- cut(melted$Timestamp, breaks="hour")
        melted$Time <- strftime(melted$Time, format="%H:%M:%S") 
        
        cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
        
        b <- ggplot(data = melted) + 
                geom_histogram(aes(x = value,fill = Time,weight = Volume),binwidth = (range(melted$value)[2] - range(melted$value)[1])/30)+
                facet_grid(. ~ Date,scales = "free_y") +
                scale_fill_manual(values=cbPalette) +
                ylab("Volume")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                xlab("Share Price")+
                scale_x_continuous(breaks = round(seq(min(melted$value), max(melted$value),length.out = 10,),digits = 3)) +       
                coord_flip()
        
        b
        
}

