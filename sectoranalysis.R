analyzesector <- function(sector){
      library(ggplot2)
      library(reshape2)
      library(dplyr)
#       library(ggthemes)
      library(Hmisc)
      data <- read.csv(file = "asx.csv",stringsAsFactors = FALSE)
      data <- data[grepl(pattern = sector,x = data[,2],ignore.case = TRUE)==TRUE,]
      data
}

plotsector <- function(data,sector){
        library(googleVis)
        library(reshape2)
        library(dplyr)
#         library(ggthemes)
        library(Hmisc)
        data <- data
        data <- data[grepl(pattern = sector,x = data[,2],ignore.case = TRUE)==TRUE,]
        data <- arrange(data,desc(data$Market.Cap))
        
cc <- gvisColumnChart(data[1:30,], 
                      xvar="Ticker", yvar="Enterprise.Value.EBITDA",
                      options=list(seriesType="bars", legend="top",
                                   bar="{groupWidth:'90%'}",
                                   width=1000, height=300,
                                   fontSize = 12,
                                   hAxis = "{direction:-1, slantedText:true, slantedTextAngle:90 }",
                                   chartArea = "{width: '90%', height: '75%'}")
                      )
cc
        
}







