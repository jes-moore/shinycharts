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

plotsector <- function(data,sector,ratio){
        library(googleVis)
        library(reshape2)
        library(dplyr)
#         library(ggthemes)
        library(Hmisc)
        data <- data
        data <- data[grepl(pattern = sector,x = data[,2],ignore.case = TRUE)==TRUE,]
        data <- data[order(data[[ratio]],decreasing = TRUE),]
        
c2 <- gvisColumnChart(data[1:min(c(15,(nrow(data)/2))),], 
                      xvar="Ticker", yvar=ratio,
                      options=list(seriesType="bars", legend="top",
                                   bar="{groupWidth:'90%'}",
                                   width=500, height=250,
                                   fontSize = 12,
                                   hAxis = "{direction:-1, slantedText:true, slantedTextAngle:90 }",
                                   chartArea = "{width: '70%', height: '70%'}")
                      )
c1 <- gvisColumnChart(data[(1+min(c(15,(nrow(data)/2)))):min(c(30,(nrow(data)))),], 
                      xvar="Ticker", yvar=ratio,
                      options=list(seriesType="bars", legend="top",
                                   bar="{groupWidth:'90%'}",
                                   width=500, height=250,
                                   fontSize = 12,
                                   hAxis = "{direction:-1, slantedText:true, slantedTextAngle:90 }",
                                   chartArea = "{width: '70%', height: '70%'}")
                      )

gvisMerge(x = c1,y = c2,horizontal = TRUE)


}







