
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(quantmod)
library(ggplot2)
library(dplyr)
library(grid)
library(TTR)
library(scales)
library(lubridate)
library(shiny)
library(DT)
library(ggvis)
# ui.R
shinyUI(
        fluidPage(
        ##Row 1
        fluidRow(
                column(width = 4,textInput("Ticker",label = "Stock Ticker XYZ",value = "ZIP")),
                column(width = 1,uiOutput("ggvis_ui")),
                column(width = 1,uiOutput("ggvis_ui1")),
                column(width = 4,dateRangeInput("dates","Date range",min = Sys.Date()-years(5),max = Sys.Date(), start = "2015-01-01",end = as.character(Sys.Date())))
        ),
        ##Row 2
        fluidRow(
                column(3,sliderInput("smaval",
                                     label = "SMA Days",
                                     min = 1,
                                     max = 100,
                                     value = 1)
                        ),
                column(3,sliderInput("emaval",
                                     label = "EMA Days", 
                                     min = 1, 
                                     max = 100, 
                                     value = 1)
                       ),
                column(3,sliderInput("bollval",
                                     label = "Bollinger", 
                                     min = 1, 
                                     max = 100, 
                                     value = 20)
                       )
                
        ),
        ##Row 3
        fluidRow(
                column(width = 12,ggvisOutput("ggvis"))
        ),
        fluidRow(
                column(width = 12,ggvisOutput("ggvis1"))
        )
))
