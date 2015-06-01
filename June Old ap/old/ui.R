
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
                column(width = 4,dateRangeInput("dates","Date range",min = Sys.Date()-years(5),max = Sys.Date(), start = "2015-01-01",end = as.character(Sys.Date())))
        ),
        ##Row 2
        fluidRow(
                column(2,checkboxGroupInput(inputId = "upperindicators", label = "Upper Indicators", 
                                   choices = list("SMA" = 1, "EMA" = 2, "Bollinger" = 3),
                                   selected = NULL)),
                
                ##Show if SMA is selected
                column(2,
                       conditionalPanel(condition = "input.upperindicators[0] == 1",
                               sliderInput("smaval",
                                           label = "SMA Days", 
                                           min = 0, 
                                           max = 100, 
                                           value = 20)
                               )
                ),
                ##Show if EMA is selected
                column(2,
                       conditionalPanel(condition = "input.upperindicators[0] == 2 || input.upperindicators[1] == 2",
                                        sliderInput("emaval",
                                                    label = "EMA Days", 
                                                    min = 0, 
                                                    max = 100, 
                                                    value = 20)
                       )
                ),
                ##Show if Bollinger is selected
                column(2,
                       conditionalPanel(condition = "input.upperindicators[0] == 3 || input.upperindicators[1] == 3 || input.upperindicators[2] == 3",
                                        sliderInput("bollval",
                                                    label = "Bollinger", 
                                                    min = 0, 
                                                    max = 100, 
                                                    value = 20)
                       )
                )
                
        ),
        ##Row 3
        fluidRow(
                column(width = 12,ggvisOutput("ggvis"))
        )
))
