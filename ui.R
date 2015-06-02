
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
shinyUI(fluidPage(
        
        sidebarLayout(fluid = TRUE,
                sidebarPanel(
                        width = 3,
                        textInput("Ticker",label = h5("Stock Ticker XYZ"),value = "ZIP"),
                        #uiOutput("ggvis_ui"),
                        #uiOutput("ggvis_ui1"),
                        dateRangeInput("dates",h5("Date range"),min = Sys.Date()-years(5),max = Sys.Date(), start = "2015-01-01",end = as.character(Sys.Date())),
                        conditionalPanel("input.tab==1",
                                         sliderInput("smaval",label = h5("Simple Moving Average Days"),min = 1,max = 100,value = 1)),
                        conditionalPanel("input.tab==1",
                                         sliderInput("bollval",label = h5("Bollinger Days"),min = 1, max = 100,value = 20)),
                        conditionalPanel("input.tab==1",
                                         sliderInput("emaval",label = h5("Exponential MA Days"),min = 1,max = 100,value = 1))
                                                
                ),
        mainPanel(
                tabsetPanel(type = "tabs",id = "tab",
                            tabPanel("Price Indicators",value = 1,ggvisOutput("ggvis"),ggvisOutput("ggvismacd"),ggvisOutput("ggvis1")),
                            tabPanel("Momentum Indicators",value = 2,ggvisOutput("ggvisrsi"),ggvisOutput("ggvismfi")),
                            tabPanel("Volume and Shorts",value = 3,ggvisOutput("ggvisvol"))
                            )
                )
        )
))

