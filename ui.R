
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
                                         sliderInput("smaval",label = h5("Simple Moving Average Days"),min = 1,max = 100,value = 1)
                                         ),
                        conditionalPanel("input.tab==1",
                                         sliderInput("bollval",label = h5("Bollinger Days"),min = 1, max = 100,value = 20)
                                         ),
                        conditionalPanel("input.tab==1",
                                         sliderInput("emaval",label = h5("Exponential MA Days"),min = 1,max = 100,value = 1)
                                         ),
                        conditionalPanel("input.tab==1",
                                         checkboxGroupInput("price",inline = TRUE, h5("Indicators"),
                                                            c("MACD" = 1,
                                                              "Elder Rays" = 2,
                                                              "Chai Osc" = 3)
                                                            )
                                         )
                ),
        mainPanel(
                tabsetPanel(type = "tabs",id = "tab",
                            tabPanel("Price Indicators",
                                     value = 1,
                                     ggvisOutput("ggvis"),
                                     conditionalPanel("input.price[0] == 1",
                                                      ggvisOutput("ggvismacd")
                                                      ),
                                     conditionalPanel("input.price[1] == 2 || input.price[0] == 2 ",
                                                      ggvisOutput("ggvis1")
                                                      ),
                                     conditionalPanel("input.price[1] == 3 || input.price[0] == 3 || input.price[2] == 3 ",
                                                      ggvisOutput("ggvischai")
                                     )
                                     ),
                            tabPanel("Momentum Indicators",value = 2,ggvisOutput("ggvisrsi"),ggvisOutput("ggvismfi")),
                            tabPanel("Volume and Shorts",value = 3,ggvisOutput("ggvisvol"))
                            )
                )
        )
))

