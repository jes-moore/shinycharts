
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
        fluidRow(
        column(width = 4,textInput("Ticker",label = "Stock Ticker XYZ",value = "ZIP")),
        column(width = 1,uiOutput("ggvis_ui")),
        column(width = 4,dateRangeInput("dates","Date range",min = Sys.Date()-years(5),max = Sys.Date(), start = "2014-01-01",end = as.character(Sys.Date())))
        ),
        fluidRow(
                column(width = 12,ggvisOutput("ggvis"))
        )
))
