
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
library(dygraphs)
options(RCHART_LIB = 'MORRIS')
currencies = sort(c("XAF", "ARS", "AUD", "BSD", "BRL", "BGN", "CAD", "CLP", "CNY", "COP", "HRK",  "CYP", "CZK", "DKK", "LTC", "BTC", "XCD", "EEK", "EUR", "FJD", 
               "XPF", "GHS", "GTQ", "HNL", "HKD", "HUF", "ISK", "INR", "IDR",  "ILS",  "JMD",  "JPY", "LVL", "LTL", "MYR", "MXN", "MAD", "MMK", "ANG", "NZD", 
               "NOK", "PKR", "PAB", "PEN", "PHP", "PLN", "Gold","QAR", "RON",  "RUB",  "SAR",  "RSD", "SGD", "ZAR", "KRW", "LKR", "SEK", "CHF", "TWD", "THB", 
               "TTD", "TND", "TRY", "AED", "GBP", "USD", "VND", "VEF","none" ))

# ui.R

shinyUI(navbarPage(title = "Intelligent Pursuit",
        tabPanel(title = "Technical Analysis",
                 value = "TA",
                 sidebarLayout(fluid = TRUE,
                        sidebarPanel(
                                width = 3,
                                textInput("Ticker",label = h5("Stock Ticker XYZ"),value = "KAR"),
                                submitButton(text = "Apply Changes"),
                                conditionalPanel("input.tab==1 || input.tab==2 || input.tab==3",
                                                 dateRangeInput("dates",h5("Date range"),min = Sys.Date()-years(5),max = Sys.Date(), start = "2015-01-01",end = as.character(Sys.Date()+days(1)))
                                                 ),
                                conditionalPanel("input.tab==1 || input.tab==2",
                                                 sliderInput("smaval",label = h5("Simple Moving Average Days"),min = 1,max = 100,value = 1)
                                                 ),
                                
                                conditionalPanel("input.tab==1",
                                                 sliderInput("bollval",label = h5("Bollinger Days"),min = 1, max = 100,value = 20)
                                                 ),
                                
                                conditionalPanel("input.tab==1 || input.tab==2",
                                                 sliderInput("emaval",label = h5("Exponential MA Days"),min = 1,max = 100,value = 1)
                                                 ),
                                
                                conditionalPanel("input.tab==2",
                                                 sliderInput("aroon",label = h5("Aroon Period"),min = 10,max = 75,value = 20)
                                ),
                                
                                conditionalPanel("input.tab==1",
                                                 checkboxGroupInput("price",selected = c(1,2,3),inline = TRUE, h5("Indicators"),
                                                                    c("MACD" = 1,
                                                                      "Elder" = 2,
                                                                      "Chai" = 3)
                                                                    )
                                                 ),
                                
                                ####################################Currency Tab#########################################################
                                ####################################Currency Tab#########################################################
                                conditionalPanel("input.tab==5",
                                                 selectInput("base",choices = currencies,selected = "USD", label = h5("Base Currency")
                                                           )
                                ),
                                
                                conditionalPanel("input.tab==5",
                                                 selectInput("comp1",choices = currencies,selected = "AUD", label = h5("Comparison Currency")
                                                             )
                                ),
                                
                                conditionalPanel("input.tab==5",
                                                 selectInput("comp2",choices = currencies,selected = "none", label = h5("Comparison Currency")
                                                 )
                                ),
                                
                                conditionalPanel("input.tab==5",
                                                 selectInput("comp3",choices = currencies,selected = "none", label = h5("Comparison Currency")
                                                 )
                                ),
                                #########################################################################################################
                                #########################################################################################################
                                
                                conditionalPanel("input.tab==2",
                                                 checkboxGroupInput("momentum",selected = c(1,2,3),inline = TRUE, h5("Indicators"),
                                                                    c("Aroon" = 1,
                                                                      "RSI" = 2,
                                                                      "MFI" = 3)
                                                 )
                                )
                        ),
                mainPanel(
                        tabsetPanel(type = "tabs",id = "tab",
                                    tabPanel("Price Indicators",
                                             value = 1,
                                             #showOutput("rchart1", "morris"),
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
                                    tabPanel("Momentum Indicators",
                                             value = 2,
                                             ggvisOutput("ggvis3"),
                                             conditionalPanel("input.momentum[0] == 1",
                                                              ggvisOutput("ggvisaroon")
                                             ),
                                             conditionalPanel("input.momentum[1] == 2 || input.momentum[0] == 2 ",
                                                              ggvisOutput("ggvisrsi")
                                             ),
                                             conditionalPanel("input.momentum[1] == 3 || input.momentum[0] == 3 || input.momentum[2] == 3 ",
                                                              ggvisOutput("ggvismfi")
                                             )
                                    ),
                                    tabPanel("Volume",value = 3,ggvisOutput("ggvissp3"),ggvisOutput("ggvisshort"),ggvisOutput("ggvissar"),ggvisOutput("ggvisvol"),ggvisOutput("ggvisadl")),
                                    tabPanel("Market Profile",value = 7,plotOutput("profile1",height = 350,width = 900),plotOutput("profile2",height = 350,width = 900)),
#                                     tabPanel("Foreign Exchange",value = 5,
#                                              h5(textOutput("ct1")),
#                                              ggvisOutput("ggviscomp1"),
#                                              conditionalPanel("input.comp2 != 'none'",
#                                                               h5(textOutput("ct2"))
#                                              ),
#                                              conditionalPanel("input.comp2 != 'none'",
#                                                               ggvisOutput("ggviscomp2")
#                                                               )
#                                              
#                                              ),
                                    tabPanel("Announcements",value = 8,div(dataTableOutput("announce"),style = "font-size:80%")),
                                    tabPanel("Statistics",value = 9,div(dataTableOutput("stats"),style = "font-size:80%"))
                                    )
                        )##Close Mainpanel
                )##Close Sidebar Layout
        ),#Close tabPanel
        tabPanel(title = "Fundamental Analysis",
                 value = "FA",
                 bootstrapPage(mainPanel(
                                       tabsetPanel(type = "tabs",id = "fa",
                                                   tabPanel("Sector Analysis",value = 4,
                                                            submitButton(text = "Apply Changes"),
                                                            selectInput(inputId = "sector",
                                                                        label = "Sector",
                                                                        choices = read.csv("sectors.csv",stringsAsFactors = FALSE,header = TRUE)
                                                            ),
                                                            selectInput(inputId = "sector_ratio1",
                                                                        selected = "Enterprise.Value",
                                                                        label = "Comparative Ratio 1",
                                                                        choices = c("Market.Cap","Enterprise.Value",'Trailing.P.E' , "Forward.P.E","PEG.Ratio","Price.Sales","Price.Book","Enterprise.Value.Revenue","Enterprise.Value.EBITDA","Total.Cash.Per.Share","Total.Debt.Equity")
                                                                        ),
                                                            htmlOutput("sector1"),
                                                            selectInput(inputId = "sector_ratio2",
                                                                        label = "Comparative Ratio 2",
                                                                        choices = c("Market.Cap","Enterprise.Value",'Trailing.P.E' , "Forward.P.E","PEG.Ratio","Price.Sales","Price.Book","Enterprise.Value.Revenue","Enterprise.Value.EBITDA","Total.Cash.Per.Share","Total.Debt.Equity")),
                                                            htmlOutput("sector2")
                                                            ),#close tabpbanel
                                                   tabPanel("Economic Calendar" , value = 1,div(dataTableOutput("table"),style = "font-size:80%")),
                                                   tabPanel("Shorting",value = 2,div(dataTableOutput("shorttable"),style = "font-size:80%")),
                                                   tabPanel("Stock Ratios",value = 3,div(dataTableOutput("ratios"),style = "font-size:80%")),
                                                   tabPanel("Upcoming Floats",value = 5,div(dataTableOutput("floats"),style = "font-size:80%")),
                                                   tabPanel("Recent Floats",value = 6,div(dataTableOutput("recent_floats"),style = "font-size:80%"))
                                                   )#Close tabsetpanel
                               )##Close Mainpanel2
                 )##Close Bootstrappage
        )#Close tabPanel2
        )##Close Navbar Page
)##Close Shiny UI




