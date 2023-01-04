#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dataset <- diamonds
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    
    # Grow: 
    tabPanel("Filter",
             tabsetPanel(
                   tabPanel("Year",
                     h4("Years", align="center"),
                     column(6,
                            plotOutput("badYearHist"),
                     ),
                     column(6,
                            plotOutput("goodYearHist"),
                     )
                   ),
                   tabPanel("Process",
                     h4("Processing Method", align="center"),
                     column(6,
                            plotOutput("badProcPie"),
                     ),
                     column(6,
                            plotOutput("goodProcPie")
                     )
                   ),
                   tabPanel("Altitude",
                     h4("Altitude", align="center"),
                     column(6,
                            plotOutput("badAltBox"),
                     ),
                     column(6,
                            plotOutput("goodAltBox")
                     )
                   )
              )
             
    ),
    # Roast:
    tabPanel("Measure",
       fluidRow(
         column(12,
                sliderInput("YEAR","Pick a Year",min=2011,max=2018,
                            value=2010,sep=""),
                plotOutput("harvestBiVar")
         ),
       ),
    ),
    
    tabPanel("Brew",
      fluidRow(
        column(12,
               h1("Brew Your Coffee!", align="center"),
        ),
      ),
      fluidRow(
        column(5, offset=1, align="center",
               sliderInput("year","Pick a Year",min=2010,max=2022,
                           value=2010,sep=""),
               sliderInput("alt","Pick an Altitude (m)",min=0,max=4500,
                           value=0,sep="")
        ),
        column(5, align="center",
               selectInput("proc", "Pick a Processing Method",
                           unique(coffeeFilt$Processing.Method)),
               selectInput("spec","Pick a Coffee Species",
                           unique(coffeeFilt$Species))
        )
      ),
      fluidRow(
        column(7,
               plotOutput("CategScores")
        ),
        column(5,
               plotOutput("TotScores")
        )
      )
    )
  )
))
