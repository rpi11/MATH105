#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Quality of Life vs. Emissions"),
  sidebarLayout(
    sidebarPanel(
      h4("• Countries", align="left"),
      selectInput("countries","Pick Some Countries",
                  countries,multiple=T),
      h4("• Categories", align="left"),
      selectInput("qol",
                  "Pick a quality of life category",qol_categories),
      selectInput("emission",
                  "Pick an emission measurement category",em_categories),
      h4("• Year", align="left"),
      sliderInput("year","Pick a Year",min=1990,max=2019,
                  value=1990,sep="",
                  animate=animationOptions(interval=500))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("QOL Time Series",plotOutput("QolCategTime")),
        tabPanel("Emission Time Series",plotOutput("EmCategTime")),
        tabPanel("Bivariate Plot At Year",plotOutput("BivarByYear")),
        tabPanel("Bivariate Size Plot by Year",plotOutput("BivarSizeByYear")),
      )
    )
  )
  
))
