library(shiny)
library(rCharts)
load("CWF.rda")
shinyUI(pageWithSidebar(
  h4("Crime Against Women In India in 2012 - By State, Union Territory and City"),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { width: 150px; }"),
      tags$style(type='text/css', ".span4 { max-width: 175px; }")
    ),
    selectInput("crimevar", "Crime Variable:",
                choices = names(CWF[,c(-1)]))),
  mainPanel(
    tabsetPanel(
      tabPanel("States and Union Territories",showOutput("chart1","dimple"),htmlOutput("gvisgeoplotstateut")),
      tabPanel("Cities", showOutput("chart2","dimple"),htmlOutput("gvisgeoplotcity"))
    )
)))