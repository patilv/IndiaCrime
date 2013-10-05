library(rCharts)
load("TotCrimesmelt.rda")
shinyUI(pageWithSidebar(
  h4("Crime Against Women In India 2001-2012"),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { width: 150px; }"),
      tags$style(type='text/css', ".span4 { max-width:175px; }")
    ),
selectInput("stateut", "Overall Crime for:",
                                    choices =c(unique(levels(TotCrimesmelt$StateUT))),select="TOTAL")),
        
mainPanel(
showOutput("TCrimeplot","nvd3"),showOutput("PCTcrimeplot","nvd3")
)))