library(rCharts)
library(googleVis)
load("CWF.rda")
load("citiesCrime.rda")

shinyServer(function(input, output) {
    
 output$chart1 <- renderChart({
 chart1=dPlot(x="StateUT", y=input$crimevar,data=CWF, type="bar",height=525,width=700,bounds = list(x=50, y=10, width=600, height=350)) 
  chart1$xAxis(type="addCategoryAxis", orderRule=input$crimevar)
  chart1$yAxis(type="addMeasureAxis", outputFormat="0f")
  chart1$addParams(dom = 'chart1')
  return(chart1)
  })
 output$gvisgeoplotstateut <- renderGvis({
   gvisGeoChart(CWF,locationvar="StateUT",colorvar=input$crimevar,
                options=list(region="IN",displayMode="region",resolution="provinces",
                             colorAxis="{colors:['blue','yellow','red']}",
                             width=700,height=600))
 })
 
 
 output$chart2 <- renderChart({
   cityplot=dPlot(x="City", y=input$crimevar,data=citiesCrime, type="bar",height=525,width=700,bounds = list(x=50, y=10, width=600, height=350)) 
   cityplot$xAxis(type="addCategoryAxis", orderRule=input$crimevar)
   cityplot$yAxis(type="addMeasureAxis",outputFormat="0f")
   cityplot$addParams(dom = 'chart2')
   return(cityplot)
 })
 
 
 output$gvisgeoplotcity <- renderGvis({
   gvisGeoChart(citiesCrime,locationvar="City",colorvar=input$crimevar, hovervar="City",
                options=list(region="IN",displayMode="markers",resolution="provinces",
                             colorAxis="{colors:['blue','yellow','red']}",
                             width=700,height=525)
   )
   
 })
 
 
 })
