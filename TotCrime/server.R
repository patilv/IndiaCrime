shinyServer(function(input, output) {
  load("TotCrimesmelt.rda")
  load("meltpcdata.rda")
  
  output$TCrimeplot <- renderChart({
    TCrimeplot=nPlot(value~Year, group="variable", data=TotCrimesmelt[which(TotCrimesmelt$StateUT==input$stateut),], 
                     type="lineWithFocusChart",height=450,width=750)
    TCrimeplot$addParams(dom = 'TCrimeplot')
    return(TCrimeplot)
  })
  
  
  output$PCTcrimeplot <- renderChart({
    PCTcrimeplot=nPlot(value~Year, group="variable", data=meltpcdata[which(meltpcdata$StateUT==input$stateut),], 
                       type="stackedAreaChart", 
                       height=600,width=750)
    PCTcrimeplot$chart(tooltip = "#! function(key, x, y, e, graph) {
  return '<h3>' + key + '</h3>' +
    '<p>' +  y + ' on ' + x + '</p>' +
    '<p>' + e.point.percval + '</p>'
}!#")
      PCTcrimeplot$addParams(dom = 'PCTcrimeplot')
    return(PCTcrimeplot)
  })
  
})
