######## Source of Data: http://ncrb.nic.in/CD-CII2012/Additional_Tables_CII_2012/Additional%20table%202012/CAWomen-CH-2001-2012.xls and http://ncrb.nic.in/CD-CII2012/cii-2012/Table%205.2.pdf
########## Preprocessing - Modified column names, made a few replacements ----- & to and, Odisha to Orissa, Delhi UT to Delhi, DandN Haveli to Dadra and Nagar Haveli, 
########## Puducherry to Pondicherry, AandN Islands to Andaman and Nicobar Islands, removed rows for total crimes in Union Territories and total crimes in states but retained the grand total for both,
########## Few of the above changes done based on previous work with googleVis geoCharts in the Indian context
############ Inserted a column for year
#R_LIBS="C:/Users/patil/Documents/R/win-library"

library(rCharts)
library(reshape2)
library(plyr)
library(scales)

CWFull=read.csv("CAWomen.csv")
citiesCrime=read.csv("citiesCrime.csv")

# Change in total crimes over years for Different states, union territories, overall India

TotCrimesmelt=melt(CWFull,id=c("Year","StateUT"))
save(TotCrimesmelt,file="TotCrimesmelt.rda")
#Saved file used in shiny app "TotCrime" in the last section of blog post for getting insights on States and UTs.
# Let's look at the Total Crimes in India over years separately here

TCrimeplot=nPlot(value~Year, group="variable", data=TotCrimesmelt[which(TotCrimesmelt$StateUT=="TOTAL"),], type="lineWithFocusChart", 
                 height=450,width=750)
TCrimeplot
TCrimeplot$save("TCrimeplot.html",cdn=T)
#TCrimeplot$publish("TCrimeplot",host='gist')

########################### % of crimes over years
PCTYears=CWFull
PCTYears$RapePercent=PCTYears$Rape*100/PCTYears$TotalCrimes
PCTYears$KidnappingAndAbductionPercent=PCTYears$KidnappingAndAbduction*100/PCTYears$TotalCrimes
PCTYears$DowryDeathsPercent =PCTYears$DowryDeaths*100/PCTYears$TotalCrimes
PCTYears$AssaultWithIntentToOutrageModestyPercent=PCTYears$AssaultWithIntentToOutrageModesty*100/PCTYears$TotalCrimes
PCTYears$InsultToModestyPercent=PCTYears$InsultToModesty*100/PCTYears$TotalCrimes
PCTYears$CrueltyByHusbandOrHisRelativesPercent=PCTYears$CrueltyByHusbandOrHisRelatives *100/PCTYears$TotalCrimes
PCTYears$ImportationOfGirlsFromForeignCountryPercent=PCTYears$ImportationOfGirlsFromForeignCountry*100/PCTYears$TotalCrimes
PCTYears$ImmoralTrafficPActPercent=PCTYears$ImmoralTrafficPAct*100/PCTYears$TotalCrimes
PCTYears$DowryProhibitionActPercent=PCTYears$DowryProhibitionAct*100/PCTYears$TotalCrimes
PCTYears$IndecentRepresentationOfWomenPActPercent=PCTYears$IndecentRepresentationOfWomenPAct*100/PCTYears$TotalCrimes
PCTYears$CommissionOfSatiPreventionActPercent=PCTYears$CommissionOfSatiPreventionAct*100/PCTYears$TotalCrimes
PCTYears$TotalCrimesPercent=PCTYears$TotalCrimes*100/PCTYears$TotalCrimes

onlypct=PCTYears[c(1,15:25,14)]
onlyraw=CWFull[c(1:12,14)]
onlypctmelt=melt (onlypct,id=c("Year","StateUT"))
onlyrawmelt=melt(onlyraw,id=c("Year","StateUT"))
names(onlypctmelt)=c("v1","v2","v3","percval")
meltpcdata=cbind(onlyrawmelt,onlypctmelt$percval)
names(meltpcdata)=c("Year","StateUT","variable","value","percval")
meltpcdata$percval=paste(round(meltpcdata$percval,digits=2),"% of Total Crimes for the year")

save(meltpcdata,file="meltpcdata.rda")
TCrimeplot1=nPlot(value~Year, group="variable", data=meltpcdata[which(meltpcdata$StateUT=="TOTAL"),], type="stackedAreaChart", 
                  height=600,width=750)
TCrimeplot1$chart(tooltip = "#! function(key, x, y, e, graph) {
  return '<h3>' + key + '</h3>' +
    '<p>' +  y + ' on ' + x + '</p>' +
    '<p>' + e.point.percval + '</p>'
}!#")
TCrimeplot1$save("TCrimeplot1.html",cdn=T)
TCrimeplot1

#############################################

# Focus on 2012 data only now and remove the row with Total values and columns for Total Crimes and  Year 
CW2012=CWFull[which(CWFull$Year==2012),]
CW2012=CW2012[which(CW2012$StateUT!="TOTAL"),]
CW2012=CW2012[c(-13,-14)]
# Since all values under the Commision of Sati Prevention Act variable are 0, let's drop that column as well

CW2012=CW2012[c(-12)]

# Correlation matrix of variables
corrmatrix<-cor(CW2012[c(-1)]) #store corr matrix
corrdata=as.data.frame(corrmatrix)
corrdata$Variable1=names(corrdata)
corrdatamelt=melt(corrdata,id="Variable1")
names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
corrmatplot$addParams(height = 450, width=1000)
corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
corrmatplot$guides(x = list(numticks = 3))
corrmatplot$addParams(staggerLabels=TRUE)
corrmatplot$save("corrmatplotstate.html",cdn=T)
corrmatplot


# heatmap of variables and State UTs
stateutmelt=ddply(melt(CW2012),.(variable),transform,rescale=rescale(value))
names(stateutmelt)=c("StateUT","Crime","value","rescale")
hmap <- rPlot(StateUT ~ Crime, color = 'rescale', data = stateutmelt, type = 'tile')
hmap$addParams(height = 600, width=1000)
hmap$guides(reduceXTicks = FALSE)
hmap$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
hmap$guides(y = list(numticks = length(unique(stateutmelt$StateUT))))
hmap$guides(x = list(numticks = 3))
hmap$save("heatmapstate.html",cdn=T)
hmap

################ Clustering (Quick reference: Quick-R, Kabacoff, http://www.statmethods.net/advstats/cluster.html)
set.seed(123)
kmeansdata=kmeans(CW2012[c(-1)],5)  # Decided on 5 for interpretation
# get cluster means 
meanvars=aggregate(CW2012[c(-1)],by=list(kmeansdata$cluster),FUN=mean)
# append cluster assignment
CW2012clust <- data.frame(CW2012, kmeansdata$cluster)

# plotting states/uts by cluster number
stategpplot=dPlot(x="StateUT", y="kmeansdata.cluster",groups="kmeansdata.cluster",data=CW2012clust,
                  type="bar",height=475,width=700,bounds = list(x=50, y=10, width=600, height=300))
stategpplot$yAxis(type="addCategoryAxis")
stategpplot$xAxis(type="addCategoryAxis",orderRule="kmeansdata.cluster")
stategpplot$save("stategpplot.html",cdn=T)
stategpplot

############## Parallel Plot#############
names(meanvars)=c("Group","Rape","KidnapAbduct","DowryDeath","AssaultModesty","InsultModesty","CrueltyHusband",
                  "Importation","ImmoralTraffic","DowryProhibit","IndecentRep")
parrstateut <- rCharts$new()
parrstateut$field('lib', 'parcoords')
parrstateut$set(padding = list(top = 25, left = 5, bottom = 10, right = 0), width=1080, height=400)
parrstateut$set(data = toJSONArray(meanvars, json = F), 
                colorby = 'Rape', 
                range = range(meanvars$Rape),
                colors = c('red','green')
)
parrstateut$setLib("parcoords")
parrstateut$save("parallelplotstate.html", cdn=T)
parrstateut



###### On to Cities

# Correlation matrix of variables
corrmatrix<-cor(citiesCrime[c(-1)]) #store corr matrix
corrdata=as.data.frame(corrmatrix)
corrdata$Variable1=names(corrdata)
corrdatamelt=melt(corrdata,id="Variable1")
names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
citiescorrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile', height = 600)
citiescorrmatplot$addParams(height = 450, width=1000)
citiescorrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
citiescorrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
citiescorrmatplot$guides(x = list(numticks = 3))
citiescorrmatplot$addParams(staggerLabels=TRUE)
citiescorrmatplot$save("corrmatplotcities.html",cdn=T)
citiescorrmatplot


# heatmap of variables and Cities
citiesmelt=ddply(melt(citiesCrime),.(variable),transform,rescale=rescale(value))
names(citiesmelt)=c("City","Crime","value","rescale")
hmapcity <- rPlot(City ~ Crime, color = 'rescale', data = citiesmelt, type = 'tile')
hmapcity$addParams(height = 600, width=1000)
hmapcity$guides(reduceXTicks = FALSE)
hmapcity$guides("{color: {scale: {type: gradient, lower: white, upper: red}}}")
hmapcity$guides(y = list(numticks = length(unique(citiesmelt$City))))
hmapcity$guides(x = list(numticks = 3))
hmapcity$save("heatmapcity.html",cdn=T)
hmapcity

################ Clustering (Quick reference: Quick-R, Kabacoff, http://www.statmethods.net/advstats/cluster.html)
set.seed(123)
kmeansdata=kmeans(citiesCrime[c(-1)],5)  # Decided on 5  - interpretation purposes
# get cluster means 
meanvars=aggregate(citiesCrime[c(-1)],by=list(kmeansdata$cluster),FUN=mean)
# append cluster assignment
citiesclust <- data.frame(citiesCrime, kmeansdata$cluster)

# plotting cities by cluster number
citiesgpplot=dPlot(x="City", y="kmeansdata.cluster",groups="kmeansdata.cluster",data=citiesclust,
                  type="bar", height=475,width=700,bounds = list(x=50, y=10, width=600, height=300))
citiesgpplot$yAxis(type="addCategoryAxis")
citiesgpplot$xAxis(type="addCategoryAxis",orderRule="kmeansdata.cluster")
citiesgpplot$save("citiesgpplot.html",cdn=T)
citiesgpplot

############## Parallel Plot#############
names(meanvars)=c("Group","Rape","KidnapAbduct","DowryDeath","AssaultModesty","InsultModesty","CrueltyHusband",
                  "Importation","ImmoralTraffic","DowryProhibit","IndecentRep")
parrcity <- rCharts$new()
parrcity$field('lib', 'parcoords')
parrcity$set(padding = list(top = 25, left = 5, bottom = 10, right = 0), width=1080, height=400)
parrcity$set(data = toJSONArray(meanvars, json = F), 
                colorby = 'Rape', 
                range = range(meanvars$Rape),
                colors = c('red','green')
)
parrcity$setLib("parcoords")
parrcity$save("parrcity.html",cdn=T)
parrcity


##########
# 2012 data for shiny app to plot different states/union territories/cities geographically for every crime type and year - App: indiacrimeshiny
###########
CWF=CWFull[which(CWFull$StateUT!="TOTAL"),]
CWF=CWF[which(CWF$Year==2012),]
CWF=CWF[c(-13)] # Don't need the Total Crimes Column 
save(CWF,file="CWF.rda")

# Data for Cities
save(citiesCrime,file="citiesCrime.rda")

################################## Done #######################