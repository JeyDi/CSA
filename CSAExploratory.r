#CSA = Cluster Soccer Analysis
rm(list = ls())
cat("\014")

path <- dirname(parent.frame(2)$ofile)
setwd(path)

#call the utility.r file for usefull environment and dataset functions
source("utility.R")

environmentSettings()

FinalDatasetLight <- loadDataset(path, "FinalDatasetLight.csv")

FinalDatasetLight <- trasformDataset(FinalDatasetLight)

description <- datasetDescription(FinalDatasetLight)
print(description)

#Get the top 20 players
top20 <- 
  FinalDatasetLight %>% 
  arrange(desc(overall)) %>% 
  head(n = 20) %>%
  as.data.frame()


#Display the players tabel for export
top20 %>% 
  select(player_name, height, weight, overall, potential, strong_foot,interceptions,gk_diving,finishing) %>% 
  datatable(., options = list(pageLength = 10),caption=(h1="Best Players")) -> datatableTop20
DT::saveWidget(datatableTop20,file="dataTable.html")
print(top20)

#Overall score charts
Desc(FinalDatasetLight$overall, plotit = TRUE) %>% print()
Desc(top20$overall, plotit = TRUE) %>% print()

#Print Interactive correlation map
top20 %>% select(-strong_foot,-player_name,-birthday, -work_rate_att, -work_rate_def) -> top20NumericVariables
iplotCorr(top20NumericVariables, reorder=TRUE, rows=TRUE, cols=TRUE,scatterplot=TRUE) -> plotCorr
htmlwidgets::saveWidget(plotCorr,file="corrMatrix.html")
print(plotCorr)

#Print relationship between overall and other numeric measurement 
#Use shiny for the graphics
#get all the measure data labels
measures <- top20NumericVariables %>% 
  select(-overall) %>% 
  names()

#Create the radarchart to see a complete profile of a single player
#Data first had to be transformed to work with the radarchart package.
radarDF <- top20 %>%
  select(-id, -overall, -height, -weight,
         -potential,-quote) %>% as.data.frame()

radarDF <- gather(radarDF, key=Label, value=Score, -player_name) %>%
  spread(key=player_name, value=Score)

chartJSRadar(scores = radarDF, showToolTipLabel = TRUE, showLegend = TRUE, responsive=TRUE) -> radarPlot
htmlwidgets::saveWidget(radarPlot,file="radarPlot.html")
print(radarPlot)

#print the releationship plot

top20 %>% 
  ggvis(x = input_select(measures, label = "Choose the x-axis:", map = as.name)) %>% 
  layer_points(y = ~overall, fill = ~player_name)  -> dynamicViz

#ATTENTION!!! PRESS ESCAPE TO EXIT THE DYNAMIC PLOT!!
