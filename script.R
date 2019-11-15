library(raster)
library(rasterVis)
library(shar)
library(sp)
library(spData)
library(tidyverse)
library(sf)
library(spocc)
library(rbison)
library(maptools)
library(rgdal)
library(knitr)
library(mapview)
library(leaflet)
library(broom)
library(leaflet.extras)
library(shiny)
library(shinydashboard)



###Calling occurrence data and converting to df###

SLF<-occ("Lycorma delicatula", has_coords=TRUE, limit=1000000,)
SLFdf<-occ2df(SLF)

EAB<-occ("Agrilus planipennis", has_coords=TRUE, limit=1000000)
EABdf<-occ2df(EAB)

HWA<-occ("Adelges tsugae", has_coords=TRUE, limit=1000000)
HWAdf<-occ2df(HWA)

ALB<-occ("Anoplophora glabripennis", has_coords=TRUE, limit=1000000)
ALBdf<-occ2df(ALB)

SWW<-occ("Sirex noctilio", has_coords=TRUE, limit=1000000)
SWWdf<-occ2df(SWW)



###Leaflet Map###

map<-leaflet()%>%
  addProviderTiles("Esri.WorldPhysical")%>%
  addCircleMarkers(lng=SLFdf$longitude, lat=SLFdf$latitude, color="green", popup=SLFdf$date, group="Spotted Lanternfly", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=EABdf$longitude, lat=EABdf$latitude, color="blue", popup=EABdf$date, group="Emerald Ash Borer", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=HWAdf$longitude, lat=HWAdf$latitude, color="yellow", popup=HWAdf$date, group="Hemlock Wooly Adelgid", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=ALBdf$longitude, lat=ALBdf$latitude, color="orange", popup=ALBdf$date, group="Asian Long-horned Beetle", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=SWWdf$longitude, lat=SWWdf$latitude, color="red", popup=SWWdf$date, group="Sirex Woodwasp", fillOpacity=0.2, radius=2)%>%
  addLayersControl(overlayGroups = c("Spotted Lanternfly", "Emerald Ash Borer", "Hemlock Wooly Adelgid", "Asian Long-horned Beetle", "Sirex Woodwasp"))

map
