library(spocc)
library(rbison)
library(maptools)
library(rgdal)
library(knitr)
library(mapview)
library(leaflet)
library(broom)
library(viridis)
library(leaflet.extras)
library(lubridate)
#library(revgeo)
library(spData)
library(htmlwidgets)
library(htmltools)
library(maptools)

#####US Bounding box#####

e1<-extent(c(-217.001953,-23.966176,20.478516,73.503461))
bbox1<-bbox(e1)

#####Not US Bounding Box#####
e2<-extent(c(xmin=-24.257813,xmax=189.843750,ymin=-47.279229,ymax=74.775843))
bbox2<-bbox(e2)


############Calling US occurrence data and converting into data frame###########
SLF<-occ("Lycorma delicatula", has_coords=TRUE, limit=1000000, geometry=bbox1)
SLFdf<-occ2df(SLF)%>%
  mutate(sname="Lycorma delicatula", cname="Spotted Lanternfly")

EAB<-occ("Agrilus planipennis", has_coords=TRUE, limit=1000000, geometry=bbox1)
EABdf<-occ2df(EAB)%>%
  mutate(sname="Agrilus planipennis", cname="Emerald Ash Borer")

HWA<-occ("Adelges tsugae", has_coords=TRUE, limit=1000000, geometry=bbox1)
HWAdf<-occ2df(HWA)%>%
  mutate(sname="Adelges tsugae", cname="Hemlock Wooly Adelgid")

ALB<-occ("Anoplophora glabripennis", has_coords=TRUE, limit=1000000, geometry=bbox1)
ALBdf<-occ2df(ALB)%>%
  mutate(sname="Anoplophera glabripennis", cname="Asian Long-horned Beetle")

SWW<-occ("Sirex noctilio", has_coords=TRUE, limit=1000000, geometry=bbox1)
SWWdf<-occ2df(SWW)%>%
  mutate(sname="Sirex Noctilio", cname="Sirex Woodwasp")

EFA<-occ("myrmica rubra", has_coords=TRUE, limit=1000000, geometry=bbox1)
EFAdf<-occ2df(EFA)%>%
  mutate(sname="Myrmica rubra", cname="European Fireant")

#####Calling non-US occurrences and converting to dataframes#####

SLFn<-occ("Lycorma delicatula", has_coords=TRUE, limit=1000000, geometry=bbox2)
SLFndf<-occ2df(SLFn)%>%
  mutate(sname="Lycorma delicatula", cname="Spotted Lanternfly", status="Native")

EAB2<-occ("Agrilus planipennis", has_coords=TRUE, limit=1000000, geometry=bbox2)
EAB2df<-occ2df(EAB2)%>%
  mutate(sname="Agrilus planipennis", cname="Emerald Ash Borer", status="Native")

HWAn<-occ("Adelges tsugae", has_coords=TRUE, limit=1000000, geometry=bbox2)
HWAndf<-occ2df(HWAn)%>%
  mutate(sname="Adelges tsugae", cname="Hemlock Wooly Adelgid", status="Native")

ALBn<-occ("Anoplophora glabripennis", has_coords=TRUE, limit=1000000, geometry=bbox2)
ALBndf<-occ2df(ALBn)%>%
  mutate(sname="Anoplophera glabripennis", cname="Asian Long-horned Beetle", status="Native")

SWWn<-occ("Sirex noctilio", has_coords=TRUE, limit=1000000, geometry=bbox2)
SWWndf<-occ2df(SWWn)%>%
  mutate(sname="Sirex Noctilio", cname="Sirex Woodwasp", status="Native")

EFAn<-occ("myrmica rubra", has_coords=TRUE, limit=1000000, geometry=bbox2)
EFAndf<-occ2df(EFAn)%>%
  mutate(sname="Myrmica rubra", cname="European Fireant", status="Native")


######Combining all invasives into one df#####
INVdf<-rbind(EABdf, HWAdf, SLFdf, ALBdf, SWWdf, EFAdf)
INVdfY<-mutate(INVdf,year=lubridate::year(INVdf$date))
INVdfYL<-mutate(INVdfY, location=revgeo(INVdfY$longitude, INVdfY$latitude))

INVdfYLsf<-st_as_sf(INVdfYL, coords=c("longitude", "latitude"), crs=3857)



#####Climate Data#####
data(world)

tmean <- getData(name = "worldclim", var="tmean", res=5)


crs(INVsp) <- CRS('+init=EPSG:4326')
crs(tmean) <- CRS('+init=EPSG:3857')

#proj4string(tmax_annual) <- CRS("+init=epsg:3857")

##Precip extract test

tmax_monthly <- getData(name = "worldclim", var="tmax", res=5)
gain(tmax_monthly)=0.1
tmax_annual<-max(tmax_monthly)
names(tmax_annual) <- "tmax"
CountryMaxima<-tmax_annual %>%
  raster::extract(INVsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

precip <- getData(name = "worldclim", var="prec", res=5)
gain(precip)=0.1
prec_annual<-max(precip)
names(prec_annual) <- "pmax"
PMaxima<-prec_annual
PrecEx<-raster::extract(prec_annual, INVsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE, crs=epsg:3857)%>%
  st_as_sf()

mutate(INVsp, pmax=PrecEx, by="geometry")

master2<-st_join(INVsf, PrecEx)

master<-mutate(INVsf, pmax=(raster::extract(prec_annual,
                                            SpatialPoints((cbind(INVdfYL$longitude, INVdfYL$latitude)),
                                                          fun=max, na.rm=TRUE, small=TRUE, sp=TRUE))))

INVsf<-st_as_sf(INVsp)


INVsp<-SpatialPoints(cbind(INVdfYL$longitude, INVdfYL$latitude, INVdfYL$cname))


tmaxex<-raster::extract(tmax_annual, c(INVdfYL$longitude, INVdfYL$latitude), fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)
tmaxexsf<-st_as_sf(tmaxex)


View(tmaxex)

tmaxexdf<-as.data.frame(tmaxex)

ggplot(PrecEx, aes())+
  geom_point()+
  ggtitle("Annual Maximum Temperature by Country")

INVprec<-extract(precip, INVdfYLsf, method=simple)




#####Extracting Environmental Space per Species#####

#Emerald Ash Borer
EABsp<-SpatialPoints(cbind(EABdf$longitude, EABdf$latitude))
EABprec<-raster::extract(prec_annual, EABsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

EABtmax<-raster::extract(tmax_annual, EABsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

EABUSclim<-st_join(EABprec, EABtmax)%>%
  mutate(status="Invasive")

EAB2sp<-SpatialPoints(cbind(EAB2df$longitude, EAB2df$latitude))
EAB2tmax<-raster::extract(tmax_annual, EAB2sp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()
EAB2prec<-raster::extract(prec_annual, EAB2sp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

EABnUSclim<-st_join(EAB2prec,EAB2tmax)%>%
  mutate(status="Native")

EABmaster<-rbind(EABnUSclim, EABUSclim)

ggplot(data=EABmaster, aes(x=tmax, y=pmax))+
  geom_jitter(aes(fill=status, color=status, alpha=0.05))

#Spotted Lanternfly
SLFsp<-SpatialPoints(cbind(SLFdf$longitude, SLFdf$latitude))
SLFprec<-raster::extract(prec_annual, SLFsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

SLFtmax<-raster::extract(tmax_annual, SLFsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

SLFUSclim<-st_join(SLFprec, SLFtmax)%>%
  mutate(status="Invasive")

SLFnsp<-SpatialPoints(cbind(SLFndf$longitude, SLFndf$latitude))
SLFntmax<-raster::extract(tmax_annual, SLFnsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()
SLFnprec<-raster::extract(prec_annual, SLFnsp, fun=max, na.rm=TRUE, small=TRUE, sp=TRUE)%>%
  st_as_sf()

SLFnUSclim<-st_join(SLFnprec,SLFntmax)%>%
  mutate(status="Native")

SLFmaster<-rbind(SLFnUSclim, SLFUSclim)


#####Plots, plots, plots!#####
ggplot(data=SLFmaster, aes(x=tmax, y=pmax))+
  geom_jitter(aes(fill=status, color=status, alpha=0.05))


###Adding geometry to INV df

mutate(INVdfYL, geometry=st_point(c(longitude, latitude)))



###dumbtest#####

matched_geom<-st_join(prec_annual, INVdfYLsf)

over.test<-over(INVdfYLsf$geometry, prec_annual$pmax)


#####Leaflet Palettes#####

#Marker Palette
pal <- colorFactor(palette = "Set1", domain = INVdfY$cname)

#Test palette
pal2 <- colorBin(c("#0C2C84", "#41B6C4", "#FFFFCC"), domain = c(0, 200),
                 na.color = "#0000000")

#Temperature Palette
tpal

#Precipitation Palette
ppal<- colorNumeric(palette="Blues", na.color = "transparent", domain=values(precleaflet))


#####New Leaflet######

combinedmap<-leaflet(INVdfY)%>%
  addTiles()%>%
  addCircleMarkers(lng=INVdfY$longitude, lat=INVdfY$latitude,
                   color = ~pal(cname), popup=INVdfY$date, fillOpacity=0.2, radius=2, group=INVdfY$year)%>%
  addLegend("bottomleft", pal = pal, values = ~cname,
            title = "Invasive Insects", opacity = 1)%>%
  #addLayersControl(overlayGroups = INVdfY$year,
  # options = layersControlOptions(collapsed = FALSE))%>%
  addSearchOSM()%>%
  addRasterImage(precleaflet, colors="Spectral", project=FALSE)




combinedmap

map2<-leaflet(INVdfY)%>%
  addTiles()%>%
  addRasterImage(precleaflet, colors="Spectral", opacity=0.5)%>%
  add
addCircleMarkers(lng=INVdfY$longitude, lat=INVdfY$latitude,
                 color = ~pal(cname), popup=INVdfY$date, fillOpacity=0.2, radius=2, options = pathOptions(pane = "ames_circles"))


#####ggmap#####

p <- ggmap(get_map(location= "United States",
                   zoom = 3, scale = 2,
                   maptype ="terrain"))

p + geom_point(data=INVdfY, aes(x=longitude, y =latitude, color=cname), size = 0.5) +
  theme(legend.position="bottom")+
  geom_raster(as.raster(prec_annual), aes())

plot(prec_annual)




########
panetest

precleaflet<-projectRasterForLeaflet(prec_annual, method="bilinear")

crs(PrecEx)<-CRS('+init=EPSG:3857')

###############Leaflet Map##############

map<-leaflet()%>%
  addProviderTiles("Esri.WorldPhysical")%>%
  addCircleMarkers(lng=SLFdf$longitude, lat=SLFdf$latitude, color="green", popup=SLFdf$date, group="Spotted Lanternfly", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=EABdf$longitude, lat=EABdf$latitude, color="blue", popup=EABdf$date, group="Emerald Ash Borer", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=HWAdf$longitude, lat=HWAdf$latitude, color="yellow", popup=HWAdf$date, group="Hemlock Wooly Adelgid", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=ALBdf$longitude, lat=ALBdf$latitude, color="orange", popup=ALBdf$date, group="Asian Long-horned Beetle", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=SWWdf$longitude, lat=SWWdf$latitude, color="red", popup=SWWdf$date, group="Sirex Woodwasp", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=CFFdf$longitude, lat=CFFdf$latitude, color="purple", popup=CFFdf$date, group="Cherry Fruit Fly", fillOpacity=0.2, radius=2)%>%
  #addCircleMarkers(lng=IFAdf$longitude, lat=IFAdf$latitude, color="black", popup=IFAdf$date, group="Imported Fire Ant", fillOpacity=0.2, radius=2)%>%
  addCircleMarkers(lng=EFAdf$longitude, lat=EFAdf$latitude, color="brown", popup=EFAdf$date, group="Imported Fire Ant", fillOpacity=0.2, radius=2)%>%
  addLayersControl(overlayGroups = c("Spotted Lanternfly", "Emerald Ash Borer", "Hemlock Wooly Adelgid", "Asian Long-horned Beetle", "Sirex Woodwasp", "Cherry Fruit Fly", "Imported European Fire Ant"))%>%
  addSearchOSM()

map

####Date of First Occurrence in US####

FirstUSOcc<-INVdfYL %>%
  select(date, cname, sname, location)%>%
  group_by(sname)%>%
  top_n(-1, date)%>%
  arrange(date)



FirstUSOcc



###lulc###
lulc()