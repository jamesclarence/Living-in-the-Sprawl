setwd("~/Documents/Blog/Living-in-the-Sprawl/Nashville Building")

nash <- read.csv("Nashville_Building_Permits.csv")

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

ggplot(nash,aes(x=Permit.Type.Description, fill=Permit.Type.Description)) +
    geom_bar(stat="bin")

### New Residential Building Permits
resnew <- filter(nash, Permit.Type.Description=="BUILDING RESIDENTIAL - NEW")

## Getting Latitude and Longitude of New Residential Building Permits
latlong <- colsplit(resnew$Mapped.Location, "\n", c("Street", "CityStZip","LatLon"))
latlong$LatLon <-gsub("\\(","",latlong$LatLon)
latlong$LatLon <- gsub("\\)","",latlong$LatLon)
latlong$LatLon <- gsub("[[:space:]]","", latlong$LatLon)
latlong2 <- colsplit(latlong$LatLon, ",", c("Lat","Lon"))

## Bind latlong2 back into resnew object
resnew <- cbind(resnew, latlong2)

# Row 1544 has an error
# Zip code is erroneously 27209; should be 37209
geocode("605 CENTERPOINT LN NASHVILLE,TN 37209")
# New Lat long is: Lat (36.31008); Lon (-86.66595)

# Replace resnew$Lat and resnew$Lon with new Lat and Lon
resnew$Lat <- replace(resnew$Lat,resnew$Lat==35.35510311100046,36.31008)
resnew$Lon <- replace(resnew$Lon,resnew$Lon==-79.76233604199967,-86.66595)

### Getting a Map and Setting it as a ggplot object
geocode("Nashville, TN")
geocode("Davidson County, TN")

# Nashville, Zoom = 10
Nash10 <- get_map(location = c(lon = -86.78333, lat = 36.16667),
                  color = "color",
                  source = "google",
                  maptype = "roadmap",
                  zoom = 10)

Nash10 <- ggmap(Nash10, extent = "device", legend = "topleft")

Nash10 +
    stat_density2d(aes(x=Lon, y=Lat,fill = ..level..,alpha = ..level..),
                   geom = "polygon", data=resnew) +
    guides(fill=F, alpha=F)

# Nashville, Zoom = 12
Nash12 <- get_map(location = c(lon = -86.78333, lat = 36.16667),
                  color = "color",
                  source = "google",
                  maptype = "roadmap",
                  zoom = 12)

Nash12 <- ggmap(Nash12, extent = "device", legend = "topleft")

Nash12 +
    stat_density2d(aes(x=Lon, y=Lat,fill = ..level..,alpha = ..level..),
                   geom = "polygon", data=resnew) +
    guides(fill=F, alpha=F)

# East Nashville
geocode("East Nashville, TN")
#  lon      lat
# -86.75972 36.17256
 # Five Points: 36.177566, -86.749935

East <- get_map(location = c(lon = -86.749935, lat = 36.177566),
                  color = "color",
                  source = "google",
                  maptype = "roadmap",
                  zoom = 13)

East <- ggmap(East, extent = "device")

East +
    stat_density2d(aes(x=Lon, y=Lat,fill = ..level..,alpha = ..level..),
                   geom = "polygon", data=resnew) +
    guides(fill=F, alpha=F)

# New Commercial Building Permits
comnew <- filter(nash, Permit.Type.Description=="BUILDING COMMERCIAL - NEW")

