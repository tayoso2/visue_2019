
# load libraries

library(plyr)
library(data.table)
library(sp)
library(sf)
library(raster)
library(maptools)
library(leaflet)
library(tmap)
library(rgdal)
library(units)
library(stringr)
library(dplyr)
library(elevatr)
library(DescTools)
library(rnaturalearth)
library(RColorBrewer)




# drawn anticlockwise on kml generator

setwd("C:/Users/TOsosanya/Desktop/Electricity/Danish DNOs")
polylines <- read.csv("polylines.csv")
polylinesW <- polylines %>%  filter(Category == "West") %>% select(-(Category)) %>% 
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>%  st_union()
polylinesW <- st_cast(polylinesW, "POLYGON")
tmap_mode("view")
qtm(polylinesW, basemap = "Esri.WorldStreetMap") + tm_style("beaver")


polylinesE <- polylines %>%  filter(Category == "East") %>% select(-(Category)) %>% 
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326) %>%  st_union()
polylinesE <- st_cast(polylinesE, "POLYGON")
tmap_mode("view")
qtm(polylinesE, basemap = "Esri.WorldStreetMap") + tm_style("beaver") + qtm(polylinesW)


n1_60kv <- read.csv("N1/Output Data/60kV Transformers GIS 1807.csv")
n1_60kv <-  n1_60kv %>% st_as_sf(., coords = c("X", "Y"), crs = 4326)
qtm(n1_60kv, basemap = "Esri.WorldStreetMap") + tm_style("beaver")
intersect <- st_intersects(n1_60kv,polylinesW)
intersect <- as.data.frame(intersect)
n1_60kv$row.id <- seq.int(nrow(n1_60kv))

mc_col.id1 <- left_join(intersect, as.data.frame(n1_60kv), by = "row.id")
mc_col.id1 <-  mc_col.id1 %>% rename(Transformer_Coast_Side = col.id)
mc_col.id1$Transformer_Coast_Side <-  "West"
mc_col.id2 <- n1_60kv %>%  anti_join(mc_col.id1, by = "AssetID")
mc_col.id2$Transformer_Coast_Side <-  "East"
mc_col.id <-  rbind(mc_col.id1,as.data.frame(mc_col.id2)) %>% arrange(row.id) %>% select(-c(row.id))
fwrite(mc_col.id,"N1/Output Data/60kV Transformers GIS 1807.csv")

unlist_geom <- do.call(rbind, st_geometry(mc_col.id2)) %>% as_tibble() %>% setNames(c("X","Y"))



# calculate elevation
setwd("C:\\Users\\TOsosanya\\Desktop\\Electricity\\PGE\\tower")
ele1 <- read.csv("steel.asset.base.cleansed.csv")
summary(ele1)
ele_na <- ele1 %>% filter(is.na(Longitude))
ele <- ele1 %>% anti_join(ele_na, by = c("Longitude", "Latitude"))
ele <- st_as_sf(ele, coords = c("Longitude","Latitude"), crs = 4326)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Create and example data.frame with additional columns
# Create an example SpatialPointsDataFrame
elevate <- get_elev_point(ele, prj = prj_dd, src = "epqs")
elev8 <- data.frame(elevate)
elev8$Tower_Altitude <- elev8$elevation
elev8$elevation <- NULL
elev8$elev_units <- NULL

# merge both df based on geometry
ele_na$Longitude <- NULL
ele_na$Latitude <- NULL
elev8$geometry <- NULL

#ele_full$Tower_Altitude[which(is.na(ele_full$Tower_Altitude))] <- mean(ele_full$Tower_Altitude, na.rm = TRUE)
elev8$Tower_Altitude[which(is.na(elev8$Tower_Altitude))] <- 0
# add the long lat back using Asset_Number
elev8 <- inner_join(elev8, ele1[,c(4,7,8)], by = "Asset_Number")
elev8 <- st_as_sf(elev8, coords = c("Longitude","Latitude"), crs = 4326)



# bring in denmarks boundary
boundary <- st_read("C:/Users/TOsosanya/Desktop/Electricity/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
boundary <- boundary[,c("ADMIN")]
denmark <- boundary[boundary$ADMIN == "Denmark",]
denmark <- st_cast(denmark, "MULTILINESTRING")
FindNearest <- function(x, y, y.name = "y") {
  # Accepts two sf objects (x and y) and determines the nearest feature in y for
  # every feature in x. 
  #
  # Args:
  #   x: An sf object containing the features for which you want find the 
  #      nearest features in y
  #   y: An sf object containing features to be assigned to x
  #   y.name: Characters prepended to the y features in the returned datatable
  #
  # Returns:
  #   A datatable containing all rows from x with the corresponding nearest 
  #   feature from y. A column representing the distance between the features is
  #   also included. Note that this object contains no geometry.
  
  #browser()
  
  # Determine CRSs
  message(paste0("x Coordinate reference system is ESPG: ", st_crs(x)$epsg))
  message(paste0("y Coordinate reference system is ESPG: ", st_crs(y)$epsg))
  
  # Transform y CRS to x CRS if required
  if (st_crs(x) != st_crs(y)) {
    message(paste0(
      "Transforming y coordinate reference system to ESPG: ",
      st_crs(x)$epsg
    ))
    y <- st_transform(y, st_crs(x))
  }
  
  # Compute distance matrix
  dist.matrix <- st_distance(x, y)
  
  # Select y features which are shortest distance
  nearest.rows <- apply(dist.matrix, 1, which.min)
  # Determine shortest distances
  nearest.distance <-
    dist.matrix[cbind(seq(nearest.rows), nearest.rows)]
  
  # Report distance units
  distance.units <- deparse_unit(nearest.distance)
  message(paste0("Distance unit is: ", distance.units))
  
  # Build data table of nearest features
  nearest.features <- y[nearest.rows,]
  nearest.features$distance <- nearest.distance
  nearest.features$rowguid <- x$rowguid
  nearest.features$Reference.Number <- x$Reference.Number
  # Remove geometries
  st_geometry(x) <- NULL
  st_geometry(nearest.features) <- NULL
  
  # Prepend names to y columns
  names(nearest.features) <- paste0(y.name, ".", names(nearest.features))
  
  # Bind datatables and return
  #output <- cbind(x, nearest.features)
  output <- nearest.features
  return(output)
}

dist <- FindNearest(steel_1,denmark)
denmark <- st_cast(denmark, "MULTILINESTRING")
dist <- FindNearest(elev8,denmark)
dist <- inner_join(elev8, dist[,c(2:3)], by = c("Asset_Number" = "y.Asset_Number"))
ele_full <- rbind.fill(dist,ele_na)
ele_full$y.distance <- as.numeric(ele_full$y.distance)
ele_full <- ele_full %>%  mutate(y.distance = y.distance/1000)
ele_full$y.distance[which(is.na(ele_full$y.distance))] <- "default"
ele_full$Tower_Distance_From_Coast <- ele_full$y.distance
ele_full$Paintwork_Distance_From_Coast <- ele_full$Tower_Distance_From_Coast
ele_full$Foundation_Distance_From_Coast <- ele_full$Tower_Distance_From_Coast
ele_full$y.distance <- NULL
ele_full$y.ADMIN <- NULL
ele_full$y.Asset_Number <- NULL

ele_full$Paintwork_Corrosion_Index[which(is.na(ele_full$Paintwork_Corrosion_Index))] <- "default"
ele_full$Tower_Altitude[which(is.na(ele_full$Tower_Altitude))] <- "default"
ele_full$Paintwork_Altitude <- ele_full$Tower_Altitude
ele_full$Foundation_Altitude <- ele_full$Tower_Altitude
ele_full$Longitude <- NULL
ele_full$Latitude <- NULL
ele_full$geometry <- NULL

ele_full$Asset_Register_Category