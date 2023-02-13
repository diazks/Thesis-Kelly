setwd("E:/Kelly/VUB/Thesis/Temperature")

install.packages("ncdf4")
install.packages("raster")
install.packages("rgdal")
install.packages("sf")
install.packages("terra")
#install.packages("gdalUtils")


library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)         # process GIS files
library(terra)      # process raster (quicker)
library(dplyr)  # process dataframe
library(lubridate)  # process date time 
#library(gdalUtils)  # process raster

#study area
study_area <- read_sf("FAO_AREAS/FAO_AREAS.shp")
study_area <- study_area %>% filter(ID %in% c(214,248))

#get the info from the nc file
data_bst <- nc_open("temp_monthly_1961_2010.nc")
# get all times------------------------
tunits <- ncatt_get(data_bst, "time", "units") # get time unit
tunits$value # months since 1901-1-1 00:00:00
times <- ncvar_get(data_bst,"time") #the same for past and future data
list_month <- as.Date('1901-01-01') + months(as.integer(times))
#############################3

tob.array <- ncvar_get(data_bst, "tob") # store the data in a 3-dimensional array
dim(tob.array) #1440 x 720 grid cells over 600 months

fillvalue <- ncatt_get(data_bst, "tob", "_FillValue")
fillvalue #1e+20 is fill value


#replace all fill values with the R-standard 'NA'.
tob.array[tob.array == fillvalue$value] <- NA
area_final <- data.frame()

for(i in 1:length(times)){
  #getting info of 1 month
  tob.slice <- tob.array[, ,i]
  #save data in a raster
  r.test <- raster(t(tob.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  raster.area <- crop(r.test,study_area)
  mask.data <- mask(raster.area, study_area)
  area.data <- as(mask.data, "SpatialPixelsDataFrame") #convertir a spatial pixel data frame para luego convertir a data frame, y aplicar sumar por anio
  area_df <- as.data.frame(area.data)
  area_time <- data.frame(area_df, time = list_month[i], month = format(list_month[i],'%m'), year = format(list_month[i],'%Y'))
  area_final <- rbind(area_final,area_time)
  }
write.csv(area_final, "data.kelly.csv")

mean.temp.year <- area_final %>% group_by(x,y,year) %>% summarise(mean.temp = mean(layer))

mean.temp.year %>% filter(year == "2008") %>%
  ggplot()+
  geom_sf(data = study_area)+
  geom_tile(aes(fill = mean.temp, x = x, y = y))
  
mean.temp.year %>% filter(year %in% c("2001","2002","2004","2008")) %>%
  ggplot()+
  geom_sf(data = study_area)+
  geom_tile(aes(fill = mean.temp, x = x, y = y))+
  facet_grid(.~ year)


## temp tru years, same but group by year only


#plot month 1
plot(raster.mask)
plot(study_area$geometry, add = T)


#save data to a GeoTIFF file.
writeRaster(r.test, "Temp_Month1.tif", "GTiff", overwrite=TRUE)


#convert entire 3d array of data to a raster brick
r_brick_test <- brick(tob.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# may have to play around with the transpose (the t() function) and flip() before the data are oriented correctly. In this example, the netcdf file recorded latitude on the X and longitude on the Y, so both a transpose and a flip in the y direction were required.
# r_brick_test <- flip(t(r_brick), direction='y')

#specific place
toolik_lon <- -3.394
toolik_lat <- 46.657
toolik_series <- extract(r_brick_test, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')


#time series
toolik_df <- data.frame(time= seq(from=1982, to=2012, by=1), tob=t(toolik_series))
ggplot(data=toolik_df, aes(x=year, y=NDVI, group=1)) +
  geom_line() + # make this a line plot
  ggtitle("Growing season NDVI at Toolik Lake Station") +     # Set title
  theme_bw() # use the black and white theme
