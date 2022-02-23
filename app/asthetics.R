if (!require("plotly")) {
  install.packages("devtools")
  library(devtools)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("rgeos")) {
  install.packages("rgeos")
  library(rgeos)
}
if (!require("maptools")) {
  install.packages("maptools")
  library(maptools)
}
if (!require("ggalt")) {
  install.packages("ggalt")
  library(ggalt)
}
if (!require("ggthemes")) {
  install.packages("ggthemes")
  library(ggthemes)
}
if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}


# Prepare the zip poly data for US
mydata <- readOGR(dsn = "../data/cb_2016_us_zcta510_500k", layer = "cb_2016_us_zcta510_500k")

# Texas zip code data
zip <- read_csv("../data/zip_code_database.csv")
tx <- filter(zip, state == "TX")
ny <- filter(zip, state == "NY")

nyc_raw <- read_csv("https://raw.githubusercontent.com/erikgregorywebb/nyc-housing/master/Data/nyc-zip-codes.csv")

names(nyc_raw)[3] <- "zip"

nyc <- filter(ny, zip %in% nyc_raw$zip)

nyc <- merge(nyc, nyc_raw)


# Get polygon data for TX only
mypoly <- subset(mydata, ZCTA5CE10 %in% nyc$zip)

# Create a new group with the first three digit.
# Drop unnecessary factor levels.
# Add a fake numeric variable, which is used for coloring polygons later.

mypoly$group <- substr(mypoly$ZCTA5CE10, 1,5)
mypoly$ZCTA5CE10 <- as.factor(mypoly$ZCTA5CE10)
mypoly$ZCTA5CE10 <- droplevels(mypoly$ZCTA5CE10)

set.seed(111)
mypoly$value <- sample.int(n = 10000, size = nrow(mypoly), replace = TRUE)

# Merge polygons using the group variable
# Create a data frame for ggplot.
mypoly.union <- unionSpatialPolygons(mypoly, mypoly$group)

mymap <- fortify(mypoly.union)

# Check how polygons are like

plot(mypoly)
plot(mypoly.union, add = T, border = "red", lwd = 1)


# Convert SpatialPolygons to data frame and aggregate the fake values
mypoly.df <- as(mypoly, "data.frame") %>%
  group_by(group) %>%
  summarise(value = sum(value))



# Find a center point for each zip code area
centers <- data.frame(gCentroid(spgeom = mypoly.union, byid = TRUE))
centers$zip <- rownames(centers)


Film_Permits <- read_csv("../data/Film_Permits.csv")

film_permits_long <- Film_Permits %>%
  dplyr::rename(zip = `ZipCode(s)`) %>% 
  separate_rows(zip) 

film_permits_long$zip <- as.factor(film_permits_long$zip)
film_permits_long$StartDateTime <- mdy_hms(film_permits_long$StartDateTime) 

film_permits_count <- film_permits_long %>% 
  dplyr::count(zip) %>% 
  filter(zip != "0")



ggplot() +
  geom_cartogram(data = mymap, aes(x = long, y = lat, map_id = id), map = mymap) +
  geom_cartogram(data = film_permits_count, map = mymap, aes(fill = n, map_id = zip))+
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()
