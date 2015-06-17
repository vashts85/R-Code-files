install.packages('ggmap')
library(ggmap)

loc <- 'Environmental Protection Agency, 1 Sabine Drive, Gulf Breeze, FL'
my_map <- get_map(
  location = loc, 
  source = 'google', 
  maptype = 'roadmap', 
  zoom = 13
)
ggmap(my_map, extent = 'panel')

pts <- data.frame(
  lon = c(-87.1930, -87.2050, -87.1571),
  lat = c(30.3473, 30.3406, 30.3380),
  lab = c('Site 1', 'Site 2', 'Home')
)
pts
ggmap(my_map, extent = 'panel',
      base_layer = ggplot(pts, aes(x = lon, y = lat))) +
  geom_text(aes(label = lab))