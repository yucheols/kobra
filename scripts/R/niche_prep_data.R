#####  data prep for niche overlap analyses

# clean up working env
rm(list = ls(all.names = T))
gc()
 
# load packages
library(terra)
library(sf)
library(ConR)
library(dplyr)


##### load environmental layers
clim <- rast(list.files(path = 'data/envs/clipped/', pattern = '.tif', full.names = T))
clim <- clim[[c('bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19')]]
print(clim)

plot(clim[[1]])


#####  load occurrence points
# Naja kaouthia
naka <- read.csv('data/occs_thin/Naja_kaouthia.csv') %>% dplyr::select(-1) %>% dplyr::select(2,1)
naka$species = 'Naja_kaouthia'
head(naka)


# Naja fuxi + Vietnam points
nafu_viet <- read.csv('data/occs_thin/Naja_fuxi_viet.csv') %>% dplyr::select(-1) %>% dplyr::select(2,1)
nafu_viet$species = 'Naja_fuxi_viet'
head(nafu_viet)


#####  Naja kaouthia mcp
# make mcp
naka_mcp <- EOO.computing(naka, method.range = 'convex.hull', show_progress = T, export_shp = T, write_shp = T)

# put 100km buffer around it
naka_mcp <- st_read('shapesIUCN/EOO_poly.shp') %>% st_transform(crs = 3035) %>% st_buffer(100000) %>% st_transform(crs = 4326)
plot(naka_mcp, add = T, col = 'blue')

# export
st_write(naka_mcp, 'mcp/Naja_kaouthia_mcp.shp')


#####  Naja fuxi mcp
# make mcp
nafu_mcp <- EOO.computing(nafu_viet, method.range = 'convex.hull', show_progress = T, export_shp = T, write_shp = T)

# put 100km buffer around it
nafu_mcp <- st_read('shapesIUCN/EOO_poly.shp') %>% st_transform(crs = 3035) %>% st_buffer(100000) %>% st_transform(crs = 4326)
plot(nafu_mcp, add = T, col = 'purple')

# export
st_write(nafu_mcp, 'mcp/Naja_fuxi_mcp.shp')


#####  "global" mcp
# make mcp
glob <- rbind(naka, nafu_viet)
glob$species = 'glob'
glob_mcp <- EOO.computing(glob, method.range = 'convex.hull', show_progress = T, export_shp = T, write_shp = T)

# put 100km buffer around it
glob_mcp <- st_read('shapesIUCN/EOO_poly.shp') %>% st_transform(crs = 3035) %>% st_buffer(100000) %>% st_transform(crs = 4326)
plot(glob_mcp, add = T, col = 'green')
