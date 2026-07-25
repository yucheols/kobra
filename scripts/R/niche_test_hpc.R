#####  analyses of niche overlap / divergence between N. kaouthia and N. fuxi

# clean up working env
rm(list = ls(all.names = T))
gc()

# load packages
library(terra)
library(sf)
library(dplyr)
library(ecospat)
library(ENMTools)

# set seed
set.seed(1111)

#### import occurrences
# Naja kaouthia
naka <- read.csv('/home/yshin/mendel-nas1/kobra/data/occs_thin/Naja_kaouthia.csv') %>% 
  dplyr::select(-1) %>% terra::vect(geom = c('long', 'lat'), crs = 'EPSG:4326', keepgeom = T)

# Naja fuxi
nafu <- read.csv('/home/yshin/mendel-nas1/kobra/data/occs_thin/Naja_fuxi.csv') %>% 
  dplyr::select(-1) %>% terra::vect(geom = c('long', 'lat'), crs = 'EPSG:4326', keepgeom = T)

head(naka)
head(nafu)

#### import MCPs for each sp. to define species-specific background
## import MCP
naka.mcp <- st_read('/home/yshin/mendel-nas1/kobra/mcp/Naja_kaouthia_mcp.shp')
nafu.mcp <- st_read('/home/yshin/mendel-nas1/kobra/mcp/Naja_fuxi_mcp.shp')

#### import envs
# climate
clim <- rast(list.files(path = '/home/yshin/mendel-nas1/kobra/data/envs/clipped/', pattern = '.tif', full.names = T))
clim <- clim[[c('bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19')]]
print(clim)

# divide temperature layers by 10
clim[[c('bio1', 'bio2', 'bio3')]] <- clim[[c('bio1', 'bio2', 'bio3')]] / 10
print(clim)


##### use ENMTools to run ecospat identity test and ecospat background test
## define range
sp1.range <- mask(clim, naka.mcp)
sp2.range <- mask(clim, nafu.mcp)

## create enmtools.species objects
kaouthia <- enmtools.species(range = sp1.range[[1]], presence.points = naka, species.name = 'Naja_kaouthia')
fuxi <- enmtools.species(range = sp2.range[[1]], presence.points = nafu, species.name = 'Naja_fuxi')

#### niche equivalency test
niche.eq <- enmtools.ecospat.id(species.1 = kaouthia, species.2 = fuxi, env = clim, nreps = 1000, R = 1000, bg.source = 'range', verbose = T)
print(niche.eq)

#### niche similarity test
# dir 1 == randomize N. fuxi within its background
niche.sim.1 <- enmtools.ecospat.bg(species.1 = kaouthia, species.2 = fuxi, env = clim, 
                                   nreps = 1000, test.type = 'asymmetric', R = 100, bg.source = 'range', verbose = T)

print(niche.sim.1)

# dir 2 == randomize N. kaouthia within its background
niche.sim.2 <- enmtools.ecospat.bg(species.1 = fuxi, species.2 = kaouthia, env = clim, 
                                   nreps = 1000, test.type = 'asymmetric', R = 100, bg.source = 'range', verbose = T)

print(niche.sim.2)


#### export output as RDS file
saveRDS(niche.eq, '/home/yshin/mendel-nas1/kobra/output/niche_eq.rds')
saveRDS(niche.sim.1, '/home/yshin/mendel-nas1/kobra/output/niche_sim1_1.rds')
saveRDS(niche.sim.2, '/home/yshin/mendel-nas1/kobra/output/niche_sim1_2.rds')
