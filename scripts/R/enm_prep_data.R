########  naja enm data processing

# clean up working env
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMwrap)
library(geodata)
library(dplyr)
library(terra)
library(ntbox)


# set seed
set.seed(123)


####  set clipping extent
ext <- c(72.999828366, 111.166822309, -0.122111598, 34.637115029)


####  get environmental data
#worldclim_global(var = 'bio', res = 2.5, path = 'data/envs')


####  climate data
# load == WorldClim v2.1 == 2.5 arcmin resolution == also clip to extent
clim <- rast(list.files(path = 'D:/env layers/wc2-5/', pattern = '.bil$', full.names = T))
clim <- crop(clim, ext)
plot(clim[[1]])

# export clipped layers
for (i in 1:nlyr(clim)) {
  writeRaster(clim[[i]], paste0('data/envs/clipped/', names(clim)[i], '.tif'), overwrite = T)
}


####  occurrence data processing
# kaouthia
occs_naka <- read.csv('data/occs/NAKAPoints.csv')
colnames(occs_naka) = c('long', 'lat')
head(occs_naka)

# fuxi
occs_nafu <- read.csv('data/occs/NAFUPoints.csv') %>% dplyr::select(-3)
colnames(occs_nafu) = c('long', 'lat')
head(occs_nafu)

# fuxi vietnam
occs_nafu_viet <- read.csv('data/occs/NAFUPoints_Viet.csv') %>% dplyr::select(-3)
colnames(occs_nafu_viet) = c('long', 'lat')
head(occs_nafu_viet)


####  thin occurrence points
# make occs list
occs_list <- list(occs_naka, occs_nafu, occs_nafu_viet)

# occs thinning
occs_thin <- occs_thinner(occs_list = occs_list, 
                          envs = raster::raster(clim[[1]]), 
                          long = 'long', 
                          lat = 'lat', 
                          spp_list = c('Naja kaouthia', 'Naja fuxi', 'Naja fuxi viet'))

# export
write.csv(occs_thin[[1]], 'data/occs_thin/Naja_kaouthia.csv')
write.csv(occs_thin[[2]], 'data/occs_thin/Naja_fuxi.csv')
write.csv(occs_thin[[3]], 'data/occs_thin/Naja_fuxi_viet.csv')


####  sample background points

## points within buffer
# make buffers
buff <- buff_maker(occs_list = occs_list, envs = clim[[1]], buff_dist = 100000)

# sample points
bg_sample <- bg_sampler(method = 'buffer', envs = raster::raster(clim[[1]]), n = 10000, occs_list = occs_list, buffer_list = buff, excludep = T)

# export
write.csv(bg_sample[[1]], 'data/bg/Naja_kaouthia_bg.csv')
write.csv(bg_sample[[2]], 'data/bg/Naja_fuxi_bg.csv')
write.csv(bg_sample[[3]], 'data/bg/Naja_fuxi_viet_bg.csv')


## bias corrected
# make bias grid
nk_bias_grid <- get_bias_grid(targ.pts = occs_thin[[1]], envs = raster(clim[[1]]), poly = raster(clim[[1]]))  # kaouthia
nf_bias_grid <- get_bias_grid(targ.pts = occs_thin[[2]], envs = raster(clim[[1]]), poly = raster(clim[[1]]))  # fuxi
nfv_bias_grid <- get_bias_grid(targ.pts = occs_thin[[3]], envs = raster(clim[[1]]), poly = raster(clim[[1]]))  # fuxi

plot(nk_bias_grid)
plot(nf_bias_grid)
plot(nfv_bias_grid)

# draw points
nk_bg <- bg_sampler(envs = clim[[1]], n = 10000, occs_list = list(occs_thin[[1]]), bias.grid = nk_bias_grid, method = 'bias.grid')
nf_bg <- bg_sampler(envs = clim[[1]], n = 10000, occs_list = list(occs_thin[[2]]), bias.grid = nk_bias_grid, method = 'bias.grid')  
nfv_bg <- bg_sampler(envs = clim[[1]], n = 10000, occs_list = list(occs_thin[[3]]), bias.grid = nk_bias_grid, method = 'bias.grid')

# export bias corrected bg points
write.csv(nk_bg, 'data/bg/Naja_kaouthia_bg_biasCor.csv')
write.csv(nf_bg, 'data/bg/Naja_fuxi_bg_biasCor.csv')
write.csv(nfv_bg, 'data/bg/Naja_fuxi_viet_bg_biasCor.csv')

#####  select environmental variables
# grab 50000 random points
rand.pts <- dismo::randomPoints(mask = raster::raster(clim[[1]]), n = 50000) %>% as.data.frame()
head(rand.pts)

# extract climate values to point
clim_ext <- extract(clim, rand.pts)
print(clim_ext)

# make correlation matrix
cor_mat <- cor(clim_ext[, -1])
print(cor_mat)

# run correlation analysis
correlation_finder(cor_mat = cor_mat, threshold = 0.7)

# selected variables == 'bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19'


