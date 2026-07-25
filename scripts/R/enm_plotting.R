#####  plot enm results

# clean up working env
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMwrap)
library(terra)
library(dplyr)
library(tidyterra)
library(ggplot2)


#####  load model object
test_models <- readRDS('output/test_models.rds')
print(test_models)


#####  response curve data
# Naja kaouthia
naka_resp <- resp_data_pull(sp.name = 'Naja_kaouthia', model = test_models$models[[1]], 
                            names.var = c('bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19'))

# Naja fuxi == model built with Vietnam data added
nafu_resp <- resp_data_pull(sp.name = 'Naja_fuxi', model = test_models$models[[3]],
                            names.var = c('bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19'))


#####  plot predictions
# first look
plot(test_models$preds[[1]])
plot(test_models$preds[[2]])
plot(test_models$preds[[3]])


#####  get thresholds and make binary maps
# load occs
naka <- read.csv('data/occs_thin/Naja_kaouthia.csv') %>% dplyr::select(-1)
nafu <- read.csv('data/occs_thin/Naja_fuxi.csv') %>% dplyr::select(-1)
nafu_viet <- read.csv('data/occs_thin/Naja_fuxi_viet.csv') %>% dplyr::select(-1)

# get thresholds
th <- get_thresh(preds = test_models$preds, occs.list = list(naka, nafu, nafu_viet), type = 'p10')
print(th)

# make binary
naja_bin <- bin_maker(preds = test_models$preds, th = th)

plot(naja_bin[[1]])
plot(naja_bin[[2]])
plot(naja_bin[[3]])


#####  plot maps
# stack continuous layers
kobra_cont <- c(rast(test_models$preds[[1]]), rast(test_models$preds[[2]]), rast(test_models$preds[[3]]))
names(kobra_cont) = c('N. kaouthia', 'N. fuxi', 'N. fuxi 2')

# plot continuous
naja_cont_plot <- ggplot() +
  geom_spatraster(data = kobra_cont) +
  facet_wrap(~ lyr) +
  coord_sf(expand = F) +
  scale_x_continuous(breaks = seq(round(ext(kobra_cont)[1] - 1, digits = 0), round(ext(kobra_cont)[2], digits = 0), by = 10)) +
  scale_fill_grass_c(palette = 'inferno',
                     na.value = NA,
                     name = 'RS') +
  theme_bw() +
  theme(strip.text = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

print(naja_cont_plot)


# stack binary
kobra_bin

# plot binary
naja_bin_plot <- ggplot() +
  geom_spatraster(data = kobra_bin) +
  facet_wrap(~ lyr) +
  coord_sf(expand = F) +
  scale_x_continuous(breaks = seq(round(ext(kobra_bin)[1] - 1, digits = 0), round(ext(kobra_bin)[2], digits = 0), by = 15)) +
  scale_fill_gradientn(colors = c('lightgrey', '#1E88E5'),
                       na.value = 'transparent') +
  theme_bw() +
  theme(strip.text = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = 'none')