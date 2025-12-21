########  naja enm

# clean up working env
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMwrap)
library(terra)
library(dplyr)


#####  load data
# climate
clim <- rast(list.files(path = 'data/envs/clipped/', pattern = '.tif', full.names = T))
clim <- clim[[c('bio1', 'bio2', 'bio3', 'bio12', 'bio14', 'bio15', 'bio18', 'bio19')]]
print(clim)

# divide temperature layers by 10
clim[[c('bio1', 'bio2', 'bio3')]] <- clim[[c('bio1', 'bio2', 'bio3')]] / 10
print(clim)

# occs
naka <- read.csv('data/occs_thin/Naja_kaouthia.csv') %>% dplyr::select(-1)
nafu <- read.csv('data/occs_thin/Naja_fuxi.csv') %>% dplyr::select(-1)
nafu_viet <- read.csv('data/occs_thin/Naja_fuxi_viet.csv') %>% dplyr::select(-1)

head(naka)
head(nafu)
head(nafu_viet)

# bg
naka_bg <- read.csv('data/bg/Naja_kaouthia_bg.csv') %>% dplyr::select(-1)
nafu_bg <- read.csv('data/bg/Naja_fuxi_bg.csv') %>% dplyr::select(-1)
nafu_viet_bg <- read.csv('data/bg/Naja_fuxi_viet_bg.csv') %>% dplyr::select(-1)

head(naka_bg)
head(nafu_bg)
head(nafu_viet_bg)


#####  run models
# run models we do
test_models <- test_multisp(taxon.list = c('Naja kaouthia', 'Naja fuxi', 'Naja fuxi vietnam'),
                            occs.list = list(naka, nafu, nafu_viet),
                            bg = list(naka_bg, nafu_bg, nafu_viet_bg),
                            envs = clim,
                            tune.args = list(fc = c('L', 'Q', 'H', 'P', 'LQ', 'LP', 'QH', 'QP', 'HP', 'LQH', 'LQP', 'LQHP', 'LQHPT'), 
                                             rm = seq(0.5,5, by = 0.5)),
                            partitions = 'checkerboard',
                            partition.settings = list(aggregation.factor = c(5,5)),
                            type = 'type1')
