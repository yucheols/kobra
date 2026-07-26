#####  plot niche test results

# clean up working env
rm(list = ls(all.names = T))
gc()

# load packages
library(dplyr)
library(ggplot2)

##### load rds files
eq_test <- readRDS('output/niche_eq.rds')
niche_test_1 <- readRDS('output/niche_sim1_1.rds')
niche_test_2 <- readRDS('output/niche_sim1_2.rds')

##### check empirical overlap and p values
# identity test ::: D_obs = 0.05071603 // I_obs = 0.1896737 // p_D = 0.000999001 // p_I = 0.000999001
print(eq_test$p.values)
print(eq_test$test.results)

# sim test 1 // p_D = 0.2147852 // p_I = 0.3476523
print(niche_test_1$p.values)

# sim test 2 // p_D = 0.05744256 // p_I = 0.12637363
print(niche_test_2$p.values)

##### plot results
head(eq_test$test.results$sim)
head(niche_test_1$test.results$sim)
head(niche_test_2$test.results$sim)

#### D
# prep data
niche.eq.D <- dplyr::select(eq_test$test.results$sim, 1)
niche.eq.D$test = 'Equivalency'
head(niche.eq.D)

niche.sim.1.D <- dplyr::select(niche_test_1$test.results$sim, 1)
niche.sim.1.D$test = 'Similarity_1'
head(niche.sim.1.D)

niche.sim.2.D <- dplyr::select(niche_test_2$test.results$sim, 1)
niche.sim.2.D$test = 'Similarity_2'
head(niche.sim.2.D)

D.data <- rbind(niche.eq.D, niche.sim.1.D, niche.sim.2.D)
head(D.data)
tail(D.data)

# plot
D.data %>%
  ggplot(aes(x = D)) +
  facet_wrap(~ test, scales = 'free') +
  geom_histogram(fill = 'cornflowerblue', color = NA, alpha = 0.4, bins = 15) +
  geom_vline(xintercept = eq_test$test.results$obs$D, linetype = 2, linewidth = 1.2) +
  xlab("Schoener's D") + ylab('Frequency') + 
  theme_bw() + 
  theme(strip.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

## export 
ggsave('output/plots/niche.D.jpg', width = 25, height = 10, dpi = 600, units = 'cm')


#### I 
# prep data
niche.eq.I <- dplyr::select(eq_test$test.results$sim, 2)
niche.eq.I$test <- 'Equivalency'
head(niche.eq.I)

niche.sim.1.I <- dplyr::select(niche_test_1$test.results$sim, 2)
niche.sim.1.I$test <- 'Similarity_1'
head(niche.sim.1.I)

niche.sim.2.I <- dplyr::select(niche_test_2$test.results$sim, 2)
niche.sim.2.I$test <- 'Similarity_2'
head(niche.sim.2.I)

I.data <- rbind(niche.eq.I, niche.sim.1.I, niche.sim.2.I)
head(I.data)
tail(I.data)

I.data %>%
  ggplot(aes(x = I)) +
  facet_wrap(~ test, scales = 'free') +
  geom_histogram(fill = 'orange', color = NA, alpha = 0.4, bins = 15) +
  geom_vline(xintercept = eq_test$test.results$obs$I, linetype = 2, linewidth = 1.2) +
  xlab("Warren's I") + ylab('Frequency') + 
  theme_bw() +
  theme(strip.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)))

## export 
ggsave('output/plots/niche.I.jpg', width = 25, height = 10, dpi = 600, units = 'cm')
