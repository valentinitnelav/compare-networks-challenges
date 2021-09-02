rm(list = ls()) #to clear the workspace
#__________________________________________________________________
#
# Networks metric and permutation test ---- 10/06/2021
#
#__________________________________________________________________
#
## Import data and packages
library(data.table)
library(dplyr)
library(bipartite)
library(bootstrapnet)
library(ggplot2)
library(ggthemes)
library(car)
library(bootstrapnet)

# to run a permutation comparison test
library(tidyr)
library(tibble)


## * Prepare data ----
# create separate data frames for wooded and alvar
# and repeat rows according to number of observations:

weissen <- fread("./estonia-case/data/interactions_with_plant_cover_2020_08_24.csv")
head(weissen)
setDT(weissen)  

# Select wooded records and only needed columns
wooded <- weissen[Management_type == "wooded meadow", .(Plant_species, Insect_species, Num_Individuals)]
wooded_expanded <- wooded[rep(seq(1, .N), times=Num_Individuals)]

# Select alvar records and only needed columns
alvar <- weissen[Management_type == "alvar pasture", .(Plant_species, Insect_species, Num_Individuals)]
alvar_expanded <- alvar[rep(seq(1, .N), times=Num_Individuals)]


alvar_expanded$Plant_species %>% unique %>% sort
alvar_expanded$Insect_species %>% unique %>% sort


# Combine the two networks in a big bucket.
# Each interaction will belong still to the right network for now.

# net 1 is wooded, net 2 is alvar
net_df <- bind_rows(wooded_expanded, alvar_expanded, .id = "net")


# interaction evenness
# NODF 
# connectance 
# diversity
# H2
index <- "H2"

N <- 1000 # Choose a number of repetitions

# Prepare two empty vectors
net_1_boot_values <- net_2_boot_values <- rep(NA, N)

for (i in 1:N){
  set.seed(i)
  
  # create a new column with randomly assigned network labels
  net_df <- net_df %>% mutate(net_shuffled = sample(net, replace = FALSE))
  
  # Using the random network labels, construct a network 1 (wooded) and compute network metric
  net_1_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 1) %>% 
    select(Plant_species, Insect_species) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
  
  # Using the random network labels, construct a network 2 (alvar) and compute network metric
  net_2_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 2) %>% 
    select(Plant_species, Insect_species) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
}


# 
# 
# # Visualise the two distribution of the network index permuted values.
# boots_df <- data.frame(web = rep(c("wooded", "alvar"), each = N),
#                        boots = c(net_1_boot_values, net_2_boot_values))
# 
# mean_boots <- boots_df %>% 
#   group_by(web) %>% 
#   summarise(mean = mean(boots),
#             median = median(boots))
# mean_boots
# 
# 
# 
# ## calculate indices for the networks that we compare.
# # First build the tow networks:
com_matWooded <- table(wooded_expanded$Plant_species, wooded_expanded$Insect_species)
com_matAlvar <- table(alvar_expanded$Plant_species, alvar_expanded$Insect_species)
# 
# # Compute the 'observed' metrics
# observed_metrics <- data.frame(web = c("wooded", "alvar"),
#                                obs = sapply(list(com_matWooded, com_matAlvar),
#                                             FUN = networklevel,
#                                             index = index))
# 
# observed_metrics
# 
# ggplot(data = boots_df,
#        aes(x = boots,
#            color = web)) +
#   geom_density() +
#   geom_vline(data = mean_boots,
#              aes(xintercept = mean,
#                  color = web),
#              linetype = "dashed") +
#   geom_vline(data = observed_metrics,
#              aes(xintercept = obs,
#                  color = web),
#              linetype = "solid") +
#   labs(x = index) +
#   theme_bw()


# Statistical test -----------------------------------------------------------------


dif <- (net_1_boot_values - net_2_boot_values)


obs_dif <- sapply(list(com_matWooded, com_matAlvar),
                  FUN = networklevel,
                  index = index) %>% diff()
obs_dif

hist(dif)
abline(v=0, col = "red", lwd = 2)
abline(v=obs_dif, col = "blue", lwd = 2)

# p-value
mean(abs(dif) >= abs(obs_dif))
