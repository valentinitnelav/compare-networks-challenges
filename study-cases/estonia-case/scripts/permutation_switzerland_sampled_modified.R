rm(list = ls()) #to clear the workspace
#__________________________________________________________________
#
# Networks metric and permutation test for different size networks ---- 01/07/2021
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
# create separate data frames for wooded and alvars
# and repeat rows according to number of observations:

weissen <- fread("./estonia-case/data/interactions_with_plant_cover_2020_08_24.csv")
head(weissen)
setDT(weissen)  

# Select wooded records and only needed columns
wooded <- weissen[Management_type == "wooded meadow",
                  .(Plant_species, Insect_species, Num_Individuals)]
wooded_expanded <- wooded[rep(seq(1, .N), times=Num_Individuals)]

# Select alvar records and only needed columns
alvar <- weissen[Management_type == "alvar pasture",
                 .(Plant_species, Insect_species, Num_Individuals)]
alvar_expanded <- alvar[rep(seq(1, .N), times=Num_Individuals)]


# index <- "interaction evenness"
index <- "H2"

N <- 1000 # Choose a number of repetitions

# Prepare two empty vectors
net_1_boot_values <- net_2_boot_values <- sample_metric_wooded <- rep(NA, N)

for (i in 1:N){
  set.seed(i)
  
  # draw random sample of wooded meadow dataset so it is the same size as alvar
  wooded_reduced <- wooded_expanded[sample(x = nrow(wooded_expanded), size = nrow(alvar_expanded)), ]
  # wooded_reduced                                    # Print sampled data
  net_df <- bind_rows(alvar_expanded, wooded_reduced, .id = "net")
  sample_metric_wooded[i] <- wooded_reduced %>% 
    select(Plant_species, Insect_species) %>% 
    table() %>% 
    bipartite::networklevel(index = index)
  
  # create a new column with randomly assigned network labels
  net_df <- net_df %>% mutate(net_shuffled = sample(net, replace = FALSE))
  
  # Using the random network labels, construct a network 1
  net_1_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 1) %>% 
    select(Plant_species, Insect_species) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
  
  # Using the random network labels, construct a network 2
  net_2_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 2) %>% 
    select(Plant_species, Insect_species) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
}


dif <- (net_1_boot_values - net_2_boot_values)

# mean H2 of the sample of wooded ~ observed H2 of the wooded at the sample point
obs_metric_wooded_mean <- mean(sample_metric_wooded)
obs_metric_wooded_mean
# observed H2 of the alvar
obs_metric_alvar <- alvar_expanded %>% 
  select(Plant_species, Insect_species) %>% 
  table() %>% 
  bipartite::networklevel(index = index)
obs_metric_alvar

obs_dif <- obs_metric_alvar - obs_metric_wooded_mean
obs_dif

hist(dif)
abline(v=0, col = "red", lwd = 2)
abline(v=obs_dif, col = "blue", lwd = 2)

# p-value
mean(abs(dif) >= abs(obs_dif))
