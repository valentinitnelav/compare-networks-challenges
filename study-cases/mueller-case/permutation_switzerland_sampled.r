

#__________________________________________________________________
#
# Networks metric and permutation test for different size networks ---- 01/07/2021
#
#__________________________________________________________________
#
## Import data and packages
#library(xlsx)
library(data.table)
library(dplyr)
library(bipartite)
library(bootstrapnet)
library(ggplot2)
library(rJava)
library(ggthemes)
library(car)
library(bootstrapnet)

# to run a permutation comparison test
library(tidyr)
library(tibble)


## * Prepare data ----
# create separate data frames for past and present
# and repeat rows according to number of observations:

setwd("C:/Users/nq89avak/Documents/iDiv/Pollination_project/Switzerland/")

{
  weissen <- read.csv("processed_data/weissen_plant_selected_tab.csv")
  head(weissen)
  setDT(weissen)  
  
  # present <- weissen[period == "present",  
  #                    .(total_individuals = abundance),
  #                    keyby = .(plant_species, insect_sp)] #insect_order, insect_family, insect_genus, 
  present <- weissen[period == "present", .(abundance, plant_species, insect_sp)]
  present_expanded <- present[rep(seq(1, .N), times=abundance)]
  
  # past <- weissen[period == "past",
  #                 .(total_individuals = abundance),
  #                 keyby = .(plant_species, insect_sp)] # insect_order, insect_family, insect_genus, 
  past <- weissen[period == "past", .(abundance, plant_species, insect_sp)]
  past_expanded <- past[rep(seq(1, .N), times=abundance)]
}



# index <- "interaction evenness"
index <- "weighted nestedness"

N <- 100 # Choose a number of repetitions

# Prepare two empty vectors
net_1_boot_values <- net_2_boot_values <- obs_metric_past <- rep(NA, N)

for (i in 1:N){
  set.seed(i)
  
  # generate random sampling to have equal datasets length between small and bigger network
  past_expanded_sampled <- past_expanded[sample(x = nrow(past_expanded), size = nrow(present_expanded))]
  net_df <- bind_rows(past_expanded_sampled, present_expanded, .id = "net")
  obs_metric_past[i] <- past_expanded_sampled %>% 
    select(plant_species, insect_sp) %>% 
    table() %>% 
    bipartite::networklevel(index = index)
  
  # create a new column with randomly assigned network labels
  net_df <- net_df %>% mutate(net_shuffled = sample(net, replace = FALSE))
  
  # Using the random network labels, construct a network 1
  net_1_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 1) %>% 
    select(plant_species, insect_sp) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
  
  # Using the random network labels, construct a network 2
  net_2_boot_values[i] <- 
    net_df %>% 
    filter(net_shuffled == 2) %>% 
    select(plant_species, insect_sp) %>% 
    table() %>% 
    bipartite::networklevel(index = index) 
}


dif <- (net_1_boot_values - net_2_boot_values)

# mean H2 of the sample of the past ~ H2 of the past
obs_metric_past_mean <- mean(obs_metric_past)
# observed H2 of the present
obs_metric_present <- present_expanded %>% 
  select(plant_species, insect_sp) %>% 
  table() %>% 
  bipartite::networklevel(index = index)

obs_dif <- obs_metric_past_mean - obs_metric_present

hist(dif, xlim = c(-abs(obs_dif), abs(obs_dif)))
abline(v=0, col = "red", lwd = 2)
abline(v=obs_dif, col = "blue", lwd = 2)

# p-value
mean(abs(dif) >= abs(obs_dif))
