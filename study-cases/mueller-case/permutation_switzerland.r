

#__________________________________________________________________
#
# Networks metric and permutation test ---- 10/06/2021
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
  
  present <- weissen[period == "present",  
                     .(total_individuals = abundance),
                     keyby = .(plant_species, insect_sp)] #insect_order, insect_family, insect_genus, 
  present_expanded <- present[rep(seq(1, .N), times=total_individuals)]
  
  past <- weissen[period == "past",
                  .(total_individuals = abundance),
                  keyby = .(plant_species, insect_sp)] # insect_order, insect_family, insect_genus, 
  past_expanded <- past[rep(seq(1, .N), times=total_individuals)]
}


# Combine the two networks in a big bucket and shuffle the data so that we break
# the order. Each interaction will belong still to the right network for now.
# set.seed(666)
net_df <- bind_rows(past_expanded, present_expanded, .id = "net")


# index <- "interaction evenness"
index <- "weighted nestedness"

N <- 1000 # Choose a number of repetitions

# Prepare two empty vectors
net_1_boot_values <- net_2_boot_values <- rep(NA, N)

for (i in 1:N){
  set.seed(i)
  
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




# Visualise the two distribution of the network index permuted values.
boots_df <- data.frame(web = rep(c("net_1", "net_2"), each = N),
                       boots = c(net_1_boot_values, net_2_boot_values))

mean_boots <- boots_df %>% 
  group_by(web) %>% 
  summarise(mean = mean(boots),
            median = median(boots))
mean_boots



## calculate weighted nestedness index for past and present networks:

com_matPresent <- table(present_expanded$plant_species, present_expanded$insect_sp)
com_matPast <- table(past_expanded$plant_species, past_expanded$insect_sp)


observed_metrics <- data.frame(web = c("net_1", "net_2"),
                               obs = sapply(list(com_matPast, com_matPresent),
                                            FUN = networklevel,
                                            index = index))

observed_metrics
# web       obs
# 1 net_1 0.3550915
# 2 net_2 0.4045391

ggplot(data = boots_df,
       aes(x = boots,
           color = web)) +
  geom_density() +
  geom_vline(data = mean_boots,
             aes(xintercept = mean,
                 color = web),
             linetype = "dashed") +
  geom_vline(data = observed_metrics,
             aes(xintercept = obs,
                 color = web),
             linetype = "solid") +
  labs(x = index) +
  theme_bw()


# Statistical test -----------------------------------------------------------------


dif <- (net_1_boot_values - net_2_boot_values)


obs_dif <- sapply(list(vazarr, vazcer),
                  FUN = networklevel,
                  index = index) %>% 
  diff()

hist(dif, xlim = c(-abs(obs_dif), abs(obs_dif)))
abline(v=0, col = "red", lwd = 2)
abline(v=obs_dif, col = "blue", lwd = 2)

# p-value
mean(abs(dif) >= abs(obs_dif))
