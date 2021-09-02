library(bipartite)
library(magrittr)
# devtools::install_github("valentinitnelav/bootstrapnet")
library(bootstrapnet)

my_web <- Safariland

# Shuffle columns
set.seed(1)
my_web_2 <- my_web[, sample(colnames(my_web))]

set.seed(1)
net <- bipartite::networklevel(web = my_web)
set.seed(1)
net2 <- bipartite::networklevel(web = my_web_2)
identical(net, net2)

is_identical <- rep(NA, length(net))
for (i in 1:length(net)){
  is_identical[i] <- identical(net[i], net2[i])
}
net[!is_identical]
# nestedness           weighted nestedness                discrepancy.HL           extinction.slope.HL 
# 15.4409845                     0.4816773                    21.0000000                     2.1522841 
# extinction.slope.LL                 robustness.HL                 robustness.LL functional.complementarity.HL 
# 1.3306936                     0.6811375                     0.5753878                   407.4448895 

# The frame2webs doesn't seem to fix the issue. the function gives everything in alphabetical order.

safa_df <- bootstrapnet::web_matrix_to_df(Safariland)
safa_df$counts <- NULL
safa_df$webID <- 1
my_web_3 <- bipartite::frame2webs(dframe = safa_df, varnames = c("lower", "higher", "webID"))[[1]]
set.seed(1)
net3 <- bipartite::networklevel(web = my_web_3)
identical(net, net3)

is_identical <- rep(NA, length(net))
for (i in 1:length(net)){
  is_identical[i] <- identical(net[i], net3[i])
}
net[!is_identical]
