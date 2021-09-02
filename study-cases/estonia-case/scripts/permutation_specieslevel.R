library(data.table)
library(bipartite)

# These links can be useful for attempting an ANOVA as permutation test. But I
# didn't use them in this script.

# https://rcompanion.org/rcompanion/d_06a.html

# https://cran.r-project.org/web/packages/bipartite/vignettes/Intro2bipartite.pdf
# https://en.wikipedia.org/wiki/Permutational_analysis_of_variance
# https://stats.stackexchange.com/questions/136126/difference-between-anova-and-permutation-test
# https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/15_-_permutationaltests.pdf
# https://rdrr.io/cran/RVAideMemoire/man/perm.anova.html

dat <- fread("./estonia-case/data/interactions_with_plant_cover_2020_08_24.csv")

wooded <- dat[Management_type == "wooded meadow", .(Plant_species, Insect_species, Num_Individuals)]
wooded_expanded <- wooded[rep(seq(1, .N), times = Num_Individuals)]

wooded_web <- wooded[, table(Plant_species, Insect_species)]

bipartite::specieslevel(wooded_web, index = "species strength", level = "lower")
