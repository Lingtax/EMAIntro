# ESM data simulator
# This will generate a simple random data set with 1 between (x) and 1 within (xi) cluster IV
# there will also be one within subjects DV (y) and a cluster ID (id)

# load packages
library(tidyverse)

# add helper function
capper <- function(x) {ifelse(x>10, 10, x)}

sim_times <- function(days, dailyobs) {
  seq(ISOdatetime(2018, 5, days,  9, 0, 0), by= "100 min", length.out = dailyobs) + 
    abs(rnorm(dailyobs, 400))
}
obs_times <- function(days, daily_obs) {
  purrr::map2(days, daily_obs, sim_times) %>% do.call(c, .)
}
  


# set dataframe scale
nclust <- 30 # number of clusters / individuals
nobs <- 30  # number of observations per cluster 
daily_obs <- 6

# set random seed
# seed <- 1 # replace with value to replicate sample
seed <- runif(1, -9999999, 9999999)
set.seed(seed)

# defines between subjects parameters a and b; constant c; &  between cluster variable x
parameters <-  tibble(id = stringi::stri_rand_strings(nclust, 8),
                      a = rnorm(nclust, rnorm(1, 0, 5), abs(rnorm(10, 0, .2))),
                      x = rnorm(nclust, rnorm(1, 5, .5), abs(rnorm(10, 0, .8))),  
                      b = rnorm(nclust, rnorm(1, 0, 5), abs(rnorm(10, 0, .2))),
                      c = rnorm(nclust, rnorm(1, 0, 5), abs(rnorm(10, 0, .8)))
)

# makes the data long
out <- parameters[rep(seq_len(nrow(parameters)), nobs), ]

# adds within cluster parameters
out <-  out %>% 
  group_by(id) %>% 
  mutate(
    surv_time = obs_times(2:6, daily_obs),
    xi = rnorm(nobs, 4, 2.1),
    error = rnorm(nobs, mean = 0, sd = 3.8), # generates error
    y = (a * x) + (b * xi) + c + error # produces outcome according to function y= ax + bxi + c + error
  )

# outputs two dataframes to reflect native output structure from instant survey and qualtrics (or equiv)

between <- select(parameters, id, x) %>%
  distinct(id, .keep_all = TRUE) %>% 
  mutate(item1 = 11 - capper(round(x + rnorm(nclust, 0, .4), 0)),
         item2 = capper(round(x + rnorm(nclust, 0, .4), 0)), 
         item3 = capper(round(x + rnorm(nclust, 0, .4), 0)), 
         item4 = capper(round(x + rnorm(nclust, 0, .4), 0)) 
         ) %>% 
  select(-x)

within <- out %>%  select(id, surv_time, xi, y)

write_csv(between, paste("baseline.csv", sep=""))
write_csv(within, paste("esm.csv", sep=""))

# trims parameter frame for recording
params <-  select(parameters, id, a, b, c)

# records "true" parameters for model diagnostics
sink(paste(seed,  "_SIMparameters.txt", sep = ""))
paste("Simulated data parameters from random seed ", seed, sep = "")
paste("Data contains ", nobs, " observations from each of ", nclust,  " clusters", sep = "")
print(params, n=Inf)
sink()
