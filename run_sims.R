### Euros 2024 Simulations

library(tidyverse)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multiprocess(workers = parallel::detectCores()-1))
#source('helpers.R')

### Simulation Parameters
n_sims <- 100
set.seed(12345)
run_date <- case_when(lubridate::hour(Sys.time()) <= 9 ~as.Date(Sys.Date()),
                      T ~ as.Date(Sys.Date() ))

run_date <- Sys.Date()

### Coefficients