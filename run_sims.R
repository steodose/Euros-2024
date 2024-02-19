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


### Read in Ratings and Schedule
df_ratings <- read_csv('rankings.csv')
schedule <- 
  read_csv('schedule.csv') %>% 
  mutate('team1_score' = ifelse(date >= run_date, NA, team1_score),
         'team2_score' = ifelse(date >= run_date, NA, team2_score)) %>% 
  mutate('team1_score' = case_when(is.na(shootout_winner) ~ team1_score,
                                   shootout_winner == team1 ~ 0.1 + team1_score,
                                   shootout_winner == team2 ~ -0.1 + team1_score))