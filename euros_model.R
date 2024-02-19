###### Euros 2024 Model #####
###### By: Stephan Teodosescu #####
##### Updated February 2024 #####

### Load Packages
library(tidyverse)
library(worldfootballR)
library(gridExtra)

# Code and model inspired by Luke Benz's work from 2018 WC: https://lukebenz.com/post/wc_model_methodology/blogpost/

### Load dataset and data cleansing
x <- read_csv("results.csv") #accessed from Kaggle on July 7, 2022: https://www.kaggle.com/datasets/martj42/international-football-results-from-1872-to-2017
n <- nrow(x)
x$date <- as.Date(x$date,"%Y-%m-%d")
x$days_since <- as.numeric(Sys.Date() - x$date)

### Duplicate the Data Set for Home and Away
home <- select(x, home_team, away_team, tournament, neutral, home_score, 
               days_since, date) %>%
    mutate(location = "H")

away <- select(x, away_team, home_team, tournament, neutral, away_score, 
               days_since, date) %>%
    mutate(location = "A")

names(home) <- c("team", "opponent", "tournament", "neutral", "goals", 
                 "days_since", "date", "location")

names(away) <- c("team", "opponent", "tournament", "neutral", "goals", 
                 "days_since", "date", "location")

x <- rbind(home, away) #bind rows of H/A data
x$location[x$neutral] <- "N"

### Classify Game Types (codes each tournament to certain game type)
#-- OM <- Other Match
#-- WC <- World Cup
#-- WCQ <- World Cup Qualifying
#-- CC <- Continental Cup
#-- CCQ <- Continental Cup Qualifying
#-- FR <- Friendly
#-- CFC <- Confederations Cup

x$game_type <- "OM"
x$game_type[x$tournament == "FIFA World Cup"] <- "WC"
x$game_type[x$tournament == "FIFA World Cup qualification"] <- "WCQ"
x$game_type[x$tournament == "Friendly"] <- "FR"
x$game_type[x$tournament == "Confederations Cup"] <- "CFC"

x$game_type[x$tournament == "AFC Asian Cup"] <- "CC"
x$game_type[x$tournament == "AFC Challenge Cup"] <- "CC"
x$game_type[x$tournament == "African Cup of Nations"] <- "CC"
x$game_type[x$tournament == "CFU Caribbean Cup"] <- "CC"
x$game_type[x$tournament == "CONCACAF Championship"] <- "CC"
x$game_type[x$tournament == "Gold Cup"] <- "CC"
x$game_type[x$tournament == "Oceania Nations Cup"] <- "CC"
x$game_type[x$tournament == "UAFA Cup"] <- "CC"
x$game_type[x$tournament == "UEFA Euro"] <- "CC"

x$game_type[x$tournament == "AFC Asian Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "AFC Challenge Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "African Cup of Nations qualification"] <- "CCQ"
x$game_type[x$tournament == "CFU Caribbean Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "CONCACAF Championship qualification"] <- "CCQ"
x$game_type[x$tournament == "Gold Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "Oceania Nations Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "UAFA Cup qualification"] <- "CCQ"
x$game_type[x$tournament == "UEFA Euro qualification"] <- "CCQ"

### Match Importance Parameters (based on those used in the FIFA rankings formula)
x <- mutate(x, "match_weight" = 
                case_when(
                    game_type == "WC" ~ 8,
                    game_type == "WCQ" ~ 3,
                    game_type == "CCQ" ~ 3,
                    game_type == "CFC" ~ 5,
                    game_type == "CC" ~ 5,
                    TRUE ~ 1
                    
                ))

### Inverts Perspective of Data Frame from Team to Opponent
invert <- function(data, score = F) {
    data <- mutate(data, tmp = team, team = opponent, opponent = tmp)
    data$tmp[data$location == "H"] <- "A"
    data$tmp[data$location == "A"] <- "H"
    data$tmp[data$location == "N"] <- "N"
    data$location <- data$tmp
    if(score) {
        data$tmp <- data$team_score
        data$team_score <- data$opponent_score
        data$opponent_score <- data$tmp
    }
    return(select(data,-tmp))
}


### Obtain W, L, T probabilities (Poisson Regression model)
match_probs <- function(lambda_1, lambda_2) {
    max_goals <- 10
    score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
    tie_prob <- sum(diag(score_matrix))
    win_prob <- sum(score_matrix[lower.tri(score_matrix)])
    loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
    return(c(win_prob, tie_prob, loss_prob))
}



### Model Fitting 

### Parameters: Team, Opponent, Match Type, Location, Days Since Previous Euros
y <- filter(x, date >= "2021/01/01")
fixtures <- read_csv("fixtures.csv") # need to update this with latest fixtures


### Fit Poisson Model
glm.futbol <- glm(goals ~ team + opponent + location, 
                  family = "poisson",
                  data = y, 
                  weights = match_weight)

### Rankings
team_num <- (length(glm.futbol$coefficients) - 1)/ 2

rankings <- data.frame("team" = sort(unique(y$team)),
                       "offense" = rep(NA, team_num),
                       "defense" = rep(NA, team_num),
                       stringsAsFactors = F)

off_scale_factor <- mean(glm.futbol$coefficients[2:team_num])
def_scale_factor <- mean(glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)], na.rm = T)
rankings$offense <- c(0, glm.futbol$coefficients[2:team_num]) - off_scale_factor
rankings$defense <- c(0, glm.futbol$coefficients[(team_num + 1):(2*team_num - 1)]) - def_scale_factor
rankings$net_rating <- rankings$offense - rankings$defense

rankings <- arrange(rankings, desc(net_rating)) %>%
    mutate(rank = 1:team_num)

write_csv(rankings, "rankings.csv")


########################## Euros 2024 Simulations ##############################

groups <- c("A", "B", "C", "D", "E", "F")

euro_sims <- data.frame("country" = unique(c(fixtures$team, fixtures$opponent)),
                      "group" = NA,
                      "exp_pts" = 0,
                      "exp_gf" = 0,
                      "exp_gd" = 0,
                      "first_in_group" = 0,
                      "second_in_group" = 0,
                      "third_in_group" = 0,
                      "r16" = 0,
                      "qtrs" = 0,
                      "semis" = 0,
                      "finals" = 0,
                      "champ" = 0,
                      stringsAsFactors = F)

### function to find Group for a team
find_group <- function(country) {
    group <- filter(fixtures, team == country | opponent == country) %>%
        slice(1) %>% 
        pull(group)
    return(group)
}

euro_sims$group <- unname(sapply(wc_sims$country, find_group))

write_csv(wc_sims, "wc_sims.csv")