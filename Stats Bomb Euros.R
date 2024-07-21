###### Stats Bomb Euros Data
###### By: Stephan Teodosescu
###### July 2024 #####

library(tidyverse)
library(StatsBombR)
library(SBpitch)
library(downloader)
library(gt)
library(gtExtras)
library(padr)
library(rvest)
library(teamcolors)
library(cowplot)
library(ggchicklet)
library(janitor)
library(ggtext)
library(glue)
library(ggimage)
library(scales)
library(prismatic)


### Inspiration comes from this post: https://mackinawstats.home.blog/2020/02/23/making-expected-goal-charts-in-r/

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}


# create aspect ration to use throughout
asp_ratio <- 1.618

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
    
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
}



## -------------------- Get data -------------------

team_mapping <- read_csv('https://raw.githubusercontent.com/steodose/Euros-2024/master/team_mapping.csv')

# download Euros 2024  event data from Stats Bomb's free repo
euros <- FreeCompetitions() %>%
    filter(competition_id==55 & season_id==282)

matches <- FreeMatches(euros)
all_events <- free_allevents(MatchesDF = matches, Parallel = T) %>%
  allclean() #StatsBomb package function to clean data

# download Copa America event data from Stats Bomb's free repo
copa <- FreeCompetitions() %>%
  filter(competition_id==223 & season_id==282)

matches_copa <- FreeMatches(copa)
all_events_copa <- free_allevents(MatchesDF = matches_copa, Parallel = T) %>%
  allclean() #StatsBomb package function to clean data


## 1. ------- Euros vs Copa Passing Rates -------- ##

# create new datasets that show passes per minute for each comp
euro_passes <-  all_events %>%
filter(type.name == 'Pass') %>%
  filter(is.na(pass.outcome.name)) #NA represent completed passes for some reason in SB data

# aggregate passes by team
euro_passes_agg <- euro_passes %>%
  select(match_id, period, minute, team.name, type.name, pass.length, pass.angle, player.name, pass.recipient.name, pass.outcome.name, pass.height.name,
         pass.body_part.name, location.x, location.y, pass.end_location.x, pass.end_location.y, 
         carry.end_location.x, carry.end_location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z) %>%
  mutate(completed_pass = ifelse(is.na(pass.outcome.name), 1, 0)) %>%
  group_by(match_id, team.name) %>%
  summarise(passes_min = sum(completed_pass)/max(minute)) %>%
  ungroup() %>%
  group_by(team.name) %>%
  summarise(team_passes_min = mean(passes_min)) %>%
  slice_max(team_passes_min, n=10) %>%
  mutate(competition = 'Euros')


#Copa
copa_passes <-  all_events_copa %>%
  filter(type.name == 'Pass') %>%
  filter(is.na(pass.outcome.name)) #NA represent completed passes for some reason in SB data

# aggregate passes by team
copa_passes_agg <- copa_passes %>%
  select(match_id, period, minute, team.name, type.name, pass.length, pass.angle, player.name, pass.recipient.name, pass.outcome.name, pass.height.name,
         pass.body_part.name, location.x, location.y, pass.end_location.x, pass.end_location.y, 
         carry.end_location.x, carry.end_location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z) %>%
  mutate(completed_pass = ifelse(is.na(pass.outcome.name), 1, 0)) %>%
  group_by(match_id, team.name) %>%
  summarise(passes_min = sum(completed_pass)/max(minute)) %>%
  ungroup() %>%
  group_by(team.name) %>%
  summarise(team_passes_min = mean(passes_min)) %>%
  slice_max(team_passes_min, n=10) %>%
  mutate(competition = 'Copa')

# join together
passes_joined <- euro_passes_agg %>%
  rbind(copa_passes_agg) %>%
  arrange(desc(team_passes_min)) %>%
  left_join(team_mapping, by = c("team.name" = "team"))

passes_joined$team_passes_min <- format(round(passes_joined$team_passes_min,1),nsmall=1) #round SRS ratings

# make bar chart viz
passes_joined %>% 
  mutate(team.name = fct_reorder(team.name, team_passes_min)) %>%
  ggplot(aes(x = team.name, y = team_passes_min, fill = competition)) +
geom_chicklet() + #same as geom_col or geom_bar
  # geom_richtext(
  #   aes(y = -6, label = logo_label, hjust = 1),
  #   label.size = 0, fill = NA
  # ) +
  geom_text(aes(label = paste0(team_passes_min, "")), fontface = "bold", 
            family = "Outfit", hjust = 1) +
  coord_cartesian(clip = "off") +
  #scale_fill_identity(guide = "none") +
  theme_custom() + 
  coord_flip() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = 'plot',
        axis.text.y=element_blank(),
        plot.margin = margin(15, 30, 15, 15)
  ) +
  theme(legend.position = "none") +
  labs(x = "", 
       y = "Avg. Passes Completed per Min", 
       title = "Euros vs Copa America", 
       subtitle = paste0("Average passes completed per min for Top 10 teams in Euros vs Copa America 2024."), 
       caption = "Data:Stats Bomb\nPlot: @steodosescu")
  

  
  

## 2. ------ xG Cumulative Race Charts (WIP) --------##

expected_goals_match <- all_events %>%
    group_by(possession_team.name) %>%
    summarise(xg_match = sum(shot.statsbomb_xg, na.rm = TRUE/n_distinct(match_id))
    ) %>%
    arrange(desc(xg_match))


# filter for the Euro Final between England and Spain
euro_final <- all_events %>%
    filter(match_id == 3943043) 

shots_filtered <- euro_final %>%
    mutate(xG = ifelse(is.na(shot.statsbomb_xg),0,shot.statsbomb_xg)) %>%  #convert NAs to 0 so they are plotted
    group_by(possession_team.name) %>%
    mutate(cumulative_xG = cumsum(xG)
    )


# make plot (WIP)
shots_filtered %>%
    ggplot(aes(timestamp, cumulative_xG, group = possession_team.name)) +
    geom_line(aes(color = possession_team.name), size = 1.5) +
    geom_point(aes(color = possession_team.name), size = 3, data = shots_filtered
               %>% filter(type.name == 'Shot'))



## 3. -------- Passing Heatmaps ---------- ##

euro_final <- allclean(euro_final)

spain <- euro_final %>%
    filter(team.name == 'Spain', type.name == 'Pass') %>%
    filter(is.na(pass.outcome.name)) #NA represent completed passes for some reason in SB data

spain <- spain %>%
    select(period, minute, type.name, pass.length, pass.angle, player.name, pass.recipient.name, pass.outcome.name, pass.height.name,
           pass.body_part.name, location.x, location.y, pass.end_location.x, pass.end_location.y, 
           carry.end_location.x, carry.end_location.y, shot.end_location.x, shot.end_location.y, shot.end_location.z)


# creating Spain's passing heatmap plot vs England 

palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = 1)

p1 <- create_Pitch(grass_colour = "floralwhite", 
                   line_colour =  "#B3CED9", 
                   background_colour = "floralwhite", 
                   goal_colour = "#15393D") + 
    geom_density_2d_filled(data = spain, aes(x = pass.end_location.x, y = pass.end_location.y, fill = ..level..,), alpha = .4, 
                           contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10)) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 120)) +
    scale_y_continuous(limits = c(0, 80)) +
    scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) + 
    theme(legend.position = "none", 
          plot.background = element_rect(colour = "floralwhite", fill = "floralwhite"),
          plot.title = element_text(hjust = .5, size = 22, family = "Outfit", face = "bold", vjust = -1),
          plot.subtitle = element_text(hjust = .5, size = 10, family = "Outfit", face = "bold", vjust = -4),
          plot.caption = element_text(hjust = .5, size = 10, family = "Outfit", face = "bold", vjust = 4)) +
    labs(title = "Spain Pass Reception Heatmap",
         subtitle = "2024 Euro Final Spain vs. England - July 14, 2024",
         caption = "Graphic: Stephan Teodosescu | Data: Stats Bomb") 

p1








