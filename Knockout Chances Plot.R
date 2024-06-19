###### Euros 2024 Knockout Qualification Plots
###### By: Stephan Teodosescu
###### JUne 2024 #####

library(tidyverse)
library(ggalt) # for dummbell plots
library(magick)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(gganimate)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)



# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
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

sim_deltas <- read_csv('/Users/Stephan/Desktop/R Projects/Euros-2024/euro_sim_deltas.csv')

team_mapping <- read_csv('https://raw.githubusercontent.com/steodose/Euros-2024/master/team_mapping.csv')



## 1. ---------------- Make Dummbell PLot --------------------


# Process final dataframe
sim_deltas <- sim_deltas %>%
  left_join(team_mapping, by = c("team" = "team")) %>% # join team mapping file
  mutate(logo_path = paste0(here::here("flags/"), team, ".png")) %>% # source country logos
  mutate(logo_label = glue::glue("<img src='{logo_path}' width='12'/>")) %>% # create html tags for geom_richtext to read
  mutate(across(starts_with("r16"), ~ . * 100)) # Convert R16 relevant columns to percentages


# final plot
sim_deltas %>% 
  mutate(team = fct_reorder(team, r16_post)) %>%
  ggplot() +
  geom_segment(aes(x = r16_pre, xend = r16_post, y = team, yend = team), color = "#b2b2b2", size = 2) +
  geom_dumbbell(aes(x = r16_pre, xend = r16_post, y = team), 
                color = NA, size_x = 5, size_xend = 5, 
                colour_x = "#2E86C1", colour_xend = "#CB4335",
                dot_guide=TRUE, dot_guide_size=0.25) +
  geom_richtext(
    aes(x = -0.0, y = team, label = logo_label, hjust = 1),
    label.size = 0, fill = NA
  ) +
  labs(title = "**Romania's Knockout Chances Drastically Improved**", 
       subtitle = glue("Difference between <span style = 'color:#2E86C1'>**pre-tournament odds**</span> and <span style = 'color:#CB4335'>**odds after Matchday 1**</span> in reaching the knockouts. Data as of **June 18**."),
       caption = "Data: Luke Benz (@recspecs730)\nGraphic: @steodosescu", 
       x = "Chance of Reaching Round of 16",
       y = "") +
  geom_text(data=sim_deltas, aes(x= r16_post, y=team, label=sprintf("%.0f%%", r16_post)),
            color="#CB4335", size=2.75, vjust=2.5, family="Titillium Web", fontface = "bold") +
  geom_text(data=sim_deltas, aes(x = r16_pre, y=team, label=sprintf("%.0f%%", r16_pre)),
            color="#2E86C1", size=2.75, vjust=2.5, family="Titillium Web", fontface = "bold") +
  theme_custom() +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.title.position = 'plot', 
        plot.title = element_markdown(face = 'bold', size = 20, hjust = 0.5),
        plot.subtitle = element_markdown(margin = margin(t = 3, b = 2, unit = "mm"), hjust = 0.5),
        panel.grid.major.x=element_line(size=0.05),
        panel.grid.major.y=element_blank())

ggsave("Knockout Chances Plot.png", height = 8, device = ragg::agg_png, dpi = 300)

# Add Euros logo to plot
knockout_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/Euros-2024/Knockout Chances Plot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/Euros-2024/UEFA_Euro_2024_Logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(knockout_plot_with_logo, "Knockout Chances Plot with Logo.png")

