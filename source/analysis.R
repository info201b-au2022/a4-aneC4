library("tidyverse")
library("ggplot2")
library("leaflet")
library("maps")
library("mapproj")

# The functions might be useful for A4
source("../source/a4-helpers.R")
data <- get_data()

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#prevent use of scientific notation for large numbers
options(scipen=999)

#get data for latest year with prison data (2015).
latest <- data %>% 
  filter(year == 2015)
  
#get percent in jail or prison
pop_all <- sum(latest$total_pop)
pop_jail <- sum(latest$total_jail_pop, na.rm = T)
pop_prison <- sum(latest$total_prison_pop, na.rm = T)
pop_jail_prison <- pop_jail + pop_prison
pct_in_jail_prison <- round((pop_jail_prison/pop_all)*100,2)

#get data for oldest year (1970)
oldest <- data %>% 
  filter(year == min(year, na.rm = T))

pop_all_old <- sum(oldest$total_pop)
pop_jail_old <- sum(oldest$total_jail_pop, na.rm = T)
pop_prison_old <- sum(oldest$total_prison_pop, na.rm = T)
pop_jail_prison_old <- pop_jail_old + pop_prison_old

#get pct in jail or prison in 1970
pct_in_jail_prison_old <- round((pop_jail_old/pop_all_old)*100,2)

#get ratio of prison population between two
ratio <- round(pct_in_jail_prison/pct_in_jail_prison_old,2)

#get percentages of inmates that are of color/white
pop_jail_prison_white <- sum(latest$white_jail_pop, na.rm = T) + sum(latest$white_prison_pop, na.rm = T)

pct_in_jail_prison_white <- round((pop_jail_prison_white/pop_jail_prison)*100,2)
  
pct_in_jail_prison_of_color <- 100 - pct_in_jail_prison_white


# This function returns a dataframe of the total jail population by year
get_year_jail_pop <- function() {
  output <- data %>% 
    group_by(year) %>% 
    summarise(jail_pop = sum(total_jail_pop, na.rm = T))
  return(output)   
}

# This function plots the jail population by year as a bar graph
plot_jail_pop_for_us <- function() {
  plot <- ggplot(get_year_jail_pop(), aes(year,jail_pop)) + 
    geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
    labs(title="Jail Population (1970-2018)", 
         x = "Year", 
         y = "Jail Population",
         caption = "The Jail Population Increased Dramatically from the 1980's to 2000's") +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  return(plot)   
} 

# This function takes in a vector of states and returns the total jail population 
# each year in that state

get_jail_pop_by_states <- function(states){
  output <- data %>% 
    filter(state %in% states) %>% 
    group_by(year, state) %>% 
    summarize(year,state,jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
    distinct() 
    
  return(output)
}


# This function takes in a vector of states and plots the total jail population 
# each year in that state as a line graph
plot_jail_pop_by_states <- function(states){
  states_data <- get_jail_pop_by_states(states)
  
  plot <- ggplot(states_data, aes(x=year,y=jail_pop,group=state)) +
    geom_line(aes(linetype=state))+
    labs(title="Jail Population By State (1970-2018)", 
         x = "Year", 
         y = "Jail Population",
         caption = "Rural States tend to show Less Dropoff")
  
  return(plot)
}

# This function returns a dataframe of all counties' white to black ratio
# in comparison to its white to black ratio's comparison to the demographics
# of its jail population
get_pct_disproportionately_black_in_jail_vs_w_to_b_ratio <- function (){
  output <- data %>% 
    filter(year == max(year)) %>% 
    #get white,black pop
    summarize(white_pop_15to64, white_jail_pop, black_pop_15to64, black_jail_pop) %>%  
    #only get states with at least 1 white & 1 black person in jail (no infinities)
    filter(white_jail_pop > 1) %>% 
    filter(black_jail_pop > 1) %>% 
    #get white to black ratio in & out of jail
    mutate(w_to_b_ratio = white_pop_15to64/black_pop_15to64,
           w_to_b_ratio_jail = white_jail_pop/black_jail_pop) %>% 
    summarize(w_to_b_ratio,w_to_b_ratio_jail) %>% 
    group_by(w_to_b_ratio ) %>% 
    #compare white to black ratio to disproportionate rate of black jailings
    summarize(w_to_b_ratio, disproportionate_rate = w_to_b_ratio/w_to_b_ratio_jail) 
  
  return(output)
}

# This function creates a scatter plot of all counties' white to black ratio
# in comparison to their disproportionate rate of incarceration. includes
# a trendline.
plot_pct_disproportionately_black_in_jail_vs_w_to_b_ratio <- function (){
  data <- get_pct_disproportionately_black_in_jail_vs_w_to_b_ratio()
  
  plot <- ggplot(data, aes(x=w_to_b_ratio,y=disproportionate_rate)) +
    geom_point()+
    labs(
      title="White to Black Ratio of County vs. Disproportionate Rate of Imprisonment", 
         x = "White to black ratio",
         y = "Disproportionate rate",
         caption = "States with a higher white to black ratio tend to have more disproportionate rates of incarceration") + 
    scale_x_continuous(trans='log2') +
    scale_y_continuous(trans='log2') +
    geom_smooth(formula = y ~ x, method = "loess") #add linear trend line
  return(plot)
}


# This function gets the non-white jail population at a given year in each state
get_non_white_jail_pop_year <- function(yearAt){
  output <- data %>% 
    filter(year == yearAt) %>% 
    group_by(state) %>% 
    #get all non-white listed races
    summarize("state" = state.name[match(state,state.abb)], 
              "non_white_jail_pop" = sum(black_jail_pop,
                                         aapi_jail_pop,
                                         latinx_jail_pop,
                                         native_jail_pop,
                                         other_race_jail_pop, na.rm = T)) %>% 
    unique()
  
  #set to lowercase to match map data
  data$state = tolower(data$state)
  
  return(output)
}

# This function gets the increase in non-white jail population between two given years
# in each state
get_increase_in_black_jail_pop_by_state <- function(y1,y2){
  value_a <- get_non_white_jail_pop_year(y1)
  value_b <- get_non_white_jail_pop_year(y2)
  
  output <- value_b

  output$non_white_jail_pop = output$non_white_jail_pop - value_a$non_white_jail_pop
  #convert to percent
  output$non_white_jail_pop = output$non_white_jail_pop * 100 / value_b$non_white_jail_pop
  
  return(output)
}
# This function maps the increase in non-white jail population between two given years
# in each state
map_diff_in_black_jail_pop_by_state<- function(y1,y2) {
  #ommit nas and nans
  df <- na.omit(get_increase_in_black_jail_pop_by_state(y1,y2))
  df$state = tolower(df$state)
  
  #get map data
  state_shape <- map_data("state") %>% 
    rename(state = region) %>% 
    left_join(df, by = "state")
  
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
      
    )
  
  #create map
  map <- ggplot(state_shape)+
    geom_polygon(
      mapping = aes(x=long,
                    y=lat, 
                    group=group, 
                    fill = non_white_jail_pop),
      color = "white",
      size=.1
    )+
    coord_map()+
    scale_fill_gradient2(low = "#8080FF",
                          high = "#FF8080",
                          mid = "#808080",
                          midpoint = 0,
                         na.value="#CCCCCC") +
    labs(fill = paste("%Increase in Non-white Jail Population\n",y1,"-",y2))+
    blank_theme
  
  return(map)
}
