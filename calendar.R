#!/usr/bin/env Rscript

# Name: calendar.R
# Author: DMP
# Description:
#   Draws a full year calendar using ggplot.
#   Usage:
#     calendar.R 2023 

# imports -----------------------------------------------------------------

library(lubridate)
library(ggplot2)

# params ------------------------------------------------------------------

colors = c("#8dd3c7",
           "#ffffb3",
           "#bebada",
           "#fb8072")

extrafont::loadfonts()
options(lubridate.week.start = 1)

# year

if (interactive()){
  year_vec = c(2023)
} else {
  args = commandArgs(trailingOnly = TRUE)
  year_vec = args
}

# data --------------------------------------------------------------------

fs::dir_create("calendars")

for (year in year_vec){
  start_date = glue::glue("{year}-01-01")
  end_date = glue::glue("{year}-12-31")
  
  all_days = seq(as.Date(start_date), as.Date(end_date), "days") |> 
    as.Date()
  
  df = data.frame(
    date = all_days,
    day = day(all_days),
    weekday = wday(all_days, label=TRUE,),
    week = isoweek(all_days),
    month = month(all_days, label = TRUE, abbr = F),
    quarter = glue::glue("Q{quarter(all_days)}")
  )
  
  # script ------------------------------------------------------------------
  
  df[1:10,]$week = 
    ifelse(df[1:10,]$week > 50, 
           0, 
           df[1:10,]$week)
  
  mw = max(df$week)
  
  df[nrow(df):(nrow(df)-10),]$week = 
    ifelse(df[nrow(df):(nrow(df)-10),]$week < 5, 
           mw+1, 
           df[nrow(df):(nrow(df)-10),]$week)
  
  mw = max(df$week)
  df$week_fct = factor(glue::glue("w{df$week}"), 
                    levels = glue::glue("w{mw:0}"))
  
  pdf(NULL)
  ggplot(
    df, aes(x = weekday, y = week_fct)
  ) + 
    geom_tile(aes( fill = quarter), alpha = 0.3) + 
    geom_text(aes(label = day), family = "Montserrat") + 
    facet_wrap(~month, scales = "free", ncol=3) + 
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "gray95"),
          legend.position = "none",
          axis.text = element_text(family = "Montserrat"),
          strip.text = element_text(size = 12, face = "bold",
                                    family = "SF Pro Display"),
          plot.title = element_text(size = 30,
                                    family = "SF Pro Display",
                                    hjust = .5,face = "bold")) +
    labs(title = glue::glue("{year}")) + 
    scale_fill_manual(values=colors)
  
  output_fn = glue::glue("calendars/calendar_{year}.png")
  
  ggsave(
    output_fn,
    dpi = 400,
    width = 8.3, height = 10.5
  )
  
  ggplot(data =  subset(df, quarter == "Q4"),
         aes(x = weekday, y = week_fct)
  ) + 
    geom_tile(color = "black", size = .5, fill = "white") + 
    geom_text(aes(label = day), size = 2, 
              family = "Montserrat", nudge_x = -.4, nudge_y = .2) + 
    facet_wrap(month~., scales = "free", ncol=1) + 
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(linetype = "solid"),
          legend.position = "none",
          axis.text = element_text(family = "Montserrat"),
          strip.text = element_text(size = 12, face = "bold",hjust = 0,
                                    family = "SF Pro Display")) 
  
  output_fn = glue::glue("calendars/calendar_{year}_Q4.png")
  
  ggsave(
    output_fn,
    dpi = 400,
    units = "mm",
    width = 130,
    height = 190
  )

}
