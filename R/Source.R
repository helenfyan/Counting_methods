# This is the source file for the counting methods paper

# Load libraries -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(rfishbase) # get LW regression coefficients

# Custom functions -----------------------------------------------------------------
# We'll rip-off and extract the function from vegan to create the ellipse
veganCovEllipse <- 
  function(cov, center, scale, n_points = 100) {
    
    require(vegan)
    
    theta <- (0:n_points) * 2 * pi/n_points
    Circle <- cbind(cos(theta), sin(theta))
    t(center + scale * t(Circle %*% chol(cov)))
  }

# Calculate the total number of NAs in a datafame
calc_na <-
  function(data_frame) {
    lapply(data_frame, function(x) sum(is.na(x)))
  }

# Preview large datasets
preview <- 
  function(data_frame, n_rows = 400) {
    data_frame %>% 
      dplyr::slice(1:n_rows) %>% 
      View()
  }

# Publication theme for figures
publication_theme <- 
  function(axis_title_size = 14, axis_text_size = 12, 
           plot_title_size = 16, title_hjust = 0,
           legend_text_size = 12, legend_title_size = 14, 
           strip_text_size = 12, legend_position = 'right',
           grid_colour = NA, background_fill = 'white',
           background_colour = 'white', strip_colour = 'grey60') {
    
    theme(plot.background = element_rect(fill = background_fill,
                                         colour = background_colour),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = grid_colour),
          panel.grid.minor = element_blank(),
          plot.title = element_text(colour = 'grey20', size = plot_title_size,
                                    hjust = title_hjust),
          strip.background = element_rect(fill = NA, colour = strip_colour,
                                          linewidth = 1),
          strip.text = element_text(colour = 'grey20', size = strip_text_size),
          axis.line = element_line(colour = 'grey60'),
          axis.ticks = element_line(colour = 'grey60'),
          axis.title = element_text(colour = 'grey20', size = axis_title_size),
          axis.text = element_text(colour = 'grey20', size = axis_text_size),
          legend.title = element_text(colour = 'grey20', size = legend_title_size),
          legend.text = element_text(colour = 'grey20', size = legend_text_size),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = legend_position) 
    
  }
