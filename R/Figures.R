# This script makes the figures for the fish censusing paper (except for the 
# multivariate plot, which was done in the Analysis.R file)

source('Source.R')

# Load libraries
library(brms) # read and wrangle model objects
library(patchwork) # stitch plots together
library(tidybayes) # use summarising functions
library(ggnewscale) # use two different colour aesthetics
library(sf) # plot a map
library(ggspatial) # add scale bar and north arrow

# Map --------------------------------------------------------------------------
# Read in GBR shapefile
gbr_shp <- 
  st_read('~/Desktop/Mapping/Shapefiles/GBR_Land/Great_Barrier_Reef_Features_(GDA94).shp')

isl_shp <- 
  gbr_shp %>% 
  # We only want the land mass for Orpheus and Fantome islands
  dplyr::filter(str_detect(GBR_NAME, c('Orpheus|Fantome')),
                GBR_NAME != 'Fantome Rocks') %>% 
  # create descriptor for reef
  mutate(reef_cat = case_when(str_detect(GBR_NAME, 'Reef') ~ 'reef',
                              TRUE ~ 'land'))

# Create a dataframe of survey locations
survey_coordinates <- 
  tibble(location = c('Orpheus', 'Fantome'),
         lon = c(146.4853, 146.5102),
         lat = c(-18.6077, -18.6603)) %>% 
  # convert to spatial points
  sf::st_as_sf(coords = c('lon', 'lat'),
               # First set to WGS84
               crs = st_crs(4326)) %>% 
  # reproject to the same as isl_shp
  st_transform(crs = st_crs(isl_shp)) %>% 
  # bring back column names for plotting
  mutate(lon = sf::st_coordinates(.)[, 1],
         lat = sf::st_coordinates(.)[, 2]) %>% 
  # remove geometry
  st_drop_geometry()

# Make a dataframe of island names
island_names <- 
  tibble(location = c('Orpheus Island', 'Fantome Island'),
         lon = c(146.515, 146.485),
         lat = c(-18.5615, -18.71)) %>% 
  # convert to spatial points
  sf::st_as_sf(coords = c('lon', 'lat'),
               # First set to WGS84
               crs = st_crs(4326)) %>% 
  # reproject to the same as isl_shp
  st_transform(crs = st_crs(isl_shp)) %>% 
  # bring back column names for plotting
  mutate(lon = sf::st_coordinates(.)[, 1],
         lat = sf::st_coordinates(.)[, 2]) %>% 
  # remove geometry
  st_drop_geometry()

# Island-specific map
isl_map <- 
  ggplot() +
  geom_sf(data = isl_shp, aes(colour = reef_cat, fill = reef_cat)) +
  # Add survey points
  geom_point(data = survey_coordinates, 
             aes(x = lon, y = lat, shape = location),
             size = 3, fill = '#ff4d4d') +
  geom_text(data = island_names,
            aes(x = lon, y = lat, label = location),
            size = 3, colour = 'grey20') +
  scale_shape_manual(values = c(21, 23)) +
  scale_x_continuous(breaks = c(146.47, 146.53)) +
  scale_y_continuous(breaks = c(-18.7, -18.6)) +
  scale_fill_manual(values = c('grey40', '#98bed0')) +
  scale_colour_manual(values = c('grey40', '#98bed0')) + #a9cada
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, colour = '#ff4d4d', linewidth = 1) +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, colour = '#ff4d4d', linewidth = 1) +
  labs(shape = '',
       fill = '') +
  coord_sf(xlim = c(146.44, 146.56),
           ylim = c(-18.73, -18.55),
           expand = FALSE) +
  # add scale bar
  ggspatial::annotation_scale(location = 'bl',
                              bar_cols = c('grey80', 'white'),
                              line_col = 'grey60',
                              text_col = 'grey20',
                              height = unit(0.15, 'cm')) +
  publication_theme(background_fill = NA,
                    background_colour = NA,
                    axis_text_size = 10) +
  theme(axis.title = element_blank(),
        axis.line = element_line(colour = '#ff4d4d'),
        legend.position = 'none')

isl_map

# Create a df of cities to orientate on GBR
city_coordinates <- 
  tibble(location = c('Townsville', 'Cairns'),
         lat = c(-19.2581, -16.8824),
         lon = c(146.8157, 145.7368))

# Create base map of entire GBR
gbr_map <- 
  ggplot() +
  geom_sf(data = gbr_shp %>% 
            mutate(reef_cat = case_when(FEAT_NAME == 'Reef' ~ 'reef',
                                        TRUE ~ 'land')),
          aes(fill = reef_cat, colour = reef_cat)) +
  geom_rect(aes(ymin = -18.5365, ymax = -18.741, xmin = 146.416, xmax = 146.5915), 
            colour = '#ff4d4d', fill = NA) +
  # Add in text and point showing where Townsville and Cairns are
  geom_point(data = city_coordinates, aes(x = lon, y = lat), 
             size = 2) +
  geom_text(data = city_coordinates,
            aes(x = lon - 0.1, y = lat, label = location),
            size = 3, hjust = 1) +
  coord_sf(xlim = c(142, 153),
           ylim = c(-25, -10.34855),
           expand = FALSE) +
  scale_fill_manual(values = c('grey40', '#98bed0')) +
  scale_colour_manual(values = c('grey40', '#98bed0')) +
  annotate('text', x = 146, y = -23, label = 'Queensland', size = 6, colour = 'grey60', 
           fontface = 'italic') +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, colour = 'grey60', linewidth = 1) +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, colour = 'grey60', linewidth = 1) +
  # We're going to manually add this because exporting as a PDF and opening in AI is f'ed up
  annotate('segment', x = 146.416, xend = 148.32, y = -18.5365, yend = -10.52,
           colour = '#ff4d4d', linewidth = 0.3, alpha = 0.5) +
  annotate('segment', x = 146.5915, xend = 152.68, y = -18.741, yend = -17.07,
           colour = '#ff4d4d', linewidth = 0.3, alpha = 0.5) +
  # add scale bar
  ggspatial::annotation_scale(location = 'bl',
                              bar_cols = c('grey80', 'white'),
                              line_col = 'grey60',
                              text_col = 'grey80') +
  # add north arrow - they all look like shit but whatever
  ggspatial::annotation_north_arrow(location = 'bl',
                                    height = unit(1, 'cm'),
                                    width = unit(0.9, 'cm'),
                                    pad_x = unit(0.37, 'in'),
                                    pad_y = unit(0.35, 'in'),
                                    which_north = 'true',
                                    style = ggspatial::north_arrow_orienteering(fill = c('grey80', 
                                                                                         'white'),
                                                                                line_col = 'grey60',
                                                                                text_col = 'grey80')) +
  publication_theme() +
  theme(axis.title = element_blank(),
        legend.position = 'none')

gbr_map

# Inset Orpheus map onto GBR map
survey_map <- 
  gbr_map + inset_element(isl_map, left = 0.45, bottom = 0.5, right = 1, top = 1)

survey_map

ggsave(filename = '../Figures/Map.png', plot = survey_map)

# Biomass, abundance, richness -------------------------------------------------
# Read in the models
abun_mod <- readRDS('../Model_outputs/Community_abundance.rds')
biomass_mod <- readRDS('../Model_outputs/Community_biomass.rds')
rich_mod <- readRDS('../Model_outputs/Community_richness.rds')

# Read in the data
survey_clean <- read_csv('../Data/Survey_clean.csv')

head(survey_clean)
preview(survey_clean)

# OK to make this plot, we want to have three panels (biomass, abundance, and
# species richness). In each plot, we'll have the method on the x axis and will
# have two clusters of points per method (Orpheus and Fantome). We'll have the
# raw data in the background and use different shapes and colour shades for
# the different locations
# Because all of our explanatory variables are categorical, we can use the draws
# Create a model list to loop through
model_list <- list(abun_mod, biomass_mod, rich_mod)
# Create an output list for plots
model_plots <- list()
# Create an output list for coefficients
coef_values <- list()

for (i in 1:length(model_list)) {
  
  # Plot-specific aesthetics: 1 = top, 2 = middle, 3 = bottom
  if (i == 1) {
    raw_points <- # Raw data points
      geom_point(aes(y = log(Abundance/Area), colour = method, fill = method),
                 position = position_jitterdodge(dodge.width = 0.5,
                                                 jitter.width = 0.3,
                                                 seed = 123),
                 size = 3, alpha = 0.6, stroke = NA) 
    logticks <- annotation_logticks(side = 'l', colour = 'grey60') # add annotation logticks
    my_legend_position <- theme(legend.position = 'top', # legend position
                                legend.direction = 'horizontal')
    y_axis_labels <- scale_y_continuous(breaks = seq(-3, 0, 1), # yaxis labels
                                        labels = signif(exp(seq(-3, 0, 1)), 2)) 
    x_title <- '' # x axis title
    y_title <- expression('Abundance (indiv. m'^-2*')') # y axis title
    plot_tag <- expression(bold('a')) # plot tag
    title_vjust <- -7
  } else if (i == 2) {
    raw_points <- # Raw data points
      geom_point(aes(y = log(Biomass/Area), colour = method, fill = method),
                 position = position_jitterdodge(dodge.width = 0.5,
                                                 jitter.width = 0.3,
                                                 seed = 123),
                 size = 3, alpha = 0.6, stroke = NA) 
    logticks <- annotation_logticks(side = 'l', colour = 'grey60')
    my_legend_position <- theme(legend.position = 'none')
    y_axis_labels <- scale_y_continuous(breaks = seq(0, 6, 2), 
                                        labels = signif(exp(seq(0, 6, 2)), 2)) 
    x_title <- ''
    y_title <- expression('Standing biomass (g m'^-2*')')
    plot_tag <- expression(bold('b'))
    title_vjust <- NULL
  } else {
    raw_points <- # Raw data points
      geom_point(aes(y = Richness/Area, colour = method, fill = method),
                 position = position_jitterdodge(dodge.width = 0.5,
                                                 jitter.width = 0.3,
                                                 seed = 123),
                 size = 3, alpha = 0.6, stroke = NA) 
    logticks <- NULL
    my_legend_position <- theme(legend.position = 'none')
    y_axis_labels <- NULL
    x_title <- 'Method'
    y_title <- expression('Species richness (spp. m'^-2*')')
    plot_tag <- expression(bold('c'))
    title_vjust <- NULL
  }
  
  # Create a dataframe of coefficients
  coef_values[[i]] <- 
    rbind(as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
            as_tibble() %>% 
            mutate(point_instant = b_Intercept + b_Distance_timeInstant,
                   point_10min = b_Intercept,
                   trans_50m = b_Intercept + b_Distance_time50m,
                   trans_20m = b_Intercept + b_Distance_time20m,
                   trans_30m = b_Intercept + b_Distance_time30m,
                   Location = 'Fantome_Island') %>% 
            dplyr::select(-starts_with('b_'), -.chain, -.iteration, -.draw),
          as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
            as_tibble() %>% 
            mutate(point_instant = b_Intercept + b_Distance_timeInstant + b_LocationOrpheus_Island,
                   point_10min = b_Intercept + b_LocationOrpheus_Island,
                   trans_50m = b_Intercept + b_Distance_time50m + b_LocationOrpheus_Island,
                   trans_20m = b_Intercept + b_Distance_time20m + b_LocationOrpheus_Island,
                   trans_30m = b_Intercept + b_Distance_time30m + b_LocationOrpheus_Island,
                   Location = 'Orpheus_Island') %>% 
            dplyr::select(-starts_with('b_'), -.chain, -.iteration, -.draw)) %>% 
    # Calculate the 50 and 90% credible intervals
    pivot_longer(-Location, names_to = 'method', values_to = 'values') %>% 
    group_by(Location, method) %>% 
    median_hdci(.width = c(0.5, 0.9)) %>% 
    ungroup() %>% 
    # clean up the method titles
    mutate(method = case_when(method == 'point_10min' ~ 'Point count\n(10 min)',
                              method == 'point_instant' ~ 'Point count\n(instant)',
                              method == 'trans_50m' ~ 'Transect\n(50 m)',
                              method == 'trans_20m' ~ 'Transect\n(20 m)',
                              method == 'trans_30m' ~ 'Transect\n(30 m)'),
           method = factor(method, levels = c('Point count\n(instant)',
                                              'Point count\n(10 min)',
                                              'Transect\n(20 m)',
                                              'Transect\n(30 m)',
                                              'Transect\n(50 m)')))
  
  # Generate the plot
  model_plots[[i]] <- 
    ggplot(survey_clean %>% 
             mutate(method = case_when(Distance_time == '10min' ~ 'Point count\n(10 min)',
                                       Distance_time == 'Instant' ~ 'Point count\n(instant)',
                                       Distance_time == '50m' ~ 'Transect\n(50 m)',
                                       Distance_time == '20m' ~ 'Transect\n(20 m)',
                                       Distance_time == '30m' ~ 'Transect\n(30 m)'),
                    method = factor(method, levels = c('Point count\n(instant)',
                                                       'Point count\n(10 min)',
                                                       'Transect\n(20 m)',
                                                       'Transect\n(30 m)',
                                                       'Transect\n(50 m)'))),
           aes(x = method, group = Location, shape = Location)) +
    raw_points +
    # Errorbar
    geom_errorbar(data = coef_values[[i]], aes(ymin = .lower, ymax = .upper, linewidth = .width),
                  width = 0, lineend = 'round', 
                  position = position_dodge(width = 0.5)) +
    # Median estimate
    geom_point(data = coef_values[[i]] %>% 
                 # avoid duplicates
                 dplyr::select(Location, method, values) %>% 
                 distinct(), aes(y = values, fill = method),
               size = 3, stroke = 1,
               position = position_dodge(width = 0.5)) +
    # Plot details
    scale_colour_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00')) +
    scale_fill_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00')) +
    scale_linewidth_continuous(breaks = c(0.5, 0.9),
                               range = c(2, 0.8)) +
    scale_shape_manual(values = c(21, 23),
                       labels = c('Fantome Island',
                                  'Orpheus Island')) +
    y_axis_labels +
    logticks +
    annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
    annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
    # Turn off two legends
    guides(colour = 'none',
           fill = 'none',
           linewidth = 'none',
           shape = guide_legend(override.aes = list(fill = 'grey40',
                                                    colour = 'grey40'))) +
    labs(x = x_title,
         y = y_title,
         size = '',
         shape = '',
         title = plot_tag) +
    publication_theme(grid_colour = 'grey90') +
    my_legend_position +
    theme(plot.title = element_text(hjust = -0.1, 
                                    vjust = title_vjust),
          axis.title.x = element_text(vjust = -0.1))
  
}

model_plots[[1]]
model_plots[[2]]
model_plots[[3]]

model_plot <- 
  # Because we added our own plot tags, we'll need negative plot spacers
  plot_spacer() + model_plots[[1]] + 
  plot_spacer() + model_plots[[2]] + 
  plot_spacer() + model_plots[[3]] +
  plot_layout(ncol = 1, heights = c(-0.15, 1, -0.15, 1, -0.15, 1)) 

#model_plot

ggsave('../Figures/Abun_biom_rich_fit.png', model_plot, height = 12, width = 8)

### Results --------------------------------------------------------------------
summary(abun_mod)
summary(biomass_mod)
summary(rich_mod) 

# Pull out the coefficients estimated from above and clean it up

med_location_vals <- list()

for (i in 1:length(model_list)) {
  
  if (names(model_list[[i]]$data)[1] %in% c('Abundance', 'Biomass')) {
    link_function <- exp
  } else {
    link_function <- as.numeric
  }
  
  med_location_vals[[i]] <- 
    rbind(as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
            as_tibble() %>% 
            mutate(point_instant = b_Intercept + b_Distance_timeInstant,
                   point_10min = b_Intercept,
                   trans_50m = b_Intercept + b_Distance_time50m,
                   trans_20m = b_Intercept + b_Distance_time20m,
                   trans_30m = b_Intercept + b_Distance_time30m,
                   Location = 'Fantome_Island') %>% 
            dplyr::select(-starts_with('b_'), -.chain, -.iteration),
          as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
            as_tibble() %>% 
            mutate(point_instant = b_Intercept + b_Distance_timeInstant + b_LocationOrpheus_Island,
                   point_10min = b_Intercept + b_LocationOrpheus_Island,
                   trans_50m = b_Intercept + b_Distance_time50m + b_LocationOrpheus_Island,
                   trans_20m = b_Intercept + b_Distance_time20m + b_LocationOrpheus_Island,
                   trans_30m = b_Intercept + b_Distance_time30m + b_LocationOrpheus_Island,
                   Location = 'Orpheus_Island') %>% 
            dplyr::select(-starts_with('b_'), -.chain, -.iteration)) %>% 
    # Convert all to real space
    mutate_if(is.numeric, link_function) %>% 
    # Calculate the 50 and 90% credible intervals
    pivot_longer(cols = -c(.draw, Location), names_to = 'method', values_to = 'values') %>% 
    # Take the average between the two locations
    group_by(method, .draw) %>% 
    summarise(values = median(values)) %>% 
    ungroup() %>% 
    # now calculate uncertainty
    group_by(method) %>% 
    median_hdci(.width = 0.9) %>% 
    ungroup() %>% 
    # remove useless columns
    dplyr::select(-.width, -.point, -.interval) %>% 
    # specify response variable
    mutate(model = names(model_list[[i]]$data)[1]) %>% 
    # round everything to 2 decimal places
    mutate_if(is.numeric, ~ round(., 2))
  
}

med_location_vals[[1]]
med_location_vals[[2]]
med_location_vals[[3]]

ggplot(med_location_vals[[1]], aes(x = values, y = method)) +
  geom_errorbar(aes(xmin = .lower, xmax = .upper), width = 0)
ggplot(med_location_vals[[2]], aes(x = values, y = method)) +
  geom_errorbar(aes(xmin = .lower, xmax = .upper), width = 0)
ggplot(med_location_vals[[3]], aes(x = values, y = method)) +
  geom_errorbar(aes(xmin = .lower, xmax = .upper), width = 0)


# We want the 90% credible intervals of each model
# Estimated coefficients
for (i in 1:length(model_list)) {
  
  coef_df <- 
    # exract the draws
    as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
    as_tibble() %>% 
    # calculate 90% CI
    pivot_longer(everything(), names_to = 'beta', values_to = 'value') %>% 
    # get rid of metadata
    dplyr::filter(!beta %in% c('.chain', '.iteration', '.draw')) %>% 
    group_by(beta) %>% 
    median_hdci(value, .width = 0.9) %>% 
    ungroup() %>% 
    # add model name
    mutate(model = names(model_list[[i]]$data)[1]) %>% 
    # remove metadata
    dplyr::select(-.width, -.point, -.interval) %>% 
    # rename and reorder columns to make life easier
    mutate(beta = gsub('b_', '', beta),
           beta = gsub('Distance_time', '', beta),
           beta = factor(beta, levels = c('Intercept', 'LocationOrpheus_Island',
                                          'Instant', '20m', '30m', '50m'))) %>% 
    arrange(beta) %>% 
    # Round everything to 2 decimal places
    mutate_if(is.numeric, ~ round(., 2))
  
  print(coef_df)
  
}

# Convert coefficients into actual estimates and we want the average between the two locations
for (i in 1:length(model_list)) {
  
  coef_df <- 
    as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
    as_tibble() %>% 
    mutate(Fantome_Island_10min = b_Intercept,
           Orpheus_Island = b_Intercept + b_LocationOrpheus_Island,
           Point_instant = b_Intercept + b_Distance_timeInstant,
           Trans_20m = b_Intercept + b_Distance_time20m,
           Trans_30m = b_Intercept + b_Distance_time30m,
           Trans_50m = b_Intercept + b_Distance_time50m) %>% 
    dplyr::select(-starts_with('b_'), -.chain, -.iteration, -.draw)
  
  # If it's abundance or biomass, we want to exponentiate
  if (i %in% 1:2) {
    
    val_df <- 
      coef_df %>% 
      mutate_if(is.numeric, exp) %>% 
      pivot_longer(everything(), names_to = 'beta', values_to = 'value') %>% 
      group_by(beta) %>% 
      median_hdci(value, .width = 0.9) %>% 
      ungroup() %>% 
      # add model name
      mutate(model = names(model_list[[i]]$data)[1]) %>% 
      # remove metadata
      dplyr::select(-.width, -.point, -.interval) %>% 
      # Round everything to 2 decimal places
      mutate_if(is.numeric, ~ round(., 2))
    
  } else {
    
    val_df <- 
      coef_df %>% 
      pivot_longer(everything(), names_to = 'beta', values_to = 'value') %>% 
      group_by(beta) %>% 
      median_hdci(value, .width = 0.9) %>% 
      ungroup() %>% 
      # add model name
      mutate(model = names(model_list[[i]]$data)[1]) %>% 
      # remove metadata
      dplyr::select(-.width, -.point, -.interval) %>% 
      # Round everything to 2 decimal places
      mutate_if(is.numeric, ~ round(., 2))
    
  }
   
    print(val_df)
  
}

# Just want median difference for location based on how results were written
for (i in 1:length(model_list)) {
  
  if (i %in% 1:2) {
    link_function <- exp
  } else {
    link_functino <- as.numeric
  }
    df <- 
      as_draws_df(model_list[[i]], variable = '^b_', regex = TRUE) %>% 
      as_tibble() %>% 
      mutate(Orpheus = b_Intercept + b_LocationOrpheus_Island,
             Fantome = b_Intercept) %>% 
      dplyr::select(Orpheus, Fantome) %>% 
      mutate_if(is.numeric, link_function) %>% 
      pivot_longer(everything(), names_to = 'location', values_to = 'value') %>% 
      group_by(location) %>% 
      median_hdci(value, .width = 0.9) %>% 
      ungroup() %>% 
      # remove unnecessary columns
      dplyr::select(-.width, -.point, -.interval) %>% 
      mutate(model = names(total_mod_list[[i]]$data)[1]) %>% 
      mutate_if(is.numeric, ~ round(., 2))
  
    print(df)
}

# Size spectrum ----------------------------------------------------------------
# We want a plot showing the size spectrum slopes for all three methods across 
# both locations
ss_model <- readRDS('../Model_outputs/Size_spectrum.rds')

survey_bin <- 
  read_csv('../Data/Binned_sizes.csv')

# So we want to plot the draws from each method for the SS relationship
# perhaps throw on the raw data points
ss_fit <- 
  rbind(survey_bin %>% 
        group_by(Location, Distance_time) %>% 
        slice_min(log10bin_mid, with_ties = FALSE) %>% 
        ungroup() %>% 
        dplyr::select(log10bin_mid, Distance_time, Location) %>% 
        mutate(val = 'min'),
      survey_bin %>% 
        group_by(Location, Distance_time) %>% 
        slice_max(log10bin_mid, with_ties = FALSE) %>% 
        ungroup() %>% 
        dplyr::select(log10bin_mid, Distance_time, Location) %>% 
        mutate(val = 'max')) %>% 
  pivot_wider(names_from = 'val', values_from = 'log10bin_mid') %>% 
  mutate(log10bin_mid = purrr::map2(min, max, seq, length.out = 100)) %>% 
  unnest(cols = log10bin_mid) %>% 
  # add fitted draws
  add_epred_draws(ss_model, re_formula = NA, ndraws = 400, seed = 123) %>% 
  # clean up the order for plotting
  mutate(Distance_time = factor(Distance_time, levels = c('Instant', '10min', 
                                                          '20m', '30m', '50m')),
         Location = gsub('_', ' ', Location),
         # create grid separator
         method_gen = case_when(Distance_time %in% c('Instant', '10min') ~ 'Point counts',
                                TRUE ~ 'Transects')) %>% 
  # generate the plot
  ggplot(aes(x = log10bin_mid, y = .epred)) +
  # add raw data points
  geom_point(data = survey_bin %>% # reorder for plotting
               mutate(Distance_time = factor(Distance_time, levels = c('Instant', '10min', 
                                                                       '20m', '30m', '50m')),
                      Location = gsub('_', ' ', Location),
                      # create grid separator
                      method_gen = case_when(Distance_time %in% c('Instant', '10min') ~ 'Point counts',
                                             TRUE ~ 'Transects')),
             aes(y = log10w_norm, shape = Location, fill = Distance_time), 
             alpha = 0.5, position = position_jitter(width = 0.05, seed = 123),
             size = 3, stroke = NA) +
  # Fitlines
  geom_line(aes(group = paste(Distance_time, Location, .draw), colour = Distance_time), 
            alpha = 0.2, show.legend = FALSE) +
  scale_colour_manual(values = c('#ff6666', '#0099cc', '#79d2a6', '#d279d2', '#ffb84d')) +
  # Add lines around min and max values
  annotate('segment', x = max(survey_bin$log10bin_mid), xend = max(survey_bin$log10bin_mid),
           y = -Inf, yend = Inf, colour = 'grey40', linewidth = 1, linetype = 'dashed',
           lineend = 'round') +
  annotate('segment', x = min(survey_bin$log10bin_mid), xend = min(survey_bin$log10bin_mid),
           y = -Inf, yend = 0, colour = 'grey40', linewidth = 1, linetype = 'dashed',
           lineend = 'round') +
  # Median fit
  ggnewscale::new_scale_color() +
  geom_smooth(aes(colour = Distance_time), 
              method = 'lm', se = FALSE, linewidth = 2,
              lineend = 'round', show.legend = FALSE) +
  # Details
  scale_colour_manual(values = c('#e60000', '#006080', '#2d8659', '#ac39ac', '#e68a00')) +
  scale_fill_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00'),
                    labels = c('Instant',
                               '10 min',
                               '20 m',
                               '30 m',
                               '50 m')) +
  scale_shape_manual(values = c(21, 23)) +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = -Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 1, colour = 'grey60') +
  annotation_logticks(base = 10, sides = 'bl', colour = 'grey60') +
  guides(shape = 'none',
         fill = guide_legend(override.aes = list(shape = 22,
                                                 size = 5,
                                                 alpha = 1))) +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  labs(y = expression(log[10] ~ 'Normalised biomass (g m'^-2 * ')'),
       x = expression(log[10] ~ 'Biomass bin mid-point (g)'),
       fill = '') +
  facet_grid(cols = vars(method_gen), rows = vars(Location)) +
  #facet_wrap(~ Location, ncol = 2, strip.position = 'top') +
  publication_theme(strip_colour = NA,
                    strip_text_size = 14) +
  theme(legend.position = c(0.42, 0.9),
        strip.text.y = element_blank(),
        axis.line = element_blank())

ss_fit

# We're also going to plot the coefficients for each slope estimate
ss_coef <- 
  rbind(as_draws_df(ss_model, variable = '^b_', regex = TRUE) %>% 
        as_tibble() %>% 
        # Remove all the interaction colons - they get weird to code with
        select_all(~ gsub(':', '', .)) %>% 
        mutate(point_10min = b_log10bin_mid,
               point_instant = b_log10bin_mid + b_log10bin_midDistance_timeInstant,
               trans_20m = b_log10bin_mid + b_log10bin_midDistance_time20m,
               trans_30m = b_log10bin_mid + b_log10bin_midDistance_time30m,
               trans_50m = b_log10bin_mid + b_log10bin_midDistance_time50m,
               Location = 'Fantome Island'),
      as_draws_df(ss_model, variable = '^b_', regex = TRUE) %>% 
        as_tibble() %>% 
        # Remove all the interaction colons - they get weird to code with
        select_all(~ gsub(':', '', .)) %>% 
        mutate(point_10min = b_log10bin_mid + b_log10bin_midLocationOrpheus_Island,
               point_instant = b_log10bin_mid + b_log10bin_midDistance_timeInstant + 
                 b_log10bin_midLocationOrpheus_Island + 
                 b_log10bin_midDistance_timeInstantLocationOrpheus_Island,
               trans_20m = b_log10bin_mid + b_log10bin_midDistance_time20m + 
                 b_log10bin_midLocationOrpheus_Island +
                 b_log10bin_midDistance_time20mLocationOrpheus_Island,
               trans_30m = b_log10bin_mid + b_log10bin_midDistance_time30m +
                 b_log10bin_midLocationOrpheus_Island +
                 b_log10bin_midDistance_time30mLocationOrpheus_Island,
               trans_50m = b_log10bin_mid + b_log10bin_midDistance_time50m +
                 b_log10bin_midLocationOrpheus_Island +
                 b_log10bin_midDistance_time50mLocationOrpheus_Island,
               Location = 'Orpheus Island')) %>% 
  dplyr::select(-starts_with('b_'), -.chain, -.iteration, -.draw) %>% 
  # Calculate the 50 and 90% credible intervals
  pivot_longer(-Location, names_to = 'method', values_to = 'values') %>% 
  group_by(Location, method) %>% 
  median_hdci(.width = c(0.5, 0.9)) %>% 
  ungroup() %>% 
  # reverse the order of coefficients
  mutate(method = factor(method, levels = c('trans_50m', 'trans_30m', 'trans_20m',
                                            'point_10min', 'point_instant'))) %>% 
  ggplot(aes(x = values, y = method)) +
  geom_vline(aes(xintercept = 0), colour = 'grey60') +
  geom_errorbar(aes(xmin = .lower, xmax = .upper, size = .width),
                width = 0, lineend = 'round') +
  geom_point(aes(fill = method, shape = Location), size = 4, stroke = 1) +
  scale_size_continuous(breaks = c(0.5, 0.9),
                        range = c(2, 1)) +
  scale_fill_manual(values = c('#e68a00', '#bf40bf', '#339966', '#0099cc', '#ff6666')) +
  scale_shape_manual(values = c(21, 23)) +
  facet_wrap(~ Location, ncol = 1, strip.position = 'right') +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = -Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = Inf, y = -Inf, yend = -Inf, linewidth = 1, colour = 'grey60') +
  labs(y = '',
       x = 'Slope estimate') +
  publication_theme(strip_colour = NA,
                    strip_text_size = 14) +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = 'none')

ss_plot <- 
  ss_fit + plot_spacer() + ss_coef + 
  plot_layout(ncol = 3, widths = c(1, -0.075, 0.3))

ggsave('../Figures/SizeSpectra_plot.pdf', ss_plot, height = 7, width = 10)





































