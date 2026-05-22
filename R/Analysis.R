# This is the analysis script for the fish counting methods paper

# Load source code
source('Source.R')

# Load analysis libraries
library(brms) # run models
library(rstan) # optimize Stan
library(DHARMa) # assess models
library(vegan) # run multivariate analyses
library(car) # test collinearity

# optimize stan
rstan_options(auto_write = TRUE)
options(mc.cores = 4) 

# CLEANING -------------------------------------------------------------------------------
# Load in the data
# The data was collected in two separate dataframes
field_clean <-
  # There is an NA count from Sterling 
  read_excel('../Data/OrpheusCountsFeb23FishDataPart2.xlsx') %>% 
  # We'll just extract the day
  mutate(Day = lubridate::day(Date)) %>% 
  # Add rest of data
  rbind(read_excel('../Data/OrpheusCountsFeb23.xlsx') %>% 
          # We'll just extract the day
          mutate(Day = str_extract(Date, '^[^/]+'),
                 Day = as.numeric(Day))) %>% 
  # let's rename the columns to make them more code friendly
  dplyr::rename('Site' = `Specific Site`,
                'Count_type' = `Count Type`,
                'Distance_time' = `Distance/Time`,
                'Size_class_cm' = `Size Class (cm)`,
                'Abundance' = 'Count') %>% 
  # replace all the spaces with underscores
  mutate_at(vars(c('Location', 'Site', 'Species')), 
            function(x) gsub(' ', '_', x)) %>% 
  # clean up species names
  mutate(Species = case_when(Species == 'Acanthurus_sp.' ~ 'Acanthurus_sp',
                             Species == 'Cheotodon_lunulatus' ~ 'Chaetodon_lunulatus',
                             Species == 'chaetodon_auriga' ~ 'Chaetodon_auriga',
                             Species == 'Chaetodon_aphippium' ~ 'Chaetodon_ephippium',
                             Species == 'Chaetodon_plebius' ~ 'Chaetodon_plebeius',
                             Species == 'Chaetodon_spculum' ~ 'Chaetodon_speculum',
                             Species == 'Cheilinus_chlorurus' ~ 'Cheilinus_chlorourus',
                             Species == 'Chlorurus_microrhinus' ~ 'Chlorurus_microrhinos',
                             Species == 'Chlorurus_splurus' ~ 'Chlorurus_spilurus',
                             Species == 'Cromileptis_altivelis' ~ 'Chromileptes_altivelis',
                             Species == 'Diploprion_fasciatum' ~ 'Diploprion_bifasciatum',
                             Species == 'Fistularis_sp.' ~ 'Fistularia_sp',
                             Species == 'Epinephalus_fuscoguttatus' ~ 'Epinephelus_fuscoguttatus',
                             Species == 'Epiniphelus_quoyanus' ~ 'Epinephelus_quoyanus',
                             Species == 'Hemigymnnus_melapterus' ~ 'Hemigymnus_melapterus',
                             Species == 'Hippocarus_longiceps' ~ 'Hipposcarus_longiceps',
                             Species == 'Kyphosis_cinerascens' ~ 'Kyphosus_cinerascens',
                             Species == 'Kyphosis_vaigiensis' ~ 'Kyphosus_vaigiensis',
                             Species == 'Labrichthyes_unilineatus' ~ 'Labrichthys_unilineatus',
                             Species == 'Lutjanus_fulvis' ~ 'Lutjanus_fulvus',
                             Species == 'Lutjanus_russeli' ~ 'Lutjanus_russellii',
                             Species == 'Lutjanus_vita' ~ 'Lutjanus_vitta',
                             Species %in% c('Plectorhynchus_chaetodontoides',
                                            'Plectrorhynchus_chaetodontoides',
                                            'plectorhinchus_chaetodontoides') ~ 'Plectorhinchus_chaetodonoides',
                             Species == 'Plectorhynchus_gibbosus' ~ 'Plectorhinchus_gibbosus',
                             Species %in% c('Plectrohynchus_lineatus',
                                            'Plectrohinchus_lineatus') ~ 'Plectorhinchus_lineatus',
                             Species == 'Siganus_coralinus' ~ 'Siganus_corallinus',
                             Species == 'Siganus_vulpinis' ~ 'Siganus_vulpinus',
                             Species == 'Sphyraena_qunie' ~ 'Sphyraena_qenie',
                             Species == 'Thalasomma_lunare' ~ 'Thalassoma_lunare',
                             TRUE ~ Species),
         Species = gsub('_sp.', '_sp', Species),
         Location = case_when(Location == 'Fantom_Island' ~ 'Fantome_Island',
                              Location == 'Pioneer_Bay' ~ 'Orpheus_Island',
                              TRUE ~ Location),
         Size_class_cm = gsub('--', '-', Size_class_cm),
         Distance_time = case_when(Distance_time %in% c('20', '20 m') ~ '20m',
                                   Distance_time %in% c('30', '30 m') ~ '30m',
                                   Distance_time %in% c('10 mins',
                                                      '10 min') ~ '10min',
                                   TRUE ~ Distance_time)) %>% 
  # OK so some people recorded small to large and others did large to small
  # So we'll just separate the values into two columns and calculate the mean size
  separate(Size_class_cm, into = c('Size_lower_cm', 'Size_upper_cm'), sep = '-') %>% 
  mutate_at(vars(contains('Size')), as.numeric) %>% 
  # Calculate the mean size
  mutate(Size_mean = round((Size_lower_cm + Size_upper_cm)/2)) %>% 
  # Replace NA with 1 for now
  mutate(Abundance = case_when(is.na(Abundance) ~ 1,
                               TRUE ~ Abundance)) %>% 
  # Remove unnecessary columns for now
  dplyr::select(-Date, -Time, -Depth, -Size_lower_cm, -Size_upper_cm) 

field_clean <- 
  field_clean %>% 
  # Calculate the 50 m transect values and bind them to the OG dataset
  bind_rows(field_clean %>% 
              # Only do this for Transects
              dplyr::filter(Count_type == 'Transect') %>% 
              group_by(Day, Location, Site, Count_type, Replicate, Observer, 
                       Species, Size_mean) %>% 
              summarise(Abundance = sum(Abundance)) %>% 
              ungroup() %>% 
              mutate(Distance_time = '50m') %>% 
              dplyr::select(names(field_clean))) 

# Check how many NAs there are  
calc_na(field_clean) # Check the NA value from the OG dataset
head(field_clean)

# Now we want to add the LW regression coefficients
fish_lw <-
  tibble(species = gsub('_', ' ', unique(field_clean$Species)),
         a = NA,
         b = NA)

head(fish_lw)

# Generate progress bar
pb <- txtProgressBar(min = 0, max = nrow(fish_lw), style = 3)

for (i in 1:nrow(fish_lw)) {
  
  # Generate the dataframe
  lw_df <-
    rfishbase::length_weight(species_list = fish_lw$species[i]) %>% 
    as_tibble() %>% 
    dplyr::filter(Type == 'TL')
  
  # Impute LW regression coefficients a and b into the dataframe
  fish_lw$a[i] <- mean(lw_df$a, na.rm = TRUE)
  fish_lw$b[i] <- mean(lw_df$b, na.rm = TRUE)
  
  # Set the progress bar
  setTxtProgressBar(pb, i)
  
}
close(pb)

head(fish_lw)
calc_na(fish_lw)

fish_lw <-
  fish_lw %>% 
  arrange(species)

# It'll be easier to just save this dataframe and manually infill the values
#write.csv(fish_lw, '../Data/Species_LW.csv', row.names = FALSE)

# Now we'll calculate the biomass of all the fishes
species_clean <- 
  field_clean %>% 
  # For some reason these species names didn't change further up 
  mutate(Species = case_when(Species == 'Chlorurus_splurus' ~ 'Chlorurus_spilurus',
                             Species == 'Chaetodon_spculum' ~ 'Chaetodon_speculum',
                             TRUE ~ Species)) %>% 
  left_join(read_csv('../Data/Species_LW.csv') %>% 
              # Replace spaces with underscores
              mutate(species = gsub(' ', '_', species)),
            by = c('Species' = 'species')) %>% 
  # Calculate standing biomass
  mutate(Biomass_g = (a * Size_mean ^ b) * Abundance) %>% 
  # Maybe we can model this as a semi-random block design
  # Ale did 2 extra point censuses on the 15th
  mutate(keep = case_when(Day == 11 & Observer == 'ACS' & Replicate %in% c(1, 4) ~ 'remove',
                          TRUE ~ 'keep')) %>% 
  dplyr::filter(Replicate < 5,
                keep == 'keep') %>% 
  mutate(block_ID = paste(Day, Replicate, Observer, sep = '_'))

# Create a dataframe of survey-level values
survey_clean <- 
  species_clean %>% 
  # Calculate the total per survey
  group_by(Day, Observer, Location, Distance_time, Replicate, block_ID) %>% 
  summarise(Biomass = sum(Biomass_g),
            Abundance = sum(Abundance)) %>% 
  ungroup() %>% 
  # Add in species richness
  left_join(species_clean %>% 
              distinct(Distance_time, block_ID, Species) %>% 
              group_by(Distance_time, block_ID) %>% 
              count(name = 'Richness') %>% 
              ungroup(), by = c('Distance_time', 'block_ID')) %>% 
  # Add in genus richness
  left_join(species_clean %>% 
              mutate(Genus = str_extract(Species, '^[^_]+')) %>% 
              distinct(Distance_time, block_ID, Genus) %>% 
              group_by(Distance_time, block_ID) %>% 
              count(name = 'Genus_richness') %>% 
              ungroup(), by = c('Distance_time', 'block_ID')) %>% 
  # Add in area surveyed
  mutate(Area = case_when(Distance_time == '20m' ~ 100,
                          Distance_time == '30m' ~ 150,
                          Distance_time == '50m' ~ 250,
                          TRUE ~ 78.5)) 

head(survey_clean)

#write.csv(survey_clean, '../Data/Survey_clean.csv', row.names = FALSE)
survey_clean <- read_csv('../Data/Survey_clean.csv')
  
# Did everyone do the same amount each day?
survey_clean %>% 
  distinct(Day, Observer, Count_type, Distance_time, Replicate) %>% 
  group_by(Day, Observer, Count_type) %>% 
  count() %>% 
  ggplot(aes(x = Day, y = n, fill = Count_type)) +
  geom_bar(stat = 'identity', position = position_dodge()) + 
  facet_wrap(~ Observer, ncol = 4) +
  theme_light()

calc_na(survey_clean)
preview(survey_clean)

# We'll need to group some of the observations together, mainly the 50m transect was done
# As a 20m and a 30m transect

# Is there a way to quantify the probability of viewing the same individual?
ggplot(survey_clean %>% 
         dplyr::filter(Distance_time %in% c('Instant', '50m')) %>% 
         group_by(Day, Location, Count_type, Observer) %>% 
         summarise(total = sum(Density_abun)) %>% 
         ungroup(),
       aes(x = Day, y = total, colour = Count_type)) +
  geom_point() +
  geom_line(aes(group = paste(Observer, Count_type))) +
  facet_wrap(~ Location, ncol = 2, scales = 'free_x') +
  theme_light()

# ANALYSES -------------------------------------------------------------------------------
### Check collinearity ---------------------------------------------------------
car::vif(lm(Abundance/Area ~ Distance_time + Location, data = survey_clean))
car::vif(lm(Biomass/Area ~ Distance_time + Location, data = survey_clean))
car::vif(lm(Richness/Area ~ Distance_time + Location, data = survey_clean))

### Abundance, biomass, and richness -------------------------------------------
# Are there differences in the abundances estimated by the two methods?
# Compare all methods for abundance
abun_mod <- 
  brm(Abundance ~ Distance_time + Location + offset(log(Area)) + (1|block_ID) + (1|Observer), 
      family = negbinomial(link = 'log'),
      survey_clean,
      prior = c(prior(normal(0, 10), class = 'b'),
                prior(normal(0, 10), class = 'Intercept'),
                prior(student_t(3, 0, 2.5), class = 'sd'),
                prior(inv_gamma(0.4, 0.3), class = 'shape')),
      iter = 4000,
      warmup = 2000,
      thin = 2,
      seed = 123,
      control = list(adapt_delta = 0.999))

#saveRDS(abun_mod, '../Model_outputs/Community_abundance.rds')
abun_mod <- readRDS('../Model_outputs/Community_abundance.rds')

pp_check(abun_mod, ndraws = 100)

# Check resids
mod_pred <- 
  posterior_predict(abun_mod, ndraws = 400, summary = FALSE)

mod_resid <- 
  createDHARMa(simulatedResponse = t(mod_pred),
               observedResponse = abun_mod$data$Abundance,
               fittedPredictedResponse = apply(mod_pred, 2, mean),
               integerResponse = TRUE)

plot(mod_resid)

summary(abun_mod)

# Biomass
biomass_mod <- 
  brm(Biomass ~ Distance_time + Location + offset(log(Area)) + (1|block_ID) + (1|Observer),
      family = Gamma(link = 'log'), # lognormal(link = 'identity'),
      survey_clean,
      prior = c(prior(normal(0, 10), class = 'b'),
                prior(normal(0, 10), class = 'Intercept'),
                prior(student_t(3, 0, 2.5), class = 'sd'),
                prior(gamma(0.01, 0.01), class = 'shape')),
      iter = 4000,
      warmup = 2000,
      thin = 2,
      seed = 123,
      control = list(adapt_delta = 0.9999))

#saveRDS(biomass_mod, '../Model_outputs/Community_biomass.rds')
biomass_mod <- readRDS('../Model_outputs/Community_biomass.rds')

pp_check(biomass_mod, ndraws = 100)

# Check resids
mod_pred <- 
  posterior_predict(biomass_mod, ndraws = 400, summary = FALSE)

mod_resid <- 
  createDHARMa(simulatedResponse = t(mod_pred),
               observedResponse = biomass_mod$data$Biomass,
               fittedPredictedResponse = apply(mod_pred, 2, mean),
               integerResponse = FALSE)

plot(mod_resid)

summary(biomass_mod)

# Richness? - Q-Q plot actually looks ok
rich_mod <- 
  brm(Richness/Area ~ Distance_time + Location + (1|block_ID) + (1|Observer), # + offset(log(Area))
      family = 'gaussian',#negbinomial(link = 'log'),
      survey_clean,
      prior = c(prior(normal(0, 10), class = 'b'),
                prior(normal(0, 10), class = 'Intercept'),
                prior(student_t(3, 0, 2.5), class = 'sd'),
                prior(student_t(3, 0, 2.5), class = 'sigma')),
      iter = 4000,
      warmup = 2000,
      thin = 2,
      seed = 123,
      control = list(adapt_delta = 0.999,
                     max_treedepth = 12))

#saveRDS(rich_mod, '../Model_outputs/Community_richness.rds')
rich_mod <- readRDS('../Model_outputs/Community_richness.rds')

pp_check(rich_mod, ndraws = 100)

# Check resids
mod_pred <- 
  posterior_predict(rich_mod, ndraws = 400, summary = FALSE)

mod_resid <- 
  createDHARMa(simulatedResponse = t(mod_pred),
               observedResponse = rich_mod$data$`Richness/Area`,#rich_mod$data$Richness,
               fittedPredictedResponse = apply(mod_pred, 2, mean),
               integerResponse = FALSE)

plot(mod_resid, quantiles = FALSE)

summary(rich_mod)
  

### Multivariate -------------------------------------------------------------------------
# We need to take the dataframe and transpose it so we have a column of site-method
# and all species as individuals columns with their relative densities
community_multi <- 
  species_clean %>% 
  # Create a transect-level reference ID
  mutate(survey_id = paste(Observer, Day, Distance_time, Replicate, sep = '_')) %>% 
  # Calculate total abundance and biomass per survey per species
  group_by(Location, Distance_time, Observer, survey_id, Species) %>% 
  summarise(Abundance = sum(Abundance),
            Biomass = sum(Biomass_g)) %>% 
  ungroup() %>% 
  # Add area
  mutate(Area = case_when(Distance_time == '20m' ~ 100,
                          Distance_time == '30m' ~ 150,
                          Distance_time == '50m' ~ 250,
                          TRUE ~ 78.5),
         # Standardise estimates by area
         Abundance_m2 = Abundance/Area,
         Biomass_m2 = Biomass/Area) %>% 
  # Keep only the columns we want
  dplyr::select(Location, survey_id, Distance_time, Observer, 
                Species, Abundance_m2) %>%  #,   Biomass_m2
  # Put species across the rows
  pivot_wider(id_cols = everything(), 
              names_from = 'Species', 
              values_from = 'Abundance_m2',
              # Replace all NAs with 0
              values_fill = list(Abundance_m2 = 0))

# We ended up running a Canonical Analysis of Principal Coordiates constrained by observer
# and location because we wanted to assess the method-driven differences in communities but
# had heterogeneity of variances which didn't allow us to use a PERMANOVA

# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/rda-and-dbrda/

# First, we need to recalculate the distance matrix because it isnt stored in the
# metaMDS function
survey_std <- wisconsin(community_multi[, c(5:length(names(community_multi)))] ^ 0.25)
survey_dist <- vegdist(decostand(survey_std, method = 'total'), 'bray')

# PERMANOVA
survey_perm <- 
  adonis2(survey_dist ~ Distance_time, 
          data = community_multi, 
          distance = 'bray', 
          permutations = 999)

survey_perm

# Is this difference due to a difference in variance?
survey_disp <- 
  betadisper(survey_dist, community_multi$Distance_time)

plot(survey_disp)

disp_anova <- permutest(survey_disp, pairwise = TRUE) # same as anova(survey_disp)

# Look at pairwise comparisons
TukeyHSD(survey_disp)$group %>% 
  as_tibble(rownames = 'comparison') %>% 
  dplyr::filter(`p adj` <= 0.05) %>% 
  mutate_at(vars(c('diff', 'lwr', 'upr')), ~ round(., 2)) %>% 
  arrange(comparison)

# Positive values = left has greater dispersion than second
# Negative values = left has lower dispersion than second

abun_cap <- 
  # Condition() removes the effects 
  capscale(survey_dist ~ Distance_time + Condition(Observer) + Condition(Location),
           data = community_multi,
           distance = 'bray')

cap_sum <- summary(abun_cap)
# 9.56% of the variance explained by conditioned variables
# 4.86% of the variance explained by constraints

# Total variance explained by CAP1 = 0.7947*0.486 = 0.0386

cap_sum$biplot

anova(abun_cap)
anova(abun_cap, by = 'axis') # perform sig test for each constrained axis
anova(abun_cap, by = 'terms') # perform stepwise sig test for each term
anova(abun_cap, by = 'margin')

plot(abun_cap)

# Add species scores
sppscores(abun_cap) <- survey_std

# Make MV plots
# We're going to plot point census and transects separately but keep the entire polygons for both
plot_scores <- 
  scores(abun_cap, display = 'sites') %>% 
  as_tibble() %>% 
  mutate(Group = community_multi$Distance_time) %>% 
  # Reorder the methods
  mutate(Group = factor(Group, levels = c('Instant', '10min', '20m', '30m', '50m'))) %>% 
  ungroup()

point_mv_plot <- 
  ggplot(plot_scores,
         aes(x = CAP1, y = CAP2)) +
  # Add x and y axis lines
  geom_vline(aes(xintercept = 0), colour = 'grey80') +
  geom_hline(aes(yintercept = 0), colour = 'grey80') +
  # Add all points
  geom_point(data = plot_scores %>% 
               dplyr::filter(!Group %in% c('Instant', '10min')),
             colour = 'grey60') +
  # Add a convex hull for the whole area
  geom_polygon(data = plot_scores %>% 
                 dplyr::slice(chull(CAP1, CAP2)),
               alpha = 0.4, colour = NA, fill = 'grey80') +
  # Add convex hulls for point censuses
  geom_polygon(data = plot_scores %>% 
                 group_by(Group) %>% 
                 dplyr::slice(chull(CAP1, CAP2)) %>% 
                 ungroup() %>% 
                 dplyr::filter(Group %in% c('Instant', '10min')),
               alpha = 0.4, show.legend = FALSE,
               aes(colour = Group, fill = Group)) +
  # Add all points
  geom_point(data = plot_scores %>% 
               dplyr::filter(Group %in% c('Instant', '10min')),
             aes(colour = Group)) +
  scale_colour_manual(values = c('#ff6666', '#0099cc'),
                      labels = c('Point count (instant)',
                                 'Point count (10 min)')) +
  scale_x_continuous(limits = c(min(plot_scores$CAP1), max(plot_scores$CAP1))) +
  scale_y_continuous(limits = c(min(plot_scores$CAP2), max(plot_scores$CAP2))) +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  scale_fill_manual(values = c('#ff6666', '#0099cc')) +
  labs(x = 'dbRDA1',
       y = 'dbRDA2') +
  publication_theme() +
  theme(legend.position = 'none')

point_mv_plot

# CAP1 = 79.47%
# CAP2 = 10.40%

trans_mv_plot <- 
  ggplot(plot_scores,
         aes(x = CAP1, y = CAP2)) +
  # Add x and y axis lines
  geom_vline(aes(xintercept = 0), colour = 'grey80') +
  geom_hline(aes(yintercept = 0), colour = 'grey80') +
  # Add all points
  geom_point(data = plot_scores %>% 
               dplyr::filter(Group %in% c('Instant', '10min')),
             colour = 'grey60') +
  # Add a convex hull for the whole area
  geom_polygon(data = plot_scores %>% 
                 dplyr::slice(chull(CAP1, CAP2)),
               alpha = 0.4, colour = NA, fill = 'grey80') +
  # Add convex hulls for transects
  geom_polygon(data = plot_scores %>% 
                 group_by(Group) %>% 
                 dplyr::slice(chull(CAP1, CAP2)) %>% 
                 ungroup() %>% 
                 dplyr::filter(!Group %in% c('Instant', '10min')),
               alpha = 0.4, show.legend = FALSE,
               aes(colour = Group, fill = Group)) +
  # Add all points
  geom_point(data = plot_scores %>% 
               dplyr::filter(!Group %in% c('Instant', '10min')),
             aes(colour = Group)) +
  scale_x_continuous(limits = c(min(plot_scores$CAP1), max(plot_scores$CAP1))) +
  scale_y_continuous(limits = c(min(plot_scores$CAP2), max(plot_scores$CAP2))) +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  scale_colour_manual(values = c('#339966', '#bf40bf', '#e68a00')) +
  scale_fill_manual(values = c('#339966', '#bf40bf', '#e68a00')) +
  labs(x = 'dbRDA1',
       y = 'dbRDA2') +
  publication_theme() +
  theme(legend.position = 'none')

trans_mv_plot

# Species cap scores
cap_scores <- 
  scores(abun_cap, display = 'species') %>% 
  as_tibble(rownames = 'species') %>% 
  # Calculate influence using pythagorem's theorem
  mutate(influence = sqrt((CAP1^2) + (CAP2^2))) %>% 
  # we'll keep the top 10
  dplyr::slice_max(influence, n = 10)

cap_scores

spp_scores_plot <- 
  cap_scores %>% 
  left_join(species_clean %>% 
              # create a refid
              mutate(refid = paste(Site, Count_type, Replicate, Distance_time, Observer, Day, sep = '-')) %>% 
              dplyr::filter(Species %in% cap_scores$species) %>% 
              distinct(Species, refid) %>% 
              group_by(Species) %>% 
              count(name = 'total_blocks') %>% 
              ungroup() %>% 
              mutate(prop = total_blocks/470 * 100),
            by = c('species' = 'Species')) %>% 
  ggplot(aes(x = CAP1, y = CAP2)) +
  geom_vline(aes(xintercept = 0), colour = 'grey80') +
  geom_hline(aes(yintercept = 0), colour = 'grey80') +
  # Not going to do arrows because it's too busy
  geom_segment(aes(x = 0, xend = CAP1, y = 0, yend = CAP2), linewidth = 0.5) +
  geom_point(aes(size = prop)) +
  # We'll have to clean this up in AI
  geom_text_repel(data = . %>% 
                    mutate(species = case_when(species == 'Acanthurus_sp' ~ 'Acanthurus spp.',
                                               TRUE ~ species),
                           species = gsub('_', ' ', species)),
                  aes(label = species),
                  fontface = 'italic') + 
  # Set it all to the same scale
  scale_x_continuous(limits = c(min(plot_scores$CAP1), max(plot_scores$CAP1))) +
  scale_size_continuous(range = c(2, 10)) +
  #scale_y_continuous(limits = c(min(plot_scores$CAP2), max(plot_scores$CAP2))) +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  labs(x = 'dbRDA1',
       y = 'dbRDA2') +
  publication_theme() +
  theme(legend.position = 'none')

spp_scores_plot

# What's the proportion of surveys that contain each of these species?
prop_spp_plot <- 
  species_clean %>% 
  # create a refid
  mutate(refid = paste(Site, Count_type, Replicate, Distance_time, Observer, Day, sep = '-')) %>% 
  dplyr::filter(Species %in% cap_scores$species) %>% 
  distinct(Species, Distance_time, refid) %>% 
  group_by(Species, Distance_time) %>% 
  count(name = 'total_blocks') %>% 
  ungroup() %>% 
  mutate(prop = total_blocks/470 * 100,
         # Clean up species names
         Species = case_when(Species == 'Acanthurus_sp' ~ 'Acanthurus_spp.',
                             TRUE ~ Species),
         Species = gsub('_', ' ', Species),
         # Reorder methods
         Distance_time = case_when(Distance_time == 'Instant' ~ 'Point count (instant)',
                                   Distance_time == '10min' ~ 'Point count (10 min)',
                                   Distance_time == '20m' ~ 'Transect (20 m)',
                                   Distance_time == '30m' ~ 'Transect (30 m)',
                                   Distance_time == '50m' ~ 'Transect (50 m)'),
         Distance_time = factor(Distance_time, levels = c('Point count (instant)',
                                                          'Point count (10 min)',
                                                          'Transect (20 m)',
                                                          'Transect (30 m)',
                                                          'Transect (50 m)'))) %>% 
  ggplot(aes(x = prop, y = reorder(Species, prop), 
             fill = Distance_time)) + 
  geom_bar(stat = 'identity', position = 'stack', colour = 'white') +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_fill_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00')) +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, linewidth = 1, colour = 'grey60') +
  labs(fill = '',
       y = '',
       x = 'Proportion of surveys (%)') +
  publication_theme() +
  theme(legend.position = c(0.75, 0.2),
        axis.text.y = element_text(face = 'italic'))

#prop_spp_plot

abundance_cap <- 
  {point_mv_plot + trans_mv_plot} / {prop_spp_plot + spp_scores_plot}

ggsave('../Figures/Abundance_CAP.pdf', abundance_cap, height = 12, width = 14)

# Need to deal with the fact that the species scores are really clumped together
# I could remove the species names and use letters A-G to designate the species in the bar plot

# Calculate the 95% confidence interval around the center of the plot
abun_ord <- 
  ordiellipse(abun_cap, community_multi$Distance_time, kind = 'se', conf = 0.95,
              display = 'sites', label = TRUE)

# Create an empty dataframe to infill for ellipse
df_ell <- data.frame()
df_centroid <- data.frame() # Empty dataframe to infill with centroid points

for (i in unique(community_multi$Distance_time)) {
  
  df_ell <- 
    rbind(df_ell,
          cbind(as.data.frame(veganCovEllipse(cov = abun_ord[[i]]$cov,
                                              center = abun_ord[[i]]$center,
                                              scale = abun_ord[[i]]$scale)),
                Group = paste(i)))
  
  df_centroid <- 
    rbind(df_centroid,
          as_tibble(abun_ord[[i]]$center, rownames = 'val') %>% 
            # Pivot it wider
            pivot_wider(id_cols = everything(), names_from = 'val', values_from = 'value') %>% 
            mutate(Group = paste(i)))
  
}

df_ell
df_centroid


#abun_cap_plot <- 
  ggplot(scores(abun_cap, display = 'sites') %>% 
         as_tibble() %>% 
         mutate(Group = community_multi$Distance_time) %>% 
         # Reorder the methods
         mutate(Group = factor(Group, levels = c('Instant', '10min', '20m', '30m', '50m'))),
       aes(x = CAP1, y = CAP2, colour = Group, fill = Group, shape = Group)) +
  # Add x and y axis lines
  geom_vline(aes(xintercept = 0), colour = 'grey80') +
  geom_hline(aes(yintercept = 0), colour = 'grey80') +
  # Add a convex hull
  geom_polygon(data = scores(abun_cap, display = 'sites') %>% 
                 as_tibble() %>% 
                 mutate(Group = community_multi$Distance_time) %>% 
                 group_by(Group) %>% 
                 dplyr::slice(chull(CAP1, CAP2)) %>% 
                 ungroup() %>% 
                 mutate(Group = factor(Group, levels = c('Instant', '10min', '20m', '30m', '50m'))),
               alpha = 0.2, colour = NA, show.legend = FALSE) +
  # Add all points
  geom_point(alpha = 0.8) +
  # Add ellipse for 95% CI around centroid
  geom_polygon(data = df_ell %>% 
                 mutate(Group = factor(Group, levels = c('Instant', '10min', '20m', '30m', '50m'))),
               alpha = 0.6, show.legend = FALSE) +
  # Add centroid point
  geom_point(data = df_centroid, size = 4, colour = 'black', stroke = 1) +
  scale_shape_manual(values = c(22, 23, 24, 15, 14),
                     labels = c('Point count (instant)',
                                'Point count (10 min)',
                                'Transect (20 m)',
                                'Transect (30 m)',
                                'Transect (50 m)')) +
  scale_fill_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00'),
                    labels = c('Point count (instant)',
                               'Point count (10 min)',
                               'Transect (20 m)',
                               'Transect (30 m)',
                               'Transect (50 m)')) +
  scale_colour_manual(values = c('#ff6666', '#0099cc', '#339966', '#bf40bf', '#e68a00'),
                      labels = c('Point count (instant)',
                                 'Point count (10 min)',
                                 'Transect (20 m)',
                                 'Transect (30 m)',
                                 'Transect (50 m)')) +
  # Add borders around top and right
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, 
           linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, 
           linewidth = 1, colour = 'grey60') +
  scale_x_continuous(limits = c(-1.7, 1.7)) +
  scale_y_continuous(limits = c(-3.9, 3.9)) +
  labs(colour = '',
       shape = '',
       fill = '') +
  publication_theme() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.1, 0.95))

abun_cap_plot

#ggsave('../Figures/Abundance_CAP.pdf', abun_cap_plot, width = 10, height = 8)

library(ggrepel)

p2 <- 
  cap_scores %>% 
  # Pull out the top 10 scores
  dplyr::slice_max(influence, n = 10) %>% 
  ggplot(aes(x = CAP1, y = CAP2)) +
  # Add x and y axis lines
  geom_vline(aes(xintercept = 0), colour = 'grey80') +
  geom_hline(aes(yintercept = 0), colour = 'grey80') +
  # Add vector loadings
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, 'cm')),
               lineend = 'butt', linejoin = 'mitre') +
  #geom_text(aes(label = species)) +
  geom_text_repel(aes(label = species)) +
  # Add borders around top and right
  annotate('segment', x = Inf, xend = Inf, y = -Inf, yend = Inf, 
           linewidth = 1, colour = 'grey60') +
  annotate('segment', x = -Inf, xend = Inf, y = Inf, yend = Inf, 
           linewidth = 1, colour = 'grey60') +
  scale_x_continuous(limits = c(-1.32, 1.69)) +
  scale_y_continuous(limits = c(-2.79, 3.86)) +
  publication_theme()

abun_cap_plot / p2

# Run a PERMANOVA
# perform the multidimensional scale
# metaMDS automatically does a fourth root transformation and a wisconsin
# double standardization
survey_mds <- metaMDS(community_multi[, 5:length(names(community_multi))], plot = FALSE)

survey_mds$stress # there is 24.2% of variation that we are unable to explain for abundance
# 25.3% for biomass
stressplot(survey_mds)

# now we'll run the PERMANOVA - this test tells us if the means of the centroids
# are significantly different from one another
adonis2(survey_dist ~ Distance_time * Observer + Location,
        data = community_multi,
        distance = 'bray',
        permutations = 999)

# cool, now that we know that they're different, we can assess whether this
# difference is due to a difference in variance - ie. are the points spread
# differently between groups
survey_perm <- betadisper(survey_dist, community_multi$Distance_time)
plot(survey_perm)
anova(survey_perm)
# the variance between groups is not significantly different from one another

# What species are contibuting to these differences?
comp_sim <- 
  as.list(simper(survey_std, community_multi$Distance_time))


### Size comparison ----------------------------------------------------------------------
# Is there a consequence of using one dataset over the other to compare, say, differences
# between sites?

field_clean %>% 
  # expand abundances so we have one observation per abundance
  dplyr::slice(rep(row_number(), Abundance)) %>% 
  ggplot(aes(x = Size_mean, fill = Distance_time)) +
  geom_histogram(bins = 12, colour = 'grey40') +
  facet_wrap(~ Distance_time, ncol = 1) +
  theme_light()

field_clean %>% 
  group_by(Distance_time) %>% 
  dplyr::slice_max(Size_mean) %>% 
  ungroup() %>% 
  dplyr::select(Distance_time, Size_mean)

# Might be worth doing a size spectrum
# Let's look at the size spectrum of each survey method
# We'll need to convert the biomass to individual biomass and then
# expand the data frame to contain one row per individual surveyed
ss_survey_data <- 
  species_clean %>% 
  # create a reference ID 
  mutate(refid = paste(Distance_time, Observer, Day, Replicate, sep = '_'),
         Biomass_i = Biomass_g/Abundance) %>% 
  # Expand the dataframe by abundances
  dplyr::slice(rep(row_number(), Abundance))

preview(ss_survey_data)

# Create bin breaks on the log2 scale
bin_breaks <- 
  2^(floor(log2(min(ss_survey_data$Biomass_i))):ceiling(log2(max(ss_survey_data$Biomass_i))))

survey_bin <- 
  ss_survey_data %>% 
  # Create a column for the midpoint of each bin
  mutate(bin_mid = cut(Biomass_i,
                       breaks = bin_breaks,
                       # close intervals on the right
                       right = FALSE,
                       include.lowest = TRUE,
                       labels = bin_breaks[-length(bin_breaks)] + 0.5 * diff(bin_breaks)),
         # Create column for the min value
         bin_min = cut(Biomass_i,
                       breaks = bin_breaks,
                       right = FALSE,
                       include.lowest = TRUE,
                       labels = bin_breaks[-length(bin_breaks)]),
         # Column for max value
         bin_max = cut(Biomass_i,
                       breaks = bin_breaks,
                       right = FALSE,
                       include.lowest = TRUE,
                       labels = bin_breaks[-1])) %>% 
  # Convert them all to numeric
  mutate_at(vars(c('bin_mid', 'bin_min', 'bin_max')), ~ as.numeric(as.character(.))) %>% 
  # Generate normalised biomass size spectrum values
  # Calculate bin width
  mutate(bin_width = bin_max - bin_min) %>% 
  # Calculate the sum per site per year
  group_by(refid, bin_mid, bin_width) %>% 
  summarise(total_biomass = sum(Biomass_i)) %>% 
  ungroup() %>% 
  # Add in survey-level info
  left_join(ss_survey_data %>% 
              distinct(block_ID, refid, Location, Observer, Distance_time),
            by = 'refid') %>% 
  # Apply normalization - scale biomass by survey area
  mutate(Area = case_when(Distance_time == '20m' ~ 100,
                          Distance_time == '30m' ~ 150,
                          Distance_time == '50m' ~ 250,
                          TRUE ~ 78.5),
         total_biomass = total_biomass/Area,
         total_biomass_norm = total_biomass/bin_width) %>% 
  # Throw everything on the log10 scale and center the bin_mid
  mutate(log10bin_mid = scale(log10(bin_mid), scale = FALSE, center = TRUE)[, 1],
         log10w_norm = log10(total_biomass_norm)) %>% 
  # remove duplicates
  distinct() 

preview(survey_bin)

# Save this separately
write.csv(survey_bin, '../Data/Binned_sizes.csv', row.names = FALSE)

ggplot(survey_bin %>% dplyr::filter(!Distance_time %in% c('20m', '30m')),
       aes(x = log10bin_mid, y = log10w_norm, colour = Distance_time)) +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_light() +
  facet_wrap(~ Location, ncol = 2) 

# Looks like these different methods are producing definitely different intercepts
# but potentially different slopes, which change our interpretation of the underlying
# functioning

# Ok, let's model this
ss_model <- 
  brm(log10w_norm ~ log10bin_mid * Distance_time * Location + (1|block_ID) + 
        (1|Observer),
      family = 'gaussian',
      survey_bin,
      prior = c(prior(normal(0, 10), class = 'b'),
                prior(normal(0, 10), class = 'Intercept'),
                prior(student_t(3, 0, 2.5), class = 'sd'),
                prior(student_t(3, 0, 2.5), class = 'sigma')),
      iter = 4000,
      warmup = 2000,
      thin = 2,
      seed = 321,
      control = list(adapt_delta = 0.999,
                     max_treedepth = 12))

#saveRDS(ss_model, '../Model_outputs/Size_spectrum.rds')
ss_model <- readRDS('../Model_outputs/Size_spectrum.rds')

pp_check(ss_model, ndraws = 100)

# Check resids
mod_pred <- 
  posterior_predict(ss_model, ndraws = 400, summary = FALSE)

mod_resid <- 
  createDHARMa(simulatedResponse = t(mod_pred),
               observedResponse = ss_model$data$log10w_norm,
               fittedPredictedResponse = apply(mod_pred, 2, mean),
               integerResponse = FALSE)

plot(mod_resid)

summary(ss_model)

# SUPPLEMENTAL TABLE 1 -----------------------------------------------------------------------------
# get a family list
library(rfishbase)

fishbase_species <- 
  rfishbase::load_taxa()

species_list <- 
  field_clean %>% 
  distinct(Location, Species) %>% 
  mutate(Location = gsub('_', ' ', Location),
         Species = gsub('_', ' ', Species),
         Genus = word(Species, 1)) %>% 
  left_join(fishbase_species %>% 
              distinct(Genus, Family),
            by = 'Genus') %>% 
  # clean up family names
  mutate(Family = case_when(Family == 'Scaridae' ~ 'Labridae',
                            TRUE ~ Family)) %>% 
  dplyr::select(-Genus) %>% 
  dplyr::select(Location, Family, Species) %>% 
  arrange(Location, Family, Species)

species_list

write.csv(species_list, '../Data/Species_list.csv', row.names = FALSE)









