
####################
# CA MMP Analysis
## output synthetic control results tables and figures for paper
####################

# set up ------------------------------------------------------------------

# OAK_DIR <- Sys.getenv('OAK')
# setwd(file.path(OAK_DIR, 'EPA'))
setwd('~/sherlock_epa/')

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 
       'lubridate', 
       'stringr', 
      # 'sjmisc', # for str_contains
       'tidyverse', 
       'stargazer', 
       'ggpubr', 
       'ggplot2'
       )

# analysis folder
analysis_dir <- file.path('Analysis', 'ca_mmp')
tab_dir <- file.path(analysis_dir, 'tables')
fig_dir <- file.path(analysis_dir,  'figures', 'paper')

# model folder
model_name <- 'main'
syn_dir <- file.path(analysis_dir, 'output', 'synthetic_control_output')
# all_files <- list.files(syn_dir)
# input_dir <- file.path(syn_dir, all_files[str_detect(all_files, model_name)])
input_dir <- file.path(syn_dir, model_name)

# tables ------------------------------------------------------------------

# a) pretreatment balance ----------------------------------------------------
pre_balance <- read_csv(file.path(input_dir, 'synthetic_control_balance.csv'))

names(pre_balance) <- c('Variables', 'Real', 'Synthetic', 'US Mean')
pre_balance$Variables <- c('Effluent Violation Rate 1996',
                           'Effluent Violation Rate Lag 1996-1997',
                           'Effluent Violation Rate Lag 1997-1998',
                           'Effluent Violation Rate Lag 1998-1999',
                           'Effluent Violation Rate Lag 1999-2000',
                           'Effluent Violation Rate 2000',
                           'Inspection Rate Avg 1996-2000',
                           'Ratio of Major Facilities Avg 1996-2000',
                           'Ratio of POTW Facilities Avg 1996-2000',
                           'Ratio of Sewage Treatment Plants Avg 1996-2000')

stargazer(pre_balance,
          summary = FALSE,
          rownames = FALSE,
          out = file.path(tab_dir, 'pretreatment_balance.tex'))

# b) control states weights -----------------------------------------------
state_weights <- read_csv(file.path(input_dir, 'synthetic_control_state_weights.csv')) %>%
  rename(state_code = unit.names)

state_names <- read_csv(file.path('Data', 'manual', 'states_and_counties.csv')) %>%
  rename(state_name = `State Name`,
         state_code = `State Abbreviation`) %>%
  select(state_name, state_code) %>%
  distinct()

weight_table <- state_weights %>%
  left_join(state_names) %>%
  rename(Weight = w.weights,
         State = state_name) %>%
  mutate(Weight = round(Weight, 2)) %>%
  select(State, Weight) %>%
  arrange(-Weight)

# make into four columns for better display
cutoff <- ceiling(nrow(weight_table)/2)
left <- weight_table[1:cutoff, ]
right <- weight_table[(cutoff+1):nrow(weight_table),]
if (nrow(right) < nrow(left)){
  right <- right %>%
    add_row(State = ' ', Weight = NA)
}

stargazer(cbind(left, right),
          summary = FALSE,
          rownames = FALSE, 
          digits = 2, 
          out = file.path(tab_dir, 'state_weight.tex'))

# variables -----------------------------------------------------------------

## set color
color1 <- '#E6945C'
color2 <- '#5CA7E6'

intervention <- 2000
window <- seq(1996, 2003, 1)

# a) control state vilolation rates ---------------------------------------
synth_data <- read_csv(file.path(input_dir, 'synthetic_control_data.csv'))
results <- readRDS(file.path(input_dir, 'synthetic_control_results.rds'))

plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, NA)) %>%
  mutate(zero_weight_state = ifelse(state_code == 'CA', 'CA', w.weights == 0)) %>%
  left_join(state_names) %>%
  mutate(label = paste0(state_name, ': ', w.weights)) %>%
  mutate(label = ifelse(state_code == 'CA', 'California: Treated', label)) %>%
  arrange(!is.na(w.weights), -w.weights, time) 
  
facet_order <- plot_df %>%
  select(state_code, w.weights) %>%
  distinct() %>%
  mutate(facet_order = row_number())

plot_df <- plot_df %>%
  left_join(facet_order)

facet_labels <- unique(plot_df$label)
names(facet_labels) <- unique(plot_df$facet_order)
  
ggplot(plot_df, aes(x = time, y = violation_rate, color = zero_weight_state)) +
  geom_line() +
  geom_point(aes(size = dmr_reporter_count)) +
  geom_vline(xintercept = intervention, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Period',
       y = 'Effluent Violation Rate') +
  facet_wrap(~facet_order,
             labeller = labeller(facet_order = facet_labels)) +
  scale_size_continuous(name = 'Facility Count') +
  scale_color_manual(name = '',
                     labels = c('California',
                                'Non-Zero Weight State',
                                'Zero Weight State'),
                     values = c('grey40', color1, color2)) +
  theme_classic() +
  theme(legend.position = 'bottom')
ggsave(file.path(fig_dir, 'donor_state_violation_rate.pdf'),
       width = 10, height = 7)

# b) CA vs the rest of US and CA vs Synthetic CA --------------------------

# ## with the rest of US
# data <- read_csv(file.path(analysis_dir, 'output', 'violation_rate_weighted_permits.csv')) %>%
#   mutate(time = monitoring_year)

## with the average of all
plot_df <- synth_data %>%
  filter(time %in% window) %>%
  mutate(rest_of_us = state_code != 'CA') %>%
  group_by(rest_of_us, time) %>%
  summarise(avg_violation_rate = mean(violation_rate, na.rm = TRUE))

p1 <- ggplot(plot_df, aes(x = time, y = avg_violation_rate, group = rest_of_us, linetype = rest_of_us)) +
  geom_line() +
  geom_vline(xintercept = 2000,
             linetype = 'dashed',
             color = color2) +
  annotate('text',
           x = 1998.3,
           y = 0.9,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = color2) +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Period',
       y = 'Effluent Violation Rate') +
  scale_linetype_discrete(name = '',
                          labels = c('California',
                                     'Average of 24 Control States.')) +
  theme_classic() +
  theme(legend.position = 'top')
p1

## with synthetic california
plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(control = state_code != 'CA') %>%
  group_by(control, time) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE))

p2 <- ggplot() +
  geom_line(data = plot_df, aes(x = time, y = avg_violation_rate, group = control, linetype = control)) +
  geom_vline(xintercept = intervention,
             linetype = 'dashed',
             color = color2) +
  annotate('text',
           x = 1998.3,
           y = 0.9,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = color2) +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Period',
       y = 'Effluent Violation Rate') +
  scale_linetype_discrete(name = '',
                          labels = c('California',
                                     'Synthetic')) +
  theme_classic() +
  theme(legend.position = 'top')
p2

ggarrange(p1, p2, ncol = 1)
ggsave(file.path(fig_dir, 'synthetic_ca_violation_rate.pdf'),
       width = 5,
       height = 8)

# c) permutation test penal -----------------------------------------------
pretreatment_loss <- readRDS(file.path(input_dir, 'pretreatment_loss.rds'))
plot_df <- readRDS(file.path(input_dir, 'permutation_plot_df.rds'))
effect_results <- read_csv(file.path(input_dir, 'main_results.csv'))

plot_permutation_test <- function(pretreatment_loss, 
                                  plot_df, 
                                  effect_results,
                                  threshold_scale, 
                                  title){
  
  rms_threshold <- pretreatment_loss$w.weight[pretreatment_loss$state_code == 'CA'] * threshold_scale
  p <- ifelse(threshold_scale == 1000000, effect_results$p_mspe_all,
              ifelse(threshold_scale == 20, effect_results$p_mspe_threshold_20,
                     ifelse(threshold_scale == 10, effect_results$p_mspe_threshold_10, 
                            ifelse(threshold_scale == 5, effect_results$p_mspe_threshold_5, 'NA'))))
  
  kept_states <- pretreatment_loss$state_code[pretreatment_loss$w.weight <= rms_threshold]
  
  sub_plot_df <- plot_df %>%
    filter(state_code %in% kept_states)
  
  ggplot() +
    geom_line(data = sub_plot_df[sub_plot_df$state_code != 'CA',], aes(x = time, y = gap, group = treatment_id, color = california)) +
    geom_line(data = sub_plot_df[sub_plot_df$state_code == 'CA',], aes(x = time, y = gap, group = treatment_id, color = california)) +
    geom_hline(yintercept = 0,
               linetype = 'dotted') +
    geom_vline(xintercept = intervention,
               linetype = 'dashed',
               color = color2) +
    annotate('text',
             x = 1998.3,
             y = 0.25,
             label = 'MMP Started \nFor Effluent Violations \nIn California',
             color = color2) +
    ylim(-0.4, 0.4) +
    scale_color_manual(name = '',
                       labels = c( 'Control States', 'California'),
                       values = c('grey80', 'black')) +
    labs(x = 'Monitoring Period',
         y = 'Gap in Effluent Violation Rate', 
         title = title,
         subtitle = paste0('Effect: ', round(effect_results$effect_did, 2), '; ', 
                           'P: ', round(p, 2), '; ', 
                           'State Count: ', length(kept_states))) + # remove CA from counting control states
    theme_classic() +
    theme(legend.position = 'none',
          plot.subtitle = element_text(color = 'grey40', size = 9),
          plot.title = element_text(size = 11))
}

### graph all donor states
p1 <- plot_permutation_test(pretreatment_loss, plot_df, effect_results, 1000000, 'No Control State Removed')
p1

plot_permutation_test(pretreatment_loss, plot_df, effect_results, 1000000, '')
ggsave(file.path(fig_dir, 'permutation_all.pdf'),
       height = 4,
       width = 5)

### remove states with MSEP 10 times higher than California's
p2 <-plot_permutation_test(pretreatment_loss, plot_df, effect_results, 20, 'Control States With MSPE > 20 Times California Removed')
p2

### remove states with MSEP 5 times higher than California's
p3 <-plot_permutation_test(pretreatment_loss, plot_df, effect_results, 10, 'Control States With MSPE > 10 Times California Removed')
p3

### remove states with MSEP 2 times higher than California's
p4 <-plot_permutation_test(pretreatment_loss, plot_df, effect_results,5, 'Control States With MSPE > 5 Times California Removed')
p4

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave(file.path(fig_dir, 'permutation_panel.pdf'),
       height = 8,
       width = 10)


# d) effect interval ------------------------------------------------------

output_synthetic_gap <- function(data, output, name){
  df <-  data %>%
    left_join(output$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
    mutate(w.weights = replace_na(w.weights, 1)) %>%
    filter(w.weights > 0) %>%
    mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
    mutate(control = state_code != 'CA') %>%
    group_by(control, time) %>%
    summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
    spread(control, avg_violation_rate) %>%
    mutate(gap = `FALSE` -`TRUE`) %>%
    mutate(model_name = name)
  return(df)
}

model_subdirs <- list.dirs(path = syn_dir,
                            full.names = FALSE,
                            recursive = FALSE) 
models <- model_subdirs[str_detect(model_subdirs, '(donor)|(loot)|(predictor)')]
names(models) <- sapply(models, function(x) {substr(x, 1, nchar(x) - 20)})

effects <- lapply(names(models), function(x) {
  sub_data <- read_csv(file.path(syn_dir, models[[x]], 'synthetic_control_data.csv'))
  sub_results <- readRDS(file.path(syn_dir, models[[x]], 'synthetic_control_results.rds'))
  
  output_synthetic_gap(sub_data, sub_results, x)
}) %>%
  bind_rows()

main_effect <- output_synthetic_gap(synth_data, results, 'zmain') # add z in front of main to control line ploting order

plot_df <- effects %>%
  bind_rows(., main_effect) %>%
  mutate(main = model_name == 'zmain')


## effluent violation rate gap
ggplot(plot_df, aes(x = time, y = gap, group = model_name, color = main)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('grey80', 'black'),
                     labels = c('Robustness Checks', 'Main Study Design')) + 
  # geom_line(data = effects, aes(x = time, y = gap, group = model_name), color = 'grey80') +
  # geom_line(data = main_effect, aes(x = time, y = gap, group = model_name), color = 'black') +
  geom_hline(yintercept = 0,
             linetype = 'dotted') +
  geom_vline(xintercept = 2000,
             linetype = 'dashed',
             color = color2) +
  ylim(-0.4, 0.4) +
  annotate('text',
           x = 1998.3,
           y = 0.25,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = color2) +
  labs(x = 'Monitoring Period',
       y = 'Gap in Effluent Violation Rate \n(California vs Synthetic California)') +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(fig_dir, 'robustness_gap.pdf'),
       height = 4, 
       width = 5)

## effluent violation rate
ggplot(data = plot_df,) +
  geom_line( aes(x = time, y = `TRUE`, group = model_name, color = main), linetype = 'longdash') +
  geom_line(aes(x = time, y = `FALSE`)) +
  scale_color_manual(name = '',
                     values = c('grey80', 'black'),
                     labels = c('Robustness Checks', 'Main Study Design')) + 
  geom_vline(xintercept = 2000,
             linetype = 'dashed',
             color = color2) +
  ylim(0.4, 1) +
  annotate('text',
           x = 1998.3,
           y = 0.9,
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = color2) +
  annotate('text',
           x = 2002, 
           y = 0.5,
           label = 'California') + 
  annotate('text',
           x = 2002,
           y = 0.85,
           label = 'Synthetic California') +
  labs(x = 'Monitoring Period',
       y = 'Effluent Violation Rate') +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(fig_dir, 'robustness_rate.pdf'),
       height = 4, 
       width = 5)




