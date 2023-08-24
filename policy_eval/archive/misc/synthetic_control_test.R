
####################
# CA MMP Analysis
## Synthetic Control
####################

# set up ------------------------------------------------------------------

OAK_DIR <- Sys.getenv('OAK')
setwd(file.path(OAK_DIR, 'EPA'))

if (!require("pacman")){
  install.packages("pacman", repos=" https://CRAN.R-project.org")
}

# pacman is a package that helps with loading libraries: https://cran.r-project.org/web/packages/pacman/index.html
library(pacman)
p_load('here', 'lubridate', 'stringr', 'tidyverse', 'Synth')

# analysis folder
analysis_dir <- file.path('Analysis', 'ca_mmp')
figure_dir <- file.path(analysis_dir, 'figures')
output_dir <- file.path(analysis_dir, 'output')

# read data ---------------------------------------------------------------

download_date <- '2020-07-07_14-57-43'
data_dir_raw <- file.path('Data', 'raw', download_date)
data_dir_manual <- file.path('Data', 'manual')

# selected donor pool states
data <- read_csv(file.path(output_dir, 'synthetic_control_data.csv')) 

synth_data <- data %>%   
  filter(donor_unit) %>%
  # create numeric unit id
  mutate(unit_id = as.factor(state_code)) %>%
  mutate(unit_id = as.numeric(unit_id)) %>%
  data.frame()

# Analysis ----------------------------------------------------------------


# 1) plot California vs. rest of US ---------------------------------------

plot_df <- data %>%
  mutate(rest_of_us = state_code != 'CA') %>%
  group_by(rest_of_us, monitoring_year) %>%
  summarise(avg_violation_rate = mean(violation_rate, na.rm = TRUE))

ggplot(plot_df, aes(x = monitoring_year, y = avg_violation_rate, group = rest_of_us, linetype = rest_of_us)) +
  geom_line() +
  geom_vline(xintercept = 2000, 
             linetype = 'dashed', 
             color = '#F8766D') +
  annotate('text', 
           x = 1998.7, 
           y = 0.9, 
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  geom_vline(xintercept = 2004, 
             linetype = 'dashed',
             color = '#00BFC4') +
  annotate('text',
           x = 2002.6,
           y = 0.9, 
           label = 'MMP Started \nFor Reporting Violations \nIn California',
           color = '#00BFC4') +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Year',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rate Trend: California vs. Rest of US') +
  scale_linetype_discrete(name = '',
                          labels = c('California',
                                     'Rest of US')) +
  theme_classic() + 
  theme(legend.position = 'top') 
ggsave(file.path(figure_dir, 'ca_us_violation_rate.pdf'),
       width = 6, height = 4)

# 2) fitting pre-intervention trend ---------------------------------------

run_synthetic_control <- function(synth_data, treatment_unit){
  
  controls_unit <- unique(synth_data$unit_id)[-treatment_unit]
  
  synth_data_prep <- dataprep(foo = synth_data,
                              # predictors = c('violation_rate'),
                              # predictors.op = 'mean',
                              special.predictors = list(
                                list('violation_rate', 1996, 'mean'),
                                list('violation_rate', 1997, 'mean'),
                                list('violation_rate', 1998, 'mean'),
                                list('violation_rate', 1999, 'mean'),
                                list('violation_rate', 2000, 'mean'),
                                list('permit_count', 2000, 'mean'),
                                list('permit_count', 1998, 'mean')
                              ),
                              time.predictors.prior = min(synth_data$monitoring_year):2000,
                              dependent = 'violation_rate',
                              unit.variable = 'unit_id',
                              unit.names.variable = 'state_code',
                              time.variable = 'monitoring_year',
                              treatment.identifier = treatment_unit, 
                              controls.identifier = controls_unit,
                              time.optimize.ssr = min(synth_data$monitoring_year):2000,
                              time.plot = min(synth_data$monitoring_year):max(synth_data$monitoring_year))
  synth_out <- synth(synth_data_prep)
  synth_tables <- synth.tab(dataprep.res = synth_data_prep,
                            synth.res = synth_out)
  results <- list(synth_data_prep, synth_out, synth_tables)
  names(results) <- c('data_prep', 'output', 'tables')
  
  return(results)
}

treatment_unit <- synth_data$unit_id[synth_data$state_code == 'CA'] %>%
  unique()

results <- run_synthetic_control(synth_data, treatment_unit)

results$data_prep$X0
results$data_prep$Z0
results$data_prep$X1
results$data_prep$Z1

results$tables$tab.pred
results$tables$tab.w

write_csv(data.frame(results$tables$tab.pred), 
          file.path(output_dir, 'synthetic_control_pred.csv'))
write_csv(data.frame(results$tables$tab.w), 
          file.path(output_dir, 'synthetic_control_weights.csv'))

# 3) plot violation rate by donor states -----------------------------------------

plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  mutate(zero_weight_state = w.weights == 0)

ggplot(plot_df, aes(x = monitoring_year, y = violation_rate, color = zero_weight_state)) +
  geom_line() +
  geom_point(aes(size = permit_count)) +
  geom_vline(xintercept = 2000.1, linetype = 'dashed', color = 'grey40') +
  labs(x = 'Monitoring Year',
       y = 'Proportion of Permit with At Least 1 Violation') +
  facet_wrap(~state_code) +
  scale_size_continuous(name = 'DMR Reporter Count') +
  scale_color_discrete(name = '',
                       labels = c('Non-Zero Weight State',
                                  'Zero Weight State')) +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure_dir, 'donor_state_violation_rate.pdf'),
       width = 12, height = 7)

 # 4) plot treatment and synthetic control comparison ----------------------

## time series trend 
## a) basic plot by the pacakge
path.plot(dataprep.res = results$data_prep,
          synth.res = results$output)
## b) fancy manual plot
plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(control = state_code != 'CA') %>%
  group_by(control, monitoring_year) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE))

ggplot(plot_df, aes(x = monitoring_year, y = avg_violation_rate, group = control, linetype = control)) +
  geom_line() +
  geom_vline(xintercept = 2000, 
             linetype = 'dashed', 
             color = '#F8766D') +
  annotate('text', 
           x = 1998.7, 
           y = 0.9, 
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  geom_vline(xintercept = 2004, 
             linetype = 'dashed',
             color = '#00BFC4') +
  annotate('text',
           x = 2002.6,
           y = 0.9, 
           label = 'MMP Started \nFor Reporting Violations \nIn California',
           color = '#00BFC4') +
  ylim(0.4, 1) +
  labs(x = 'Monitoring Year',
       y = 'Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rate Trend: California vs. Synthetic California') +
  scale_linetype_discrete(name = '',
                       labels = c('California',
                                  'Synthetic')) +
  theme_classic() + 
  theme(legend.position = 'top') 
ggsave(file.path(figure_dir, 'ca_synthetic_violation_rate.pdf'),
       width = 6, height = 4)

## gaps between california and synthetic california
## a) simple plot from package
gaps.plot(dataprep.res = results$data_prep,
          synth.res = results$output)
## b) fancy manual plot
plot_df <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(control = state_code != 'CA') %>%
  group_by(control, monitoring_year) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
  spread(control, avg_violation_rate) %>%
  mutate(gap = `FALSE` -`TRUE`)
  
ggplot(plot_df, aes(x = monitoring_year, y = gap)) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 'dotted') +
  geom_vline(xintercept = 2000, 
             linetype = 'dashed', 
             color = '#F8766D') +
  annotate('text', 
           x = 1998.7, 
           y = 0.1, 
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  geom_vline(xintercept = 2004, 
             linetype = 'dashed',
             color = '#00BFC4') +
  ylim(-0.2, 0.15) +
  annotate('text',
           x = 2002.6,
           y = 0.1, 
           label = 'MMP Started \nFor Reporting Violations \nIn California',
           color = '#00BFC4') +
  labs(x = 'Monitoring Year',
       y = 'Gap in Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rate Gap: California vs. Synthetic California') +
  theme_classic() 
ggsave(file.path(figure_dir, 'ca_synthetic_gap.pdf'),
       width = 6, height = 4)


# 5) permutation test -----------------------------------------------------

## a) run synthetic control for each of states in the donor pool 
treatment_units <- unique(synth_data$unit_id)

permutation_test <- lapply(treatment_units, run_synthetic_control, synth_data = synth_data)

## b) plot gaps distribution

make_synth_gap_table <- function(permutation_test, treatment_unit){
  synth_data %>%
    left_join(permutation_test[[treatment_unit]]$tables$tab.w[, c('w.weights', 'unit.names')], 
              by = c('state_code' = 'unit.names')) %>%
    mutate(w.weights = replace_na(w.weights, 1)) %>%
    filter(w.weights > 0) %>%
    mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
    mutate(control = unit_id != treatment_unit) %>%
    group_by(control, monitoring_year) %>%
    summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
    spread(control, avg_violation_rate) %>%
    mutate(gap = `FALSE` -`TRUE`) %>%
    mutate(treatment_id = treatment_unit)
}

plot_df <- lapply(treatment_units, make_synth_gap_table, permutation_test = permutation_test) %>%
  do.call(rbind, .) %>%
  left_join(synth_data[, c('state_code', 'unit_id')], by = c('treatment_id' = 'unit_id')) %>%
  distinct() %>%
  mutate(california = state_code == 'CA')

ggplot(plot_df, aes(x = monitoring_year, y = gap, group = treatment_id, color = california)) +
  geom_line() +
  geom_hline(yintercept = 0,
             linetype = 'dotted') +
  geom_vline(xintercept = 2000, 
             linetype = 'dashed', 
             color = '#F8766D') +
  annotate('text', 
           x = 1998.7, 
           y = 0.15, 
           label = 'MMP Started \nFor Effluent Violations \nIn California',
           color = '#F8766D') +
  geom_vline(xintercept = 2004, 
             linetype = 'dashed',
             color = '#00BFC4') +
  #ylim(-0.2, 0.2) +
  annotate('text',
           x = 2002.6,
           y = 0.15, 
           label = 'MMP Started \nFor Reporting Violations \nIn California',
           color = '#00BFC4') +
  scale_color_manual(name = '',
                     labels = c( 'Control States', 'California'),
                     values = c('grey80', 'black')) +
  labs(x = 'Monitoring Year',
       y = 'Gap in Proportion of Permit with At Least 1 Violation',
       title = 'Effluent Violation Rate Gap: California vs. Control States') +
  theme_classic() +
  theme(legend.position = 'top')
ggsave(file.path(figure_dir, 'ca_permutation_test_gap.pdf'),
       width = 6, height = 4)


# 6) formalize effect and significance ------------------------------------

## a) pretreatment RMS
results$output$loss.w

## b) effect: difference in differences
results$data_prep

tmp <- synth_data %>%
  left_join(results$tables$tab.w[, c('w.weights', 'unit.names')], by = c('state_code' = 'unit.names')) %>%
  mutate(w.weights = replace_na(w.weights, 1)) %>%
  filter(w.weights > 0) %>%
  mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
  mutate(california = state_code == 'CA') %>%
  group_by(california, monitoring_year) %>%
  summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
  mutate(post_intervention = monitoring_year > 2000) %>%
  group_by(post_intervention, california) %>%
  summarise(avg_violation_rate = mean(avg_violation_rate))

# effect of MMP: -0.1188513
effect <- (tmp$avg_violation_rate[tmp$california == TRUE & tmp$post_intervention == TRUE] - tmp$avg_violation_rate[tmp$california == FALSE & tmp$post_intervention == TRUE]) -
          (tmp$avg_violation_rate[tmp$california == TRUE & tmp$post_intervention == FALSE] - tmp$avg_violation_rate[tmp$california == FALSE & tmp$post_intervention == FALSE])

## c) p-value

make_synth_effect_table <- function(synth_data, permutation_test, treatment_unit){
  tmp <- synth_data %>%
            left_join(permutation_test[[treatment_unit]]$tables$tab.w[, c('w.weights', 'unit.names')], 
                      by = c('state_code' = 'unit.names')) %>%
            mutate(w.weights = replace_na(w.weights, 1)) %>%
            filter(w.weights > 0) %>%
            mutate(adjusted_violation_rate = violation_rate*w.weights) %>%
            mutate(control = unit_id != treatment_unit) %>%
            group_by(control, monitoring_year) %>%
            summarise(avg_violation_rate = sum(adjusted_violation_rate, na.rm = TRUE)) %>%
            mutate(post_intervention = monitoring_year > 2000) %>%
            group_by(post_intervention, control) %>%
            summarise(avg_violation_rate = mean(avg_violation_rate))
  effect <- (tmp$avg_violation_rate[tmp$control == FALSE & tmp$post_intervention == TRUE] - tmp$avg_violation_rate[tmp$control == TRUE & tmp$post_intervention == TRUE]) -
            (tmp$avg_violation_rate[tmp$control == FALSE & tmp$post_intervention == FALSE] - tmp$avg_violation_rate[tmp$control == TRUE & tmp$post_intervention == FALSE])
  effect
}

treatment_units <- unique(synth_data$unit_id)

permutation_test_effects <- lapply(treatment_units, make_synth_effect_table, 
                                   synth_data = synth_data,
                                   permutation_test = permutation_test) %>%
  do.call(rbind, .) %>%
  data.frame()
names(permutation_test_effects) <- 'effect'
permutation_test_effects$unit_id <- treatment_units 
permutation_test_effects <- permutation_test_effects %>%
  left_join(synth_data[, c('unit_id', 'state_code')]) %>%
  distinct()

## plot permutation test distribution
ggplot(permutation_test_effects, aes(x = effect)) +
  geom_density() +
  geom_vline(xintercept = permutation_test_effects$effect[permutation_test_effects$state_code == 'CA'],
             linetype = 'dashed',
             color = '#F8766D') +
  theme_classic()

## approximate p-value 
summary(permutation_test_effects$effect)
mean(permutation_test_effects$effect)
sd(permutation_test_effects$effect)

p_value <- pnorm(permutation_test_effects$effect[permutation_test_effects$state_code == 'CA'],
      mean = mean(permutation_test_effects$effect),
      sd = sd(permutation_test_effects$effect),
      lower.tail = TRUE)

# Appendix: testing synth package -----------------------------------------

data('basque')

dataprep_out <- dataprep(foo = basque,
                         predictors = c('school.illit', 'school.prim', 'school.med',
                                        'school.high', 'school.post.high', 'invest'),
                         predictors.op = 'mean', # the operator
                         time.predictors.prior = 1964:1969, # the entire time frame from the #beginning to the end
                         special.predictors = list(
                           list('gdpcap', 1960:1969, 'mean'),
                           list('sec.agriculture', seq(1961,1969,2),'mean'),
                           list('sec.energy',seq(1961,1969,2),'mean'),
                           list('sec.industry', seq(1961,1969,2),'mean'),
                           list('sec.construction', seq(1961,1969,2),'mean'),
                           list('sec.services.venta', seq(1961,1969,2),'mean'),
                           list('sec.services.nonventa',seq(1961,1969,2),'mean'),
                           list('popdens', 1969, 'mean')),
                         dependent = 'gdpcap', # dependent variable
                         unit.variable = 'regionno', #identifying unit numbers
                         unit.names.variable = 'regionname', #identifying unit names
                         time.variable = 'year', #time-periods
                         treatment.identifier = 17, #the treated case
                         controls.identifier = c(2:16, 18), #the control cases; all others #except number 17
                         time.optimize.ssr = 1960:1969, #the time-period over which to optimize
                         time.plot = 1955:1997 #the entire time period before/after the treatment
                         )

str(dataprep_out)

# Z1: the treatment trend before the treatment
dataprep_out$Z1
# Z0: the control trend before the treatment
dataprep_out$Z0
# X1: the treatment predictors before the treatment
dataprep_out$X1
# X0: the control predictors after the treatment
dataprep_out$X0

synth_out <- synth(data.prep.obj = dataprep_out, 
                   method = 'BFGS')

synth_tables <- synth.tab(dataprep.res = dataprep_out,
                          synth.res = synth_out)
names(synth_tables)

# tab.pred: comparing treatment and synthetic control by predictors
synth_tables$tab.pred
# variable weights
synth_tables$tab.v
# unit weights
synth_tables$tab.w
# MSE output
synth_tables$tab.loss

# plot the changes before and after the intervention
path.plot(synth.res=synth_out,
          dataprep.res = dataprep_out, 
          Ylab="real per-capita gdp (1986 USD, thousand)",
          Xlab="year",
          Ylim = c(0,12),
          Legend = c("Basque country",
                     "synthetic Basque country"),
          Legend.position = "bottomright")

# plot the gaps between the treatment and the synthetic control
gaps.plot(synth.res = synth_out, 
          dataprep.res = dataprep_out,
          Ylab = 'gap in real per-capita GDP (1986 USD, thousand)', Xlab= 'year',
          Ylim = c(-1.5,1.5), 
          Main = NA)

# permutation test


