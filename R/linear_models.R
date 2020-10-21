library(tidyverse)
library(lme4)
library(tidybayes)

rm(list = ls())

### Section 3 Linear Models ###
elbowsBin <- readRDS("data/interim/train_elbows_bins.rds")

# Overall aggregate score
mean(elbowsBin$rater_score)

# Complete Pooling model
pool <- lm(rater_score ~ Bin, data = elbowsBin)
saveRDS(pool, 'models/linear_complete_pool.rds')
summary(pool)
confint(pool)

# No pooling Model
no_pooling <- lmList(rater_score ~ Bin|RaterID, data = elbowsBin) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "No pooling")

# Random Intercepts Model - Linear
random_intercept <- lmer(rater_score ~ Bin + (1|RaterID), data = elbowsBin)
summary(random_intercept)
saveRDS(random_intercept, 'models/linear_random_intercept.rds')

# Random Intercepts + Slopes (Coefficients) - Linear
random_coefs <- lmer(rater_score ~ Bin + (Bin|RaterID), data = elbowsBin)
summary(random_coefs)
saveRDS(random_coefs, 'models/linear_random_coefficients.rds')

## Consolidate all results in one big table ready for plotting
df_pool <- data.frame(
  Model = "Complete pooling",
  RaterID = unique(elbowsBin$RaterID),
  Intercept = coef(pool)[1], 
  Slope_Bin = coef(pool)[2], row.names = NULL)

df_intercept <- coef(random_intercept)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "Random Intercept")

df_slope <- coef(random_coefs)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "Random Coefs")

df_models <- bind_rows(df_pool, no_pooling, df_intercept, df_slope) %>% 
  left_join(elbowsBin, by = "RaterID") 

df_models$Model <- factor(df_models$Model, c("Complete pooling", "No pooling", "Random Intercept", "Random Coefs"))
write.csv(df_models, 'data/output/linear_models_summary.csv')

###### Individual Coefficients for different ######
###### learners of different abilities ############
# Use this section to extract information for specific learners
# of interest.

# Sort by score
sort_raters <- elbowsBin %>%
  group_by(RaterID) %>%
  summarise(total_score = mean(rater_score)) %>%
  arrange(desc(total_score)) %>%
  pull(RaterID)

# List of raters of interest for final table
selected_raters <- sort_raters[seq(1, 226, 27)][c(1:4,6:9)]

######## Rater of interest ID=47 ########
no_pooling %>% filter(RaterID == 47)

elbowsBin %>%
  do(single_ols = lm(rater_score ~ Bin, data = .)) %>% 
  filter(RaterID == 47) %>% 
  pull(single_ols) %>% .[[1]] %>% confint()

df_intercept %>% filter(RaterID == 47)

sjPlot::get_model_data(random_intercept, type = "re") %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = value + fixef(random_intercept)[1], 
         value = round(value, 2)) %>%
  filter(term == 47)

sjPlot::get_model_data(random_coefs, type = "re") %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = if_else(facet == "Bin",
                         value + fixef(random_coefs)[2], 
                         value + fixef(random_coefs)[1]), 
         value = round(value, 2)) %>%
  filter(term == 47)



