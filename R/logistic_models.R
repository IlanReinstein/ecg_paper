library(tidyverse)
library(lme4)
library(tidybayes)

rm(list = ls())

elbows <- readRDS("data/interim/train_elbows.rds")
elbows$sclSeq <- elbows$Sequence/20
elbows$RaterID <- as.numeric(elbows$RaterID)

# Complete Pooling Logistic model
pool <- glm(Accuracy ~ sclSeq, data = elbows)
summary(pool)
confint(pool)
saveRDS(pool, 'models/logistic_complete_pool.rds')
# No Pooling Logistic Model
no_pooling <- lmList(Accuracy ~ sclSeq|RaterID, data = elbows, family = "binomial") %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "No pooling")

# Random Intercepts - Logistic
random_intercept <- glmer(Accuracy ~ sclSeq + (1|RaterID), data = elbows, family = "binomial")
summary(random_intercept)
saveRDS(random_intercept, 'models/logistic_random_intercepts.rds')
# Random Coefficients - Logistic
random_coefs <- glmer(Accuracy ~ sclSeq + (1 + sclSeq|RaterID), data = elbows, family = "binomial")
summary(random_coefs)
saveRDS(random_coefs, 'models/logistic_random_coefficients.rds')

# consolidate in one big table ready for plotting

df_pool <- data_frame(
  Model = "Complete pooling",
  RaterID = unique(elbows$RaterID),
  Intercept = coef(pool)[1], 
  Slope_Seq = coef(pool)[2])

df_intercept <- coef(random_intercept)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "Random Intercept")

df_slope <- coef(random_coefs)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "Random Coefs")

df_models <- bind_rows(df_pool, no_pooling, df_intercept, df_slope) %>% 
  left_join(elbows, by = "RaterID") %>%
  mutate(prob = arm::invlogit(Intercept + Slope_Seq*sclSeq))

df_models$Model <- factor(df_models$Model, c("Complete pooling", "No pooling", "Random Intercept", "Random Coefs"))
write.csv(df_models, "data/output/logistic_models_summary.csv")


###### Individual Coefficients for different ######
###### learners of different abilities ############
# Use this section to extract information for specific learners
# of interest.

# Sort by score
sort_raters <- elbows %>%
  group_by(RaterID) %>%
  summarise(total_score = mean(Accuracy)) %>%
  arrange(desc(total_score)) %>%
  pull(RaterID)


# List of raters of interest for final table
selected_raters <- sort_raters[seq(1, 226, 27)][c(1:4,6:9)]

######## Rater of interest ID=47 ########
no_pooling %>% filter(RaterID == 59)

elbows %>%
  group_by(RaterID) %>%
  do(single_ols = glm(Accuracy ~ sclSeq, data = ., family = 'binomial')) %>% 
  filter(RaterID == 59) %>% 
  pull(single_ols) %>% .[[1]] %>% confint()

sjPlot::get_model_data(random_intercept, type = "re", transform = NULL) %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = value + fixef(random_intercept)[1], 
         value = round(value, 2)) %>%
  filter(term == 59)


sjPlot::get_model_data(random_coefs, type = "re", transform = NULL) %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = if_else(facet == "Bin",
                         value + fixef(random_coefs)[2], 
                         value + fixef(random_coefs)[1]), 
         value = round(value, 2)) %>%
  filter(term == 59)