library(tidyverse)
library(lme4)
library(tidybayes)

rm(list = ls())
color_pals <- RColorBrewer::brewer.pal(5, "Set1")
red <- color_pals[1]
blue <- color_pals[2]
green <- color_pals[3]

### Section 3 ###
elbowsBin <- readRDS("data/interim/train_elbows_bins.rds")

sort_raters <- elbowsBin %>%
  group_by(RaterID) %>%
  summarise(total_score = mean(rater_score)) %>%
  arrange(desc(total_score)) %>%
  pull(RaterID)

# elbowsBin$Bin0 <- elbowsBin$Bin - 1

selected_raters <- sort_raters[seq(1, 226, 27)][c(1:4,6:9)]


mean(elbowsBin$rater_score)

pool <- lm(rater_score ~ Bin, data = elbowsBin)
summary(pool)
confint(pool)


df_pool <- data.frame(
  Model = "Complete pooling",
  RaterID = unique(elbowsBin$RaterID),
  Intercept = coef(pool)[1], 
  Slope_Bin = coef(pool)[2], row.names = NULL)

no_pooling <- lmList(rater_score ~ Bin|RaterID, data = elbowsBin) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "No pooling")

no_pooling %>% filter(RaterID == 47)

lm(rater_score ~ Bin, data = elbowsBin %>% filter(RaterID == 2)) %>% summary()

elbowsBin %>%
  do(single_ols = lm(rater_score ~ Bin, data = .)) %>% 
  filter(RaterID == 47) %>% 
  pull(single_ols) %>% .[[1]] %>% confint()

random_intercept <- lmer(rater_score ~ Bin + (1|RaterID), data = elbowsBin)


df_intercept <- coef(random_intercept)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "Random Intercept")


df_intercept %>% filter(RaterID == 47)

sjPlot::get_model_data(random_intercept, type = "re") %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = value + fixef(random_intercept)[1], 
         value = round(value, 2)) %>%
  filter(term == 47)

random_coefs <- lmer(rater_score ~ Bin + (Bin|RaterID), data = elbowsBin)

df_slope <- coef(random_coefs)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Bin = Bin) %>% 
  add_column(Model = "Random Coefs")

summary(random_coefs)

sjPlot::get_model_data(random_coefs, type = "re") %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = if_else(facet == "Bin",
                         value + fixef(random_coefs)[2], 
                         value + fixef(random_coefs)[1]), 
         value = round(value, 2)) %>%
  filter(term == 59)
  

df_models <- bind_rows(df_pool, no_pooling, df_intercept, df_slope) %>% 
  left_join(elbowsBin, by = "RaterID") 

df_models$Model <- factor(df_models$Model, c("Complete pooling", "No pooling", "Random Intercept", "Random Coefs"))
jit_pos <- position_jitter(width = 0.05, height = 0.02, seed = 42)

df_models %>%
  filter(Model %in% c("Complete pooling")) %>%
  ggplot(aes(Bin, rater_score)) + 
  geom_point(alpha = 0.7, position = jit_pos) + 
  geom_abline(aes(intercept = Intercept, slope = Slope_Bin), color = color_pals[1], size = .75) + 
  theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+#limits = c(0, 4)) + 
  labs(title = "Aggregate Regression - Complete Pooling", subtitle = "Elbow Dataset - Linear Learning",
       y = "Predicted Bin Score") -> pool_plot



df_models %>%
  filter(Model %in% c("No pooling")) %>%
  ggplot(aes(Bin, rater_score)) + 
  geom_abline(aes(intercept = Intercept, slope = Slope_Bin), color = color_pals[2], size = .5, alpha = 0.5) + 
  geom_point(alpha = 0.7, position = jit_pos) +
  geom_abline(aes(intercept = coef(pool)[1], slope = coef(pool)[2]), color = color_pals[1], size = 0.6)+
  theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+ #limits = c(0, 4)) +
  labs(title = "Individual Regressions - No pooling",
       y = "Predicted Bin Score", 
       subtitle = "Elbow Dataset - Linear Learning") -> no_pool_plot

# Figure 1, 800*370
cowplot::plot_grid(pool_plot, no_pool_plot)

df_models %>%
  filter(Model %in% c("Random Intercept")) %>%
  ggplot(aes(Bin, rater_score)) + 
  geom_abline(aes(intercept = Intercept, slope = Slope_Bin), color = color_pals[3], size = .65, alpha = 0.5) + 
  geom_point(alpha = 0.7, position = jit_pos) +
  geom_abline(aes(intercept = coef(pool)[1], slope = coef(pool)[2]), color = color_pals[1], size = 0.6)+
  theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+ #limits = c(0, 4)) +
  labs(title = "Random Intercepts",
       y = "Predicted Bin Score",
       subtitle = "Elbow Dataset - Linear Learning") -> ri_plot

# Figure 2
cowplot::plot_grid(no_pool_plot, ri_plot)

df_models %>%
  filter(Model %in% c("Random Coefs")) %>%
  ggplot(aes(Bin, rater_score)) + 
  geom_abline(aes(intercept = Intercept, slope = Slope_Bin), color = color_pals[4], size = .65, alpha = 0.5) + 
  geom_point(alpha = 0.7, position = jit_pos) +
  geom_abline(aes(intercept = coef(pool)[1], slope = coef(pool)[2]), color = color_pals[1], size = 0.7)+
  theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+ #limits = c(0, 4)) +
  labs(title = "Random Intercept and Slopes",
       y = "Predicted Bin Score", 
       subtitle = "Elbow Dataset - Linear Learning") -> rc_plot

# Figure 3
cowplot::plot_grid(ri_plot, rc_plot)

# Figure 4

rater_subset <- df_models %>% 
  filter(RaterID %in% selected_raters) %>%
  mutate(ID = factor(RaterID, levels = selected_raters, labels = paste0("ID ", selected_raters)))

ggplot(rater_subset) + 
  aes(x = Bin, y = rater_score) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(aes(intercept = Intercept, slope = Slope_Bin, 
                  color = Model),
              size = .75) + 
  geom_point() +
  facet_wrap(~ ID, nrow = 2) +
  # labs(x = xlab, y = ylab) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(0,4)) + 
  theme_tidybayes() +
  # Fix the color palette 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = "none") + 
  labs(y = "Predicted Bin Score")

#########################################
####### Logistic Regression ########

elbows <- readRDS("data/interim/train_elbows.rds")
elbows$sclSeq <- elbows$Sequence/20
elbows$RaterID <- as.numeric(elbows$RaterID)

pool <- glm(Accuracy ~ sclSeq, data = elbows)
sjPlot::get_model_data(pool, type = "est")
confint(pool)


df_pool <- data_frame(
  Model = "Complete pooling",
  RaterID = unique(elbows$RaterID),
  Intercept = coef(pool)[1], 
  Slope_Seq = coef(pool)[2])


no_pooling <- lmList(Accuracy ~ sclSeq|RaterID, data = elbows, family = "binomial") %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "No pooling")


random_intercept <- glmer(Accuracy ~ sclSeq + (1|RaterID), data = elbows, family = "binomial")

df_intercept <- coef(random_intercept)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "Random Intercept")

sjPlot::get_model_data(random_intercept, type = "re", transform = NULL) %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = value + fixef(random_intercept)[1], 
         value = round(value, 2)) %>%
  filter(term == 59)

random_coefs <- glmer(Accuracy ~ sclSeq + (1 + sclSeq|RaterID), data = elbows, family = "binomial")

df_slope <- coef(random_coefs)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>% 
  mutate(RaterID = as.numeric(RaterID)) %>%
  rename(Intercept = `(Intercept)`, Slope_Seq = sclSeq) %>% 
  add_column(Model = "Random Coefs")


sjPlot::get_model_data(random_coefs, type = "re", transform = NULL) %>%
  dplyr::select(term, estimate, conf.low, conf.high, facet) %>%
  pivot_longer(-c(term, facet)) %>% 
  mutate(value = if_else(facet == "Bin",
                         value + fixef(random_coefs)[2], 
                         value + fixef(random_coefs)[1]), 
         value = round(value, 2)) %>%
  filter(term == 59)


df_models <- bind_rows(df_pool, no_pooling, df_intercept, df_slope) %>% 
  left_join(elbows, by = "RaterID") %>%
  mutate(prob = arm::invlogit(Intercept + Slope_Seq*sclSeq))

df_models$Model <- factor(df_models$Model, c("Complete pooling", "No pooling", "Random Intercept", "Random Coefs"))
jit_pos <- position_jitter(width = 0.05, height = 0.02, seed = 42)

df_models %>%
  ggplot(aes(Sequence, Accuracy, group = RaterID)) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F,
              size = 0.5, alpha =  0.5, color = color_pals[2]) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F,
              group = 1, size = 0.5, alpha =  0.5, color = color_pals[1]) +
  # geom_point(alpha = 0.7, position = jit_pos) + 
  # geom_curve(aes(intercept = Intercept, slope = Slope_Seq), color = color_pals[1], size = .75) + 
  tidybayes::theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent) + 
  # scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+#limits = c(0, 4)) + 
  labs(title = "Complete and No Pooling", 
       subtitle = "Elbow Dataset - Nonlinear Learning",
       y = "Probability of Correct Response") -> p1

df_models %>%
  filter(Model == "Random Intercept") %>%
  ggplot(aes(Sequence, Accuracy, group = RaterID)) +
  geom_line(aes(y = prob), color = green) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F,
              group = 1, size = 0.5, alpha =  0.5, color = red) +
  # geom_point(alpha = 0.7, position = jit_pos) + 
  # geom_curve(aes(intercept = Intercept, slope = Slope_Seq), color = color_pals[1], size = .75) + 
  tidybayes::theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
  # scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+#limits = c(0, 4)) + 
  labs(title = "Random Intercept", 
       subtitle = "Elbow Dataset - Nonlinear Learning",
       y = "Probability of Correct Response") -> p2

df_models %>%
  filter(Model == "Random Coefs") %>%
  ggplot(aes(Sequence, Accuracy, group = RaterID)) +
  geom_line(aes(y = prob), color = color_pals[4]) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = F,
              group = 1, size = 0.5, alpha =  0.5, color = red) +
  # geom_point(alpha = 0.7, position = jit_pos) + 
  # geom_curve(aes(intercept = Intercept, slope = Slope_Seq), color = color_pals[1], size = .75) + 
  tidybayes::theme_tidybayes() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
  # scale_x_continuous(labels = c("0-20", "21-40", "41-60", "61-80"))+#limits = c(0, 4)) + 
  labs(title = "Random Intercept and Slopes", 
       subtitle = "Elbow Dataset - Nonlinear Learning",
       y = "Probability of Correct Response") -> p3

# Figure 5, 800*370
cowplot::plot_grid(p1, p2, p3, nrow = 1)



