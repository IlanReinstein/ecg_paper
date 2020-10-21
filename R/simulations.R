library(tidyverse)
library(lme4)
library(arm)
library(merTools)

## Data Reading
rm(list = ls())
source("R/utils.R")
set.seed(987656)

select <- dplyr::select
# Elbow Radiographs
elbows <- readRDS("data/interim/train_elbows.rds")
elbowsBin <- readRDS("data/interim/train_elbows_bins.rds")

sort_raters <- elbowsBin %>%
  group_by(RaterID) %>%
  summarise(total_score = mean(rater_score)) %>%
  arrange(desc(total_score)) %>%
  pull(RaterID)

selected_raters <- sort_raters[seq(1, 226, 27)][c(1:4,6:9)]
# True values
n.sims <- 10000
n.items <- 80
n.learner <- length(unique(elbows$RaterID))
y <- elbows$Accuracy

## Transforming Data

# Rescale Sequence variable for fitting issues. If Sequence is left
# in its raw scale, glmer throws convergence warnings
elbows$sclSeq <- elbows$Sequence/20


######## Model Fitting #####
# Section 5 Models in the paper
# No pooling
m0 <- glm(Accuracy ~ sclSeq, data = elbows, family = binomial(link = "logit"))

# Multilevel Model - No diagnosis
m1 <- glmer(Accuracy ~ 1 + sclSeq + (1 + sclSeq|RaterID), data = elbows, 
                   family = binomial(link = "logit"))

# Multilevel Model with Diagnosis
m2 <- glmer(Accuracy ~ 1 + sclSeq*CaseType2 + (1 + sclSeq|RaterID), data = elbows, 
                     family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))

################### Model Evaluation #######################
## Simulations
## compute Real proportion of Correct Responses between all learners
##############################################################

###############################
### Mean and Standard Deviation of the 
### total correct responses across all learners
###############################

m0.rep <- simulate_obs(m0, data = elbows, pooling = T, n_sims = n.sims)$sim_labels
m1.rep <- simulate_obs(m1, data = elbows, pooling = F, n_sims = n.sims)$sim_labels
m2.rep <- simulate_obs(m2, data = elbows, pooling = F, n_sims = n.sims)$sim_labels

g1 <- summary_stat_plot(y_sims = m0.rep, y_real = y, model = "Complete Pooling", 
                        n_items = n.items, n_learners = n.learner)
g2 <- summary_stat_plot(y_sims = m1.rep, y_real = y, model = "MLM - No Diagnosis",
                        n_items = n.items, n_learners = n.learner)
g3 <- summary_stat_plot(y_sims = m2.rep, y_real = y, model = "MLM - With Diagnosis",
                        n_items = n.items, n_learners = n.learner)

## Mean
leg <- cowplot::get_legend(g1$mean_plot + theme(legend.position = "bottom"))
grid1 <- cowplot::plot_grid(g1$mean_plot + theme(legend.position = "none"), 
                            g2$mean_plot + theme(legend.position = "none"), 
                            g3$mean_plot + theme(legend.position = "none"), nrow = 1)
#800*320
cowplot::plot_grid(grid1, leg, nrow = 2, rel_heights = c(1, 0.1))
ggsave('sims_output/elbow-mean_sims.png', width = 8, height = 3.2, units = 'in')

## SD
leg <- cowplot::get_legend(g1$sd_plot + theme(legend.position = "bottom"))
grid1 <- cowplot::plot_grid(g1$sd_plot + theme(legend.position = "none"), 
                            g2$sd_plot + theme(legend.position = "none"), 
                            g3$sd_plot + theme(legend.position = "none"), nrow = 1)

cowplot::plot_grid(grid1, leg, nrow = 2, rel_heights = c(1, 0.1))
ggsave('sims_output/elbow-sd_sims.png', width = 8, height = 3.2, units = 'in')


###### Aggregate Learning Curve #########
# Plot proportion: Selected test statistic
# Compute Aggregate proportion - observed
# Only done for the biggest model Eq 14

sim_ratio <- array(NA, c(n.items, n.sims))
sim_ratio <- apply(m2.rep, 2, test_ratio, n.items)

elbows %>%
  group_by(Sequence) %>%
  summarise(prop_correct = mean(Accuracy), n = n()) %>%
  mutate(prop_se = sqrt(prop_correct*(1 - prop_correct)/n))  -> elbow_agg

elbow_agg %>%
  ggplot(aes(x = Sequence, y = prop_correct)) + 
  geom_point() +
  # geom_pointrange(aes(ymax = prop_correct + prop_se,
  #                 ymin = prop_correct - prop_se), alpha = 0.4) + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0.48, 0.81)) +
  theme_classic() +
  labs(y = "Proportion Correct", x = "Item in Sequence", 
       # title = "Aggregrate Learning Curve for learners", 
       title = "Observed Data - Elbow Dataset") -> p1

q50 <- apply(sim_ratio, 1, quantile, 0.5)
conf50 <- t(apply(sim_ratio, 1, quantile, c(0.25, 0.75)))
conf95 <- t(apply(sim_ratio, 1, quantile, c(0.025, 0.975)))

## Plot simulations vs Real Data
long_sims <- data.frame(cbind(q50, conf50, conf95)) %>%
  mutate(n = 1:n()) 

names(long_sims)[1:5] <- c("mid50", "lb50", "ub50", "lb95", "ub95")

ggplot(long_sims, aes(x = n)) +
  geom_linerange(aes(ymin = lb95, ymax = ub95, color = "95% PI"), alpha = 0.5, size = 1) +
  geom_linerange(aes(ymin = lb50, ymax = ub50, color  = "50% PI"), alpha = 0.8, size = 1.5) +
  geom_point(data = elbow_agg, aes(x = Sequence, y = prop_correct, color = "Observed"), size = 1.5) +
  # geom_line(aes(y = mid50, color = "Sim Median"), size = 1, alpha = 0.7) +
  geom_smooth(aes(y = mid50, color = "Median Trend"), method = "loess", size = 1, se = F) +
  scale_color_manual(name = "",  
    values = c(`Median Trend` = "darkred",
      `50% PI`= "skyblue4", 
      `95% PI` = "gray65",
      `Observed` = "black"),
    guide = guide_legend(override.aes = list(linetype = c(rep("solid", 2), rep("blank", 2)),
                                             shape = c(NA, NA, 16, 16)))) +
  scale_y_continuous(limits = c(0.48, 0.81)) + 
  labs(y = "Proportion Correct", x = "Item in Sequence",
       # title = "Aggregate Learning Curve for Learners",
       title = "Model Simulated Data - Elbow Dataset") +
  theme_classic() -> p2

cowplot::plot_grid(p1, p2, rel_widths = c(0.8, 1.1))
ggsave('sims_output/elbow-sims_v_observed.png', width = 8, height = 3.2, units = 'in')

#################################
#### New Data Predictions #######

elbows_test <- readRDS("data/interim/test_elbows.rds")
elbows_test$sclSeq <- (elbows_test$Sequence + 80)/20
test_items <- 20
test_learners <- length(unique(elbows_test$RaterID)) # same 226
y_test <- elbows_test$Accuracy

y_preds <- simulate(m2, nsim = n.sims, newdata = elbows_test)

true.mean <- mean(total_correct(y_test, test_items))
true.sd <- sd(total_correct(y_test, test_items))

pred_n.correct <- sapply(1:n.sims, function(s) total_correct(y_preds[,s], test_items))

pred.means <- data.frame(mean_correct = colMeans(pred_n.correct))
mean_summary <- mean(pred.means$mean_correct)

pred.sds <- data.frame(sd_correct = apply(pred_n.correct, 2, sd))
sd_summary <- mean(pred.sds$sd_correct)


pred.means %>%
  ggplot(aes(x = mean_correct)) + geom_histogram(bins = 30, color = "darkgray", fill = "white", size = 0.5) +
  geom_vline(aes(xintercept = true.mean, color = "True Mean"), size = 1.5) +
  geom_vline(aes(xintercept = mean_summary + 0.06, color = "Simulations Mean"), size = 1) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(breaks = seq(14, 16, 0.5)) +
  scale_color_discrete(name = "") + 
  labs(title = "Testing Data", x = "Mean of Correct Responses") 

pred.sds %>%
  ggplot(aes(x = sd_correct)) + geom_histogram(bins = 30, color = "darkgray", fill = "white", size = 0.5) +
  geom_vline(aes(xintercept = true.sd, color = "True Mean"), size = 1.5) +
  geom_vline(aes(xintercept = sd_summary + 0.06, color = "Simulations SD"), size = 1) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_color_discrete(name = "") + 
  labs(title = "Testing Data", x = "SD of Correct Responses") 


###### Prediction of next 20 items ######
#### Aggregate Learning Curve #########

pred_ratio <- array(NA, c(test_items, n.sims))
pred_ratio <- apply(y_preds, 2, test_ratio, test_items)

q50 <- apply(pred_ratio, 1, quantile, 0.5)
conf50 <- t(apply(pred_ratio, 1, quantile, c(0.25, 0.75)))
conf95 <- t(apply(pred_ratio, 1, quantile, c(0.025, 0.975)))

## Plot simulations vs Real Data
long_sims <- data.frame(cbind(q50, conf50, conf95)) %>%
  mutate(n = 1:n()) 

names(long_sims)[1:5] <- c("mid50", "lb50", "ub50", "lb95", "ub95")


elbows_test %>%
  group_by(Sequence) %>%
  summarise(prop_correct = mean(Accuracy), n = n()) %>%
  mutate(prop_se = sqrt(prop_correct*(1 - prop_correct)/n)) -> elbow_agg_test


ggplot(long_sims, aes(x = n + 80)) +
  geom_linerange(aes(ymin = lb95, ymax = ub95, color = "95% PI"), alpha = 0.5, size = 1) +
  geom_linerange(aes(ymin = lb50, ymax = ub50, color  = "50% PI"), alpha = 0.8, size = 1.5) +
  geom_point(data = elbow_agg_test, aes(x = Sequence + 80, y = prop_correct, color = "Observed"), size = 1.5) +
  # geom_line(aes(y = mid50, color = "Sim Median"), size = 1, alpha = 0.7) +
  geom_smooth(aes(y = mid50, color = "Median Trend"), method = "loess", size = 1, se = F) +
  scale_color_manual(
    name = "",
    values = c(`Median Trend` = "darkred",
               `50% PI`= "skyblue4",
               `95% PI` = "gray65",
               `Observed` = "black"),
    guide = guide_legend(override.aes = list(linetype = c(rep("solid", 2), rep("blank", 2)),
                                             shape = c(NA, NA, 16, 16)))) +
  scale_x_continuous() +
  labs(y = "Proportion Correct", x = "Item in Sequence",
       title = "Aggregate Learning Curve for Learners",
       subtitle = "Held-out Data - Elbow Dataset") +
  theme_classic() # -> p2

cowplot::plot_grid(p1, p2, rel_widths = c(0.8, 1.1))


########## Prediction Intervals ############

m0_preds <- arm::sim(m0, n.sims)
X <- cbind(rep(1, nrow(elbows_test)), elbows_test$sclSeq)
p.sim <- sapply(1:n.sims, function(s) invlogit(X %*% coef(m0_preds)[s,]))

conf95 <- t(apply(p.sim, 1, quantile, c(0.025, 0.5, 0.975)))

p.intervals <- cbind(elbows_test, conf95) %>%
  filter(RaterID %in% selected_raters, Sequence == 1) %>% 
  arrange(factor(RaterID, levels = selected_raters))


test_df <- expand.grid(RaterID = selected_raters, sclSeq = 4.05)
m1_preds <- predictInterval(m1, newdata = test_df, n.sims = n.sims, type = "probability", include.resid.var = F)
round(m1_preds, 2)


test_df <- expand.grid(RaterID = selected_raters, sclSeq = 4.05, CaseType2 = "Normal")
m2_preds <- predictInterval(m2, newdata = test_df, n.sims = n.sims, type = "probability", include.resid.var = F)
round(m2_preds, 2)

test_df <- expand.grid(RaterID = selected_raters, sclSeq = 4.05, CaseType2 = "Abnormal/Other")
m2_preds <- predictInterval(m2, newdata = test_df, n.sims = n.sims, type = "probability", include.resid.var = F)
round(m2_preds, 2)


no_pool <- elbows %>% 
  filter(RaterID %in% selected_raters) %>%
  group_by(RaterID)  %>%
  do(single_glm = glm(Accuracy ~ sclSeq, data = ., family = binomial))%>%
  arrange(factor(RaterID, levels = selected_raters))

x_tilde <- c(1, 4.05)
sim_list <- lapply(no_pool$single_glm, function(x) coef(arm::sim(x, n.sims)))


no_pool.sim <- sapply(sim_list, function(x) sapply(1:n.sims, function(s) invlogit(x_tilde %*% x[s,])))
round(t(apply(no_pool.sim, 2, quantile, c(0.025, 0.5, 0.975))),2)

elbows_test %>%
  filter(RaterID %in% selected_raters) %>%
  group_by(RaterID) %>%
  summarise(mean_score = mean(Accuracy))%>%
  arrange(factor(RaterID, levels = selected_raters)) %>%
  mutate(mean_score = round(mean_score, 2))


#### Confidence Intervals ####
m <- random_intercept

coef(m)[["RaterID"]] %>% 
  rownames_to_column("RaterID") %>%
  filter(RaterID == 47)

m.sims <- arm::sim(m, 1e4)

int_sims <- fixef(m)[1] + m.sims@ranef$RaterID[,,1]
slp_sims <- fixef(m)[2] + m.sims@ranef$RaterID[,,2]

data.frame(t(apply(int_sims, 2, quantile, c(0.025, 0.975)))) %>%
  rownames_to_column("RaterID") %>% filter(RaterID == 47)

df_intercept %>% filter(RaterID == 47)
confint(m)
# Beyond this point is only the ECG Data set, most of the code is
# the same except for the prediction on new data.
#----------------------------------------------------------------
########################## ECGs #################################

rm(list = ls())
source("R/utils.R")
ecg <- readRDS("data/interim/ecg_set1.rds")
ecg_test <- readRDS("data/interim/ecg_set2.rds")

# True values
n.sims <- 10000
n.items <- 100
n.learner <- length(unique(ecg$userId))
y <- ecg$correctDx
y_test <- ecg_test$correctDx


## Transforming Data
# Rescale Sequence variable for fitting issues. If Sequence is left
# in its raw scale, glmer throws convergence warnings (one reason to move to rstanarm)
ecg$sclSeq <- ecg$Sequence/20
ecg_test$sclSeq <- ecg_test$Sequence/20

######## Model Fitting #######
# Section 5 Models in the paper
m0 <- glm(correctDx ~ sclSeq, data = ecg, family = binomial(link = "logit"))
# ECG Data
m0.ecg <- glmer(correctDx ~ 1 + sclSeq + (1 + sclSeq|userId), data = ecg, 
                   family = binomial(link = "logit"))


m1.ecg <- glmer(correctDx ~ 1 + sclSeq*caseType2 + (1 + sclSeq|userId), data = ecg, 
                   family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))


################### Model Evaluation ###############
## Simulations
####################################################


###############################
### Mean and Standard Deviation of the 
### total correct responses across all learners
###############################

m0.rep <- simulate_obs(m0, data = ecg, pooling = T, n_sims = n.sims)$sim_labels
m1.rep <- simulate_obs(m0.ecg, data = ecg, pooling = F, n_sims = n.sims)$sim_labels
m2.rep <- simulate_obs(m1.ecg, data = ecg, pooling = F, n_sims = n.sims)$sim_labels


g1 <- summary_stat_plot(y_sims = m0.rep, y_real = y, model = "Complete Pooling", 
                        n_items = n.items, n_learners = n.learner)
g2 <- summary_stat_plot(y_sims = m1.rep, y_real = y, model = "MLM - No Diagnosis",
                        n_items = n.items, n_learners = n.learner)
g3 <- summary_stat_plot(y_sims = m2.rep, y_real = y, 
                        model = "MLM - With Diagnosis", n.items, n.learner)

## Mean
leg <- cowplot::get_legend(g1$mean_plot + theme(legend.position = "bottom"))
grid1 <- cowplot::plot_grid(g1$mean_plot + theme(legend.position = "none"), 
                            g2$mean_plot + theme(legend.position = "none"), 
                            g3$mean_plot + theme(legend.position = "none"), nrow = 1)
cowplot::plot_grid(grid1, leg, nrow = 2, rel_heights = c(1, 0.1))

## SD
leg <- cowplot::get_legend(g1$sd_plot + theme(legend.position = "bottom"))
grid1 <- cowplot::plot_grid(g1$sd_plot + theme(legend.position = "none"), 
                            g2$sd_plot + theme(legend.position = "none"), 
                            g3$sd_plot + theme(legend.position = "none"), nrow = 1)
cowplot::plot_grid(grid1, leg, nrow = 2, rel_heights = c(1, 0.1))

###### Aggregate Learning Curve #########
# Plot proportion: Selected test statistic
# Compute Aggregate proportion - observed
# Only done for the biggest model Eq 14

source("R/utils.R")

sim_ratio <- array(NA, c(n.items, n.sims))
sim_ratio <- apply(m2.rep, 2, test_ratio, n.items)

ecg %>%
  group_by(Sequence) %>%
  summarise(prop_correct = mean(correctDx), n = n()) %>%
  mutate(prop_se = sqrt(prop_correct*(1 - prop_correct)/n))  -> ecg_agg

ecg_agg %>%
  ggplot(aes(x = Sequence, y = prop_correct)) + 
  geom_point() +
  # geom_pointrange(aes(ymax = prop_correct + prop_se,
  #                     ymin = prop_correct - prop_se), alpha = 0.4) + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0.37, 0.72)) +
  theme_classic() +
  labs(y = "Proportion Correct", x = "Number of Items", 
       # title = "Population Level Learning Curve", 
       title = "Observed Data - ECG Dataset") -> p1

q50 <- apply(sim_ratio, 1, quantile, 0.5)
conf50 <- t(apply(sim_ratio, 1, quantile, c(0.25, 0.75)))
conf95 <- t(apply(sim_ratio, 1, quantile, c(0.025, 0.975)))

## Plot simulations vs Real Data
long_sims <- data.frame(cbind(q50, conf50, conf95)) %>%
  mutate(n = 1:n()) 

names(long_sims)[1:5] <- c("mid50", "lb50", "ub50", "lb95", "ub95")
# ecg_agg_test$key <- "real"

ggplot(long_sims, aes(x = n)) +
  geom_linerange(aes(ymin = lb95, ymax = ub95, color = "95% PI"), alpha = 0.5, size = 1) +
  geom_linerange(aes(ymin = lb50, ymax = ub50, color  = "50% PI"), alpha = 0.8, size = 1.5) +
  
  geom_point(data = ecg_agg, aes(x = Sequence, y = prop_correct, color = "Observed"), size = 1.5) +
  # geom_line(aes(y = mid50, color = "Sim Median"), size = 1, alpha = 0.7) +
  geom_smooth(aes(y = mid50, color = "Median Trend"), method = "loess", size = 1, se = F) +
  scale_color_manual(
    name = "",
    values = c(
      `Median Trend` = "darkred",
      `50% PI`= "skyblue4",
      `95% PI` = "gray65",
      `Observed` = "black"),
    guide = guide_legend(override.aes = list(linetype = c(rep("solid", 2), rep("blank", 2)),
                                             shape = c(NA, NA, 16, 16)))) +
  scale_y_continuous(limits = c(0.37, 0.72)) +
  labs(y = "Proportion Correct", x = "Number of Items",
       # title = "Population Level Learning Curve",
       title = "Model Simulated Data - ECG Dataset") +
  theme_classic() -> p2


cowplot::plot_grid(p1, p2, rel_widths = c(0.8, 1.1))

new_sims <- simulate(m1.ecg, allow.new.levels = T, newdata = ecg_test, 
                     nsim = n.sims, re.form = NA)
new_ratio <- array(NA, c(n.items, n.sims))
new_ratio <- apply(new_sims, 2, test_ratio, n.items) 

ecg_test %>%
  group_by(Sequence) %>%
  summarise(prop_correct = mean(correctDx), n = n()) %>%
  mutate(prop_se = sqrt(prop_correct*(1 - prop_correct)/n))  -> ecg_agg_test

