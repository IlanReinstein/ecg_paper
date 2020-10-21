library(tidyverse)
library(ggplot2)
library(lme4)

# library(arm)
# library(magrittr)
# library(bayesplot)
# library(sjPlot)
# library(tidybayes)

options(digits = 2)
options(mc.cores = parallel::detectCores())
color_scheme_set("brightblue")
select <- dplyr::select

red <- RColorBrewer::brewer.pal(3, "Set1")[1]
blue <- RColorBrewer::brewer.pal(3, "Set1")[2]
######### Data Reading and Processing ########
elbows <- readRDS("data/interim/train_elbows.rds")

elbows$sclSeq <- elbows$Sequence/20
######## Section 3 Models ############
######################################


##### Linear Learning Models #########
## TODO

######################################
##### Nonlinear Learning Models ######

###### Multilevel Logistic - No Item or Diagnosis Effects
elbows$tSeq <- elbows$Sequence - 1
# Population level, Complete Pooling Model
nlm0 <- glm(Accuracy ~ sclSeq, data = elbows, family = binomial(link = "logit"))
# stan_nlm0 <- stan_glm(Accuracy ~ Sequence, data = train, family = binomial(link = "logit"))
summary(nlm0)
sjPlot::get_model_data(nlm0, transform = "plogis")

predict(nlm0, type = "response", se.fit = T) -> nlm0.preds
elbows$preds <- nlm0.preds$fit
elbows$preds.se <- nlm0.preds$se.fit

elbows %>% filter(RaterID == 50) %>% 
  ggplot(aes(x = Sequence, y = preds, 
             ymax = preds + 1.96*preds.se, ymin = preds - 1.96*preds.se)) +
  geom_pointrange() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y= "Probability of Correct Response", title = "Complete Pooling Model", subtitle = "Elbow Dataset - Learner 50")



# Random Coefficients, Learning Rates and base knowledge estimates by person
nlm1.elbows <- glmer(Accuracy ~ sclSeq + (1 |RaterID), data = elbows, family = binomial(link = "logit"))
summary(nlm1.elbows)

# Join Table with prediction for Within-learner learning curve
elbowPlot <- bind_cols(elbows, predictInterval(nlm1.elbows, include.resid.var = F,type = "probability", stat = "median"))

# Single learner curve, No Diagnosis
green <- RColorBrewer::brewer.pal(3, "Set2")[1]

elbowPlot %>% filter(RaterID == 50) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr)) + 
  geom_pointrange(alpha = 0.7, color = green) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits= c(0.5, 0.85), breaks = c(0.4, 0.5, 0.6, 0.7, 0.8)) + #, limits= c(0.4, 0.95), breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve, No diagnosis",
       subtitle = paste0("Elbow Dataset - Learner ", 50)) + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 19), title = element_text(size = 20))



# Loop through all raters and save
# for(rater in unique(train$RaterID)){
#   elbowPlot %>% filter(RaterID == rater) -> rater_df
#   ggplot(rater_df, aes(x = Sequence, y = fit, ymin = lwr, ymax = upr)) + 
#     geom_pointrange(alpha = 0.7, color = "#66C2A5") + 
#     scale_y_continuous(limits = c(0,1)) +
#     labs(y = "Probability of Correct Response", color = "Diagnosis", 
#          title = "Learning Curve with Diagnosis-Sequence Interaction",
#          subtitle = paste0("Elbow Dataset - Learner ", rater))  -> p
#   
#   ggsave(paste0("figures/learning-curves-elbows/noDx/lc_elbows_noDx_", rater, ".png"), p, device = "png", width = 3, height = 2)
# }


######### Section 4 Models ###########


######### 1PL Model (IRT) ######

### GLMER definition
# one_pl.elbow <- glmer(Accuracy ~ -1 + CaseID + (1|RaterID),
#                     data = train, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))
# saveRDS(one_pl.elbow, "models/1pl-elbows_3-2019.rds")

# For time saving, load previously fitted model
one_pl.elbow <- readRDS("models/1PL-elbows.rds")
summary(one_pl.elbow)
# Extract parameter estimates for item difficulties

# Fixed Effects
fixef(one_pl.elbow) %>% as.data.frame() %>%
  rownames_to_column("CaseID") %>% 
  mutate(CaseID = str_replace(CaseID, "CaseID", "")) -> one_pl_data

names(one_pl_data)[2] <- "one_pl_logit"

######### Plot ICC for 1PL #####

item.diff <- -1*coef(summary(one_pl.elbow))[,1]
item.diff <- data.frame(
  item.diff = as.numeric(item.diff), item = paste0("V", 1:80))

{
  theta.s <- seq(-6.5, 6.5, .1) # Person abilities for prediction
  pred.prob <- c() # Vector to hold predicted probabilities
  test.info.df <- c() # Vector to hold test info
  for (i in theta.s) { # Loop through abilities
    for (j in 1:80) { # Loop through items
      l <- i - item.diff$item.diff[j] # log-odds is ability - difficulty
      l <- exp(-l) # Exponentiate -log-odds
      l <- 1 / (1 + l) # Calculate predicted probability
      pred.prob <- c(pred.prob, l) # Store predicted probability
      l <- l * (1 - l) # Calculate test information
      test.info.df <- c(test.info.df, l) # Store test information
    }
  }
  # Save it all to data frame
  test.info.df <- data.frame(
    theta = sort(rep(theta.s, 80)),
    item = rep(paste0("V", 1:80), length(theta.s)),
    info = test.info.df,
    prob = pred.prob,
    diff = item.diff$item.diff
  )
  rm(i, j, theta.s, pred.prob, l) # Clean environment
}

ggplot(test.info.df, aes(x = theta, y = prob, group = reorder(item, diff, mean))) +
  geom_line(color = color_pals[2], alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = -7:7, limits = c(-7, 7)) +
  labs(x = "Person ability", y = "Probability of correct response", colour = "Item",
       title = "Joint Item Characteristic Plot", subtitle = "Elbow Dataset") + 
  geom_segment(aes(y = 0.5, yend = 0.5, x = -Inf, xend = 4.156322), linetype = "dashed", alpha = 0.5, size = 0.2) +
  geom_segment(aes(y = -Inf, yend = 0.5, x = 4.156322, xend = 4.156322), linetype = "dashed", alpha = 0.5, size = 0.2) +
  tidybayes::theme_tidybayes()

  
  
######### LLTM Model (IRT) ######

# lltm.elbow <- glmer(Accuracy ~ 1 + CaseDx + (1 + CaseDx|CaseID) + (1|RaterID),
#                     data = train, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))
# 
# saveRDS(lltm.elbow , "models/lltm-elbows_4-2019.rds")

# LOAD Model
lltm.elbow <-  readRDS("models/LLTM-elbows.rds")

# Extract Fixed effects for each diagnosis

lltm_data <- broom::tidy(lltm.elbow)  %>%
  filter(group == "fixed") %>%
  dplyr::select(CaseDx = term, FE_dx = estimate)%>%
  mutate(CaseDx = str_replace(CaseDx, "CaseDx", ""), 
         FE_dx = -1*FE_dx) %>%
  arrange(desc(FE_dx))

lltm_data$CaseDx[str_detect(lltm_data$CaseDx,"(Intercept)")] <- "Normal"

elbowLabels <- elbows %>% 
  group_by(CaseDx, CaseID) %>% summarise()
# Extract Random Effects for items
item_lltm_data <- ranef(lltm.elbow)$CaseID %>% 
  rownames_to_column("CaseID") %>%
  rename('Normal' = `(Intercept)`) %>%
  gather(dx, RE_item, -CaseID) %>% 
  mutate(dx = str_replace(dx, "CaseDx", "")) %>% 
  left_join(elbowLabels) %>% 
  filter(CaseDx == dx) %>% 
  left_join(lltm_data)

item_lltm_data$CaseDx <- factor(item_lltm_data$CaseDx, levels = lltm_data$CaseDx)

###### Tukey plot for Elbow DataSet ######


item_lltm_data  %>% 
  ggplot(aes(x = CaseDx, y = FE_dx + RE_item, fill = CaseDx)) + 
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.5, outlier.size = 1) +
  geom_point(size = 2, alpha = 0.7, show.legend = F) +
  # geom_point(aes(y = FE_dx + logit)) + 
  geom_point(aes(y = FE_dx), shape = 18, size = 6, show.legend = F) +
  theme_tidybayes() +
  theme(axis.ticks.x = element_blank(), legend.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17), title = element_text(size = 18)) +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(limits = c(-4,4), breaks = seq(-4,4,1)) +
  labs(y = "Log of Odds of Accurate Response", x = "", 
       title = "Distributions of Difficulty by Diagnosis", 
       subtitle = "Elbow Dataset",
       fill = "Diagnosis") -> p2

####### MAKE ONE BIG DATASET WITH ITEM EFFECTS ######

item_lltm_data %>% 
  left_join(one_pl_data) %>%
  left_join(elbows) %>% 
  arrange(RaterID, Sequence)-> elbows_wide

elbows_wide$CaseDx <- relevel(factor(elbows_wide$CaseDx), ref = "Normal")


###### Noisy Learning Curves ######


elbowPlot <- bind_cols(elbows_wide, predictInterval(nlm1.elbows, level = 0.95, include.resid.var = F))



## Generate learning curve by computing linear predictor:
## logit(p) =  linear growth - fixed effect (diagnosis) - random effect item
## i.e., combine Random coefficients model learning curves and correct with LLTM difficulty estimates.

elbowPlot %>% filter(RaterID == 34) %>%
  ggplot(aes(x = Sequence, y = invlogit(fit - (FE_dx + RE_item)))) + 
  geom_line(alpha = 0.7) + geom_point(aes(color = factor(CaseDx, levels = lltm_data$CaseDx))) + 
  stat_smooth(method = "loess", se = F) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.ticks.x = element_blank(), legend.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17), title = element_text(size = 18)) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Item Effects Correction",
       subtitle = "Elbow Dataset - Learner 34") + scale_color_brewer(palette = "Set1")


## Learning Curves Corrected by Diagnosis 

## Combine RHS of random coefficients logistic regression with RHS of LLTM

## Add fitted lines from multilevel logistic random coefficients to the wide data set

## Compute Difficulty Correction Parameters

## Add correction terms to diagnosis difficulty for noisy curve
## Estimate correction parameter for diagnosis using LLTM difficulty

test_m1 <- glmer(Accuracy ~ sclSeq + (sclSeq|RaterID) + one_pl_logit,
                 data = elbows_wide, family = binomial,
                 control = glmerControl(optimizer = 'bobyqa'))

elbowPlot <- bind_cols(elbows_wide, predictInterval(test_m1, level = 0.95, type = "probability", include.resid.var = F))

elbowPlot %>% filter(RaterID == 34) %>%
  ggplot(aes(x = Sequence, y = fit)) +
  geom_line(alpha = 0.7) + geom_point(aes(color = factor(CaseDx, levels = lltm_data$CaseDx))) +
  stat_smooth(method = "loess", se = F) +
  # scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(y = "Probability of Correct Response", color = "Diagnosis",
       title = "Learning Curve with Diagnosis Correction Term",
       subtitle = "Elbow Dataset - Learner 34") +
  scale_color_brewer(palette = "Paired")

# ### Add additional item parameter for correcting noisy curve
# test_m2 <- glmer(Accuracy ~ sclSeq + (sclSeq|RaterID) + FE_dx + RE_item,
#       data = elbows_wide, family = binomial,
#       control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))
# 
# elbowPlot <- bind_cols(elbows_wide, predictInterval(test_m2, level = 0.95))
# 
# elbowPlot %>% filter(RaterID == 34) %>%
#   ggplot(aes(x = Sequence, y = invlogit(fit))) + 
#   geom_point(aes(color = factor(CaseDx, levels = lltm_data$CaseDx))) + 
#   # stat_smooth(method = "loess", se = F) + 
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
#   labs(y = "Probability of Correct Response", color = "Diagnosis", 
#        title = "Learning Curve with Diagnosis Correction Term",
#        subtitle = "Elbow Dataset - Learner 34") + 
#   scale_color_brewer(palette = "Paired")



########### Section 5 ############
##################################

#### Introducing Diagnosis into model as interaction ####

# Random Coefficients for persons plus interaction term and caseType

#### NORMAL - ABNORMAL ####

# m3.elbows <- glmer(Accuracy ~ 1 + sclSeq*CaseType + (1 + sclSeq|RaterID), data = train, 
#             family = binomial(link = "logit"))
# 
# # Add prediction interval at within-learner level
# 
# elbowPlot.nabn <- bind_cols(train, predictInterval(m3.elbows, level = 0.95, include.resid.var = F, type = "probability"))
# 
# # Plot Individual Learning Curve
# 
# elbowPlot.nabn %>% filter(RaterID == 50) %>%
#   ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = CaseType)) + 
#   geom_pointrange() +
#   scale_y_continuous(limits = c(0.49,0.9), labels = scales::percent) +
#   labs(y = "Probability of Correct Response", color = "Diagnosis", 
#        title = "Learning Curve with Diagnosis",
#        subtitle = "Elbow Dataset - Learner 50") + scale_color_brewer(palette = "Set2") 
# 
# # Plot Marginal Curves for Diagnosis (ignore learners)
# 
# plot_model(m3.elbows, transform = NULL, type = "pred", terms = c("sclSeq", "CaseType")) + 
#   scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
#   scale_x_continuous(labels = c(0,20,40,60,80)) + 
#   labs(y = "Probabilty of Correct Response", x = "Number of Items Reviewed", title = "Learning Curve by Diagnosis", 
#        subtitle = "Elbow Dataset - Normal/Abnormal Diagnosis", color = "Diagnosis")
# 
# # All learners plot
#   
# ggplot(elbowPlot.nabn, aes(x = Sequence, y = fit, group = RaterID, color = CaseType)) + geom_line( alpha = 0.7, show.legend = F) + 
#   scale_y_continuous(labels = scales::percent, limits = c(0.25,0.9)) +
#   scale_color_brewer(palette = "Set2") +
#   labs(title = "Random Coefficients model with Diagnosis-Sequence Interaction", subtitle = "Elbow Dataset", y = "Log-Odds of Correct Answer") + facet_wrap(~CaseType)

# Loop through al learners and save

# for(rater in unique(train$RaterID)){
#   elbowPlot %>% filter(RaterID == rater) -> rater_df
#   ggplot(rater_df, aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = CaseType)) + 
#     geom_pointrange(alpha = 0.7) + 
#     scale_y_continuous(limits = c(0,1)) +
#     labs(y = "Probability of Correct Response", color = "Diagnosis", 
#          title = "Learning Curve with Diagnosis",
#          subtitle = paste0("Elbow Dataset - Learner ", rater)) + scale_color_brewer(palette = "Set2") -> p
#   
#   ggsave(paste0("figures/learning-curves-elbows/nAbnDx/lc_elbows_nabn_", rater, ".png"), p, device = "png", width = 5.5, height = 4)
# }


#### NORMAL-ABNORMAL-SUPRACONDYLAR ####

m3.1.elbows <- glmer(Accuracy ~ 1 + sclSeq*CaseType2 + (1 + sclSeq|RaterID), data = elbows, 
                     family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))

predp <- predict(m3.1.elbows, allow.new.levels = T, type = "response")
print(c(mean(predp[elbows$Accuracy == 1]), mean(1 - predp[test$Accuracy == 0])))
print(sum(log(c(predp[elbows$Accuracy == 1], 1 - predp[test$Accuracy == 0]))))

# Within-learner prediciton interval

elbowPlot.3dx <- bind_cols(elbows, predictInterval(m3.1.elbows, type = "probability", include.resid.var = F))
elbowPlot.3dx2 <- bind_cols(test, predictInterval(m3.1.elbows, newdata = test, type = "probability", include.resid.var = F))
# Individual learning curve

elbowPlot.3dx <- elbowPlot.3dx %>% group_by(RaterID, CaseType2) %>% add_tally()
elbowPlot.3dx2 <- elbowPlot.3dx2 %>% group_by(RaterID, CaseType2) %>% add_tally()

elbowPlot.3dx$CaseType2 <- factor(elbowPlot.3dx$CaseType2, 
                                  labels =  c("Normal (N = 33)", "Supracondylar (N = 19)", "Abnormal/Other (N = 28)"),
                                  levels = c("Normal", "Supracondylar", "Abnormal/Other"))

elbowPlot.3dx2$CaseType2 <- factor(elbowPlot.3dx2$CaseType2, 
                                  labels =  c("Normal (N = 8)", "Supracondylar (N = 6)", "Abnormal/Other (N = 6)"),
                                  levels = c("Normal", "Supracondylar", "Abnormal/Other"))

elbowPlot.3dx %>% filter(RaterID == 34) %>%
  ggplot( aes(x = Sequence + 80, y = fit, ymin = lwr, ymax = upr, color = CaseType2)) + 
  geom_pointrange(alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), #limits = c(0.4, 0.9) ,
                     # breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
                     ) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       x = "Sequence",
       subtitle = paste0("Elbow Dataset - Learner ", 50)) + scale_color_brewer(palette = "Set2") + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 19), title = element_text(size = 20)) 


elbowPlot %>% group_by(Sequence) %>%
  summarise(prop_fit = mean(fit), lwr = quantile(fit, 0.25), upr = quantile(fit, 0.75)) %>% 
  bind_cols(elbow_agg) %>%
  ggplot(aes(Sequence, ymin = lwr, ymax = upr)) + geom_line(aes(y = prop_fit)) +
  geom_point(aes(y = prop_correct), size = 1.5) + geom_ribbon(alpha = 0.3)
