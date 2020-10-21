library(tidyverse)
library(ggplot2)
library(lme4)
library(rstanarm)
library(arm)
library(magrittr)
library(bayesplot)
library(sjPlot)
library(tidybayes)


options(digits = 2)
color_scheme_set("brightblue")
select <- dplyr::select

######### Data Reading and Processing #######

ecg <- readRDS("data/interim/clean_ecg_set1.rds")
ecg$sclSeq <- ecg$Sequence/20
ecgLabels <- ecg %>% group_by(cardiologistDxLabel, ecgId) %>% summarise()

#### Testing ####
# #### TESTING SET
# 
# ecg2 <- readRDS("data/interim/clean_ecg_set2.rds") %>% ungroup() %>% mutate(userId = as.character(userId),
#                                                                     ecgId = as.character(ecgId)) %>% arrange(userId)
# ecg2$cardiologistDxLabel[str_detect(ecg2$cardiologistDxLabel,"Bundle")] <- "Left Bundle Branch Block"
# ecg2$cardiologistDxLabel <- relevel(factor(ecg2$cardiologistDxLabel), ref = "Normal ECG")
# ecg2 <- within(ecg2, caseType <- if_else(cardiologistDxLabel == "Normal ECG", "Normal", "Abnormal"))
# 
# ecg2$sclSeq <- ecg2$Sequence/20
# 
# ecg2 <- within(ecg2, caseType2 <- if_else(cardiologistDxLabel == "Normal ECG", "Normal", if_else(str_detect(cardiologistDxLabel, "STEMI"), "STEMI", "Abnormal/Non-STEMI")))
# 
# ecg2$caseType <- relevel(factor(ecg2$caseType), ref = "Normal")
# ecg2$caseType2 <- relevel(factor(ecg2$caseType2), ref = "Normal")
# 


ecg %>% 
  group_by(Sequence) %>%
  summarise(n = n(), p = sum(correctDx)/n()) %>%
  mutate(se_p = sqrt(p*(1 - p)/n)) %>%
  ggplot(aes(Sequence, p)) + 
  geom_pointrange(aes(ymin = p-se_p, ymax = p+se_p)) + 
  scale_y_continuous(limits = c(0, 1)) + geom_smooth()

ecg %>% 
  group_by(Sequence, caseType) %>%
  summarise(n = n(), p = sum(correctDx)/n()) %>%
  mutate(se_p = sqrt(p*(1 - p)/n)) %>%
  ggplot(aes(Sequence, p, color = caseType)) + 
  geom_pointrange(aes(ymin = p-se_p, ymax = p+se_p)) + 
  scale_y_continuous(limits = c(0, 1))

ecg %>% 
  group_by(Sequence, caseType2) %>%
  summarise(n = n(), p = sum(correctDx)/n()) %>%
  mutate(se_p = sqrt(p*(1 - p)/n)) %>%
  ggplot(aes(Sequence, p, color = caseType2)) + 
  geom_pointrange(aes(ymin = p-se_p, ymax = p+se_p)) + 
  scale_y_continuous(limits = c(0, 1))


######### Section 3 Models #########
####################################

##### Linear Learning Models 
### TODO

##### Nonlinear Learning Models 
##### Multilevel Logistic - No Item or Diagnosis Effects

# Population level, Complete Pooling Model
nlm0 <- glm(correctDx ~ sclSeq, data = ecg, family = binomial(link = "logit"))

# Random Coefficients, Learning Rates and base knowledge estimates by person
nlm1 <- glmer(correctDx ~ sclSeq + (sclSeq|userId), data = ecg, family = binomial(link = "logit"))


predict(nlm1, newdata = ecg2, type = "response", allow.new.levels = T)


ecgPlot <- bind_cols(ecg, merTools::predictInterval(nlm1, level = 0.95, include.resid.var = F, type = "probability"))


ecgPlot %>% filter(userId == 10423) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = caseType)) + 
  geom_pointrange(alpha = 0.7, color =  "#377EB8") +
  scale_y_continuous(limits = c(0.3,0.8), labels = scales::percent) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Diagnosis-Sequence Interaction",
       subtitle = "ECG Dataset - Learner 10423")


######### Section 4 ##########
##############################

####### One-Parameter Logistic ############

# one_pl.ecg <- glmer(correctDx ~ -1 + ecgId + (1|userId),
#                     data = ecg, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = "bobyqa))
# saveRDS(one_pl.ecg, "models/ecg_1pl-3-2019.rds")

# LoadRasch/1PL Model
one_pl.ecg <- readRDS("models/ecg_1pl-3-2019.rds")
broom::tidy(one_pl.ecg)
fixef(one_pl.ecg) %>% as.data.frame() %>%
  rownames_to_column("ecgId") %>% 
  mutate(ecgId = str_replace(ecgId, "ecgId", "")) -> one_pl_data

names(one_pl_data)[2] <- "one_pl_logit"


###### ICC Curve for 1PL ######
item.diff <- -1*coef(summary(one_pl.ecg))[,1]
item.diff <- data.frame(
  item.diff = as.numeric(item.diff), item = paste0("V", 1:100))

item.diff <- filter(item.diff, item.diff < 30)

{
  theta.s <- seq(-6,6,.1) # Person abilities for prediction
  pred.prob <- c() # Vector to hold predicted probabilities
  test.info.df <- c() # Vector to hold test info
  for (i in theta.s) { # Loop through abilities
    for (j in 1:99) { # Loop through items
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
    theta = sort(rep(theta.s, 99)),
    item = rep(paste0("V", 1:99), length(theta.s)),
    info = test.info.df,
    prob = pred.prob,
    diff = item.diff$item.diff
  )
  rm(i, j, theta.s, pred.prob, l) # Clean environment
}

ggplot(test.info.df, aes(x = theta, y = prob, group = reorder(item, diff, mean))) + geom_line(color = blue, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = -6:6, limits = c(-6, 6)) +
  labs(x = "Person ability", y = "Probability of correct response", colour = "Item",
       title = "Joint Item Characteristic Plot", subtitle = "ECG Dataset")



####### LLTM ############## 

# lltm.ecg <- glmer(correctDx ~ 1 + cardiologistDxLabel + (1 + cardiologistDxLabel|ecgId) + (1|userId),
#                     data = ecg, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = "bobyqa"))
# # 
# saveRDS(lltm.ecg, "models/lltm-ecg_4-2019.rds")

lltm.ecg <-  readRDS("models/lltm-ecg_4-2019.rds")


lltm_data <- broom::tidy(lltm.ecg) %>%
  filter(group == "fixed") %>%
  dplyr::select(cardiologistDxLabel = term, FE_dx = estimate) %>%
  mutate(cardiologistDxLabel= str_replace(cardiologistDxLabel, "cardiologistDxLabel", ""),
         FE_dx = -1*FE_dx) %>%
  arrange(desc(FE_dx))

lltm_data$cardiologistDxLabel[str_detect(lltm_data$cardiologistDxLabel,"(Intercept)")] <- "Normal ECG"


item_lltm_data <- ranef(lltm.ecg)$ecgId %>% 
  rownames_to_column("ecgId") %>%
  rename('Normal ECG' = `(Intercept)`) %>%
  gather(dx, RE_item, -ecgId) %>% 
  mutate(dx = str_replace(dx, "cardiologistDxLabel", "")) %>% 
  left_join(ecgLabels) %>% 
  filter(cardiologistDxLabel == dx) %>%
  left_join(lltm_data) 

item_lltm_data$cardiologistDxLabel <- factor(item_lltm_data$cardiologistDxLabel, levels = lltm_data$cardiologistDxLabel)
######Tukey plot for Elbow DataSet #####

item_lltm_data %>% 
  ggplot(aes(x = cardiologistDxLabel, y = FE_dx + RE_item, fill = cardiologistDxLabel)) + 
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.5, outlier.size = 1) +
  geom_point(size = 2, alpha = 0.7, show.legend = F) +
  # geom_point(aes(y = FE_dx + logit)) + 
  geom_point(aes(y = FE_dx), shape = 18, size = 6, show.legend = F) +
  theme_tidybayes() + 
  theme(axis.ticks.x = element_blank(), 
        legend.text = element_text(size = 13),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17), 
        title = element_text(size = 18)) +
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4,4,1)) +
  labs(y = "Log of Odds of Accurate Response", x = "", 
       title = "Distributions of Difficulty by Diagnosis", 
       subtitle = "ECG Dataset",
       fill = "Diagnosis") -> p1

#############################
# MAKE ONE BIG DATSET WITH ITEM EFFECTS
item_lltm_data %>% 
  left_join(one_pl_data) %>%
  left_join(ecg) %>% 
  arrange(userId, Sequence) -> ecg_wide

ecg_wide$cardiologistDxLabel <- relevel(factor(ecg_wide$cardiologistDxLabel), ref = "Normal ECG")

######### Noisy Learning Curves #######

## Add fitted lines from mutilevel logistic random coefficients to the wide data set

ecgPlot <- bind_cols(ecg_wide, predictInterval(nlm1, level = 0.95, include.resid.var = F))



## Generate learning curve by computing linear predictor:
## logit(p) =  linear growth - fixed effect (diagnosis) - random effect item
## i.e., combine Random coefficients model learning curves and correct with LLTM difficulty estimates.

ecgPlot %>% filter(userId == 14173) %>%
  ggplot(aes(x = Sequence, y = invlogit(fit - (FE_dx + RE_item)))) + 
  geom_line(alpha = 0.7) + geom_point(aes(color = factor(cardiologistDxLabel, levels = lltm_data$cardiologistDxLabel))) + 
  stat_smooth(method = "loess", se = F) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.ticks.x = element_blank(), legend.text = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17), title = element_text(size = 18)) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Item Effects Correction",
       subtitle = "ECG Dataset - Learner 14173") + scale_color_brewer(palette = "Paired")


# Learning Curves Corrected By Diagnosis

## Add correction terms to iagnosis difficulty for noisy curve
# Estimate correction parameter for diagnosis using LLTM difficulty

test_m1.ecg <- glmer(correctDx ~ sclSeq + (sclSeq|userId) + ecgId,
                 data = ecg_wide, family = binomial,
                 control = glmerControl(optimizer = 'bobyqa'))

merTools::predictInterval(test_m1.ecg, type = "probability")


ecgPlot <- bind_cols(ecg_wide, merTools::predictInterval(test_m1.ecg, type = "probability"))

ecgPlot %>% filter(userId == 14173) %>%
  ggplot(aes(x = Sequence, y = invlogit(fit))) +
  geom_line() + geom_point(aes(color = factor(cardiologistDxLabel, levels = lltm_data$cardiologistDxLabel))) +
  stat_smooth(method = "loess", se = F) +
  # scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(y = "Probability of Correct Response", color = "Diagnosis",
       title = "Learning Curve with Diagnosis Correction Term",
       subtitle = "ECG Dataset - Learner 14173") +
  scale_color_brewer(palette = "Paired")

### Add additional item parameter for correcting noisy curve
# test_m2 <- glmer(Accuracy ~ sclSeq + (sclSeq|RaterID) + FE_dx + RE_item,
#                  data = elbows_wide, family = binomial,
#                  control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))
# 
# elbowPlot <- bind_cols(elbows_wide, predictInterval(test_m2, level = 0.95))
# 
# elbowPlot %>% filter(RaterID == 34) %>%
#   ggplot(aes(x = Sequence, y = invlogit(fit))) + 
#   geom_line() + geom_point(aes(color = CaseDx)) + 
#   stat_smooth(method = "loess", se = F) + 
#   scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
#   labs(y = "Probability of Correct Response", color = "Diagnosis", 
#        title = "Learning Curve with Diagnosis Correction Term",
#        subtitle = "Elbow Dataset - Learner 34")



########## Section 5 Models ##########
#####################################
## Introducing Diagnosis into model as interaction 

# Random Coefficients + Population-Level Diagnosis Parameter
## NORMAL-ABNORMAL
m3.ecg <- glmer(correctDx ~ 1 + sclSeq*caseType + (1 + sclSeq|userId),
            data = ecg, family = binomial(link = "logit"), contrasts = list(caseType = contr.sum))

plot_model(m3.ecg, transform = NULL, type = "pred", terms = c("sclSeq", "caseType")) + 
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + scale_x_continuous(labels = c(0,20,40,60,80, 100)) + 
  labs(y = "Probabilty of Correct Response", x = "Number of Items Reviewed", title = "Learning Curve by Diagnosis", 
       subtitle = "ECG Dataset - Normal/Abnormal Diagnosis")

ecgPlot.nabn <- bind_cols(ecg_wide, predictInterval(m3.ecg, level = 0.95, include.resid.var = F, type = "probability"))


ecgPlot.nabn %>% filter(userId == 10423) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = caseType)) + 
  geom_pointrange(alpha = 0.7) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Diagnosis",
       subtitle = "ECG Dataset - Learner 10423") + scale_color_brewer(palette = "Set2") -> p1


ggplot(ecgPlot, aes(x = Sequence, y = invlogit(fit), group=userId, color = caseType)) + geom_line(alpha = 0.7, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Random Coefficients model with Diagnosis-Sequence Interaction", subtitle = "ECG Dataset", y = "Log-Odds of Correct Answer") + facet_wrap(~caseType)

# Random Coefficients for persons plus interaction term and caseType
## NORMAL-ABNORMAL-STEMI
m3.1.ecg <- glmer(correctDx ~ 1 + sclSeq*caseType2 + (1 + sclSeq|userId),
              data = ecg, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa"))

confint(m3.1.ecg)
summary(m3.1.ecg)

plot_model(m3.1.ecg, transform = NULL, type = "pred", terms = c("sclSeq", "caseType2")) + 
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + scale_x_continuous(labels = c(0,20,40,60,80, 100)) + 
  labs(y = "Probability of Correct Response", x = "Number of Items Reviewed", title = "Learning Curve by Diagnosis", 
       subtitle = "ECG Dataset - Normal/Abnormal/STEMI Diagnosis", color = "") + theme(legend.position = "bottom")


ecgPlot.3dx <- bind_cols(ecg, predictInterval(m3.1.ecg, level = 0.5, type = "probability", stat = "median"))


ecgPlot.3dx %>% filter(userId == 10423) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = caseType2)) + 
  geom_pointrange(alpha = 0.7) +
  scale_y_continuous(limits = c(0.25, 0.9), labels = scales::percent) +
  labs(y = "Probability of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Diagnosis",
       subtitle = "ECG Dataset - Learner 10423") + scale_color_brewer(palette = "Set2") + 
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 19), title = element_text(size = 20))

leg <- cowplot::get_legend(p2)
gridExtra::grid.arrange(gridExtra::arrangeGrob(p1 + theme(legend.position = "none", title = element_blank()), 
                                               p2 + theme(legend.position = "none", title = element_blank()), nrow = 1), nrow = 2, 
                        heights = c(10, 1))

## Group Level learning curves
ggplot(ecgPlot, aes(x = Sequence, y = fit, group=userId, color = caseType2)) + geom_line(alpha = 0.7, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Random Coefficients model with Diagnosis-Sequence Interaction", subtitle = "ECG Dataset", y = "Probability of Correct Answer") + facet_wrap(~caseType2)


#### Predictions on New Data

ecgPlot <- bind_cols(ecg, predictInterval(m3.ecg, level = 0.95, type="probability", include.resid.var = F))
ecgPlot2 <- bind_cols(ecg2, predictInterval(m3.ecg, newdata = ecg2, level = 0.95, type="probability", include.resid.var = F))

ecgPlot2 %>% filter(userId == 11513) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = caseType)) +
  geom_pointrange(alpha = 0.7) +
  scale_y_continuous(limits = c(0.2,0.8), labels = scales::percent) +
  labs(y = "Probabilty of Correct Response", color = "Diagnosis",
       title = "Learning Curve with Diagnosis",
       subtitle = "ECG Dataset - Learner 10423") + scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom")


### No diagnosis Model

ecg2$preds <- predict(nlm1, re.form=NA, newdata = ecg2, allow.new.levels = T)
mm <- model.matrix(terms(nlm1), ecg2)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(nlm1),mm))
tvar1 <- pvar1+VarCorr(nlm1)$userId[1] + VarCorr(nlm1)$userId[4] ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
newdat <- data.frame(
  ecg2
  , plo = ecg2$preds-cmult*sqrt(pvar1)
  , phi =ecg2$preds+cmult*sqrt(pvar1)
  , tlo = ecg2$preds-cmult*sqrt(tvar1)
  , thi = ecg2$preds+cmult*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat %>% filter(userId == 11513), aes(x=Sequence, y=invlogit(preds)))
g0 + geom_pointrange(aes(ymin = invlogit(tlo), ymax = invlogit(thi)),alpha = 0.7, color = "#66C2A5") +
  labs(title="CI based on fixed-effects uncertainty ONLY") + scale_y_continuous(limits = c(0,1))

# Two Diagnoses

ecg2$preds <- predict(m3.ecg, re.form=NA, newdata = ecg2, allow.new.levels = T)
mm <- model.matrix(terms(m3.ecg), ecg2)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(m3.ecg),mm))
tvar1 <- pvar1+VarCorr(m3.ecg)$userId[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
newdat <- data.frame(
  ecg2
  , plo = ecg2$preds-cmult*sqrt(pvar1)
  , phi = ecg2$preds+cmult*sqrt(pvar1)
  , tlo = ecg2$preds-cmult*sqrt(tvar1)
  , thi = ecg2$preds+cmult*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat %>% filter(userId == 11513), aes(x=Sequence, y=invlogit(preds), color = caseType))
g0 + geom_pointrange(aes(ymin = invlogit(tlo), ymax = invlogit(thi)),alpha = 0.7) +
  labs(title="CI based on fixed-effects uncertainty ONLY") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  scale_color_brewer(palette = "Set2")




ecg2$preds <- predict(m3.1.ecg, re.form=NA, newdata = ecg2, allow.new.levels = T)
mm <- model.matrix(terms(m3.1.ecg), ecg2)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(m3.1.ecg),mm))
tvar1 <- pvar1+VarCorr(m3.1.ecg)$userId[1]  ## must be adapted for more complex models
cmult <- 2 ## could use 1.96
newdat <- data.frame(
  ecg2
  , plo = ecg2$preds-cmult*sqrt(pvar1)
  , phi =ecg2$preds+cmult*sqrt(pvar1)
  , tlo = ecg2$preds-cmult*sqrt(tvar1)
  , thi = ecg2$preds+cmult*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat %>% filter(userId == 11513), aes(x=Sequence, y=invlogit(preds), color = caseType2))
g0 + geom_pointrange(aes(ymin = invlogit(tlo), ymax = invlogit(thi)),alpha = 0.7) +
  labs(title="CI based on fixed-effects uncertainty ONLY") + scale_y_continuous(limits = c(0,1)) + 
  scale_color_brewer(palette = "Set2")


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



m1.sims <- simulate(m3.ecg, re.form=NULL, newdata = ecg2, allow.new.levels = T, nsim = 2000, seed = 42)
ecg2$preds_bin <- apply(m1.sims, 1, getmode)
ecg2 %>% filter(userId == 6623) -> user.11513
sum(user.11513$correctDx == user.11513$preds_bin)




mySumm <- function(.) {
  predict(., newdata=ecg2, allow.new.levels = T, type = "response")
}

sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

boot1 <- lme4::bootMer(nlm1, mySumm, nsim = 25, use.u = F, type = "parametric")
boot2 <- lme4::bootMer(m3.ecg, mySumm, nsim = 25, use.u = F, type = "parametric")
boot3 <- lme4::bootMer(m3.1.ecg, mySumm, nsim = 25, use.u = F, type = "parametric")

ecgFull <- bind_rows(ecg, ecg2)

m3.1.full <- glmer(correctDx ~ 1 + sclSeq*caseType2 + (1 + sclSeq*caseType2|userId), 
                   data= ecgFull,family = binomial,
                   control = glmerControl(optimizer = "bobyqa"), contrasts = c(caseType2 = contr.sum))


ecgPlot.full <- bind_cols(ecgFull, predictInterval(m3.1.full, level = 0.95, include.resid.var = F, type = "probability"))


ecgPlot.full %>% filter(userId == 10423) %>%
  ggplot(aes(x = Sequence, y = fit, ymin = lwr, ymax = upr, color = caseType2)) + 
  geom_pointrange(alpha = 0.7) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(y = "Probabilty of Correct Response", color = "Diagnosis", 
       title = "Learning Curve with Diagnosis",
       subtitle = "ECG Dataset - Learner 10423") + scale_color_brewer(palette = "Set2") + 
  theme(legend.position = "bottom")

## Group Level learning curves
ggplot(ecgPlot.full, aes(x = Sequence, y = fit, group=userId, color = caseType2)) + geom_line(alpha = 0.7, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Random Coefficients model with Diagnosis-Sequence Interaction", subtitle = "ECG Dataset", y = "Probability of Correct Answer") + facet_wrap(~caseType2)

plot_model(m3.1.full, transform = NULL, type = "pred", terms = c("sclSeq", "caseType2")) + 
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + scale_x_continuous(labels = c(0,20,40,60,80, 100)) + 
  labs(y = "Probabilty of Correct Response", x = "Number of Items Reviewed", title = "Learning Curve by Diagnosis", 
       subtitle = "ECG Dataset - Normal/Abnormal Diagnosis")

