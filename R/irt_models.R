library(tidyverse)

########### ELBOW DATA #############


## Read Data
elbows <- readRDS("data/interim/train_elbows.rds")
elbowLabels <- elbows %>% 
  group_by(CaseDx, CaseID) %>% summarise()

### One-Parameter Logistic (1PL) ###
# one_pl.elbow <- glmer(Accuracy ~ -1 + CaseID + (1|RaterID),
#                     data = train, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))


# For time saving, load previously fitted model
one_pl.elbow <- readRDS("models/1PL-elbows.rds")
summary(one_pl.elbow)
# Extract parameter estimates for item difficulties

# Fixed Effects
fixef(one_pl.elbow) %>% as.data.frame() %>%
  rownames_to_column("CaseID") %>% 
  mutate(CaseID = str_replace(CaseID, "CaseID", "")) -> one_pl_data

names(one_pl_data)[2] <- "one_pl_logit"

##### Data for Plot ICC for 1PL #####

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

write.csv(test.info.df, 'data/output/1PL-elbows-ICC.csv')

###### LLTM Model (IRT) ######

# lltm.elbow <- glmer(Accuracy ~ 1 + CaseDx + (1 + CaseDx|CaseID) + (1|RaterID),
#                     data = train, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = 'nloptwrap', calc.derivs = FALSE))
# 

# LOAD Model
lltm.elbow <-  readRDS("models/LLTM-elbows.rds")
summary(lltm.elbow)
# Extract Fixed effects for each diagnosis

lltm_data <- broom::tidy(lltm.elbow)  %>%
  filter(group == "fixed") %>%
  dplyr::select(CaseDx = term, FE_dx = estimate)%>%
  mutate(CaseDx = str_replace(CaseDx, "CaseDx", ""), 
         FE_dx = -1*FE_dx) %>%
  arrange(desc(FE_dx))

lltm_data$CaseDx[str_detect(lltm_data$CaseDx,"(Intercept)")] <- "Normal"

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
saveRDS(item_lltm_data, 'data/output/LLTM-elbows.rds')


########### ECG DATA #############
rm(list = ls())
ecg <- readRDS('data/interim/ecg_set1.rds')

ecgLabels <- ecg %>% 
  group_by(cardiologistDxLabel, ecgId) %>% summarise()

### One-Parameter Logistic (1PL) ###


# one_pl.ecg <- glmer(correctDx ~ -1 + ecgId + (1|userId),
#                     data = ecg, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = "bobyqa))

one_pl.ecg <- readRDS("models/1PL-ecg.rds")
summary(one_pl.ecg)

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

saveRDS(test.info.df, 'data/output/1PL-ecg-ICC.rds')


####### LLTM ############## 

# lltm.ecg <- glmer(correctDx ~ 1 + cardiologistDxLabel + (1 + cardiologistDxLabel|ecgId) + (1|userId),
#                     data = ecg, family = binomial(link = "logit"),
#                     control = glmerControl(optimizer = "bobyqa"))

lltm.ecg <-  readRDS("models/LLTM-ecg.rds")
summary(lltm.ecg)

lltm_data <- broom::tidy(lltm.ecg) %>%
  filter(group == "fixed") %>%
  dplyr::select(cardiologistDxLabel = term, FE_dx = estimate) %>%
  mutate(cardiologistDxLabel= str_replace(cardiologistDxLabel, "cardiologistDxLabel", ""),
         FE_dx = -1*FE_dx) %>%
  arrange(desc(FE_dx))

lltm_data$cardiologistDxLabel[str_detect(lltm_data$cardiologistDxLabel, "(Intercept)")] <- "Normal ECG"


item_lltm_data <- ranef(lltm.ecg)$ecgId %>% 
  rownames_to_column("ecgId") %>%
  rename('Normal ECG' = `(Intercept)`) %>%
  gather(dx, RE_item, -ecgId) %>% 
  mutate(dx = str_replace(dx, "cardiologistDxLabel", "")) %>% 
  left_join(ecgLabels) %>% 
  filter(cardiologistDxLabel == dx) %>%
  left_join(lltm_data) 

item_lltm_data$cardiologistDxLabel <- factor(item_lltm_data$cardiologistDxLabel, levels = lltm_data$cardiologistDxLabel)

saveRDS(item_lltm_data, 'data/output/LLTM-ecg.rds')
