#two potential methods: GLM and random forest
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(MASS)
library(EnvStats)
library(goftest)
library(actuar)
library(fitdistrplus)
library(randomForest)


hazards <- read.csv('SOA_hazards.csv') %>% arrange(Year) %>% filter(Property.Damage != 0, Hazard.Event != "Fog") %>% mutate (Property.Damage = log(Property.Damage))


events <- hazards %>% mutate(
  Hail = str_detect(Hazard.Event, "Hail"),
  Wind = str_detect(Hazard.Event, "Wind"),
  Severe.thunderstorm = str_detect(Hazard.Event, "Severe Storm"), #all entries with severe storm have thunder storm too
  Lightning = str_detect(Hazard.Event, "Lightning"),
  Coastal = str_detect(Hazard.Event, "Coastal"),
  Flooding = str_detect(Hazard.Event, "Flooding"),
  Wildfire = str_detect(Hazard.Event, "Wildfire"),
  Tornado = str_detect(Hazard.Event, "Tornado"),
  Winter.weather = str_detect(Hazard.Event, "Winter Weather"),
  Hurricane = str_detect(Hazard.Event, "Hurricane"),
  Tropical.storm = str_detect(Hazard.Event, "Tropical Storm"),
  Heat = str_detect(Hazard.Event, "Heat"),
  Drought = str_detect(Hazard.Event, "Drought"),
  .keep = c("unused"))


r1 <- events %>% filter(Region == 1)
r2 <- events %>% filter(Region == 2)
r3 <- events %>% filter(Region == 3)
r4 <- events %>% filter(Region == 4)
r5 <- events %>% filter(Region == 5)
r6 <- events %>% filter(Region == 6)

#_______________________________RANDOM FOREST: see which predictors are the most important
#FYI: random forest basically averages a large number of randomly generated decision trees
#note that random forests cannot extrapolate like glms can
#therefore we're only interested in what predictors RF deems important here - we're not using RFs to predict damages

tuneRF(r1[, -7], r1[, 7]) #indicates mtry =  is optimal
tuneRF(r2[, -7], r2[, 7]) #indicates mtry =  is optimal
tuneRF(r3[, -7], r3[, 7]) #indicates mtry =  is optimal
tuneRF(r4[, -7], r4[, 7]) #indicates mtry =  is optimal
tuneRF(r5[, -7], r5[, 7]) #indicates mtry =  is optimal
tuneRF(r6[, -7], r6[, 7]) #indicates mtry =  is optimal

rF1 <- randomForest(Property.Damage ~ ., data = r1, mtry = 12, importance = TRUE) #6
plot(rF1) #Plot of MSE as number of trees change
varImpPlot(rF1) #dotplot of which predictors are important: the more right the dot is, the more important

rF2 <- randomForest(Property.Damage ~ ., data = r2, mtry = 12, importance = TRUE)
plot(rF2)
varImpPlot(rF2) 

rF3 <- randomForest(Property.Damage ~ ., data = r3, mtry = 12, importance = TRUE) #6
plot(rF3)
varImpPlot(rF3) 

rF4 <- randomForest(Property.Damage ~ ., data = r4, mtry = 12, importance = TRUE)
plot(rF4)
varImpPlot(rF4) 

rF5 <- randomForest(Property.Damage ~ ., data = r5, mtry = 12, importance = TRUE)
plot(rF5)
varImpPlot(rF5) 

rF6 <- randomForest(Property.Damage ~ ., data = r6, mtry = 12, importance = TRUE)
plot(rF6)
varImpPlot(rF6) 

rF1
rF2
rF3
rF4
rF5
rF6

#% Var explained ranges from 25% to 45%

#now for all regions combined - region is now a predictor
tuneRF(events[, -7], events[, 7]) #indicates mtry =  is optimal
(rF <- randomForest(Property.Damage ~ ., data = events, mtry = 12, importance = TRUE)) #60.38% Var explained
plot(rF)
varImpPlot(rF)
#the model where regions are aggregate performs better because the predictor "region" has high explanatory power
#splitting RFs by region removes region as a predictor, and hence reduces the explanatory power


#____________________________________GLM____________________________________________________

GLM <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = events) #for entire country, where region is a predictor

#stepwise regression: both backwards and forwards selection are performed, each model is compared using AIC
GLM_step <- stepAIC(GLM, direction = "both", trace = FALSE)
summary(GLM_step) #stepwise selection removed two predictors: drought and tropical storms


#now run a glm by region

GLM1 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r1)
GLM2 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r2)
GLM3 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r3)
GLM4 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r4)
GLM5 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r5)
GLM6 <- glm(Property.Damage ~ ., family = Gamma(link = "inverse"), data = r6)



GLM_step1 <- stepAIC(GLM1, direction = "both", trace = FALSE)
GLM_step2 <- stepAIC(GLM2, direction = "both", trace = FALSE)
GLM_step3 <- stepAIC(GLM3, direction = "both", trace = FALSE)
GLM_step4 <- stepAIC(GLM4, direction = "both", trace = FALSE)
GLM_step5 <- stepAIC(GLM5, direction = "both", trace = FALSE)
GLM_step6 <- stepAIC(GLM6, direction = "both", trace = FALSE)

summary(GLM_step1)
summary(GLM_step2)
summary(GLM_step3)
summary(GLM_step4)
summary(GLM_step5)
summary(GLM_step6)


#calculate overall p values of each model
#the test statistic is null deviance - residual deviance, with # degrees of freedom = df(null deviance) - df(residual deviance)
#I used an online calculator to compute the p-value: https://www.statology.org/chi-square-p-value-calculator/

tstat <- 206.74 - 170.64 #glm_step
df <- 3030 - 3013

tstat1 <- 40.058 - 30.654 #glm_step1
df1 <- 551 - 538

tstat2 <- 39.617 - 28.619 #glm_step2
df2 <- 705 - 693

tstat3 <- 51.032 - 41.830 #glm_step3
df3 <- 720 - 706

tstat4 <- 29.60 - 25.135 #glm_step4
df4 <- 461 - 451

tstat5 <- 25.583 - 21.627 #glm_step5
df5 <- 381 - 372

tstat6 <- 18.312 - 13.563 #glm_step6
df6 <- 207 - 198

#p_values:
#all: 0.004448
#region 1: 0.741793
#region 2: 0.529090
#region 3: 0.817897
#region 4: 0.923942
#region 5: 0.914285
#region 6: 0.855617

#so the model including region as a predictor performs the best (we want a small p-value)