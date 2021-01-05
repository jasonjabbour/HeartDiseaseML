# Load libraries
library(lattice)
library(ggfortify)
library(ggplot2)
library(glmnet)

# Set working directory and load file
sourcedir <- "C:/Users/Student/Desktop/SYS 4021/DS 4003"
setwd(sourcedir)

heart <- read.table("heart_data.csv", sep = ",", header = T)

# Set categorical and ordinal variables as factors
heart$slope_of_peak_exercise_st_segment <- as.factor(heart$slope_of_peak_exercise_st_segment)
heart$thal <- as.factor(heart$thal)
heart$chest_pain_type <- as.factor(heart$chest_pain_type)
heart$num_major_vessels <- as.factor(heart$num_major_vessels)
heart$resting_ekg_results <- as.factor(heart$resting_ekg_results)
heart$fasting_blood_sugar_gt_120_mg_per_dl <- as.factor(heart$fasting_blood_sugar_gt_120_mg_per_dl)
heart$sex <- as.factor(heart$sex)
heart$exercise_induced_angina <- as.factor(heart$exercise_induced_angina)
heart$heart_disease_present <- as.factor(heart$heart_disease_present)

# Regression models:
heart.glm <- glm(heart_disease_present~slope_of_peak_exercise_st_segment+thal+resting_blood_pressure+chest_pain_type+num_major_vessels+fasting_blood_sugar_gt_120_mg_per_dl+resting_ekg_results+serum_cholesterol_mg_per_dl+oldpeak_eq_st_depression+sex+age+max_heart_rate_achieved+exercise_induced_angina, data = heart, family="binomial")
drop1(heart.glm, response~., test = "Chi", data = heart)
# Drop 1 significant predictors: thal, chest_pain_type, num_major_vessels, oldpeak_eq_st_depression, sex 

heart.step <- step(heart.glm, trace=20)
summary(heart.step)

# features from the stepwise: thalnormal, thalreversible_defect, chest_pain_type2, chest_pain_type3, chest_pain_type4, num_major_vessels1, num_major_vessels2, num_major_vessels3, oldpeak_eq_st_depression,sex1, exercise_induced_angina1 

# Assumption verification
# Assumptions for logistic regression
# 1. The response is binary - yes
# 2. Observations are independent - all patients are independent from each other
# 3. Predictors are independent (no multicollinearity/check VIF)
# 4. No extreme outliers or influential points (check Cook's Distance)
# 5. There is a linear relationship between the predictors and the logit of response - can only be checked for quantitative predictors (which we don't have)
# 6. Sufficiently large sample size - must be great than 30

## Multicollinearity
source("VIF.R")
keep.dat<-vif_func(in_frame=heart[,c('thal','chest_pain_type','num_major_vessels', 'oldpeak_eq_st_depression','exercise_induced_angina')],thresh=5,trace=T)
# All VIFs<5 => no/low multicollinearity

source("SPM_Panel.R")
uva.pairs(heart[,c('thal','chest_pain_type','num_major_vessels', 'oldpeak_eq_st_depression','exercise_induced_angina')])
#largest correlation coefficient is 0.36
## No remaining features have multicollinearity issues

## Outliers and Influential Points
autoplot(heart.step, which = 4)
# No points with a Cook's Distance > 0.5 => no influential points

## Fix below to make heat map of correlation among variables
#df.drop(heart[,c('thal','chest_pain_type','num_major_vessels', 'oldpeak_eq_st_depression','exercise_induced_angina')],axis='columns',inplace=True)
#dummies = pd.get_dummies(df.Sex)
#df = pd.concat([heart,dummies],axis='columns')
#sns.heatmap(df.corr())

source("ROC.R")
heart.pred <- predict(heart.step, type = "response", data = heart$heart_disease_present)
score.table(heart.pred, heart$heart_disease_present, .5)

# the stepwise model made 11 false negatives and 14 false positives out of 180


# Ridge Regression:
# Setting the independent and response variables
x_var <- data.matrix(heart[, c("slope_of_peak_exercise_st_segment","thal","resting_blood_pressure","chest_pain_type","num_major_vessels","fasting_blood_sugar_gt_120_mg_per_dl","resting_ekg_results","serum_cholesterol_mg_per_dl","oldpeak_eq_st_depression","sex","age","max_heart_rate_achieved","exercise_induced_angina")])
y_var <- heart[, "heart_disease_present"]

# Using glmnet function to build the ridge regression
ridge_cv <- cv.glmnet(x_var, y_var, family = "binomial", alpha = 0)

# Build model with optimal lambda value
best_ridge <- glmnet(x_var, y_var, alpha = 0, family = "binomial", lambda = ridge_cv$lambda.min)
coef(best_ridge)

# Spread of data for remaining features

ggplot(as.data.frame(table(heart$thal)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity") + labs(x="Thallium Stress")
ggplot(as.data.frame(table(heart$chest_pain_type)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")+ labs(x="Chest Pain Type")
ggplot(as.data.frame(table(heart$num_major_vessels)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")+ labs(x="# of Vessels Colored")
ggplot(as.data.frame(table(heart$oldpeak_eq_st_depression)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")+ labs(x="ST Depression, Exercise Stress")
ggplot(as.data.frame(table(heart$sex)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")+ labs(x="Gender")
ggplot(as.data.frame(table(heart$exercise_induced_angina)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")+ labs(x="Exercise Induced Chest Pain")

