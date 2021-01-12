##THE DATA FRAME TO USE AFTER YOU RUN THIS CODE IS CALLED final_df. 
##TRY THE ONE WITH THE LOG TRANSFORMED CONTINUOUS VARIABLES AS WELL
  ##THIS ONE IS CALLED log_df

# install.packages("broom")
# install.packages("glmnet")
#install.packages("caret") 
# install.packages("MASS")
# install.packages("FSelector")
# install.packages("matrixStats")
#install.packages("PreProcess")
#install.packages("tidyselect")
#install.packages("tidyverse")

##load libraries 
library(caret) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(glmnet)
library(MASS)
library(FSelector)
library(matrixStats)


##import dataset 
raw_data <- read.csv("heart_data.csv")

df <- data.frame(raw_data)

##convert thal categories into numbers 
df$thal = recode(df$thal, normal = 0, reversible_defect = 1, fixed_defect = 2)
df$thal <- as.factor(df$thal)

##establish categorical variables 
df$chest_pain_type <- as.factor(df$chest_pain_type)
df$fasting_blood_sugar_gt_120_mg_per_dl <- as.factor(df$fasting_blood_sugar_gt_120_mg_per_dl)
df$sex <- as.factor(df$sex)
df$exercise_induced_angina <- as.factor(df$exercise_induced_angina)
df$num_major_vessels <- as.factor(df$num_major_vessels)
df$slope_of_peak_exercise_st_segment <- as.factor(df$slope_of_peak_exercise_st_segment)
df$resting_ekg_results <- as.factor(df$resting_ekg_results)

#renmae
df = df %>% rename(patient_id = `ï..patient_id`)

#separate continuous and categorical variables 
df_continuous <- df[, c('patient_id', 'resting_blood_pressure', 'serum_cholesterol_mg_per_dl', 'oldpeak_eq_st_depression', 'age', 'max_heart_rate_achieved')]
df_categorical <- df[, c('patient_id', 'chest_pain_type', 'fasting_blood_sugar_gt_120_mg_per_dl', 'sex', 
                         'exercise_induced_angina', 'num_major_vessels', 'slope_of_peak_exercise_st_segment', 
                         'resting_ekg_results', 'thal')]

##Chi-Squared test for Independence 
chi_df <- df_categorical[, -c(3:5)]
chi_df['heart_disease_present'] <- df$heart_disease_present
chisq.test(chi_df$chest_pain_type, chi_df$heart_disease_present)
chisq.test(chi_df$thal, chi_df$heart_disease_present)
chisq.test(chi_df$num_major_vessels, chi_df$heart_disease_present)
chisq.test(chi_df$slope_of_peak_exercise_st_segment, chi_df$heart_disease_present)
chisq.test(chi_df$resting_ekg_results, chi_df$heart_disease_present) ##insignifigant 

##Convert thal to binary
df['thal.1'] <- rep(0, nrow(df))
df$thal.1[which(df$thal == 1)] <- 1
df$thal.1 <- as.factor(df$thal.1)
#remove initial thal and resting_ekg variables from the dataframe 
drops <- c('thal', 'resting_ekg_results')
df <- df[ , !(names(df) %in% drops)]
#update df_categorical 
df_categorical <- df_categorical[ , !(names(df_categorical) %in% drops)]
df_categorical['thal.1'] <- df$thal.1

##rescale continuous variables 
scaler <- preProcess(df_continuous[, 2:5], method=c("scale", "center"))
scaled <- predict(scaler, df_continuous[, 2:5])
scaled["patient_id"] <- df$patient_id

##One-hot code categorical variables
df_one_hot <- df_categorical[, c('chest_pain_type', 'num_major_vessels', 'slope_of_peak_exercise_st_segment')]
dmy <- dummyVars("~ .", data = df_one_hot)
trsf <- data.frame(predict(dmy, newdata = df_one_hot))
trsf['patient_id'] <- df_categorical$patient_id
##re-add binary variables 
trsf <- merge(trsf, df_categorical[, c('patient_id', 'fasting_blood_sugar_gt_120_mg_per_dl', 'sex', 
                         'exercise_induced_angina', 'thal.1')], by = 'patient_id')
#create final dataframe (without patient ID)
final_df <- merge(scaled, trsf, by = 'patient_id')
final_df <- subset(final_df, select = -c(patient_id))
final_df['heart_disease_present'] <- df$heart_disease_present
#check isolated chi squared 
##separate those with heart disease into a separate dataframe 
df_disease <- final_df[final_df$heart_disease_present == 1, ]

##Low varience feature elimination 
colnames(numeric_df)
numeric_df <- sapply(final_df, as.numeric)
vars <- colVars(as.matrix(numeric_df))
#eliminate columns with low variance 
drops_2 <- c('chest_pain_type.1', 'num_major_vessels.3', 'slope_of_peak_exercise_st_segment.3')
final_df <- final_df[ , !(names(final_df) %in% drops_2)]

#log transformed continuous 
log_continuous <- log(df_continuous[,-1] + .1)
log_continuous['patient_id'] <- df$patient_id
log_continuous['heart_disease_present'] <- df$heart_disease_present
final_id <- final_df
final_id['patient_id'] <- df$patient_id
log_df <- merge(log_continuous, final_id[, 5:18], by = "patient_id")
log_df <- subset(log_df, select = -c(patient_id))

##MODELING STUFF IGNORE 
log.glm.main <- glm(heart_disease_present~., data = log_df, family = binomial)
##summary(log.glm.main)
##Logistic regression 
#general 
glm.main <- glm(heart_disease_present~., data = final_df, family = binomial)
##summary(glm.main)
step.glm.main <- step(glm.main, trace=F)
##summary(step.glm.main)





