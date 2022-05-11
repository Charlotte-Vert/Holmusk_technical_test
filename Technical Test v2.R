
# 0.  Preparation ---------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)
library(eeptools) # convert date of birth into age
library(gridExtra) # combine several graphs
library(corrplot) # correlation plot
library(leaps) # regsubsets() for regression
library(ggExtra) # plot histogram margin on scatterplot
library(naniar) # use mcar_test() to check hypothesis of MCAR

# Load four csv files into dataframes
df_demographics <- read.csv(file = 'demographics.csv')          
df_clinical <- read.csv(file = 'clinical_data.csv')
df_billid <- read.csv(file = 'bill_id.csv')
df_billamount <- read.csv(file = 'bill_amount.csv')


# 1. Cleaning clinical_data.csv ------------------------------------------

str(df_clinical)
summary(df_clinical)
prop.table(table(df_clinical$medical_history_2, exclude = NULL))
prop.table(table(df_clinical$medical_history_5, exclude = NULL))
# Medical_history_2 and medical_history_5 have missing values (N= 233 (7%) and 304 (9%))

mcar_test(df_clinical) # p value = 0.315, fail to reject hypothesis of MCAR
df_clinical_naomit <- na.omit(df_clinical) # remove NA values and use df_clinical_naomit for sensitivity analysis

# proceed with treating NA as another level

# Medical_history_3 coded in chr
unique(df_clinical$medical_history_3)
# Medical_history_3 coded in 0/1/yes/no, need to convert yes/no into 1/0
df_clinical <- df_clinical %>%
  mutate(medical_history_3 = recode(medical_history_3, 
                                    'Yes'= '1',
                                    'No'= '0'))

df_clinical_naomit <- df_clinical_naomit %>%
  mutate(medical_history_3 = recode(medical_history_3, 
                                    'Yes'= '1',
                                    'No'= '0'))

# Code medical histories, preop_medications and symptoms as factor of 0/1
clinical_col = seq(4,21,1) # convert 4th to 21st columns to factor
for (x in clinical_col) {
  df_clinical[,x] <- factor(df_clinical[,x], exclude = NULL)
}

# Create a new column for length of stay (LOS) and code in numeric format
df_clinical <- df_clinical %>%
  mutate(date_of_admission = as.Date(date_of_admission), 
         date_of_discharge = as.Date(date_of_discharge),
         LOS = date_of_discharge - date_of_admission)
df_clinical$LOS <- as.numeric(df_clinical$LOS)

clinical_naomit_col = seq(4,21,1) # convert 4th to 21st columns to factor
for (x in clinical_naomit_col) {
  df_clinical_naomit[,x] <- factor(df_clinical_naomit[,x], exclude = NULL)
}

# Create a new column for length of stay (LOS) and code in numeric format
df_clinical_naomit <- df_clinical_naomit %>%
  mutate(date_of_admission = as.Date(date_of_admission), 
         date_of_discharge = as.Date(date_of_discharge),
         LOS = date_of_discharge - date_of_admission)
df_clinical_naomit$LOS <- as.numeric(df_clinical_naomit$LOS)

# 2. Cleaning demographics.csv --------------------------------------------

summary(df_demographics)

# Check unique characters in each variable
unique(df_demographics[, 2])
unique(df_demographics[, 3])
unique(df_demographics[, 4])

# Re-organize the characters for gender, race and resident_status
df_demographics <- df_demographics %>%
  mutate(gender = recode(gender, 'f' = 'Female', 'm' = 'Male'),
         race = recode(race, 'chinese' = 'Chinese', 'India' = 'Indian'),
         resident_status = recode(resident_status, 'Singapore citizen' = 'Singaporean'))

# Code gender, race, resident_status as factors
demo_col = seq(2,4,1)
for (y in demo_col) {
  df_demographics[,y] <- factor(df_demographics[,y], exclude = NULL)
}

# Check any incorrect spelling/wrong factor types
sapply(df_demographics, levels)

# Convert date of birth into ages using today's date
df_demographics <- df_demographics %>%
  mutate(date_of_birth = as.Date(date_of_birth))
df_demographics$age <- round(age_calc(df_demographics$date_of_birth, units = 'years'))


# 3. Combine bill_id.csv & bill_amount.csv ------------------------

str(df_billid)
str(df_billamount)
summary(df_billid)
summary(df_billamount)

# Join bill id with bill amount
df_bill <- merge(df_billid, df_billamount, by = 'bill_id')
df_bill <- summarize(group_by(df_bill, patient_id, date_of_admission), 
                       sum(amount),.groups = NULL) %>%
  rename('amount'= 'sum(amount)')

df_bill <- df_bill %>%
  mutate(date_of_admission = as.Date(date_of_admission))


# 4. Generating master data frame -----------------------------------------

# Merge clinical data with bill data
df_clinical <- rename(df_clinical, 'patient_id' = 'id')
df_master <- merge(df_bill, df_clinical, by=c('patient_id', 'date_of_admission'))

df_clinical_naomit <- rename(df_clinical_naomit, 'patient_id' = 'id')
df_master_naomit <- merge(df_bill, df_clinical_naomit, by=c('patient_id', 'date_of_admission'))


# Create new column for re-admission status and bmi
df_master <- df_master %>%
  arrange(patient_id, date_of_admission) %>%
  group_by(patient_id) %>%
  mutate(daysSinceLast = as.integer(date_of_admission - lag(date_of_discharge))) %>%
  ungroup() %>%
  mutate(readmitted_status = if_else(daysSinceLast >=0, 1, 0)) %>%
  mutate(bmi = weight/(height*height)*10000)
df_master$readmitted_status[is.na(df_master$readmitted_status)] <- 0
df_master$readmitted_status <- factor(df_master$readmitted_status)

df_master_naomit <- df_master_naomit %>%
  arrange(patient_id, date_of_admission) %>%
  group_by(patient_id) %>%
  mutate(daysSinceLast = as.integer(date_of_admission - lag(date_of_discharge))) %>%
  ungroup() %>%
  mutate(readmitted_status = if_else(daysSinceLast >=0, 1, 0)) %>%
  mutate(bmi = weight/(height*height)*10000)
df_master_naomit$readmitted_status[is.na(df_master_naomit$readmitted_status)] <- 0
df_master_naomit$readmitted_status <- factor(df_master_naomit$readmitted_status)

# Merge data from demographics.csv
df_master <- merge(df_master, df_demographics, by = 'patient_id')
df_master <- df_master[-c(35,29)] # delete unused columns: date_of_birth & daysSinceLast

df_master_naomit <- merge(df_master_naomit, df_demographics, by = 'patient_id')
df_master_naomit <- df_master_naomit[-c(35,29)] # delete unused columns: date_of_birth & daysSinceLast


# Move categorical variables to the front and continuous/discrete variables to the back
df_master <- df_master %>%
  relocate(date_of_discharge, .before = amount) %>%
  relocate(readmitted_status, gender, race, resident_status, 
           .before = medical_history_1)
  
str(df_master)


df_master_naomit <- df_master_naomit %>%
  relocate(date_of_discharge, .before = amount) %>%
  relocate(readmitted_status, gender, race, resident_status, 
           .before = medical_history_1)
# 5. Hist for continuous variables -------------------------------------------

# 5.1: bill amount
ggplot(df_master, aes(x = amount)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) + 
  geom_density(color = "red") +
  labs(title = "Histogram of amount")

ggplot(df_master, aes(x = log(amount))) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) +
  geom_density(color = "red") +
  labs(x = "log(amount)", 
       title = "Histogram of log(amount)")

# 5.2: lab results
hist_lb1 <- ggplot(df_master, aes(x = lab_result_1)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) +
  geom_density(color = "red") +
  labs(title = "Histogram of lab result 1") +
  ylim(0.00, 0.30)
  
hist_lb2 <- ggplot(df_master, aes(x = lab_result_2)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) +
  geom_density(color = "red") +
  labs(title = "Histogram of lab result 2") +
  ylim(0.00, 0.30)
  
hist_lb3 <-ggplot(df_master, aes(x = lab_result_3)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) +
  geom_density(color = "red") +
  labs(title = "Histogram of lab result 3") +
  ylim(0.00, 0.03)

grid.arrange(hist_lb1, hist_lb2, hist_lb3, nrow =1)

# 5.3: weight, height and bmi
hist_weight <-ggplot(df_master, aes(x = weight)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white", bins = 50) +
  geom_density(color = "red") +
  labs(title = "Histogram of weight")

hist_weight_gender <-ggplot(df_master, aes(x = weight, color = gender)) +
  geom_histogram(aes(y = ..density..), fill = "white", bins = 50, position = "identity") +
  geom_density() +
  labs(title = "Histogram of weight by gender") +
  theme(legend.position = "bottom")

hist_height <-ggplot(df_master, aes(x = height)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density(color = "red") +
  labs(title = "Histogram of height")

hist_height_gender <-ggplot(df_master, aes(x = height, color = gender)) +
  geom_histogram(aes(y = ..density..), fill = "white", position = "identity") +
  geom_density() +
  labs(title = "Histogram of height by gender") +
  theme(legend.position = "bottom")

hist_bmi <-ggplot(df_master, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density(color = "red") +
  labs(title = "Histogram of bmi")

hist_bmi_gender <-ggplot(df_master, aes(x = bmi, color = gender)) +
  geom_histogram(aes(y = ..density..), fill = "white", position = "identity") +
  geom_density() +
  labs(title = "Histogram of bmi by gender") +
  theme(legend.position = "bottom")

grid.arrange(hist_weight, hist_height, hist_bmi, 
             hist_weight_gender, hist_height_gender, hist_bmi_gender,
             nrow =2)
# 5.4: age
hist_age <- ggplot(df_master, aes(x = age)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density(color = "red") +
  labs(title = "Histogram of age")

# 5.5: Length of Stay
hist_LOS <- ggplot(df_master, aes(x = LOS)) +
  geom_histogram(aes(y = ..density..), color = 1, fill = "white") +
  geom_density(color = "red") +
  labs(title = "Histogram of length of stay")


# 6. Barplot for categorical variables -----------------------------------

# 6.1: demographic data
bar_readmit <- ggplot(data = df_master, aes(x = readmitted_status, 
                                            color = readmitted_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white", width = 0.5) +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_gender <- ggplot(data = df_master, aes(x = gender, color = gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white", width = 0.5) +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_race <- ggplot(data = df_master, aes(x = race, color = race)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_resident <- ggplot(data = df_master, aes(x = resident_status, color = resident_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

grid.arrange(bar_readmit, bar_race, bar_gender,  bar_resident, nrow = 2)

# 6.2: medical history
bar_mh1 <- ggplot(data = df_master, aes(x = medical_history_1, color = medical_history_1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh2 <- ggplot(data = df_master, aes(x = medical_history_2, color = medical_history_2)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh3 <- ggplot(data = df_master, aes(x = medical_history_3, color = medical_history_3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh4 <- ggplot(data = df_master, aes(x = medical_history_4, color = medical_history_4)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh5 <- ggplot(data = df_master, aes(x = medical_history_5, color = medical_history_5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh6 <- ggplot(data = df_master, aes(x = medical_history_6, color = medical_history_6)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_mh7 <- ggplot(data = df_master, aes(x = medical_history_7, color = medical_history_7)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

grid.arrange(bar_mh1, bar_mh2, bar_mh3, bar_mh4, bar_mh5, bar_mh6, bar_mh7, nrow = 3)

# 6.3: preop medication
bar_pom1 <- ggplot(data = df_master, aes(x = preop_medication_1, color = preop_medication_1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_pom2 <- ggplot(data = df_master, aes(x = preop_medication_2, color = preop_medication_2)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_pom3 <- ggplot(data = df_master, aes(x = preop_medication_3, color = preop_medication_3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_pom4 <- ggplot(data = df_master, aes(x = preop_medication_4, color = preop_medication_4)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_pom5 <- ggplot(data = df_master, aes(x = preop_medication_5, color = preop_medication_5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_pom6 <- ggplot(data = df_master, aes(x = preop_medication_6, color = preop_medication_6)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

grid.arrange(bar_pom1, bar_pom2, bar_pom3, bar_pom4, bar_pom5, bar_pom6, nrow = 2)

# 6.4: symptoms
bar_sm1 <- ggplot(data = df_master, aes(x = symptom_1, color = symptom_1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_sm2 <- ggplot(data = df_master, aes(x = symptom_2, color = symptom_2)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_sm3 <- ggplot(data = df_master, aes(x = symptom_3, color = symptom_3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_sm4 <- ggplot(data = df_master, aes(x = symptom_4, color = symptom_4)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

bar_sm5 <- ggplot(data = df_master, aes(x = symptom_5, color = symptom_5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "white") +
  ylab("Proportions") +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "bottom")

grid.arrange(bar_sm1, bar_sm2, bar_sm3, bar_sm4, bar_sm5, nrow = 2)


# 7. Univariate analysis -------------------------------------------------

# Correlation Matrix for Continuous variables
df_continuous <- df_master[,c(4, 27:34)] %>%
  mutate(log_amount = log(amount))
df_continuous <- df_continuous[,2:10]
cont <- cor(df_continuous)
corrplot(cont, method = 'number') 
# age and log_amount has moderate positive association
# bmi is highly associated with height and weight

# Association between categorical variables and log(amount)
df_categorical <- df_master[, c(4, 5:26)] %>%
  mutate(amount = log(amount))
df_categorical <- rename(df_categorical, 'log_amount' = 'amount')
aov_list = list()
  for (i in 2:23){
    # ANOVA for 22 variables, from re-admitted status to symptom_5
    aov_amount_var <- aov(df_categorical$log_amount ~ df_categorical[,i], df_categorical)
    
    # save as aov_resilience, to be exported as csv
    aov_list[i-1] <- summary(aov_amount_var)
  }

# Using alpha level of 0.05, check which categorical variables are significantly associated with log(amaount)
names(df_categorical)[c(3:6, 11, 12, 14, 18:23)]
# gender, race, resident status, mh1, mh6, mh7, pom2, pom6, symptom 1-5


# 8.  Multi-Variable Analysis ---------------------------------------------

# recode df_master into df_regression for easy editing in modelling
df_regression <- df_master %>%
  mutate(amount = log(amount))
df_regression <- df_regression[, -c(1,2,3,30,31)] # remove patient id, date of admission, discharge, weight and height
df_regression <- rename(df_regression, 
                        'log_amount' = 'amount',
                        'mh1' = 'medical_history_1',
                        'mh2' = 'medical_history_2',
                        'mh3' = 'medical_history_3',
                        'mh4' = 'medical_history_4',
                        'mh5' = 'medical_history_5',
                        'mh6' = 'medical_history_6',
                        'mh7' = 'medical_history_7',
                        'pom1' = 'preop_medication_1',
                        'pom2' = 'preop_medication_2',
                        'pom3' = 'preop_medication_3',
                        'pom4' = 'preop_medication_4',
                        'pom5' = 'preop_medication_5',
                        'pom6' = 'preop_medication_6',
                        'sm1' = 'symptom_1',
                        'sm2' = 'symptom_2',
                        'sm3' = 'symptom_3',
                        'sm4' = 'symptom_4',
                        'sm5' = 'symptom_5')

df_regression_naomit <- df_master_naomit %>%
  mutate(amount = log(amount))
df_regression_naomit <- df_regression_naomit[, -c(1,2,3,30,31)] # remove patient id, date of admission, discharge, weight and height
df_regression_naomit <- rename(df_regression_naomit, 
                        'log_amount' = 'amount',
                        'mh1' = 'medical_history_1',
                        'mh2' = 'medical_history_2',
                        'mh3' = 'medical_history_3',
                        'mh4' = 'medical_history_4',
                        'mh5' = 'medical_history_5',
                        'mh6' = 'medical_history_6',
                        'mh7' = 'medical_history_7',
                        'pom1' = 'preop_medication_1',
                        'pom2' = 'preop_medication_2',
                        'pom3' = 'preop_medication_3',
                        'pom4' = 'preop_medication_4',
                        'pom5' = 'preop_medication_5',
                        'pom6' = 'preop_medication_6',
                        'sm1' = 'symptom_1',
                        'sm2' = 'symptom_2',
                        'sm3' = 'symptom_3',
                        'sm4' = 'symptom_4',
                        'sm5' = 'symptom_5')


# use all possible subsets approach
m_allsub <-regsubsets(log_amount ~ ., data = df_regression,
                      nbest=1, nvmax=33)
model_summary <- summary(m_allsub)
result <- cbind(model_summary$which, round(cbind(rsq=model_summary$rsq, 
                                       adjr2=model_summary$adjr2, 
                                       cp=model_summary$cp, 
                                       bic=model_summary$bic, 
                                       rss=model_summary$rss), 3))

lm(log_amount ~ sm1+sm2+sm3+sm4+sm5+mh1+mh6+age+bmi+gender+race+resident_status, 
   data = df_regression)

m_allsub_naomit <-regsubsets(log_amount ~ ., data = df_regression_naomit,
                      nbest=1, nvmax=33)
model_summary_naomit <- summary(m_allsub_naomit)
result_naomit <- cbind(model_summary_naomit$which, round(cbind(rsq=model_summary_naomit$rsq, 
                                                 adjr2=model_summary_naomit$adjr2, 
                                                 cp=model_summary_naomit$cp, 
                                                 bic=model_summary_naomit$bic, 
                                                 rss=model_summary_naomit$rss), 3))

# Plot curves for BIC, Cp, Adjusted R2 and RSS
par(mfrow = c(2,2))
plot(model_summary$bic)
plot(model_summary$cp)
plot(model_summary$adjr2)
plot(model_summary$rss)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(model_summary_naomit$bic)
plot(model_summary_naomit$cp)
plot(model_summary_naomit$adjr2)
plot(model_summary_naomit$rss)
par(mfrow = c(1,1))
# No difference in sensitivity analysis

# pick model #14 as the final model considering a balance of low bic, low cp, low rss and high adjusted r square
result[14,]
# gender, race, resident status, mh1, mh6, sm1, sm2, sm3, sm4, sm5, bmi, age 
model1 <- lm(log_amount ~ gender + race + resident_status + mh1 + mh6 + sm1 + sm2 + sm3 +
               sm4 + sm5 + bmi + age,
            data = df_regression)
# Diagnostic plot for Model 1
plot(model1)

# Sensitivity analysis: remove outliers in bill amount
Q1 <- quantile(df_master$amount, .25)
Q3 <- quantile(df_master$amount, .75)
IQR <- IQR(df_master$amount)
no_amount_outliers <- subset(df_master, df_master$amount> (Q1 - 1.5*IQR) & df_master$amount< (Q3 + 1.5*IQR))
no_amount_outliers <- no_amount_outliers[, -c(1,2,3,30,31)] # remove patient id, date of admission, discharge, weight and height
no_amount_outliers <- rename(no_amount_outliers, 
                        'mh1' = 'medical_history_1',
                        'mh2' = 'medical_history_2',
                        'mh3' = 'medical_history_3',
                        'mh4' = 'medical_history_4',
                        'mh5' = 'medical_history_5',
                        'mh6' = 'medical_history_6',
                        'mh7' = 'medical_history_7',
                        'pom1' = 'preop_medication_1',
                        'pom2' = 'preop_medication_2',
                        'pom3' = 'preop_medication_3',
                        'pom4' = 'preop_medication_4',
                        'pom5' = 'preop_medication_5',
                        'pom6' = 'preop_medication_6',
                        'sm1' = 'symptom_1',
                        'sm2' = 'symptom_2',
                        'sm3' = 'symptom_3',
                        'sm4' = 'symptom_4',
                        'sm5' = 'symptom_5')
m_allsub2 <-regsubsets(amount ~ ., data = no_amount_outliers, method = 'backward',
                      nbest=1, nvmax=33)
model_summary2 <- summary(m_allsub2)
result2 <- cbind(model_summary2$which, round(cbind(rsq=model_summary2$rsq, 
                                                 adjr2=model_summary2$adjr2, 
                                                 cp=model_summary2$cp, 
                                                 bic=model_summary2$bic, 
                                                 rss=model_summary2$rss), 3))
par(mfrow = c(2,2))
plot(model_summary2$bic)
plot(model_summary2$cp)
plot(model_summary2$adjr2)
plot(model_summary2$rss)
par(mfrow = c(1,1))

#pick model #14
result2[14,]
# gender, race, resident status, mh1, mh6, sm1, sm2, sm3, sm4, sm5, bmi, age 
mfinal2 <- lm(amount ~ race + resident_status + mh1 + mh6 + sm1 + sm2 + sm3 +
               sm4 + sm5 + bmi + age,
             data = no_amount_outliers)
# Removing outliers in bill amount makes no difference in variable selection

# 9.  Amount versus significant variables ---------------------------------

# scatterplot for bmi versus amount, grouped by resident status
amount_bmi <- ggplot(df_regression, aes(x = bmi, y = log_amount, color = resident_status))+
  geom_point(alpha = 0.7, size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_smooth(method = lm, se = TRUE)
amount_bmi <- ggMarginal(amount_bmi, type = "histogram" , fill = "steelblue")

amount_age <- ggplot(df_regression, aes(x = age, y = log_amount, color = resident_status))+
  geom_point(alpha = 0.7, size = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_smooth(method = lm, se = TRUE) +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())
amount_age <- ggMarginal(amount_age, type = "histogram" , fill = "steelblue")

grid.arrange(amount_bmi, amount_age, nrow = 1)


# boxplot for race versus amount, grouped by resident status
ggplot(df_regression, aes(x = race, y = log_amount, fill = resident_status))+
  geom_boxplot() +
  facet_grid(gender ~ .) +
  theme_minimal() +
  theme(legend.position = "bottom")

# boxplot for medical histories and symptoms versus amount
amount_mh1 <- ggplot(df_regression, aes(x = mh1, y = log_amount, fill = mh1))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu")

amount_mh6 <- ggplot(df_regression, aes(x = mh6, y = log_amount, fill = mh6))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

amount_sm1 <- ggplot(df_regression, aes(x = sm1, y = log_amount, fill = sm1))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

amount_sm2 <- ggplot(df_regression, aes(x = sm2, y = log_amount, fill = sm2))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

amount_sm3 <- ggplot(df_regression, aes(x = sm3, y = log_amount, fill = sm3))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

amount_sm4 <- ggplot(df_regression, aes(x = sm4, y = log_amount, fill = sm4))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

amount_sm5 <- ggplot(df_regression, aes(x = sm5, y = log_amount, fill = sm5))+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="BuPu") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank())

grid.arrange(amount_mh1, amount_mh6, amount_sm1, amount_sm2, amount_sm3, 
             amount_sm4, amount_sm5, nrow = 1)


