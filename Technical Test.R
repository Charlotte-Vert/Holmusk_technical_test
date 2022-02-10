
# 0.  Preparation ---------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)
library(eeptools) # used to convert date of birth into age
library(gridExtra) # used to combine several graphs, need to delete
library(corrplot)
library(leaps) # regsubsets() for regression

# new comment 

# Load four csv files into dataframes
df_demographics <- read.csv(file = 'demographics.csv')          
df_clinical <- read.csv(file = 'clinical_data.csv')
df_billid <- read.csv(file = 'bill_id.csv')
df_billamount <- read.csv(file = 'bill_amount.csv')


# 1. Cleaning clinical_data.csv ------------------------------------------

str(df_clinical)
summary(df_clinical)

# Medical_history_2 and medical_history_5 have missing values (N= 233 and 304)
prop.table(table(df_clinical$medical_history_2, exclude = NULL))
prop.table(table(df_clinical$medical_history_5, exclude = NULL))
# Medical_history_2: 7% is missing value, hence treat NA as a new category instead of imputation
# Medical_history_5: 9% is missing value, hence treat NA as a new category instead of imputation

# Medical_history_3 coded in chr
unique(df_clinical$medical_history_3)
# Medical_history_3 coded in 0/1/yes/no, need to convert yes/no into 1/0
df_clinical <- df_clinical %>%
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

# Merge data from demographics.csv
df_master <- merge(df_master, df_demographics, by = 'patient_id')
df_master <- df_master[-c(34,29)] # delete unused columns: date_of_birth & daysSinceLast

df_backup <- df_master # back up of df_master

# Move categorical variables to the front and continuous/discrete variables to the back
df_master <- df_master %>%
  relocate(date_of_discharge, .before = amount) %>%
  relocate(readmitted_status, gender, race, resident_status, 
           .before = medical_history_1) %>%
  
str(df_master)


# 0. Draft ------------------------------------------------

# Histogram for all continuous variables
qplot(amount, data = df_master, bins = 50) # bill amount is right skewed, take log(amount)
qplot(log(amount), data = df_master, bins = 50) # log(amount) approximates normal distribution
qplot(lab_result_1, data = df_master, bins = 30)
qplot(lab_result_2, data = df_master, bins = 30)
qplot(lab_result_3, data = df_master, bins = 30)
qplot(weight, data = df_master, bins = 30)
qplot(height, data = df_master, bins = 50)
qplot(LOS, data = df_master, bins = 30)
qplot(age, data = df_master, bins = 30)

grid.arrange(p1, p2, nrow =1)

# Plots all columns - UNABLE TO SAVE THE PLOTS
ln <- length(names(df_master))
for (i in 4:ln){
if(is.factor(df_master[,i])){
  ggplot(data = df_master, aes(x = df_master[,i], y = amount)) +
    geom_bar(stat = "identity") +
    labs(x = names(df_master)[i])
} else{
  ggplot(data = df_master, aes(x = df_master[,i])) +
    geom_histogram()
}
}


# 5. Plot for every single variable------------------------------------------

# Self-define basic_plot function
basic_plot <- function(i, data.frame){
  df_plot = data.frame
  if(is.factor(df_plot[,i])){ # if categorical variable, plot barplot
    ggplot(data = df_plot, aes(x = df_plot[,i])) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      labs(x = names(df_plot)[i]) +
      ylab("Proportions") +
      ylim(0, 1)
  } else{ # if continuous variable, plot histogram
    ggplot(data = df_plot, aes(x = df_plot[,i])) +
      geom_histogram() +
      labs(x = names(df_plot)[i])
  }
}

# Histogram for continuous variables
basic_plot(4, df_master) # Bill amount is right skewed, take log(amount)
hist_amount <- ggplot(data = df_master, aes(x = log(df_master[,4]))) +
  geom_histogram() +
  labs(x = names(df_master)[4]) # log(amount) approximates normal distribution
basic_plot(27, df_master) # Lab result 1 approximates normal distribution
basic_plot(28, df_master) # Lab result 2 approximates normal distribution
basic_plot(29, df_master) # Lab result 3 approximates normal distribution
basic_plot(30, df_master) # Weight approximates normal distribution
basic_plot(31, df_master) # Height approximates normal distribution
basic_plot(32, df_master) # Length of stay approximates normal distribution
basic_plot(33, df_master) # Age approximates bi-modal normal distribution
basic_plot(34, df_master) # BMI approximates bi-modal normal distribution

# Barplots for categorical variables  
# Barplot for re-admitted status
bar_readmission <- basic_plot(5, df_master) 
# Barplot for gender
bar_gender <- basic_plot(6, df_master) # approximately equal number of female and male
# Barplot for race
bar_race <- basic_plot(7, df_master) # majority is Chinese, followed by Malay
# Barplot for resident_status
bar_resident <- basic_plot(8, df_master) # majority is Citizen, followed by PR
# Barplot for medical history 1
bar_mh1 <- basic_plot(9, df_master) 
# Barplot for medical history 2
bar_mh2 <- basic_plot(10, df_master) 
# Barplot for medical history 3
bar_mh3 <- basic_plot(11, df_master) 
# Barplot for medical history 4
bar_mh4 <- basic_plot(12, df_master) 

# Proportions for categorical variables
prop.table(table(df_master$medical_history_1)) # 17% yes
prop.table(table(df_master$medical_history_2)) # 7% yes, not balanced
prop.table(table(df_master$medical_history_3)) # 14% yes
prop.table(table(df_master$medical_history_4)) # 5% yes, not balanced
prop.table(table(df_master$medical_history_5)) # 6% yes, 9% NA, not balanced
prop.table(table(df_master$medical_history_6)) # 25% yes
prop.table(table(df_master$medical_history_7)) # 25% yes

prop.table(table(df_master$preop_medication_1)) # 50.4% yes
prop.table(table(df_master$preop_medication_2)) # 59.1% yes
prop.table(table(df_master$preop_medication_3)) # 82% yes
prop.table(table(df_master$preop_medication_4)) # 52% yes
prop.table(table(df_master$preop_medication_5)) # 82% yes
prop.table(table(df_master$preop_medication_6)) # 74% yes

prop.table(table(df_master$symptom_1)) # 62% yes
prop.table(table(df_master$symptom_2)) # 66% yes
prop.table(table(df_master$symptom_3)) # 54% yes
prop.table(table(df_master$symptom_4)) # 73% yes
prop.table(table(df_master$symptom_5)) # 53% yes


# 6. Univariate analysis -------------------------------------------------

# Correlation between x variables and y

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


# 7.  Multi-Variable Analysis ---------------------------------------------

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

# use all possible subsets approach
m_allsub <-regsubsets(log_amount ~ ., data = df_regression,
                      nbest=1, nvmax=33)
model_summary <- summary(m_allsub)
result <- cbind(model_summary$which, round(cbind(rsq=model_summary$rsq, 
                                       adjr2=model_summary$adjr2, 
                                       cp=model_summary$cp, 
                                       bic=model_summary$bic, 
                                       rss=model_summary$rss), 3))
plot(model_summary$bic)
plot(model_summary$cp)
plot(model_summary$adjr2)
plot(model_summary$rss)

# pick model #13 as the final model considering a balance of low bic, low cp, low rss and high adjusted r square
result[13,]
# race, resident status, mh1, mh6, sm1, sm2, sm3, sm4, sm5, bmi, age 
mfinal <- lm(log_amount ~ race + resident_status + mh1 + mh6 + sm1 + sm2 + sm3 +
               sm4 + sm5 + bmi + age,
            data = df_regression)


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
plot(model_summary2$bic)
plot(model_summary2$cp)
plot(model_summary2$adjr2)
plot(model_summary2$rss)
#pick model #13
result2[13,]
# race, resident status, mh1, mh6, sm1, sm2, sm3, sm4, sm5, bmi, age 
mfinal2 <- lm(amount ~ race + resident_status + mh1 + mh6 + sm1 + sm2 + sm3 +
               sm4 + sm5 + bmi + age,
             data = no_amount_outliers)


# 0. Backup ---------------------------------------------------------------

qplot(race, amount, data = df_master, geom = "boxplot")
qplot(amount, data = df_master, fill = race, bins = 50)
qplot(amount, data = df_master, facets = race ~ ., bins = 50)
qplot(age, amount, data = df_master, facets = . ~ race, geom = c("point", "smooth"))
qplot(age, amount, data = df_master, facets = . ~ race) + 
  geom_smooth()

hist(df_master$amount, breaks = 100) # bill amount is right skewed, take log(amount)
qplot(log(amount), data = df_master, color = gender, geom = "density") 


chisq.test(df_regression$sm5, df_regression$sm3)
chisq.test(df_regression$sm5, df_regression$sm1)
chisq.test(df_regression$sm5, df_regression$sm2)
chisq.test(df_regression$sm5, df_regression$gender)
chisq.test(df_regression$pom1, df_regression$sm2)
