#Install packages
install.packages('stringr')
install.packages('tidyverse')
install.packages("lmtest")
install.packages("car")
library(tidyverse)
library(lmtest)
library(stringr)
library(dplyr)
library(mice)
library(car)
library(readr)
library(faraway)
library(performance)
#-------------------------------------------------------------------------------------------------------------
#Loading data
employee = read_csv("eda_data.csv")
View(employee)
str(employee)
#-------------------------------------------------------------------------------------------------------------
#Data Cleaning
employee <- employee %>%
  dplyr::select('Rating', 'Size', 'Industry', 'Revenue', 'avg_salary', 'age', 'python_yn', 'R_yn', 'spark', 'aws','excel', 'job_simp')
summary(is.na(employee))
str(employee)
summary(employee)
#----------------------------------------------------------------------------------------------------------
#Data Cleaning - Rating
# Create a boxplot
boxplot(employee$Rating, ylab = "Rating Value", horizontal = TRUE)
# Check the length of the Rating variable
length(employee$Rating)
# Calculate quartiles and IQR
quartiles <- quantile(employee$Rating, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(employee$Rating, na.rm = TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
# Subset the data to remove outliers
data_no_outlier <- subset(employee, Rating > Lower & Rating < Upper)
# Check the length of the data without outliers
length(data_no_outlier$Rating)
# Round the Rating variable
data_no_outlier$Rating <- round(data_no_outlier$Rating)
# Mutate the Rating variable to categorical levels
data_no_outlier <- data_no_outlier %>%
  mutate(Rating = cut(Rating, breaks = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), labels = c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied")))
# Display summary statistics
summary(data_no_outlier)
#------------------------------------------------------------------------------------------------------------
#Data Cleaning - Age
boxplot(employee$age, horizontal= T)
min_age = sum(employee$age < 18)
min_age
max_age = sum(employee$age > 70)
max_age

## There are 290 instances of min_age<18 & 156 instances of max_age>70 which are outliers. 
getWorkingAge = employee$age[employee$age <66 & employee$age > 23 & !is.na(employee$age)]
quantile(getWorkingAge)

## To replace <18 outliers with IQR Q1 (30) & replace >70 outliers with IQR Q3(52)
employee$age[employee$age<18] = 30
employee$age[employee$age>70] = 52
hist(employee$age)
#--------------------------------------------------------------------------------------------------------
#Data Cleaning - Average Salary
# average salary*1000
employee$avg_salary = employee$avg_salary*1000
#------------------------------------------------------------------------------------------------------------
#Data Cleaning - Job Title Simplified
# Replace NA with other tech jobs 
employee$job_simp = str_replace_all(employee$job_simp, "\\bna\\b", "Other tech jobs")
#---------------------------------------------------------------------------------------------------------
#Data Cleaning - Number of Employees categorized in company size
#1 to 200 employees - “Small”
#201 to 1000 employees - ‘Medium’
#1001 and above - ‘Large’
employee$Size <- ifelse(employee$Size %in% c("1 to 50 employees", "51 to 200 employees"), "small", 
                        ifelse(employee$Size %in% c("201 to 500 employees", "501 to 1000 employees"), "medium", 
                               ifelse(employee$Size %in% c("1001 to 5000 employees", "5001 to 10000 employees", "10000+ employees"), "large", NA)))
#----------------------------------------------------------------------------------------------------------------
#Data Cleaning - Revenue categorized by business size
#Less than $1 million (USD) - ‘micro-bus’
#$1 million to 10 million (USD) - ‘small-bus’
#$10 million to 50 million (USD) - ‘medium-bus’
#$50 million and above (USD) - ‘large-bus’
employee$Revenue<- ifelse(employee$Revenue == "Less than $1 million (USD)", "micro-bus", 
                          ifelse(employee$Revenue %in% c("$1 to $5 million (USD)", "$5 to $10 million (USD)"), "small-bus", 
                                 ifelse(employee$Revenue %in% c("$10 to $25 million (USD)","$25 to $50 million (USD)", "$50 to $100 million (USD)"), "medium-bus", 
                                        ifelse(employee$Revenue %in% c("$100 to $500 million (USD)", "$500 million to $1 billion (USD)", "$1 to $2 billion (USD)", "$2 to $5 billion (USD)", "$5 to $10 billion (USD)", "$10+ billion (USD)"), "large-bus", NA))))
#-----------------------------------------------------------------------------------------------------------
#Data Cleaning - Impute Missing Value Revenue(195) and Size(2) by MICE. polyreg method was chosen because there are more than 2 factor variables.
employee <- employee %>% 
  mutate(
    Size = as.factor(Size),
    Revenue = as.factor(Revenue)
  )
init = mice(employee, maxit = 0)

meth = init$method
predM = init$predictorMatrix
meth[c("Size")] = "polyreg"
meth[c("Revenue")] = "polyreg"

imp_rev <- mice(employee, m=5, method= meth, predictorMatrix = predM, maxit = 10, seed = 20)

employee <- complete(imp_rev)
#------------------------------------------------------------------------------------------------------------
#Data Cleaning - Industry
employee <- subset(employee, Industry != "-1")
#-----------------------------------------------------------------------------------------------------------
#Save data after clean
# Create clean data and read
write_csv(employee, "employee_clean.csv")
employee_clean <- read_csv("employee_clean.csv")
View(employee_clean)

employee_clean <- employee_clean %>%
  mutate(Rating = cut(Rating, breaks = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), labels = c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied")))
#--------------------------------------------------------------------------------------------------------------
# Exploratory Data Analysis
# compare average salary distribution for each company size
ggplot(data = employee_clean, aes(x = Size, y = avg_salary))+labs(title="CompanySize vs AverageSalary") + geom_boxplot()
#Overall, the 3 batches of data look as if they were generally distributed in a similar way.The interquartile ranges are reasonably similar (as shown by the lengths of the boxes).The median average salary of small-sized company is greater than large-sized company and medium-sized company.There are outliers observed across large and medium-sized companies which indicate that these data values are far away from other data values which needs further evaluation.

#compare average salary distribution for rating (job satisfaction)
ggplot(data = employee_clean, aes(x = Rating, y = avg_salary))+labs(title="Rating vs AverageSalary") + geom_boxplot()
#Because Dissatisfied and Very Dissatisfied both have the value 0, they will not appear in the box plot. Overall, individuals who rated very satisfied scored higher median average salary compared to individuals who rated neutral and satisfied.

#compare salary distribution for each tech job
ggplot(data = employee_clean, aes(x = job_simp, y = avg_salary)) +labs(title="Tech Job vs AverageSalary") + geom_boxplot()
#Overall, director position has higher median average salary compared to mle, data scientist, data engineer, other tech jobs, manager and analyst. As shown in analyst, data engineer, manager and mle column, the box plots are comparatively short which indicates that individuals that are in these mentioned positions have around the same average salary with each other. In addition, the 4 sections of the box plot for manager and mle are relatively uneven in size. This indicates that individuals in that particular position respectively have similar average salary at certain parts of the scale, but in other parts of the scale, individuals in that position are more variable in their average salary. The short upper whisker in mle boxplot indicates that their average salary are varied among the least positive quartile group, and very similar for the most positive quartile group. There are outliers observed across all the positions which indicate that these data values are far away from other data values which needs further evaluation.
#------------------------------------------------------------------------------------------------------------

#Multiple Linear Regression
#Feature Selection and Normalization
sal_df<-
  dplyr::select(employee_clean,-c("Industry", "Rating"))
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}
sal_df[sapply(sal_df, is.numeric)] <- lapply(sal_df[sapply(sal_df, is.numeric)],norm_minmax)
sal_df[sapply(sal_df, is.character)] <- lapply(sal_df[sapply(sal_df, is.character)],as.factor)
str(sal_df)
#-----------------------------------------------------------------------------------------------------------
# Create Multiple Regression Model
slmModel=lm(avg_salary~.,data=sal_df)
summary(slmModel)
#-----------------------------------------------------------------------------------------------------------
# Check collinearity
vif(slmModel)
#------------------------------------------------------------------------------------------------------------
#Interaction model
#age*job_simp
is.factor(sal_df$job_simp)
sal_df$job_simp = as.factor(sal_df$job_simp)

is.factor(sal_df$job_simp)
levels(sal_df$job_simp)

slmModel1_interaction = lm(avg_salary ~ . + age:job_simp, data=sal_df)
summary(slmModel1_interaction)

#age*python_yn 
is.factor(sal_df$python_yn)
sal_df$python <- NA
sal_df$python[sal_df$python_yn == 1] = "Yes"
sal_df$python[sal_df$python_yn == 0] = "No"

sal_df$python = as.factor(sal_df$python)
is.factor(sal_df$python)
levels(sal_df$python)

slmModel2_interaction = lm(avg_salary ~ . + age:python, data=sal_df)
summary(slmModel2_interaction)
#----------------------------------------------------------------------------------------------------------
#Addition model
slmModel_addition = lm(avg_salary~., data=sal_df)
summary(slmModel_addition)
#----------------------------------------------------------------------------------------------------------
#Anova
anova(slmModel_addition, slmModel1_interaction)
anova(slmModel_addition, slmModel2_interaction)

#Model with interaction is better than Model without interaction
#-------------------------------------------------------------------------------------------------------------
##Check assumption with Interaction model
##Check assumption
par(mfrow = c(2, 2))
plot(slmModel1_interaction)

#Linearity: the response can be written as a linear combination of the predictors. (With noise about this true linear relationship.)
plot(fitted(slmModel1_interaction), resid(slmModel1_interaction), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",
     main = "salary: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

#Homoscedasticity
bptest(slmModel1_interaction)

#Independence of Errors
durbinWatsonTest(slmModel1_interaction)

#Normality: the distribution of the errors should follow a normal distribution.
qqnorm(resid(slmModel1_interaction), col = "darkgrey")
qqline(resid(slmModel1_interaction), col = "dodgerblue", lwd = 2)
shapiro.test(resid(slmModel1_interaction))

#Multicollinearity
vif(slmModel1_interaction)

##The diagnostic plots show residuals in four different ways:

#Residuals vs Fitted: is used to check the assumptions of linearity. If the residuals are spread equally around a horizontal line without distinct patterns (red line is approximately horizontal at zero), that is a good indication of having a linear relationship.

#Normal Q-Q: is used to check the normality of residuals assumption. If the majority of the residuals follow the straight dashed line, then the assumption is fulfilled.

#Scale-Location: is used to check the homoscedasticity of residuals (equal variance of residuals). If the residuals are spread randomly and the see a horizontal line with equally (randomly) spread points, then the assumption is fulfilled.

#Residuals vs Leverage: is used to identify any influential value in our data set. Influential values are extreme values that might influence the regression results when included or excluded from the analysis. Look for cases outside of a dashed line.
#------------------------------------------------------------------------------------------------------------
#Transformation
slmModel1_interaction_log = lm(log(avg_salary+1) ~ . + age:job_simp, data = sal_df)


par(mfrow = c(1, 2))
plot(fitted(slmModel1_interaction_log), resid(slmModel1_interaction_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(slmModel1_interaction_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(slmModel1_interaction_log), col = "dodgerblue", lwd = 2)

sqrt(mean(resid(slmModel1_interaction) ^ 2))
sqrt(mean(resid(slmModel1_interaction_log) ^ 2))
sqrt(mean((sal_df$avg_salary - fitted(slmModel1_interaction)) ^ 2))
sqrt(mean((sal_df$avg_salary - exp(fitted(slmModel1_interaction_log))) ^ 2))
#-------------------------------------------------------------------------------------------------------------
# Install and load the MASS package
install.packages("MASS")
library(MASS)

# Assuming 'response' is your dependent variable and 'predictors' is a data frame of independent variables
slmModel <- lm(avg_salary ~ ., data = sal_df)


#backward using BIC
n = length(resid(slmModel))
slmModel_back_bic = step(slmModel, direction = "backward" , k = log(n))
#the procedure is the same, at each step, we look to improve BIC
#which R still labels AIC
coef(slmModel_back_bic)

#forward search using BIC
slmModel_start = lm(avg_salary ~ 1, data = sal_df)
slmModel_forw_bic = step(
  slmModel_start,
  scope = avg_salary ~ Size + Revenue + age + python_yn + R_yn + aws + excel + job_simp + spark,
  direction = "forward", k = log(n))

#stepwise search using BIC
slmModel_both_bic = step(
  slmModel_start,
  scope = avg_salary ~ Size + Revenue + age + python_yn + R_yn + aws + excel + job_simp + spark,
  direction = "both", k = log(n)
)
#-------------------------------------------------------------------------------------------------------------
## Check lại bằng AIC
#backward using AIC

slmModel = lm(avg_salary ~ ., data = sal_df)
slmModel_back_aic = step(slmModel, direction = "backward")
#results: 
#at the 1st step, the model has AIC = -2941.81
#each row: shows the effect of deleting each of the current predictors
#"-" signs at the beginning of each row: consider removing a predictor
#<none> keeping the current model
#the process continues until we reach the model avg_salary ~ Size + age + python_yn + aws + job_simp
#as removing any additional variable will not improve the AIC
#this model is stored in slmModel_back_aic
coef(slmModel_back_aic)

#forward search using AIC
slmModel_start = lm(avg_salary ~ 1, data = sal_df)
slmModel_forw_aic = step(
  slmModel_start,
  scope = avg_salary ~ Size + Revenue + age + python_yn + R_yn + aws + excel + job_simp + spark,
  direction = "forward")

#stepwise search using AIC
#Start with avg_salary ~ 1 and search up to full model
slmModel_both_aic = step(
  slmModel_start,
  scope = avg_salary ~ Size + Revenue + age + python_yn + R_yn + aws + excel + job_simp + spark,
  direction = "both"
)


summary(slmModel)$adj.r.squared
summary(slmModel_back_aic)$adj.r.squared
summary(slmModel_back_bic)$adj.r.squared

