## Same codes form Kaggle analysis but import raw data from Github folder and more functions not supported on Kaggle

#Install libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gt)

#Import raw data
heart.data.raw <- read.csv("https://raw.githubusercontent.com/nluu1/Bioinformatics-projects/3c7137f962364fd7d5a9ed1e016a64fefc645b86/in-R/data/heart.csv")
View(heart.data.raw)

#create a copy of raw data to manipulate
data <- heart.data.raw
View(data)

#Take a look at the top and bottom 10 rows
View(head(data,10))
View(tail(data,10))

#Take a look at desired rows/columns: data[rows,column]
data[1] #all values cp column
data[1,] #all values in the first row

#Peek at data
summary(data)
str(data)
dim(data)
typeof(data)
ls(data)

#Take a look at desired rows/columns: data[rows,column]
head(data[3],10) #top 10 values cp column
data[3,] #all values in the first row
#Take a look at top 10 values for 3 seperate columns
head(data[c(1:2,14)],10)


## Drop the NULL before factoring and leveling!!
# Drop the null values
missing_ca_indeces <- which(data$ca %in% 4)
missing_thal_indeces <-which(data$thal %in% 0)
missing_values_indeces <- c(missing_ca_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]

# Transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)

# Give a better name to the factor values for the graphs
levels(data$sex) <- c("Female", "Male")
levels(data$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$fbs) <- c("No", "Yes")
levels(data$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$exang) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$target) <- c("Yes", "No")

#Check for dplicate rows
duplicated(data)

#Make a summary of statistic table
cont_data <- select(data, age, trestbps, chol, thalach, oldpeak)

cont_data <- lapply(cont_data,as.numeric)
sum_table <- data.frame(
    Mean = sapply(cont_data,mean),
    Median = sapply(cont_data,median),
    Variance = sapply(cont_data,var),
    SD = sapply(cont_data,sd)
)
round(sum_table,digits=3)


### Removing ouliers  ###NEED UPDATE ON LOOP
boxplot(data$chol)

#Create a copy of the data 
data_sub <- data

#Create a function to calculate the data that are out of IQR range
remove_outlier <- function(x) {
    Q1 <- quantile(x, probs=.25)
    Q3 <- quantile(x, probs=.75)
    IQR <- IQR(x)
    upper_limit = Q3 + (IQR*1.5)
    lower_limit = Q1 - (IQR*1.5)
    x > lower_limit & x < upper_limit
}

#remove the outliers that are out of range
data_sub <- subset(data_sub,remove_outlier(data_sub$chol))
boxplot(data_sub$chol)

#original data boxplot
boxplot(data)

data_sub <- subset(data_sub,remove_outlier(data_sub$thalach))
data_sub <- subset(data_sub,remove_outlier(data_sub$trestbps))

#boxplot without outliers for age, chol, thalach
boxplot(data_sub)

# Create ggplot with outliers
ggplot(data, aes(y=chol)) +
geom_boxplot()

#ggplot without outliers
ggplot(data, aes(y=chol)) +                             
  geom_boxplot(outlier.shape = NA)

##############################
#Histogram 

#Age
ggplot(data, aes(x=age))+
    geom_histogram(color = "black", fill = "green", bins = 12)+
    labs(x="Age", title = "Age Distribution")

#Trestbps
ggplot(data, aes(x=trestbps))+
    geom_histogram(color = "black", fill = "green", bins = 12)+
    labs(x="Blood Pressure (mmHg)", title = "Blood Pressure")

#Chol
ggplot(data, aes(x=chol))+
    geom_histogram(color = "black", fill = "green", bins = 12)+
    labs(x="Cholesterol level (mg/dL)", title = "Cholesterol")

#Thalach
ggplot(data, aes(x=thalach))+
    geom_histogram(color = "black", fill = "green", bins = 12)+
    labs(x="Max Heart rate (bpm)", title = "Thalach - Maximum heart rate")

#OLdpeak
ggplot(data, aes(x=oldpeak))+
    geom_histogram(color = "black", fill = "green", bins = 12)+
    labs(x="Peak", title = "Old-Peak")

#Target
ggplot(data, aes(target, fill=target)) + 
    geom_bar() +
    labs(x="Disease", y="Number of patients",
         title = "Healthy or Sick Individuals") +
    scale_fill_discrete(name = "Have disease?")

#Sex
ggplot(data, aes(sex, fill=sex)) + 
    geom_bar() +
    labs(x="Sex", y="Number of patients",
         title = "Gender (Male or Female)") +
    scale_fill_discrete(name = "Gender")

#CP
ggplot(data, aes(cp, fill=cp)) + 
    geom_bar() +
    labs(x="Chest Pain Type", y="Number of patients",
         title = "Chest Pain Type") +
    scale_fill_discrete(name = "Type")

#FBS
ggplot(data, aes(fbs, fill=fbs)) + 
    geom_bar() +
    labs(x="Fasting Blood Sugar", y="Number of patients",
         title = "Plasma Glucose Fasting") +
    scale_fill_discrete(name = "Fasting")

#Gender vs Target
ggplot(data, aes(sex, fill=target)) + 
    geom_bar(position='dodge') +
    labs(fill="Disease", x="Gender", y="Number of patients",
         title = "Gender (F/M) vs. Class (Healthy/Sick)")

#Age vs Target
ggplot(data, aes(age, fill=target)) + 
    geom_bar() +
    labs(fill="Disease", x="Age", y="Number of patients",
         title = "Age vs. Class (Healthy/Sick)")

#CP vs Target
ggplot(data, aes(cp, fill=target)) + 
    geom_bar(position='dodge') +
    labs(fill="Disease", x="Type", y="Number of patients",
         title = "Chest pain type vs. Class (Healthy/Sick)")

#Slope vs target
ggplot(data, aes(slope, fill=target)) + 
    geom_bar() +
    labs(fill="Disease", x="Slope", y="Number of patients",
         title = "Slope of ST Segment vs. Class (Healthy/Sick)")

#Fasting blood sugar vs Target
ggplot(data, aes(fbs,fill=target)) + 
    geom_bar(position='dodge') +
    labs(fill="Disease", x="Fasting blood sugar", y="Number of patients",
         title = "Fasting Blood Sugar vs. Class (Healthy/Sick)")

#Slope vs Oldpeak vs Target
ggplot(data, aes(x=slope, y=oldpeak, fill=target)) +
    geom_boxplot() +
    labs(fill="Disease", x="Slope of ST segment", y="Depression of ST segment")

#Multiple variables plot with facet_wrap
#Age vs Target vs sex
ggplot(data, aes(x=age, fill=target)) + 
    geom_bar(position='dodge')+
    labs(fill="Disease", x="Age", y="Number of patients",
         title = "Age vs. Class(Healthy/Sick) vs. Gender")+
    facet_wrap(~sex)
