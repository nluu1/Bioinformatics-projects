## Reference code: http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics

##Installing Packages
# install.packages('dplyr')
# instal.packages("tidyverse")

##Load library
library(dplyr)
library(ggplot2)
library(rlang) #for function()
library(gridExtra) #for grid

#--- Importing raw data from Github, set strings to factors:

cardio_data_raw <- read.csv("https://raw.githubusercontent.com/PhyloGrok/AnalyzeBloodwork/master/data/Cardio.csv",stringsAsFactors = T,)

#Refer to another name to preserve the original data-set:
cardio_data <- cardio_data_raw

#--- Exploring dataset:
View(cardio_data)

nrow(cardio_data)
ncol(cardio_data)

head(cardio_data,n=10)
tail(cardio_data,n=10)

summary(cardio_data)

str(cardio_data)

#-- Cleaning/Preparing data:

#Transform some variable (e.g logical) to R factors (that wasn't transformed from read.csv)
cardio_data$PG_fasting <- as.factor(cardio_data$PG_fasting)
cardio_data$angina <- as.factor(cardio_data$angina)

#Define levels for categorical values
levels(cardio_data$sex) <- c("Female", "Male")
levels(cardio_data$chest_pain_type) <- c("Asymptomatic", "Abnormal Angina", "Angina", "NoTang")
levels(cardio_data$PG_fasting) <- c("No", "Yes")
levels(cardio_data$ECG_rest) <- c("Adnormal", "Hyp", "Normal")
levels(cardio_data$angina) <- c("No", "Yes")
levels(cardio_data$slope) <- c("Down", "Flat", "Up")
levels(cardio_data$thal) <- c("Fix", "Normal", "Rev")
levels(cardio_data$class) <- c("Healthy", "Sick")

#--- Descriptive statistics: Mean, Median, SD, Print out a table of the summary stastistics

##Create a sub table of continuous data
cont_data <- select(cardio_data, age, BP, cholesterol,maxHR,peak)
##Summary table: Mean, Median, SD
cont_data <- lapply(cont_data,as.numeric)
sum_table <- data.frame(
    Mean = sapply(cont_data,mean),
    Median = sapply(cont_data,median),
    Variance = sapply(cont_data,var),
    SD = sapply(cont_data,sd)) 

sum_table <- sum_table %>% mutate_all(round,3)

View(sum_table,'Summary Statistics')

#--- Print out Histograms for the continuous data

#Function for continuous data cont.plot
cont.plot <- function(data,X,xlabel,title){
    ggplot(data=data,aes(x={{X}}))+
        geom_histogram(color = "black", fill = "green", bins = 12)+
        labs(x=xlabel, title=title)
} 
    
#Age
age <- cont.plot(cardio_data,X=age,xlabel="Age",title="Age Distribution")

#BP
BP <- cont.plot(cardio_data,X=BP,xlabel="Blood Pressure (mmHg)",title="Blood Pressure")

#Cholesterol
chol <- cont.plot(cardio_data,X=cholesterol,xlabel="Cholesterol level (mg/dL)",title="Cholesterol")

#Max HR
HR <- cont.plot(cardio_data,X=maxHR,xlabel="Maximum Heart Rate (bpm)",title="Maximum Heart Rate")

#Peak
peak <- cont.plot(cardio_data,X=peak,xlabel="Peak",title="Peak")

### Spreadsheet of histograms for continuous data
cont.histogram <- grid.arrange(age,BP,chol,HR,peak, ncol=3)

#-- Print out Histograms for the categorical data

#Function for continuous data cat.plot
cat.plot <- function(data,X,xlabel,title){
    ggplot(data=data, aes(x={{X}}, fill={{X}})) + 
        geom_bar() +
        labs(x=xlabel, y="Number of patients",
             title = title) +
        scale_fill_discrete(name = xlabel)
}

#Class (healthy and sick)
class <- cat.plot(data=cardio_data,X=class,
                  xlabel="Disease conditition",
                  title="Healthy or Sick Individuals")

#Sex
sex <- cat.plot(data=cardio_data,X=sex,
                  xlabel="Gender",
                  title="Gender (Male or Female)")


#chest_pain_type:
cp <- cat.plot(data=cardio_data,X=chest_pain_type,
                xlabel="Chest Pain Type",
                title="Chest Pain Type")

#PG_fasting
pgfast <- cat.plot(data=cardio_data,X=PG_fasting,
               xlabel="PG Fasting",
               title="Plasma Glucose Fasting")

#ECG_rest
ecg <- cat.plot(data=cardio_data,X=ECG_rest,
                   xlabel="ECG rest",
                   title="Electrocardiogram on rest")

#Thal
thal <- cat.plot(data=cardio_data,X=thal,
                xlabel="Thalach",
                title="Maximum heart rate achieved (thalach)")

#Angina
angina <- cat.plot(data=cardio_data,X=angina,
                 xlabel="Angina",
                 title="Angina chest pain")


#slope
slope <- cat.plot(data=cardio_data,X=slope,
                 xlabel="Slope",
                 title="Slope of ST Segment")

### Spreadsheet of histograms for categorical data
cat.barplot <- grid.arrange(class, sex, cp, pgfast, ecg, thal, angina, slope, ncol=3, nrow=3)


##Essentially, we do not want to just look at each categorical data, but we want to compare them
#---- Here, I will compare data with Healthy/Sick status

######Add ' position = "fill" ' in geom_bar() if want to see 2 column for each, otherwise they are stacked

#Function for comparison of other data features with health condition (healthy/sick)
compare.plot <- function(data,X,fill,xlabel,title){
  ggplot(data=data, aes(x={{X}}, fill=class)) + 
    geom_bar() +
    labs(fill="Disease", x=xlabel, y="Number of patients",
         title = title)
}

#Gender vs Class (Healthy/Sick)
sex.class <- compare.plot(data=cardio_data, X=sex, 
                          xlabel = "Gender",title="Gender (F/M) vs. Class (Healthy/Sick)")

#Age vs Class (Healthy/Sick)
age.class <- compare.plot(data=cardio_data, X=age, 
                          xlabel = "Age",title="Age vs. Class (Healthy/Sick)")

#chest pain type vs Class (Healthy/Sick)
cp.class <- compare.plot(data=cardio_data, X=chest_pain_type, 
                          xlabel = "Chest pain type",title="Chest pain type vs. Class (Healthy/Sick)")

#ECG_rest vs Class (Healthy/Sick)
ecg.class <- compare.plot(data=cardio_data, X=ECG_rest, 
                         xlabel = "ECG Rest",title="Electrocardiogram on rest vs. Class (Healthy/Sick)")

#thalach vs Class (Healthy/Sick)
thal.class <- compare.plot(data=cardio_data, X=thal, 
                          xlabel = "Thalach",title="Maximum heart rate achieved vs. Class (Healthy/Sick)")

#Slope vs Class (Healthy/Sick)
slope.class <- compare.plot(data=cardio_data, X=slope, 
                          xlabel = "Slope",title="Slope of ST Segment vs. Class (Healthy/Sick)")

### Spreadsheet of histograms for data comparison among healthy/sick individuals
plot.class <- grid.arrange(sex.class, cp.class, ecg.class, thal.class, slope.class, age.class, ncol=3, nrow=3)

#Multiple variables: e.g #Slope vs Class vs Peak
ggplot(cardio_data, aes(x=slope, y=peak, fill=class)) +
    geom_boxplot() +
    labs(fill="Disease", x="Slope of ST segment", y="Depression of ST segment")

## Hypothesis testing (T-tests)
#in process