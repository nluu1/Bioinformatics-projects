## Same codes form Kaggle analysis but import raw data from Github folder and more functions not supported on Kaggle

#Install libraries
library(ggplot2)
library(dplyr)

#Import raw data
heart.data.raw <- read.csv("https://raw.githubusercontent.com/nluu1/Bioinformatics-projects/3c7137f962364fd7d5a9ed1e016a64fefc645b86/in-R/data/heart.csv")
View(heart.data.raw)

#create a copy of raw data to manipulate
data <- heart.data.raw
View(data)

#Take a look at the top and bottom 10 rows
View(head(data,10))
View(tail(data,10))


