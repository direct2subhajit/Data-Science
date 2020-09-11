## This R markdown consists of Exploratory data analysis of Diamonds Dataset

# clear all existing variables from console
cat("\014") 
# clear all data variables 
rm(list = ls())

# Checking working directory
getwd()

# Loading library
library(ggplot2)
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("GGally")
install.packages('RColorBrewer')
library(ggcorrplot)
library(corrplot)
library(tidyverse)
library(GGally)
#--------------------------------------------
# Read Input    
#--------------------------------------------
diamond <- read.csv("diamonds.csv",header = TRUE)
head(diamond,5)

#Number of Observations
dim(diamond)
str(diamond)
#Number of Variables
names(diamond)

#Ordered Factors in DataSet

print("cut")
table(diamond$cut)
print("color")
table(diamond$color)
print("clarity")
table(diamond$clarity)

#Diamonds Dataset Summary

summary(diamond)

#Price Histogram

qplot(x = price, data = diamond, color = I('black'), fill = I('blue'), binwidth = 400)

#Price Summary

summary(diamond$price)

#Number of diamonds costs less than $500

sum(diamond$price < 500)
qplot(x = price, data = subset(diamond, diamond$price < 500), color = I('black'), fill = I('blue'), binwidth = 10)

#Number of diamonds costs less than $250

sum(diamond$price < 250)

#Number of diamonds costs greater than equal to $15000

sum(diamond$price >= 15000)
qplot(x = price, data = subset(diamond, diamond$price >= 15000), color = I('black'), fill = I('blue'), binwidth = 10)

#Exploring the large peak in price Histogram

qplot(x = price, data = diamond, color = I('black'), fill = I('blue'), binwidth = 20) +
  scale_x_continuous(limits = c(0,800))

#Price Histogram Based on Cut of the Diamond

qplot(x = price, data = diamond, color = I('black'), fill = I('blue'), binwidth = 400) +
  facet_wrap(~diamond$cut, ncol = 3,  scales = "free")

#Cut with highest Priced Diamond

by(diamond$price,diamond$cut,max)

#Cut with Lowest Priced Diamond

by(diamond$price,diamond$cut,min)

#Cut with Lowest Median Priced Diamond

by(diamond$price,diamond$cut,median)

#Price Per Carat Histogram

qplot(x = price/carat, data = diamond, color = I('black'), fill = I('blue')) +
  scale_x_log10() +
  facet_wrap(~diamond$cut, ncol  = 3,  scales = "free")

#Box plots By clarity

g <- ggplot(diamond, aes(clarity, price))
g + geom_boxplot(varwidth = T, fill = "plum") + 
  labs(title = "Box plot", 
       subtitle = "Box Plot On Clarity Basis",
       caption = "Source: diamond",
       x = "Clarity",
       y = "Price")

#Box plots By cut

g1 <- ggplot(diamond, aes(cut, price))
g1 + geom_boxplot(varwidth = T, fill = "plum") + 
  labs(title = "Box plot", 
       subtitle = "Box Plot On Cut Basis",
       caption = "Source: diamond",
       x = "Cut",
       y = "Price")

#Box plots By color

g2 <- ggplot(diamond, aes(color, price))
g2 + geom_boxplot(varwidth = T, fill = "plum") + 
  labs(title = "Box plot", 
       subtitle = "Box Plot On Color Basis",
       caption = "Source: diamond",
       x = "Color",
       y = "Price")
      
#Price Range for Middle 50% of the diamonds with the color D

summary(subset(diamond,color == "D")$price)

#IQR of the diamonds with the best color 

IQR(subset(diamond,color == "D")$price)

#Price Range for Middle 50% of the diamonds with the color J

summary(subset(diamond,color == "J")$price)

#IQR of the diamonds with the Worst color

IQR(subset(diamonds,color == "J")$price)

#Price per carat of diamonds across the different colors of diamonds using box plots.

g3 <- ggplot(diamond, aes(color, price/carat))
g3 + geom_boxplot(varwidth = T, fill = "plum") + 
  labs(title = "Box plot", 
       subtitle = "Box Plot On Price per Carat",
       caption = "Source: diamond",
       x = "Color",
       y = "Price per carat ")

#The Weight of Diamonds vs carat

qplot(geom = "freqpoly", x = carat, data = diamond , bins = 500) +
  scale_x_continuous(limits = c(0,2.5))

#Correlation matrix

set.seed(42) #Yep, inserting the "cool data science seed thing to do #42" :) Taken from https://rpubs.com/anthonycerna/diamondspredictions

diamond_samp <- diamond[sample(1:length(diamond$price), 25000), ] #Looking at the first 25,000 diamonds

ggpairs(diamond_samp, outlier.shape = I('.')) #Taken from https://rpubs.com/taylorwhite/diamondPricing 
