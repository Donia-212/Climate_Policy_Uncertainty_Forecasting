library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fpp3)
library(tsibble)
library(fable)
library(readxl)
library(stats)
library(patchwork)
library(rugarch)
library(pracma)
library(e1071)
library(tseries)
library(nonlinearTseries)
library(seastests)
library(car)

##################################################### Functions #####################################################

SUMMARY <- function(data) {
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  CoV <- function(data) {
    return(sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE) * 100)
  }
  
  Entropy <- function(data) {
    probs <- table(data) / length(data)
    return(-sum(probs * log(probs), na.rm = TRUE))
  }
  
  Min <- min(data, na.rm = TRUE)
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Median <- median(data, na.rm = TRUE)
  Mean <- mean(data, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  Max <- max(data, na.rm = TRUE)
  CoV_value <- CoV(data)
  Entropy_value <- Entropy(data)
  
  summ <- tibble(
    Min = Min,
    Q1 = Q1,
    Median = Median,
    Mean = Mean,
    Q3 = Q3,
    Max = Max,
    CoV = CoV_value,
    Entropy = Entropy_value
  )
  
  return(summ)
}

################################################## Primary US CPU ################################################## 

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,139]
cpu <- unlist(cpu)

n = 24
set.seed(100)
train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
SUMMARY(train_24$cpu_index)


################################################## Alternate US CPU ################################################## 

cpu_data <- read_excel('GCPU_Data.xlsx')[1:282,1:2] 
cpu_data$Date <- as.Date(cpu_data$date)
cpu_data$Date <- ymd(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)
covariates_reduced <- covariates_reduced[154:nrow(covariates_reduced),]  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,2]
cpu <- unlist(cpu)

n = 24
set.seed(100)
train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
SUMMARY(train_24$CPU_US)


################################################## Global CPU ################################################## 

cpu_data <- read_excel('GCPU_Data.xlsx')[1:282,c(1,4)] 
cpu_data$Date <- as.Date(cpu_data$date)
cpu_data$Date <- ymd(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)
covariates_reduced <- covariates_reduced[154:nrow(covariates_reduced),]  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,2]
cpu <- unlist(cpu)

n = 24
set.seed(100)
train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
SUMMARY(train_24$`GCPU(PPP-adjusted GDP)`)
