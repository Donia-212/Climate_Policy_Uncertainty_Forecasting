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

statistical_tests_summary <- function(data) {
  
  if (!is.numeric(data)){
    stop("Data must be numeric")
  } 
  
  if(any(is.na(data))){
    data <- na.approx(data)
  }
  
  skewness_value <- skewness(data)
  kurtosis_value <- kurtosis(data)
  
  if(is.nan(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value)){
    if(nonlinearityTest(data, verbose = TRUE)$Keenan$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }else{
    if(nonlinearityTest(data, verbose = TRUE)$Tsay$p.value <= 0.05){
      nonlinearity_value <- 'Non-linear'
    }else{
      nonlinearity_value <- 'Linear'
    }
  }
  
  check_seasonality <- function(data) {
    seasonality_value <- NULL
    
    tryCatch({
      if (isSeasonal(data, test = "seasdum", freq = 12)) {
        seasonality_value <- 'Seasonal'
      } else {
        seasonality_value <- 'Non-seasonal'
      }
    }, error = function(e) {
      if (isSeasonal(data, test = "combined", freq = 12)) {
        seasonality_value <<- 'Seasonal'
      } else {
        seasonality_value <<- 'Non-seasonal'
      }
    })
    
    return(seasonality_value)
  }
  seasonality_value <- check_seasonality(data)
  
  if(kpss.test(data)$p.value > 0.05){
    stationarity_value <- 'Startionary'
  } else{
    stationarity_value <- 'Non-stationary'
  } 
  
  hurst_Hs_value <- as.numeric(hurstexp(data)[1])
 
  result <- tibble(
    Skewness = skewness_value,
    Kurtosis = kurtosis_value,
    Linearity = nonlinearity_value,
    Seasonality = seasonality_value,
    Stationarity = stationarity_value,
    LongRangeDependence = hurst_Hs_value
  )
  return(result)
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
statistical_tests_summary(train_24$cpu_index)



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
statistical_tests_summary(train_24$CPU_US)


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
statistical_tests_summary(train_24$`GCPU(PPP-adjusted GDP)`)
