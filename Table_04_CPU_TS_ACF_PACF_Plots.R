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

##################################################### Plots #####################################################

ts_analysis_plots <- function(train_series, start_date) {
  
  # Automatically detect the date format
  if (grepl("\\d{4}-\\d{2}-\\d{2}", start_date)) {
    date_format <- "%Y-%m-%d"
  } else if (grepl("\\d{4}/\\d{2}/\\d{2}", start_date)) {
    date_format <- "%Y/%m/%d"  # Added condition for 'YYYY/MM/DD'
  } else if (grepl("\\d{2}/\\d{2}/\\d{4}", start_date)) {
    date_format <- "%m/%d/%Y"
  } else if (grepl("\\d{2}-\\d{2}-\\d{4}", start_date)) {
    date_format <- "%d-%m-%Y"
  } else if (grepl("\\d{2}\\.\\d{2}\\.\\d{4}", start_date)) {
    date_format <- "%d.%m.%Y"
  } else {
    stop("Unknown date format. Please provide a date in a recognized format.")
  }
  
  # Convert the start date to a Date object
  start_date <- as.Date(start_date, format = date_format)
  
  # Generate a sequence of dates for the training data
  train_dates <- seq.Date(from = start_date, by = paste(frequency(train_series), "month"), length.out = length(train_series))
  
  # Combine all the series in one data frame
  train_data <- data.frame(Date = train_dates, Value = train_series)
  
  
  # Time Series Plot
  time_series_plot <- ggplot(train_data, aes(x = Date, y = Value)) +
    geom_line(color = 'dodgerblue3', linewidth = 1.5) +
    labs(x = "Time", y = "CPU Index") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y-%m") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_text(size = 20, face = 'bold'), 
      axis.title.y = element_text(size = 20, face = 'bold'), 
      axis.text.x = element_text(size = 18, face = 'bold'),  
      axis.text.y = element_text(size = 18, face = 'bold'))    
  
  # ACF Plot
  diff_train_series <- diff(train_series, differences = ndiffs(train_series), ci = 0.8)
  acf_plot <- ggAcf(diff_train_series, size = 1.5) +
    geom_point(color = 'navy blue', size = 1.5) +
    ggtitle(NULL) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_text(size = 20, face = 'bold'), 
      axis.title.y = element_text(size = 20, face = 'bold'), 
      axis.text.x = element_text(size = 18, face = 'bold'),  
      axis.text.y = element_text(size = 18, face = 'bold'))  
  
  
  # PACF Plot
  pacf_plot <- ggPacf(train_series) +
    geom_point(color = 'navy blue', size = 1.5) +
    ggtitle(NULL) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.title.x = element_text(size = 20, face = 'bold'), 
      axis.title.y = element_text(size = 20, face = 'bold'), 
      axis.text.x = element_text(size = 18, face = 'bold'),  
      axis.text.y = element_text(size = 18, face = 'bold')) 
  
  
  # Combine all plots
  combined_plot <- grid.arrange(time_series_plot, acf_plot, pacf_plot, ncol = 3)
  
  # Return the combined plot
  return(combined_plot)
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

train_24 <- cpu_data[1:(nrow(cpu_data) - n), c(1,ncol(cpu_data))]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])
ts_analysis_plots(train_series = unlist(train_24[,2]), start_date = '1987-04-01')



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
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])
ts_analysis_plots(train_series = unlist(cpu_data[,2]), start_date = '2000-01-31')



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
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])
ts_analysis_plots(train_series = unlist(cpu_data[,2]), start_date = '2000-01-31')
