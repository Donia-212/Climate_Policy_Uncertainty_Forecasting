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
library(patchwork)


########################################################## Visualization Plot: Primary US ##########################################################

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

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

# BSTS
ss <- AddLocalLinearTrend(list(), train_6$cpu_index)
ss <- AddAutoAr(ss, train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$cpu_index, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)

fit_6 = (bsts_6$original.series - colMeans(bsts_6$one.step.prediction.errors[-(1:SuggestBurn(.1, bsts_6)),]))[2:length(bsts_6$original.series)]
fitted_6 = as.numeric(rbind(matrix(fit_6, ncol = 1), bsts_6_pred$mean[1]))
data_train_6 <- as_tibble(cbind.data.frame('Date' = cpu_data$Date[1:(length(cpu) - n)],
                                           'Train' = cpu_data$cpu_index[1:(length(cpu) - n)],
                                           'Fitted' = fitted_6))

ggplot(data = data_train_6, aes(x = Date)) +
  geom_line(aes(y = Train, color = 'Training Data'), linewidth = 2.5) +
  geom_line(aes(y = Fitted, color = 'BSTS Fitted'), linewidth = 2.5) +
  scale_x_date(date_breaks = "75 months", date_labels = "%Y-%m-%d", limits = c(ymd("1987-01-01"), ymd("2022-12-01"))) +
  ylab('CPU Index') +
  xlab('Time') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS Fitted' = 'forestgreen',
    'Training Data' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')

ARIMA_6_pred <- c(222.20669,234.7890855,234.4439647,264.6541696,178.3970244,215.5832136)

data_6 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_6_pred$interval[1,]),
                                     'UL' = as.numeric(bsts_6_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_6,
                                     'BSTS' = bsts_6_pred$mean,
                                     'ARIMA' = ARIMA_6_pred))

max(data_6$UL - data_6$LL)


ggplot(data = data_6, aes(x = Date)) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2.5) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2.5) +
  geom_point(aes(y = Test, color = 'Ground Truth'), size = 4) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-01"), ymd("2023-06-01"))) +
  scale_y_continuous(limits = c(0, round(max(data_train_6$Train)))) +
  ylab('CPU Index') +
  xlab('Time') +
  #ggtitle('CPU Index: 6 Month Holdout') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'ARIMA' = 'orange',
    'Ground Truth' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



########################################################## Visualization Plot: Alternate US ##########################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
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

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

# BSTS
ss <- AddLocalLinearTrend(list(), train_6$CPU_US)
ss <- AddSeasonal(ss, train_6$CPU_US, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$CPU_US, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)

fit_6 = (bsts_6$original.series - colMeans(bsts_6$one.step.prediction.errors[-(1:SuggestBurn(.1, bsts_6)),]))[2:length(bsts_6$original.series)]
fitted_6 = as.numeric(rbind(matrix(fit_6, ncol = 1), bsts_6_pred$mean[1]))
data_train_6 <- as_tibble(cbind.data.frame('Date' = cpu_data$Date[1:(length(cpu) - n)],
                                           'Train' = cpu_data$CPU_US[1:(length(cpu) - n)],
                                           'Fitted' = fitted_6))

ggplot(data = data_train_6, aes(x = Date)) +
  geom_line(aes(y = Train, color = 'Training Data'), linewidth = 2.5) +
  geom_line(aes(y = Fitted, color = 'BSTS Fitted'), linewidth = 2.5) +
  scale_x_date(date_breaks = "45 months", date_labels = "%Y-%m-%d", limits = c(ymd("2000-01-01"), ymd("2022-12-31"))) +
  ylab('CPU Index') +
  xlab('Time') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS Fitted' = 'forestgreen',
    'Training Data' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')

ARIMA_6_pred <- c(2.566715,2.61829,2.942542,3.103225,3.488636,3.803545)

data_6 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_6_pred$interval[1,]),
                                     'UL' = as.numeric(bsts_6_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_6,
                                     'BSTS' = bsts_6_pred$mean,
                                     'ARIMA' = ARIMA_6_pred))

max(data_6$UL - data_6$LL)


ggplot(data = data_6, aes(x = Date)) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2.5) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2.5) +
  geom_point(aes(y = Test, color = 'Ground Truth'), size = 4) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-31"), ymd("2023-07-01"))) +
  scale_y_continuous(limits = c(-1, round(max(data_train_6$Train)))) +
  ylab('CPU Index') +
  xlab('Time') +
  #ggtitle('CPU Index: 6 Month Holdout') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'ARIMA' = 'orange',
    'Ground Truth' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')



########################################################## Visualization Plot: Global ##########################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
cpu_data <- read_excel('GCPU_Data.xlsx')[1:282,c(1,4)] 
cpu_data$Date <- as.Date(cpu_data$date)
cpu_data$Date <- ymd(cpu_data$Date)
setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network')
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)
covariates_reduced <- covariates_reduced[154:nrow(covariates_reduced),]  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,2]
cpu <- unlist(cpu)

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

# BSTS
ss <- AddLocalLevel(list(), train_6$`GCPU(PPP-adjusted GDP)`)
ss <- AddAutoAr(ss, train_6$`GCPU(PPP-adjusted GDP)`)
bsts_6 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$`GCPU(PPP-adjusted GDP)`, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)

fit_6 = (bsts_6$original.series - colMeans(bsts_6$one.step.prediction.errors[-(1:SuggestBurn(.1, bsts_6)),]))[2:length(bsts_6$original.series)]
fitted_6 = as.numeric(rbind(matrix(fit_6, ncol = 1), bsts_6_pred$mean[1]))
data_train_6 <- as_tibble(cbind.data.frame('Date' = cpu_data$Date[1:(length(cpu) - n)],
                                           'Train' = cpu_data$`GCPU(PPP-adjusted GDP)`[1:(length(cpu) - n)],
                                           'Fitted' = fitted_6))

ggplot(data = data_train_6, aes(x = Date)) +
  geom_line(aes(y = Train, color = 'Training Data'), linewidth = 2.5) +
  geom_line(aes(y = Fitted, color = 'BSTS Fitted'), linewidth = 2.5) +
  scale_x_date(date_breaks = "45 months", date_labels = "%Y-%m-%d", limits = c(ymd("2000-01-01"), ymd("2022-12-31"))) +
  ylab('CPU Index') +
  xlab('Time') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS Fitted' = 'forestgreen',
    'Training Data' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')

ARIMA_6_pred <- c(162.170023,163.2881402,164.1957105,164.5362481,164.708343,164.7845872)

data_6 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_6_pred$interval[1,]),
                                     'UL' = as.numeric(bsts_6_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_6,
                                     'BSTS' = bsts_6_pred$mean,
                                     'ARIMA' = ARIMA_6_pred))

max(data_6$UL - data_6$LL)


ggplot(data = data_6, aes(x = Date)) +
  geom_line(aes(y = ARIMA, color = 'ARIMA'), linewidth = 2.5) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2.5) +
  geom_point(aes(y = Test, color = 'Ground Truth'), size = 4) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-31"), ymd("2023-07-01"))) +
  scale_y_continuous(limits = c(0, round(max(data_train_6$Train)))) +
  ylab('CPU Index') +
  xlab('Time') +
  #ggtitle('CPU Index: 6 Month Holdout') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'ARIMA' = 'orange',
    'Ground Truth' = 'red'
  )) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 15, face = 'bold'), 
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.position = 'bottom')
