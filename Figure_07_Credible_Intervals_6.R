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

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,139]
cpu <- unlist(cpu)

########################################################## Visualization Plots ##########################################################

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


ARNN_6_pred <- c(226.3579411,238.4473562,234.1059522,237.7988202,207.9639836,238.0278949)
NLinear_6_pred <- c(219.4547027,270.8637208,259.823853,246.6027547,220.4746336,253.7184355)

data_6 <- as_tibble(cbind.data.frame('LL' = as.numeric(bsts_6_pred$interval[1,]),
                                     'UL' = as.numeric(bsts_6_pred$interval[2,]),
                                     'Date' = cpu_data$Date[(length(cpu) - n + 1):length(cpu)],
                                     'Test' = test_6,
                                     'BSTS' = bsts_6_pred$mean,
                                     'NLinear' = NLinear_6_pred))

max(data_6$UL - data_6$LL)

ggplot(data = data_6, aes(x = Date)) +
  geom_line(aes(y = BSTS, color = 'BSTS'), linewidth = 2.5) +
  geom_line(aes(y = NLinear, color = 'NLinear'), linewidth = 2.5) +
  geom_point(aes(y = Test, color = 'Ground Truth'), size = 4) +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = 'gold', alpha = .25) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d", limits = c(ymd("2023-01-01"), ymd("2023-06-01"))) +
  scale_y_continuous(limits = c(0, round(max(data_train_6$Train)))) +
  ylab('CPU Index') +
  xlab('Time') +
  labs(color = 'Models') +
  scale_color_manual(values = c(
    'BSTS' = 'forestgreen',
    'NLinear' = 'orange',
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
