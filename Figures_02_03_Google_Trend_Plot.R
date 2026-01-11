library(ggplot2)
library(readr)
library(dplyr)
library(tsibble)
library(fable)
library(readxl)
library(stats)
library(patchwork)

##################################################### Google Trends: Figure 02 #####################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
trends <- read.csv("Google_Trends_US_WW.csv")
trends$Date <- as.Date(paste0(trends$Date, "-01"), format = "%Y-%m-%d")

g1 <- ggplot(data = trends, aes(x = as.Date(Date))) +
  geom_line(aes(y = Environmental.Policy...Worldwide., color = 'Environmental Policy (Worldwide)'), linewidth = 2) +
  geom_line(aes(y = Environmental.Policy...United.States., color = 'Environmental Policy (United States)'), linewidth = 2) +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y-%m-%d", limits = c(ymd("2004-01-01"), ymd("2021-06-01"))) +
  ylab('Interest') +
  xlab('Date') +
  labs(color = 'Search Terms') +
  ggtitle('Interest Over Time') +
  scale_color_manual(values = c(
    'Environmental Policy (Worldwide)' = '#960018',
    'Environmental Policy (United States)' = 'dodgerblue3'
    )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 20, face = 'bold'), 
        axis.title = element_text(size = 25, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 20, face = 'bold'), 
        legend.title = element_text(size = 25, face = 'bold'),
        legend.position = 'bottom')


g2 <- ggplot(data = trends, aes(x = as.Date(Date))) +
  geom_line(aes(y = Carbon.Credits...Worldwide., color = 'Carbon Credits (Worldwide)'), linewidth = 2) +
  geom_line(aes(y = Carbon.Credits...United.States., color = 'Carbon Credits (United States)'), linewidth = 2) +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y-%m-%d", limits = c(ymd("2004-01-01"), ymd("2021-06-01"))) +
  ylab('Interest') +
  xlab('Date') +
  labs(color = 'Search Terms') +
  ggtitle('Interest Over Time') +
  scale_color_manual(values = c(
    'Carbon Credits (Worldwide)' = '#960018',
    'Carbon Credits (United States)' = 'dodgerblue3'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 20, face = 'bold'), 
        axis.title = element_text(size = 25, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 20, face = 'bold'), 
        legend.title = element_text(size = 25, face = 'bold'),
        legend.position = 'bottom')

g1 + g2



##################################################### Google Trends: Figure 03 #####################################################

cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

trends <- read.csv("GooleTrendsData.csv")
trends$Date <- as.Date(paste0(trends$Date, "-01"), format = "%Y-%m-%d")

ggplot(data = trends, aes(x = as.Date(Date))) +
  geom_line(aes(y = Climate.policy...Worldwide., color = 'Climate Policy'), linewidth = 2) +
  geom_line(aes(y = Climate.Risk...Worldwide., color = 'Climate Risk'), linewidth = 2) +
  geom_line(aes(y = cpu_data$cpu_index[202:411], color = 'CPU'), linewidth = 2) +
  scale_x_date(date_breaks = "3 year", date_labels = "%Y-%m-%d", limits = c(ymd("2004-01-01"), ymd("2021-06-01"))) +
  ylab('Interest') +
  xlab('Date') +
  labs(color = 'Search Terms') +
  ggtitle('Interest Over Time') +
  scale_color_manual(values = c(
    'Climate Risk' = '#960018',
    'Climate Policy' = 'dodgerblue3',
    'CPU' = 'forestgreen'
  )) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text = element_text(size = 20, face = 'bold'), 
        axis.title = element_text(size = 25, face = "bold"),
        plot.title = element_text(size = 30, face = 'bold'),
        legend.text = element_text(size = 20, face = 'bold'), 
        legend.title = element_text(size = 25, face = 'bold'))
