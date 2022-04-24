library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(plotly)

# Data Provided by UCI ML Repository
# https://archive.ics.uci.edu/ml/datasets/Facebook+Live+Sellers+in+Thailand

# Data Cleaning

# import dataset
data <- read.csv("Live_20210128.csv")

# remove empty columns
data <- data[, -c(13:16)]

# check data type and statistic summary for each column 
summary(data)

# check missing value
sum(is.na(data))

# substract year and month from status_published

data$status_published_year <- format(as.Date(data$status_published,'%m/%d/%Y'), '%Y')
data$status_published_month <- format(as.Date(data$status_published,'%m/%d/%Y'), '%m')

# group by status_type, year, and month
facebook_data=data %>% group_by(status_type, status_published_year,status_published_month) %>% 
  dplyr::summarise(total_num_reactions = sum(num_reactions)/n(),
                   total_num_comments = sum(num_comments)/n(),
                   total_num_shares = sum(num_shares)/n(),
                   total_num_likes = sum(num_likes)/n(),
                   total_num_loves = sum(num_loves)/n(),
                   total_num_wows = sum(num_wows)/n(),
                   total_num_hahas = sum(num_hahas)/n(),
                   total_num_sads = sum(num_sads)/n(),
                   total_num_angrys = sum(num_angrys)/n())

facebook_data$Date<-as.yearmon(paste(facebook_data$status_published_year, 
                                     facebook_data$status_published_month), "%Y %m")

post<-data %>% group_by(status_published_year, status_type) %>% dplyr::summarise(n=n())

