##################################################
# ECON 418-518 Exam 3
# Devin Sciarrino
# The University of Arizona
# devinsciarrino@arizona.edu 
# 12 December 2024
###################################################

#####################
# Preliminaries
#####################



# Clear environment, plot pane, and console
rm(list = ls())
graphics.off()
cat("\014")

# Turn off scientific notation
options(scipen = 999)

# pacman installation
if (!require(pacman)) install.packages("pacman")

# Load packages
pacman::p_load(ISLR2, caret, randomForest, data.table, ggplot2, glmnet, boot)

#set seed
set.seed(418518)

#setwd
setwd("~/Downloads")

#load data
dt <- read.csv("ECON_418-518_Exam_3_Data.csv")


#####################
# Problem 3
#####################


#################
# Question (ii)
#################

#is november comlumn creation
dt$is.nov <- ifelse(dt$time_period == "Nov",1,0)

#is New Jersey column creation
dt$is.nj <- ifelse(dt$state == 1,1,0)

#Mean employment for NJ in Feb
njfeb <- mean(dt$total_emp[dt$is.nj == 1 & dt$is.nov == 0])
njfeb

#Mean employment for NJ in Nov
njnov <- mean(dt$total_emp[dt$is.nj == 1 & dt$is.nov == 1])
njnov

#Mean employment for PA in Feb
pafeb <- mean(dt$total_emp[dt$is.nj == 0 & dt$is.nov == 0])
pafeb

#Mean employment for PA in Nov
panov <- mean(dt$total_emp[dt$is.nj == 0 & dt$is.nov == 1])
panov



#################
# Question (iii)
#################

#DiD estimate
DiD <- (njnov - njfeb) - (panov - pafeb)
DiD


#################
# Question (iv)
#################

#DiD regression
reg1 <- lm(total_emp ~ is.nov + is.nj + I(is.nov * is.nj), data = dt)
summary(reg1)

#confidene interval
att <- coef(reg1)["I(is.nov * is.nj)"]
se_att <- sqrt(vcov(reg1)["is.nov", "is.nj"])
z_value <- 1.96
margin_of_error <- z_value * se_att
lower_bound <- att - margin_of_error
upper_bound <- att + margin_of_error

cat("95% Confidence Interval for ATT: (", lower_bound, ", ", upper_bound, ")\n")

#################
# Question (vii)
#################

#DiD model with restaurant fixed effects
reg2 <- lm(total_emp ~ is.nov + is.nj + I(is.nov * is.nj) + factor(restaurant_id), data = dt)
summary(reg2)







