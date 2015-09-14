#This script analyzes commercial banking data from 2Q2015 fro the US. 
#Want to see if there is a correlation between diversification on the liability side of a banks balance sheet and profitability as measured by ROE and ROA

#Loading some packages
library(lattice)
library(stats)
library(tseries)
library(Hmisc)

#Cleaning up
rm(list = ls())

#Setting up the working directory
setwd("~/Research/MMF/cross_section_us_banks/2q2015")

#Reading the data set of assets and liabilities

asset_liab_db <- read.csv("assets_liabilities_2q2015.csv", header = TRUE)

#Summary of the data base

summary(asset_liab_db)

#REading data on income and expenses

income_exp_db <- read.csv("income_expenses_2q2015.csv", header = TRUE)

summary(income_exp_db)

#First thing we are going to do is to merge the databases together using the unique fdic identifier

data_base <- merge(asset_liab_db, income_exp_db,by = "cert")

#Dropping the other data sets
rm(asset_liab_db,income_exp_db)

#Summary of data_base
summary(data_base)

#Dropping observations that are not commercial banks
data_base <- data_base[which(data_base$bkclass.x == "N"| data_base$bkclass.x == "SM"| data_base$bkclass.x == "NM"),]
summary(data_base)

#In order to do our comparisons, we need to construct our variables of profitability, return on equity and return on assets
data_base <- transform(data_base, roe =( (noij*200)/eqtot), roa =((noij*200)/asset)  )

attach(data_base) 

summary(roe) 
summary(roa)

hist(roe, breaks = 500)

#Missing trimming the data base
