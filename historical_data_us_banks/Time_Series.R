#Exploring Time Series Data
#Loading required packages
library(lattice)
library(stats)
library(tseries)
library(Hmisc)
#Cleaning up
rm(list = ls())
# Annual daata from FDIC Insured Banks from 1934-2014 
#Setting the working directory
setwd("~/Documents/Research Uchicago/Historical Data US Banking/Commercial Banks")
#Reading the Historical Data Set on
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
summary(data_set)

#Lets get a subset of the variables that we are going to be working with

data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds", "net_income", "assets", "equity_capital_total" )]
summary(data_set)

#Calculating fed funds as a percentage of loabilities and ROA and ROE

data_set <- transform(data_set, mmf = (liab_borrowed_funds/liab_total_deposits)*100, roa = (net_income/assets)*100, roe = (net_income/equity_capital_total)*100 )
summary(data_set)

#Lets look at the data
attach(data_set)
hist(mmf, breaks =50)
par(mfrow=c(3,1))
plot(year, roa,col = "red", type = "b", cex = 0.5, lty = 2, main = "Return on Assets Through Time", xlab = "Year", ylab = "ROA" )
plot(year, roe,col = "blue", type = "b", cex = 0.5, lty = 2, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
plot(year, mmf,col = "purple", type = "b", cex = 0.5, lty = 2, main = "Money Market Funding % of total deposits", xlab = "Year", ylab = "Money Market Funding" )
summary(mmf)

#Lets look at all of the graphics together. To to this, we are going to normalize the data by its sample mean
par(mfrow=c(1,1))

plot(year, roa/mean(roa),col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "ROA, ROE and Money Market Funding",sub = "Normilized by Variable Mean", xlab = "Year", ylab = "ROA, ROE and MMF", ylim = c(-2,3) )
lines(year, roe/mean(roe),col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, mmf/mean(mmf),col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "Money Market Funding % of total deposits", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topleft", c("ROA", "ROE", "MMF"), lty = c(2,2,2), col = c("red", "blue", "black"), pch = c(0,1,2), cex = 0.75)

par(mfrow=c(1,1))
plot(mmf, roe, col = "red", cex=.8, main= "ROE vs MMF" )
abline(lm(roe~mmf))
reg_1_roe_mmf <- lm(roe~mmf)
summary(reg_1_roe_mmf)
par(mfrow=c(1,1))
plot(mmf, roa, col = "blue", cex=.8, main= "ROA vs MMF" )
abline(lm(roa~mmf))
reg_1_roa_mmf <- lm(roa~mmf)
summary(reg_1_roa_mmf)

#Lets control for a common time trend
reg_2_roe_mmf <- lm(roe~mmf+ year)
summary(reg_2_roe_mmf)

reg_2_roa_mmf <- lm(roa~mmf+ year)
summary(reg_2_roa_mmf)

time_trend <- c(81:1)
detach(data_set)
data_set <- transform(data_set, trend = time_trend)
attach(data_set)

reg_3_roe_mmf <- lm(roe~mmf+ trend)
summary(reg_3_roe_mmf)

reg_3_roa_mmf <- lm(roa~mmf+ trend)
summary(reg_3_roa_mmf)

detach(data_set)

#Now, lets look at the analyisis between profitability measures and diversification in the liability side of the market
#Lets start by cleaning up again and loading the original data base
rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
summary(data_set)
#Lets look at the first liability clasification. In order to do this, we need to drop data that is prior to 1944

data_set <- subset(data_set, year > 1943)

#Now we want to keep only the variables that we want
data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]

#Constructing the HHI index on liabilities
data_set <- transform(data_set, hhi = (liab_total_deposits/liab_total)^2+ (liab_borrowed_funds/liab_total)^2+(liab_subordinated_notes/liab_total)^2+(liab_other/liab_total)^2, mmf = (liab_borrowed_funds/liab_total_deposits)*100, roa = (net_income/assets)*100, roe = (net_income/equity_capital_total)*100  )

attach(data_set)
par(mfrow=c(3,1))
plot(year, hhi,col = "red", type = "b", cex = 0.5, lty = 2, main = "HHI of Liabilities Through Time", xlab = "Year", ylab = "HHI" )
plot(year, roa,col = "purple", type = "b", cex = 0.5, lty = 2, main = "Return on Assets Through Time", xlab = "Year", ylab = "ROA" )
plot(year, roe,col = "blue", type = "b", cex = 0.5, lty = 2, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )

#Lets look at all of the graphics together. To to this, we are going to normalize the data by its sample mean
par(mfrow=c(1,1))

plot(year, roa/mean(roa),col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "ROA, ROE and HHI",sub = "Normilized by Variable Mean", xlab = "Year", ylab = "ROA, ROE and HHI", ylim = c(-0.5,2) )
lines(year, roe/mean(roe),col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, hhi/mean(hhi),col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topleft", c("ROA", "ROE", "HHI"), lty = c(2,2,2), col = c("red", "blue", "black"), pch = c(0,1,2), cex = 0.75)

par(mfrow=c(1,1))
plot(hhi, roe, col = "red", cex=.8, main= "ROE vs HHI" )
abline(lm(roe~hhi))
reg_1_roe_hhi <- lm(roe~hhi)
summary(reg_1_roe_hhi)
par(mfrow=c(1,1))
plot(hhi, roa, col = "blue", cex=.8, main= "ROA vs HHI" )
abline(lm(roa~hhi))
reg_1_roa_hhi <- lm(roa~hhi)
summary(reg_1_roa_hhi)

#Lets control for a common time trend

time_trend <- c(71:1)
detach(data_set)
data_set <- transform(data_set, trend = time_trend)
attach(data_set)

reg_2_roe_hhi <- lm(roe~hhi+ trend)
summary(reg_2_roe_hhi)

reg_2_roa_hhi <- lm(roa~hhi+ trend)
summary(reg_2_roa_hhi)

summary(lm(roa~ trend))


detach(data_set)
#LEts continue doing some exploratory analysis

rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
summary(data_set)

data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]
data_set <- transform(data_set, dep_rat = (liab_total_deposits/liab_total)*100)
attach(data_set)

plot(year, dep_rat,col = "purple", type = "b", cex = 0.5, lty = 2, main = "Total Deposits as percentage of total liabilities", xlab = "Year", ylab = "Deposits (% of Total Liab)" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
detach(data_set)

rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- subset(data_set, year > 1943)
data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]
data_set <- transform(data_set,dep_rat = (liab_total_deposits/liab_total)*100, dep_funds = (liab_borrowed_funds/liab_total)*100, dep_notes = (liab_subordinated_notes/liab_total)*100, dep_other = (liab_other/liab_total)*100  )
attach(data_set)
par(mfrow = c(1,1))
plot(year, dep_rat,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "Liabilities",sub = "(% of Total Liabilities)", xlab = "Year", ylab = "Liabilities", ylim = c(0,100) )
lines(year, dep_funds,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, dep_notes,col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
lines(year, dep_other,col = "purple", type = "b", cex = 0.5, lty = 2,pch=3, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("left", c("Deposits", "Borrowing", "Notes", "Other"), lty = c(2,2,2,2), col = c("red", "blue", "black", "purple"), pch = c(0,1,2,3), cex = 0.75)

detach(data_set)


rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- subset(data_set, year > 1943)
data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]
data_set <- transform(data_set,dep_rat = (liab_total_deposits/liab_total)*100, dep_funds = (liab_borrowed_funds/liab_total)*100, dep_notes = (liab_subordinated_notes/liab_total)*100, dep_other = (liab_other/liab_total)*100  )
attach(data_set)
par(mfrow = c(1,1))
plot(year, (dep_rat-mean(dep_rat))/mean(dep_rat),col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "Liabilities",sub = "(% of Total Liabilities, normalized by mean)", xlab = "Year", ylab = "Liabilities", ylim = c(-2,2) )
lines(year, (dep_funds-mean(dep_funds))/mean(dep_funds),col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, (dep_other-mean(dep_other))/mean(dep_other),col = "purple", type = "b", cex = 0.5, lty = 2,pch=3, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
lines(year, (dep_notes-mean(dep_notes))/mean(dep_notes),col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topleft", c("Deposits", "Borrowing", "Notes", "Other"), lty = c(2,2,2,2), col = c("red", "blue", "black", "purple"), pch = c(0,1,2,3), cex = 0.75)

detach(data_set)


rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- subset(data_set, year > 1943)
data_set <- data_set[,c("year", "liab_total_deposits", "liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]
data_set <- transform(data_set,dep_rat = (liab_total_deposits/liab_total)*100, dep_funds = (liab_borrowed_funds/liab_total)*100, dep_notes = (liab_subordinated_notes/liab_total)*100, dep_other = (liab_other/liab_total)*100  )
attach(data_set)
par(mfrow = c(2,2))
plot(year, dep_rat,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "Deposits",sub = "(% of Total Liabilities)", xlab = "Year", ylab = "Deposits")
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_funds,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Borrowed Funds",sub = "(% of Total Liabilities)", xlab = "Year", ylab = "Funds" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_notes,col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "Notes",sub = "(% of Total Liabilities)", xlab = "Year", ylab = "Notes" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_other,col = "purple", type = "b", cex = 0.5, lty = 2,pch=3, main = "Other Liabilities",sub = "(% of Total Liabilities)", xlab = "Year", ylab = "Other" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")


detach(data_set)

#Looking at a broader definition of concentration and HHI index
rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- subset(data_set, year > 1964)
data_set <- data_set[,c("year", "liab_total_deposits","dep_indiv_corp","dep_us_govmt","dep_state_pol","dep_other", "fed_funds_purchased", "demand_notes_other_liab", "sub_notes_deb"  ,"liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]

data_set <- transform(data_set, hhi = (dep_indiv_corp/liab_total)^2+ (dep_us_govmt/liab_total)^2+ (dep_state_pol/liab_total)^2+ (dep_other/liab_total)^2+ (fed_funds_purchased/liab_total)^2+ (demand_notes_other_liab/liab_total)^2+ (sub_notes_deb/liab_total)^2+(liab_other/liab_total)^2, mmf = (liab_borrowed_funds/liab_total_deposits)*100, roa = (net_income/assets)*100, roe = (net_income/equity_capital_total)*100  )


attach(data_set)
par(mfrow=c(3,1))
plot(year, hhi,col = "red", type = "b", cex = 0.5, lty = 2, main = "HHI of Liabilities Through Time", xlab = "Year", ylab = "HHI" )
plot(year, roa,col = "purple", type = "b", cex = 0.5, lty = 2, main = "Return on Assets Through Time", xlab = "Year", ylab = "ROA" )
plot(year, roe,col = "blue", type = "b", cex = 0.5, lty = 2, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )

#Lets look at all of the graphics together. To to this, we are going to normalize the data by its sample mean
par(mfrow=c(1,1))

plot(year, roa/mean(roa),col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "ROA, ROE and HHI",sub = "Normilized by Variable Mean", xlab = "Year", ylab = "ROA, ROE and HHI", ylim = c(-0.5,2) )
lines(year, roe/mean(roe),col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, hhi/mean(hhi),col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("bottomleft", c("ROA", "ROE", "HHI"), lty = c(2,2,2), col = c("red", "blue", "black"), pch = c(0,1,2), cex = 0.75)

par(mfrow=c(1,1))
plot(hhi, roe, col = "red", cex=.8, main= "ROE vs HHI" )
abline(lm(roe~hhi))
reg_3_roe_hhi <- lm(roe~hhi)
summary(reg_3_roe_hhi)
par(mfrow=c(1,1))
plot(hhi, roa, col = "blue", cex=.8, main= "ROA vs HHI" )
abline(lm(roa~hhi))
reg_3_roa_hhi <- lm(roa~hhi)
summary(reg_3_roa_hhi)

#Lets control for a common time trend

time_trend <- c(50:1)
detach(data_set)
data_set <- transform(data_set, trend = time_trend)
attach(data_set)

reg_4_roe_hhi <- lm(roe~hhi+ trend)
summary(reg_4_roe_hhi)

reg_4_roa_hhi <- lm(roa~hhi+ trend)
summary(reg_4_roa_hhi)

summary(lm(roa~ trend))


detach(data_set)

#Final HHI dissagergation. We want to see the results from deposits.

rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- data_set[,c("year", "liab_total_deposits","dep_indiv_corp","dep_us_govmt","dep_state_pol","dep_other", "fed_funds_purchased", "demand_notes_other_liab", "sub_notes_deb"  ,"liab_borrowed_funds", "liab_subordinated_notes","liab_other","liab_total" , "net_income", "assets", "equity_capital_total" )]
data_set <- transform(data_set, hhi = (dep_indiv_corp/liab_total_deposits)^2+ (dep_us_govmt/liab_total_deposits)^2+ (dep_state_pol/liab_total_deposits)^2+ (dep_other/liab_total_deposits)^2, mmf = (liab_borrowed_funds/liab_total_deposits)*100, roa = (net_income/assets)*100, roe = (net_income/equity_capital_total)*100  )


attach(data_set)
par(mfrow=c(3,1))
plot(year, hhi,col = "red", type = "b", cex = 0.5, lty = 2, main = "HHI of Liabilities Through Time", xlab = "Year", ylab = "HHI" )
plot(year, roa,col = "purple", type = "b", cex = 0.5, lty = 2, main = "Return on Assets Through Time", xlab = "Year", ylab = "ROA" )
plot(year, roe,col = "blue", type = "b", cex = 0.5, lty = 2, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )

#Lets look at all of the graphics together. To to this, we are going to normalize the data by its sample mean
par(mfrow=c(1,1))

plot(year, roa/mean(roa),col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "ROA, ROE and HHI",sub = "Normilized by Variable Mean", xlab = "Year", ylab = "ROA, ROE and HHI", ylim = c(-0.5,2) )
lines(year, roe/mean(roe),col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, hhi/mean(hhi),col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("topleft", c("ROA", "ROE", "HHI"), lty = c(2,2,2), col = c("red", "blue", "black"), pch = c(0,1,2), cex = 0.75)

par(mfrow=c(1,1))
plot(hhi, roe, col = "red", cex=.8, main= "ROE vs HHI" )
abline(lm(roe~hhi))
reg_5_roe_hhi <- lm(roe~hhi)
summary(reg_5_roe_hhi)
par(mfrow=c(1,1))
plot(hhi, roa, col = "blue", cex=.8, main= "ROA vs HHI" )
abline(lm(roa~hhi))
reg_5_roa_hhi <- lm(roa~hhi)
summary(reg_5_roa_hhi)

#Lets control for a common time trend

time_trend <- c(81:1)
detach(data_set)
data_set <- transform(data_set, trend = time_trend)
attach(data_set)

reg_6_roe_hhi <- lm(roe~hhi+ trend)
summary(reg_6_roe_hhi)

reg_6_roa_hhi <- lm(roa~hhi+ trend)
summary(reg_6_roa_hhi)

summary(lm(roa~ trend))


detach(data_set)


#Lets look at the composition of deposit base through time

rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- data_set[,c("year", "liab_total_deposits", "dep_indiv_corp","dep_us_govmt","dep_state_pol","dep_other" )]
data_set <- transform(data_set,dep_indiv_corp_rat = (dep_indiv_corp/liab_total_deposits)*100, dep_us_govmt_rat = (dep_us_govmt/liab_total_deposits)*100, dep_state_pol_rat = (dep_state_pol/liab_total_deposits)*100, dep_other_rat = (dep_other/liab_total)*100  )
attach(data_set)
par(mfrow = c(1,1))
plot(year, dep_indiv_corp_rat,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "Deposits by Type",sub = "(% of Total Deposits)", xlab = "Year", ylab = "Deposits", ylim = c(0,100))
lines(year, dep_us_govmt_rat,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Return on Equity Through Time", xlab = "Year", ylab = "ROE" )
lines(year, dep_state_pol_rat,col = "purple", type = "b", cex = 0.5, lty = 2,pch=3, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
lines(year, dep_other_rat,col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "HHI liabilities", xlab = "Year", ylab = "Money Market Funding" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")
legend("left", c("Indiv&Corp", "FedGovmt", "StateGovmt", "Other"), lty = c(2,2,2,2), col = c("red", "blue", "black", "purple"), pch = c(0,1,2,3), cex = 0.6)

detach(data_set)


rm(list = ls())
data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- data_set[,c("year", "liab_total_deposits", "dep_indiv_corp","dep_us_govmt","dep_state_pol","dep_other" )]
data_set <- transform(data_set,dep_indiv_corp_rat = (dep_indiv_corp/liab_total_deposits)*100, dep_us_govmt_rat = (dep_us_govmt/liab_total_deposits)*100, dep_state_pol_rat = (dep_state_pol/liab_total_deposits)*100, dep_other_rat = (dep_other/liab_total)*100  )
attach(data_set)
par(mfrow = c(2,2))
plot(year, dep_indiv_corp_rat,col = "red", type = "b", cex = 0.5, lty = 2,pch = 0 ,main = "Deposits Indiv & Corporations",sub = "(% of Total Deposits)", xlab = "Year", ylab = "Deposits")
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_us_govmt_rat,col = "blue", type = "b", cex = 0.5, lty = 2, pch=1, main = "Deposit Fed Gov",sub = "(% of Total Deposits)", xlab = "Year", ylab = "Deposits" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_state_pol_rat,col = "black", type = "b", cex = 0.5, lty = 2,pch=2, main = "Deposit State Gov",sub = "(% of Total Deposits)", xlab = "Year", ylab = "Deposits" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")

plot(year, dep_other_rat,col = "purple", type = "b", cex = 0.5, lty = 2,pch=3, main = "Other Deposits",sub = "(% of Total Deposits)", xlab = "Year", ylab = "Deposits" )
abline(v=c(2008), lwd=1.5, lty=2, col="gray")
abline(v=c(1973), lwd=1.5, lty=2, col="gray")
abline(v=c(1987), lwd=1.5, lty=2, col="gray")
abline(v=c(1989), lwd=1.5, lty=2, col="gray")
abline(v=c(1998), lwd=1.5, lty=2, col="gray")


detach(data_set)

data_set <- read.csv("consolidated_historical_com_bank_data_r.csv", header = TRUE)
data_set <- data_set[,c("year", "liab_total_deposits", "dep_indiv_corp","dep_us_govmt","dep_state_pol","dep_other" )]
data_set <- transform(data_set,dep_indiv_corp_rat = (dep_indiv_corp/liab_total_deposits)*100, dep_us_govmt_rat = (dep_us_govmt/liab_total_deposits)*100, dep_state_pol_rat = (dep_state_pol/liab_total_deposits)*100, dep_other_rat = (dep_other/liab_total)*100  )
attach(data_set)

a<- dep_indiv_corp_rat+ dep_us_govmt_rat + dep_other_rat + dep_state_pol_rat

plot(a)


qt(.005,59)
library(stats)

qchisq(0.95, 23)
