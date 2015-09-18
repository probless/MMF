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



#In order to do our comparisons, we need to construct our variables of profitability, return on equity and return on assets  Net interest margin, non interest income

data_base <- transform(data_base, roe =( (noij*200)/eqtot), roa =((noij*200)/asset), net_interest_margin = (nim*200/asset), non_interest_income = (nonii*200)/asset, interest_income = (intinc*200)/asset)

#Now we are going to trim the data set in order to eliminate outliers of both ROA and ROE and NIM
attach(data_base) 
data_base <- data_base[which(roe < mean(roe) + 3*sd(roe) & roe > mean(roe) - 3*sd(roe)  ),] 
detach(data_base)
attach(data_base)
data_base <- data_base[which(roa < mean(roa) + 3*sd(roa) & roa > mean(roa) - 3*sd(roa)  ),]
detach(data_base)
attach(data_base)
data_base <- data_base[which(net_interest_margin < mean(net_interest_margin) + 3*sd(net_interest_margin) & net_interest_margin > mean(net_interest_margin) - 3*sd(net_interest_margin)  ),]
detach(data_base)
attach(data_base)
data_base <- data_base[which(non_interest_income < mean(non_interest_income) + 3*sd(non_interest_income) & non_interest_income > mean(non_interest_income) - 3*sd(non_interest_income)  ),]
detach(data_base)
attach(data_base)
data_base <- data_base[which(interest_income < mean(interest_income) + 3*sd(interest_income) & interest_income > mean(interest_income) - 3*sd(interest_income)  ),]
detach(data_base)



#Looking at the of roe and roa and Net Interets MArgin
attach(data_base)
summary(roe)
hist(roe, breaks = 200)

plot(density(roe))

detach(data_base)


attach(data_base)
summary(roa)
hist(roa, breaks = 200)

plot(density(roa))

detach(data_base)

attach(data_base)
summary(net_interest_margin)
hist(net_interest_margin, breaks = 200)
plot(density(net_interest_margin))
detach(data_base)

attach(data_base)
summary(non_interest_income)
hist(non_interest_income, breaks = 200)
plot(density(non_interest_income))
detach(data_base)

attach(data_base)
summary(interest_income)
hist(interest_income, breaks = 200)
plot(density(interest_income))
detach(data_base)




#Lets analyze the relationship between noniterest income by banks and roa

attach(data_base)
plot(non_interest_income,roa, col = "red", cex = 0.8, main = "Return on Assets vs Non-Interest Income", xlab = "Non-interest Income", ylab = "ROA" )
abline(lm(roa~non_interest_income))
detach(data_base)


#Lets analyize the relationship between interest income by banks and roa

attach(data_base)
plot(interest_income,roa, col = "blue", cex = 0.8, main = "Return on Assets vs Interest Income", xlab = "Interest Income", ylab = "ROA" )
abline(lm(roa~interest_income))
detach(data_base)

attach(data_base)
data_base <- transform(data_base, rel_income = interest_income/(non_interest_income+1))
detach(data_base)

attach(data_base)
data_base <- data_base[which(rel_income < mean(rel_income) + 3*sd(rel_income) & rel_income > mean(rel_income) - 3*sd(rel_income)  ),]
detach(data_base)


attach(data_base)
summary(rel_income)
hist(rel_income, breaks = 200)
plot(density(rel_income))
detach(data_base)

attach(data_base)
plot(rel_income,roa, col = "pink", cex = 0.8, main = "Return on Assets vs Relative Income ",sub = "Interes / NonInterest Income", xlab = "Interest Income", ylab = "ROA" )
abline(lm(roa~rel_income))
detach(data_base)


attach(data_base)
summary(lm(roa~ non_interest_income + interest_income))
detach(data_base)
#Now we are going to construct the appropriate measurues of diversification.

data_base_2 <- subset(data_base, select = c (roa, roe, net_interest_margin,interest_income, non_interest_income, cert, dep, frepp,idobrmtg,   subnd, idoliab, tradel, asset))

rm(data_base)
summary(data_base_2)
data_base_2 <- transform(data_base_2, total_liab = dep + frepp +  idobrmtg + subnd + idoliab + tradel)
data_base_2 <- transform(data_base_2, hhi = (dep/total_liab)^2 + (frepp/total_liab)^2 +  (idobrmtg/total_liab)^2 + (subnd/total_liab)^2 + (idoliab/total_liab)^2 + (tradel/total_liab)^2 )

attach(data_base_2)
summary(hhi)
hist(hhi, breaks =300, main = "Histogram of HHI index of concentration in Liabilities" )
detach(data_base_2)

#Lets analyze at a first glance the relationship between profitability as measured by ROE and ROA and hhi.

attach(data_base_2)
plot (hhi, roe, col = "red", cex =0.75, main = "HHi vs ROE", xlab = "HHI", ylab = "ROE")
abline(lm(roe~ hhi))
summary(lm(roe~ hhi))
detach(data_base_2)

attach(data_base_2)
plot (hhi, roa, col = "blue", cex =0.75, main = "HHi vs ROA", xlab = "HHI", ylab = "ROA")
abline(lm(roa~ hhi))
summary(lm(roa~ hhi))
detach(data_base_2)

data_base_2 <- transform(data_base_2, mmf = (frepp/total_liab)*100)

attach(data_base_2)
plot (mmf, roe, col = "red", cex =0.75, main = "MMF vs ROE", xlab = "MMF", ylab = "ROE")
abline(lm(roe~ mmf))
summary(lm(roe~ mmf))
detach(data_base_2)

attach(data_base_2)
plot (mmf, roa, col = "blue", cex =0.75, main = "MMF vs ROA", xlab = "MMF", ylab = "ROA")
abline(lm(roa~ mmf))
summary(lm(roa~ mmf))
detach(data_base_2)

attach(data_base_2)
summary(lm(roe ~ mmf + hhi))
detach(data_base_2)

attach(data_base_2)
summary(lm(roa ~ mmf + hhi))
detach(data_base_2)

attach(data_base_2)
summary(asset)
mean(hhi[which(asset > median(asset))])
mean(hhi[which(asset < median(asset))])
detach(data_base_2)

attach(data_base_2)
mean(roe[which(asset > median(asset))])
mean(roe[which(asset < median(asset))])
detach(data_base_2)

#We are going to create a variable of size that ranks banks in terms of assets by deciles
attach(data_base_2)
size <- c(1:length(asset))
for(i in 1:length(asset)){
if(asset[i]<= quantile(asset, probs = .1) ){
  size[i] <- 0}
  else if(asset[i]<= quantile(asset, probs = .2) & asset[i]> quantile(asset, probs = .1)  ){
      size[i] <- 1}
      else if(asset[i]<= quantile(asset, probs = .3) & asset[i]> quantile(asset, probs = .2)  ){
          size[i] <- 2}
      else if(asset[i]<= quantile(asset, probs = .4) & asset[i]> quantile(asset, probs = .3)  ){
          size[i] <- 3}
          else if(asset[i]<= quantile(asset, probs = .5) & asset[i]> quantile(asset, probs = .4)){
              size[i] <- 4}
              else if(asset[i]<= quantile(asset, probs = .6) & asset[i]> quantile(asset, probs = .5)){
                  size[i] <- 5}
                  else if(asset[i]<= quantile(asset, probs = .7) & asset[i]> quantile(asset, probs = .6)){
                      size[i] <- 6}
                      else if(asset[i]<= quantile(asset, probs = .8) & asset[i]> quantile(asset, probs = .7)){
                          size[i] <- 7}
                          else if(asset[i]<= quantile(asset, probs = .9) & asset[i]> quantile(asset, probs = .8)){
                              size[i] <- 8}
                              else{
                                size[i] <- 9
                              }
}
detach(data_base_2)
data_base_2 <- cbind(data_base_2, size)
rm(size, i)

#Running the regression of roe vs hhi and size
attach(data_base_2)
summary(lm(roe~ hhi + size))
detach(data_base_2)

attach(data_base_2)
summary(lm(roa~ hhi + size))
detach(data_base_2)

attach(data_base_2)
summary(lm(roe~ hhi + size + mmf))
detach(data_base_2)

attach(data_base_2)
summary(lm(hhi~ size ))
detach(data_base_2)

attach(data_base_2)
summary(lm(roe~ size +  mmf ))
detach(data_base_2)

