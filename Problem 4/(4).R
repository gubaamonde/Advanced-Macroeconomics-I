#Install Packages -----



#Library ----
library(readr)
library(readxl)
library(dplyr)

#Loading Data ----
Government <- read_excel("~/R/Macro Avançada I/Problema 4/Shares of gross domestic product Government consumption expenditures and gross investment.xls")
Fixed_invest_nonresid <- read_excel("~/R/Macro Avançada I/Problema 4/Shares of gross domestic product Gross private domestic investment Fixed investment Nonresidential .xls")
Fixed_invest_resid <- read_excel("~/R/Macro Avançada I/Problema 4/Shares of gross domestic product Gross private domestic investment Fixed investment Residential.xls")
Privete_domestic_invest <- read_excel("~/R/Macro Avançada I/Problema 4/Shares of gross domestic product Gross private domestic investment.xls")


#*Plot ----
plot(x = Government$observation_date, y= Government$A822RE1Q156NBEA, type ="l", ylim = c(0,30), col = 1, ylab ="Share of Gross Domestic Product", xlab = "Time")
lines(x = Fixed_invest_nonresid$observation_date,Fixed_invest_nonresid$A008RE1Q156NBEA, type = "l", col =2)
lines(x = Fixed_invest_resid$observation_date,Fixed_invest_resid$A011RE1Q156NBEA, type = "l", col =3)
lines(x = Privete_domestic_invest$observation_date,Privete_domestic_invest$A006RE1Q156NBEA, type = "l", col=4)
legend("topright",legend=c("Government Consumption and expenditure", "Fixed Invest Non Residential","Fixed Invest Residential", "Gross private domestic investment" ), col=1:4, lty=rep(1,4), ncol=1)












a