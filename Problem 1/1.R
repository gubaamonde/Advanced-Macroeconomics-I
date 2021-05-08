#Install Packages ----
install.packages("quantmod")





#Library ----
library(dplyr)
library(mFilter)
library(readr)
library(readxl)
library(ggplot2)





#Loading Data ----
CPI <- read_excel("~/R/Macro Avançada I/Problema 1/inflation.xls")
GPDI <- read_excel("~/R/Macro Avançada I/Problema 1/real gross private domestic investment.xls")
M2 <- read_excel("~/R/Macro Avançada I/Problema 1/real M2 money.xls")
CONSUMP_EXPEND_PC <- read_excel("~/R/Macro Avançada I/Problema 1/real personal consumption expenditure per capita.xls")
CREDIT_NONFINANCIAL <- read_excel("~/R/Macro Avançada I/Problema 1/total credit to private non-financial sector .xls")
RGDP_PC <- read_excel("~/R/Macro Avançada I/Problema 1/US Real GDP per capita.xls")
UNEMP <- read_excel("~/R/Macro Avançada I/Problema 1/civilian unemployment rate.xls")
HOURS_WORKED <- read_excel("~/R/Macro Avançada I/Problema 1/hours worked.xls")




#Data ----
#* CPI ----

CPI_curto = CPI$CPIAUCSL[517:891]

CPI.hp1 = hpfilter(CPI_curto, freq  = 1600)
CPI.hp2 = hpfilter(CPI_curto, freq  = 0)
CPI.hp3 = hpfilter(CPI_curto, freq  = 1000000000)

xx.CPI = seq(as.Date('1990-01-01'), as.Date('2021-03-01'), "month" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x = xx.CPI, y = CPI_curto ,main="Hodrick-Prescott filter of CPI: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.CPI, y = CPI.hp1$trend, col = 2)
lines(x = xx.CPI, y =CPI.hp2$trend,col=3)
lines(x = xx.CPI, y =CPI.hp3$trend,col=4)
legend("topleft",legend=c("CPI", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 1000000000))), col=1:4, lty=rep(1,4), ncol=1)

plot(x = xx.CPI, y=CPI.hp1$cycle,main="Hodrick-Prescott filter of CPI: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.CPI, y=CPI.hp2$cycle, col = 3)
lines(x = xx.CPI, y=CPI.hp3$cycle,col=4)
legend("topleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 1000000000))), col=2:4, lty=rep(1,4), ncol=1)





#* Unemployment ----

UNEMP_curto = UNEMP$UNRATE[517:879]

UNEMP.hp1 = hpfilter(UNEMP_curto, freq  = 1600)
UNEMP.hp2 = hpfilter(UNEMP_curto, freq  = 1)
UNEMP.hp3 = hpfilter(UNEMP_curto, freq  = 10000)

xx.UNEMP = seq(as.Date('1991-01-01'), as.Date('2021-03-01'), "month" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.UNEMP, y =  UNEMP_curto ,main="Hodrick-Prescott filter of Unemployment: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.UNEMP, y =  UNEMP.hp1$trend, col = 2)
lines(x = xx.UNEMP, y =  UNEMP.hp2$trend,col=3)
lines(x = xx.UNEMP, y =  UNEMP.hp3$trend,col=4)
legend("topleft",legend=c("UNEMPLOYMENT",expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1,  cex = 1, pt.cex = 1)

plot( x = xx.UNEMP, y= UNEMP.hp1$cycle,main="Hodrick-Prescott filter of Unemployment: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.UNEMP,y= UNEMP.hp2$cycle, col = 3)
lines(x = xx.UNEMP,y= UNEMP.hp3$cycle,col=4)
legend("topleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1)





#* GPDI ----

GPDI_curto = log(GPDI$GPDIC1[1:297])

GPDI.hp1 = hpfilter(GPDI_curto, freq  = 1600)
GPDI.hp2 = hpfilter(GPDI_curto, freq  = 1)
GPDI.hp3 = hpfilter(GPDI_curto, freq  = 10000)

xx.GPDI = seq(as.Date('1947-01-01'), as.Date('2021-01-01'), "quarter" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.GPDI, y =  GPDI_curto ,main="Hodrick-Prescott filter of Real Gross Private Domestic Investment: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.GPDI, y =  GPDI.hp1$trend, col = 2)
lines(x = xx.GPDI, y =  GPDI.hp2$trend,col=3)
lines(x = xx.GPDI, y =  GPDI.hp3$trend,col=4)
legend("topleft",legend=c("GPDI",expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1,  cex = 1, pt.cex = 1)

plot( x = xx.GPDI, y= GPDI.hp1$cycle,main="Hodrick-Prescott filter of Real Gross Private Domestic Investment: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.GPDI,y=  GPDI.hp2$cycle, col = 3)
lines(x = xx.GPDI,y=  GPDI.hp3$cycle,col=4)
legend("topright",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1,  cex = 1, pt.cex = 1)





#* M2 ----

M2_curto = log(M2$M2REAL[373:747])

M2.hp1 = hpfilter(M2_curto, freq  = 1600)
M2.hp2 = hpfilter(M2_curto, freq  = 1)
M2.hp3 = hpfilter(M2_curto, freq  = 10000)

xx.M2 = seq(as.Date('1990-01-01'), as.Date('2021-03-01'), "month" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.M2, y =  M2_curto ,main="Hodrick-Prescott filter of M2 Money Stock: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.M2, y =  M2.hp1$trend, col = 2)
lines(x = xx.M2, y =  M2.hp2$trend,col=3)
lines(x = xx.M2, y =  M2.hp3$trend,col=4)
legend("topleft",legend=c("M2", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1)

plot( x = xx.M2, y= M2.hp1$cycle,main="Hodrick-Prescott filter of M2 Money Stock: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.M2,y=  M2.hp2$cycle, col = 3)
lines(x = xx.M2,y=  M2.hp3$cycle,col=4)
legend("topleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1)





#* CONSUMP_EXPEND_PC ----

CONSUMP_EXPEND_PC_curto = log(CONSUMP_EXPEND_PC$A794RX0Q048SBEA[1:297])

CONSUMP_EXPEND_PC.hp1 = hpfilter(CONSUMP_EXPEND_PC_curto, freq  = 1600)
CONSUMP_EXPEND_PC.hp2 = hpfilter(CONSUMP_EXPEND_PC_curto, freq  = 1)
CONSUMP_EXPEND_PC.hp3 = hpfilter(CONSUMP_EXPEND_PC_curto, freq  = 10000)

xx.CONSUMP_EXPEND_PC = seq(as.Date('1947-01-01'), as.Date('2021-01-01'), "quarter" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.CONSUMP_EXPEND_PC, y =  CONSUMP_EXPEND_PC_curto ,main="Hodrick-Prescott filter of Real Personal Consumption Expenditure per capita: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.CONSUMP_EXPEND_PC, y =  CONSUMP_EXPEND_PC.hp1$trend, col = 2)
lines(x = xx.CONSUMP_EXPEND_PC, y =  CONSUMP_EXPEND_PC.hp2$trend,col=3)
lines(x = xx.CONSUMP_EXPEND_PC, y =  CONSUMP_EXPEND_PC.hp3$trend,col=4)
legend("topleft",legend=c("Real Personal Consumption Expenditure PC", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1,  cex = 0.75, pt.cex = 1)

plot( x = xx.CONSUMP_EXPEND_PC, y= CONSUMP_EXPEND_PC.hp1$cycle,main="Hodrick-Prescott filter of Real Personal Consumption Expenditure per capita: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.CONSUMP_EXPEND_PC,y=  CONSUMP_EXPEND_PC.hp2$cycle, col = 3)
lines(x = xx.CONSUMP_EXPEND_PC,y=  CONSUMP_EXPEND_PC.hp3$cycle,col=4)
legend("bottomleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1)





#* CREDIT_NONFINANCIAL ----

CREDIT_NONFINANCIAL_curto = log(CREDIT_NONFINANCIAL$QUSPAMUSDA[98:300])

CREDIT_NONFINANCIAL.hp1 = hpfilter(CREDIT_NONFINANCIAL_curto, freq  = 1600)
CREDIT_NONFINANCIAL.hp2 = hpfilter(CREDIT_NONFINANCIAL_curto, freq  = 1)
CREDIT_NONFINANCIAL.hp3 = hpfilter(CREDIT_NONFINANCIAL_curto, freq  = 10000)

xx.CREDIT_NONFINANCIAL = seq(as.Date('1970-01-01'), as.Date('2020-07-01'), "quarter" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.CREDIT_NONFINANCIAL, y =  CREDIT_NONFINANCIAL_curto ,main="Hodrick-Prescott filter of Total Credit to Private Non-financial Sector: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.CREDIT_NONFINANCIAL, y =  CREDIT_NONFINANCIAL.hp1$trend, col = 2)
lines(x = xx.CREDIT_NONFINANCIAL, y =  CREDIT_NONFINANCIAL.hp2$trend,col=3)
lines(x = xx.CREDIT_NONFINANCIAL, y =  CREDIT_NONFINANCIAL.hp3$trend,col=4)
legend("topleft",legend=c("Total Credit to Private Non-financial Sector", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1, cex = 0.75)

plot( x = xx.CREDIT_NONFINANCIAL, y= CREDIT_NONFINANCIAL.hp1$cycle,main="Hodrick-Prescott filter of Total Credit to Private Non-financial Sector: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.CREDIT_NONFINANCIAL,y=  CREDIT_NONFINANCIAL.hp2$cycle, col = 3)
lines(x = xx.CREDIT_NONFINANCIAL,y=  CREDIT_NONFINANCIAL.hp3$cycle,col=4)
legend("topleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1)





#* RGDP_PC ----

RGDP_PC_curto = log(RGDP_PC$A939RX0Q048SBEA[93:297])

RGDP_PC.hp1 = hpfilter(RGDP_PC_curto, freq  = 1600)
RGDP_PC.hp2 = hpfilter(RGDP_PC_curto, freq  = 1)
RGDP_PC.hp3 = hpfilter(RGDP_PC_curto, freq  = 10000)

xx.RGDP_PC = seq(as.Date('1970-01-01'), as.Date('2021-01-01'), "quarter" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.RGDP_PC, y =  RGDP_PC_curto ,main="Hodrick-Prescott filter of Real US GDP per capita: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.RGDP_PC, y =  RGDP_PC.hp1$trend, col = 2)
lines(x = xx.RGDP_PC, y =  RGDP_PC.hp2$trend,col=3)
lines(x = xx.RGDP_PC, y =  RGDP_PC.hp3$trend,col=4)
legend("topleft",legend=c("Real GDP PC", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=1:4, lty=rep(1,4), ncol=1)

plot( x = xx.RGDP_PC, y= RGDP_PC.hp1$cycle,main="Hodrick-Prescott filter of Real US GDP per capita: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.RGDP_PC,y=  RGDP_PC.hp2$cycle, col = 3)
lines(x = xx.RGDP_PC,y=  RGDP_PC.hp3$cycle,col=4)
legend("bottomleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 1)),expression(paste(lambda, " = ", 10000))), col=2:4, lty=rep(1,4), ncol=1)





#* HOURS_WORKED ----

HOURS_WORKED_curto = log(HOURS_WORKED$AWHNONAG[313:687])

HOURS_WORKED.hp1 = hpfilter(HOURS_WORKED_curto, freq  = 1600)
HOURS_WORKED.hp2 = hpfilter(HOURS_WORKED_curto, freq  = 1)
HOURS_WORKED.hp3 = hpfilter(HOURS_WORKED_curto, freq  = 100000000)

xx.HOURS_WORKED= seq(as.Date('1990-01-01'), as.Date('2021-03-01'), "month" )

par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.HOURS_WORKED, y =  HOURS_WORKED_curto ,main="Hodrick-Prescott filter of  Hours Worked: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.HOURS_WORKED, y =  HOURS_WORKED.hp1$trend, col = 2)
lines(x = xx.HOURS_WORKED, y =  HOURS_WORKED.hp2$trend,col=3)
lines(x = xx.HOURS_WORKED, y =  HOURS_WORKED.hp3$trend,col=4)
legend("bottomleft",legend=c("Hours Worked", expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 100000000))), col=1:4, lty=rep(1,4), ncol=1)

plot( x = xx.HOURS_WORKED, y= HOURS_WORKED.hp1$cycle,main="Hodrick-Prescott filter of Hours Worked: Cycle",col=2, ylab="", type ="l", xlab ="")
lines(x = xx.HOURS_WORKED,y=  HOURS_WORKED.hp2$cycle, col = 3)
lines(x = xx.HOURS_WORKED,y=  HOURS_WORKED.hp3$cycle,col=4)
legend("bottomleft",legend=c(expression(paste(lambda, " = ", 1600)), expression(paste(lambda, " = ", 0)),expression(paste(lambda, " = ", 100000000))), col=2:4, lty=rep(1,4), ncol=1)











