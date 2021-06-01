#Install Packages ----



#Library ----
library(dynlm)
library(dplyr)
library(readr)
library(readxl)
library(tseries)
library(zoo)
library(lmtest)
library(mFilter)
library(writexl)



#Loading Data ----
X3_month_Treasury_Bill <- read_excel("~/R/Macro Avançada I/Problema 7/3MTB_QUARTER.xls")
CPI <- read_excel("~/R/Macro Avançada I/Problema 7/CPI_QUARTER.xls")
M2 <- read_excel("~/R/Macro Avançada I/Problema 7/M2_QUARTER.xls")
UNEMP <- read_excel("~/R/Macro Avançada I/Problema 7/UNEMP_QUARTER.xls")
GDPPC <- read_excel("~/R/Macro Avançada I/Problema 7/US Real GDP per capita.xls")
M2PERCENT <- read_excel("~/R/Macro Avançada I/Problema 7/M2PERCENT_QUARTER.xls")



#Dates ----

CPI_curto = ts(CPI$CPIAUCSL_PCH[136:296], start=c(1981,1),end=c(2021,01), frequency = 4)
GDP_curto = ts(GDPPC$A939RX0Q048SBEA[137:297], start=c(1981,1),end=c(2021,01), frequency = 4)
UNEMP_curto = ts(UNEMP$UNRATE[133:293], start=c(1981,1),end=c(2021,01), frequency = 4)
X3TB_curto = ts(X3_month_Treasury_Bill$DTB3[109:269], start=c(1981,1),end=c(2021,01), frequency = 4)
M2_curto = ts(M2$WM2NS, start=c(1981,1),end=c(2021,01), frequency = 4)
M2PERCENT_curto = ts(M2PERCENT$WM2NS_PCH, start=c(1981,1),end=c(2021,01), frequency = 4)


dummy = matrix(0, nrow = 161, ncol= 1)
dummy[112] = 1 #2008 crisis

df = as.data.frame(cbind(GDP_PC = GDP_curto, 
                          UNEMP = UNEMP_curto,
                          TB3 = X3TB_curto,
                          M2 = M2_curto,
                          CPI = CPI_curto,
                         dummy = dummy,
                         M2PERCENT = M2PERCENT_curto
                    
))


rownames(df) = xx.dates

#Reg ----
par(mfrow=c(1,2))
plot.ts(df$UNEMP)
plot.ts(df$CPI)


resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
} #Reset par()

par(resetPar())

plot(x=df$UNEMP, y = df$CPI)
plot(x = df$M2PERCENT, y = df$CPI)


reg1 = dynlm(CPI~ UNEMP +dummy, data = df)
summary(reg1)

reg2  =dynlm(CPI ~ M2  + TB3 + lag(UNEMP,1) +dummy , data  =df)
summary(reg2)

reg3 = dynlm(GDP_PC~ M2PERCENT, data = df )
summary(reg3)

reg4 = dynlm(CPI ~ M2PERCENT, data = df)
summary(reg4)

















