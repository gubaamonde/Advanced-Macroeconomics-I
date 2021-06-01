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
X3MONTH <- read_excel("~/R/Macro Avançada I/Problema 7/3MTB_MONTH.xls")
CPIMONTH <- read_excel("~/R/Macro Avançada I/Problema 7/CPI_MONTH.xls")
M2MONTH <- read_excel("~/R/Macro Avançada I/Problema 7/M2_MONTH.xls")
UNEMPMONTH <- read_excel("~/R/Macro Avançada I/Problema 7/UNEMP_MONTH.xls")
M2MONTHPERCENT <- read_excel("~/R/Macro Avançada I/Problema 7/M2PERCENT_MONTH.xls")



#Dates ----

CPI_curto_2 = ts(CPIMONTH$CPIAUCSL_PCH, start=c(1981,1),end=c(2020,12), frequency = 12)
UNEMP_curto_2 = ts(UNEMPMONTH$UNRATE, start=c(1981,1),end=c(2020,12), frequency = 12)
X3TB_curto_2 = ts(X3MONTH$DTB3, start=c(1981,1),end=c(2020,12), frequency = 12)
M2_curto_2 = ts(M2MONTH$M2, start=c(1981,1),end=c(2020,12), frequency = 12)
M2PERCENT_curto_2 = ts(M2MONTHPERCENT$M2_PCH, start=c(1981,1),end=c(2020,12), frequency = 12)


dummy_2 = matrix(0, nrow = 480, ncol= 1)
dummy_2[335] = 1 #2008 crisis

df_2 = as.data.frame(cbind(UNEMP = UNEMP_curto_2,
                         TB3 = X3TB_curto_2,
                         M2 = M2_curto_2,
                         CPI = CPI_curto_2,
                         dummy = dummy_2,
                         M2Percent = M2PERCENT_curto_2
                         
))

df_2$Date = seq(as.Date('1981-01-01'), as.Date('2020-12-01'), "month" )

#Reg ----


plot(x = df_2$M2Percent, y=df_2$CPI)
plot(x = df_2$M2, y = df_2$CPI)


reg1_2 = dynlm(CPI~ M2Percent, data =df_2)
summary(reg1_2)

reg2_2 = dynlm(CPI~ UNEMP + dummy, data = df_2)
summary(reg2_2)

reg3_2  =dynlm(CPI ~ M2  + TB3 + lag(UNEMP,1) +dummy, data  =df_2)
summary(reg3_2)

reg4_2 = dynlm(M2 ~ CPI +UNEMP +TB3 +dummy , data = df_2)
summary(reg4_2)

reg5_2 = dynlm(M2 ~ CPI +dummy , data = df_2)
summary(reg5_2)

reg6_3 = dynlm(CPI ~ M2 +dummy , data = df_2)
summary(reg6_3)

reg7_2 = dynlm(CPI ~ var(CPI),data = df_2)
summary(reg7_2)


#relação entre volatilidade incerteza e inflação
# por isso a regressão acima. 





