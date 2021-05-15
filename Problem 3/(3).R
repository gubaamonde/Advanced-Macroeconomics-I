#Install Packages ----
install.packages("quantmod")





#Library ----
library(dplyr)
library(mFilter)
library(readr)
library(readxl)
library(ggplot2)





#Loading Data ----
VIX <- read_excel("~/R/Macro Avançada I/Problema 3/VIXCLS.xls")
GDPPC <- read_excel("~/R/Macro Avançada I/Problema 3/US Real GDP per capita.xls")





##GDP ------
GDP_curto = GDPPC[173:297,]
ln_gdp = log(GDP_curto$A939RX0Q048SBEA)
RGDP_PC.hp1  = hpfilter(ln_gdp, freq  = 1600)
xx.RGDP_PC = seq(as.Date('1990-01-01'), as.Date('2021-01-01'), "quarter" )

##*HP Filter GDP per capita
par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(x =  xx.RGDP_PC, y =  ln_gdp ,main="Hodrick-Prescott filter of Real US GDP per capita: Trend",col=1, ylab="", type ="l", xlab ="")
lines(x = xx.RGDP_PC, y =  RGDP_PC.hp1$trend, col = 2)
legend("topleft",legend=c("Real GDP PC", expression(paste(lambda, " = ", 1600))), col=1:4, lty=rep(1,4), ncol=1)

plot( x = xx.RGDP_PC, y= RGDP_PC.hp1$cycle,main="Hodrick-Prescott filter of Real US GDP per capita: Cycle",col=2, ylab="", type ="l", xlab ="")
legend("bottomleft",legend=c(expression(paste(lambda, " = ", 1600))), col=2:4, lty=rep(1,4), ncol=1)





##VIX ----
VIX_curto = VIX[1:125,]





##Plot ----

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
} #Reset par()

par(resetPar())
par(mar = c(5, 4, 4, 4) + 0.3)              
plot(VIX_curto, pch = 16, col = 2, type = "l", xlab = "Time", ylab = "VIX")              
par(new = TRUE)                             
plot(RGDP_PC.hp1$cycle, pch = 17, col = 3,              
     axes = FALSE, xlab = "", ylab = "", type = "l")
axis(side = 4, at = pretty(range(RGDP_PC.hp1$cycle)))      
mtext("Cycle Log GDP per Capita", side = 4, line = 3)             











