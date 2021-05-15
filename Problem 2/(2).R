#Install Packages ----





#Library ----
library(readr)
library(readxl)
library("urca")


#Loading Data ----
OUTPUT_HOUR <- read_excel("~/R/Macro Avançada I/Problema 1/Output per person.xls")
RGDP_PC <- read_excel("~/R/Macro Avançada I/Problema 1/US Real GDP per capita.xls")

#Plot ----
gdp = RGDP_PC[2:297,]

plot(x  = gdp$observation_date,y=  gdp$A939RX0Q048SBEA, type ="l", ylab = "", col ="blue", xlab = "Time")
par(new = TRUE)
plot(x  = OUTPUT_HOUR$observation_date,y =OUTPUT_HOUR$OPHPBS, type = "l", xaxt = "n", yaxt = "n",ylab = "", xlab = "", col="red")
legend("topleft", c("GDP", "OUTPUT"),col = c("blue", "red"), lty = c(1, 1))


#Conclusão: Aumento do PIB per capita é muito influenciado por produtividade

 