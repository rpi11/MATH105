library(MASS)
library(leaps)
library(tidyverse)

coffee <- read.csv("merged_data_cleaned.csv", header=TRUE)

coffeeFilt5 <- na.omit(dplyr::select(coffee, Species, Country.of.Origin,
                                    Region, Harvest.Year, Variety, Processing.Method,
                                    Aroma, Flavor, Aftertaste, Acidity, Body,
                                    Balance, Uniformity, Clean.Cup, Sweetness,
                                    Cupper.Points, Total.Cup.Points, altitude_mean_meters))

coffeeFilt6 <- coffeeFilt5

for(i in seq(1,nrow(coffeeFilt5))){
  if(grepl("United States",coffeeFilt5[i,2])){
    coffeeFilt5[i,2] <- "United States"
  }
  else if(grepl("Tanzania",coffeeFilt5[i,2])){
    coffeeFilt5[i,2] <- "Tanzania"
  }
  else if(grepl("Cote",coffeeFilt5[i,2])){
    coffeeFilt5[i,2] <- "Cote d'Ivoire"
  }
  
  if(grepl("March",coffeeFilt5[i,4])){
    coffeeFilt5[i,4] <- "2010"
  }
  else if(grepl("Colombia",coffeeFilt5[i,4])){
    coffeeFilt5[i,4] <- "2011"
  }
  
  if(grepl("crop",coffeeFilt5[i,4]) || grepl("Through",coffeeFilt5[i,4]) ||
     grepl("to",coffeeFilt5[i,4]) || grepl("y-A",coffeeFilt5[i,4]) ||
     coffeeFilt5[i,4] == "Mayo a Julio"|| coffeeFilt5[i,4] == "Abril - Julio"||
     coffeeFilt5[i,4] == ""|| is.na(coffeeFilt5[i,4])){
    coffeeFilt5 <- coffeeFilt5[-i,]
  }
  
  if(grepl("7",coffeeFilt5[i,4]) || grepl("23 July",coffeeFilt5[i,4]) ||
     grepl("- April",coffeeFilt5[i,4]) || grepl("- 20",coffeeFilt5[i,4]) ||
     grepl("-20",coffeeFilt5[i,4]) || grepl("Fall 2",coffeeFilt5[i,4]) ||
     grepl("January",coffeeFilt5[i,4]) || grepl("-Mar",coffeeFilt5[i,4]) ||
     grepl("/",coffeeFilt5[i,4]) && coffeeFilt5[i,4] != "2017"){
    coffeeFilt5[i,4] <- paste("20", substr(coffeeFilt5[i,4],
                                          nchar(coffeeFilt5[i,4])-1,
                                          nchar(coffeeFilt5[i,4])),sep = "")
  }
  
}

coffeeFilt5[,4] <-  strtoi(coffeeFilt5[,4])

coffeeFilt4 <- filter(coffeeFilt5, Processing.Method=="Washed / Wet"|
                       Processing.Method=="Natural / Dry"|
                       Processing.Method=="Pulped natural / honey"|
                       Processing.Method=="Semi-washed / Semi-pulped")

coffeeFilt3 <- filter(coffeeFilt4, 
       altitude_mean_meters < 5000)

coffeeFilt <- mutate(coffeeFilt3,
                     Process.Factor=ifelse(Processing.Method=="Natural / Dry", 0,
                                           ifelse(Processing.Method=="Pulped natural / honey", 1,
                                                  ifelse(Processing.Method=="Semi-washed / Semi-pulped",2,3))))


names(coffeeFilt[7:17])
c.null = lm(Body~1, data=coffeeFilt)
c.full = lm(Body ~ Harvest.Year + altitude_mean_meters +
              factor(Process.Factor) + factor(Species), data=coffeeFilt)
c.stepfwd = stepAIC(c.null, scope=list(lower=c.null, upper=c.full),
                    direction="forward")
summary(c.stepfwd)


cupperPointsCalc <- function(year, alt, proc){
  beta0=-4.357e+01
  beta1=2.531e-02
  beta2=1.559e-04
  if(proc == "Washed / Wet"){
    beta3=-1.770e-01
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

sweetnessCalc <- function(year, alt, proc, spec){
  beta0=0
  beta1=0
  beta2=0
  if(proc == "Washed / Wet"){
    beta3=0
  }
  else{
    beta3=0
  }
  
  if(spec=="Robusta"){
    beta4=10-2.212400
  }
  else{
    beta4=10
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3+beta4)
}

cleanCupCalc <- function(year, alt, proc){
  beta0=-9.005e+01
  beta1=4.955e-02
  beta2=8.658e-05
  if(proc == "Washed / Wet"){
    beta3=0
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

uniformityCalc <- function(year, alt, proc){
  beta0=-3.689e+01
  beta1=2.319e-02
  beta2=5.329e-05
  if(proc == "Washed / Wet"){
    beta3=0
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

balanceCalc <- function(year, alt, proc){
  beta0=-3.216e+01
  beta1=1.966e-02
  beta2=1.131e-04
  if(proc == "Washed / Wet"){
    beta3=-1.166e-01
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

bodyCalc <- function(year, alt, proc){
  beta0=-2.097e+01
  beta1=1.412e-02
  beta2=1.027e-04
  if(proc == "Washed / Wet"){
    beta3=-1.167e-01
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

acidityCalc <- function(year, alt, proc){
  beta0=-1.480e+01
  beta1=1.102e-02
  beta2=1.411e-04
  if(proc == "Washed / Wet"){
    beta3=-5.717e-02
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

aftertasteCalc <- function(year, alt, proc){
  beta0=-2.693e+01
  beta1=1.700e-02
  beta2=1.262e-04
  if(proc == "Washed / Wet"){
    beta3=-1.136e-01
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

flavorCalc <- function(year, alt, proc){
  beta0=-2.428e+01
  beta1=1.575e-02
  beta2=1.375e-04
  if(proc == "Washed / Wet"){
    beta3=-1.431e-01
  }
  else{
    beta3=0
  }
  
  return(beta0+(beta1*year)+(beta2*alt)+beta3)
}

aromaCalc <- function(year, alt, proc){
  beta0=7.4603071
  beta1=0.0001261
  if(proc == "Washed / Wet"){
    beta2=-0.0762717
  }
  else{
    beta2=0
  }
  
  return(beta0+(beta1*alt)+(beta2))
}

