library(tidyverse)

data <- na.omit(read.csv("WDIData_FORMAT2.csv", header=TRUE, sep="|"))
countries <- read.csv("countries.csv", header=FALSE)
names(countries) = "Countries"

em_categories <- read.csv("emission_categ.csv", header=TRUE)
qol_categories <- read.csv("QOL_categ.csv", header=TRUE)
all_categories <- full_join(em_categories,qol_categories,by="Category")

em_data <- filter(data, Category %in% em_categories$Category)
qol_data <- filter(data, Category %in% qol_categories$Category)

em_categ <- em_categories[,1]
qol_categ <- qol_categories[,1]
countr <- countries[,1]
