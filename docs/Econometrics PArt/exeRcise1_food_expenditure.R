rm(list = ls())
# if you have not installed the packages dplyr, magrittr,lattice, MASS, memisc, stargazer
# install.packages("PACKAGENAME")
# otherwise: Loading of several packages we work with


library(dplyr)
library(magrittr)
library(lattice)
library(MASS)
library(memisc)
library(stargazer)
library(lmtest) 

getwd() # this is the folder where the data (csv file) should be saved. If you want to change it, use the setwd() command
# for example: setwd("C:/Users/Benedikt/Dropbox/County Complexity")
# Loading of the dataset (csv for English, csv2 for German excel files; 

data <- read.csv2(file = "food2.csv", header = TRUE)

# first overview over the data
data %>% names()
names(data)
dim(data)#how many observations

# recoding and labelling of the variables 
data$food_exp = data$V1
data$income   <- data$V2

#exploration and summary food_exp
data$food_exp
data$food_exp %>% summary()
data$food_exp %>% boxplot()

colors() # colors available to your disposale through the installed packages. 
data$food_exp %>% hist (main = "What's on top",
                        col = "chocolate3",
                        xlab = "What's on the bottom",
                        ylab = "What's written on the side")
hist(data$food_exp)

#exploration and summary income
data$income
data$income %>% summary()
data$income %>% boxplot()

data$income %>% hist (main = "Histogram Income",
                        col = "turquoise3",
                        xlab = "Income",
                        ylab = "Häufigkeit")
mean(data$income)
median(data$income) # =20.03 --> 2.000USD p.Week - this is way above the US Median income of 60.000USD (2015)

# Scatterplot
plot(x = data$income ,
     y= data$food_exp,
     main = "Scatterplot",  #Whats written at the top
     xlab = "Income",      #Whats written on the bottem
     ylab = "Food Expenditure",     #Whats written on the side
     ylim = c(0,600),    #Scala y-lim
     col  = rainbow(40)   #color rainbow with 40 shades
     )

rainbow(40)
colors()



# linear OLS-regression
# ~ for MAC (ALT N) ~ for Windows (ALTGR +)
reg <- lm (food_exp ~ income , data=data) # Calculating the OLS-Regression Model
reg
summary(reg)
abline(reg,col = "red") #  A ONE UNIT INCREASE IN INCOME (which is 100 Dollars) speaks 
# c.p., on average and approximately to a 10Dollar increase in food expenditures 

resid(reg)
stargazer(reg, 
          type ="text",
          covariate.labels = c("income","Intercept"),
          dep.var.labels = "food expenditures")


# Two options of exporting
mtable(... = reg)               # 1 | memisc-package by ZU-professor Martin Elff
stargazer(reg,type = "text")    # 2 | stargazer-package



resid(reg)                      # List of residuals
plot(density(resid(reg)))       # A density plot
qqnorm(resid(reg))              # A quantile normal plot - good for checking normality
qqline(resid(reg))

# predicting the values vs. observed
predict(reg)
data$reg_predict <- predict(reg)

# Predicted Values vs. Residuals
plot(x=data$reg_predict, 
     y = resid(reg),
     main="Plot",
     xlab = "Fitted Values Income",
     ylab = "Residuals")
# heteroscedasticity!
bptest(reg)# test for heteroscedasticity

# a cool first glance at your regression through plotting the reg 
par(mfrow=c(2,2)) # View 4 charts at the same time
plot(reg)

# on Cooks distance: identifying influential points: the change in the regression model if one observation is excluded


