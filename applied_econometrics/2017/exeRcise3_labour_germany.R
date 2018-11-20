# Working Directory 
rm(list=ls())
getwd()
setwd()

# Loading of several packages we work with

library(dplyr)
library(magrittr)
library(memisc)
library(stargazer)




#No 1.1
data <- read.csv2(file = "labour_germany2.csv",header = TRUE)

#overview over the data
names(data)

#exploration and summary all variables
#Summary
for (q in names(data)){
  print(q)  
  print(eval(parse(text=(paste("data$",q," %>% summary()",sep="")))))
}
#Historgram
par(mfrow=c(2,2))
for (q in names(data)){
  eval(parse(text=(paste("data$",q," %>% hist(main = '",q,"') ",sep=""))))
}

#Boxplot
par(mfrow=c(2,2))
for (q in names(data)){
  eval(parse(text=(paste0("data$",q," %>% boxplot(main = '",q,"')"))))
}

#No 1.2
# Logartithmization of the variables

data$LABOUR_LOG <- log(data$LABOUR)
data$OUTPUT_LOG <- log(data$OUTPUT)
data$WAGE_LOG <- log(data$WAGE)
data$CAPITAL_LOG <- log(data$CAPITAL)


# No.1.3 

reg <- lm (LABOUR_LOG ~ WAGE_LOG + OUTPUT_LOG + CAPITAL_LOG, data = data)
summary(reg) 
stargazer (reg,type="html")
stargazer (reg,type="text")
# Interpretation 

# No. 2 Visual detection of heteroskedasticity
# No. 2.1 predicting the values vs. observed

data$LABOUR_LOG_PREDICTED <- predict(reg)
data$LABOUR_LOG_PREDICTED 



# No 2.2 Residuals 

data$RESIDUALS <- data$LABOUR_LOG - data$LABOUR_LOG_PREDICTED # manually 
resid(reg)                      # List of residuals
cbind(data$RESIDUALS,resid(reg)) # of course the same
par(mfrow=c(1,1))
plot(density(resid(reg)))       # A density plot - doesn'T look that Gaussian.



# No 2.3 Predicted Values vs. Residuals
plot(x= data$LABOUR_LOG_PREDICTED, y = resid(reg), type = "p",  xlim = NULL, ylim = NULL,
     log = "", main = "PREDICTED VS. RESIDUAL PLOT", sub = NULL, xlab = "Predicted LABOUR_LOG", ylab = "RESIDUALS")

# No. 2.4. interpretation
# a) Error does not have same variance given values
# b) The OLS estimators do not have the smallest error variance
# c) Not BLUE anymore


# No. 3 Breusch-Pagan-Test for Heteroskedasticity
#No 3.1
data$resid_squared <- (resid(reg))^2 ; data$resid_squared

# No 3.2
aux_reg <- lm (resid_squared ~ WAGE_LOG + OUTPUT_LOG + CAPITAL_LOG , data=data) # Corrected
summary(aux_reg) 


#No 3.3
# Breusch-Pagan-Test manually (LM Test)
summary(aux_reg)$r.squared * nrow(data)
# now check Chi-Test statistic for k-degrees of freedom (with k being the estimated coefficients including constant minus 1)

# No 3.4 Breusch-Pagan-Test with R
library(lmtest)
bptest(formula = resid(reg) ~ WAGE_LOG + OUTPUT_LOG + CAPITAL_LOG , data=data ,studentize = TRUE)


# No. 4 White Test for Heteroskedasticity
install.packages("tseries")
library(tseries)
white.test(aux_reg)

library(sandwich)
reg_rob= coeftest(reg, vcov. = vcovHC)
reg_rob

?white.test

