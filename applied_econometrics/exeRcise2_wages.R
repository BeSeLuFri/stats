rm(list = ls())
getwd() # In this folder you NEED to load the excel sheet "Wages2" 

# Loading of several packages we work with (if not installed, please install first "install.packages())
library(dplyr)
library(magrittr)
library(memisc)
library(stargazer)
library(car)

# No.1 
# No. 1.1 
# Today we have again a german excel sheet --> read.csv2 to import 
wage <- read.csv2(file = "wages2.csv",header = TRUE) 


# Overview over our data
names(wage)         #traditional (old-school-R)-way
wage %>% names()    #or short in pipe-style
dim(wage)           # shows how many observations & variables
# Variables are already labeled appropiately



#Getting an overview of all variables
 # becaus the following is just too much work we use a loop applying the functions to all variables
    #summary(wage$variable) hist(wage$variable) boxplot(wage$variable)
print(wage$WAGE)
wage$WAGE %>% summary()
# code for showing summarys for all q in wage$
for (q in names(wage)){       # q as a substitution for all "names" in dataset "wage"
print(q)                      # print(q) --> show us the q's in following shape
print(eval(parse(text=(paste0("wage$",q,                     # all q (variables) in wages
                             " %>% summary()" )))))              # show us the summaries
                                                             
}


#now Plots
par(mfrow=c(3,4))   # we will take a look at more plots on the same time, this sets that we have
                    # 3 rows and 4 coloums from now on

#same as above but this time we plot histograms --> hist(main = '",q,"')
for (q in names(wage)){
  print(q)  
  print(eval(parse(text=(paste0("wage$",q," %>% hist(main = '",q,"') ")))))
}

#same again but this time we plot boxplots --> boxplot(main = '",q,"')
for (q in names(wage)){
  print(q)  
  print(eval(parse(text=(paste("wage$",q," %>% boxplot(main = '",q,"')",sep="")))))
}



#No. 1.2 Histogram of WAGE
par(mfrow=c(1,1))   #now we only want to look at 1 plot so 1 column & 1 row
wage$WAGE %>% hist (main = "Histogram of Wages",
                        xlab = "Wages",
                        ylab = "Frequency")


#No. 1.3 Regression Model 
reg <- lm (WAGE ~ EDUC + EXPER , data = wage)
summary(reg)




# No. 2  Scaling
# No. 2.1 WAGE scaling
wage$WAGETH <- wage$WAGE/1000

# No. 2.2 New Regression
reg2 <- lm (WAGETH ~ EDUC + EXPER , data = wage)
summary(reg2) 
stargazer (reg,reg2,type = "text", 
           covariate.labels = c("Education", "Expertise", "Intercept"),
           dep.var.labels = c("Wage", "Wage in 1000$")
           )

# No. 2.3 recoding education in months
wage$EDUCM <- wage$EDUC * 12



# No. 2.4 New Regression
reg3 <- lm (WAGE ~ EDUCM + EXPER , data = wage)
summary(reg3) 
stargazer (reg,reg3,type = "text")




# No. 2.5   z-standardization --> (observations-mean)/sd
#WAGE
mean_wage <- mean(wage$WAGE,na.rm = TRUE) ; mean_wage # with ; first code it than print it
sd_wage   <- sd(wage$WAGE,na.rm = TRUE)   ; sd_wage
wage$WAGE_Z <- (wage$WAGE-mean_wage)/sd_wage 

#EDUCATION
mean_educ <- mean(wage$EDUC,na.rm = TRUE) ; mean_educ
sd_educ   <- sd(wage$EDUC,na.rm = TRUE)   ; sd_educ
wage$EDUC_Z <- (wage$EDUC-mean_educ)/sd_educ ; wage$EDUC_Z

#EXPERIENCE
mean_exper <- mean(wage$EXPER,na.rm = TRUE) ; mean_exper 
sd_exper   <- sd(wage$EXPER,na.rm = TRUE)   ; sd_exper
wage$EXPER_Z <- (wage$EXPER-mean_exper )/sd_exper ; wage$EXPER_Z


#Z-Regression
reg4 <- lm (WAGE_Z ~ EDUC_Z + EXPER_Z , data = wage)
summary(reg4) 
stargazer (reg,reg4,type = "text")

# the short command for standardizing is scale:
wage$EXPER_ZZ=scale(wage$EXPER)



# No. 3 nonlinear relationships
# No. 3.1 ln(wage)
reg5 <- lm (log(WAGE) ~ EDUC+ EXPER , data = wage) # log-level
summary(reg5) 
stargazer (reg,reg5,type = "text")


# No. 3.2 Regression with additional variables
reg6 <- lm (log(WAGE) ~ EDUC + EXPER + TENURE + IQ , data = wage)


# No. 3.3 hypothesis testing
summary(reg6) #look at significance of TENURE & IQ



# No. 3.4 Model Comparison
stargazer (reg5,reg6,type="text", 
           covariate.labels = c("Education", "Experience", "Tenure", "IQ", "Intercept"), 
           dep.var.labels = "Wage") 

# Joint F-Test
lht(reg6, c("IQ = 0", "TENURE = 0"), white.adjust = "hc1")

