#######################################################################################
############################# Piccolo files extended ##################################
######################  Nonlinear Analyses to determine Intercepts #################
################################# March. 21, 2022 ##############################



library(rgl)
library(lme4)
library(car); # vif 
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(AICcmodavg)
library(rsq)
library(nls)
# install.packages("stats")
library(stats)
library(aomisc)
library(devtools)
#install.packages("devtools")
#install_github("onofriandreapg/aomisc")
library(AICcmodavg)
# loading package
library(aomisc)

X <- c(1, 3, 5, 7, 9, 11, 13, 20)
Y <- c(8.22, 14.0, 17.2, 16.9, 19.2, 19.6, 19.4, 19.6)
Y <- c(19.6, 19.4, 19.6, 19.2, 16.9, 17.2, 14.0, 8.22)


# nls fit
model <- nls(Y ~ NLS.asymReg(X, init, m, plateau) )
summary(model)
plot(model, log="")
AIC(model)
# drm fit
model <- drm(Y ~ X , fct = DRC.asymReg(fixed=c(0,NA,NA)))
plot(model, log="", main = "Asymptotic regression", 
     ylim = c(0,25), xlim = c(0,200))







####
.X <- seq(5, 50, 5)
Y <- c(12.6, 74.1, 157.6, 225.5, 303.4, 462.8, 
       669.9, 805.3, 964.2, 1169)
YX<- rnorm(8,10,1)

# nls fit
model <- nls(Y ~ NLS.poly2(X, a, b, c))
summary(model)
#drc fit
model <- drm(Y ~ X,YX, fct = DRC.poly2())
summary(model)
## 
## Model fitted: Second Order Polynomial (3 parms)
## 
## Parameter estimates:
## 
##                 Estimate Std. Error t-value  p-value    
## a:(Intercept) -23.515000  31.175139 -0.7543  0.47528    
## b:(Intercept)   5.466470   2.604011  2.0993  0.07395 .  
## c:(Intercept)   0.371561   0.046141  8.0527 8.74e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error:
## 
##  26.50605 (7 degrees of freedom)
plot(model, log = "", main = "2nd order polynomial")
