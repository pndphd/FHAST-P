#######################################################################################
############################# Piccolo files extended ##################################
######################  Detection range parameter statistical analyses #################
################################# March. 15, 2022 ##############################
# Polynomial or other regression didn't work. So, I am trying to prepare the data
# for a nonlinear fit. Here, I am trying to narrow down the most important 
# factors that affect the maximum detection distance.


# Here I am trying to test what 'Factors' affect the detection distances.

library(rgl)
library(lme4)
library(car); # vif 
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(AICcmodavg)
library(rsq)
library(ggpubr)
library(AICcmodavg)
library(r2glmm)


# Purpose of the statistical analyses:
#     I want to estimate the furthest (95%) detection distance (XYZ axes) to fit the ellipsoid.
#     Furthest detection distance may vary depending on variables such as fish size, velocity, or water depth.
#     Depth is correlated with the feeder location (prey location) so if we include depth variable this would interact and confuse the results..
#     Feeder locations has the strongest affect, right? One side (y-axis or Z-axis) skewed prey would end up shorter detection distance.
#     Before estimating (calculating) the intercepts, I am trying to test what factors affect the detection distance.
#     Then the task is to standardize the Y axis of the feeder.
# What variables should I include:
# 1. Velocity
# 2. Fish.length
# 3. Feeder locations!
# 3. Depth: Not relevant (correlative with the FeederZ 0.47)

#### 0. Select Data to use

Detect.rangeXZ<-StandardVelocityDepth%>%
  filter(nDetectX<=findmaxdist(nDetectX,99),
         nDetectZ<=findmaxdist(nDetectZ,100))

#### 1. Correlation test #####

variable.check <- Detect.rangeXZ[,c(1,2,3,5,15)]
str(variable.check)
par(mfrow=c(1,1))
chart.Correlation(variable.check, histogram=TRUE, pch=19)
cor.test(Detect.rangeXZ$Depth, Detect.rangeXZ$FeederZ)


#### 2. Statistical analyses for X axis detection range #####

dcX2.1<-(glmer(nDetectX~FeederY.std+FeederZ+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
vif(dcX2.1);
dcX2.2<-(glmer(nDetectX~FeederY.std+        +Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.21<-(glmer(nDetectX~poly(FeederY.std,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.22<-(glmer(nDetectX~FeederY.std+       +Fish.length+poly(Velocity,2)+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.3<-(glmer(nDetectX~FeederY.std+FeederZ +           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.4<-(glmer(nDetectX~FeederY.std+FeederZ +Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.5<-(glmer(nDetectX~FeederY.std+                    +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.6<-(glmer(nDetectX~FeederY.std+       +Fish.length          +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX2.7<-(glmer(nDetectX~FeederY.std+FeederZ+                     +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

dcX3.1<-(glmer(nDetectX~FeederY.std+FeederZ+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX3.2<-(glmer(nDetectX~FeederY.std+FeederZ+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX3.3<-(glmer(nDetectX~           +FeederZ+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

dcX4.1<-(glmer(nDetectX~poly(FeederY.std,2)+poly(FeederZ,2)+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX4.2<-(glmer(nDetectX~        FeederY.std+poly(FeederZ,2)+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX4.3<-(glmer(nDetectX~        FeederY.std+poly(FeederZ,2)+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX4.4<-(glmer(nDetectX~                   +poly(FeederZ,2)+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

dcX5.1<-(glmer(nDetectX~poly(FeederY.std,2)+poly(FeederZ,2)+poly(Depth,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX5.2<-(glmer(nDetectX~        FeederY.std+poly(FeederZ,2)+poly(Depth,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX5.3<-(glmer(nDetectX~        FeederY.std+poly(FeederZ,2)+poly(Depth,2)+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcX5.4<-(glmer(nDetectX~                   +poly(FeederZ,2)+poly(Depth,2)+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

summary(dcX5.2)

dcX_Null<-(glmer(nDetectX~1                                      +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

# AIC check
modeldcX<-list(dcX_Null,dcX2.1,dcX2.2,dcX2.21,dcX2.22,
               dcX2.3,dcX2.4,dcX2.5,dcX2.6,dcX2.7,dcX3.1,dcX3.2,dcX3.3,
               dcX4.1,dcX4.2,dcX4.3,dcX4.4,
               dcX5.1,dcX5.2,dcX5.3,dcX5.4)
model.namesdcX<-c('dcX_Null','dcX2.1','dcX2.2','dcx2.21','dcX2.22',
                  'dcX2.3','dcX2.4','dcX2.5','dcX2.6','dcX2.7','dcX3.1','dcX3.2','dcX3.3',
                  'dcX4.1','dcX4.2','dcX4.3','dcX4.4',
                  'dcX5.1','dcX5.2','dcX5.3','dcX5.4')
(summaryAICdcX<-aictab(modeldcX,model.namesdcX,second.ord=FALSE))
summary(dcX4.2) # 
rsq(dcX5.2)


Detect.rangeXZ %>%
# filter(Depth==0.60, Fish.no=="12")%>%
 #select(Fish.length)%>%
 # summary()
  ggplot(aes(x=nDetectX, y=nDetectZ))+
  geom_point()+
  #scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
  #scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))+
  geom_point(aes(x=fitted(dcX5.2), y=nDetectZ, color="red")) + #,col=Experiment))
  ggtitle(paste("dcX5.2"))


# Results: Velocity and Fish.length and FeederY.std affects the X-axis Detection range.







#### 3. Y axis detection range #####

Detect.rangeXZ %>%
  ggplot(aes(x=nDetectY_oneside))+
  geom_histogram()


dcY1.1<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+poly(FeederZ,2)+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.2<-(glmer(nDetectY_oneside~        FeederY.std+poly(FeederZ,2)+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.3<-(glmer(nDetectY_oneside~        FeederY.std+poly(FeederZ,2)+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.4<-(glmer(nDetectY_oneside~                   +poly(FeederZ,2)+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.5<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+   Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.6<-(glmer(nDetectY_oneside~     FeederY.std+        FeederZ+   Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.7<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+   Depth+            +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.8<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+   Depth+Fish.length+         +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY1.9<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+   Depth+                     +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY2.1<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+poly(FeederZ,2)+poly(Depth,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY2.2<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+poly(Depth,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY2.3<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+poly(Depth,2)+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY2.4<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+poly(Depth,2)+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY2.5<-(glmer(nDetectY_oneside~poly(FeederY.std,2)+     FeederZ+poly(Depth,2)+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY3.1<-(glmer(nDetectY_oneside~     FeederY.std+        poly(FeederZ,2)+   Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY3.2<-(glmer(nDetectY_oneside~     FeederY.std+        poly(FeederZ,2)+   poly(Depth,2)+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY3.3<-(glmer(nDetectY_oneside~     FeederY.std+        FeederZ+   Depth+            +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))
dcY3.4<-(glmer(nDetectY_oneside~     FeederY.std+        FeederZ+   Depth+Fish.length+         +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

summary(dcY1.5)

dcY_Null<-(glmer(nDetectY_oneside~1                                      +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=Detect.rangeXZ))

# AIC check
modeldcY<-list(dcY_Null,dcY1.1,dcY1.2,dcY1.3,dcY1.4,dcY1.5,dcY1.6,dcY1.7,dcY1.8,dcY1.9,
               dcY2.1,dcY2.2,dcY2.3,dcY2.4,dcY2.5,
               dcY3.1,dcY3.2,dcY3.3,dcY3.4)
model.namesdcY<-c('dcY_Null','dcY1.1','dcY1.2','dcY1.3','dcY1.4','dcY1.5','dcY1.6','dcY1.7','dcY1.8','dcY1.9',
                  'dcY2.1','dcY2.2','dcY2.3','dcY2.4','dcY2.5',
                  'dcY3.1','dcY3.2','dcY3.3','dcY3.4')
(summaryAICdcY<-aictab(modeldcY,model.namesdcY,second.ord=FALSE))



Detect.rangeXZ %>%
  # filter(Depth==0.60, Fish.no=="12")%>%
  #select(Fish.length)%>%
  # summary()
  ggplot(aes(x=nDetectY_oneside, y=nDetectZ,color=Experiment))+
  geom_point()+
  #scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
  #scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))+
  geom_point(aes(x=fitted(dcY1.5), y=nDetectZ, color="prediction")) + #,col=Experiment))
  ggtitle(paste("dcY1.5"))


# Results: Fish.length and Velocity affects the Y-axis Detection range.







#### 4. Z axis detection range #####

 # Even if I use the filtered data... it might not have good enough data to show the effects on the Z-intercept due to depth being restricted.
# So, although I did the analyses below, I see that the velocity maybe significant. However, this may be the 
# reason because some of the data are limited in depth. So the depth data and the z-axis data are confounded

highestpointXZ.df$Data.type<-"X"
highestpointYZ.df$Data.type<-"Y"
highestpointTotal.df<-rbind(highestpointXZ.df,highestpointYZ.df)


dcZ1.1<-(glmer(nDetectZ~Depth+FeederY.std+FeederZ+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
vif(dcZ1.1);
dcZ1.2<-(glmer(nDetectZ~Depth+FeederY.std+FeederZ+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ1.3<-(glmer(nDetectZ~Depth+FeederY.std+FeederZ+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ1.4<-(glmer(nDetectZ~Depth+FeederY.std+FeederZ+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))

dcZ2.1<-(glmer(nDetectZ~poly(Depth,2)+poly(FeederY.std,2)+FeederZ+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ2.2<-(glmer(nDetectZ~poly(Depth,2)+poly(FeederY.std,2)+FeederZ+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ2.3<-(glmer(nDetectZ~poly(Depth,2)+poly(FeederY.std,2)+FeederZ+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ2.4<-(glmer(nDetectZ~poly(Depth,2)+poly(FeederY.std,2)+FeederZ+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))

dcZ3.1<-(glmer(nDetectZ~Depth+poly(FeederY.std,2)+FeederZ+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ3.2<-(glmer(nDetectZ~Depth+poly(FeederY.std,2)+FeederZ+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ3.3<-(glmer(nDetectZ~Depth+poly(FeederY.std,2)+FeederZ+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ3.4<-(glmer(nDetectZ~Depth+poly(FeederY.std,2)+FeederZ+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))

dcZ4.1<-(glmer(nDetectZ~poly(Depth,2)+FeederY.std+FeederZ+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ4.2<-(glmer(nDetectZ~poly(Depth,2)+FeederY.std+FeederZ+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ4.3<-(glmer(nDetectZ~poly(Depth,2)+FeederY.std+FeederZ+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))
dcZ4.4<-(glmer(nDetectZ~poly(Depth,2)+FeederY.std+FeederZ+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))

summary(dcZ3.2)
dcZ_Null<-(glmer(nDetectZ~                                      (1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointTotal.df))

# AIC check
modeldcZ<-list(dcZ_Null,dcZ1.1,dcZ1.2,dcZ1.3,dcZ1.4,
              # dcZ2.1,dcZ2.2,dcZ2.3,dcZ2.4,
               dcZ3.1,dcZ3.2,dcZ3.3,dcZ3.4)
              # dcZ4.1,dcZ4.2,dcZ4.3,dcZ4.4)
model.namesdcZ<-c('dcZ_Null','dcZ1.1', 'dcZ1.2', 'dcZ1.3', 'dcZ1.4',
               #   'dcZ2.1', 'dcZ2.2', 'dcZ2.3', 'dcZ2.4',
                  'dcZ3.1', 'dcZ3.2', 'dcZ3.3', 'dcZ3.4')
                #  'dcZ4.1', 'dcZ4.2', 'dcZ4.3', 'dcZ4.4')
(summaryAICdcZ<-aictab(modeldcZ,model.namesdcZ,second.ord=FALSE))
summary(dcZ3.2) 

# result: Velocity
highestpointTotal.df %>%
  ggplot(aes(x=Velocity, y=nDetectZ)) + #, color=Experiment)) +
  geom_point()+
  geom_point(aes(x=nDetectY_oneside, y=fitted(dcZ3.2), color="prediction")) + #,col=Experiment))
  ggtitle(paste("dcZ2.3"))

# Conclusion: While, I have conducted this analyses along with using the raw data (Detect.rangeXZ) the results are not clear.
# Therefore, I should probably use the mean??


