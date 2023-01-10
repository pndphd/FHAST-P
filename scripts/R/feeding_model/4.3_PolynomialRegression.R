###################################################################################
######################## July 2021, Piccolo files extended ########################
######################  Detection range parameter estimate at 95% #################
######### Mar.28, 2022 Estimate the furthest point using Polynomial ###############

# Over the conversation with Peter, it seemed possible to use a polynomial regression to fit the data
# However, I will only be using the points that are likely to be in the outer contour.
# With only the highest data points per certain range interval.
# Conclusion: Polynomial/glmm regressions don't work well with my data.

library(purrr)

#####################################################    
#### 1. Extract 99% data from StandardVelocityDepth data #####  

str(StandardVelocityDepth)

Detect.rangeXZ<-StandardVelocityDepth%>%
  filter(nDetectX<=findmaxdist(nDetectX,99),
         nDetectZ<=findmaxdist(nDetectZ,100))

table(StandardVelocityDepth$block)

  
# Block number for Depth experiment: 31-46
# Block number for Velocity ex: 1-30
# For Depth experiment, I have 3 lengths: 0.053, 0.058, 0.062 (two fish)


# When fish size is identical to 0.062 but with different depth experiments.

  table(Detect.rangeXZ %>%
          filter(Experiment=="Depth",Fish.length==0.062)%>%
          group_by(Depth)%>%
          select(block) )


# use the pmap function to run multiple variables
# use pmap_dfr to combine results to a dataframe

#####################################################################    
#### 2. Extract contour points using the hihestpointXZ function #####  

#function I will be using is: collect.highestpointXZ
  
blocksetscombine<-seq(1,46,1)
highestpointXZ.df<-pmap_dfr(list(blocksetscombine,rep(0.93,46)),
                            collect.highestpointXZ) #


highestpointXZ.df %>%
  ggplot(aes(x=nDetectX,y=nDetectZ, color=as.factor(Fish.length) ))+
  geom_point()+
  scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
  scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))


#######################################################################    
#### 3. Fit extracted data to polynomial regression and glm(m)s  ######

variable.check <- highestpointXZ.df[,c(1:5,13,15:18)]
str(variable.check)
par(mfrow=c(1,1))
chart.Correlation(variable.check, histogram=TRUE, pch=19)
cor.test(StandardVelocityDepth$Depth, StandardVelocityDepth$FeederZ)

# 4.1 Fit polynomial, glm, glmms
# Polynomial
p1m1<-(lm(nDetectZ~polym(nDetectX,degree=2,raw=TRUE)+polym(Velocity,degree=2,raw=TRUE)+polym(Depth,degree=2,raw=TRUE)+polym(Fish.length,degree=2,raw=TRUE), data=highestpointXZ.df))
p1m2<-(lm(nDetectZ~polym(nDetectX,degree=2,raw=TRUE)+Velocity+polym(Depth,degree=2,raw=TRUE)+Fish.length, data=highestpointXZ.df))
p1m3<-(lm(nDetectZ~polym(nDetectX,degree=2,raw=TRUE)+Velocity+Depth+Fish.length, data=highestpointXZ.df))
p1m4<-(lm(nDetectZ~polym(nDetectX,degree=2,raw=TRUE), data=highestpointXZ.df))
p1m5<-(lm(nDetectZ~polym(nDetectX,degree=2,raw=TRUE)+Velocity+Fish.length, data=highestpointXZ.df))
# GLM
g1m1<-glm(nDetectZ~Depth+nDetectX+nDetectY_oneside+Velocity+Fish.length, family=binomial,data=highestpointXZ.df)
g1m2<-glm(nDetectZ~nDetectX+Velocity+Fish.length, family=binomial,data=highestpointXZ.df)
g1m3<-glm(nDetectZ~Velocity+Fish.length, family=binomial,data=highestpointXZ.df)
g1m4<-glm(nDetectZ~nDetectX+Velocity+Fish.length, family=binomial,data=highestpointXZ.df)
g1m5<-glm(nDetectZ~nDetectX+Velocity, family=binomial,data=highestpointXZ.df)
g1m6<-glm(nDetectZ~nDetectX, family=binomial,data=highestpointXZ.df)
g1m7<-glm(nDetectZ~nDetectX+Fish.length, family=binomial,data=highestpointXZ.df)
g1m_Null<-glm(nDetectZ~1, family=binomial,data=highestpointXZ.df)

highestpointXZ.df$FeederY
highestpointXZ.df$FeederY.std

#GLMMs
g1.1<-(glmer(nDetectZ~nDetectX+FeederZ+FeederY.std+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=0, data=highestpointXZ.df))
g1.2<-(glmer(nDetectZ~nDetectX+        FeederY.std+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.3<-(glmer(nDetectZ~nDetectX+        FeederY.std+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.4<-(glmer(nDetectZ~nDetectX+        FeederY.std+Depth+           +         (1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.5<-(glmer(nDetectZ~         FeederZ+FeederY.std+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.6<-(glmer(nDetectZ~         FeederZ+FeederY.std+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.7<-(glmer(nDetectZ~         FeederZ+FeederY.std+Depth+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.8<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.9<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+     +Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.91<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+Depth+          +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.92<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+Depth+          +        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.93<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+Depth+Fish.length+       +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.70<-(glmer(nDetectZ~         poly(FeederZ,2)+FeederY.std+Depth+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.51<-(glmer(nDetectZ~          poly(FeederZ,2)+FeederY.std+Depth+Fish.length+Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.61<-(glmer(nDetectZ~          poly(FeederZ,2)+FeederY.std+Depth+           +Velocity+(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.62<-(glmer(nDetectZ~          poly(FeederZ,2)+FeederY.std+Depth+Fish.length+        +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.71<-(glmer(nDetectZ~         poly(FeederZ,2)+FeederY.std+Depth+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g1.72<-(glmer(nDetectZ~nDetectX+poly(FeederZ,2)+FeederY.std+Depth+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))

g2.1<-(glmer(nDetectZ~poly(nDetectX,2)+poly(FeederZ,2)+FeederY.std+Depth+              +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))
g2.2<-(glmer(nDetectZ~poly(nDetectX,2)+FeederZ+FeederY.std+Depth+                    +(1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=1, data=highestpointXZ.df))

summary(g2.1)

g1_Null<-(glmer(nDetectZ~ (1|Experiment)+(1|Fish.no), family=Gamma, nAGQ=0, data=highestpointXZ.df))

#Compare AICs
AIC(g1m3)
summary(g1.51)

# AIC check
modelporg<-list(g1_Null,g1.1,g1.2,g1.3,g1.4,g1.5,g1.6,g1.7,g1.8,g1.9,g1.91,g1.92,g1.93,g1.71,g1.61,g1.51,g1.62,g1.72,g2.1)
model.namesporg<-c('g1_Null','g1','g2','g3','g4','g5','g6','g7','g1.8','g1.9','g1.91','g1.92','g1.93','g1.71','g1.61','g1.51','g1.62','g1.72','g2.1' ) #, 'dcX2.4', 'dcX2.5', 'dcX2.6','dcX2.7')
(summaryAICporg<-aictab(modelporg,model.namesporg,second.ord=FALSE))
summary(g1.72) #
str(highestpointXZ.df)

# R square fit
install.packages("r2glmm")
library(r2glmm)
r2beta(g1.1, method="sgv")
r2beta(g1.2, method="sgv")
r2beta(g1.3, method="sgv")
r2beta(g1.4, method="sgv")
r2beta(g1.5, method="sgv")
r2beta(g1.6, method="sgv",data=highestpointXZ.df)
r2beta(g1.7, method="sgv",data=highestpointXZ.df)
r2beta(g1.71, method="sgv",data=highestpointXZ.df) # 0.593
r2beta(g1.72, method="sgv",data=highestpointXZ.df) # 0.596
r2beta(g1.61, method="sgv",data=highestpointXZ.df)
r2beta(g1.62, method="sgv",data=highestpointXZ.df)
r2beta(g2.1, method="sgv",data=highestpointXZ.df) # 0.596


# 4.2 Visualize fitted data

# All data
highestpointXZ.df %>%
  ggplot(aes(x=nDetectX, y=nDetectZ))+
  geom_point()+
  scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
  scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))+
  geom_point(aes(x=nDetectX,y=fitted(g2.1)),col="red")



# Depth 60 (because, it has the most data for the detection area)

# Filter data points (Depth==0.60) Visualize the fitted data 
predpolym4<-predict(p1m4,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predpolym5<-predict(p1m5,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predg1m1<-predict(g1m1,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predg1m3<-predict(g1m3,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predg1.71<-predict(g1.71,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predg1.72<-predict(g1.72,highestpointXZ.df%>%filter(Depth==0.60),type="response" )
predg2.1<-predict(g2.1,highestpointXZ.df%>%filter(Depth==0.60),type="response" )

# Visualize in new dataset while controlling for other factors
g2.1.new<-expand.grid(nDetectX=seq(0.5,2.0,length=17),
                      FeederZ=0.6,
                      FeederY.std= 0,
                      Depth=0.60,
                      Experiment=c("Depth"),
                      Fish.no=c("1","3","4","5","8","11","12","13","14"))


g2.1.new$predg2.1<-predict(g2.1,newdata=g2.1.new,allow.new.levels=TRUE,type="response" )

highestpointXZ.df %>%
  filter(Depth==0.60)%>%
  ggplot(aes(x=nDetectX, y=nDetectZ))+
  geom_point()+
  scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
  scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))+
  geom_point(data=g2.1.new,aes(x=nDetectX, y=predg2.1,col=Experiment))



.###########################
#### 5. Conclusion  ######

# It seems like the polynomial and glmm doesn't work to fit the model as it is best
# at fitting the overall average..
# Unfortunately my data doesn't have a clear aligned contour which is why it doesn't work well..


# End of script