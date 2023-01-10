###################################################################################
######################## July 2021, Piccolo files extended ########################
######################  Detection range parameter estimate at 95% #################
####################### Mar. 17, 2022 Extract the furthest point ##################
library(rgl)
library(lme4)
library(car); # vif 
library(ggpubr)
library(aomisc)
library(PerformanceAnalytics)
library(AICcmodavg)
library(rsq)
library(nls)
library(stats)
library(aomisc)
library(devtools)
# loading package

#####

# Mar.28, 2022
# https://www.r-bloggers.com/2020/02/a-collection-of-self-starters-for-nonlinear-regression-in-r/
# https://www.statforbiology.com/2020/stat_nls_usefulfunctions/  


#########################################################    
#### 1. Standardized data (with 99% x,y, axis distance) #####  
Detect.rangeXZ.c

#########################################################    
#### 2. Extract contour points #####
# This code only runs on "Detect.rangeXZ.c"

highestpointXZ.df<-map_df(seq(1,27,1),collect.highestpointSquareX)

highestpointYZ.df<-map_df(seq(1,27,1),collect.highestpointSquareY)
  
########################################################    
#### 3. Mirror the data points ####
# X vs Z (mirror the datapoints with the mid point of both min and max of x values )


plotMirroredData<-function(k,axis){
  if(axis=="X"){
  nDX.mid<-(max(k$nDetectX)+min(k$nDetectX))/2
  k$nDetectX.t<-nDX.mid-(k$nDetectX-nDX.mid)
  
  p1<-k%>%
    ggplot(aes(x=nDetectX,y=nDetectZ))+
    geom_point()+
    xlim(0,1.5)+
    ylim(-0,0.75)
  
  p1.t<-k%>%
    ggplot(aes(x=nDetectX.t,y=nDetectZ))+
    geom_point()+
    scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75,0.01))
  }
    #geom_vline(xintercept=0.35, color="red")+   
    #geom_vline(xintercept=0.48, color="red") #  
  if(axis=="Y"){
  nDY.mid<-(max(k$nDetectY_oneside)+min(k$nDetectY_oneside))/2
  k$nDetectY_oneside.t<-nDY.mid-(k$nDetectY_oneside-nDY.mid)

  p1<-k%>%
    ggplot(aes(x=nDetectY_oneside,y=nDetectZ))+
    geom_point()+
    xlim(0,0.6)+
    ylim(-0,0.75)
  
  
  p1.t<-k%>%
    ggplot(aes(x=nDetectY_oneside.t,y=nDetectZ))+
    geom_point()+
    scale_x_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1),minor_breaks = seq(0,0.6,  0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))
  }
  
  ggarrange(p1,p1.t,
            ncol=1,nrow=2)
  
}

# example
plotMirroredData(highestpointXZ.df %>% filter(HighPointClass==3),"X")
plotMirroredData(highestpointYZ.df %>% filter(HighPointClass==2),"Y")


#########


# Use moving window to find the best fit using AIC values.
estimatePlateau<-function(k,axis,i,j,m){ 
  #k: data, i: low end, j:high end, m: interval
  #k <- highestpointXZ.df%>%filter(HighPointClass==1)
  #axis<-"X"
  #i <- 0.4
  #j <- 0.6
  #m <- 0.05

  nDX.mid<-(max(k$nDetectX)+min(k$nDetectX))/2
  k$nDetectX.t<-nDX.mid-(k$nDetectX-nDX.mid)
  
  nDY.mid<-(max(k$nDetectY_oneside)+min(k$nDetectY_oneside))/2
  k$nDetectY_oneside.t<-nDY.mid-(k$nDetectY_oneside-nDY.mid)
  
  movingrange.c<-c(0,seq(i,j,m)) # these are c values that shift the points closer to the origin
  resultAIC<-matrix(0,length(movingrange.c),5)
  colnames(resultAIC)<-c("c-value","Aicvalue","New_shiftedFocal","PlateauZ","MaxDetectXY")
  resultAIC[,1]<-movingrange.c
  
  
  if(axis=="X"){
  for (n in 1:length(movingrange.c)){
   
    k$nDetectX.t.c<-k$nDetectX.t-movingrange.c[n]
    #m1 <- drm(nDetectZ ~ nDetectX.t.c, data=k, fct = DRC.negExp() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
    #m1.1 <- drm(nDetectZ ~ nDetectX.t.c, data=k, fct = DRC.powerCurve() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
    m1.2 <- lm(nDetectZ ~ nDetectX.t.c, data=k )
    
    #summary(m1.2)
    #plot(m1, log="", main = "Asymptotic regression", 
    #     ylim = c(0,1.2), xlim = c(0,1.5))              
    
    shiftedFocal<-nDX.mid-(0.5-nDX.mid)-movingrange.c[n]
    
    
    #m1
    #shiftedFocal<-nDY.mid-(0-nDY.mid)-movingrange.c[n] #nDY.mid-(0-nDY.mid) is to transform. Fish starts from zero in the Y axis.
    #paramA<-m1$coefficients[1]
    #paramC<-m1$coefficients[2]
    #PlateauZ<-paramA*(1-exp(-paramC*shiftedFocal))
    
    #m1.1
   # paramA<-m1.1$coefficients[1]
  #  paramB<-m1.1$coefficients[2]
  #  PlateauZ<-paramA*(shiftedFocal^paramB)
    
    #m1.2
    paramB<-m1.2$coefficients[[1]]
    paramA<-m1.2$coefficients[[2]]
    PlateauZ<- (paramA*shiftedFocal)+paramB
    MaxDetectX.t<- (-paramB/paramA)
    MaxDetectX<- nDX.mid + (nDX.mid-MaxDetectX.t)
    
    resultAIC[n,2]<-AIC(m1.2)
    resultAIC[n,3]<-shiftedFocal # This is the point where the 0.5 in X axis would have changed if it were to be mirrored and then shifted due to moving.range
    resultAIC[n,4]<-PlateauZ
    resultAIC[n,5]<-MaxDetectX
    
    }
  }
  if(axis=="Y"){
    for (n in 1:length(movingrange.c)){
      
      k$nDetectY_oneside.t.c<-k$nDetectY_oneside.t-movingrange.c[n]
      #m1 <- drm(nDetectZ ~ nDetectY_oneside.t.c, data=k, fct = DRC.negExp() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
      #m1.1 <- drm(nDetectZ ~ nDetectY_oneside.t.c, data=k, fct = DRC.powerCurve() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
      m1.2 <- lm(nDetectZ ~ nDetectY_oneside.t.c, data=k ) 
      
      #summary(m1)
      #plot(m1, log="", main = "Asymptotic regression", 
      #     ylim = c(0,1.2), xlim = c(0,1.5))              
      
      shiftedFocal<-nDY.mid-(0-nDY.mid)-movingrange.c[n] #nDY.mid-(0-nDY.mid) is to transform. Fish starts from zero in the Y axis.
      #m1
      #paramA<-m1$coefficients[1]
      #paramC<-m1$coefficients[2]
      #PlateauZ<-paramA*(1-exp(-paramC*shiftedFocal))

      #m1.1
      #paramA<-m1.1$coefficients[1]
      #paramB<-m1.1$coefficients[2]
      #PlateauZ<-paramA*(shiftedFocal^paramB)

      #m1.2
      paramB<-m1.2$coefficients[[1]]
      paramA<-m1.2$coefficients[[2]]
      PlateauZ<- (paramA*shiftedFocal)+paramB
      MaxDetectY.t<- (-paramB/paramA)
      MaxDetectY<- nDY.mid + (nDY.mid-MaxDetectY.t)
      
      resultAIC[n,2]<-AIC(m1.2)
      resultAIC[n,3]<-shiftedFocal # This is the point where the 0.5 in X axis would have changed if it were to be mirrored and then shifted due to moving.range
      resultAIC[n,4]<-PlateauZ
      resultAIC[n,5]<-MaxDetectY

    }
  }
    # print( paste("New0.5",shifted0.5))
    # print( paste("PlateauY ", PlateauY  ))
     resultAIC
}

# example
PlateauResultsX<-estimatePlateau(highestpointXZ.df%>%filter(HighPointClass==4),axis="X",0.7,0.85,0.01)
PlateauResultsX
PlateauResultsY<-estimatePlateau(highestpointYZ.df%>%filter(HighPointClass==1),axis="Y",0.05,0.15,0.005)
PlateauResultsY
  

#############

# Only this function doesn't read the dataset from the name but instead read the highpointclass.
newPlotPlateau<-function(hpclass,axes,c){ ### make sure I use if {} the else{} in the funciton to runn pmap correctly.
  
  if(axes=="X"){
   
    kdata<-highestpointXZ.df %>% filter(HighPointClass==hpclass)
  # rename the file name to accomodat series of number.. like the Highclasspoint 4 or st..
  nDX.mid<-(max(kdata$nDetectX)+min(kdata$nDetectX))/2
  kdata$nDetectX.t<-nDX.mid-(kdata$nDetectX-nDX.mid)
  
  kdata$nDetectX.t.c<-(kdata$nDetectX.t)-c
   #m2 <- drm(nDetectZ  ~ nDetectX.t.c , data=k, fct = DRC.negExp() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
   #m2.1 <- drm(nDetectZ  ~ nDetectX.t.c , data=k, fct = DRC.powerCurve() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
   m2.2 <- lm(nDetectZ  ~ nDetectX.t.c , data=kdata )
   
   
   # Where the 0.5 would have shifted due to mirroring and moving range c

  shiftedFocal<-nDX.mid-(0.5-nDX.mid)-c
  #m2
  #paramA<-m2$coefficients[1]
  #paramC<-m2$coefficients[2]
  #PlateauZ<-paramA*(1-exp(-paramC*shiftedFocal))
  
  #m2.1
 # paramA<-m2.1$coefficients[1]
#  paramB<-m2.1$coefficients[2]
#  PlateauZ<-paramA*(shiftedFocal^paramB)
  
  #m2.2
  
  paramB<-m2.2$coefficients[[1]]
  paramA<-m2.2$coefficients[[2]]
  PlateauZ<- (paramA*shiftedFocal)+paramB
  MaxDetectX.t<- (-paramB/paramA)
  MaxDetectXorY<- nDX.mid + (nDX.mid-MaxDetectX.t)

  #plot(m2, log="", main = "Asymptotic regression", 
  #     ylim = c(0,1.0), xlim = c(0,1.5))
  #abline(v=shiftedFocal, col="blue")
  
  #plot(m2.1, log="", main = "Asymptotic regression", 
  #     ylim = c(0,1.0), xlim = c(0,1.5))
  #abline(v=shiftedFocal, col="blue")
  S.Vel<-max(kdata$Velocity)
  S.Length<-max(kdata$Fish.length)
  S.Highpointclass<-max(kdata$HighPointClass)
  S.Experiment<-as.numeric(kdata$Experiment)[1]
  S.Depth<-max(kdata$Depth)
  S.NumberPoints<-nrow(kdata)
  s.DataType<-"X"
  
  #data.frame(S.Vel,S.Length,S.Highpointclass,S.Experiment,S.Depth,PlateauZ,MaxDetectXorY,S.NumberPoints,"X")
  #print(MaxDistSummary)
  
    XdataPlot<-ggplot(data=kdata,aes(x=nDetectX.t.c, y=nDetectZ))+
    geom_point()+
    stat_smooth(method = "lm", col = "red")+
      ggtitle(paste("Class:",S.Highpointclass," Velocity:",S.Vel,"Length:",S.Length))+
    scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))
   
  #data.frame(S.Vel,S.Length,S.Highpointclass,S.Experiment,S.Depth,PlateauZ,MaxDetectXorY,S.NumberPoints,"X")

  print(XdataPlot)
    
  #print( paste("New0.5",shiftedFocal))
  #print( paste("PlateauZ ", PlateauZ))
  #print( paste("MaxYDetection",MaxDetectX))
  data.frame(S.Vel,S.Length,S.Highpointclass,S.Experiment,S.Depth,PlateauZ,MaxDetectXorY,S.NumberPoints,s.DataType)
  
  }
  else{
    kdata<-highestpointYZ.df %>% filter(HighPointClass==hpclass)
    nDY.mid<-(max(kdata$nDetectY_oneside)+min(kdata$nDetectY_oneside))/2
    kdata$nDetectY_oneside.t<-nDY.mid-(kdata$nDetectY_oneside-nDY.mid)
    
    kdata$nDetectY_oneside.t.c<-(kdata$nDetectY_oneside.t)-c
    #m2 <- drm(nDetectZ  ~ nDetectY_oneside.t.c , data=k, fct = DRC.negExp() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
    #m2.1 <- drm(nDetectZ  ~ nDetectY_oneside.t.c , data=k, fct = DRC.powerCurve() ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
    m2.2 <- lm(nDetectZ  ~ nDetectY_oneside.t.c , data=kdata ) #DRC.asymReg(fixed=c(0.03,NA,NA)))
    
    # Where the 0.5 would have shifted due to mirroring and moving range c
    shiftedFocal<-nDY.mid-(0-nDY.mid)-c #nDY.mid-(0-nDY.mid) is to transform. Fish starts from zero in the Y axis.
    #m2
    #paramA<-m2$coefficients[1]
    #paramC<-m2$coefficients[2]
    #PlateauZ<-paramA*(1-exp(-paramC*shiftedFocal))
    
    #m2.1
    #paramA<-m2.1$coefficients[1]
    #paramB<-m2.1$coefficients[2]
    #PlateauZ<-paramA*(shiftedFocal^paramB)

    #m2.2
    
    paramB<-m2.2$coefficients[[1]]
    paramA<-m2.2$coefficients[[2]]
    PlateauZ<- (paramA*shiftedFocal)+paramB # y=ax+b formula
    MaxDetectY.t<- (-paramB/paramA) # assume y=0
    MaxDetectXorY<- nDY.mid + (nDY.mid-MaxDetectY.t)

   # plot(m2, log="", main = "Asymptotic regression", 
   #       ylim = c(0,1.0), xlim = c(0,0.6))
   #  abline(v=shiftedFocal, col="blue")
    
   # plot(m2.1, log="", main = "Asymptotic regression", 
  #       ylim = c(0,1.0), xlim = c(0,0.6))
  #  abline(v=shiftedFocal, col="blue")
    S.Vel<-max(kdata$Velocity)
    S.Length<-max(kdata$Fish.length)
    S.Highpointclass<-max(kdata$HighPointClass)
   S.Experiment<-as.numeric(kdata$Experiment)[1]
    S.Depth<-max(kdata$Depth)
    S.NumberPoints<-nrow(kdata)
    s.DataType<-"Y"
    #data.frame(S.Vel,S.Length,S.Highpointclass,S.Experiment,S.Depth,PlateauZ,MaxDetectXorY,S.NumberPoints,"Y")
    #print(MaxDistSummary) 
    
    
    YdataPlot<-kdata %>%
      ggplot(aes(x=nDetectY_oneside.t.c, y=nDetectZ))+
      geom_point()+
      stat_smooth(method = "lm", col = "red")+
      ggtitle(paste("Class:",S.Highpointclass," Velocity:",S.Vel,"Length:",S.Length))+
      scale_x_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1),minor_breaks = seq(0,0.6,  0.01))+
      scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))
    
    print(YdataPlot)
    
    data.frame(S.Vel,S.Length,S.Highpointclass,S.Experiment,S.Depth,PlateauZ,MaxDetectXorY,S.NumberPoints,s.DataType)
    
    
    
    #print( paste("NewFocal",shiftedFocal))
    #print( paste("PlateauZ ", PlateauZ  ))
    #print( paste("MaxYDetection",MaxDetectY))
    }
  }

# example
newPlotPlateau(6,axes="X",0)

newPlotPlateau(3,axes="Y",0)


########################################################################
####### Application #######
# 1. example to linear models

MaxDetectX<-pmap_dfr(list(seq(1,27,1),rep("X",27),rep(0,27)) ,newPlotPlateau)
MaxDetectY<-pmap_dfr(list(seq(1,27,1),rep("Y",27),rep(0,27)) ,newPlotPlateau)

MaxParameter<-rbind(MaxDetectX,MaxDetectY)


# 1.1 Z-intercept (PlateauZ) predictions
MaxParameterZ<-MaxParameter %>%
  filter(!is.na(PlateauZ),!is.na(MaxDetectXorY),S.NumberPoints>=3,
         PlateauZ<1.5, PlateauZ>0.5)

MaxParameterZ
p1  <-lm(PlateauZ~S.Vel+S.Length,data=MaxParameterZ)
p1.2<-lm(PlateauZ~      S.Length,data=MaxParameterZ)
p1.3<-lm(PlateauZ~poly(S.Vel,2)+poly(S.Length,2),data=MaxParameterZ)
p1.4<-lm(PlateauZ~poly(S.Vel,2)+S.Length,data=MaxParameterZ)
p1.5<-lm(PlateauZ~S.Vel+poly(S.Length,2),data=MaxParameterZ)
p1.6<-lm(PlateauZ~poly(S.Vel,2),data=MaxParameterZ)
p1.7<-lm(PlateauZ~poly(S.Length,2),data=MaxParameterZ)
p1.8<-lm(PlateauZ~S.Vel,data=MaxParameterZ)

p_Null<-lm(PlateauZ~1,data=MaxParameterZ)


modelp<-list(p_Null,p1,p1.2,p1.3,p1.4,p1.5,p1.6,p1.7,p1.8)
model.namesp<-c('p_Null','p1','p1.2','p1.3','p1.4','p1.5','p1.6','p1.7','p1.8')
(summaryAICp<-aictab(modelp,model.namesp,second.ord=TRUE))
summary(p1.2)
summary(MaxParameterZ$PlateauZ)

# Z axis Model averaging

zmAV<-lm(PlateauZ~S.Vel+I(S.Vel^2)+S.Length+I(S.Length^2), data=MaxParameterZ)
summary(zmAV)
options(na.action = "na.fail") # need to run dredge
zmAV2<-dredge(zmAV,beta="none",evaluate = T,rank=AICc)
options(na.action = "na.omit") # set back to default

zmAV3<-(model.avg(zmAV2,cumsum(weight) <= .95)) # subset = delta <= 4)) # cumsum(weight) <= .7)) 
summary(zmAV3)

v=c(0.58,0.30,0.48,0.56)
l=c(0.083,0.056,0.061,0.081)

0.9202+(0.7394*(v^2))+(-0.7709*v)+(-34.5407*l^2)+(2.9616*l)




MaxParameterZ %>%
  ggplot(aes(x=S.Length,y=PlateauZ, label=S.Highpointclass))+
  geom_point()+
  geom_text(hjust=0.1, vjust=0.1)+
  stat_smooth(method = "lm", col = "red")


MaxParameterZ %>%
  ggplot(aes(x=S.Vel,y=PlateauZ, label=S.Highpointclass))+
  geom_point()+
  geom_text(hjust=0.1, vjust=0.1)+
  stat_smooth(method = "lm", col = "red")


MaxParameterZ %>%
select(PlateauZ)%>%
  summary()

## We didn't find the relationshoip between the Z (palteauZ) intercept and the Velocity or fish.length.
# This means that the plateauZ does not increase oversize or velocity or simply probably due to lack of sample size
# Therefore, we should be using the average of the Z intercept for the model.. wihch is 0.7661.
# However, I need to add a couple more cm on top of this size so that the fish would have time to respond to the prey 
# arriving at that max height.


# 1.2 X-intercept predictions (Deprecated !! This method generates too litttle sample size)

MaxParameterX<-MaxParameter %>%
  filter(!is.na(PlateauZ),!is.na(MaxDetectXorY),S.NumberPoints>=3,
         s.DataType=="X",PlateauZ<1.5)

MaxParameterX
px1  <-lm(MaxDetectXorY ~S.Vel+S.Length,data=MaxParameterX)
px1.2<-lm(MaxDetectXorY~      S.Length,data=MaxParameterX)
px1.3<-lm(MaxDetectXorY~      S.Vel,data=MaxParameterX)
px1.4<-lm(MaxDetectXorY~ poly(S.Vel,2)+poly(S.Length,2),data=MaxParameterX)
px1.5<-lm(MaxDetectXorY~ poly(S.Vel,2)+S.Length,data=MaxParameterX)
px1.6<-lm(MaxDetectXorY~ S.Vel+poly(S.Length,2),data=MaxParameterX)

px_NUll<-lm(MaxDetectXorY~1,data=MaxParameterX)

modelpx<-list(px_NUll,px1,px1.2,px1.3,px1.4,px1.5,px1.6)
model.namespx<-c('px_Null','px1','px1.2','px1.3','px1.4','px1.5','px1.6')
(summaryAICpx<-aictab(modelpx,model.namespx,second.ord=TRUE))
summary(px1.4) # 


# 1.3 Y-intercept predictions (Deprecated!! : generates too littel sample size)
MaxParameterY<-MaxParameter %>%
  filter(!is.na(PlateauZ),!is.na(MaxDetectXorY),S.NumberPoints>=3,
         s.DataType=="Y",PlateauZ<1.5)

MaxParameterY
py1  <-lm(MaxDetectXorY ~S.Vel+S.Length,data=MaxParameterY)
py1.2<-lm(MaxDetectXorY~      S.Length,data=MaxParameterY)
py1.3<-lm(MaxDetectXorY~      S.Vel,data=MaxParameterY)
py1.4<-lm(MaxDetectXorY~ poly(S.Vel,2)+poly(S.Length,2),data=MaxParameterY)
py1.5<-lm(MaxDetectXorY~ poly(S.Vel,2)+S.Length,data=MaxParameterY)
py1.6<-lm(MaxDetectXorY~ S.Vel+poly(S.Length,2),data=MaxParameterY)

py_NUll<-lm(MaxDetectXorY~1,data=MaxParameterY)

modelpy<-list(py_NUll,py1,py1.2,py1.3,py1.4,py1.5,py1.6)
model.namespy<-c('py_Null','py1','py1.2','py1.3','py1.4','py1.5','py1.6')
(summaryAICpy<-aictab(modelpy,model.namespy,second.ord=TRUE))

# None of the variables are significant. I might as well use the raw data to extract the furthest points.



#############################################################################
####  Use raw data to extract furthest extent data points from both X and Y.

Detect.rangeXZ.c
str(Detect.rangeXZ.c)

# 46 individual experimnts (block) has been summarised to 27 HighPointClasses.
Detect.rangeXZ.c %>%
  select(block)%>%
  summary()

Detect.rangeXZ.c %>%
  select(HighPointClass)%>%
  summary()


library(MuMIn)
# X axis prediction
XMaxParam<-data.frame(Detect.rangeXZ.c %>%
  group_by(HighPointClass) %>%
  summarise(FurthestX=max(nDetectX),s.Vel=max(Velocity),s.Fish.length=max(Fish.length)) )

hist(XMaxParam$FurthestX)
xm1<-lm(FurthestX~s.Vel+s.Fish.length, data=XMaxParam)
xm2<-lm(FurthestX~s.Vel             , data=XMaxParam)
xm3<-lm(FurthestX~poly(s.Vel,2)+poly(s.Fish.length,2)  , data=XMaxParam)
xm4<-lm(FurthestX~poly(s.Vel,2)+s.Fish.length  , data=XMaxParam)
xm5<-lm(FurthestX~s.Vel+poly(s.Fish.length,2)  , data=XMaxParam)
xm6<-lm(FurthestX~s.Fish.length  , data=XMaxParam)

xm_Null<-lm(FurthestX~1 , data=XMaxParam)

modelxm<-list(xm_Null,xm1,xm2,xm3,xm4,xm5,xm6)
model.namesxm<-c('xm_Null','xm1','xm2','xm3','xm4','xm5','xm6')
(summaryAICxm<-aictab(modelxm,model.namesxm,second.ord=TRUE))
summary(xm4) # Plynomial model that predicts the furthest X axis

# X axis Model averaging
install.packages("metafor")
library(metafor)
eval(metafor:::.MuMIn)
xmAV<-lm(FurthestX~s.Vel+I(s.Vel^2)+s.Fish.length+I(s.Fish.length^2), data=XMaxParam)
summary(xmAV)
options(na.action = "na.fail") # need to run dredge
xmAV2<-dredge(xmAV,beta="none",evaluate = T,rank=AICc)
options(na.action = "na.omit") # set back to default

xmAV3<-(model.avg(xmAV2,cumsum(weight) <= .95)) # subset = delta <= 4)) # cumsum(weight) <= .7)) 
summary(xmAV3)

v=c(0.58,0.30,0.48,0.56)
l=c(0.083,0.056,0.061,0.081)
0.88915+(-1.59704*(v^2))+(0.73901*v)+(0.63983*l^2)+(0.06965*l)

# Copy paste the x axis moel averaging codes and apply to the Y axis prediction.
# Go ahead and extract the estimates and copy paste it to the simulaiton code and run it.

# Y axis prediction
YMaxParam<-data.frame(Detect.rangeXZ.c %>%
                        filter(nDetectY_oneside<0.5)%>%
                        group_by(HighPointClass) %>%
                        summarise(FurthestY=max(nDetectY_oneside),s.Vel=min(Velocity),s.Fish.length=min(Fish.length)) )

hist(YMaxParam$FurthestY)
ym1<-lm(FurthestY~s.Vel+s.Fish.length, data=YMaxParam)
ym2<-lm(FurthestY~s.Vel             , data=YMaxParam)
ym3<-lm(FurthestY~poly(s.Vel,2)+poly(s.Fish.length,2)  , data=YMaxParam)
ym4<-lm(FurthestY~poly(s.Vel,2)+s.Fish.length  , data=YMaxParam)
ym5<-lm(FurthestY~s.Vel+poly(s.Fish.length,2)  , data=YMaxParam)
ym6<-lm(FurthestY~s.Fish.length  , data=YMaxParam)

ym_Null<-lm(FurthestY~1 , data=YMaxParam)

modelym<-list(ym_Null,ym1,ym2,ym3,ym4,ym5,ym6)
model.namesym<-c('ym_Null','ym1','ym2','ym3','ym4','ym5','ym6')
(summaryAICym<-aictab(modelym,model.namesym,second.ord=TRUE))
summary(ym1) # ym1 and ym2 has similar R-square values.

# Y axis Model averaging

ymAV<-lm(FurthestY~s.Vel+I(s.Vel^2)+s.Fish.length+I(s.Fish.length^2), data=YMaxParam)
summary(ymAV)
options(na.action = "na.fail") # need to run dredge
ymAV2<-dredge(ymAV,beta="none",evaluate = T,rank=AICc)
options(na.action = "na.omit") # set back to default

ymAV3<-(model.avg(ymAV2,cumsum(weight) <= .95)) # subset = delta <= 4)) # cumsum(weight) <= .7)) 
summary(ymAV3)

v=c(0.58,0.30,0.48,0.56)
l=c(0.083,0.056,0.061,0.081)

0.53269+(0.01084*(v^2))+(-0.52829*v)+(-1.50790*l^2)+(-0.22174*l)





### End of script
















########################################################################
####### Application (Deprecated) #######
# !! (Deprecated!! 
# 2. example to fit nonlinear models
# X axis data
plotMirroredData(highestpointXZ.df %>% filter(HighPointClass==4),"X")
PlateauResultsX<-estimatePlateau(highestpointXZ.df%>%filter(HighPointClass==4),axis="X",
                                 0.7,
                                 0.85,
                                 0.01)
cvalueLowestAicX<-data.frame(PlateauResultsX)%>%filter(Aicvalue==min(Aicvalue))%>%select(c.value)%>%pull()

newPlotPlateau(highestpointXZ.df %>% filter(HighPointClass==4),axes="X",0) #cvalueLowestAicX)

# Y axis data
plotMirroredData(highestpointYZ.df %>% filter(HighPointClass==4),"Y")
PlateauResultsY<-estimatePlateau(highestpointYZ.df%>%filter(HighPointClass==4),axis="Y",
                                 0.05,
                                 0.13,
                                 0.005)
cvalueLowestAicY<-data.frame(PlateauResultsY)%>%filter(Aicvalue==min(Aicvalue))%>%select(c.value)%>%pull()
newPlotPlateau(highestpointYZ.df %>% filter(HighPointClass==4),axes="Y",0)





################################
# Deprecated! To fit DRC.negEXP()

fittedhighestpointX<-data.frame(highestpointXZ.df %>%  
  group_by(HighPointClass) %>%
  arrange(Fish.length) %>%
  filter(row_number()==1)%>%
  select(Velocity,Depth,Fish.length,block,Experiment,HighPointClass)%>%
  mutate(MaxDetectZ=case_when(HighPointClass==1~0.636616304518153 ,HighPointClass==2~1.07756263261563 ,HighPointClass==3~1.41569854695833,
                              HighPointClass==4~0.301588249611125 ,HighPointClass==5~ -999, HighPointClass==6~-999 ,
                              HighPointClass==7~-999 ,HighPointClass==8~0.286822642825388 ,HighPointClass==9~-999 ,
                              HighPointClass==10~-999 ,HighPointClass==11~0.213026505783371 ,HighPointClass==12~-999 ,
                              HighPointClass==13~-999 ,HighPointClass==14~0.392558332159864 ,HighPointClass==15~-999 ,
                              HighPointClass==16~0.412047181317964 ,HighPointClass==17~0.551930607238294 ,HighPointClass==18~-999 ,
                              HighPointClass==19~-999 ,HighPointClass==20~-999 ,HighPointClass==21~-999 ,
                              HighPointClass==22~0.329985704485872 ,HighPointClass==23~0.281771325848063 ,HighPointClass==24~-999,
                              HighPointClass==25~-999 ,HighPointClass==26~0.262678539327196 ,HighPointClass==27~-999),
DataType='X'))

fittedhighestpointY<-data.frame(highestpointYZ.df %>%  
             group_by(HighPointClass) %>%
             arrange(Fish.length) %>%
             filter(row_number()==1)%>%
             select(Velocity,Depth,Fish.length,block,Experiment,HighPointClass)%>%
             mutate(MaxDetectZ=case_when(HighPointClass==1~0.650545850436803 ,HighPointClass==2~0.826329264307647 ,HighPointClass==3~ 0.745601711925923,
                                         HighPointClass==4~0.39964921132599 ,HighPointClass==5~-999 ,HighPointClass==6~-999 ,
                                         HighPointClass==7~-999 ,HighPointClass==8~-999,HighPointClass==9~-999 ,
                                         HighPointClass==10~-999 ,HighPointClass==11~0.257245620835764 ,HighPointClass==12~-999 ,
                                         HighPointClass==13~0.28813623318919 ,HighPointClass==14~0.29305573509308 ,HighPointClass==15~0.250155112293318 ,
                                         HighPointClass==16~-999 ,HighPointClass==17~0.247558049752334 ,HighPointClass==18~0.289830445956582 ,
                                         HighPointClass==19~0.588183599288258 ,HighPointClass==20~0.308819316732707 ,HighPointClass==21~-999 ,
                                         HighPointClass==22~0.310720231595476 ,HighPointClass==23~0.164772233062083 ,HighPointClass==24~0.27226503464113 ,
                                         HighPointClass==25~0.412694774530376 ,HighPointClass==26~0.455311244918634 ,HighPointClass==27~-999),
                    DataType="Y"))
# what's the higest nDetectZ points for each HighPointClass types
data.frame(Detect.rangeXZ.c %>%
             group_by(HighPointClass)%>%
             summarise(maxnDetectZ=max(nDetectZ)))

# Filter points to be used for linear analysis
fittedhighestpointXY<-rbind(fittedhighestpointX,fittedhighestpointY)
fittedhighestpointXY %>%
  filter(MaxDetectZ>0.3, MaxDetectZ<0.5)




