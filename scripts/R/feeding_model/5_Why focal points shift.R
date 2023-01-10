###################################################################################
######################## July 2021, Piccolo files extended ########################
######################  Why do focal points change? #################
####################### Jul.20,2021: Seperate sheets ##############################
## Q) Why do focal points change?
  # x axis: Fish.no, pre_focalX,             , Velocity, 
  # y axis:          pre_focalY, pre_CaptureY, Velocity, Feeder left/right
  # z axis:          pre_focalZ, pre_CaptureX, Velocity



# April.6, 2022
## NOTICE:  I would need to change the next_Focal values as the Depth data FocalY is not calibrated!!




library(rgl)
library(lme4)
library(PerformanceAnalytics)
library(gbm);library(caret)
library(tidyr);library(dplyr)
library(corrplot)
#install.packages("Metrics");
library(Metrics)
library(MuMIn)
#install.packages("AICcmodavg")
library(AICcmodavg)
# install.packages("rsq"); # R-sqaured for linear models
library(rsq) 
library(car); # vif 

library(scatterplot3d)
#####################################################################
#### 1. How different are focal points to the next focal points? ####
# Focal1-> Capture vs Return -> Focal2; how consistently do fish come back to the origin####
# very sparsed.Why? is it becasue of the previous capture location? Individual fish? Velocity?

  # 1.1 Plot all Focal points
    open3d()
    plot3d(testdata1.2$FocalX,testdata1.2$FocalY,testdata1.2$FocalZ,
           col='red',
           xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35) )
    plot3d(testdata1.2$ReturnX,testdata1.2$ReturnY,testdata1.2$ReturnZ, 
           xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35) ,
           col="black", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    table(testdata1.2$Velocity)
    table(testdata1.2$Fish.no)


  # 1.2 Plot focal points: individual Fish and Velocity--> They vary slightly depending on fish and velocity
    testdata1.2 %>%
      ggplot(aes(FocalX,FocalY, color=as.factor(Fish.no)) )+
      facet_wrap(~ Fish.no + Velocity)+
      geom_point() + xlim(-0.2,1.0) +ylim(0,1.0) + 
      labs(title="Focal locations",color= "Velocity")
  
      ggplot(testdata1.2, aes(x=(Velocity),y=FocalX, color=factor(Fish.no)))+
        geom_point()
      ggplot(testdata1.2, aes(x=as.factor(Velocity),y=FocalX, color=factor(Velocity)))+
        geom_boxplot()

      
#################################################################################
#### 2. Does previous capture location affect the next focal point location? ####
### NOTE!! Use Outliars to detect changes ###

# How does capture positions look like?
testdata1.2 %>%
  ggplot(aes(CaptureX,CaptureY, color=as.factor(Fish.no)) )+
  facet_wrap(~ Fish.no + Velocity)+
  geom_point() + xlim(-0.2,1.0) +ylim(0,1.0) + 
  labs(title="Capture locations",color= "Velocity")

    # 2.1 Focal locations are closer to the previous capture locations?
      # Z-AXIS
        OutliarZ<-data.frame(testdata1.2 %>% # filter ones that only have previous locations
                               select(Order,Fish.no,Velocity,pre_FocalX:pre_FocalZ,pre_CaptureX:pre_CaptureZ,pre_ReturnX:pre_ReturnZ,FocalX:FocalZ,CaptureX:CaptureZ,ReturnX:ReturnZ,next_FocalX:next_FocalZ) %>%
                               filter(FocalX !=0 & CaptureX !=0 & ReturnZ !=0 & FocalZ > -0.15 & !is.na(next_FocalY)  & !is.na(pre_FocalX) & !is.na(pre_CaptureX) & !is.na(pre_ReturnX))  )
        nrow(OutliarZ) 
        open3d()
        #previous focal/capture/return
        plot3d(OutliarZ$pre_FocalX,OutliarZ$pre_FocalY,OutliarZ$pre_FocalZ,
               col='red',
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8)
        text3d(OutliarZ$pre_FocalX,OutliarZ$pre_FocalY,OutliarZ$pre_FocalZ,rownames(OutliarZ),adj=c(-0.6,0.25))
        
        plot3d(OutliarZ$pre_CaptureX,OutliarZ$pre_CaptureY,OutliarZ$pre_CaptureZ,
               col='blue',
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8, add=TRUE)
        
        plot3d(OutliarZ$pre_ReturnX,OutliarZ$pre_ReturnY,OutliarZ$pre_ReturnZ,
               col='black',
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8, add=TRUE)
        
        # Past Focal and capture
        segments3d(OutliarZ[1,c(4,7)],OutliarZ[1,c(5,8)],OutliarZ[1,c(6,9)],col=2,lwd=3) 
        segments3d(OutliarZ[2,c(4,7)],OutliarZ[2,c(5,8)],OutliarZ[2,c(6,9)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(4,7)],OutliarZ[3,c(5,8)],OutliarZ[3,c(6,9)],col=4,lwd=2) 
        
        # Past capture and return
        segments3d(OutliarZ[1,c(7,10)],OutliarZ[1,c(8,11)],OutliarZ[1,c(9,12)],col=2,lwd=2) 
        segments3d(OutliarZ[2,c(7,10)],OutliarZ[2,c(8,11)],OutliarZ[2,c(9,12)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(7,10)],OutliarZ[3,c(8,11)],OutliarZ[3,c(9,12)],col=4,lwd=2) 
        
        
        #current focal/capture/return
        plot3d(OutliarZ$FocalX,OutliarZ$FocalY,OutliarZ$FocalZ,
               col='red',
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8, add=TRUE)
        plot3d(OutliarZ$CaptureX,OutliarZ$CaptureY,OutliarZ$CaptureZ, 
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8 ,
               col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
        plot3d(OutliarZ$ReturnX,OutliarZ$ReturnY,OutliarZ$ReturnZ, 
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8 ,
               col="black", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
        
        
        #next focal
        plot3d(OutliarZ$next_FocalX,OutliarZ$next_FocalY,OutliarZ$next_FocalZ, 
               xlim=c(-0.3,1.5), ylim=c(0,1.0),zlim=c(0,0.35), size=8 ,
               col="red", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
        
        # Past return and Current Focal
        segments3d(OutliarZ[1,c(10,13)],OutliarZ[1,c(11,14)],OutliarZ[1,c(12,15)],col=2,lwd=2) 
        segments3d(OutliarZ[2,c(10,13)],OutliarZ[2,c(11,14)],OutliarZ[2,c(12,15)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(10,13)],OutliarZ[3,c(11,14)],OutliarZ[3,c(12,15)],col=4,lwd=2) 
        
        # Current Focal and capture
        segments3d(OutliarZ[1,c(13,16)],OutliarZ[1,c(14,17)],OutliarZ[1,c(15,18)],col=2,lwd=2) 
        segments3d(OutliarZ[2,c(13,16)],OutliarZ[2,c(14,17)],OutliarZ[2,c(15,18)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(13,16)],OutliarZ[3,c(14,17)],OutliarZ[3,c(15,18)],col=4,lwd=2) 
        # Current capture and return  
        segments3d(OutliarZ[1,c(16,19)],OutliarZ[1,c(17,20)],OutliarZ[1,c(18,21)],col=2,lwd=2) 
        segments3d(OutliarZ[2,c(16,19)],OutliarZ[2,c(17,20)],OutliarZ[2,c(18,21)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(16,19)],OutliarZ[3,c(17,20)],OutliarZ[3,c(18,21)],col=4,lwd=2) 
        # Return and next focal   
        segments3d(OutliarZ[1,c(19,22)],OutliarZ[1,c(20,23)],OutliarZ[1,c(21,24)],col=2,lwd=2) 
        segments3d(OutliarZ[2,c(19,22)],OutliarZ[2,c(20,23)],OutliarZ[2,c(21,24)],col=3,lwd=2) 
        segments3d(OutliarZ[3,c(19,22)],OutliarZ[3,c(20,23)],OutliarZ[3,c(21,24)],col=4,lwd=2) 
        
      
      
  # 2.2 How many of other outliars do we have in otehr axes?         
        data.frame(testdata1.2 %>% # filter ones that only have previous locations
                     select(Order,Fish.no,Velocity,pre_FocalX:pre_FocalZ,pre_CaptureX:pre_CaptureZ,pre_ReturnX:pre_ReturnZ,FocalX:FocalZ,CaptureX:CaptureZ,ReturnX:ReturnZ,next_FocalX:next_FocalZ) %>%
                     filter(FocalX !=0 & CaptureX !=0 & ReturnZ !=0 & FocalX > 0.7 )) # n=9  # & !is.na(next_FocalY)  & !is.na(pre_FocalX) & !is.na(pre_CaptureX) & !is.na(pre_ReturnX))  )
        data.frame(testdata1.2 %>% # filter ones that only have previous locations
                     select(Order,Fish.no,Velocity,pre_FocalX:pre_FocalZ,pre_CaptureX:pre_CaptureZ,pre_ReturnX:pre_ReturnZ,FocalX:FocalZ,CaptureX:CaptureZ,ReturnX:ReturnZ,next_FocalX:next_FocalZ) %>%
                     filter(FocalX !=0 & CaptureX !=0 & ReturnZ !=0 & FocalX < 0.2 )) # n=29 # & !is.na(next_FocalY)  & !is.na(pre_FocalX) & !is.na(pre_CaptureX) & !is.na(pre_ReturnX))  )
        
        data.frame(testdata1.2 %>% # filter ones that only have previous locations
                     select(Order,Fish.no,Velocity,pre_FocalX:pre_FocalZ,pre_CaptureX:pre_CaptureZ,pre_ReturnX:pre_ReturnZ,FocalX:FocalZ,CaptureX:CaptureZ,ReturnX:ReturnZ,next_FocalX:next_FocalZ) %>%
                     filter(FocalX !=0 & CaptureX !=0 & ReturnZ !=0 & (FocalY > 0.7 | FocalY < 0.3) )) # n=60 # & !is.na(next_FocalY)  & !is.na(pre_FocalX) & !is.na(pre_CaptureX) & !is.na(pre_ReturnX))  )
        # 불과 얼마 안 지나서 끝까지 가네.. 그러니까 일부가 Y axis 를 움직이는것 같은데??
        sort(testdata1.2$FocalY)[nrow(testdata1.2)*0.9]
        sort(testdata1.2$FocalY)[nrow(testdata1.2)*0.1]    
        
      
      
##########################################################################
##### 3. Statistical analyses: when/why focal point locations change ####
# library(lme4)
# library(MuMIn)
# library("PerformanceAnalytics")
  
        
# Filter dataset that have both pre_focal/capture and current focal. Therefore, without zeros and NAs in those variables..
        testdata1.3<-testdata1.2 %>%
  filter(Capture !=0 &FocalX !=0 & !is.na(FocalX) & pre_FocalX !=0  & !is.na(pre_FocalX) & !is.na(pre_CaptureX) & !is.na(pre_Feeder.low.high))
nrow(testdata1.3) # n=413
str(testdata1.3)

testdata1.3 %>%
  ggplot( aes(x=(Velocity),color=Fish.no ) )+
  geom_histogram(fill="white")


    # 3.0 Gradient-Boosting

    
    str(testdata1.3)
    # pre_FocalX, pre_CaptureX, pre_Feeder.left.right ( vs. pre_Feeder.low.high),
    #  Velocity, Order, Feeder.left.right; pre_Feeder.left.right+ pre_Feeder.low.high + pre_Feeder.left.right.low.high
    #  pre_Feeder left.right vs lowhigh vs leftrightlowhigh ; feeder and capture positions will overlap
table(testdata1.3$Prey.no)
    testdata1.3[testdata1.3$Prey.no == 66.2,]
    
    # Aug.16,2021; Put fish.length in the models and test them...
    GB_data<-testdata1.3 %>%
      select(FocalX,FocalY,FocalZ,Fish.no,Velocity,Prey.no,Feeder.no,pre_FocalX,pre_FocalY,pre_FocalZ,pre_CaptureX,pre_CaptureY,pre_CaptureZ,
             FeederY,FeederZ,Fish.length,Order)
    #GB_data$pre_Feeder.left.right <- factor(GB_data$pre_Feeder.left.right)
    #GB_data$pre_Feeder.low.high <- factor(GB_data$pre_Feeder.low.high)
    #GB_data$pre_Feeder.left.right.low.high <- factor(GB_data$pre_Feeder.left.right.low.high)
    GB_data$Fish.no <- factor(GB_data$Fish.no)
    GB_data$Feeder.no <- factor(GB_data$Feeder.no)
    GB_data$Prey.no.f <- factor(GB_data$Prey.no)
    str(GB_data)
    
    

        # 3.0.1 divide train vs. test data        
          indexes = createDataPartition(GB_data$FocalX, p = .7, list = F)
          #indexes <- sample(nrow(testdata1.3), 0.7*nrow(testdata1.3), replace = FALSE)
          train <- GB_data[indexes, ];nrow(train);
          test <- GB_data[-indexes, ];nrow(test); 
          
          str(GB_data)
          str(train);names(train)
        
        # 3.0.2 set gbm model
          # X axis
          mod_gbmX = gbm(FocalX ~.,
                        data = train[-c(2,3,4,6,7,18)],
                        distribution = "gaussian",
                        cv.folds = 10,
                        shrinkage = .001,
                        n.trees = 10000,
                        interaction.depth=3) 
          par(mfrow=c(1,1))
          print(mod_gbmX)
          sqrt(min(mod_gbmX$cv.error)) # 0.1142973
          gbm.perf(mod_gbmX, method = "cv")
          par(mar = c(5, 8, 1, 1)) 
          summary(mod_gbmX)
         
          
          predX = predict.gbm(object = mod_gbmX,
                             newdata = test,
                             n.trees = 10000, # later change to 5000
                             type = "response")
          # accuracy test
          cbind(predX,test$FocalX,predX-test$FocalX)
          mean(predX-test$FocalX) # -0.009675741
          mean(abs(predX-test$FocalX))#  0.06947218
          # Plot relative influence
          par(mar = c(5, 8, 1, 1)) 
          summary(mod_gbmX)
          summary(
            mod_gbmX, 
            cBars = 10,
            method = relative.influence, # also can use permutation.test.gbm
            las = 2
          )
          
          hist(train$FocalY)
          
          # Y axis # We should use FocalY instead because we don't have gammma distribution
          mod_gbmY = gbm((FocalY)  ~.,
                         data = train[-c(1,3,4,6,7,18)],
                         distribution = "gaussian",
                         cv.folds = 10,
                         shrinkage = .001,
                         n.trees = 10000,
                         interaction.depth=3) 
          
          print(mod_gbmY)
          sqrt(min(mod_gbmY$cv.error)) # 0.07706252
          gbm.perf(mod_gbmY, method = "cv")
          summary(mod_gbmY)
          
          predY = predict.gbm(object = mod_gbmY,
                              newdata = test,
                              n.trees = 10000, # later change to 5000
                              type = "response")
          #accuracy test
          cbind(predY,test$FocalY,predY-test$FocalY)
          plot(predY,test$FocalY, xlim=c(0,1), ylim=c(0,1))
          mean(predY-test$FocalY) # 0.01031744
          sd(predY-test$FocalY) # 0.0736965
          mean(abs(predY-test$FocalY)) # 0.05258342
          # Plot relative influence
          par(mar = c(5, 8, 1, 1)) 
          summary(mod_gbmY)
          summary(
            mod_gbmY, 
            cBars = 10,
            method = relative.influence, # also can use permutation.test.gbm
            las = 2
          )
          
         
          # Z axis
          mod_gbmZ = gbm(FocalZ ~.,
                         data = train[-c(1,2,4,6,7,18)],
                         distribution = "gaussian",
                         cv.folds = 10,
                         shrinkage = .001,
                         n.trees = 10000,
                         interaction.depth=3) 
          
          print(mod_gbmZ)
          sqrt(min(mod_gbmZ$cv.error)) #0.02513185
          gbm.perf(mod_gbmZ, method = "cv")
          summary(mod_gbmZ)
          
          predZ = predict.gbm(object = mod_gbmZ,
                              newdata = test,
                              n.trees = 10000, # later change to 5000
                              type = "response")
          #accuracy test        
          cbind(predZ,test$FocalZ,predZ-test$FocalZ)
          mean(predZ-test$FocalZ)  # 0.002579908
          sd(predZ-test$FocalZ) # 0.03070244
          mean(abs(predZ-test$FocalZ)) # 0.0166625
          
                # Plot relative influence
                par(mar = c(5, 8, 1, 1)) 
                summary(mod_gbmZ)
                summary(
                  mod_gbmZ, 
                  cBars = 10,
                  method = relative.influence, # also can use permutation.test.gbm
                  las = 2
                )

  # 3.1 Focal X    
    # 3.1.1 Linear Model
       par(mar= c(5.1, 4.1, 4.1, 2.1))
          str(testdata1.3)
      hist(testdata1.3$FocalX)
      shapiro.test(testdata1.3$FocalX)
      
    # 3.1.3 GLMM
      summary(testdata1.3$FocalX);nrow(testdata1.3);str(testdata1.3)

        # Correlation test
          str(testdata1.3)
          names(testdata1.3)
          
          # Need testing: Velocity, Prey.no, Feeder.no, 
          # pre_FocalX, pre_FocalY, pre_FocalZ, 
          # pre_CaptureX,pre_CaptureY,pre_CaptureZ
          # pre_Feeder locations (updownleftright)
   
          
          my_data <- testdata1.3[, c(2,3,7,8,33:38,48,53)]
          chart.Correlation(my_data, histogram=TRUE, pch=19)
          
          # pre_FocalX vs pre_CaptureX
          cor.test(testdata1.3$pre_FocalX, testdata1.3$pre_CaptureX, data=testdata1.3)
          plot(testdata1.3$pre_FocalX, testdata1.3$pre_CaptureX)
          plot(testdata1.3$Velocity, testdata1.3$Fish.length)
          names(testdata1.3)
        #  pre_FocalX, pre_CaptureX, pre_Feeder.left.right ( vs. pre_Feeder.low.high),
        #  Velocity, Order, Feeder.left.right; pre_Feeder.left.right+ pre_Feeder.low.high + pre_Feeder.left.right.low.high

      # Test fish 
          testdata1.3 %>%
            ggplot( aes(x=CaptureX, y=CaptureY, color=as.factor(Fish.no) ) )+
              geom_point()
          testdata1.3 %>%
            ggplot( aes(x=as.factor(Fish.no), y=CaptureX) )+
            geom_boxplot()
          
      # pre_Feeder left.right vs lowhigh vs leftrightlowhigh ; feeder and capture positions will overlap
          # Conclusion: I should choose either Feederlocation or Capture locations
          testdata1.3 %>%
            ggplot( aes(x=CaptureY, y=CaptureZ, color=Feeder.left.right.low.high))+
            geom_point()
          testdata1.3 %>%
            ggplot( aes(x=CaptureX, y=CaptureY, color=Feeder.left.right.low.high))+
            geom_point()
          testdata1.3 %>%
            ggplot( aes(x=CaptureX, y=CaptureZ, color=Feeder.left.right.low.high))+
            geom_point()
          
          summary(testdata1.3$FocalX)
          
      m1.1<-(glmer(FocalX~Fish.length+pre_FocalX+pre_CaptureX+Velocity+scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      vif(m1.1);
      m1.2<-(glmer(FocalX~Fish.length+pre_FocalX+             Velocity+scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.3<-(glmer(FocalX~Fish.length+pre_FocalX+                      scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.4<-(glmer(FocalX~Fish.length+pre_FocalX+                                                    (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.5<-(glmer(FocalX~Fish.length+                        Velocity+                              (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.6<-(glmer(FocalX~            pre_FocalX+             Velocity+                              (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.7<-(glmer(FocalX~                                    Velocity+                              (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.8<-(glmer(FocalX~                       pre_CaptureX+                                       (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.9<-(glmer(FocalX~            pre_FocalX+                                                    (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      m1.10<-(glmer(FocalX~           pre_FocalX+                      scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      
      m1_Null<-(glmer(FocalX~(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
         
      summary(m1.1)
      str(testdata1.3)
       # AIC check
      modelsX<-list(m1.1,m1.2,m1.3,m1.4,m1.5,m1.6,m1.7,m1.8,m1.9,m1.10,m1_Null)
      model.namesX<-c('m1.1','m1.2','m1.3','m1.4','m1.5','m1.6','m1.7','m1.8','m1.9','m1.10','m1_Null')
      
      (summaryAICX<-aictab(modelsX,model.namesX,second.ord=FALSE))
      
       # m1.9,m1.4
       summary(m1.9); summary(m1.4)
       plot(m1.9)
       nrow(testdata1.3)
       residuals(m1.9)
       #rsq(m1.5) # # round off error however for m1.4.,only explains 30.8 % of the variance
       #rsq(m4.5) # lmer models 41 % but weird... with only one varialbe how could it be so high?
       
       # Plot predicted vs actual (see how accurate my model is)
       ggplot(testdata1.3, aes(x=FocalX, y=predict(m1.9, type='response')))+
         geom_point()+
         xlim(0,1)+
         ylim(0,1)+
         geom_abline(intercept = 0, slope = 1, color="red", 
                     linetype="dashed", size=1.5) # difficult to have an accurate model....
            
         
             # Model averaging: Not as accurate when preeicted..
              # GLMM
  
             selectionTableX<-aictab(modelsX,model.namesX, second.ord = FALSE)
             selectionTableX; # summary(m1.4)
             confset(cand.set=Cand.modelsX)
             evidence(aic.table=selectionTableX)
             
             modavg(cand.set=modelsX,model.namesX, parm="pre_FocalX")
             modavgShrink(cand.set=modelsX,model.namesX, parm="pre_FocalX")
             modavgShrink(cand.set=modelsX,model.namesX, parm="Fish.length")
             
            # prediction doesn't work  for the gamma distribution
             fam.link.mer(m1.4)
             
             maverage<-modavgPred(cand.set = modelsX, modnames = model.namesX, newdata=testdata1.3,
                        gamdisp = gamma.dispersion(m1.4),
                        second.ord = TRUE, nobs = NULL, uncond.se = "revised",
                        conf.level = 0.95,c.hat = 1,
                        type = "response") 
            
        # In common the Fish.length and the pre_FocalX is significant
       
       ggplot(testdata1.3, aes(x=Fish.length, y=FocalX))+
         geom_point()+
         geom_smooth(method=lm)
       ggplot(testdata1.3, aes(x=pre_FocalX, y=FocalX))+
         geom_point()+
         geom_smooth(method=lm)

      
  # 3.2 FocalY   
   # 3.2.1 Does FocalY change depending on the velocity?  
      par(mfrow=c(1,1))
      hist(testdata1.3$FocalY)
      shapiro.test(testdata1.3$FocalY)
   
    # 3.2.2 GLMM Analysis  
      summary(testdata1.3$FocalY) # None of the Y are smaller than 0 which is good. 
      summary(testdata1.3$FocalY);nrow(testdata1.3);str(testdata1.3)
      
      # Correlation test
      str(testdata1.3)
      names(testdata1.3)
      
      # Need testing: Velocity, Order, Feeder.no, 
      # pre_FocalX, pre_FocalY, pre_FocalZ, 
      # pre_CaptureX,pre_CaptureY,pre_CaptureZ
 
      plot(as.factor(testdata1.3$pre_Feeder.left.right), testdata1.3$pre_CaptureY) # should not use pre_Feeder.left.right
      
      my_data <- testdata1.3[, c(2,3,6,34,35:38,48,53)]
      chart.Correlation(my_data, histogram=TRUE, pch=19)
      
      names(testdata1.3)

      n1.1<-(glmer(FocalY~Fish.length+pre_FocalY+pre_CaptureY+Velocity+FeederY+ scale(Preyintro_interval_new)+ (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      vif(n1.1)
      n1.2<-(glmer(FocalY~            pre_FocalY+pre_CaptureY+Velocity+FeederY+ scale(Preyintro_interval_new) +(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.3<-(glmer(FocalY~            pre_FocalY+pre_CaptureY+Velocity+FeederY+                              + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.4<-(glmer(FocalY~            pre_FocalY+pre_CaptureY+         FeederY+                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.5<-(glmer(FocalY~            pre_FocalY+                     +FeederY+                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
     
      n1.6<-(glmer(FocalY~            pre_FocalY+pre_CaptureY+Velocity        +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.7<-(glmer(FocalY~Fish.length+pre_FocalY+pre_CaptureY+Velocity        +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.8<-(glmer(FocalY~            pre_FocalY+pre_CaptureY+Velocity        +  scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.9<-(glmer(FocalY~            pre_FocalY+pre_CaptureY         +FeederY+  scale(Preyintro_interval_new)+(1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.10<-(glmer(FocalY~Fish.length+pre_FocalY+pre_CaptureY                 +                               (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      n1.11<-(glmer(FocalY~Fish.length+pre_FocalY+pre_CaptureY+         FeederY+                               (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      
      n1_Null<-glmer(FocalY~    (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=0, data=testdata1.3)
      
     library(ggplot2)
      #AICc check
      modelsY<-list(n1.1,n1.2,n1.3,n1.4,n1.5,n1.6,n1.7,n1.8,n1.9,n1.10,n1.11,n1_Null)
      model.namesY<-c('n1.1','n1.2','n1.3','n1.4','n1.5','n1.6','n1.7','n1.8','n1.9','n1.10','n1.11','n1_Null')
      
      (summaryAICY<-aictab(modelsY,model.namesY,second.ord=FALSE))
      
      # Significant variables: pre_FocalY+pre_CaptureY+FeederY+Velocity
      summary(n1.3);summary(n1.2);summary(n1.1)
      # rsq(n1.2) # doesn't work
      
      # Plot predicted vs actual (see how accurate my model is)
      ggplot(testdata1.3, aes(x=FocalY,y=predict(n1.3, type='response')))+
        geom_point()+
        xlim(0.15,0.8)+ylim(0.15,0.8)+
        geom_abline(intercept = 0, slope = 1, color="red", 
                    linetype="dashed", size=1.5) # difficult to have an accurate model....


      # Plot results
      ggplot(testdata1.3,aes(x=pre_FocalY,y=FocalY) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=pre_CaptureY,y=FocalY) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=FeederY, y=FocalY) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=Velocity, y=FocalY) )+ 
        geom_point()+
        geom_smooth(method=lm)


     
  # 3.3 FocalZ   
      # Make FocalZ values that are above the surface to zero
      summary(testdata1.3$FocalZ)
      testdata1.3$CaptureZ[testdata1.3$CaptureZ > 0.3]<- 0.3
      testdata1.3$pre_CaptureZ[testdata1.3$pre_CaptureZ > 0.3]<- 0
      testdata1.3[testdata1.3$pre_CaptureZ > 0.3,]
      
      # 3.3.1 Does FocalZ change depending on the velocity?  

      par(mfrow=c(1,1))
      hist(testdata1.3$FocalZ)
      summary(lm(FocalY~Velocity, data=testdata1.3))
      plot(FocalZ~Velocity, data=testdata1.3)
     ## Important test to select variables; therefore I have to choose oen variable out of the two
      plot(as.factor(testdata1.3$pre_Feeder.low.high), testdata1.3$pre_CaptureZ) # remove pre_Feeder.low.high
      
      # 3.3.2 GLMM Analysis  
      summary(testdata1.3$FocalZ);nrow(testdata1.3);str(testdata1.3)
      
      names(testdata1.3)
      cbind(testdata1$FocalZ,
      testdata1.3$FocalZ,abs(testdata1$FocalZ)+testdata1.3$FocalZ)
      ## 
      p3.1<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+ pre_FocalZ + scale(Fish.length)+ FeederZ+ scale(Preyintro_interval_new) + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      vif(p3.1)
      p3.2<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+ pre_FocalZ + scale(Fish.length)+         + scale(Preyintro_interval_new)+ (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.3<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+ pre_FocalZ + scale(Fish.length)+                                        + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      
      p3.4<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+            + scale(Fish.length)+         +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=-1, data=testdata1.3))
      p3.5<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+                          + scale(Fish.length)+         +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.6<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+             + pre_FocalZ + scale(Fish.length)+ FeederZ +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.7<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+             + pre_FocalZ + scale(Fish.length)+         +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.8<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+             + pre_FocalZ +                      +                                       (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.9<-(glmer(FocalZ~             + Velocity+ pre_CaptureX+ pre_FocalZ + scale(Fish.length)+ FeederZ +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.10<-(glmer(FocalZ~             + Velocity+ pre_CaptureX+ pre_FocalZ + scale(Fish.length)+         +                                (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.11<-(glmer(FocalZ~             + Velocity+ pre_CaptureX+ pre_FocalZ +                     +                                       (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.12<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+            + scale(Fish.length)+FeederZ+                                 (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.13<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX+            + scale(Fish.length)+          scale(Preyintro_interval_new) + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.14<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+ pre_CaptureX                                                                         + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.15<-(glmer(FocalZ~ pre_CaptureZ+ Velocity+                                                                                      + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      p3.16<-(glmer(FocalZ~               Velocity+ pre_CaptureX                                                                         + (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      
      p_Null<-(glmer(FocalZ~  (1|Fish.no)+(1|Feeder.no), family=Gamma, nAGQ=1, data=testdata1.3))
      
      summary(p3.4)

      #AICc check
      modelsZ<-list(p3.1,p3.2,p3.3,p3.4,p3.5,p3.6,p3.7,p3.8,p3.9,p3.10,p3.11,p3.12,p3.13,p3.14,p3.15,p3.16, p_Null)
      model.namesZ<-c('p3.1','p3.2','p3.3','p3.4','p3.5','p3.6','p3.7','p3.8','p3.9','p3.10','p3.11','p3.12','p3.13','p3.14','p3.15','p3.16','p_Null')
      
      (summaryAICZ<-aictab(modelsZ,model.namesZ,second.ord=FALSE) )
      
      summary(p3.4); 
      summary(p3.13);
      
      
      # Significant variables: pre_CaptureZ+Velocity+pre_CaptureX+Fish.length+(pre_FocalZ)+scale(Preyintro_interval_new)
      # Plot predicted vs actual (see how accurate my model is)
      ggplot(testdata1.3, aes(x=FocalZ,y=predict(p3.4, type='response')))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color="red", 
                    linetype="dashed", size=1.5)+ # difficult to have an accurate model....
        xlim(0,0.3)+
        ylim(0,0.15)
      
      hist(testdata1.3$FocalZ)
      # Significant variables: pre_CaptureZ+Velocity+pre_CaptureX+Fish.length+(pre_FocalZ)+scale(Preyintro_interval_new)
      

      ggplot(testdata1.3,aes(x=pre_CaptureZ,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=Velocity,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=pre_CaptureX,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=Fish.length,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=pre_FocalZ,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      ggplot(testdata1.3,aes(x=Preyintro_interval_new,y=FocalZ) )+
        geom_point()+
        geom_smooth(method=lm)
      
      




