#######################################################################################
######################## Aug. 10, 2021, Piccolo files extended ########################
##############################  Why do fish miss a prey? ##############################
#######################################################################################
## Q) When/why do fish miss a prey?
library(car)
library(ggplot2)
library(dplyr)
library(rsq) 
library(car); # vif 
library(AICcmodavg) 
library(PerformanceAnalytics)
library(lme4)


#Accuracy test for binomial:  https://rstudio-pubs-static.s3.amazonaws.com/153750_24869b0fb71f4b9f9a6c3bc6747bb12d.html



# 1. Filter only the real prey.no (integers). 
# Because non-integer numbers are multiple attemps within one prey...
# and would end up not non-success and may also overlap with captures as well.
testdata6<-testdata1_scaled_velocity.block.abs4%>%
  mutate(Order_total=1:nrow(testdata1_scaled_velocity.block.abs4))

      # Find ones that have multiple Prey.no's and capture failure
        data.frame(testdata6%>%
                   select(Order_total,Fish.no,Velocity,Order, Prey.no, Capture,FocalX) %>%
                  filter( Prey.no-floor(Prey.no) > 0.05 ) )
      
      #  Remove the rows (not captuer but multiple movements) from testdata6
          testdata6.1<-testdata6[-c(463,498,994,1274,1303,1543,1668,1773,2479,2547,2558,
                             2572,2576,2595,2602,2609),]
      # Data varefication to see each have 100 counts    
          data.frame(testdata6.1 %>%
            group_by(Fish.no, Velocity) %>%
              summarise(numbersofeach=n() ) 
              )

# 2. Location of feeders
    # subsetting data (testdata1.1.1) for faster computation (only one velocity as the feeder XY are the same anyways)
    testdata6.2<-testdata6.1 %>%
      filter(Velocity==0.29)
      
    ggplot(testdata6.2, aes(x=FeederY, y=FeederZ))+
      geom_point()+
      xlim(0,1)+
      ylim(0,0.3)+
      geom_text(label=testdata6.2$Feeder.no,vjust=2)
    

# 3. Capture locations in relation to the feeder locations. To see where the prey items flow in relation to the feeder location

    str(testdata6.1)
      testdata6.1$Feeder.no<-factor(testdata6.1$Feeder.no)
      
      meanCapture_feeder.no<-testdata6.1%>%
        group_by(Feeder.no)%>%
        summarise(meanCapY=mean(FeederY,na.rm=TRUE), meanCapZ=mean(FeederZ,na.rm=TRUE) )
      
      ggplot (testdata6.1, aes(x=CaptureY, y=CaptureZ, col=as.factor(Feeder.no)) )+
        geom_point()+
        facet_wrap(~Feeder.no,nrow=2)+
        xlim(0,1)+
        ylim(0,0.3)+
        theme(legend.position="none")+
        geom_point(data=data.frame(meanCapture_feeder.no), mapping=aes(x=meanCapY,y=meanCapZ), col="black")
        

# 4. Distance from median focal point to the feeder (YZ)

Dist.median.fish<-matrix(0,nrow(testdata6.1),1)
median.FocalY<-median(testdata6.1$FocalY, na.rm=TRUE);
median.FocalZ<-median(testdata6.1$FocalZ, na.rm=TRUE)

    # Fill Dist.median.fish
      for (i in 1:nrow(testdata6.1) ){
      Dist.median.fish[i]<-sqrt( (median.FocalY-testdata6.1$FeederY[i])^2 + (median.FocalZ-testdata6.1$FeederZ[i])^2 )
      }
      str(Dist.median.fish)
      hist(Dist.median.fish, main="Dist; medianfish to feeders")
    
      testdata6.1$Dist.median.fish<-Dist.median.fish

    # Standardized FeederY
      FeederY.std<-matrix(0,nrow(testdata6.1),1)
        for (k in 1:nrow(testdata6.1)){
          if(testdata6.1$FeederY[k] <0.5){
            FeederY.std[k]<- 0.5-testdata6.1$FeederY[k]
          } else{ FeederY.std[k]<- testdata6.1$FeederY[k]-0.5
            }
        }
hist(FeederY.std)          
testdata6.1$FeederY.std<-FeederY.std


# 5. Correlation test among variables
    testdata6.1
    names(testdata6.1)
    str(testdata6.1)
    names(testdata6.1)
    testdata6.1 %>%
      select(FocalX,FocalY,FocalZ) %>%
      summary()
    
    variable.check <- testdata6.1[, c(2,3,8,48,49,54,57)]
    str(variable.check)
    par(mfrow=c(1,1))
    chart.Correlation(variable.check, histogram=TRUE, pch=19)
    cor.test(testdata6.1$dist.pre.feeder, testdata6.1$FeederY.std)
    
    str(testdata6.1)
    hist(testdata6.1$FeederY.std)
    
# 6. Model selection (GLMM)
    
    str(testdata6.1)
    # previousfeedercenter<-which(testdata6.1$FeederY > 0.40 & testdata6.1$FeederY < 0.60 )+1 # row number where the previous feeder came through the center range
    table(testdata6.1$dist.pre.feeder)
    q1<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
     # check VIF using car package
      vif(q1)
    q2<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    q3<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+                       (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
    q4<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+       +scale(Preyintro_interval_new)+                       (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
    q5<-(glmer(Capture~                               +Velocity+FeederY.std+       +scale(Preyintro_interval_new)+                       (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
    q6<-(glmer(Capture~                               +Velocity+FeederY.std+                                     +dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
    q7<-(glmer(Capture~                               +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+                       (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
    q_Null<-(glmer(Capture~(1|Fish.no)+(1|Feeder.no), data=testdata6.1,nAGQ=1,family=binomial))
    summary(q2)
    q8<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+           +FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder      +(1|Fish.no)+(1|Feeder.no) , data=testdata6.1[-c(preynumberone),],nAGQ=1,family=binomial))
   
    closerones<-which(testdata6.1$FeederY.std <=0.15)
    # closerones<-which(testdata6.1$dist.pre.feeder >=0.4 & testdata6.1$dist.pre.feeder <=0.6)
   
    r1<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+ Velocity:scale(Preyintro_interval_new)+FeederY.std:dist.pre.feeder+  FeederY.std+FeederZ+scale(Preyintro_interval_new)+dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),], nAGQ=1,family=binomial))
    vif(r1)
    r2<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+                                        FeederY.std:dist.pre.feeder+  FeederY.std+FeederZ+scale(Preyintro_interval_new)+dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),], nAGQ=1,family=binomial))
    r3<-(glmer(Capture~scale(Order)+                  +Velocity+                                        FeederY.std:dist.pre.feeder+  FeederY.std+FeederZ+scale(Preyintro_interval_new)+dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),], nAGQ=0,family=binomial))
    r4<-(glmer(Capture~scale(Order)+                  +Velocity+                                        dist.pre.feeder:FeederY.std+  FeederY.std+FeederZ+scale(Preyintro_interval_new)+dist.pre.feeder+(1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),], nAGQ=0,family=binomial))
    
    
    Velocity + FeederY.std + FeederZ + scale(Preyintro_interval_new) + dist.pre.feeder
    Velocity:scale(Preyintro_interval_new)
    FeederY.std:dist.pre.feeder
    FeederZ: dist.pre.feeder
    
    s1<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+I(Velocity^2)+FeederY.std+I(FeederY.std^2)+FeederZ+scale(Preyintro_interval_new)+ I(scale(Preyintro_interval_new)^2)+ dist.pre.feeder+ I(dist.pre.feeder^2)+  (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    summary(t1)
    s2<-(glmer(Capture~scale(Order)+                  Velocity+I(Velocity^2)+FeederY.std+I(FeederY.std^2)+FeederZ+scale(Preyintro_interval_new)+ I(scale(Preyintro_interval_new)^2)+ dist.pre.feeder+ I(dist.pre.feeder^2)+  (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    s3<-(glmer(Capture~scale(Order)+                  Velocity+I(Velocity^2)+FeederY.std+                +FeederZ+scale(Preyintro_interval_new)+ I(scale(Preyintro_interval_new)^2)+ dist.pre.feeder+ I(dist.pre.feeder^2)+  (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    s4<-(glmer(Capture~scale(Order)+                  Velocity+I(Velocity^2)+FeederY.std+                +FeederZ+scale(Preyintro_interval_new)+ I(scale(Preyintro_interval_new)^2)+ dist.pre.feeder                      +  (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],                    nAGQ=0,family=binomial))

    
    t1<-(glmer(Capture~scale(Order)+scale(Fish.length)+Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 Velocity:scale(Preyintro_interval_new)+FeederY.std:dist.pre.feeder +
                 I(Velocity^2)+ I(FeederY.std^2)+ I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t2<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 Velocity:scale(Preyintro_interval_new)+FeederY.std:dist.pre.feeder +
                 I(Velocity^2)+ I(FeederY.std^2)+ I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t3<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 Velocity:scale(Preyintro_interval_new)+
                 I(Velocity^2)+ I(FeederY.std^2)+ I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t4<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                                                      +FeederY.std:dist.pre.feeder +
                 I(Velocity^2)+ I(FeederY.std^2)+ I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t5<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 
                 I(Velocity^2)+ I(FeederY.std^2)+ I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t6<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 
                 I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+ I(dist.pre.feeder^2)+ (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t7<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 
                 I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+                     + (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t8<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ 
                 
                 I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+                     + (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t9<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+      +scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                 
                 I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+                     + (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t10<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                                                       FeederY.std:dist.pre.feeder+
                 I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+                     + (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    t11<-(glmer(Capture~scale(Order)+                  +Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ 
                  FeederY.std:dist.pre.feeder+
                  I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+                     + (1|Fish.no)+(1|Feeder.no), data=testdata6.1[-c(preynumberone),],nAGQ=0,family=binomial))
    
    summary(t7)
    
    
    # Specifically testing whether the fish are looking on one side
    
      # 1. Divide vertically spaces into group 1-4 left to right.
      # 2. Previous feeder is from group 1. Test the capture success of groups 2 and 3 (equi-distance from the center)
      # Therefore, select previous feeders that are from group 1 or group 4
          # then, select the current feeders that are from groups 2 or 3.
          # try comparing the capture success between groups 2 and 3.
          # Summary: pre1: 2 vs 3 ,  pre4: 2 vs 3
    
    str(testdata6.1)
    testdata6.1 %>%
      filter(pre_Feeder.left.right == "Left") %>%
      group_by(Feeder.left.right) %>%
      summarise(Capturemean=mean(Capture),Capturesd=sd(Capture))
      
    
    
    
    
    
    
    
    library(mgcv)
    #install.packages("gamm4")
    library(gamm4)
    u1<-gamm4(Capture~ Order+s(Velocity,k=3)+s(FeederY.std,k=3)+s(FeederZ,k=3)+s(Preyintro_interval_new,k=3) + s(dist.pre.feeder,k=3), random=~(1|Fish.no), data=testdata6.1, family=binomial)
    
    
    testdata6.1[is.na(testdata6.1$Velocity)]
    testdata6.1[is.na(testdata6.1$Capture)]
    
    
    
    length(testdata6.1$dist.pre.feeder)
    str(testdata6.1[-c(preynumberone),])
    
    
    
    
    simulationOutput <- simulateResiduals(fittedModel=TMB1, refit=F)
    plot(simulationOutput)
   
    
    install.packages("DHARMa")
    library(DHARMa)
    install.packages("glmmTMB")
    citation("DHARMa")
    
    
  library(glmmTMB)  
    library(lme4)
    library(Matrix)
     TMB1<-summary(glmmTMB(Capture~scale(Order)+Velocity+FeederY.std+FeederZ+scale(Preyintro_interval_new)+ dist.pre.feeder+ 
                       
                       I(Velocity^2)+                 + I(scale(Preyintro_interval_new)^2)+    
                       (1|Fish.no) +(1|Feeder.no), data=testdata6.1[-preynumberone,], family=binomial ) )
    
     
     
    rsq(s4) # explains 41.13 % of variance
     plot(c(1:nrow(testdata6.1) ),residuals(R2))
     
     ggplot(aes(x=dist.pre.feeder, y=FeederY.std), data=testdata6.1[closerones,])+
       geom_point()
     
    range(testdata6.1[-c(preynumberone),]$dist.pre.feeder)
    xwei<-seq(0.05,0.85,0.01)
    modelwi<-predict(q8, type="response")
    plot(y=testdata6.1$Capture,x=testdata6.1$dist.pre.feeder)
    lines(xwei,modelwi)
    
    #AICc check
    
    modelsCP<-list(q1,q2,q3,q4,q5,q6,q7,q_Null,q8,r1,r2,r3,r4,s4,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)
    model.namesCP<-c('q1','q2','q3','q4','q5','q6','q7','q_Null','q8','r1','r2','r3','r4','s4','t1','t2','t3','t4','t5','t6','t7','t8','t9','t10','t11')
    (summaryAIC.CP<-aictab(modelsCP,model.namesCP,second.ord=FALSE) )
    
    
    library(MuMIn)
    r.squaredGLMM(t7) #0.59288
    
    # significant variables: scale(Order),Velocity,FeederY.std,FeederZ,Preyintro_interval_new, 
##   
    
    
    
    
    # Plot significant variables
      # 1.Order
          summary(testdata6.1$Prey.no)
          testdata6.1 %>%
            select(Prey.no,Capture) %>%
            mutate(Prey.no.cat=case_when(Prey.no > 0 & Prey.no < 21~ 1  ,
                                         Prey.no > 20 & Prey.no < 41~ 2,
                                         Prey.no > 40 & Prey.no < 61~3,
                                         Prey.no > 60 & Prey.no < 81~4,
                                         Prey.no > 80 & Prey.no < 101~5) ) %>%
            
            group_by(Prey.no.cat) %>%
            summarise(Capture.mean.Prey.no=mean(Capture)) %>%
               ggplot(aes(x=Prey.no.cat, y=Capture.mean.Prey.no))   +
                geom_point() +
                geom_line() +
            ylim(0,0.5)+
                ylab("Mean capture success")+
                xlab("Prey order in quantiles (1-100)")
          
      # 2. Velocity
          summary(testdata6.1$Velocity)
          testdata6.1 %>%
            select(Velocity,Capture) %>%
            mutate(Velocity.cat=case_when(Velocity >= min(Velocity) & Velocity <= min(Velocity)+(max(Velocity)-min(Velocity))*0.25   ~ "V(0.29-0.28)",
                                          Velocity > min(Velocity)+(max(Velocity)-min(Velocity))*0.25 & Velocity <= min(Velocity)+(max(Velocity)-min(Velocity))*0.5  ~ "V(0.38-0.47)",
                                          Velocity > min(Velocity)+(max(Velocity)-min(Velocity))*0.5  & Velocity <= min(Velocity)+(max(Velocity)-min(Velocity))*0.75 ~ "V(0.47-0.55)",
                                          Velocity > min(Velocity)+(max(Velocity)-min(Velocity))*0.75  & Velocity <= max(Velocity)    ~"V(0.55-0.58)" ) ) %>%
            
            group_by(Velocity.cat) %>%
            summarise(Capture.mean.Velocity=mean(Capture)) %>%
            ggplot(aes(x=as.factor(Velocity.cat), y=Capture.mean.Velocity,group=1))    +
            geom_point() +
            geom_line() +
            ylim(0,1)+
            ylab("Mean capture success")+
            xlab("Velocity in quantiles")
          
          library(dplyr)
          library(ggplot2)
          library(tidyr)
      #3. FeederY.std (distance from the FeederY; distance from the center)
          summary(testdata6.1$FeederY.std)
          testdata6.1 %>%
            select(FeederY.std,Capture) %>%
            mutate(FeederY.std.cat=case_when(FeederY.std >= min(FeederY.std) & FeederY.std <= min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.25   ~ "0.05-0.12",
                                          FeederY.std > min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.25 & FeederY.std <= min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.5  ~ "0.12-0.21",
                                          FeederY.std > min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.5  & FeederY.std <= min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.75 ~ "0.21-0.31",
                                          FeederY.std > min(FeederY.std)+(max(FeederY.std)-min(FeederY.std))*0.75  & FeederY.std <= max(FeederY.std)    ~"0.31-0.40" ) ) %>%
            
            group_by(FeederY.std.cat) %>%
            summarise(Capture.mean.FeederY.std=mean(Capture)) %>%
            ggplot(aes(x=as.factor(FeederY.std.cat), y=Capture.mean.FeederY.std,group=1))    +
            geom_point() +
            geom_line() +
            ylim(0,0.8)+
            ylab("Mean capture success")+
            xlab("FeederY distance (meters) from the center in quantiles")
        
          
      #4. Feeder Z (dist. from the FeederZ)

          summary(testdata6.1$FeederZ)
            
          testdata6.1 %>%
            select(FeederZ,Capture) %>%
            mutate(FeederZ.cat=case_when(FeederZ >= min(FeederZ) & FeederZ <= min(FeederZ)+(max(FeederZ)-min(FeederZ)) * 0.50   ~ "Lower",
                                         FeederZ > min(FeederZ)+(max(FeederZ)-min(FeederZ)) * 0.50  & FeederZ <= max(FeederZ) ~ "High"
                                              ) ) %>%
            
            group_by(FeederZ.cat) %>%
            summarise(Capture.mean.FeederZ=mean(Capture)) %>%
            ggplot(aes(x=FeederZ.cat, y=Capture.mean.FeederZ,group=1))    +
            geom_point() +
            geom_line() +
            ylim(0,0.5) +
            ylab("Mean capture success")+
            xlab("FeederZ distance (meters) in quantiles")
          
      #5. Preyintro_interval_new
          summary(testdata6.1$Preyintro_interval_new)
          str(testdata6.1) #$Preyintro_interval_new)
          testdata6.1 %>%
            select(Preyintro_interval_new,Capture) %>%
            mutate(Preyintro_interval_new.cat=case_when(Preyintro_interval_new >= min(Preyintro_interval_new) & Preyintro_interval_new <= min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.25   ~ "<10s(-4.60-9.99s)",
                                             Preyintro_interval_new > min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.25 & Preyintro_interval_new <= min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.5  ~ "<15s(9.99-14.70s)",
                                             Preyintro_interval_new > min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.5  & Preyintro_interval_new <= min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.75 ~ "<20s(14.70-19.62s)", 
                                             Preyintro_interval_new > min(Preyintro_interval_new)+(max(Preyintro_interval_new)-min(Preyintro_interval_new))*0.75  & Preyintro_interval_new <= max(Preyintro_interval_new) ~ "<35s(19.62-34.71s)"  ) ) %>%
            
            group_by(Preyintro_interval_new.cat) %>%
            summarise(Capture.mean.Preyintro_interval_new=mean(Capture)) %>%
            ggplot(aes(x=as.factor(Preyintro_interval_new.cat), y=Capture.mean.Preyintro_interval_new,group=1))    +
            geom_point() +
            geom_line() +
            ylim(0,0.5)+
            ylab("Mean capture success")+
            xlab("Preyintro_interval (Seconds) in quantiles")
          
          
          ggplot(testdata6.1, aes(x=Preyintro_interval_new,y=Capture)) +
            geom_point(alpha=0.01)
      
      # 6 .Distance to previous feeder to current feeder
          quartz()
      summary(testdata6.1$dist.pre.feeder)
      
      head(cbind(testdata6$dist.pre.feeder,testdata1.2$dist.pre.feeder),15)
      nrow(testdata6.1)
      nrow(testdata1.2)
      testdata6.1[-c(preynumberone),] %>%
        select(dist.pre.feeder, Capture) %>%
        filter(!is.na(dist.pre.feeder))%>%
       # mutate(dist.pre.feeder.cat=case_when(dist.pre.feeder >= min(dist.pre.feeder) & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.25   ~ "0~0.124",
      #                                       dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.25 & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.5  ~ "0.124~0.250",
      #                                       dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.5  & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.75 ~ "0.250~0.380", 
      #                                       dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.75  & dist.pre.feeder <= max(dist.pre.feeder) ~ "0.380~0.79"  ) ) %>%
        
        mutate(dist.pre.feeder.cat=case_when(dist.pre.feeder >= min(dist.pre.feeder) & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.20   ~ "First",
                                             dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.20 & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.40  ~ "Second",
                                             dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.40 & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.60  ~ "Third",
                                             dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.60 & dist.pre.feeder <= min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.80  ~ "Fourth",
                                             dist.pre.feeder > min(dist.pre.feeder)+(max(dist.pre.feeder)-min(dist.pre.feeder))*0.80  & dist.pre.feeder <= max(dist.pre.feeder) ~ "Last"  ) ) %>%
        group_by(dist.pre.feeder.cat) %>%
        summarise(Capture.mean.dist.pre.feeder.cat=mean(Capture)) %>%
        ggplot(aes(x=as.factor(dist.pre.feeder.cat), y=Capture.mean.dist.pre.feeder.cat,group=1) )   +
        geom_point() +
        geom_line() +
        ylim(0,0.5)+
        #scale_x_discrete(limits=c("0~0.124","0.124~0.250","0.250~0.380","0.380~0.79"))+
        scale_x_discrete(limits=c("First","Second","Third","Fourth","Last"))+
       ylab("Mean capture success")+
        xlab("Distance between previous and current feeder (m) in quantiles")
      
      
 testdata6.1 %>%
   select(Capture, dist.pre.feeder) %>%
   filter(dist.pre.feeder>0.6) %>%
   summarise(meandd=mean(Capture))
   ggplot(aes(x=dist.pre.feeder, y=Capture)) +
  
      
      
      
      
                  
      # 7 .Fish.length 
      summary(testdata6.1$Fish.length)
          testdata6.1 %>%
            select(Fish.length,Capture) %>%
            mutate(Fish.length.cat=case_when(Fish.length >= min(Fish.length) & Fish.length <= min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.25   ~ "0.066-0.072",
                                             Fish.length > min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.25 & Fish.length <= min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.5  ~ "0.072-0.076",
                                             Fish.length > min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.5  & Fish.length <= min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.75 ~ "0.076-0.080", 
                                             Fish.length > min(Fish.length)+(max(Fish.length)-min(Fish.length))*0.75  & Fish.length <= max(Fish.length) ~ "0.080-0.081"  ) ) %>%
            
            group_by(Fish.length.cat) %>%
            summarise(Capture.mean.Fish.length.cat=mean(Capture)) %>%
            ggplot(aes(x=as.factor(Fish.length.cat), y=Capture.mean.Fish.length.cat,group=1))    +
            geom_point() +
            geom_line() +
            ylim(0,0.5)+
            ylab("Mean capture success")+
            xlab("Fish sizes(m) in quantiles")
          
      
    
    # Model accuracy check
      rsq(s4) # explains 41.13 % of variance
      plot(c(1:nrow(testdata6.1[-c(preynumberone),]) ),residuals(s4))
      
               nrow(testdata6.1[-c(preynumberone),] )
      
      
    # Measure predicted vs actual (see how accurate my model is)
      binomial.accuracy<-data.frame(cbind(testdata6.1$Capture, predict(s4, type="response") ) )
      names(binomial.accuracy)
      # https://rstudio-pubs-static.s3.amazonaws.com/153750_24869b0fb71f4b9f9a6c3bc6747bb12d.html
      binomial.accuracy2<-binomial.accuracy %>%
        mutate(X3=1*(X2>0.5)+0) # haviung bracket creates conditional...
      sum(binomial.accuracy2$X3)/sum(binomial.accuracy2$X1) # 0.9100758
      
      





