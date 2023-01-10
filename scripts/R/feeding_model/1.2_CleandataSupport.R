###################################################################################
######################## Mar.13 2022, Piccolo files extended ######################
#############################  Functions and tests ################################
###################################################################################

# Things to do:
# 1: Look up map function in purr to simplify my codes


# 1. Visualize the Z coordinate: Visualization
# 2. Function: Visualize where the focal/detection points are to estimate correct scale
# 3. Check Z-scaling is correct
# 4. Fish size
# 5. Pipedelay time analysis

#### 1. Visualize the Z coordinate: Visualization ####
testdata1 %>%
  select(FocalY,CaptureY,ReturnY)%>%
  summary()

testdata1 %>%
  select(FocalX, FocalY, FocalZ,CaptureX,CaptureY,CaptureZ)%>%
  summary()

testdata1 %>%
  filter(FocalX !=0) %>%
  ggplot(aes(FocalX,color=as.factor(Fish.no)) ) +
  geom_histogram(fill="white", position="dodge")

testdata1 %>%
  filter(FocalX !=0) %>%
  ggplot(aes(FocalY,color=as.factor(Fish.no)) ) +
  geom_histogram(fill="white", position="dodge")

testdata1 %>%
  filter(FocalX !=0) %>%
  ggplot(aes(FocalZ,color=as.factor(Fish.no)) ) +
  geom_histogram(fill="white", position="dodge")



#### 2. Function 1: Visualize where the focal/detection points are to estimate correct scale ####

calibratefishfocaldetectionpoint<-function(a,b,c,d,e,f,g) { # a is velocity, b is depth ad c is additional calibration
  select<-dplyr::select
  
  if (b != 0.15){
  p1.1<-data.frame(testdata1 %>%
                     filter(Velocity==a & Depth ==b & Fish.no==d & !is.na(FocalZ)) %>%
                     select(FocalZ, DetectZ) %>%
                     mutate(fztrans= abs(-b-(FocalZ))-c,
                            dztrans= abs(-b-(DetectZ))-c)
  )%>%
    filter(fztrans<b*1.10 & dztrans<b*1.10)%>%
    summarise(fzmean=mean(fztrans),
              fzlow=min(fztrans),
              dzmin=min(dztrans),
              dzmean=mean(dztrans),
              dzmax=max(dztrans))
  
  
  p1<-testdata1 %>%
    filter(Velocity==a & Depth== b & Fish.no==d) %>%
    ggplot(aes(x=FocalX, y=abs(-b-FocalZ)-c ))+
    geom_point()+
    geom_point(aes(x=DetectX, y=abs(-b-DetectZ)-c),colour="red")+
    xlim(-0.5,1.2) +
    ylim(-0.2,0.65) +
    geom_text(x=0.5,y=0.65,label="Fish1")+
    geom_text(x=0.5,y=0.61,label=paste("fZmean: ",format(p1.1[1],digit=3) ))+
    geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p1.1[2],digit=3) ))+
    geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p1.1[3],digit=3) ))+
    geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p1.1[4],digit=3) ))+
    geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p1.1[5],digit=3) )) 
  
  
  p2.1<-data.frame(testdata1 %>%
                     filter(Velocity==a & Depth ==b & Fish.no==e & !is.na(FocalZ)) %>%
                     select(FocalZ, DetectZ) %>%
                     mutate(fztrans= abs(-b-(FocalZ))-c,
                            dztrans= abs(-b-(DetectZ))-c)
  )%>%
    filter(fztrans<b*1.10 & dztrans<b*1.10)%>%
    summarise(fzmean=mean(fztrans),
              fzlow=min(fztrans),
              dzmin=min(dztrans),
              dzmean=mean(dztrans),
              dzmax=max(dztrans))
  p2<-testdata1 %>%
    filter(Velocity==a & Depth==b & Fish.no==e) %>%
    ggplot(aes(x=FocalX, y=abs(-b-FocalZ)-c ))+
    geom_point()+
    geom_point(aes(x=DetectX, y=abs(-b-DetectZ)-c),colour="red")+
    xlim(-0.5,1.2) +
    ylim(-0.2,0.65) +
    geom_text(x=0.5,y=0.65,label="Fish2")+
    geom_text(x=0.5,y=0.61,label=paste("fZmean: ",format(p2.1[1], digit=3 )))+
    geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p2.1[2], digit=3 )))+
    geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p2.1[3],digit=3) ))+
    geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p2.1[4],digit=3) ))+
    geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p2.1[5],digit=3) )) 
  

  p3.1<-data.frame(testdata1 %>%
                     filter(Velocity==a & Depth ==b & Fish.no==f, !is.na(FocalZ)) %>%
                     select(FocalZ, DetectZ) %>%
                     mutate(fztrans= abs(-b-(FocalZ))-c,
                            dztrans= abs(-b-(DetectZ))-c)
  )%>%
    filter(fztrans<b*1.5 & dztrans<b*1.50)%>%
    summarise(fzmean=mean(fztrans),
              fzlow=min(fztrans),
              dzmin=min(dztrans),
              dzmean=mean(dztrans),
              dzmax=max(dztrans))
  
  p3<-testdata1 %>%
    filter(Velocity==a & Depth==b & Fish.no==f) %>%
    ggplot(aes(x=FocalX, y=abs(-b-FocalZ)-c ))+
    geom_point()+
    geom_point(aes(x=DetectX, y=abs(-b-DetectZ)-c),colour="red")+
    xlim(-0.5,1.2) +
    ylim(-0.2,0.65) +
    geom_text(x=0.5,y=0.65,label="Fish3")+
    geom_text(x=0.5,y=0.61,label=paste("fZmean: ", format(p3.1[1], digit=3 )))+
    geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p3.1[2], digit=3 )))+
    geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p3.1[3],digit=3) ))+
    geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p3.1[4],digit=3) ))+
    geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p3.1[5],digit=3) ))  
  
  
  
  p4.1<-data.frame(testdata1 %>%
                     filter(Velocity==a & Depth ==b & Fish.no==g, !is.na(FocalZ)) %>%
                     select(FocalZ, DetectZ) %>%
                     mutate(fztrans= abs(-b-(FocalZ))-c,
                            dztrans= abs(-b-(DetectZ))-c)
  )%>%
    filter(fztrans<b*1.10 & dztrans<b*1.10)%>%
    summarise(fzmean=mean(fztrans),
              fzlow=min(fztrans),
              dzmin=min(dztrans),
              dzmean=mean(dztrans),
              dzmax=max(dztrans))
  
  p4<-testdata1 %>%
    filter(Velocity==a & Depth==b & Fish.no==g) %>%
    ggplot(aes(x=FocalX, y=abs(-b-FocalZ)-c ))+
    geom_point()+
    geom_point(aes(x=DetectX, y=abs(-b-DetectZ)-c),colour="red")+
    xlim(-0.5,1.2) +
    ylim(-0.2,0.65) +
    geom_text(x=0.5,y=0.65,label="Fish4")+
    geom_text(x=0.5,y=0.61,label=paste("fZmean: ", format(p4.1[1], digit=3) ))+
    geom_text(x=0.5,y=0.57,label=paste("fZmin:  ", format(p4.1[2], digit=3)))+
    geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p4.1[3],digit=3) ))+
    geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p4.1[4],digit=3) ))+
    geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p4.1[5],digit=3) )) 
  
  ggarrange(p1,p2,p3,p4,
            labels=c("Fish1",
                     "Fish2",
                     "Fish3",
                     "Fish4",
                     ncol=2,nrow=2))
  }
  else{
  #  a=0.30
  #  b=0.15
  #  d=1
  #  c=0.12
  #  g=4
    cprime=0.11
    p1.1<-data.frame(testdata1 %>%
                       filter(Velocity==a & Depth ==b & Fish.no==d & !is.na(FocalZ)) %>%
                       select(FocalZ, DetectZ, CaptureZ) %>%
                       mutate(fztrans= 0.15+FocalZ+c, 
                              dztrans= 0.15+DetectZ+c-cprime
                              )
    )%>%
      filter(fztrans<b*1.20 & fztrans>-0.05 & dztrans < b*1.20 & dztrans > -0.05)%>%
      summarise(fzmean=mean(fztrans),
                fzlow=min(fztrans),
                dzmin=min(dztrans),
                dzmean=mean(dztrans),
                dzmax=max(dztrans))
    
    
    p1<-testdata1 %>%
      filter(Velocity==a & Depth== b & Fish.no==d) %>%
      ggplot(aes(x=FocalX, y=0.15+FocalZ+c ))+
      geom_point()+
      geom_point(aes(x=DetectX, y=0.15+DetectZ+c-cprime), colour="red")+
      xlim(-0.5,1.2) +
      ylim(-0.2,0.65) +
      geom_text(x=0.5,y=0.65,label="Fish1 Depth 0.15")+
      geom_text(x=0.5,y=0.61,label=paste("fZmean: ",format(p1.1[1],digit=3) ))+
      geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p1.1[2],digit=3) ))+
      geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p1.1[3],digit=3) ))+
      geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p1.1[4],digit=3) ))+
      geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p1.1[5],digit=3) )) 
    
    
    p2.1<-data.frame(testdata1 %>%
                       filter(Velocity==a & Depth ==b & Fish.no==e & !is.na(FocalZ)) %>%
                       select(FocalZ, DetectZ) %>%
                       mutate(fztrans= 0.15+FocalZ+c,
                              dztrans= 0.15+DetectZ+c-cprime)
    )%>%
      filter(fztrans<b*1.20 & fztrans>-0.05 & dztrans < b*1.20 & dztrans > -0.05)%>%
      summarise(fzmean=mean(fztrans),
                fzlow=min(fztrans),
                dzmin=min(dztrans),
                dzmean=mean(dztrans),
                dzmax=max(dztrans))
    p2<-testdata1 %>%
      filter(Velocity==a & Depth==b & Fish.no==e) %>%
      ggplot(aes(x=FocalX, y=0.15+FocalZ+c ))+
      geom_point()+
      geom_point(aes(x=DetectX, y=0.15+DetectZ+c-cprime),colour="red")+
      xlim(-0.5,1.2) +
      ylim(-0.2,0.65) +
      geom_text(x=0.5,y=0.65,label="Fish2 Depth 0.15")+
      geom_text(x=0.5,y=0.61,label=paste("fZmean: ",format(p2.1[1], digit=3 )))+
      geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p2.1[2], digit=3 )))+
      geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p2.1[3],digit=3) ))+
      geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p2.1[4],digit=3) ))+
      geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p2.1[5],digit=3) )) 
    
    
    p3.1<-data.frame(testdata1 %>%
                       filter(Velocity==a & Depth ==b & Fish.no==f, !is.na(FocalZ)) %>%
                       select(FocalZ, DetectZ) %>%
                       mutate(fztrans= 0.15+FocalZ+c,
                              dztrans= 0.15+DetectZ+c-cprime)
    )%>%
      filter(fztrans<b*1.20 & fztrans>-0.05 & dztrans < b*1.20 & dztrans > -0.05)%>%
      summarise(fzmean=mean(fztrans),
                fzlow=min(fztrans),
                dzmin=min(dztrans),
                dzmean=mean(dztrans),
                dzmax=max(dztrans))
    
    p3<-testdata1 %>%
      filter(Velocity==a & Depth==b & Fish.no==f) %>%
      ggplot(aes(x=FocalX, y=0.15+FocalZ+c))+
      geom_point()+
      geom_point(aes(x=DetectX, y=0.15+DetectZ+c-cprime),colour="red")+
      xlim(-0.5,1.2) +
      ylim(-0.2,0.65) +
      geom_text(x=0.5,y=0.65,label="Fish3 Depth 0.15")+
      geom_text(x=0.5,y=0.61,label=paste("fZmean: ", format(p3.1[1], digit=3 )))+
      geom_text(x=0.5,y=0.57,label=paste("fZmin: ",format(p3.1[2], digit=3 )))+
      geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p3.1[3],digit=3) ))+
      geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p3.1[4],digit=3) ))+
      geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p3.1[5],digit=3) ))  
    
    
    
    p4.1<-data.frame(testdata1 %>%
                       filter(Velocity==a & Depth ==b & Fish.no==g, !is.na(FocalZ)) %>%
                       select(FocalZ, DetectZ) %>%
                       mutate(fztrans= 0.15+FocalZ+c,
                              dztrans= 0.15+DetectZ+c-cprime)
    )%>%
      filter(fztrans<b*1.20 & fztrans>-0.05 & dztrans < b*1.20 & dztrans > -0.05)%>%
      summarise(fzmean=mean(fztrans),
                fzlow=min(fztrans),
                dzmin=min(dztrans),
                dzmean=mean(dztrans),
                dzmax=max(dztrans))
    
    p4<-testdata1 %>%
      filter(Velocity==a & Depth==b & Fish.no==g) %>%
      ggplot(aes(x=FocalX, y=0.15+FocalZ+c))+
      geom_point()+
      geom_point(aes(x=DetectX, y=0.15+DetectZ+c-cprime),colour="red")+
      xlim(-0.5,1.2) +
      ylim(-0.2,0.65) +
     # scale_y_continuous(breaks = seq(-0.2, 0.65, by = 5))+
      geom_text(x=0.5,y=0.65,label="Fish4 Depth 0.15")+
      geom_text(x=0.5,y=0.61,label=paste("fZmean: ", format(p4.1[1], digit=3) ))+
      geom_text(x=0.5,y=0.57,label=paste("fZmin:  ", format(p4.1[2], digit=3)))+
      geom_text(x=0.5,y=0.53,label=paste("dzmin: ",format(p4.1[3],digit=3) ))+
      geom_text(x=0.5,y=0.49,label=paste("dzmean: ",format(p4.1[4],digit=3) ))+
      geom_text(x=0.5,y=0.45,label=paste("dzmax: ",format(p4.1[5],digit=3) )) 
    
    ggarrange(p1,p2,p3,p4,
              labels=c("Fish1",
                       "Fish2",
                       "Fish3",
                       "Fish4",
                       ncol=2,nrow=2))
    
  }
    
}



# velocity, depth, calibration, fish.Nos (velocity experiment and depth experiment has different fish numbers)        
# velocity is 0.30 if I want to see the depth experiment data
# depth is 0.30 if I want to see the velocity experiment data
# Fish number is 1-4 for depth, and 1,3,5,8 for velocity experiment
# Depth: 0.15, 0.30,0.45,0.60

calibratefishfocaldetectionpoint(0.30,0.15,0.13,1,2,3,4)
calibratefishfocaldetectionpoint(0.30,0.30,0.07,1,2,3,4)
calibratefishfocaldetectionpoint(0.30,0.45,0.14,1,2,3,4)
calibratefishfocaldetectionpoint(0.30,0.60,0.38,1,2,3,4)

# Results: Correct calibration
# 0.15 depth: 0
# 0.30 depth: 0.07
# 0.45 depth: 0.23 for fish 1,2,and 4 vs ~ 0.14 for fish 3
# 0.60 depth: 0.38







#### 3. Check Z-scaling is correct ####
# Check if the calibration has been correctly applied
calibtest<-testdata1_scaledZ %>%
  filter(Velocity !=0.3) %>%
  select(Fish.no,FocalZ)%>%
  group_by(Fish.no) %>%
  summarise(fZmean=mean(FocalZ, na.rm=TRUE)) # Mean of FocalZ from velocity experiment : 0.0515

testdata1_scaledZ %>%
  ggplot(aes(x=FocalX, y=FocalZ, color=as.factor(Depth)) )+
  geom_point()+
  geom_point(aes(x=DetectX, y=DetectZ),color="red") +
  xlim(-0.15,1.2) +
  ylim(-0.05,0.70) 


# Plot histogram to validate scaling of z axis       
testdata1_scaledZ %>%
  filter(Velocity != 0.30) %>% 
  ggplot(aes(CaptureZ)) +
  geom_histogram()

hist(testdata1_scaledZ$FocalZ, xlim=c(0,0.3))
hist(testdata1_scaledZ$CaptureZ)
hist(testdata1_scaledZ$ReturnZ)
boxplot(testdata1_scaledZ$FocalZ)



#### 4. Fish size ####
Fishsizes<-read_excel("Piccolo_data_compilev4.5.xlsx","Fishsizes")
str(testdata1_scaled);str(Fishsizes)
table(testdata1_scaled$Velocity)

Fishsizes1<-Fishsizes %>%
  mutate(Length= case_when(Experiment=="depth" ~ Length,
                           Experiment =="velocity"~ Length*0.001)) %>%
  group_by(Experiment,Fish.no) %>%
  summarise(Length=max(Length) )

ggplot(Fishsizes1, aes(x=Fish.no, y=Length, color=as.factor(Fish.no)) )+
  facet_wrap(~Experiment)+
  geom_point()+
  ggtitle("'final'")


#### 5. Pipedelay time analysis ####
# Why do some pipes have more delays? Is it because of human effect or just pipe effect?

# Pipedelayanalysis: New data frame just for the statistical analysis 
Pipedelayanalysis<-data.frame(testdata1_scaled_velocity.block) %>%
  filter(Pipedelay > 0) 

# New variable: FeederY.std.pi
FeederY.std.pi<-matrix(0,nrow(Pipedelayanalysis),1)
for (k in 1:nrow(Pipedelayanalysis)){
  if(Pipedelayanalysis$FeederY[k] <0.5){
    FeederY.std.pi[k]<-0.5-Pipedelayanalysis$FeederY[k]
  } else{ FeederY.std.pi[k]<-Pipedelayanalysis$FeederY[k]-0.5
  }
}
Pipedelayanalysis$FeederY.std.pi<-FeederY.std.pi

# Statistical Analyses
str(Pipedelayanalysis)
#without Feeder.no
pi1<-glmer(Pipedelay~factor(Feeder.no)+FeederY+FeederY.std.pi+scale(Order)+ Velocity+ Depth+ (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
vif(pi1) # FeederZ generates correlation
summary(pi2.3)
pi1.1<-glmer(Pipedelay~factor(Feeder.no)+scale(Order)+ Velocity+ Depth+ (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.2<-glmer(Pipedelay~factor(Feeder.no)+            + Velocity+ Depth+ (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.3<-glmer(Pipedelay~factor(Feeder.no)+scale(Order)+         + Depth+ (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.4<-glmer(Pipedelay~factor(Feeder.no)+scale(Order)+ Velocity+        (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.5<-glmer(Pipedelay~factor(Feeder.no)+scale(Order)+         +        (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.6<-glmer(Pipedelay~factor(Feeder.no)+                               (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi1.7<-glmer(Pipedelay~                  scale(Order)+                     (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )


# without Feeder.no
pi2<-glmer(Pipedelay~ FeederY+FeederY.std.pi+scale(Order)+ Velocity+ Depth+ (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi2.1<-glmer(Pipedelay~ FeederY+FeederY.std.pi+scale(Order)+ Velocity+     + (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi2.2<-glmer(Pipedelay~ FeederY+FeederY.std.pi+scale(Order)+               + (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi2.3<-glmer(Pipedelay~ FeederY+              +scale(Order)+               + (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )
pi2.4<-glmer(Pipedelay~ FeederY+                                           + (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )


pi_Null<-glmer(Pipedelay~              + (1|block) +(1|Experiment) , family=Gamma, nAGQ=0, data=Pipedelayanalysis )

#AICc check
modelsPipe<-list(pi1,pi1.1,pi1.2,pi1.3,pi1.4,pi1.5,pi1.6,pi1.7,pi2,pi2.1,pi2.2,pi2.3,pi2.4,pi_Null)
model.namesPipe<-c('pi1','pi1.1','pi1.2','pi1.3','pi1.4','pi1.5','pi1.6','pi1.7','pi2','pi2.1','pi2.2','pi2.3','pi2.4','pi_Null')
( summaryAICpipe<-aictab(modelsPipe,model.namesPipe,second.ord=FALSE) )

summary(pi1.6)
rsq(pi1.4)
# Plot predicted vs actual (see how accurate my model is)
ggplot(Pipedelayanalysis, aes(x=Pipedelay, y=predict(pi1.6, type='response')))+
  geom_point()+
  xlim(0,10)+
  ylim(0,10)+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5) # difficult to have an accurate model....

# Conclusion: Feeder.no's have the most significant affect on the pipeline delay..
ggplot(Pipedelayanalysis, aes(x=as.factor(Feeder.no), y=Pipedelay ))+
  geom_boxplot()

data.frame(testdata1_scaled_velocity.block %>%
             filter(Pipedelay > 0) %>%
             group_by(Feeder.no) %>%
             summarise(pipedelay.mean=mean(Pipedelay), pipedelay.sd=sd(Pipedelay), count=n() ) )
str(Pipedelayanalysis)

# velocity vs delay timeing;; not so much..
