###################################################################################
######################## July 2021, Piccolo files extended ########################
###############################  Standardize data ##########################
### Need to standardize both Velocity and Depth experiments to one axis ####

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
library(purrr)

#####################################################    
#### 1. Standardize all axes detection distances #####  
medfpX<-0.5
medfpY<-0.5
medfpZ<-0.03

str(testdata1.2)
select<-dplyr::select
StandardVelocityDepth<-testdata1.2 %>% # previous name was Detect.range
  # remove points that I artificially inserted in the excel sheet...
  filter(!((Experiment=="Depth" & Depth==0.3 & Fish.no=="11" & Prey.no==99)|
             (Depth==0.15 & Fish.no=="11" & Prey.no==5)| 
             (Depth==0.15 & Fish.no=="11" & Prey.no==6)|
             (Depth==0.15 & Fish.no=="11" & Prey.no==68))) %>%
  select(Velocity,Depth,Fish.length,FeederY,FeederZ,Fish.no,DetectX:DetectZ,FocalX:FocalZ,block,Experiment)%>%
  mutate(FeederYpush=case_when(Experiment=="Velocity"~ FeederY, # velocity range 0 to 1.0
                               Experiment=="Depth" ~ FeederY+0.2),# depth range 0 to 0.6
         FeederY.std=case_when(FeederYpush<0.5 ~ 0.5-FeederYpush,
                               FeederYpush>=0.5 ~ FeederYpush-0.5),
         nDetectX=medfpX+DetectX-FocalX,
         FocalY_push=case_when(Experiment=="Velocity"~ FocalY, # velocity range 0 to 1.0
                               Experiment=="Depth" ~FocalY+0.2),
         DetectY_push=case_when(Experiment=="Velocity"~ DetectY, # velocity range 0 to 1.0
                                Experiment=="Depth" ~DetectY+0.2),
         nDetectY=medfpY+DetectY_push-FocalY_push,
         nDetectY_oneside=case_when(nDetectY<0.5 ~ 0.5-nDetectY,
                                    nDetectY>=0.5 ~ nDetectY-0.5),
         nDetectZ=medfpZ+DetectZ-FocalZ)%>%
  filter(nDetectY_oneside>0, nDetectY_oneside<0.5, nDetectZ>0, nDetectZ<0.65) %>%
  select(-FeederYpush,-FocalY_push,-DetectY_push)


#####
findmaxdist<-function(x,p){ # x is dataset, p is the percentile e.g., 95,98,99 etc..
  u<-unique(x)
  sort(u,decreasing=FALSE)[length(u)*p/100]
}


#####
Detect.rangeXZ<-StandardVelocityDepth%>%
  filter(nDetectX<=findmaxdist(nDetectX,99),
         nDetectY_oneside<=findmaxdist(nDetectY_oneside,99),
         nDetectZ<=findmaxdist(nDetectZ,100))

Detect.rangeXZ.c<-Detect.rangeXZ %>%
  mutate(HighPointClass=case_when(Fish.length==0.053 & Velocity==0.30~1,
                                  Fish.length==0.058 & Velocity==0.30~2,
                                  Fish.length==0.062 & Velocity==0.30~3,
                                  Fish.length==0.08& Velocity==0.29~4,
                                  Fish.length==0.08& Velocity==0.38~5,
                                  Fish.length==0.08& Velocity==0.45~6,
                                  Fish.length==0.08& Velocity==0.49~7,
                                  Fish.length==0.08& Velocity==0.55~8,
                                  Fish.length==0.08& Velocity==0.58~9,
                                  
                                  Fish.length==0.081& Velocity==0.29~10,
                                  Fish.length==0.081& Velocity==0.38~11,
                                  Fish.length==0.081& Velocity==0.45~12,
                                  Fish.length==0.081& Velocity==0.49~13,
                                  Fish.length==0.081& Velocity==0.55~14,
                                  Fish.length==0.081& Velocity==0.58~15,
                                  
                                  Fish.length==0.082& Velocity==0.29~16,
                                  Fish.length==0.082& Velocity==0.38~17,
                                  Fish.length==0.082& Velocity==0.45~18,
                                  Fish.length==0.082& Velocity==0.49~19,
                                  Fish.length==0.082& Velocity==0.55~20,
                                  Fish.length==0.082& Velocity==0.58~21,
                                  
                                  Fish.length==0.083& Velocity==0.29~22,
                                  Fish.length==0.083& Velocity==0.38~23,
                                  Fish.length==0.083& Velocity==0.45~24,
                                  Fish.length==0.083& Velocity==0.49~25,
                                  Fish.length==0.083& Velocity==0.55~26,
                                  Fish.length==0.083& Velocity==0.58~27
    )
  )

#### End of script.