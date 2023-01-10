###################################################################################
######################## July 2021, Piccolo files extended ########################
#############################  Clean data ##########################################
####################### Jul.20,2021: Separate sheets ###############################
# Mar.31, 2022: Change z axis transform function to correct max min values of focal and detection points.

summarise<-dplyr::summarise
select<-dplyr::select

library(ggpubr)
library(tidyverse)
library(fs)
# install.packages("readxl")
library(readxl)
library(MuMIn)
library(lme4)
#install.packages("AICcmodavg")
library(AICcmodavg)
library(car)
# install.packages("dplyr")
#library(rgl)
# install.packages("scatterplot3d")
library(rsq)
library(dplyr)

#############################################################
##### 1. Import data #####

setwd("~/UC Santa Cruz project/UCSC_Research/Small_experiment/Piccolo_research/Piccolo_mathematica_files")
path<-"Piccolo_data_compilev4.5.xlsx"
excel_sheets(path)

    #### 1.1 Import excel files ####
    SH1V2<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V2")
    SH1V3<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V3")
    SH1V4<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V4")
    SH1V5<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V5")
    SH1V55<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V55")
    SH1V6<-read_excel("Piccolo_data_compilev4.5.xlsx","SH1V6")
    
    SH3V2<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V2")
    SH3V3<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V3")
    SH3V4<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V4")
    SH3V5<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V5")
    SH3V55<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V55")
    SH3V6<-read_excel("Piccolo_data_compilev4.5.xlsx","SH3V6")
    
    SH4V2<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V2")
    SH4V3<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V3")
    SH4V4<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V4")
    SH4V5<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V5")
    SH4V55<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V55")
    SH4V6<-read_excel("Piccolo_data_compilev4.5.xlsx","SH4V6")
    
    SH5V2<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V2")
    SH5V3<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V3")
    SH5V4<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V4")
    SH5V5<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V5")
    SH5V55<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V55")
    SH5V6<-read_excel("Piccolo_data_compilev4.5.xlsx","SH5V6")
    
    SH8V2<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V2")
    SH8V3<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V3")
    SH8V4<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V4")
    SH8V5<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V5")
    SH8V55<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V55")
    SH8V6<-read_excel("Piccolo_data_compilev4.5.xlsx","SH8V6")
    
    ### Depth data
    SH1D15<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH1D15")
    SH1D30<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH1D30")
    SH1D45<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH1D45")
    SH1D60<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH1D60")
    
    SH2D15<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH2D15")
    SH2D30<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH2D30")
    SH2D45<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH2D45")
    SH2D60<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH2D60")
    
    SH3D15<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH3D15")
    SH3D30<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH3D30")
    SH3D45<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH3D45")
    SH3D60<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH3D60")
    
    SH4D15<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH4D15")
    SH4D30<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH4D30")
    SH4D45<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH4D45")
    SH4D60<-read_excel("Piccolo_data_compilev4.5.xlsx","DSH4D60")
    
    
    # Check names
    names(SH1V2)
    
    #### 1.2 Select columns ####
    select<-dplyr::select
    
    SH1V2m<-SH1V2 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1V3m<-SH1V3 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1V4m<-SH1V4 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1V5m<-SH1V5 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1V55m<-SH1V55 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1V6m<-SH1V6 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    SH3V2m<-SH3V2 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3V3m<-SH3V3 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3V4m<-SH3V4 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3V5m<-SH3V5 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3V55m<-SH3V55 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3V6m<-SH3V6 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    SH4V2m<-SH4V2 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4V3m<-SH4V3 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4V4m<-SH4V4 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4V5m<-SH4V5 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4V55m<-SH4V55 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4V6m<-SH4V6 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    SH5V2m<-SH5V2 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH5V3m<-SH5V3 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH5V4m<-SH5V4 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH5V5m<-SH5V5 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH5V55m<-SH5V55 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH5V6m<-SH5V6 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    
    SH8V2m<-SH8V2 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH8V3m<-SH8V3 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH8V4m<-SH8V4 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH8V5m<-SH8V5 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH8V55m<-SH8V55 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH8V6m<-SH8V6 %>%
      select(Fish.no,Velocity,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    # Depth data
    SH1D15m<-SH1D15 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1D30m<-SH1D30 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1D45m<-SH1D45 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH1D60m<-SH1D60 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
   
    SH2D15m<-SH2D15 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH2D30m<-SH2D30 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH2D45m<-SH2D45 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH2D60m<-SH2D60 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    SH3D15m<-SH3D15 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3D30m<-SH3D30 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3D45m<-SH3D45 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH3D60m<-SH3D60 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    SH4D15m<-SH4D15 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4D30m<-SH4D30 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4D45m<-SH4D45 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    SH4D60m<-SH4D60 %>%
      select(Fish.no,Depth,Order,Prey.no,Preyintroductionseconds,Feeder.no,FeederY,FeederZ,Pipedelay,Capture,Maneuvernotcapture,FocalX:DetectZ)
    
    
############################
##### 2. Combine data #####

  # Velocity data
Piccolo_compile_velocity<-rbind(SH1V2m,SH1V3m,SH1V4m,SH1V5m,SH1V55m,SH1V6m,
                       SH3V2m,SH3V3m,SH3V4m,SH3V5m,SH3V55m,SH3V6m,
                       SH4V2m,SH4V3m,SH4V4m,SH4V5m,SH4V55m,SH4V6m,
                       SH5V2m,SH5V3m,SH5V4m,SH5V5m,SH5V55m,SH5V6m,
                       SH8V2m,SH8V3m,SH8V4m,SH8V5m,SH8V55m,SH8V6m)
Piccolo_compile_velocity2<-Piccolo_compile_velocity%>%
  mutate(Depth=0.30,.before=Order)

summary(Piccolo_compile_velocity2$Pipedelay)

 # depth data
Piccolo_compile_depth<-rbind(SH1D15m,SH1D30m,SH1D45m,SH1D60m,
                             SH2D15m,SH2D30m,SH2D45m,SH2D60m,
                             SH3D15m,SH3D30m,SH3D45m,SH3D60m,
                             SH4D15m,SH4D30m,SH4D45m,SH4D60m)

Piccolo_compile_depth2<-Piccolo_compile_depth%>%
  mutate(Velocity=0.30,.before=Depth)

Piccolo_compile<-rbind(Piccolo_compile_velocity2, Piccolo_compile_depth2)
    
    
    data.frame(Piccolo_compile)
    str(Piccolo_compile)
    Piccolo_compile$DetectZ<-as.numeric(Piccolo_compile$DetectZ) # change character to numeric
    Piccolo_compile$Return2Z<-as.numeric(Piccolo_compile$Return2Z) # change character to numeric
    
    Piccolo_compile
    names(Piccolo_compile)

#################################################################################
##### 3. Do not remove outliars in this dataset as this should be a raw version.
    
    
#################################################################################
##### 4. Add new columns: Return 1,2 and pre_focal/capture, scaling the columns etc..#####

  # Columns that should be calculated separately from velocity vs. depth experiment
    # Fish size
    # Scaling Z coordinate

    
  ## 4.1  Change Return1 to Return2 when Return2 exists 
      testdata1<-Piccolo_compile; str(testdata1)
      testdata1$ReturnX<-NA # make empty columns
      testdata1$ReturnY<-NA
      testdata1$ReturnZ<-NA
      str(testdata1)
      
      table(testdata1$Return1Z)
      summary(testdata1$Return2X)
      
      for (i in 1: nrow(testdata1)){ 
        if( testdata1$Return2X[i] == 0 | is.na(testdata1$Return2X[i]) ) {testdata1$ReturnX[i]<-testdata1$Return1X[i]; 
        testdata1$ReturnY[i]<-testdata1$Return1Y[i]; 
        testdata1$ReturnZ[i]<-testdata1$Return1Z[i] } else{testdata1$ReturnX[i]<-testdata1$Return2X[i];
        testdata1$ReturnY[i]<-testdata1$Return2Y[i]; testdata1$ReturnZ[i]<-testdata1$Return2Z[i]}
      }
      
  ## 4.2 Scale the Z coordinates (focal,capture, return)
     # Current Z axis visualizaiton is in 1.2_CleandataSupport.R
      
     # Don't scale X and Y as I won't be able to apply Gamma dsitribution later
            
       #   # function to scale X
            #    FocalX.med<-median(testdata1$FocalX, na.rm=TRUE)
            #    scale2fishX <- function(x, na.rm = FALSE){
            #      x-FocalX.med }
            #    testdata1_scaledXYZ<-testdata1 %>% 
            #      mutate_at(c("FocalX","CaptureX","ReturnX"), scale2fishX ) %>%
            #      mutate_at(c("FocalY","CaptureY","ReturnY"), scale2fishY ) %>%
            ###      mutate_at(c("FocalZ","CaptureZ","ReturnZ"), scale2fishZ )
                
              
                # function to scale Y
            #    FocalY.med<-median(testdata1$FocalY, na.rm=TRUE)
                
            #    scale2fishY <- function(x, na.rm = FALSE){
            #      if(x < FocalY.med){
            #        -(FocalY.med - x)
            #      }else{x-FocalY.med}
            #    }
                
            #    testdata1_scaledY<-testdata1 %>% 
            #      mutate_at(c("FocalY","CaptureY","ReturnY"), scale2fish )
                
     # scale2fishZ <- function(x, na.rm = FALSE){
    #                            abs(-0.3-x)
    #                          }
     
      # testdata1_scaledZ

        testdata1_scaledZ<-testdata1 %>%
        mutate(FocalZ= case_when(Velocity != 0.30 ~ abs(-0.3-FocalZ), # velocity experiment
                                Velocity == 0.30 & Depth ==0.15 ~ 0.15+FocalZ+0.13,  # depth experiment
                                Velocity == 0.30 & Depth ==0.30 ~ abs(-0.30-FocalZ)-0.07,
                                Velocity == 0.30 & Depth ==0.45 & (Fish.no== 1 | Fish.no == 2 |Fish.no == 4) ~ abs(-0.45-FocalZ)-0.23,
                                Velocity == 0.30 & Depth ==0.45 & Fish.no== 3 ~ abs(-0.45-FocalZ)-0.14,
                                Velocity == 0.30 & Depth ==0.60 ~ abs(-0.60-FocalZ)-0.38 ),
              CaptureZ= case_when(Velocity != 0.30 ~ abs(-0.3-CaptureZ),
                                  Velocity == 0.30 & Depth ==0.15 ~ 0.15+CaptureZ+0.13-0.11,
                                  Velocity == 0.30 & Depth ==0.30 ~ abs(-0.30-CaptureZ)-0.07,
                                  Velocity == 0.30 & Depth ==0.45 & (Fish.no== 1 |Fish.no== 2 |Fish.no== 4) ~ abs(-0.45-CaptureZ)-0.23,
                                  Velocity == 0.30 & Depth ==0.45 & Fish.no== 3 ~ abs(-0.45-CaptureZ)-0.14,
                                  Velocity == 0.30 & Depth ==0.60 ~ abs(-0.60-CaptureZ)-0.38),
              DetectZ= case_when(Velocity != 0.30 ~ abs(-0.3-DetectZ),
                                  Velocity == 0.30 & Depth ==0.15 ~ 0.15+DetectZ+0.13-0.11,
                                  Velocity == 0.30 & Depth ==0.30 ~ abs(-0.30-DetectZ)-0.07,
                                  Velocity == 0.30 & Depth ==0.45 & (Fish.no== 1 |Fish.no== 2 |Fish.no== 4) ~ abs(-0.45-DetectZ)-0.23,
                                  Velocity == 0.30 & Depth ==0.45 & Fish.no== 3 ~ abs(-0.45-DetectZ)-0.14,
                                  Velocity == 0.30 & Depth ==0.60 ~ abs(-0.60-DetectZ)-0.38),
              ReturnZ= case_when(Velocity != 0.30 ~ abs(-0.3-ReturnZ),
                                 Velocity == 0.30 & Depth ==0.15 ~ 0.15+ReturnZ+0.13,
                                 Velocity == 0.30 & Depth ==0.30 ~ abs(-0.30-ReturnZ)-0.07,
                                 Velocity == 0.30 & Depth ==0.45 & (Fish.no== 1 |Fish.no== 2 |Fish.no== 4) ~ abs(-0.45-ReturnZ)-0.23,
                                 Velocity == 0.30 & Depth ==0.45 & Fish.no== 3 ~ abs(-0.45-ReturnZ)-0.14,
                                 Velocity == 0.30 & Depth ==0.60 ~ abs(-0.60-ReturnZ)-0.38),
              FeederZ= case_when(Velocity != 0.30 ~ abs(-0.3-FeederZ),
                                  Velocity == 0.30 ~ abs(-0.6-FeederZ))
                                )
   
 ###############################   
       
                
  ## 4.3 Scale Y coordinate for Depth experiment data (deprecated)
        testdata1_scaledY <-testdata1_scaledZ
  ## 4.4 Experiment type + Feeder number on Depth experiment (+20)

        testdata1_scaledY2<-testdata1_scaledY %>%
          mutate(Experiment= case_when(Velocity !=0.30 ~ "Velocity", Velocity == 0.30 ~ "Depth") )
        testdata1_scaledY2$Experiment<-factor(testdata1_scaledY2$Experiment)
        
        testdata1_scaled<-testdata1_scaledY2 %>%
          mutate(Feeder.no= case_when(Velocity !=0.30 ~ Feeder.no, Velocity == 0.30 ~ Feeder.no +20 ) )
        str(testdata1_scaled)
        table(testdata1_scaled$Feeder.no)     
        
        
  ## 4.5 'next' focal coordinate
      testdata1_scaled$next_FocalX<-NA
      testdata1_scaled$next_FocalY<-NA
      testdata1_scaled$next_FocalZ<-NA
      
      for (p in 1:(nrow(testdata1_scaled)-1)){
        testdata1_scaled$next_FocalX[p] <- testdata1_scaled$FocalX[p+1]
        testdata1_scaled$next_FocalY[p] <- testdata1_scaled$FocalY[p+1]
        testdata1_scaled$next_FocalZ[p] <- testdata1_scaled$FocalZ[p+1]
        
      }
  
  ## 4.6 'previous' focal/capture/return coordinates 
      testdata1_scaled$pre_FocalX<-NA; testdata1_scaled$pre_FocalY<-NA; testdata1_scaled$pre_FocalZ<-NA
      testdata1_scaled$pre_CaptureX<-NA; testdata1_scaled$pre_CaptureY<-NA; testdata1_scaled$pre_CaptureZ<-NA
      testdata1_scaled$pre_ReturnX<-NA; testdata1_scaled$pre_ReturnY<-NA; testdata1_scaled$pre_ReturnZ<-NA
      
      for (q in 2:(nrow(testdata1_scaled)-1)){
        testdata1_scaled$pre_FocalX[q] <- testdata1_scaled$FocalX[q-1]
        testdata1_scaled$pre_FocalY[q] <- testdata1_scaled$FocalY[q-1]
        testdata1_scaled$pre_FocalZ[q] <- testdata1_scaled$FocalZ[q-1]
        
        testdata1_scaled$pre_CaptureX[q] <- testdata1_scaled$CaptureX[q-1]
        testdata1_scaled$pre_CaptureY[q] <- testdata1_scaled$CaptureY[q-1]
        testdata1_scaled$pre_CaptureZ[q] <- testdata1_scaled$CaptureZ[q-1]
        
        testdata1_scaled$pre_ReturnX[q] <- testdata1_scaled$ReturnX[q-1]
        testdata1_scaled$pre_ReturnY[q] <- testdata1_scaled$ReturnY[q-1]
        testdata1_scaled$pre_ReturnZ[q] <- testdata1_scaled$ReturnZ[q-1]
        
        
      }
      str(testdata1_scaled)
  table(testdata1_scaled$ReturnX)
  ## 4.7 Divide upper and lower feeders (this only applied when doing Velocity experiment)
    #  testdata1_scaled$Feeder.low.high<-NA
    #  for (j in 1: nrow(testdata1_scaled)){
    #    if(testdata1_scaled$Feeder.no[j]<=10){
    ##      testdata1_scaled$Feeder.low.high[j]<-"Low"
    #    }else{testdata1_scaled$Feeder.low.high[j]<-"High" }
    #  }
  ## 4.8 Divide feeder left/right (this only applied when doing Velocity experiment)
    ##  testdata1_scaled$Feeder.left.right<-NA
    #  for (j in 1: nrow(testdata1_scaled)){
    #   if(testdata1_scaled$Feeder.no[j] <= 5 | (testdata1_scaled$Feeder.no[j] >= 11 & testdata1_scaled$Feeder.no[j] <= 15) ){
    #      testdata1_scaled$Feeder.left.right[j]<-"Left"
    #    }else{testdata1_scaled$Feeder.left.right[j]<-"Right" }
    #  }
      
      
  ## 4.9 Divide feeder left/right and low/high   (this only applied when doing Velocity experiment)
    #  testdata1_scaled$Feeder.left.right.low.high<-NA
    ##  for (j in 1: nrow(testdata1_scaled)) {
    #    if(testdata1_scaled$Feeder.no[j] <= 5) {
    #      testdata1_scaled$Feeder.left.right.low.high[j]<-"Left.Low"
    #    } else if(testdata1_scaled$Feeder.no[j] > 5 & testdata1_scaled$Feeder.no[j] < 11){
    #      testdata1_scaled$Feeder.left.right.low.high[j]<-"Right.Low"
    ##    } else if(testdata1_scaled$Feeder.no[j] >= 11 & testdata1_scaled$Feeder.no[j] < 16){
    #      testdata1_scaled$Feeder.left.right.low.high[j]<-"Left.High"
    #    }else {testdata1_scaled$Feeder.left.right.low.high[j]<-"Right.High" }
    #  }
    #  table(testdata1_scaled$Feeder.left.right.low.high)
    #  table(testdata1_scaled$Feeder.no)
    #  str(testdata1_scaled)
      
  ## 4.10 Put previous Feeder locations as a variable (this only applied when doing Velocity experiment)
    #  testdata1_scaled$pre_Feeder.left.right<-NA
    #  testdata1_scaled$pre_Feeder.low.high<-NA
    #  testdata1_scaled$pre_Feeder.left.right.low.high<-NA
      
    #  for (r in 2:(nrow(testdata1_scaled)-1)){
    ##    testdata1_scaled$pre_Feeder.left.right[r] <- testdata1_scaled$Feeder.left.right[r-1]
    #    testdata1_scaled$pre_Feeder.low.high[r] <- testdata1_scaled$Feeder.low.high[r-1]
    #    testdata1_scaled$pre_Feeder.left.right.low.high[r] <- testdata1_scaled$Feeder.left.right.low.high[r-1]
        
    #  }
      
  ## 4.7 Fish.no (change the Depth data fish to 11-14)
  testdata1_scaled<-testdata1_scaled %>%
    mutate(Fish.no=case_when( # velocity experiment 
      Velocity != 0.30 & Fish.no == 1~ 1,
      Velocity != 0.30 & Fish.no ==3~ 3,
      Velocity != 0.30 & Fish.no ==4~ 4,
      Velocity != 0.30 & Fish.no ==5~ 5,
      Velocity != 0.30 & Fish.no ==8~ 8,
        # depth experiment
      Velocity == 0.30 & Fish.no ==1~ 11 ,
      Velocity == 0.30 & Fish.no ==2~ 12,
      Velocity == 0.30 & Fish.no ==3~ 13,
      Velocity == 0.30 & Fish.no ==4~ 14 
    ) )
  table(testdata1_scaled$Fish.no)
  
  
  
  ## 4.10.1 Add distance between previous feeder to current feeder
      # This helps to test whether the fish focus on where the previous prey was coming from rather than looking in the center..
      testdata1_scaled$dist.pre.feeder<-NA
      str(testdata1_scaled)
      
      testdata1_scaled$FeederY
      testdata1_scaled$FeederZ
      preynumberone<-which(testdata1_scaled$Prey.no == 1)
      
      hist(testdata1_scaled$FeederY)
    # fill in with the distance between the previous feeder to the current feeder 
      for (s in 2:(nrow(testdata1_scaled))){
        testdata1_scaled$dist.pre.feeder[s] <- sqrt( (testdata1_scaled$FeederY[s] - testdata1_scaled$FeederY[s-1])^2 +    
                                                     (testdata1_scaled$FeederZ[s] - testdata1_scaled$FeederZ[s-1])^2 ) 
      }
      hist(testdata1_scaled$FeederY)
    # Fill numbers that have the number 1 prey which is the distance of Y from the center line..
      for (t in preynumberone){
        testdata1_scaled$dist.pre.feeder[t]<- NA #    abs( 0.5-testdata1_scaled$FeederY[t] )
        }
    
      
      #for (s in 2:(nrow(testdata1_scaled)-1)){
      #  if(testdata1_scaled$FeederY[s] * testdata1_scaled$FeederY[s-1] < 0 ) {
      #  testdata1_scaled$distY.pre.feeder[s] <- abs( testdata1_scaled$FeederY[s] - testdata1_scaled$FeederY[s-1] ) 
      #    }
      #  if(testdata1_scaled$FeederY[s] * testdata1_scaled$FeederY[s-1] > 0 ){
      #    testdata1_scaled$distY.pre.feeder[s] <- abs( abs(testdata1_scaled$FeederY[s]) - abs(testdata1_scaled$FeederY[s-1]) ) 
      #    }
      #  }
      
      
      str(testdata1_scaled)
      
      hist(testdata1_scaled$dist.pre.feeder)
      table(testdata1_scaled$dist.pre.feeder)
      
   ## 4.11 Change some variables to factors
      
      str(testdata1_scaled)
      testdata1_scaled$Fish.no<-factor(testdata1_scaled$Fish.no)
      #testdata1_scaled$Feeder.low.high<-factor(testdata1_scaled$Feeder.low.high)
      #testdata1_scaled$Feeder.left.right<-factor(testdata1_scaled$Feeder.left.right)
      #testdata1_scaled$Feeder.left.right.low.high<-factor(testdata1_scaled$Feeder.left.right.low.high)
      #testdata1_scaled$pre_Feeder.left.right<-factor(testdata1_scaled$pre_Feeder.left.right)
      #testdata1_scaled$pre_Feeder.low.high<-factor(testdata1_scaled$pre_Feeder.low.high)
      #testdata1_scaled$pre_Feeder.left.right.low.high<-factor(testdata1_scaled$pre_Feeder.left.right.low.high)
      
    ## 4.12 Fish size data
      Fishsizes<-read_excel("Piccolo_data_compilev4.5.xlsx","Fishsizes")
      str(testdata1_scaled);str(Fishsizes)
      table(testdata1_scaled$Velocity)

      Fishsizes1<-Fishsizes %>%
        filter(Length.type=="final.true")%>%
        group_by(Experiment, Fish.no) %>%
        summarise(Length=max(Length))
      
            ggplot(Fishsizes1, aes(x=as.factor(Fish.no), y=Length, color=as.factor(Fish.no)) )+
            facet_wrap(~Experiment)+
            geom_point()+
            ggtitle("'final'")
            
            Fishsizes1$Length
       
            testdata1_scaled_velocity<-testdata1_scaled %>%
              mutate(Fish.length=case_when( # velocity experiment 
                                          Velocity != 0.30 & Fish.no == 1~ Fishsizes1$Length[5],
                                          Velocity != 0.30 & Fish.no == 3~ Fishsizes1$Length[6],
                                          Velocity != 0.30 & Fish.no == 4~ Fishsizes1$Length[7],
                                          Velocity != 0.30 & Fish.no == 5~ Fishsizes1$Length[8],
                                          Velocity != 0.30 & Fish.no == 8~ Fishsizes1$Length[9],
                                          # depth experiment
                                          Velocity == 0.30 & Fish.no == 11~ Fishsizes1$Length[1],
                                          Velocity == 0.30 & Fish.no == 12~ Fishsizes1$Length[2],
                                          Velocity == 0.30 & Fish.no == 13~ Fishsizes1$Length[3],
                                          Velocity == 0.30 & Fish.no == 14~ Fishsizes1$Length[4]
                                               ) )
            str(testdata1_scaled_velocity)
            table(testdata1_scaled_velocity$Fish.no)
            
    ## 4.13 Add prey introduction time
      # 4.13.1 Statistical test: what affects the pipe delay time? 
            # 1) Make a column to identify individual experiment
            testdata1_scaled_velocity %>%
              # filter(Pipedelay !=0) %>%
              group_by(Feeder.no) %>%
              summarise(howmany=n())
            
            testdata1_scaled_velocity.block<-testdata1_scaled_velocity %>%
              mutate(block=case_when(Velocity==0.29 & Fish.no=="1"~ 1,Velocity==0.29 & Fish.no=="3"~ 2,Velocity==0.29 & Fish.no=="4"~ 3, Velocity==0.29 & Fish.no=="5"~ 4,Velocity==0.29 & Fish.no=="8"~ 5,
                                     Velocity==0.38 & Fish.no=="1"~ 6,Velocity==0.38 & Fish.no=="3"~ 7,Velocity==0.38 & Fish.no=="4"~ 8, Velocity==0.38 & Fish.no=="5"~ 9,Velocity==0.38 & Fish.no=="8"~ 10,
                                     Velocity==0.45 & Fish.no=="1"~ 11,Velocity==0.45 & Fish.no=="3"~ 12,Velocity==0.45 & Fish.no=="4"~ 13, Velocity==0.45 & Fish.no=="5"~ 14,Velocity==0.45 & Fish.no=="8"~ 15,
                                     Velocity==0.49 & Fish.no=="1"~ 16,Velocity==0.49 & Fish.no=="3"~ 17,Velocity==0.49 & Fish.no=="4"~ 18, Velocity==0.49 & Fish.no=="5"~ 19,Velocity==0.49 & Fish.no=="8"~ 20,
                                     Velocity==0.55 & Fish.no=="1"~ 21,Velocity==0.55 & Fish.no=="3"~ 22,Velocity==0.55 & Fish.no=="4"~ 23, Velocity==0.55 & Fish.no=="5"~ 24,Velocity==0.55 & Fish.no=="8"~ 25,
                                     Velocity==0.58 & Fish.no=="1"~ 26,Velocity==0.58 & Fish.no=="3"~ 27,Velocity==0.58 & Fish.no=="4"~ 28, Velocity==0.58 & Fish.no=="5"~ 29,Velocity==0.58 & Fish.no=="8"~ 30,
                                     #depth experiment
                                     Velocity==0.30 & Depth==0.15 & Fish.no=="11"~31, Velocity==0.30 & Depth==0.15 & Fish.no=="12"~32, Velocity==0.30 & Depth==0.15 & Fish.no=="13"~33, Velocity==0.30 & Depth==0.15 & Fish.no=="14"~34,
                                     Velocity==0.30 & Depth==0.30 & Fish.no=="11"~35, Velocity==0.30 & Depth==0.30 & Fish.no=="12"~36, Velocity==0.30 & Depth==0.30 & Fish.no=="13"~37, Velocity==0.30 & Depth==0.30 & Fish.no=="14"~38,
                                     Velocity==0.30 & Depth==0.45 & Fish.no=="11"~39, Velocity==0.30 & Depth==0.45 & Fish.no=="12"~40, Velocity==0.30 & Depth==0.45 & Fish.no=="13"~41, Velocity==0.30 & Depth==0.45 & Fish.no=="14"~42,
                                     Velocity==0.30 & Depth==0.60 & Fish.no=="11"~43, Velocity==0.30 & Depth==0.60 & Fish.no=="12"~44, Velocity==0.30 & Depth==0.60 & Fish.no=="13"~45, Velocity==0.30 & Depth==0.60 & Fish.no=="14"~46
              ))
            
            ## see 1.2_CleandataSupport for the statistical results.
            
            
     # 4.13.2 Adding updated Pipeline delay data (for zeros; non-capture data) as a new column 
        # 0) Code copied from the 1.2_CleandataSupport.R
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
            
            
            # 1) What are the average mean delay time for each Feeder.no vs Velocity? 
               #Splitting Velocity generates results based off of small number of samples.. 
               # so.. don't use Velocity
               library(dplyr)
               meandelaytimeperfeeder <- Pipedelayanalysis %>% # Using only above zero data
                 group_by(Feeder.no) %>%
                 dplyr::summarise(meandelaytime=mean(Pipedelay), sddelaytime=sd(Pipedelay), number=n() )
               data.frame(meandelaytimeperfeeder) 
               
              # add feeders 10, 20, 35, 36, 40 as they didn't have data using above zero
               meandelaytimeperfeeder2 <-data.frame(meandelaytimeperfeeder) %>%    
                    add_row(Feeder.no= c(10,20), meandelaytime= mean(Pipedelayanalysis$Pipedelay[1:922]), sddelaytime=NA) %>% # mean of Velocity experiment
                    add_row(Feeder.no= c(35,36,40), meandelaytime= mean(Pipedelayanalysis$Pipedelay[922:nrow(Pipedelayanalysis)]), sddelaytime=NA) #mean of Depth experiment

        # 2) Change the Pipedelay data that are smaller than 0 --> 0.01 (this is an outliar)
               testdata1_scaled_velocity.block.abs<-data.frame(testdata1_scaled_velocity.block) %>%
               mutate(Pipedelay=replace(Pipedelay, Pipedelay < 0 ,0.01)) 
               
               testdata1_scaled_velocity.block.abs %>%
                 filter(Pipedelay == 0.01)

        # 3) New column with updated (new) pipedelay time; if delay time is zero (the ones not captured), it will use the average time from each feeder       
             Pipedelay_new<-matrix(0,nrow(testdata1_scaled_velocity.block.abs),1)
             
             testdata1_scaled_velocity.block.abs$Pipedelay
             head(testdata1_scaled_velocity.block.abs)
             
              for(k in 1:nrow(testdata1_scaled_velocity.block.abs)) {
                if(testdata1_scaled_velocity.block.abs$Pipedelay[k] == 0 ){
                  Pipedelay_new[k]<- meandelaytimeperfeeder2$meandelaytime[meandelaytimeperfeeder2$Feeder.no == testdata1_scaled_velocity.block.abs$Feeder.no[k] ]  
                   } else { Pipedelay_new[k]<- testdata1_scaled_velocity.block.abs$Pipedelay[k] }
              }
             summary(testdata1_scaled_velocity.block.abs$Pipedelay)
            

               testdata1_scaled_velocity.block.abs2<- testdata1_scaled_velocity.block.abs %>%
               mutate(Preyintroductionseconds_new=Preyintroductionseconds+Pipedelay_new,
                      Pipedelay_new=Pipedelay_new ) 

             
             testdata1_scaled_velocity.block.abs2 %>%
               select(Feeder.no, Velocity, Preyintroductionseconds_new)
             
           # 4) New column; amount of available time from current prey to the next prey
             testdata1_scaled_velocity.block.abs2 %>%
               select(Order,Velocity,Fish.no,Capture,Preyintroductionseconds_new,Pipedelay_new) %>%
               slice(1:10)
             
             Preyintro_interval<-matrix(0,nrow(testdata1_scaled_velocity.block.abs2),1)
             
             for(w in 2:nrow(testdata1_scaled_velocity.block.abs2)) {
               Preyintro_interval[w]<- testdata1_scaled_velocity.block.abs2$Preyintroductionseconds_new[w]-testdata1_scaled_velocity.block.abs2$Preyintroductionseconds_new[w-1]
             }
             
             testdata1_scaled_velocity.block.abs3<-testdata1_scaled_velocity.block.abs2 %>%
               mutate(Preyintro_interval=Preyintro_interval)
            
             str(testdata1_scaled_velocity.block.abs3)
             testdata1_scaled_velocity.block.abs4 <- testdata1_scaled_velocity.block.abs3 %>%
               mutate(Preyintro_interval_new = case_when(Order==1 ~ Preyintroductionseconds_new,
                                                      Order!=1 ~ Preyintro_interval) )
            # See if there are Preyintro_interval_new < 0.
             testdata1_scaled_velocity.block.abs4 %>%
                                 filter(Preyintro_interval_new < -0.5) %>%
                            select(Fish.no, Prey.no, Velocity,Capture, Preyintroductionseconds_new, Preyintro_interval_new)
             summary(testdata1_scaled_velocity.block.abs4$Preyintro_interval_new)
             
             # I will continue working with minus values (n=17) as it shows the intensity of preyintro interval values.
             
             
                        
######################################
#### 5. Get clean data without the NAs; testdata1.2 ####
             
testdata1.2<-testdata1_scaled_velocity.block.abs4 %>%
  filter(!is.na(FocalY) & !is.na(CaptureY) & FocalX !=0 )
nrow(testdata1.2) # 1020 --> 1657 (after Depth data was included)
str(testdata1.2)
names(testdata1.2)
  
