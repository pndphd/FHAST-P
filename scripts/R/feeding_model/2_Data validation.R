###################################################################################
######################## July 2021, Piccolo files extended ########################
##################  Compare with Mathematica & Piccolo Figures 5 ##################
####################### Jul.20,2021: Separate sheets ##############################
library(rgl)


########### Velocity data comparison #########


#### 1. Compare with Mathematica Figures  (2D figures) ####
# Plot in 2D to make sure I am using the correct data ####  
# Check with one fish and one velocity (e.g., SH1V2 )

    
    # 1.1 Focal
    F1V29<-testdata1.2 %>%
      filter(Velocity != 0.30, Fish.no == 1, Velocity == 0.29) %>%
      select(Fish.no, Velocity,FocalX:CaptureZ,DetectX:ReturnZ)
    F1V29d<-data.frame(F1V29)
    
     par(mfrow=c(3,1))
    plot(F1V29[,c(3,5)], xlim=c(-0.1,1.0), ylim=c(0,0.3));grid()
    plot(F1V29[,c(3,4)], xlim=c(-0.1,1.0), ylim=c(0,1.0));grid()
    plot(F1V29[,c(4,5)], xlim=c(0,1.0), ylim=c(0,0.3));grid()
    
    # 1.2 Capture
    plot(F1V29[,c(6,8)], xlim=c(-0.1,1.0), ylim=c(0,0.3));grid()
    plot(F1V29[,c(6,7)], xlim=c(-0.2,1.0), ylim=c(0,1.0));grid()
    plot(F1V29[,c(7,8)], xlim=c(0,1.0), ylim=c(0,0.3));grid()
    
    # 1.3 Return
    #remove rows with 0's in the data
    F1V29r<-F1V29 %>%
      filter(ReturnX !=0 & ReturnY !=0 & ReturnZ !=0)
    plot(F1V29r[,c(12,14)], xlim=c(0,1.0), ylim=c(0,0.3));grid()
    plot(F1V29r[,c(12,13)], xlim=c(-0.2,1.0), ylim=c(0,1.0));grid()
    plot(F1V29r[,c(13,14)], xlim=c(0,1.0), ylim=c(0,0.3));grid()
    

################################################
#### 2. Compare with Piccolo 2007 (velocity paper) Figure 5. ####

  # 2.1 Compare Capture
    # Normalize data
    Capture.sub<-testdata1.2 %>%
      filter(Velocity !=0.30) %>%
    select(Velocity,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) 
  
    medfpX<-median(Capture.sub$FocalX)
    medfpY<-median(Capture.sub$FocalY)
    medfpZ<-median(Capture.sub$FocalZ)
    
    Capture.sub$nCaptureX <- medfpX+(Capture.sub$CaptureX-Capture.sub$FocalX)
    Capture.sub$nCaptureY <- medfpY+(Capture.sub$CaptureY-Capture.sub$FocalY)
    Capture.sub$nCaptureZ <- medfpZ+(Capture.sub$CaptureZ-Capture.sub$FocalZ)

    ## Compare with Piccolo 2007
      table(Capture.sub$Velocity)
      par(mfrow=c(3,2))
      #velocity 0.29
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.29]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.29], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)", main="Normalized prey capture side view XZ,v=0.29");grid()
      #velocity 0.38
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.38]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.38], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)", main="Normalized prey capture side view XZ,v=0.38");grid()
      #velocity 0.45
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.45]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.45], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)",main="Normalized prey capture side view XZ,v=0.45");grid()
      #velocity 0.49
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.49]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.49], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)",main="Normalized prey capture side view XZ,v=0.49");grid()
      #velocity 0.55
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.55]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.55], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)",main="Normalized prey capture side view XZ,v=0.55");grid()
      #velocity 0.58
      plot(Capture.sub$nCaptureX[Capture.sub$Velocity==0.58]-medfpX, Capture.sub$nCaptureZ[Capture.sub$Velocity==0.58], xlim=c(-1.0,1.0), ylim=c(0,0.35), xlab="X-axis (towards the feeder)",ylab="Y-axis (water depth)",main="Normalized prey capture side view XZ,v=0.58");grid()

  # 2.2 Compare Detection location  
    # Normalize data
      Detect.sub<-testdata1.2 %>%
        filter(Velocity != 0.30) %>%
        select(Velocity, DetectX:DetectZ, FocalX,FocalY,FocalZ)
      
      str(testdata1.2)
      medfpX<-median(Detect.sub$FocalX, na.rm=TRUE)
      medfpY<-median(Detect.sub$FocalY, na.rm=TRUE)
      medfpZ<-median(Detect.sub$FocalZ, na.rm=TRUE)
      
      Detect.sub$nDetectX<-medfpX+(Detect.sub$DetectX-Detect.sub$FocalX)
      Detect.sub$nDetectY<-medfpY+(Detect.sub$DetectY-Detect.sub$FocalY)
      Detect.sub$nDetectY.oneside<-NA
      for (k in 1:nrow(Detect.sub) ){
        if(Detect.sub$nDetectY[k] > medfpY ){
          Detect.sub$nDetectY.oneside[k] <- medfpY - (Detect.sub$nDetectY[k]-medfpY)
        } else{Detect.sub$nDetectY.oneside[k]<- Detect.sub$nDetectY[k] }
      } ;  
      hist(Detect.sub$nDetectY) # validate if all Y's have moved to one side
      hist(Detect.sub$nDetectY.oneside) # validate if all Y's have moved to one side
      
      Detect.sub$nDetectZ<-medfpZ+(Detect.sub$DetectZ-Detect.sub$FocalZ)
      
    # Plot  
      par(mfrow=c(3,2))
      # velocity 0.29
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.29], Detect.sub$nDetectZ[Detect.sub$Velocity==0.29], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.29], Detect.sub$nDetectZ[Detect.sub$Velocity==0.29], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()
      # velocity 0.38
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.38], Detect.sub$nDetectZ[Detect.sub$Velocity==0.38], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.38], Detect.sub$nDetectZ[Detect.sub$Velocity==0.38], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()
      # velocity 0.45
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.45], Detect.sub$nDetectZ[Detect.sub$Velocity==0.45], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.45], Detect.sub$nDetectZ[Detect.sub$Velocity==0.45], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()
      # velocity 0.49
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.49], Detect.sub$nDetectZ[Detect.sub$Velocity==0.49], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.49], Detect.sub$nDetectZ[Detect.sub$Velocity==0.49], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()
      # velocity 0.55
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.55], Detect.sub$nDetectZ[Detect.sub$Velocity==0.55], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.55], Detect.sub$nDetectZ[Detect.sub$Velocity==0.55], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()
      # velocity 0.58
      plot(Detect.sub$nDetectY.oneside[Detect.sub$Velocity==0.58], Detect.sub$nDetectZ[Detect.sub$Velocity==0.58], xlim=c(0,1.0), ylim=c(0,0.35), main="Normalized prey detection Front view YZ");grid()
      plot(Detect.sub$nDetectX[Detect.sub$Velocity==0.58], Detect.sub$nDetectZ[Detect.sub$Velocity==0.58], xlim=c(0,1.5), ylim=c(0,0.35), main="Normalized prey detection Side view XZ");grid()

########### End of Velocity data #######
      
      
#####################################################################################      
#####################################################################################     
      
      
########### Beginning of Depth data #########
      
      ################################################
      #### 2. Compare with Piccolo Depth paper 2003 Figure  ####
      
      # 2.0 Compare Focal points (Depth 0.60 and Fish.no==4)
        # X vs Z
        testdata1.2 %>% # 
        filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
        select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
        ggplot(aes(x=FocalX, y=FocalZ))+
        geom_point()+
        xlim(-0.3,1.3)+
        ylim(0,0.15)
      
        # X vs Y
        testdata1.2 %>% # 
        filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
        select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
        ggplot(aes(x=FocalX, y=FocalY))+
        geom_point()+
        xlim(-0.3,1.3)+
        ylim(0,1)
        
        # Y vs Z
        testdata1.2 %>% # 
        filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
        select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
        ggplot(aes(x=FocalY, y=FocalZ))+
        geom_point()+
        xlim(-0.1,1.1)+
        ylim(-0.01,0.6)
      
      
      # 2.1 Compare Capture (Depth 0.60 and Fish.no==4)
      
        # X vs Z
        testdata1.2 %>% # 
          filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
          select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
          ggplot(aes(x=CaptureX, y=CaptureZ))+
          geom_point()+
          xlim(-0.3,1.3)+
          ylim(-0.01,0.65)
        
        # X vs Y
        testdata1.2 %>% # 
          filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
          select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
          ggplot(aes(x=CaptureX, y=CaptureY))+
          geom_point()+
          xlim(-0.3,1.3)+
          ylim(0,1)
        
        # Y vs Z
        testdata1.2 %>% # 
          filter(Velocity == 0.30 & Depth==0.60 & Fish.no==4) %>%
          select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ) %>%
          ggplot(aes(x=CaptureY, y=CaptureZ))+
          geom_point()+
          xlim(0,1)+
          ylim(-0.2,0.65)
        
        
       # 2.2 Compare Detection distance with Piccolo Fig.4  (for each Depth all fish inucluded)
        
        # Conclusion: the data are correctly in but the z-axis is just 0.2 lower. However, the author
        # may have made a mistake to plot them using the minus values of z axis before transforming them.
        # This will have the coordinates to be upside down.
        
        # 2.2.1 Depth 0.15
        
        Detect.sub.depth15<-testdata1.2 %>% # 
          filter(FocalX !=0 & DetectX !=0 & Velocity == 0.30 & Depth==0.15) %>%
          select(Fish.no,Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ)
        
        # Normalize data

          medfpX15<-median(Detect.sub.depth15$FocalX)
          medfpY15<-median(Detect.sub.depth15$FocalY)
          medfpZ15<-median(Detect.sub.depth15$FocalZ)
          
          Detect.sub.depth15$nDetectX<-medfpX15+(Detect.sub.depth15$DetectX-Detect.sub.depth15$FocalX)
          Detect.sub.depth15$nDetectY<-medfpY15+(Detect.sub.depth15$DetectY-Detect.sub.depth15$FocalY)
          Detect.sub.depth15$nDetectY.oneside<-NA
          for (k in 1:nrow(Detect.sub.depth15) ){
            if(Detect.sub.depth15$nDetectY[k] > medfpY15 ){
              Detect.sub.depth15$nDetectY.oneside[k] <- medfpY15 - (Detect.sub.depth15$nDetectY[k]-medfpY15)
            } else{Detect.sub.depth15$nDetectY.oneside[k]<- Detect.sub.depth15$nDetectY[k] }
          } ;  
          Detect.sub.depth15$nDetectZ<-medfpZ15+(Detect.sub.depth15$DetectZ-Detect.sub.depth15$FocalZ)
          
          ## Compare with Piccolo 2007 (side view: X:Z and Y:Z)
          Detect.sub.depth15 %>%
            filter(Fish.no==4) %>%
            ggplot(aes(x=nDetectX, y=nDetectZ, colour=as.factor(Fish.no) ))+
            geom_point()+
            xlim(-0.5,1.5)+
            ylim(-0.1,0.6)

          Detect.sub.depth15 %>%
            ggplot(aes(x=nDetectX, y=nDetectZ))+
            geom_point()+
            xlim(-0.5,1.5)+
            ylim(0,0.6)
          
          Detect.sub.depth15 %>%
            ggplot(aes(x=nDetectY.oneside, y=nDetectZ)) +
            geom_point()+
            xlim(0,1.0)+
            ylim(0,0.6)
          
          
        # 2.2.2 Depth 0.30
          
          Detect.sub.depth30<-testdata1.2 %>% # 
            filter(FocalX !=0 & DetectX !=0 & Velocity == 0.30 & Depth==0.30) %>%
            select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ)
          
          # Normalize data
          
          medfpX30<-median(Detect.sub.depth30$FocalX)
          medfpY30<-median(Detect.sub.depth30$FocalY)
          medfpZ30<-median(Detect.sub.depth30$FocalZ)
          
          Detect.sub.depth30$nDetectX<-medfpX30+(Detect.sub.depth30$DetectX-Detect.sub.depth30$FocalX)
          Detect.sub.depth30$nDetectY<-medfpY30+(Detect.sub.depth30$DetectY-Detect.sub.depth30$FocalY)
          Detect.sub.depth30$nDetectY.oneside<-NA
          for (k in 1:nrow(Detect.sub.depth30) ){
            if(Detect.sub.depth30$nDetectY[k] > medfpY30 ){
              Detect.sub.depth30$nDetectY.oneside[k] <- medfpY30 - (Detect.sub.depth30$nDetectY[k]-medfpY30)
            } else{Detect.sub.depth30$nDetectY.oneside[k]<- Detect.sub.depth30$nDetectY[k] }
          } ;  
          Detect.sub.depth30$nDetectZ<-medfpZ30+(Detect.sub.depth30$DetectZ-Detect.sub.depth30$FocalZ)
          
          ## Compare with Piccolo 2007 (side view: X:Z)
          Detect.sub.depth30 %>%
            ggplot(aes(x=nDetectX, y=nDetectZ))+
            geom_point()+
            xlim(-0.5,1.2)+
            ylim(0,0.6)
          Detect.sub.depth30 %>%
            ggplot(aes(x=nDetectY.oneside, y=nDetectZ ))+
            geom_point()+
            xlim(0,1.0)+
            ylim(0,0.6)
      
        # 2.2.3 Depth 0.45
          
          Detect.sub.depth45<-testdata1.2 %>% 
            filter(FocalX !=0 & DetectX !=0 & Velocity == 0.30 & Depth==0.45) %>%
            select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ)
          
          # Normalize data
          
          medfpX45<-median(Detect.sub.depth45$FocalX)
          medfpY45<-median(Detect.sub.depth45$FocalY)
          medfpZ45<-median(Detect.sub.depth45$FocalZ)
          
          Detect.sub.depth45$nDetectX<-medfpX45+(Detect.sub.depth45$DetectX-Detect.sub.depth45$FocalX)
          Detect.sub.depth45$nDetectY<-medfpY45+(Detect.sub.depth45$DetectY-Detect.sub.depth45$FocalY)
          Detect.sub.depth45$nDetectY.oneside<-NA
          for (k in 1:nrow(Detect.sub.depth45) ){
            if(Detect.sub.depth45$nDetectY[k] > medfpY30 ){
              Detect.sub.depth45$nDetectY.oneside[k] <- medfpY45 - (Detect.sub.depth45$nDetectY[k]-medfpY45)
            } else{Detect.sub.depth45$nDetectY.oneside[k]<- Detect.sub.depth45$nDetectY[k] }
          } ;  
          Detect.sub.depth45$nDetectZ<-medfpZ45+(Detect.sub.depth45$DetectZ-Detect.sub.depth45$FocalZ)
          
          ## Compare with Piccolo 2007 (side view: X:Z)
          Detect.sub.depth45 %>%
            ggplot(aes(x=nDetectX, y=nDetectZ))+
            geom_point()+
            xlim(-0.5,1.2)+
            ylim(0,0.6)
          Detect.sub.depth45 %>%
            ggplot(aes(x=nDetectY.oneside, y=nDetectZ ))+
            geom_point()+
            xlim(0,1.0)+
            ylim(0,0.6)
          
      # 2.2.4 Depth 0.60
          
          Detect.sub.depth60<-testdata1.2 %>% # using data that did not transform the z-axis
            filter(FocalX !=0 & DetectX !=0 & Velocity == 0.30 & Depth==0.60) %>%
            select(Depth,DetectX:DetectZ,CaptureX:CaptureZ, FocalX,FocalY,FocalZ)
          
          # Normalize data
          
          medfpX60<-median(Detect.sub.depth60$FocalX)
          medfpY60<-median(Detect.sub.depth60$FocalY)
          medfpZ60<-median(Detect.sub.depth60$FocalZ)
          
          Detect.sub.depth60$nDetectX<-medfpX60+(Detect.sub.depth60$DetectX-Detect.sub.depth60$FocalX)
          Detect.sub.depth60$nDetectY<-medfpY60+(Detect.sub.depth60$DetectY-Detect.sub.depth60$FocalY)
          Detect.sub.depth60$nDetectY.oneside<-NA
          for (k in 1:nrow(Detect.sub.depth60) ){
            if(Detect.sub.depth60$nDetectY[k] > medfpY60 ){
              Detect.sub.depth60$nDetectY.oneside[k] <- medfpY60 - (Detect.sub.depth60$nDetectY[k]-medfpY60)
            } else{Detect.sub.depth60$nDetectY.oneside[k]<- Detect.sub.depth60$nDetectY[k] }
          } ;  
          Detect.sub.depth60$nDetectZ<-medfpZ60+(Detect.sub.depth60$DetectZ-Detect.sub.depth60$FocalZ)
          
          ## Compare with Piccolo 2007 (side view: X:Z)
          Detect.sub.depth60 %>%
            ggplot(aes(x=nDetectX, y=nDetectZ))+
            geom_point()+
            xlim(-0.5,1.2)+
            ylim(0,0.65)
          Detect.sub.depth60 %>%
            ggplot(aes(x=nDetectY.oneside, y=nDetectZ))+
            geom_point()+
            xlim(0,1)+
            ylim(0,0.6)
             
         
     