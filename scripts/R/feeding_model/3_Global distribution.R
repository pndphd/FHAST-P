###################################################################################
######################## July 2021, Piccolo files extended ########################
######################  Global distributions of Fish and Feeders  #######################
####################### Jul.20,2021: Separate sheets ##############################
library(rgl)
####################################
#### 1. Feeder distribution ########

    # 1.1 Feeder distribution (because it seems like they detected the prey from one side more than the other)
    par(mfrow=c(1,1))
    
testdata1.1<- testdata1.2%>%
  filter(Prey.no %%1 < 1)

hist(testdata1.1$Feeder.no, xlim=c(1,40), breaks=seq(1,40,1))
    table(testdata1.1$Feeder.no)
    
    str(testdata1.1)
    ggplot(testdata1.1, aes(x=Feeder.no, color=Experiment))+
      geom_histogram(fill="white",alpha=0.5,
                     position="identity",binwidth=1)
    
    # 1.2 With same feeder, does the fish catch them in identical capture coordinates--> NO.. the range changes alot.
    testdata1.1 %>%
     filter(Experiment=="Depth")%>% # or put Experiment instead
      ggplot(aes(x=CaptureY,y=CaptureZ,color=factor(Feeder.no)) ) + geom_point() + 
      xlim(0,1.0) + ylim(0,0.65)
    
    str(testdata1.2)


############################################################
#### 2. Global (total): Histograms of average locations ####
    # 2.1 Focal XYZ 
    par(mfrow=c(3,3))
    data.frame(testdata1.2[testdata1.2$FocalX == 0 | testdata1.2$FocalY == 0 | testdata1.2$FocalZ == 0,]) # check if there is 0 value
    hist(testdata1.2$FocalX, xlim=c(-0.2,1.0)); abline(v = mean(testdata1.2$FocalX), col = "blue", lwd = 2)
    hist(testdata1.2$FocalY, xlim=c(0,1.0)); abline(v = mean(testdata1.2$FocalY), col = "blue", lwd = 2)
    hist(testdata1.2$FocalZ, xlim=c(0,0.35)); abline(v = mean(testdata1.2$FocalZ), col = "blue", lwd = 2)
    
    summary(testdata1.2$FocalX) # -0.1 < X < 1.0
    summary(testdata1.2$FocalY) # 0 < Y < 1.0
    summary(testdata1.2$FocalZ) # 0 < Z < 0.3
    
    # 2.2 Capture XYZ
    data.frame(testdata1.2[testdata1.2$CaptureX == 0 | testdata1.2$CaptureY == 0 | testdata1.2$CaptureZ == 0,]) # check if there is 0 value
    hist(testdata1.2$CaptureX, xlim=c(-0.3,1.0)); abline(v = mean(testdata1.2$CaptureX), col = "blue", lwd = 2)
    hist(testdata1.2$CaptureY, xlim=c(0,1.0)); abline(v = mean(testdata1.2$CaptureY), col = "blue", lwd = 2)
    hist(testdata1.2$CaptureZ, xlim=c(0,0.35)); abline(v = mean(testdata1.2$CaptureZ), col = "blue", lwd = 2)
    
    summary(testdata1.2$CaptureX)
    summary(testdata1.2$CaptureY) # 0<Y<1.0 Although some of them are smaller than 0..let;s proceed??
    summary(testdata1.2$CaptureZ)
    # Key: !is.na () is to select the non-NAs.
    # change the CaptureY (<0) to zero??
    
    # 2.3 Return XYZ
    hist(testdata1.2$ReturnX,  xlim=c(-0.2,1.0)); abline(v = mean(testdata1.2$ReturnX), col = "blue", lwd = 2)
    hist(testdata1.2$ReturnY, xlim=c(0,1.0)); abline(v = mean(testdata1.2$ReturnY), col = "blue", lwd = 2)
    hist(testdata1.2$ReturnZ, xlim=c(0,0.35)); abline(v = mean(testdata1.2$ReturnZ), col = "blue", lwd = 2)
    summary(testdata1.2$ReturnX)
    summary(testdata1.2$ReturnY) # 0<Y<1.0
    rownames(testdata1.2[testdata1.2$Return1Y !=0 & !is.na(testdata1.2$Return1Y) & testdata1.2$Return1Y < 0.01,])
    data.frame(testdata1.2[testdata1.2$Return1X == 0 | testdata1.2$Return1Y == 0 | testdata1.2$Return1Z == 0,])
    summary(testdata1.2$ReturnZ) # 0<Z<0.3
    
    # 2.4 Detect XYZ
    hist(testdata1.2$DetectX, xlim=c(-0.2,1.5)); abline(v = mean(testdata1.2$DetectX), col = "blue", lwd = 2)
    hist(testdata1.2$DetectY, xlim=c(0,1.0)); abline(v = mean(testdata1.2$DetectY), col = "blue", lwd = 2)
    hist(testdata1.2$DetectZ, xlim=c(0,0.35)); abline(v = mean(testdata1.2$DetectZ, na.rm=TRUE), col = "blue", lwd = 2)
    summary(testdata1.2$DetectX)
    summary(testdata1.2$DetectY) # 0<Y<1.0
    testdata1.2[!is.na(testdata1.2$DetectY) & testdata1.2$DetectY < 0.01,]
    summary(testdata1.2$DetectZ) # -0.3<Z<0


#####################################################
#### 3. Global (total) Average: Focal, Capture, Return1, Return ####
    meanfocal<- c(mean(testdata1.2$FocalX,na.rm=TRUE),mean(testdata1.2$FocalY,na.rm=TRUE),mean(testdata1.2$FocalZ,na.rm=TRUE) )
    meancapture<- c(mean(testdata1.2$CaptureX,na.rm=TRUE),mean(testdata1.2$CaptureY,na.rm=TRUE),mean(testdata1.2$CaptureZ,na.rm=TRUE) )
    meanreturn<- c(mean(testdata1.2$ReturnX,na.rm=TRUE),mean(testdata1.2$ReturnY,na.rm=TRUE),mean(testdata1.2$ReturnZ,na.rm=TRUE) )
    
    meanXs<-c(mean(testdata1.2$FocalX,na.rm=TRUE),mean(testdata1.2$CaptureX,na.rm=TRUE),mean(testdata1.2$ReturnX,na.rm=TRUE))
    meanYs<-c(mean(testdata1.2$FocalY,na.rm=TRUE),mean(testdata1.2$CaptureY,na.rm=TRUE),mean(testdata1.2$ReturnY,na.rm=TRUE))
    meanZs<-c(mean(testdata1.2$FocalZ,na.rm=TRUE),mean(testdata1.2$CaptureZ,na.rm=TRUE),mean(testdata1.2$ReturnZ,na.rm=TRUE))
    
    par(mfrow=c(1,1))
    fish.test<-rbind(meanfocal,meancapture,meanreturn)
    
    open3d()
    plot3d(fish.test, shp=16,size=10, col="red") # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(fish.test, shp=16,size=10,col="red",xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(fish.test, shp=16,size=10,col="red",xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.3) )
    
    segments3d(meanXs[c(1,2)],meanYs[c(1,2)],meanZs[c(1,2)],col=4,lwd=2) 
    segments3d(meanXs[c(2,3)],meanYs[c(2,3)],meanZs[c(2,3)],col=5,lwd=2) 

    
########################################################################
##### 4. Each Velocity based Distribution: average locations ###########

velocity_coords<-testdata1.2 %>%
  group_by(Velocity) %>%
  summarise(FocalX=mean(FocalX), FocalY=mean(FocalY),FocalZ=mean(FocalZ),
            CaptureX=mean(CaptureX), CaptureY=mean(CaptureY),CaptureZ=mean(CaptureZ),
            ReturnX=mean(ReturnX), ReturnY=mean(ReturnY),ReturnZ=mean(ReturnZ) )

    # validate my data
    velocity_coords.d<-data.frame(velocity_coords)
    open3d()
    
    # plot focal
    plot3d(velocity_coords.d[,2:4], shp=16,size=10,col='red', add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(velocity_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0)) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(velocity_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.35),useLegend=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    
    #plot capture
    plot3d(velocity_coords.d[,5:7], shp=16,size=10, col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    #plot return
    plot3d(velocity_coords.d[,8:10], shp=16,size=10, col="black", add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    
    segments3d(velocity_coords.d[1,c(2,5)],velocity_coords.d[1,c(3,6)],velocity_coords.d[1,c(4,7)],col=2,lwd=2) 
    segments3d(velocity_coords.d[2,c(2,5)],velocity_coords.d[2,c(3,6)],velocity_coords.d[2,c(4,7)],col=3,lwd=2) 
    segments3d(velocity_coords.d[3,c(2,5)],velocity_coords.d[3,c(3,6)],velocity_coords.d[3,c(4,7)],col=4,lwd=2) 
    segments3d(velocity_coords.d[4,c(2,5)],velocity_coords.d[4,c(3,6)],velocity_coords.d[4,c(4,7)],col=5,lwd=2) 
    segments3d(velocity_coords.d[5,c(2,5)],velocity_coords.d[5,c(3,6)],velocity_coords.d[5,c(4,7)],col=6,lwd=2) 
    segments3d(velocity_coords.d[6,c(2,5)],velocity_coords.d[6,c(3,6)],velocity_coords.d[6,c(4,7)],col=7,lwd=2) 
    

##########################################################################
##### 5. Each Individual fish based distributions; average locations #####

Individual_coords<-testdata1.2 %>%
  group_by(Fish.no) %>%
  summarise(FocalX=mean(FocalX), FocalY=mean(FocalY),FocalZ=mean(FocalZ),
            CaptureX=mean(CaptureX), CaptureY=mean(CaptureY),CaptureZ=mean(CaptureZ),
            ReturnX=mean(ReturnX), ReturnY=mean(ReturnY),ReturnZ=mean(ReturnZ) )

    # validate my data
    Individual_coords.d<-data.frame(Individual_coords)
    open3d()
    
    #plot focal
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red', add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0)) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.35),useLegend=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    #plot capture
    plot3d(Individual_coords.d[,5:7], shp=16,size=10, col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    #plot return
    plot3d(Individual_coords.d[,8:10], shp=16,size=10, col="black", add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    #put Fish.no labels under the plot
    with(Individual_coords.d,text3d(Individual_coords.d[,2],Individual_coords.d[,3],Individual_coords.d[,4],Fish.no, pos=1))
    with(Individual_coords.d,text3d(Individual_coords.d[,5],Individual_coords.d[,6],Individual_coords.d[,7],Fish.no, pos=1))
    with(Individual_coords.d,text3d(Individual_coords.d[,8],Individual_coords.d[,9],Individual_coords.d[,10],Fish.no, pos=1))
    
    segments3d(Individual_coords.d[1,c(2,5)],Individual_coords.d[1,c(3,6)],Individual_coords.d[1,c(4,7)],col=2,lwd=2) 
    segments3d(Individual_coords.d[2,c(2,5)],Individual_coords.d[2,c(3,6)],Individual_coords.d[2,c(4,7)],col=3,lwd=2) 
    segments3d(Individual_coords.d[3,c(2,5)],Individual_coords.d[3,c(3,6)],Individual_coords.d[3,c(4,7)],col=4,lwd=2) 
    segments3d(Individual_coords.d[4,c(2,5)],Individual_coords.d[4,c(3,6)],Individual_coords.d[4,c(4,7)],col=5,lwd=2) 
    segments3d(Individual_coords.d[5,c(2,5)],Individual_coords.d[5,c(3,6)],Individual_coords.d[5,c(4,7)],col=6,lwd=2) 
    

#################################################################
##### 6. One fish one velocity (e.g., Fish 1 velocity 0.29) #####

F1V29<-testdata1.2 %>%
  filter(Fish.no == 1, Velocity == 0.29) %>%
  select(Fish.no, Velocity,FocalX:CaptureZ,DetectX:ReturnZ)
F1V29d<-data.frame(F1V29)

    #plot focal
    open3d()
    plot3d(F1V29[,3:5], shp=16,size=10,col='red', add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(F1V29[,3:5], shp=16,size=10,col='red',xlim=c(-0.1,1.0)) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(F1V29[,3:5], shp=16,size=10,col='red',xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.35),useLegend=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    #plot capture
    plot3d(F1V29[,6:8], shp=16,size=10, col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    #plot return
    plot3d(F1V29[,12:14], shp=16,size=10, col="black", add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    

    
    
#################################################################
##### 7. Only Depth Experiment  #####
##### 7.1. Each Depth based distributions; average locations #####
    depth_coords<-testdata1.2 %>%
      filter(Velocity ==0.30) %>%
      group_by(Depth) %>%
      summarise(FocalX=mean(FocalX), FocalY=mean(FocalY),FocalZ=mean(FocalZ),
                CaptureX=mean(CaptureX), CaptureY=mean(CaptureY),CaptureZ=mean(CaptureZ),
                ReturnX=mean(ReturnX), ReturnY=mean(ReturnY),ReturnZ=mean(ReturnZ, na.rm=TRUE) )
    
    # validate my data
    depth_coords.d<-data.frame(depth_coords)
    open3d()
    
    # plot focal
    plot3d(depth_coords.d[,2:4], shp=16,size=10,col='red', add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(depth_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0)) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(depth_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.65),useLegend=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    
    #plot capture
    plot3d(depth_coords.d[,5:7], shp=16,size=10, col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    #plot return
    plot3d(depth_coords.d[,8:10], shp=16,size=10, col="black", add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
  
##### 7.2. Each Individual fish based distributions; average locations #####
    
    Individual_coords<-testdata1.2 %>%
      filter(Velocity==0.30) %>%
      group_by(Fish.no) %>%
      summarise(FocalX=mean(FocalX), FocalY=mean(FocalY),FocalZ=mean(FocalZ),
                CaptureX=mean(CaptureX), CaptureY=mean(CaptureY),CaptureZ=mean(CaptureZ),
                ReturnX=mean(ReturnX), ReturnY=mean(ReturnY),ReturnZ=mean(ReturnZ) )
    
    # validate my data
    Individual_coords.d<-data.frame(Individual_coords)
    open3d()
    
    #plot focal
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red', add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0)) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    plot3d(Individual_coords.d[,2:4], shp=16,size=10,col='red',xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,0.35),useLegend=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    #plot capture
    plot3d(Individual_coords.d[,5:7], shp=16,size=10, col="blue", add=TRUE) # ,xlim=c(-0.1,1.0) ) #, ylim=c(0,1.0), zlim=c(0,-0.3) )
    #plot return
    plot3d(Individual_coords.d[,8:10], shp=16,size=10, col="black", add=TRUE) # ,xlim=c(-0.1,1.0) , ylim=c(0,1.0), zlim=c(0,-0.3) )
    
    #put Fish.no labels under the plot
    with(Individual_coords.d,text3d(Individual_coords.d[,2],Individual_coords.d[,3],Individual_coords.d[,4],Fish.no, pos=1))
    with(Individual_coords.d,text3d(Individual_coords.d[,5],Individual_coords.d[,6],Individual_coords.d[,7],Fish.no, pos=1))
    with(Individual_coords.d,text3d(Individual_coords.d[,8],Individual_coords.d[,9],Individual_coords.d[,10],Fish.no, pos=1))
    
    segments3d(Individual_coords.d[1,c(2,5)],Individual_coords.d[1,c(3,6)],Individual_coords.d[1,c(4,7)],col=2,lwd=2) 
    segments3d(Individual_coords.d[2,c(2,5)],Individual_coords.d[2,c(3,6)],Individual_coords.d[2,c(4,7)],col=3,lwd=2) 
    segments3d(Individual_coords.d[3,c(2,5)],Individual_coords.d[3,c(3,6)],Individual_coords.d[3,c(4,7)],col=4,lwd=2) 
    segments3d(Individual_coords.d[4,c(2,5)],Individual_coords.d[4,c(3,6)],Individual_coords.d[4,c(4,7)],col=5,lwd=2) 
   
     ## end of script
    
    