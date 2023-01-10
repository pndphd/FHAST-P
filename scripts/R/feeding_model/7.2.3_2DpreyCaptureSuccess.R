###################################################################################
###################################################################################
######## 2D Prey Capture success calculation #########
# Mar.30, 2021
library(tidyverse)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(purrr)
library(tictoc)

# Assumptions: 
# 1.1) a prey comes in a certain rate (0.1 meter apart)
# 1.2) one prey per each unit of volume 
# 2) 0.6 meter X 0.3 X 0.4 meter is the feeding area
# 3.1) Fish gets one prey and attacks the next prey immediately (no delay of swallowing)
# 3.2) Fish tends to move to the origin after the catch
# 4.1) This is a 2D model. Fish does not move back and forth. 
#     So, the model does not consider the distance/time it should consider for moving back and forth. 
#     So, when the next prey is coming towards, it will have both benefits of not spending energy moving. 
#     I am not sure how this will increase/decrease the capture rate. 
#     It might level off from both benefits/costs
# 5.1) Prey comes in a normal distribution pattern, closer to the center volume of the area.
# 6.1) Fish would attempt to attack every prey, rather than strategically (time efficient) aiming for a specific prey item. 
#    So, it can not choose a certain prey to increase capture success. It attempts on all prey.

#install.packages("truncnorm")
library(truncnorm)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(MuMIn)


# capture success rate from Piccolo 2008 (can be used to verify my model??)
Piccolo2008<-read.csv("C:/Users/harbi/Documents/UC Santa Cruz project/UCSC_Research/Small_experiment/Piccolo_capturesuccess.csv",header=TRUE)
names(Piccolo2008)<-c("Y_coord","Z_coord","Depth","Velocity","Capturesuccess","Species","Experiment") # HEre the x and Y coords are supposed to be Y and Z.

PiccoloV<-Piccolo2008 %>%
  select(Y_coord,Z_coord,Depth,Velocity,Capturesuccess,Experiment)%>%
  filter(Experiment=="Velocity")
PiccoloD<-Piccolo2008 %>%
  select(Y_coord,Z_coord,Depth,Velocity,Capturesuccess,Experiment)%>%
  filter(Experiment=="Depth")
str(PiccoloV)
hist(PiccoloV$Capturesuccess+1)
ggplot(PiccoloV, aes(x=Capturesuccess))+
  geom_histogram()

m1<-lm(Capturesuccess~Y_coord+Z_coord+Velocity, data=PiccoloV);plot(m1)
m1.2<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Y_coord^2)+I(Z_coord^2)+I(Velocity^2), data=PiccoloV);plot(m1.2);summary(m1.2)
m1.3<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Z_coord^2), data=PiccoloV);
m1.4<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Y_coord^2), data=PiccoloV);
m1.5<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Velocity^2), data=PiccoloV);
m1.6<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Z_coord^2)+I(Velocity^2), data=PiccoloV);
m1.7<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Y_coord^2)+I(Velocity^2), data=PiccoloV);

plot(m1.7);
modelm<-list(m1,m1.2,m1.3,m1.4,m1.5,m1.6,m1.7)
model.namesm<-c('m1','m1.2','m1.3','m1.4','m1.5','m1.6','m1.7')
(summaryAICm<-aictab(modelm,model.namesm,second.ord=TRUE))
summary(m1.7) # adjusted R-square: 0.7341

# Capture success Model averaging
m1.2<-lm(Capturesuccess~Y_coord+Z_coord+Velocity+I(Y_coord^2)+I(Z_coord^2)+I(Velocity^2), data=PiccoloV);
summary(m1.2)
options(na.action = "na.fail") # need to run dredge
m1.2AV<-dredge(m1.2,beta="none",evaluate = T,rank=AICc)
options(na.action = "na.omit") # set back to default

m1.2AV2<-(model.avg(m1.2AV,cumsum(weight) <= .95)) # subset = delta <= 4)) # cumsum(weight) <= .7)) 
summary(m1.2AV2)

# Example
v=c(0.58,0.30,0.48,0.56,0.58)
Ycoord<-c(5,10,15,25,30)
Zcoord<-c(15,20,35,5,25)

# Polynomial formula to predict capture success in Piccolo 2008 data.
152.6-(277.8*v)+(147.9*v^2)+(-1.827*Ycoord)+(0.03128*Ycoord^2)+(-0.6142*Zcoord)+(0.006679*Zcoord^2)

log(2.7)
plot(Capturesuccess~Velocity, data=PiccoloV)


# I should later try to change the detection distance and width (so that it can exactly replicate the piccolo paper results)
# This means excluding some of the outliars e.g., only keeping 98% or 95 % of the data.






 # running individual values in a function formula
library(tictoc)

tic()
Capturesuccess_FHAST2022(simulation=1000,preydensity=270, maxpreyconsider=301, tankdepth=16,
                         tanklength=110, tankwidth=24, preyspeeds=13, fishsize=0.07 ) 
toc()


Capturesuccess_FHAST2022<- function(simulation,preydensity,maxpreyconsider,
                                    tankdepth,tanklength,tankwidth,preyspeeds,fishsize){
# Fixed
  
  
# Varied
#  simulation=2000
#  preyspeeds=20
#  fishsize=0.07
#  tankdepth=30
#  preydensity=1
#  maxpreyconsider=25 # in a meter square distance. Preys are aligned and in equal distance between preys.
#  tanklength<-100
#  tankwidth<-100
  
  # Internal   
  stopsimulation<-simulation+500-300
  numberofprey<-simulation+500

model.result.length<-length(preydensity) *length(tankdepth) * length(preyspeeds) * length(fishsize)
model.results<-matrix(0,model.result.length,7)
colnames(model.results)<-c("CaptureSuccess","Density","Depth","Velocity","FishLength","NumberOfPrey_1min","CaptureSuccess_1min")
  
  
prey_catch<-matrix(0,numberofprey,14) # first 10 is for trial (help remove bias from early entry of prey)
colnames(prey_catch)<-c("PreyX2","PreyZ2","PreyY2","FishX1","FishZ1","FishY1", "Dist_to prey_lateral","Intercept_successdist","Capture","Dist_preytoorigin","Return_maxdist","newfishX3","newfishZ3","newfishY3")

#Sampling prey randomly
set.seed(1234) # set.seed(NULL)
prey_catch[,1]<-runif(numberofprey, min=-(tankwidth/2)+0.02,max=(tankwidth/2)-0.02); 
prey_catch[,2]<-runif(numberofprey, min=1.2,max=tankdepth-0.02); 

# Estimate max detection distance (formula from 4.5_Detectionrange_parameter.R)
# Note: the x and y axes are opposite in this script compared to Piccolo paper.
# Here, Y is the distance towards the feeder, X is the width, Z is the depth
prey.lateral.dist.y.b<- 100*(0.88915+(-1.59704*(preyspeeds*0.01)^2)+(0.73901*(preyspeeds*0.01))+(0.63983*fishsize^2)+(0.06965*fishsize)) 

detect.width.x.a<- 100*(0.53269+(0.01084*((preyspeeds*0.01)^2))+(-0.52829*(preyspeeds*0.01))+(-1.50790*fishsize^2)+(-0.22174*fishsize))  # Fig.4 Piccolo

detect.height.z.c<- 100*(0.9202+(0.7394*((preyspeeds*0.01)^2))+(-0.7709*(preyspeeds*0.01))+(-34.5407*fishsize^2)+(2.9616*fishsize))  # y value # 41.15282 -0.25872*velocities )# same as the x width  #23.8  # mean of detect.heights


#fish first launch location
prey_catch[1,4]<- 0
prey_catch[1,5]<- 2
prey_catch[1,6]<- 0
fish_originx<-prey_catch[1,4]
fish_originz<-prey_catch[1,5]
fish_originy<-prey_catch[1,6]
prey_catch[1,9]<- 2
prey_catch[1,12]<- prey_catch[1,4]
prey_catch[1,13]<- prey_catch[1,5]

interceptionspeed <- (-0.11*(preyspeeds/100)+0.45)*100 # Piccolo paper reference. converted to cm
returnspeed <- (1.39*(preyspeeds/100)-0.13)*100# Piccolo paper reference

# Fish attacking abilities to determine whether it will make the move or not depending on how many prey comes through..

 BTPT<-(tanklength/preydensity)/preyspeeds # Between Prey-Time: Amount of time between one prey to the next.
 Intercept_successdist<- interceptionspeed * BTPT # What's the max distance the fish can go while successfully catching.

# Up to how many preys can it detect?
# This is based on how far the fish can detect within the 'prey.lateral.dist.y.b'
# and also the prey density.Here,  Higher prey density means the fish will have more preys inside the detection area.

(dist.btw.prey<-tanklength/(preydensity) ) # This is the amount of distance between prey
(totalPrey.DetectionZone<-floor(prey.lateral.dist.y.b/dist.btw.prey)+1 ) # Amount of prey within the detection zone. round down the value for example, floor(2.7) gives 2

 # Deciding the maximum prey to consider
 if (totalPrey.DetectionZone >= maxpreyconsider){
   totalPrey.DetectionZone <- maxpreyconsider
 } else{
   totalPrey.DetectionZone <- totalPrey.DetectionZone
 }
 

#Returning code has changed slightly. Previously, my fish would return towards the position as soons as it captures the prey.lateral.dist.y.b
#However, now as soon as it captures the prey, it then searches (and determines) the next prey in that position rather than
#returning towards the origin. It will only try to retun to the position, when the prey within the detection area are
#all not catchable..


#1) Most case, the fish will probaby not make it to the origin (0,2) since it will continuously have to make decisions about the next incoming prey
#2) For approximation, the fish makes decisions based on the time between preys rather than when the prey reaches its detection area. This is becuase, there is not much diference in prey entering time depending on the prey density.
#This is becuase, 100 is the max (in order to control for density) and varying densities does not necessarily
#lead to larger or shorter distance between BTSD. Assuming 98 is the furthest Y, density 1 would have 100-98cm from the next prey to enter.
#Density 3 would have 99-98cm. Density 10 would have 100-98cm... and so on. So, it does not make much difference.
#3) I realized that the mechanism when catchable prey detected vs. none would have a different process...
#When no prey is detected, the function should read the next row, whereas when the next catchable prey is detected,
#it rather skips rows and directly moves to the row of the catchable prey.
##This means that when there is no prey, it continuosly updates the decision based on the incoming prey and whenever there is no prey, it will move slowly return towards the origin.
#Also when the prey is in the detection distance but in a far range, it will not preventively move towards the prey but instead remain in the posiiton unitl the
#prey gets closer and then bursts to catch it.
#4) All in all, the fish will move towrads the origin bits by bits when there is a miss...
#This mechanism is probably more realistic since the fish did not always quickly (or directly) return to the bottom origin in the videos taken from the wild. 
#Nor does it prepare the arrival of the fish by moving early. And also probably becuase the prey density was very low in the experiment.

 #### Function: Which prey is captured? ####
 nextCapturePrey.row<-function(p.row.start,p.row.end){
   # This code determines whether the next several preys are a capture or miss up to the max prey consideration preys.
   # It is determined based on the current location of the fish.
   # It doesn't think about how far to return and the new fish location, or how to respond to the next prey.
   # It only determines if it is a capture or miss over the next few preys.
   # After that, it will determine which prey (first, second or third etc.) is possible to catch.
   
   
   # 2.1) This is to determine the distance of the preys  
     #p.row.start<-1
   (p.row.start.plus1<-p.row.start+1) # excluding the prey already on the 2d plane
     #p.row.end<-2
   
   for (a in p.row.start.plus1:p.row.end){
     prey_catch[a,3]<-dist.btw.prey*(a-p.row.start)
   } 
   
   # 2.2) Determine if any of the preys are within the reaction distance from the first fish's perspective
   for (d in p.row.start.plus1:p.row.end){
     
     fitPreyEllipsoid<-prey.lateral.dist.y.b * sqrt(1-( (prey_catch[d,1]-prey_catch[p.row.start,4])^2/detect.width.x.a^2)- ((prey_catch[d,2]-prey_catch[p.row.start,5])^2/detect.height.z.c^2)) 
     fitPreyEllipsoid[is.na(fitPreyEllipsoid)]<- -0.1
     if(fitPreyEllipsoid < 0){ # if it doesn't fit I will assign -0.1
       prey_catch[d,3]<- -0.1
     } else{
       prey_catch[d,3]<-prey_catch[d,3]
     }
   }
   # 2.3 Is the prey within capture success distance?
   #May remove BTPT and the formula below
   #BTPT<-(tanklength/preydensity+1)/preyspeeds # Between Prey-Time: Amount of time between one prey to the next.
   #Intercept_successdist<- interceptionspeed * BTPT # What's the max distance the fish can go while successfully catching.
   
   for ( f in p.row.start.plus1:p.row.end){
     Intercept_successdist<-interceptionspeed * (prey_catch[f,3]/preyspeeds)
     prey_catch[f,8]<-Intercept_successdist
   }
   
   for ( g in p.row.start.plus1:p.row.end){
     
     # Dist_to_ prey_lateral; 2D distance from fish to the potential prey (in 2D) using Euclidean distance formula
     (prey_catch[g,7]<-sqrt((prey_catch[g,1]-prey_catch[p.row.start,4])^2+(prey_catch[g,2]-prey_catch[p.row.start,5])^2) )
     #prey_catch
     # I may need to find out a way to hold (or fix) the first fish location e.g., prey_catch[1,4]. How do I keep the [1,4]?
   }
   
   # 2.4 Is it capture success or a miss?
   
   for ( h in p.row.start.plus1:p.row.end){
     if (prey_catch[h,8]<0){
       prey_catch[h,9]<-2
     } else if(prey_catch[h,8] >= prey_catch[h,7]){ # if the capturesucces distance is longer than the distance to prey lateral, this is a capture.
       prey_catch[h,9]<-1
     } else {
       prey_catch[h,9]<-2
     }
   }
   
   # 2.5 Which prey is captured the earliest? 
   
   earliestPreyCaptureRow<-min(which(prey_catch[c(p.row.start.plus1:p.row.end),9]==1))
   if(earliestPreyCaptureRow==Inf){
     earliestPreyCaptureRow<-0
   }
   earliestPreyCaptureRow
   #print(prey_catch)
   
 }
 ## end of nextCaptuePrey.row function
 
 
i<-1
nextCapturePrey.ROW<-nextCapturePrey.row(p.row.start=i,p.row.end=(i-1)+totalPrey.DetectionZone)


repeat{
  
  
if(nextCapturePrey.ROW==0){ # if prey is not available over the next several steps, then return towards the origin
  
  # If max returning distance is shorter than the full distance to the origin, stop in between.
    # Amount of time possible to return is equal to the amount of time between preys because
    # as soon as the next prey moves inside the detection zone, the fish would need to decide whether it will catch or not.
    # Currently, I am using an approximate time (time between preys) rather than the specific time it will take the next prey to touch the detection zone.
  
  # 1) newfishXZ3 <- FishXZ1 + ReturnAlpha
  prey_catch[i,11]<- BTPT * preyspeeds
  fishtorigin<-sqrt((prey_catch[i,4]-fish_originx)^2 + (prey_catch[i,5]-fish_originz)^2) 

  if(prey_catch[i,11] >=fishtorigin ){ 
    # if the return_dist is further than the distance from fish to origin it will reach the origin
    prey_catch[i,12]<-fish_originx
    prey_catch[i,13]<-fish_originz
  } else if(fishtorigin==0) {
    # if fish to origin is zero, it means that the fish is already in the origin.
    prey_catch[i,12]<-fish_originx
    prey_catch[i,13]<-fish_originz
  } else{
    # This is when the return calculation is needed because it is limited by time (BTPT)
    prey_catch[i,12]<- prey_catch[i,4] + (prey_catch[i,11]/fishtorigin)*(fish_originx-prey_catch[i,4]) #  consider prey location + return to the origin with left over time
    prey_catch[i,13]<- prey_catch[i,5] + (prey_catch[i,11]/fishtorigin)*(fish_originz-prey_catch[i,5]) #  consider prey location + return to the origin with left over time
  }
  # 2) Move to next row
  i<-i+1

  # 3) Capture <-2
  prey_catch[i,9]<-2
  

  # 4) FishXZ1 (next row) <- newfishXZ3 (previous row)
  prey_catch[i,4]<- prey_catch[i-1,12]  
  prey_catch[i,5]<- prey_catch[i-1,13]
  
  # 5) Determine Prey Capture 
  nextCapturePrey.ROW<-nextCapturePrey.row(p.row.start=i, p.row.end=(i-1)+totalPrey.DetectionZone )
 
  # end of Prey Miss 
  
} else { # if there are capturable preys in the capture success distance
  
  # 1) No return (skip current row)
  # 2) Move to the capturable prey row
  
  i<-i+nextCapturePrey.ROW
  
  # 3) FishXZ1 <- PreyCoordinate
      # Fish capture the prey so the new coordinate will be where the prey was
  prey_catch[i,4]<-prey_catch[i,1]
  prey_catch[i,5]<-prey_catch[i,2]
  
  # 4) Capture <-1
  prey_catch[i,9]<-1

  #5) Determine Prey Capture 
  nextCapturePrey.ROW<-nextCapturePrey.row(p.row.start=i, p.row.end=(i-1)+totalPrey.DetectionZone)
  
}

  if(i>stopsimulation){ # (i>stopsimulation) I am giving ample room to test various prey densities up to 50.
    
    break
  
   } 
  
 } # end of repeat

# print(prey_catch)
prey_catch.df<-data.frame(prey_catch)
CS.n<-prey_catch.df %>%
  slice(501:stopsimulation)%>% #  slice(100:5100)
  nrow()
CS.c<-prey_catch.df %>%
  slice(501:stopsimulation)%>%
  filter(Capture==1)%>%
  nrow()
CS<-CS.c/CS.n

preyin1min<-(preyspeeds*60)/(tanklength/preydensity) # The number of prey that would pass the 100cm or tanklength area in 1 min.
CSin1min<-CS*preyin1min

model.results<-c(CS, CSin1min, preydensity,maxpreyconsider,
                 tankdepth,tankwidth,tanklength,preyspeeds,fishsize,simulation)
names(model.results)<-c("CS", "CSin1min", "preydensity","maxpreyconsider",
                        "tankdepth","tankwidth","tanklength","preyspeeds","fishsize","simulation")
print(model.results)


} # end of function Capturesuccess_FHAST2022



#### Run the function using the purrr package ####

library(purrr)
# install.packages("furrr")
library(furrr)
# install.packages("tictoc")
library(tictoc)
preydensity<-c( 1.46, 7.3, 36.5, 182.5, 87.6, 131.4, 262.8) # 0.70,1.43,4.23,8.46,12.69)
tankdepth=c(16,30,50,100)
tankwidth=c(61,80,100)
tanklength=c(150)
preyspeeds<-c(25,32,40)
fishsize<-c(0.32) # 0.07)
maxpreyconsider<-c(2,3,4,5,10) #,26)
simulation<-c(1000)


(combination_df<-expand.grid(simulation,preydensity,maxpreyconsider,tankdepth,tankwidth,tanklength,preyspeeds,fishsize))
colnames(combination_df)<-c("simulation","preydensity","maxpreyconsider","tankdepth","tankwidth","tanklength","preyspeeds","fishsize")

# library(purrr)
#library(furrr)
#tic()
#future::plan(multicore)# , workers="7")
#result_CS<- future_pmap(combination_df,Capturesuccess_FHAST2022) %>%
#           transpose() %>% map(unlist) %>% data.frame()
#toc()

tic()
result_CS<- pmap(combination_df,Capturesuccess_FHAST2022) %>% transpose() %>% map(unlist) %>% data.frame()
toc()

#library(map)


#################################################
### End of simulations

names(result_CS)<-c("CS","CSin1min","Density","MaxPreyConsider","Depth","Width","Length","Velocity","FishLength","Simulations")

result_CS2<-result_CS %>%
  mutate(CS=case_when((Density==0.001) ~ 0,
                                  #TRUE~CaptureSuccess,
                                  (Velocity==0.001) ~0,
                                  #TRUE~ CaptureSuccess,
                                  (Depth==0.001)~ 0,
                                  TRUE~ CS), # True here means: when none of the previous conditions are true..
         CSin1min=case_when((Density==0.001)~0,
                                       #TRUE~CaptureSuccess_1min,
                                       (Velocity==0.001)~0,
                                       #TRUE~ CaptureSuccess_1min,
                                       (Depth==0.001)~0,
                                       TRUE~ CSin1min) ) 


result_CS2 %>%
  filter(MaxPreyConsider==2, Depth==50, Velocity==30) 
  # filter(Density>=1 | Velocity>=1| Depth>=1) %>%
  # select(CaptureSuccess)

preydensity<-c( 1.46, 7.3, 36.5, 182.5, 87.6, 131.4, 262.8) # 0.70,1.43,4.23,8.46,12.69)
tankdepth=c(16,30,50,100)
tankwidth=c(61,80,100)
tanklength=c(150)
preyspeeds<-c(25,32,40)
fishsize<-c(0.32)
maxpreyconsider<-c(2,3,4,5,10)


result_CS2 %>%
  select(MaxPreyConsider)%>%
  table()

# Plot
result_CS2 %>%
  #filter(Density>=1 & Velocity>=1 & Depth>=1)%>%
  #mutate(Velocity=factor(Velocity))%>%
  filter(Depth==16,Width==61,Length==150, Velocity==40)%>% # MaxPreyConsider==15
  ggplot(aes(x=Density, y=CSin1min, color=as.factor(MaxPreyConsider)) )+
  geom_point()+
  geom_line()+
  #scale_fill_discrete(name = "Velocity (cm/s)")+
  ylim(0,400)+
  xlab("Prey Density/tank length")+
  ylab("Prey Intake/min")
  
 

result_CS2 %>%
  filter(Density>=0 & Velocity==13 & Depth==16 & Width==24 & Length==110)%>%
  # mutate(Depth=factor(Depth))%>%
  #filter(Density!=0)%>%
  #filter(Velocity==13, FishLength==0.07)%>%
  ggplot(aes(x=Density, y=CS, color=Depth) )+
  geom_point()+
  geom_line()+
  #scale_fill_discrete(name = "Velocity (cm/s)")+
  ylim(0,1)+
  xlab("Prey Density/m2")+
  ylab("Capture Success (%)")
 


# Statistical modeling # 
result_CS %>%
  select(CaptureSuccess) %>%
  #filter(CaptureSuccess<3)%>%
  ggplot(aes(x=CaptureSuccess)) +
  geom_histogram()

result_CS %>% filter(CaptureSuccess==0) %>% nrow()/
  125/nrow(result_CS) # roughly there are 16% of zeros in the data..

result_CS%>%names()
# Correlation check #
names(result_CS)
variable.check <- result_CS[,c(2,3,4,5)]
str(variable.check)
par(mfrow=c(1,1))
chart.Correlation(variable.check, histogram=TRUE, pch=19)

result_CS$CaptureSuccess_1min.r<-round(result_CS$CaptureSuccess_1min) 

result_CS %>%
  ggplot(aes(x=log(CaptureSuccess_1min.r)))+
  geom_histogram()
result_CS %>%
 # filter(CaptureSuccess_1min.r<2)%>%
  ggplot(aes(x=(CaptureSuccess_1min.r)))+
  geom_histogram()

CS1<-glm(CaptureSuccess_1min.r~poly(Density,2)+poly(Depth,2)+poly(Velocity,2)+FishLength, data=result_CS, family=poisson)
summary(CS1)
CS1.1<-glm(CaptureSuccess_1min.r~I(Density)+I(Density^2)+I(Depth)+I(Depth^2)+I(Velocity)+I(Velocity^2)+FishLength, data=result_CS, family=poisson)
summary(CS1.1)
CS2<-glm(CaptureSuccess_1min.r~poly(Density,2)+poly(Depth,2)+poly(Velocity,2),            data=result_CS, family=poisson)
CS2.1<-glm(CaptureSuccess_1min.r~I(Density)+I(Density^2)+I(Depth)+I(Depth^2)+I(Velocity)+I(Velocity^2), data=result_CS, family=poisson)
summary(CS2.1)
CS3<-glm(CaptureSuccess_1min.r~Density+        poly(Depth,2)+poly(Velocity,2),            data=result_CS, family=poisson)
CS4<-glm(CaptureSuccess_1min.r~poly(Density,2)+poly(Depth,2)+     Velocity,            data=result_CS, family=poisson)
CS5<-glm(CaptureSuccess_1min.r~poly(Density,2)+     Depth+   poly(Velocity,2),            data=result_CS, family=poisson)
CS6<-glm(CaptureSuccess_1min.r~Density+        poly(Depth,2)+    (Velocity),            data=result_CS, family=poisson)
CS7<-glm(CaptureSuccess_1min.r~Density+        poly(Depth,2)+     Velocity,            data=result_CS, family=poisson)
CS8<-glm(CaptureSuccess_1min.r~Density+        Depth+        poly(Velocity,2),            data=result_CS, family=poisson)
CS9<-glm(CaptureSuccess_1min.r~poly(Density,2)+poly(Depth,2)+poly(Velocity,2)+poly(FishLength,2), data=result_CS, family=poisson)
summary(CS2)

modelCS<-list(CS1,CS2,CS3,CS4,CS5,CS6,CS7,CS8,CS9)
model.namesCS<-c('CS1','CS2','CS3','CS4','CS5','CS6','CS7','CS8','CS9')
(summaryAIC<-aictab(modelCS,model.namesCS,second.ord=FALSE))

# Density vs Captures success
library(modelr)
ExpandGrid_CS.DEN<-data_grid(result_CS,
                         Depth=30,
                         Velocity=30,
                         Density=seq_range(Density,8),.model=CS2)

library(insight)
??get.predicted
insight_CS<-as.data.frame(get_predicted(CS2.1,data=ExpandGrid_CS.DEN,ci=0.95, preserve_range=FALSE)) # this format is to just use the T2.3I parameter
insight_CS$CaptureSuccess_1min.r<-insight_CS$Predicted
CSplot.df<-cbind(ExpandGrid_CS.DEN,insight_CS)# 

  CSplot<-
    result_CS %>%
    filter(Density>=1)%>%
    ggplot(aes(x=Density, y=CaptureSuccess_1min.r))+
    geom_jitter(width=1,alpha=0.2)

  CSplot+
  geom_ribbon(data=CSplot.df,
              aes(ymin=CI_low,ymax=CI_high),
              fill="steelblue2",
              alpha=0.3)+
    geom_line(data=CSplot.df,aes(x=Density,y=CaptureSuccess_1min.r),color="firebrick")+
  ggtitle("Number of prey captures/min")+
  xlab("Prey density/m2")+
  ylab("Number of preys")+
  ylim(0,65)

  # Velocity vs Captures success
  library(modelr)
  ExpandGrid_CS.V<-data_grid(result_CS,
                           Depth=30,
                           Velocity=seq_range(Velocity,8),
                           Density=10,.model=CS1)
  
  insight_CS<-as.data.frame(get_predicted(CS2.1,data=ExpandGrid_CS.V,ci=0.95, preserve_range=FALSE)) # this format is to just use the T2.3I parameter
  insight_CS$CaptureSuccess_1min.r<-insight_CS$Predicted
  CSplot.df<-cbind(ExpandGrid_CS.V,insight_CS)# 
  
  (CSplot<-
      result_CS%>%
      filter(Density>=1)%>%
      ggplot(aes(x=Velocity, y=CaptureSuccess_1min.r))+
    geom_jitter(width=2,alpha=0.1) )
  
  CSplot+
     geom_ribbon(data=CSplot.df,
                aes(ymin=CI_low,ymax=CI_high),
                fill="steelblue2",
                alpha=0.7)+
    geom_line(data=CSplot.df,aes(x=Velocity,y=CaptureSuccess_1min.r),color="firebrick")+
    ggtitle("Number of prey captures/min")+
    xlab("Water Velocity(cm/s)")+
    ylab("Number of preys")+
    ylim(0,65)
  # Depth vs Captures success
  library(modelr)
  ExpandGrid_CS.DP<-data_grid(result_CS,
                             Depth=seq_range(Depth,8),
                             Velocity=30,
                             Density=10,.model=CS1)
  
  insight_CS<-as.data.frame(get_predicted(CS2.1,data=ExpandGrid_CS.DP,ci=0.95, preserve_range=FALSE)) # this format is to just use the T2.3I parameter
  insight_CS$CaptureSuccess_1min.r<-insight_CS$Predicted
  CSplot.df<-cbind(ExpandGrid_CS.DP,insight_CS)# 
  
  (CSplot<-
      result_CS%>%
      filter(Density>=0)%>%
      ggplot(aes(x=Depth, y=CaptureSuccess_1min.r))+
    #geom_point(size=0)+
    geom_jitter(width=3,alpha=0.2) )
  
  CSplot+
     geom_ribbon(data=CSplot.df,
                aes(ymin=CI_low,ymax=CI_high),fill="steelblue2",
                alpha=0.5)+
    geom_line(data=CSplot.df,aes(x=Depth,y=CaptureSuccess_1min.r),color="firebrick")+
    
    ggtitle("Number of prey captures/min")+
    xlab("Water Depth(cm)")+
    ylab("Number of preys")+
    ylim(0,65)
   
  
## end of script

  Comment Aug.17, 2022
  I would want to add Depth 0.001 and make more combinations in the data to test.
  Later, I would use negative binomial or zero inflated model instead of the regular poisson model
  
 

4000mg/0.07mg per individfual or 0.59mg
4000/0.59
4000/0.07

# How many hours does it need for a fish to have 57200 or 6800 prey a day?
57000/(20*60) # 47.5 hours if the prey is very small
6800/(20*60) # 5.6 hours if the prey is a reasonable size








#### Brine shrimp weight ####
# ref: https://pubag.nal.usda.gov/catalog/2220148 # it's also in the Mendeley

shrimplength<-c(0.92,0.92,0.96,1.04,2.60,2.74,2.81,2.98)
shrimpweight<-c(0.40,0.41,0.43,0.48,4.4,4.4,4.4,4.9) # multiply 10^-5 to be in grams term
summary(lm(shrimpweight~shrimplength))
plot(x=shrimplength,y=shrimpweight)
-1.70338+2.23427*(2.0)
# 2.76516 * 10^-5(g)


#### Figure: Density and Velocity and their effect on necessary feeding hours to fill their daily Cmax ####
Densities<-c(1,2,3,6)
Velocities<-c(0.3,0.6)
SrcFig1<-expand.grid(Densities,Velocities)
names(SrcFig1)<-c("Densities","Velocities")
Hours<-c(44.3,22.1,14.7,7.37,22.1,11.1,7.4,3.7)
SrcFig1$Hours<-Hours

str(SrcFig1)
SrcFig1 %>%
  ggplot(aes(x=Velocities,y=Hours,group=Densities,label=Hours,color=as.factor(Densities)))+
  geom_point(show.legend = TRUE)+
  geom_line(size=1)+
  labs(colour = "Prey Densities")+
  scale_y_continuous(name="Hours needed to pass a fish (hrs)",
                     limits=c(0,50))+
  geom_text(size = 3,position = position_stack(vjust = 0.1))
  

#### Density and Velocity and capturesuccess ####

######velocity varies; Time is not equal!


densityvary.result<-read.csv("C:/Users/harbi/Documents/UC Santa Cruz project/UCSC_Research/Small_experiment/Piccolo_research/FHAST/scripts/R/feeding_model/Piccolo_capturesuccess.densityvary.csv",header=TRUE)
names(densityvary.result)<-c("velocity","modeltype","capturerate","density")

densityvary.result %>%
  ggplot(aes(x=velocity, y=capturerate, group=density,color=as.factor(density))) +
  geom_point()+
  geom_point(data=densityvary.result%>%filter(modeltype == "CS_Piccolo"),color="black")+
  labs(color="Densities")+
 geom_line(data=densityvary.result%>%filter(modeltype != "CS_Piccolo"),size=1)+
  stat_smooth(data=densityvary.result%>%filter(modeltype == "CS_Piccolo"),method = "lm",color="black") + #, col = "red")+
  scale_x_continuous(name="Velocities",
                     limits=c(27,62))+
  scale_y_continuous(name="Amount of captures",
                     limits=c(0,100))
  
##### density figure type 1
  # view in density to the x axis; Time is not equal!

  ggplot(data=densityvary.result%>%filter(modeltype!="CS_Piccolo"),aes(x=density, y=capturerate, group=velocity, color=as.factor(velocity))) +
  geom_point()+
  geom_point(data=densityvary.result%>%filter(modeltype == "CS_Piccolo"),color=NA )+
    labs(color="Velocity")+
  geom_line(data=densityvary.result%>%filter(modeltype != "CS_Piccolo"),size=1)+
    scale_x_continuous(name="Densities",
                     limits=c(1,6))+
  scale_y_continuous(name="Amount of captures",
                     limits=c(0,100))

  ##### density figure type 2; When time is given equal
  densityvary.result %>%filter(modeltype !="CS_Piccolo")%>%
    mutate(capturerate_time=case_when(density==1~capturerate,
                                      density==2~capturerate*2,
                                      density==3~capturerate*3,
                                      density==4~capturerate*4,
                                      density==5~capturerate*5,
                                      density==6~capturerate*6))%>%
    ggplot(aes(x=density, y=capturerate_time, group=velocity, color=as.factor(velocity))) +
    geom_point()+
    labs(color="Velocity")+
    geom_line(size=1)+
    scale_x_continuous(name="Densities")+
    scale_y_continuous(name="Amount of captures",
                       limits=c(0,200))



