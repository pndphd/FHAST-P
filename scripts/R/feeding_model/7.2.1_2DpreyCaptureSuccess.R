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

May3,2022 
Look into why PiccoloD data is increasing rather than constant.
 Maybe I digitiezed the wrong figure. Or the values are just wrong? I should have subtracted 15?
 Since my experiment is controlling for the total amount of prey..it makes sense to be equal.. not increase.
 Go back and look at the excel sheet@!@!

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
Capturesuccess_FHAST2022(numberofprey = 1000, preydensity=1, tankdepth=30,
                         preyspeeds=20, fishsize=0.07) # , maxpreyconsideration=?

Add maxpreyconsideration
# add the number of maximum prey it can make decisions. Even when there are 10 preys within the detection zone,
# I don't think they would be able to determine the possibility of catches for all 10 preys at once and
# make optimal choices.. I think they consider up to a certain amount of preys to reach a reasonable capture success.
# This is because, animals in general don't always make the best choice due to lack of information or 
# the lack of abilities in general. On top of that, thinking too much could be detrimental for their survival.
# For example, eating a small prey coming towards you right now is beneficial then waiting for a larger prey to come even if you know it will come
# becusae, you never know what will happen in the next second..

Aug.23, 2022
Add 50 preys to the number you want to test not 10. This is because the prey range could be up to 25 in our site.
So, we are trying to remove the bias from the first and last 25 preys at the beginning of the experiment as they move in and out of the chamber...


Below code works fine but I am trying to make a whole new method (including the max number of considertaion)
on the next version so just save this version as it is..


Capturesuccess_FHAST2022<- function(numberofprey,preydensity,tankdepth,preyspeeds,fishsize){
tankwidth<-100

(model.result.length<-length(preydensity) *length(tankdepth) * length(preyspeeds) * length(fishsize))
model.results<-matrix(0,model.result.length,7)
colnames(model.results)<-c("CaptureSuccess","Density","Depth","Velocity","FishLength","NumberOfPrey_1min","CaptureSuccess_1min")
  
#for (j in preyspeeds){
  
prey_catch<-matrix(0,numberofprey+10+1,14) # first 10 is for trial (help remove bias from early entry of prey)
colnames(prey_catch)<-c("PreyX2","PreyZ2","PreyY2","FishX1","FishZ1","FishY1", "Dist_to prey_lateral","Intercept_maxdist","Capture","Dist_preytoorigin","Return_maxdist","newfishX3","newfishZ3","newfishY3")

#Sampling prey randomly
prey_catch[,1]<-runif(numberofprey+10+1, min=-(tankwidth/2)+0.02,max=(tankwidth/2)-0.02); 
prey_catch[,2]<-runif(numberofprey+10+1, min=1.2,max=tankdepth-0.02); 

# Estimate max detection distance (formula from 4.5_Detectionrange_parameter.R)
# Note: the x and y axes are opposite in this script.
(prey.lateral.dist.y.b<- 100*(0.88915+(-1.59704*(preyspeeds*0.01)^2)+(0.73901*(preyspeeds*0.01))+(0.63983*fishsize^2)+(0.06965*fishsize)) )

(detect.width.x.a<- 100*(0.53269+(0.01084*((preyspeeds*0.01)^2))+(-0.52829*(preyspeeds*0.01))+(-1.50790*fishsize^2)+(-0.22174*fishsize)) ) # Fig.4 Piccolo

(detect.height.z.c<- 100*(0.9202+(0.7394*((preyspeeds*0.01)^2))+(-0.7709*(preyspeeds*0.01))+(-34.5407*fishsize^2)+(2.9616*fishsize)) ) # y value # 41.15282 -0.25872*velocities )# same as the x width  #23.8  # mean of detect.heights


# Fitting PreyY2 to a ellipsoid. This is preyspeedsust to run for the first row.

(prey_catch[1,3]<-prey.lateral.dist.y.b * sqrt(1-(prey_catch[1,1]^2/detect.width.x.a^2)- ((prey_catch[1,2]-2)^2/detect.height.z.c^2)) )
prey_catch[1,3][is.na(prey_catch[1,3])]<-0

# If density increases, the next prey will have arrived in a closer distance from the fish or sometimes inside of the ellipsoid.
# So, for the ones that have already arrived closer (due to high prey density), I am forcing to 
# change the Y-distance (where the prey is down the water) not to fit to the ellipsoid. These prey would have the distance of 120/preydensity.

if( prey_catch[1,3] > (100/preydensity) ){ 
  prey_catch[1,3] <- (100/preydensity)
} else{ prey_catch[1,3] <- prey_catch[1,3] }



#fish first launch location
prey_catch[1,4]<- 0
prey_catch[1,5]<- 2
prey_catch[1,6]<- 0
fish_originx<-prey_catch[1,4]
fish_originz<-prey_catch[1,5]
fish_originy<-prey_catch[1,6]

(interceptionspeed <- (-0.11*(preyspeeds/100)+0.45)*100) # Piccolo paper reference. converted to cm
(returnspeed <- (1.39*(preyspeeds/100)-0.13)*100)# Piccolo paper reference

# Fish attacking abilities to determine whether it will make the move or not depending on how many prey comes through..
BTPT<-(100/preydensity)/preyspeeds # Between Prey-Time: Amount of time between one prey to the next.
Intercept_successdist<- interceptionspeed * BTPT # What's the max distance the fish can go while successfully catching.

# Up to how many preys can it detect?
# This is based on how far the fish can detect within the 'prey.lateral.dist.y.b'
# and also the prey density.Here,  Higher prey density means the fish will have more preys inside the detection area.


#(dist.btw.prey<-100/preydensity) # This is the amount of distance between prey
#(maxpreyconsider<-floor(prey.lateral.dist.y.b/dist.btw.prey)+1) # round down the value for example, floor(2.7) gives 2


for (i in 1:numberofprey+10) { # do not add additional 1 on the end since we just needed an extra row for the codes to pass..
  
  # Dist_to_ prey_lateral; 2D distance from fish to the potential prey (in 2D) using Euclidean distance formula
  (prey_catch[i,7]<-sqrt((prey_catch[i,1]-prey_catch[i,4])^2+(prey_catch[i,2]-prey_catch[i,5])^2) )
  prey_catch
  

  #Intercept max 2D lateral distance; The amount of distance the fish can attack in 2D lateral (since we are only considering 
  # 2D movement of the fish),based on the amount of time there is left for the prey to pass the origin plane
  (fish_interception_max_dist <- Intercept_successdist )#(prey_catch[i,3]/j)*interceptionspeed)
  prey_catch[i,8]<-fish_interception_max_dist
  
# new code Aug.15, 2022
  # the way I code is that the we (or the fish) already know whether they will catch the prey or not.
  # This is to maximize capture success by minimizing unnecessary movements.
  # Once we know that the capture is possible or not, the fish decide to move..
  
  
  # 1. Capture determination.  Based on the dist to prey destination and fish location
 
  # Decision to see if the incoming prey is even going to be in the detection range in the first place..
  thisfish_ellipsoid_touchY2<-prey.lateral.dist.y.b * sqrt(1-( (prey_catch[i,1]-prey_catch[i,12])^2/detect.width.x.a^2)- ((prey_catch[i,2]-prey_catch[i,13])^2/detect.height.z.c^2)) 
  thisfish_ellipsoid_touchY2[is.na(thisfish_ellipsoid_touchY2)]<-0
 
   if( thisfish_ellipsoid_touchY2 > 0 & prey_catch[i,7] < prey_catch[i,8]){ # if the fish is available to go further than the prey destination, it's a catch
    prey_catch[i,9]<-1
  } else { # if not, the fish missed it..
    prey_catch[i,9]<-2
  }
  

  #2. New fish location 1 (intercept prey)
  if(prey_catch[i,9]==1){ # if prey catch is possible (successful), put fish to the prey 2d lateral location
    prey_catch[i,12]<-prey_catch[i,1]; # New fish location equals to where the prey was caught
    prey_catch[i,13]<-prey_catch[i,2]; 
  }else{  # If the prey is not catchable, remain at the spot and I will update the location below based on whether the next prey is capturable
    prey_catch[i,12]<- prey_catch[i,4] 
      prey_catch[i,13]<-prey_catch[i,5]
  }
  
  
  # 3. How much time (or distance) can fish move back to origin after capture success (or fail)?
  # This is determined by whether the next prey is catchable or not.
  # IF catchable, it will directly attack from there. If not, it will return towards the origin.
  
  # Dist_to_ prey_lateral; 2D distance from fish to the next potential prey (in 2D) using Euclidean distance formula
  (prey_catch[i+1,7]<-sqrt((prey_catch[i+1,1]-prey_catch[i,12])^2+(prey_catch[i+1,2]-prey_catch[i,13])^2) )
  prey_catch[i+1,8]<-fish_interception_max_dist
  
  newfish_ellipsoid_touchY2<-prey.lateral.dist.y.b * sqrt(1-( (prey_catch[i+1,1]-prey_catch[i,12])^2/detect.width.x.a^2)- ((prey_catch[i+1,2]-prey_catch[i,13])^2/detect.height.z.c^2)) 
  newfish_ellipsoid_touchY2[is.na(newfish_ellipsoid_touchY2)]<-0
  

  if( newfish_ellipsoid_touchY2 > 0 & prey_catch[i+1,7]< prey_catch[i+1,8]) { # If the next prey is a potential capture success, then remain at the location
    prey_catch[i+1,9]<-1 # assign success to Capture column
    prey_catch[i+1,4]<- prey_catch[i,12] 
    prey_catch[i+1,5]<-prey_catch[i,13]
  } else{ # If the next prey is a fail, then return towards the origin.
    prey_catch[i+1,9]<-2 # assign failure to Capture column
    returnmax_dist<-(BTPT*returnspeed)
    prey_catch[i,11]<-returnmax_dist
        # return towards the origin
        if(prey_catch[i,11]>= sqrt((prey_catch[i,12]-fish_originx)^2 + (prey_catch[i,13]-fish_originz)^2)){ # if max returning distance is longer than the dist to prey lateral, simply assign origin locations.
            prey_catch[i+1,4]<- fish_originx
            prey_catch[i+1,5]<- fish_originz
        } else{ # If max returning distance is shorter than the full distance to the origin, stop in between.
          fishtorigin<-sqrt((prey_catch[i,12]-fish_originx)^2 + (prey_catch[i,13]-fish_originz)^2)
          prey_catch[i+1,4]<- prey_catch[i,12]  + (prey_catch[i,11]/fishtorigin)*(fish_originx-prey_catch[i,12]) ; # fish_originx # # consider prey location + return to the origin with left over time
          prey_catch[i+1,5]<- prey_catch[i,13]  + (prey_catch[i,11]/fishtorigin)*(fish_originz-prey_catch[i,13]); # fish_originy # # consider prey location + return to the origin with left over time
              }
        }
    
  
  
    # 4. Determine the new Y prey_catch[i+1,3] of the Prey location, before going over to the next row...
# If there was a catch, the available time to the next catchable prey would be 100/density
# If there was a catch, but next is a fail, then extra time is given to wait for the next prey on top of returning to the origin
# If there was a fail, but then a catch, it will have had more time to react to the next prey..
# If there was a fail, but then another fail, it will have a lot more ready time for the next one.. (but this case, give a max limit which is the ellipsoid boundary distance)

        if(prey_catch[i,9]==1 & prey_catch[i+1,9]==1){ # when catch and another catch
          prey_catch[i+1,3]<-100/preydensity
        } else if(prey_catch[i,9]==1 & prey_catch[i+1,9]==2){ # when catch and fail; distance between prey-distance the fish would need to go back to the origin
          prey_catch[i+1,3]<- (100/preydensity)-(preyspeeds*( prey_catch[i,7]/returnspeed) )
        } else if(prey_catch[i,9]==2 & prey_catch[i+1,9]==1){ # when fail and catch; then the fish would just go for the next prey.. so twice the difference  of distance between preys
          prey_catch[i+1,3]<-2*(100/preydensity)
        } else {# (prey_catch[i,9]==2 & prey_catch[i+1,9]==2){# when fail and another fail: 
          prey_catch[i+1,3]<-2*(100/preydensity)
        }

# fit the preyY2 to the new fish location; this will be used to determine if the prey_catch[i+1,3] should be changed.
# If newfish_ellipsoid_touchY2
        
        if (prey_catch[i+1,3]> newfish_ellipsoid_touchY2 ){ # if the distance of the prey Y2 is further than the ellipsoid fit,
          # Change the value to newfish_ellipsoid_touchY2
          # Above inside the condtions, I will use the fitted Y2 based on the new fish location which is the 
          prey_catch[i+1,3]<-newfish_ellipsoid_touchY2
        } else{
          prey_catch[i+1,3]<-prey_catch[i+1,3]
        }

  } # end of loop i

prey_catch.m<-as.data.frame(prey_catch)[11:nrow(prey_catch)-1,] # remove first ten and bottom row

captureatvelocity<-prey_catch.m[prey_catch.m$Capture <2,];nrow(captureatvelocity)

data.location <- which(preyspeeds==preyspeeds)
captureatvelocity<-prey_catch.m[prey_catch.m$Capture <2,]

model.results[data.location,1]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,2]<- preydensity
model.results[data.location,3]<- tankdepth
model.results[data.location,4]<- preyspeeds
model.results[data.location,5]<- fishsize
model.results[data.location,6]<- (preyspeeds*60)/(100/preydensity) # The number of prey that would pass the 100cm area in 1 min.
model.results[data.location,7]<- model.results[data.location,1] * 0.01* model.results[data.location,6]

data.frame(model.results)
} 


#### Run the function using the purrr package ####

library(purrr)
# install.packages("furrr")
library(furrr)
# install.packages("tictoc")
library(tictoc)
preydensity<-c(0.001,1,5,10,15,20)
tankdepth=c(20,40,60,80,100) 
preyspeeds<-c(20,30,40,50,60)
fishsize<-c(0.05,0.06,0.07,0.08,0.1)
numberofprey<-100

(combination_df<-expand.grid(numberofprey,preydensity,tankdepth,preyspeeds,fishsize))
colnames(combination_df)<-c("numberofprey","preydensity","tankdepth","preyspeeds","fishsize")
5^4
#preydensity<-c(5,10)
##tankdepth<-c(30)
#preyspeeds<-c(20)
#fishsize<-c(0.07,0.08)
#numberofprey<-100


tic()
future::plan(multicore)# , workers="7")
result_CS<- future_pmap(combination_df,Capturesuccess_FHAST2022) %>%
  transpose() %>% map(unlist) %>% data.frame()
toc()

library(tictoc)
tic()
result_CS<- pmap(combination_df,Capturesuccess_FHAST2022) %>%
  transpose() %>% map(unlist) %>% data.frame()
toc()


#################################################
### End of simulations

result_CS%>%names()
result_CS%>%select(FishLength)%>%table()
result_CS<-result_CS %>%
  mutate(CaptureSuccess=case_when((Density==0.001) ~ 0,
                                  #TRUE~CaptureSuccess,
                                  (Velocity==0.001) ~0,
                                  #TRUE~ CaptureSuccess,
                                  (Depth==0.001)~ 0,
                                  TRUE~ CaptureSuccess), # True here means: when none of the previous conditions are true..
         CaptureSuccess_1min=case_when((Density==0.001)~0,
                                       #TRUE~CaptureSuccess_1min,
                                       (Velocity==0.001)~0,
                                       #TRUE~ CaptureSuccess_1min,
                                       (Depth==0.001)~0,
                                       TRUE~ CaptureSuccess_1min) ) 


result_CS %>%
  filter(Density>=1 | Velocity>=1| Depth>=1) %>%
  select(CaptureSuccess)
# Plot
result_CS %>%
  filter(Density>=1 & Velocity>=1 & Depth>=1)%>%
  mutate(Velocity=factor(Velocity))%>%
  filter(Depth==20, FishLength==0.07)%>%
  ggplot(aes(x=Density, y=CaptureSuccess, color=Velocity) )+
  geom_point()+
  geom_line()+
  #scale_fill_discrete(name = "Velocity (cm/s)")+
  ylim(0,100)+
  xlab("Prey Density/m2")+
  ylab("Capture Success (%)")+
  labs(fill = "Dose (mg)")
 

result_CS %>%
  filter(Density>=1 & Velocity>=1 & Depth>=1)%>%
  mutate(Depth=factor(Depth))%>%
  filter(Density!=0)%>%
  filter(Velocity==20, FishLength==0.07)%>%
  ggplot(aes(x=Density, y=CaptureSuccess, color=Depth) )+
  geom_point()+
  geom_line()+
  #scale_fill_discrete(name = "Velocity (cm/s)")+
  ylim(0,100)+
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



