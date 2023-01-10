###################################################################################
###################################################################################
######## 2D Prey Capture success calculation #########
# Mar.30, 2021
library(tidyverse)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)

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

Capturesuccess_Piccolo2007(numberofprey = 50, preydensity=1, fishsize=0.053, 
                           tankwidth=60,tankdepth=c(15,30,45,60), # tankwidth=60 or tankdepth=30
                           ) # or VaryVelocity
                          
Capturesuccess_Piccolo2007<- function(numberofprey,preydensity,fishsize,tankwidth,tankdepth){
 
preydensity<-c(1,5,10,15,20)
tankdepth=c(20,30,50,80,100) 
preyspeeds<-c(20,30,40,50,60)
fishsize<-c(0.05,0.06,0.07,0.08,0.1)

preydensity<-5
tankdepth<-30
preyspeeds<-20
fishsize<-0.07

numberofprey<-100
tankwidth=100


(model.result.length<-length(preydensity) *length(tankdepth) * length(preyspeeds) * length(fishsize))
model.results<-matrix(0,model.result.length,6)
colnames(model.results)<-c("CaptureSuccess","Density","Depth","Velocity","NumberOfPrey_1min","CaptureSuccess_1min")
  
#for (j in preyspeeds){
  
prey_catch<-matrix(0,numberofprey+10+1,14) # first 10 is for trial (help remove bias from early entry of prey)
colnames(prey_catch)<-c("PreyX2","PreyZ2","PreyY2","FishX1","FishZ1","FishY1", "Dist_to prey_lateral","Intercept_maxdist","Capture","Dist_preytoorigin","Return_maxdist","newfishX3","newfishZ3","newfishY3")

#Sampling prey randomly
prey_catch[,1]<-runif(numberofprey+10+1, min=-(tankwidth/2)+0.02,max=(tankwidth/2)-0.02); 
prey_catch[,2]<-runif(numberofprey+10+1, min=1.2,max=tankdepth-0.02); 

# Estimate max detection distance (formula from 4.5_Detectionrange_parameter.R)
# Note: the x and y axes are opposite in this script.
(prey.lateral.dist.y.b<- 100*(0.88915+(-1.59704*(j*0.01)^2)+(0.73901*(j*0.01))+(0.63983*fishsize^2)+(0.06965*fishsize)) )

(detect.width.x.a<- 100*(0.53269+(0.01084*((j*0.01)^2))+(-0.52829*(j*0.01))+(-1.50790*fishsize^2)+(-0.22174*fishsize)) ) # Fig.4 Piccolo

(detect.height.z.c<- 100*(0.9202+(0.7394*((j*0.01)^2))+(-0.7709*(j*0.01))+(-34.5407*fishsize^2)+(2.9616*fishsize)) ) # y value # 41.15282 -0.25872*velocities )# same as the x width  #23.8  # mean of detect.heights


# Fitting PreyY2 to a ellipsoid. This is just to run for the first row.

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

(interceptionspeed <- (-0.11*(j/100)+0.45)*100) # Piccolo paper reference. converted to cm
(returnspeed <- (1.39*(j/100)-0.13)*100)# Piccolo paper reference

# Fish attacking abilities to determine whether it will make the move or not depending on how many prey comes through..
BTPT<-(100/preydensity)/j # Amount of time between one prey to the next.
Intercept_successdist<- interceptionspeed * BTPT # What's the max distance the fish can go while successfully catching.


for (i in 1:numberofprey+10) { # do not add additional 1 on the end since we just needed an extra row for the codes to pass..

  
  # Dist_to_ prey_lateral; 2D distance from fish to the potential prey (in 2D) using Euclidean distance formula
  (prey_catch[i,7]<-sqrt((prey_catch[i,1]-prey_catch[i,4])^2+(prey_catch[i,2]-prey_catch[i,5])^2) )
  prey_catch
  

  #Intercept max 2D lateral distance; The amount of distance the fish can attack in 2D lateral (since we are only considering 
  # 2D movement of the fish),based on the amount of time there is left for the prey to pass the origin plane
  (fish_interception_max_dist <- Intercept_successdist )#(prey_catch[i,3]/j)*interceptionspeed)
  prey_catch[i,8]<-fish_interception_max_dist
  
# new code Aug.15, 2022
  # the way I code it that the we (or the fish) already know whether they will catch the prey or not.
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
          prey_catch[i+1,3]<- (100/preydensity)-(j*( prey_catch[i,7]/returnspeed) )
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

data.location <- which(preyspeeds==j)
captureatvelocity<-prey_catch.m[prey_catch.m$Capture <2,]

model.results[data.location,1]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,2]<- preydensity
model.results[data.location,3]<- tankdepth
model.results[data.location,4]<- j
model.results[data.location,5]<- fishsize
model.results[data.location,6]<- (j*60)/(100/preydensity) # The number of prey that would pass the 100cm area in 1 min.
model.results[data.location,7]<- model.results[data.location,1] * 0.01* model.results[data.location,5]

} #end of loop j




#################################################
### End of simulations












 
  
#summarise the results

model.results.m<-as.data.frame(model.results)
model.results.m.l<-gather(model.results.m, modeltype, capturerate,CS_Piccolo:CS_Model, factor_key=TRUE)
print(data.frame(model.results.m.l))
#print ( model.results.m.l %>%
#          filter(velocity==29 | velocity==39 | velocity== 48 | velocity==54 | velocity==61) %>%
#          group_by(velocity , modeltype) %>%
#          summarise(Csuccess=mean(capturerate)) )


Figure_velocityandcapturerate<-ggplot(model.results.m.l, aes(x=velocity,y=capturerate, color=modeltype))+
  geom_point()+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
  ggtitle("Capture rate vs velocity in different models",paste0("(CS_Model: prey density=",preydensity,", N=",numberofprey,")"))

Figure_velocityandcapturerateV<-ggplot(PiccoloV, aes(x=(Velocity)*100 ,y=Capturesuccess))+
  geom_point()+
  stat_smooth(method = "lm",color="black") + #, col = "red")+
  labs(x="Velocity", y="Capture Success(%)")+
  geom_point(data=model.results.m.l %>% filter(modeltype=="CS_Model"),aes(x=velocity,y=capturerate,color="red",size=1))+

  geom_line(data=model.results.m.l %>% filter(modeltype=="CS_Model"),aes(x=velocity,y=capturerate,color="red"))+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
  ggtitle("Capture rate vs velocity in different models",paste0("(CS_Model: prey density=",preydensity,", N=",numberofprey,")"))

Figure_velocityandcapturerateD<-ggplot(PiccoloD, aes(x=Depth*100 ,y=Capturesuccess))+
  geom_point()+
  stat_smooth(method = "lm") + #, col = "red")+
  labs(x="Depth")+
  geom_point(data=model.results.m.l %>% filter(modeltype=="CS_Model"),aes(x=Depth,y=capturerate,color=modeltype))+
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,10))+
  ggtitle("Capture rate vs Depth in different models",paste0("(CS_Model: prey density=",preydensity,", N=",numberofprey,")"))







## Capture locations
Figure_capture29<-ggplot(prey29[prey29$Capture <2,], aes(x=PreyX2, y=PreyZ2), colour=Capture)+
  geom_point()+
  scale_x_continuous(limits=c(-50,50),breaks=seq(-50,50,10))+
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10))+
  ggtitle("Capture location @ 29cm/s")+
  labs(x = "Y-coordinate", y = "Z-coordinate")

Figure_capture39<-ggplot(prey39[prey39$Capture <2,], aes(x=PreyX2, y=PreyZ2), colour=Capture)+
  geom_point()+
  scale_x_continuous(limits=c(-50,50),breaks=seq(-50,50,10))+
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10))+
  ggtitle("Capture location @ 39cm/s")+
  labs(x = "Y-coordinate", y = "Z-coordinate")

Figure_capture48<-ggplot(prey48[prey48$Capture <2,], aes(x=PreyX2, y=PreyZ2), colour=Capture)+
  geom_point()+
  scale_x_continuous(limits=c(-50,50),breaks=seq(-50,50,10))+
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10))+
  ggtitle("Capture location @ 48cm/s")+
  labs(x = "Y-coordinate", y = "Z-coordinate")

Figure_capture54<-ggplot(prey54[prey54$Capture <2,], aes(x=PreyX2, y=PreyZ2), colour=Capture)+
  geom_point()+
  scale_x_continuous(limits=c(-50,50),breaks=seq(-50,50,10))+
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10))+
  ggtitle("Capture location @ 56cm/s")+
  labs(x = "Y-coordinate", y = "Z-coordinate")

Figure_capture61<-ggplot(prey61[prey61$Capture <2,], aes(x=PreyX2, y=PreyZ2), colour=Capture)+
  geom_point()+
  scale_x_continuous(limits=c(-50,50),breaks=seq(-50,50,10))+
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,10))+
  ggtitle("Capture location @ 61cm/s")+
  labs(x = "Y-coordinate", y = "Z-coordinate")

if(dataType=="VaryVelocity"){plot(Figure_velocityandcapturerateV)}else if(dataType=="VaryDepth"){
  plot(Figure_velocityandcapturerateD)
}


#ggarrange(Figure_velocityandcapturerate2,Figure_capture29,Figure_capture39,Figure_capture48,Figure_capture54,Figure_capture61,
#          ncol=2,nrow=3,labels=c("A","B","C","D","E","F"))

} #end of function   






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



