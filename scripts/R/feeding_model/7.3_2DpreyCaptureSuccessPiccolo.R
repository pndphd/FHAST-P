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

Capturesuccess_Piccolo2007(numberofprey = 5, fishsize=0.053, 
                           tankwidth=100,tankdepth=30, # tankwidth=60 or tankdepth=30
                           dataType="VaryVelocity") # or VaryDepth
                          # When VaryDepth, you can have multiple tankdepth but when VaryVelocity, you should fix to one.

Capturesuccess_Piccolo2007<- function(numberofprey,fishsize,tankwidth,tankdepth,dataType){
 
numberofprey<-10000
fishsize<-0.07
tankwidth=100
tankdepth=30 # for "VaryVelocity" experiment
dataType="VaryVelocity" # or VaryDepth
preyspeeds<-c(29,39,48,54,61) # cm
sensitivity<-c(-5,-3,0,3,5)

model.results<-matrix(0,length(preyspeeds),7)
colnames(model.results)<-c("velocity","CS_Piccolo","CS_Model_minus5cm","CS_Model_minus3cm","CS_Model",
                           "CS_Model_plus3cm","CS_Model_plus5cm")



##### VaryVelocity loop ####

for (j in preyspeeds){
  
  for (snt in sensitivity){ 
    
prey_catch<-matrix(0,numberofprey+1,14)
colnames(prey_catch)<-c("PreyX2","PreyZ2","PreyY2","FishX1","FishZ1","FishY1", "Dist_to prey_lateral","Intercept_maxdist","Capture","Dist_preytoorigin","Return_maxdist","newfishX3","newfishZ3","newfishY3")

#Sampling prey randomly
prey_catch[,1]<-runif(numberofprey+1, min=-(tankwidth/2)+0.02,max=(tankwidth/2)-0.02); 
prey_catch[,2]<-runif(numberofprey+1, min=1.2,max=tankdepth-0.02); 

# Estimate max detection distance (formula from 4.5_Detectionrange_parameter.R)
# Note: the x and y axes are opposite in this script.

#sheadV<-c(29,39,48,54,61)
#sheadY<-c(50,45,38,40,30)
#sheadY.df<-data.frame(sheadV,sheadY)
#summary(lm(sheadY~sheadV, data=sheadY.df))

#sheadV<-c(29,39,48,54,61)
#sheadX<-c(42,38,29,24,23)
#sheadX.df<-data.frame(sheadV,sheadX)
#summary(lm(sheadX~sheadV, data=sheadX.df))

(prey.lateral.dist.y.b<- 66.9372+(-0.5701*j)+snt)
   
(detect.width.x.a<- 61.68263+(-0.65980*j) +snt)

(detect.height.z.c<- 
    100*(0.9202+(0.7394*((j*0.01)^2))+(-0.7709*(j*0.01))+(-34.5407*fishsize^2)+(2.9616*fishsize)) ) # y value # 41.15282 -0.25872*velocities )# same as the x width  #23.8  # mean of detect.heights
  +snt

#fish first launch location
prey_catch[1,4]<- 0
prey_catch[,5]<-2
prey_catch[1,6]<- 0
fish_originx<-prey_catch[1,4]
fish_originz<-prey_catch[1,5]
fish_originy<-prey_catch[1,6]

(interceptionspeed <- (-0.11*(j/100)+0.45)*100) # Piccolo paper reference. converted to cm
(returnspeed <- (1.39*(j/100)-0.13)*100)# Piccolo paper reference

for (i in 1:numberofprey) {
  
  prey_catch[i,3]<-prey.lateral.dist.y.b * sqrt(1-((prey_catch[i,1]-prey_catch[i,4])^2/detect.width.x.a^2)- ((prey_catch[i,2]-prey_catch[i,5])^2/detect.height.z.c^2))
  prey_catch[i,3][is.na(prey_catch[i,3])]<-0
  

  if( prey_catch[i,3] > 0 & prey_catch[i,3] < 75 ){ 
    prey_catch[i,3] <- prey_catch[i,3]
  } else if(prey_catch[i,3]==0){
    prey_catch[i,3] <- 0
  } else { prey_catch[i,3] <- 75 }
  
  
  # Dist_to_ prey_lateral; 2D distance from fish to the potential prey (in 2D) using Euclidean distance formula
  prey_catch[i,7]<-sqrt((prey_catch[i,1]-prey_catch[i,4])^2+(prey_catch[i,2]-prey_catch[i,5])^2)
  prey_catch
  
  #Intercept max 2D lateral distance; The amount of distance the fish can attack in 2D lateral (since we are only considering 
  # 2D movement of the fish),based on the amount of time there is left for the prey to pass the origin plane
  (fish_interception_max_dist <- (prey_catch[i,3]/j)*interceptionspeed)
  prey_catch[i,8]<-fish_interception_max_dist
  

  #Capture success determination (1: capture, 2: missed)
  if (prey_catch[i,7] <= prey_catch[i,8]) { # when fish can catch the prey
    prey_catch[i,9] <- 1;} else { # when fish can't catch the prey.. move towards the prey and then return towards the origin
      prey_catch[i,9] <- 2;
        }# end of if statement (capture determination)
    } # end of individual prey number iterations 
  
prey_catch.m<-as.data.frame(prey_catch)[1:nrow(prey_catch)-1,] # remove bottom row

#if (j==29){
#  prey29<-prey_catch.m
#} else if (j==39){
#  prey39<-prey_catch.m
#} else if(j==48){
#  prey48<-prey_catch.m
#} else if(j==54){
#  prey54<-prey_catch.m
#} else if(j==61){
#  prey61<-prey_catch.m
#} else{prey30<-prey_catch.m}


data.location <- which(preyspeeds==j)
col.location <- which(sensitivity==snt)+2
captureatvelocity<-prey_catch.m[prey_catch.m$Capture <2,]

model.results[data.location,1]<-j
model.results[data.location,2]<- (-151.9*(j/100) +104.8) # Formula from Piccolo et al. 2008
model.results[data.location,col.location]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,col.location]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,col.location]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,col.location]<- (nrow(captureatvelocity)/numberofprey)*100
model.results[data.location,col.location]<- (nrow(captureatvelocity)/numberofprey)*100

} # end of loop snt

} #end of loop j

Aug.23, 2022
Thing to do:
  Try to refit the Piccolo et al. 2007 Fig2 with polynomial.And then compare the results..
So simply refit it with a polynomial and then add additional line (poly fitted) to the figures below..


Piccolo2007_Fig2<-read.csv("C:/Users/harbi/Documents/UC Santa Cruz project/UCSC_Research/Small_experiment/Piccolo2007_Fig2.csv",header=FALSE)
Piccolo2007_Fig2.1<-data.frame(Piccolo2007_Fig2,"ModelType"<-"CS_Piccolo")
colnames(Piccolo2007_Fig2.1)<-c("Velocity","CaptureSuccess","ModelType")
# Plot results
#library(reshape2)
#library(tidyr)
model.results.df<-data.frame(model.results)
library(reshape2)
model.results.df.long<-model.results.df%>%melt(id.vars="velocity")
  colnames(model.results.df.long)<-c("Velocity","ModelType","CaptureSuccess")

  str(model.results.df.long)
  
  model.results.df.long %>%
    filter( ModelType != "CS_Model_minus5cm" & ModelType != "CS_Model_plus5cm") %>%
    ggplot(aes(x=Velocity, y=CaptureSuccess, color=ModelType))+
    geom_point()+
    geom_line()+
    geom_point(data=Piccolo2007_Fig2.1, aes(x=Velocity*100, y=CaptureSuccess))+
    ylim(0,100)+
    scale_color_manual(labels = c("Experiment_2007", "Simulation_3cm shorter","Simulation","Simulation_3cm longer"), values=c("steelblue","red","#859900","purple") )+
    xlab("Water Velocity (cm/s)")+
    ylab("Capture Success")
  
  model.results.df.long %>%
    filter( ModelType=="CS_Piccolo" | ModelType == "CS_Model") %>%
    ggplot(aes(x=Velocity, y=CaptureSuccess, color=ModelType))+
    geom_point()+
    geom_line()+
    geom_point(data=Piccolo2007_Fig2.1, aes(x=Velocity*100, y=CaptureSuccess))+
    ylim(0,100)+
    scale_color_manual(labels = c("Experiment_2007", "Simulation"), values = c("steelblue", "red"))+
    xlab("Water Velocity (cm/s)")+
    ylab("Capture Success")
    
    
  str(Piccolo2007_Fig2)
  
  # The conclusion: By simply estimating the amount of time to react to the prey
  # (by using the detection distance) it showed such a similar result from the paper.. 
  # In addition, our model gave ample amount of time whereas, Piccolo varied the time so that
  # the fish would not get habituated.. considering this variance, we believe that 
  # our simple model did a great job of estimating the capture success.
  
  # This sparked us to explore more using this method..
  # We thought this could be a good setting to test how prey densities coupled with velocity could
  # affect the capture success. 
  # It seemed like the captuer success was quite sensitive to the detection distance.
  # Therefore, it seemed like the detection distance was very important..
  # THerefore,,, we decided to estimate them more accurately across different sizes of fish and velocities
  # using additional data from Piccolo.
    
    

  
############# End script #######  
library(ggplot2)



#################################################







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



  
  
################################################
### I may remove it later ###
#Plotting feeders from Piccolo given files



FeederX<-c(0.107,0.187,0.258,0.36,0.442,0.552,0.619,0.703,0.798,0.885,0.112,0.193,0.29,0.375,0.443,0.553,0.637,0.722,0.815,0.905)
FeederY<-c(-0.161, -0.165,-0.167, -0.164, -0.155, -0.157, -0.158, -0.157, -0.163, -0.157, -0.08, -0.07, -0.062, -0.058, -0.06,-0.053, -0.052, -0.054, -0.048, -0.045)
plot(x=FeederX,y=FeederY,ylim=c(-0.3,0))


# use Steelhead 1; Velocity 5
normalized_to_median_pccoord_SH1_vel5<-rbind(c(0.321528,0.567012,-0.0650155),c(0.463401,0.569878,-0.187539),c(0.453219,0.52389,-0.162217),
                                             c(0.296826,0.473341,-0.0656586),c(0.47121,0.619534,-0.1567),
                                             c(0.732207,0.426162,-0.197194),c(-0.0693324,0.705165,-0.0190352),c(0.473953,0.478527,-0.190999),
                                             c(0.101399,0.601077,-0.0269493),c(0.538066,0.481328,-0.186443),c(0.381307,0.541631,-0.0119063),
                                             c(0.229991,0.435089,-0.0792844),c(0.324061,0.36528,-0.179993),c(0.37712,0.478382,-0.0545803),
                                             c(0.524096,0.552967,-0.174706),c(0.495695,0.637848,-0.10762),c(0.518247,0.598354,-0.16671),
                                             c(0.480574,0.548166,-0.116095),c(0.371393,0.627294,-0.0127517),c(0.491949,0.559829,-0.105232),
                                             c(0.380206,0.524519,-0.0637717),c(0.0942347,0.36611,-0.0198775),c(-0.0781239,0.384106,-0.0524274),
                                             c(0.396569,0.440919,-0.109192) )

normalizedpdcoordlist<-rbind(c(0.746195,0.567012,-0.0650155),c(0.659401,0.569878,-0.187539),
                             c(0.828885,0.52389,-0.162217),c(0.623492,0.473341,-0.0656586),
                             c(0.699876,0.619534,-0.1567),c(1.04254,0.426162,-0.197194),
                             c(0.763668,0.705165,-0.0190352),c(0.65362,0.478527,-0.190999),
                             c(0.575066,0.601077,-0.0269493),c(0.799399,0.481328,-0.186443),
                             c(0.78964,0.541631,-0.0119063),c(0.638325,0.435089,-0.0792844),
                             c(0.618061,0.36528,-0.179993),c(0.703787,0.478382,-0.0545803),
                             c(0.785429,0.552967,-0.174706),c(0.871361,0.637848,-0.10762),
                             c(0.77958,0.598354,-0.16671),c(0.790908,0.548166,-0.116095),
                             c(0.828727,0.627294,-0.0127517),c(0.867616,0.559829,-0.105232),
                             c(0.690539,0.524519,-0.0637717),c(0.535235,0.36611,-0.0198775),
                             c(0.379209,0.384106,-0.0524274),c(0.657902,0.440919,-0.109192))



normalized_pccoord_SH1_vel5

medianfpcoord<-c(0.573375,0.516244,-0.256333)
meanfpcoord<-c(0.479852,0.50144,-0.255342) # actually a meanfocalpointcoord
# medianfpcoord<-c(10,10,10)
# pccoord<-rbind(c(4,55,16,0.55),c(9,121,6,0.55),c(11,158,5,0.44),c(14,210,4,0.36),c(16,233,7,0.62),c(18,255,4,0.36),
#               c(30,440,16,0.55),c(31,453,4,0.36),c(37,541,15,0.44),c(42,622,3,0.26),c(46,685,16,0.55),c(53,792,5,0.44),
#               c(56,839,3,0.26),c(62,929,5,0.44),c(64,960,6,0.55),c(68,1008,5,0.44),c(71,1062,7,0.62),c(73,1095,6,0.55),
#               c(74,1099,16,0.55),c(85,1271,7,0.62),c(86,1286,5,0.44),c(88,1316,14,0.38),c(91,1360,14,0.38),c(92,1379,4,0.36) )

pccoord1<-rbind( c(63.4667,0.433565,0.559157,-0.0568074,1),c(130.767,0.494296,0.579275,-0.177861,2),
                 c(162.133,0.474003,0.531566,-0.160926,3),c(224.167,0.338958,0.454593,-0.0539222,4),
                 c(239.167,0.488487,0.629009,-0.143366,5),c(260.2,0.756299,0.445081,-0.207121,6),
                 c(450.467,-0.0808193,0.722124,-0.0193873,7),c(457.967,0.236377,0.425787,-0.193734,8),
                 c(547.333,-0.135121,0.568728,-0.0265971,9),c(628.167,0.326409,0.448792,-0.192402,10),
                 c(694.167,-0.13714,0.554183,-0.0114629,11),c(797.867,0.230515,0.445961,-0.0873985,12),
                 c(845.1,0.071405,0.28923,-0.189534,13),c(934.6,0.12504,0.432086,-0.0625708,14),
                 c(968.6,0.340633,0.540387,-0.176241,15),c(1014.37,0.502452,0.486478,-0.102657,16),
                 c(1067.83,0.528841,0.610991,-0.169891,17),c(1101.13,0.489657,0.555862,-0.105196,18),
                 c(1107.73,0.409743,0.62881,0.000512343,19),c(1278.37,0.227193,0.566083,-0.105763,20),
                 c(1291.5,0.169148,0.494832,-0.0750165,21),c(1321.67,0.0937109,0.388178,-0.0147188,22),
                 c(1366.43,-0.0172281,0.38259,-0.0445202,23),c(1384.9,0.158805,0.411324,-0.111541,24) )


pccoord<-pccoord1[,c(2:4)]


fpcoordinate<-medianfpcoord+pccoord-normalized_pccoord_SH1_vel5
# normalize to 0,0,0
normalizedtozeropccoord<-c(0,0,0)+pccoord-fpcoordinate
normalizedtozeropccoord1<-medianfpcoord+pccoord-fpcoordinate

plot(normalizedtozeropccoord[,1],normalizedtozeropccoord[,3])

plot(pccoord[,1],pccoord[,3])

steelhead1v5_normalized<-rbind( c(0.20748610229644748, 0.050768049843042395, 0.19131716390405643, -0.2518472310368861, 0.050768049843042395, 0.19131716390405643),
                                c(0.10202538752352819, 0.053633698176911326, 0.06879408859571923, -0.10997461247647478, 0.053633698176911326, 0.06879408859571923),
                                c(0.2861768527040677, 0.007645829873793586, 0.09411519595393122, -0.12015648062925766, 0.007645829873793586, 0.09411519595393122),
                                c(0.07678377611831555, -0.04290299908285128, 0.19067404006008284, -0.2765495572150128, -0.04290299908285128, 0.19067404006008284),
                                c(0.14516790020297443, 0.10329000307515446, 0.09963308493894169, -0.10216543313035992, 0.10329000307515446, 0.09963308493894169),
                                c(0.49449818573771853, -0.09008247303340666, 0.059139100463881766, 0.15883151907105575, -0.09008247303340666, 0.059139100463881766),
                                c(0.2582923590883469, 0.1889208388055471, 0.23729747245080507, -0.6427076409116471, 0.1889208388055471, 0.23729747245080507),
                                c(0.09491137527940896, -0.03771664062724889, 0.06533410976312376, -0.09942195805389831, -0.03771664062724889, 0.06533410976312376),
                                c(0.04035737688652258, 0.08483294231421229, 0.22938338419013937,-0.47197595644682677, 0.08483294231421229, 0.22938338419013937),
                                c(0.24735716110827072, -0.03491622137871553, 0.06988985994978353, -0.03530950555837992, -0.03491622137871553, 0.06988985994978353),
                                c(0.24959845538748773, 0.025387380966294337, 0.24442638335844347, -0.19206821127913876, 0.025387380966294337, 0.24442638335844347),
                                c(0.09828294343449695, -0.0811547547599274, 0.17704823423385546,-0.34338372323218974, -0.0811547547599274, 0.17704823423385546),
                                c(0.06868587668515641, -0.150963794474328, 0.07634013448207982,-0.24931412331485564, -0.150963794474328, 0.07634013448207982),
                                c(0.15707814159920724, -0.037861691668797604, 0.20175238441165194,-0.19625519173416628, -0.037861691668797604, 0.20175238441165194),
                                c(0.23338753347199748, 0.036723054338697825, 0.08162637809241746,-0.04927913319465316, 0.036723054338697825, 0.08162637809241746),
                                c(0.32865288573579965, 0.12160397942975643, 0.14871281768696346,-0.07768044759752568, 0.12160397942975643, 0.14871281768696346),
                                c(0.22753854719252908, 0.08210975278287425, 0.089623108023688,-0.05512811947412155, 0.08210975278287425, 0.089623108023688),
                                c(0.2428659383720212, 0.0319219523037545, 0.14023753783517767,-0.09280072829470176, 0.0319219523037545, 0.14023753783517767),
                                c(0.2926849336958195, 0.11104992949804682, 0.24358089939097663,-0.20198173297087935, 0.11104992949804682, 0.24358089939097663),
                                c(0.32490697884716946, 0.04358528893374858, 0.15110014370822936,-0.08142635448615582, 0.04358528893374858, 0.15110014370822936),
                                c(0.14249711803661086, 0.00827480200080366, 0.1925609089277761,-0.19316954863011207, 0.00827480200080366, 0.1925609089277761),
                                c(-0.002140523771253733, -0.15013400450441894, 0.23645515328104325,-0.47914052377130195, -0.15013400450441894, 0.23645515328104325),
                                c(-0.15683246476639, -0.1321384162866679, 0.20390525234975967,-0.6514991314330889, -0.1321384162866679, 0.20390525234975967),
                                c(0.10586014447319425, -0.07532470805312275, 0.14714113517895833,-0.17680652219357684, -0.07532470805312275, 0.14714113517895833))


colnames(steelhead1v5_normalized)<-c("pdcoordx","pdcoordy","pdcoordz","pccoordx","pccoordy","pccoordz")                



normalized_to_median_pccoord_SH1_vel5
medianfpcoord
pccoord



par(mfrow=c(2,1))
plot(steelhead1v5_normalized[,4],steelhead1v5_normalized[,6]) # x and z axis

###########


medianfpcoord<-rbind( c(0.223379,0.498589,-0.226412),c(0.223379,0.498589,-0.226412),c(0.223379,0.498589,-0.226412) )
medianfpcoord<-c(0.223379,0.498589,-0.226412)

zero_fpcoord<-rbind( c(0,0.51,-0.3), c(0,0.51,-0.3),c(0,0.51,-0.3))
zero_fpcoord<-rbind( c(0.76,0.51,-0.3), c(0.76,0.51,-0.3),c(0.76,0.51,-0.3) )

pccoord<-rbind( c(0.0131152,0.422758,-0.102854),c(0.247181,0.284968,-0.0935606),c(0.362025,0.301231,-0.126997) )
normalizedpccoordlist<-rbind( c(0.190792,0.315661,-0.172198), c(0.32375,0.250491,-0.0784611), c(0.311089,0.50732,-0.182282))
fpcoordinate<-medianfpcoord+pccoord-normalizedpccoordlist
normalized_zero_pccoordlist<- zero_fpcoord + pccoord+fpcoordinate


coho3v2_normalised_zero<-rbind(c(-0.0325865289949385, -0.18292802188617896, 0.05421432526991142), c(0.10037143418791772, -0.24809812933899345, 0.14795071697250656),
                               c(0.08770966214791637, 0.008730350342616766, 0.04412944839566335) )

# Although two types of data are plotted in different coordinates... if they are true.. they should still share the identical pattern within the data

sqrt(sum((coho3v2_normalised_zero[1,]-normalizedpccoordlist[1,])^2))
sqrt(sum((coho3v2_normalised_zero[2,]-normalizedpccoordlist[2,])^2))
sqrt(sum((coho3v2_normalised_zero[3,]-normalizedpccoordlist[3,])^2))

sqrt(sum((pccoord[1,]-coho3v2_normalised_zero[1,])^2))
sqrt(sum((pccoord[2,]-coho3v2_normalised_zero[2,])^2))
sqrt(sum((pccoord[3,]-coho3v2_normalised_zero[3,])^2))


plot(coho3v2_normalised_zero[,1], coho3v2_normalised_zero[,3])

## Where the heck is the (0,0,0) in the Mathematica codes????
# (1.1; 0.05)
mean(c(0.33,0.34,0.37)) #0.3466667
# (1.1; 0.95)
mean(c(0.31, 0.27, 0.26)) #0.28
# (0.1; 0.05)
mean(c(0.25, 0.31, 0.32)) # 0.2933333
# (0.1; 0.95)
mean(c(0.29,0.28, 0.28)) #0.2833333

1.56-0.8




############# Below is some real codes...


#calculate the average direction of the prey 
library(dplyr)
apply(select(prey_catch.m[prey_catch.m$Capture <2,],c(1,2)),2,mean)


#capture success considering the Hill and Grossman 1993. Success rate differs by the distance of prey from the fish

# 1. Convert 75mm fork length to standard length 
# (REF: Konstantin Karpov and Gerald S. Kweiecien 1988 Marine Resources Administrative report No.88-9 )

# Title: Conversions between total, fork, and standard lengths for 41 species in 15 families of fish from California Using Preserved and fresh specimens
# 1. Standard length as a function of fork length
# Salmonidae
# Salmo clarkii--> changed name to: Oncorhynchus clarkii (Cutthroat trout)
-10.170+0.930*75
# S. gairdnerii--> changed name to: Oncorhynchus mykiss (Rainbow trout)
-0.424+0.893*75; # 66.551mm

rainbowtrout_size<-66 # mm STL based on the reference above
rainbowtrout_size*0.5 # up to 33mm
rainbowtrout_size*1.5 # up to 99mm
rainbowtrout_size*2.5 # up to 165mm


# 2. Capture success percentage based on the Hill and Grossman 
# at 30cm/s water velocity and 15 degree celcius water
#  97 % <- 0-0.5 STL
#  68 % <- 0.5-1.5 STL
#  20 % <- 1.5-2.5 STL

nrow(prey_catch.m[prey_catch.m$Capture < 2 & prey_catch.m$`Dist_to prey` <= 3.3,]) * 0.97
nrow(prey_catch.m[prey_catch.m$Capture < 2 & prey_catch.m$`Dist_to prey` > 3.3 & prey_catch.m$`Dist_to prey` <= 9.9,]) * 0.68
nrow(prey_catch.m[prey_catch.m$Capture < 2 & prey_catch.m$`Dist_to prey` > 9.9 & prey_catch.m$`Dist_to prey` <= 16.5 ,]) * 0.2

prey_catch.m[prey_catch.m$Capture < 2 & prey_catch.m$`Dist_to prey` > 16.5,]






##############################################################################
##### Below is Dudley's research
##### Compare shelter to food limits #####

resolution = 200
number = 9
coverMin = 0.01
coverMax = 0.5
countMin = 10
countMax = 1000
velocityMin = 1 # cm/s
velocityMax = 100 # cm/s
cellArea = 2000^2 #cm^2
depth = 200 # cm
fishLength = 5 # cm
temperature = 12 # C


(200^2)*9
str(data)
data = setNames(data.frame(matrix(0,nrow = resolution^2*number)),"cover") %>%
  mutate(cover = rep(seq(coverMin, coverMax, length.out = resolution), each = resolution*number)) %>%
  mutate(velocity = rep(seq(velocityMin, velocityMax, length.out = resolution), resolution*number)) %>%
  mutate(count = rep(rep(seq(countMin, countMax, length.out = number), each = resolution, resolution))) %>%
  mutate(maxSwimSpeed = (fishMaxSwimParamA*fishLength + fishMaxSwimParamB)*
           (fishMaxSwimParamC*temperature^2 + fishMaxSwimParamD*temperature + fishMaxSwimParamE)) %>%
  mutate(logistD = log(0.9/0.1)) %>%
  mutate(logistC = log(0.1/0.9)) %>%
  mutate(logistA = (logistC - logistD)/(fishCaptureParam1 - fishCaptureParam9)) %>%
  mutate(logistB = logistC - (logistA*fishCaptureParam1)) %>%
  mutate(z = logistA + logistB*velocity/maxSwimSpeed) %>%
  mutate(captureSuccess = exp(z)/(1+exp(z))) %>%
  mutate(detectionDistance = fishDetectDistParamA + fishDetectDistParamB*fishLength) %>%
  mutate(captureArea = 2 * detectionDistance * min(depth, detectionDistance)) %>%
  mutate(driftIntake = captureSuccess * habDriftConc * velocity * captureArea) %>%
  mutate(driftTotal = cellArea*depth*velocity*habDriftConc/habDriftRegenDist) %>%
  mutate(fractionEaten = pmin(1,driftIntake/driftTotal)) %>%
  mutate(fractionOccupied = pmin(1,fishLength^2/(cellArea*cover))) %>%
  mutate(eatOverOcc = fractionEaten-fractionOccupied)



# Plot the 2D Graph
j=ggplot(data, aes(x=cover, y=velocity, z = eatOverOcc, fill = eatOverOcc)) +
  theme_bw(base_size = 15)+
  facet_wrap( ~ count, ncol=sqrt(number)) +
  geom_raster(interpolate = T)+
  scale_y_continuous(name = "Velocity (cm/s)")+
  scale_x_continuous(name = "Cover")+
  scale_fill_viridis(name = "Eaten - Occ.")
windows()
print(j)

# ##### Experiment on reducing velocity when spawning #####
# 
# fishLenght = 50
# reductionFacctor = 0.03
# resolution = 100
# 
# getSlope = function(reduceVelocity, reduceTime)
# {
#   #reduceVelocity = 0.5
#   #reduceTime = 1
#   number = 100
#   slope = -18.9
#   intercept = 336
#   data = setNames(data.frame(matrix(0,nrow = number)),"temp") %>% 
#     mutate(temp = seq(9, 16, length.out = number),
#            waterVelocity = temp*slope + intercept,
#            experiencedVelocity = waterVelocity * reduceVelocity,
#            weight = fishWeightParamA*fishLength^fishWeightParamB,
#            restMet = fishRespParamA*weight^fishRespParamB*exp(fishRespParamC*temp),
#            activeMet = fishRespParamA*weight^fishRespParamB*exp(fishRespParamC*temp)*exp(fishRespParamD*experiencedVelocity),
#            met = restMet*(1 - reduceTime) + (reduceTime)*activeMet)
#   
#   # Get a linear model 
#   model = lm(met ~ temp, data = data)
#   slope = summary(model)$coefficients[2]
#   return(slope)
# }

# makes the 2D histogram of the slope values
# data = setNames(data.frame(matrix(0,nrow = resolution^2)),"reduceVelocity") %>% 
#   mutate(reduceVelocity = rep(seq(0, 0.5, length.out = resolution), each = resolution),
#          reduceTime = rep(seq(0, 0.5, length.out = resolution), resolution),
#          slope = map2_dbl(reduceVelocity, reduceTime, ~getSlope(.x,.y)))

startT = 9
endT = 16
slope = -24.65
intercept = 385
startV = startT*slope + intercept
endV = endT*slope + intercept
lineRes = 1000

lineData = setNames(data.frame(matrix(0,nrow = lineRes)),"reduceVelocity") %>% 
  mutate(reduceVelocity = seq(0.001, 0.5, length.out = lineRes),
         reduceTime = (1-exp(fishRespParamC*(endT - startT)))/
           (exp(fishRespParamC*(endT - startT))*(exp(fishRespParamD*endV*reduceVelocity)-1)-
              (exp(fishRespParamD*startV*reduceVelocity)-1)))


g = ggplot(lineData, aes(x = reduceVelocity, y = reduceTime)) +
  theme_classic(base_size = 25) +
  labs(x = "Velocity Fraction", y = "Time Fraction") +
  scale_x_continuous(expand = c(0,0), limits = c(0,0.5)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.5)) +
  geom_line(size = 1) 
windows()
print(g)  

#ggsave("C:/Users/peter.dudley/Documents/Research/Projects/Redd RSF/Papers/Redd RSF/Figures/Trade Off.tiff", plot = g, dpi = 600)
