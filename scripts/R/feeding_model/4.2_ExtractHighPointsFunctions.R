###################################################################################
######################## July 2021, Piccolo files extended ########################
###################### Extract Highest points around the contour ##########################


# 1. Findmaxdist: find the max distance based on the percentage.
# 2. findmaxdepthrange: find the optimal range to extract the points. Returns a two row of dataframe 
# 3. findrangegap: Used this to find the average gap between the furhtest to the x coord that matches the highest Z. Turns out 0.27 is good.. z
# 4. range.figure: Draw a figure for function findmaxdepthrange(). Use this to go over all the figures and validate my range.
# 5. highestpointXZ: Extract points that are in the range.
# 6. collect.highestpointXZ: Run map() on highestpointXZ using collect.highestpointXZ.



##############################################  
#### 1. Findmaxdist ####
findmaxdist<-function(x,p){ # x is dataset, p is the percentile e.g., 95,98,99 etc..
  u<-unique(x)
  sort(u,decreasing=FALSE)[length(u)*p/100]
}

##############################################  
#### 2. Find the range of max depth range #####

# 2.1  This function identifies the outer countour point just under the max water depth  

findmaxdepthrange<-function(b,g){
  
  maxdepth<-Detect.rangeXZ %>%
    filter(block==b)%>%
    summarise(max.depth=first(Depth))
  maxwidth<-Detect.rangeXZ %>%
    filter(block==b)%>%
    summarise(max.width=max(nDetectX))
  
  if( Detect.rangeXZ %>% filter(block==b)%>% select(nDetectZ) %>% max()< maxdepth[,1]+0.06 )  {
    Detect.rangeXZ %>%
      filter(block==b) %>%
      filter(case_when(Depth==0.15 ~ maxwidth[,1]-nDetectX < 0.27/2.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.30 ~ maxwidth[,1]-nDetectX < 0.27/2,  # these numbers are based on Kwan's trial and error.
                       Depth==0.45 ~ maxwidth[,1]-nDetectX < 0.27/1.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.60 ~ maxwidth[,1]-nDetectX < 0.27)) %>% # these numbers are based on Kwan's trial and error.
      filter(nDetectZ > max(nDetectZ)*g | nDetectX==maxwidth[,1] ) %>%
      filter(nDetectX==max(nDetectX)| nDetectZ==max(nDetectZ)) %>%
      arrange(desc(nDetectX))%>%
      slice(1:2)
    #dplyr::select(Depth,Velocity,Fish.no,Fish.length,nDetectX, nDetectZ)
  } else{
    Detect.rangeXZ %>%
      filter(block==b) %>%
      filter(case_when(Depth==0.15 ~ maxwidth[,1]-nDetectX < 0.27/2.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.30 ~ maxwidth[,1]-nDetectX < 0.27/2, # these numbers are based on Kwan's trial and error.
                       Depth==0.45 ~ maxwidth[,1]-nDetectX < 0.27/1.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.60 ~ maxwidth[,1]-nDetectX < 0.27)) %>% # these numbers are based on Kwan's trial and error.
      filter(nDetectZ > maxdepth[,1] | nDetectX==maxwidth[,1] )  %>%
      filter(nDetectX==max(nDetectX)| nDetectZ==max(nDetectZ)) %>%
      arrange(desc(nDetectX))%>%
      slice(1:2)
    #dplyr::select(Depth,Velocity,Fish.no,Fish.length,nDetectX, nDetectZ)
  }
}

# test an example
 findmaxdepthrange(46,0.93) # practice with block 1 with threshold 0.95-0.8-~

###############################################  
#### 3. Find the range of max depth range #####

# Support: Looking for what the max distance between the furthest and highest (but furhter out) points are
# this is to limit the search range.

findrangegap<-function(a){
  findmaxdepthrange(a,0.93)$nDetectX[1]-findmaxdepthrange(a,0.93)$nDetectX[2]
}

#############################################################  
#### 4. Draw a figure for function findmaxdepthrange() #####

range.figure<-function(b,g){
  
  fig.vel<-Detect.rangeXZ %>%
    filter(block==b)%>%
    select(Velocity)%>%
    slice(1)%>%
    pull(1)
  
  fig.dep<-Detect.rangeXZ %>%
    filter(block==b)%>%
    select(Depth)%>%
    slice(1)%>%
    pull(1)
  
  Detect.rangeXZ %>%
    filter(block==b)%>%
    ggplot(aes(x=nDetectX,y=nDetectZ))+
    geom_point()+
    scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.05),minor_breaks = seq(0,1.5,  0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.05),minor_breaks = seq(0,0.75, 0.01))+
    geom_vline(xintercept=findmaxdepthrange(b,g)$nDetectX[1]+0.005, color="red")+
    geom_vline(xintercept=findmaxdepthrange(b,g)$nDetectX[2]-0.005, color="red")+
    ggtitle(paste0("Block:",paste(b),
                   " Velocity:",paste(fig.vel),
                   " Depth:",paste(fig.dep),
                   " Threshold:",paste(g)))
}


# Support: Verify the results of the range by looking at each range per block
# Run the range.figure on multiple blocks..

# example of range.figure
# range.figure(46,0.93) # this does not work for multiple vectors..

# Run all blocks
# blocksetscombine<-seq(1,46,1)
# blocksetscombine%>%
# map(range.figure,0.93) # map function works on multiple vectors and lists.


#####################################################    
#### 5. Extract points that are in the range  ######

highestpointXZ<-function(x,a,b){ # a is the possible highest (nDetectZ within the Depth) and b is the furthest point (nDetectX)
  r=(b-a)/5
  x %>%
    mutate(rangeX=case_when(nDetectX >= a-0.005 & nDetectX<=a+r ~ 1,
                            nDetectX > a+r & nDetectX<= a+r*2 ~ 2,
                            nDetectX > a+r*2 & nDetectX<= a+r*3 ~ 3,
                            nDetectX > a+r*3 & nDetectX<= a+r*4 ~ 4,
                            nDetectX > a+r*4 & nDetectX<= a+r*5 ~ 5,
                            nDetectX < a-0.005 ~ 6))%>% 
    filter(nDetectX>=a & nDetectX<=b )%>%
    group_by(rangeX)%>%
    filter(nDetectZ==max(nDetectZ))%>%
    #dplyr::select(Velocity,Depth,Fish.length,Fish.no,nDetectX,nDetectY,nDetectY_oneside,nDetectZ,rangeX)%>%
    filter(rangeX != 6 )%>%
    arrange(desc(rangeX))
}




highestpointSquare<-function(x,a,b){ # a is the possible highest (nDetectZ within the Depth) and b is the furthest point (nDetectX)

  data.range <- x %>%
  filter(nDetectX>= a & nDetectX<= b)%>%
    select(Velocity,Depth,Experiment,Fish.no,nDetectX,nDetectY,nDetectZ,Fish.length)%>%
    mutate(squareInOut=NA)%>%
    arrange(nDetectX,decreasing = FALSE)
  
  for (i in 1: nrow(data.range)){
    data.range2<-data.range%>%
      filter(data.range$nDetectX[i] > nDetectX , data.range$nDetectZ[i] > nDetectZ)
    if(nrow(data.range2)<1){data.range$squareInOut[i]<-"Collect.this"}
    else{data.range$squareInOut[i]<-"Don't.Collect"}
  }
  
  data.range %>%
    filter(squareInOut=="Collect.this")
}

#example
highestpointSquare(Detect.rangeXZ%>%filter(Depth==0.60),0.3 ,0.7)



blocknumbers<-seq(1,46,1)
pmap_df(list(blocknumbers,rep(0.93,46)),findmaxdepthrange)




# Example of block 38
#block38<-highestpointXZ(Detect.rangeXZ %>%
#                          filter(block==38),findmaxdepthrange(38)$nDetectX[2],
#                        findmaxdepthrange(38)$nDetectX[1])

#plm1  <-lm(nDetectZ~polym(nDetectX, degree=2, raw=TRUE), data=block38)  

#block38%>%
#  ggplot(aes(x=nDetectX, y=nDetectZ))+
#  geom_point()+
#  scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
#  scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))+
#  geom_point(aes(x=nDetectX,y=fitted(plm1)),col="red")


########################################################################    
#### 6. Run map() on highestpointXZ using collect.highestpointXZ  ######

# Wrapping the highestpointXZ to efficiently run the map function..

collect.highestpointXZ<-function(bb,g){ ## bb is the block number, g is the percent acception
  highestpointXZ(Detect.rangeXZ %>% filter(block==bb),
                 findmaxdepthrange(bb,g)$nDetectX[2],
                 findmaxdepthrange(bb,g)$nDetectX[1])
}

collect.highestpointSquare<-function(bb,g){ ## bb is the block number, g is the percent acception
  highestpointSquare(Detect.rangeXZ %>% filter(block==bb),
                 findmaxdepthrange(bb,g)$nDetectX[2],
                 findmaxdepthrange(bb,g)$nDetectX[1])
}

# example
#collect.highestpointXZ(1,0.93)

# use the pmap function to run multiple variables
# use pmap_dfr to combine results to a dataframe

#blocksetscombine<-seq(1:46,1)
#highestpointXZ.df<-pmap_dfr(list(blocksetscombine,rep(0.93,46)),collect.highestpointXZ) #



#highestpointXZ.df %>%
#  ggplot(aes(x=nDetectX,y=nDetectZ, color=as.factor(Fish.length) ))+
#  geom_point()+
#  scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.1),minor_breaks = seq(0,1.5,  0.01))+
#  scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.1),minor_breaks = seq(0,0.75, 0.01))

# End of script