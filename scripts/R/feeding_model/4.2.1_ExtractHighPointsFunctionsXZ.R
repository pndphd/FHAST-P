##

# 1. Find the correct range (a and b) for each of the combination
# Fish.length vs Velocity

findmaxdepthrangeX<-function(x,b){
  
  maxdepth<-x %>%
    filter(HighPointClass==b)%>%
    summarise(max.depth=max(Depth))
  maxwidth<-x %>%
    filter(HighPointClass==b)%>%
    summarise(max.width=max(nDetectX))
  
  if( x %>% filter(HighPointClass==b)%>% select(nDetectZ) %>% max()< maxdepth[,1]+0.06 )  {
    x %>%
      filter(HighPointClass==b) %>%
      filter(case_when(Depth==0.15 ~ maxwidth[,1]-nDetectX < 0.27/2.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.30 ~ maxwidth[,1]-nDetectX < 0.27/2,  # these numbers are based on Kwan's trial and error.
                       Depth==0.45 ~ maxwidth[,1]-nDetectX < 0.27/1.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.60 ~ maxwidth[,1]-nDetectX < 0.27)) %>% # these numbers are based on Kwan's trial and error.
      filter(nDetectZ > max(nDetectZ)*0.93 | nDetectX==maxwidth[,1] ) %>%
      filter(nDetectX==max(nDetectX)| nDetectZ==max(nDetectZ)) %>%
      arrange(desc(nDetectX))%>%
      slice(1:2)
    #dplyr::select(Depth,Velocity,Fish.no,Fish.length,nDetectX, nDetectZ)
  } else{
    x %>%
      filter(HighPointClass==b) %>%
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

#example
#findmaxdepthrangeX(Detect.rangeXZ.c,1)



# 2. Plot Figures to see if the ranges are correct.
range.figureX<-function(x,b){
  
  #x<-Detect.rangeXZ.c
  #b<-1
  
  fig.leng<-x %>%
    filter(HighPointClass==b)%>%
    select(Fish.length)%>%
    slice(1)%>%
    pull(1)
  
  fig.vel<-x %>%
    filter(HighPointClass==b)%>%
    select(Velocity)%>%
    slice(1)%>%
    pull(1)
  
  fig.dep<-x %>%
    filter(HighPointClass==b)%>%
    select(Depth)%>%
    unique()
    
  
  x %>%
    filter(HighPointClass==b)%>%
    ggplot(aes(x=nDetectX,y=nDetectZ))+
    geom_point()+
    scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.05),minor_breaks = seq(0,1.5,  0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.05),minor_breaks = seq(0,0.75, 0.01))+
    geom_vline(xintercept=findmaxdepthrangeX(x,b)$nDetectX[1]+0.005, color="red")+
    geom_vline(xintercept=findmaxdepthrangeX(x,b)$nDetectX[2]-0.005, color="red")+
    ggtitle(paste0("Fish.length:",paste(fig.leng),
                   " Velocity:",paste(fig.vel),
                   " Depth:",paste(fig.dep),
                   " HighPointClass:",paste(b)))
}

#example
#range.figureX(Detect.rangeXZ.c, 1)

combine.range.figureX<-function(b){
  range.figureX(Detect.rangeXZ.c, b)
}

#example
#map(seq(1,27,1),combine.range.figureX)


# 3. Extract highest points from the range acquired from the function findmaxdepthrange()


highestpointSquareX<-function(x,c,d){ # a is the possible highest (nDetectZ within the Depth) and b is the furthest point (nDetectX)
  
  
  if(is.na(c)==TRUE){x%>% filter(nDetectX==max(nDetectX))}else{
  
  data.range <- x %>%
    filter(nDetectX>= c & nDetectX<= d)%>%
    #ggplot(aes(x=nDetectX,y=nDetectZ))+
    #geom_point()+
    #scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.05),minor_breaks = seq(0,1.5,  0.01))+
    #scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.05),minor_breaks = seq(0,0.75, 0.01))+
    #geom_point(data=kwan1, aes(x=nDetectX, y=nDetectZ, color="red"))
    select(Velocity,Depth,Fish.length,FeederY,FeederY.std,FeederZ,Fish.no,nDetectX:nDetectZ,nDetectY_oneside,FocalX:FocalZ,block,Experiment,HighPointClass)%>%
    mutate(squareInOut=NA)%>%
    arrange(nDetectX,decreasing = FALSE)
  
  for (i in 1: nrow(data.range)){
   
    data.range2<-data.range%>%
      filter(data.range$nDetectX[i] < nDetectX & data.range$nDetectZ[i] < nDetectZ)
    
    if(nrow(data.range2)<1){data.range$squareInOut[i]<-"Collect.this"}else{data.range$squareInOut[i]<-"Don't.Collect"}
  }
  
  data.range %>%
  filter(squareInOut=="Collect.this")}
}

#example
#highestpointSquareX(Detect.rangeXZ.c%>%filter(HighPointClass==5),
#                   findmaxdepthrangeX(Detect.rangeXZ.c,b=5)$nDetectX[2],
#                   findmaxdepthrangeX(Detect.rangeXZ.c,b=5)$nDetectX[1])

collect.highestpointSquareX<-function(b){
  highestpointSquareX(Detect.rangeXZ.c%>%filter(HighPointClass==b),
                     findmaxdepthrangeX(Detect.rangeXZ.c,b)$nDetectX[2],
                     findmaxdepthrangeX(Detect.rangeXZ.c,b)$nDetectX[1])
                }
#map_df(seq(1,27,1),collect.highestpointSquareX)


#Detect.rangeXZ.c %>%
#  select(HighPointClass) %>%
#  summary()

### end of script


