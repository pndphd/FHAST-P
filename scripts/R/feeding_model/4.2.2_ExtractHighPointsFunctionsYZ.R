# Extract YZ

# 1. Find max depth range Y
findmaxdepthrangeY<-function(x,b){
  #x<-Detect.rangeXZ.c
  #b=1
  
  maxdepth<-x %>%
    filter(HighPointClass==b)%>%
    summarise(max.depth=max(Depth))
  maxwidth<-x %>%
    filter(HighPointClass==b)%>%
    summarise(max.width=max(nDetectY_oneside))
  
  if( x %>% filter(HighPointClass==b)%>% select(nDetectZ) %>% max()< maxdepth[,1]+0.06 )  {
    x %>%
      filter(HighPointClass==b) %>%
      filter(case_when(Depth==0.15 ~ maxwidth[,1]-nDetectY_oneside < 0.27/2.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.30 ~ maxwidth[,1]-nDetectY_oneside < 0.27/2,  # these numbers are based on Kwan's trial and error.
                       Depth==0.45 ~ maxwidth[,1]-nDetectY_oneside < 0.27/1.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.60 ~ maxwidth[,1]-nDetectY_oneside < 0.27)) %>% # these numbers are based on Kwan's trial and error.
      filter(nDetectZ > max(nDetectZ)*0.93 | nDetectY_oneside==maxwidth[,1] ) %>%
      filter(nDetectY_oneside==max(nDetectY_oneside)| nDetectZ==max(nDetectZ)) %>%
      arrange(desc(nDetectY_oneside))%>%
      slice(1:2)
    #dplyr::select(Depth,Velocity,Fish.no,Fish.length,nDetectY_oneside, nDetectZ)
  } else{
    x %>%
      filter(HighPointClass==b) %>%
      filter(case_when(Depth==0.15 ~ maxwidth[,1]-nDetectY_oneside < 0.27/2.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.30 ~ maxwidth[,1]-nDetectY_oneside < 0.27/2, # these numbers are based on Kwan's trial and error.
                       Depth==0.45 ~ maxwidth[,1]-nDetectY_oneside < 0.27/1.5, # these numbers are based on Kwan's trial and error.
                       Depth==0.60 ~ maxwidth[,1]-nDetectY_oneside < 0.27)) %>% # these numbers are based on Kwan's trial and error.
      filter(nDetectZ > maxdepth[,1] | nDetectY_oneside==maxwidth[,1] )  %>%
      filter(nDetectY_oneside==max(nDetectY_oneside)| nDetectZ==max(nDetectZ)) %>%
      arrange(desc(nDetectY_oneside))%>%
      slice(1:2)
    #dplyr::select(Depth,Velocity,Fish.no,Fish.length,nDetectY_oneside, nDetectZ)
  }
}

#example
# findmaxdepthrangeY(Detect.rangeXZ.c,1)


# 2. Plot Figures to see if the ranges are correct.
range.figureY<-function(x,b){
  
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
    ggplot(aes(x=nDetectY_oneside,y=nDetectZ))+
    geom_point()+
    scale_x_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.05),minor_breaks = seq(0,0.6,  0.01))+
    scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.05),minor_breaks = seq(0,0.75, 0.01))+
    geom_vline(xintercept=findmaxdepthrangeY(x,b)$nDetectY_oneside[1]+0.005, color="red")+
    geom_vline(xintercept=findmaxdepthrangeY(x,b)$nDetectY_oneside[2]-0.005, color="red")+
    ggtitle(paste0("Fish.length:",paste(fig.leng),
                   " Velocity:",paste(fig.vel),
                   " Depth:",paste(fig.dep),
                   " HighPointClass:",paste(b)))
}

#example
#range.figureY(Detect.rangeXZ.c, 8)

combine.range.figureY<-function(b){
  range.figureY(Detect.rangeXZ.c, b)
}

#example
#map(seq(1,27,1),combine.range.figureY)



# Problem with the line 97. Doesn't recognize to select one points...
# But is data.range a permenant dataset ?? where did I get it grom?


highestpointSquareY<-function(x,c,d){ # a is the possible highest (nDetectZ within the Depth) and b is the furthest point (nDetectX)
  
  if(is.na(c)==TRUE){x %>% filter(nDetectY_oneside==max(nDetectY_oneside))}else{
    
    data.range <- x %>%
      filter(nDetectY_oneside>= c & nDetectY_oneside<= d)%>%
      #ggplot(aes(x=nDetectX,y=nDetectZ))+
      #geom_point()+
      #scale_x_continuous(limits=c(0,1.5), breaks=seq(0,1.5,0.05),minor_breaks = seq(0,1.5,  0.01))+
      #scale_y_continuous(limits=c(0,0.75),breaks=seq(0,0.75,0.05),minor_breaks = seq(0,0.75, 0.01))+
      #geom_point(data=kwan1, aes(x=nDetectX, y=nDetectZ, color="red"))
      select(Velocity,Depth,Fish.length,FeederY,FeederY.std,FeederZ,Fish.no,nDetectX:nDetectZ,nDetectY_oneside,FocalX:FocalZ,block,Experiment,HighPointClass)%>%
      mutate(squareInOut=NA)%>%
      arrange(nDetectY_oneside,decreasing = FALSE)
    
    for (i in 1: nrow(data.range)){
      
      data.range2<-data.range%>%
        filter(data.range$nDetectY_oneside[i] < nDetectY_oneside & data.range$nDetectZ[i] < nDetectZ)
      
      if(nrow(data.range2)<1){data.range$squareInOut[i]<-"Collect.this"}else{data.range$squareInOut[i]<-"Don't.Collect"}
    }
    
    data.range %>%
      filter(squareInOut=="Collect.this")}
}

#example
#highestpointSquareY(Detect.rangeXZ.c%>%filter(HighPointClass==5),
#                   findmaxdepthrangeY(Detect.rangeXZ.c,b=5)$nDetectY_oneside[2],
#                   findmaxdepthrangeY(Detect.rangeXZ.c,b=5)$nDetectY_oneside[1])

collect.highestpointSquareY<-function(b){
  highestpointSquareY(Detect.rangeXZ.c%>%filter(HighPointClass==b),
                     findmaxdepthrangeY(Detect.rangeXZ.c,b)$nDetectY_oneside[2],
                     findmaxdepthrangeY(Detect.rangeXZ.c,b)$nDetectY_oneside[1])
}
#map_df(seq(1,27,1),collect.highestpointSquareY)


## end of script