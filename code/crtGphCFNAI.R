  rm(list=ls())
  options(scipen = 999,digits = 4)
  require(tidyverse)
  library(parallel)
  library(timeSeries)
  library(zoo)
  library(grid)
  library(gridExtra)
  library(scales)
  #########################################################################################
  ################# peak ##################################################################
  load("~/Dropbox/Research/datingRecession/cfnai/output/peak2020.Rdata")
  df <- opt[[42]][30:52,]
  p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="Mar 2020 Vintage"),size=0.5)+
    geom_vline(xintercept=as.numeric(df$date[25]),linetype=1)+
    geom_hline(aes(yintercept=0))+
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
    theme(legend.position = c(0.15,0.3),legend.text = element_text(colour = "black",size=12))+
    labs(x="mon/year",y="CFNAI MA3")+
    theme(legend.title=element_blank(),axis.title = element_text(size=12))
  p2 <- ggplot(data=df)+geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
    geom_line(aes(x=date,y=c1,linetype="optimal stopping time"),size=0.5)+
    geom_vline(xintercept=as.numeric(df$date[25]),linetype=1)+
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
    labs(x="mon/year",y="probability")+
    theme(legend.position = c(0.15,0.3),legend.text = element_text(colour = "black",size=12))+
    theme(legend.title=element_blank(),axis.title = element_text(size=12))
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g <- rbind(g1,g2,size="first")
  g$widths <- unit.pmax(g1$widths,g2$widths)
  grid.newpage()
  grid.draw(g)
ggsave(filename="~/Dropbox/Research/datingRecession/cfnai/graph/peak2020_cfnai.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)



  
