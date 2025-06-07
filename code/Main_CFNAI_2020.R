  rm(list=ls())
  options(scipen = 999,digits = 4)
  set.seed(1245)
  # ipak function: install and load multiple R packages.
  # check to see if packages are installed. Install them if they are not, then load them into the R session.
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  packages <- c("tidyverse","xts","zoo","parallel","grid","gridExtra","gtable","timeSeries","dplyr")
  ipak(packages)
    setwd("/home/elrond/Dropbox/Research/datingRecession/cfnai")
  #############################################################################################
  ################ user defined functions #####################################################
  ############################################################################  #################
  crtBSquid <- function(ts,oth){
    bsquid <- new("BSquid",
                  ts = ts,
                  nSim = oth$nSim,
                  # the prior for prob of each state (0.9,0.1)
                  pi0 = c(0.95,0.05),
                  pi12 = 0.03,
                  model=list( paramNames = c("p11","mu1","mu2","phi","sigma"),
                              param = oth$param,
                              output = c(0,1,2,3,4,5),
                              model.null.erdis = "dnorm(y-(1-model$param[[4]]$value)*model$param[[2]]$value-model$param[[4]]$value*y_1,0,model$param[[5]]$value)" ,
                              model.alt.erdis = "dnorm(y-(1-model$param[[4]]$value)*model$param[[3]]$value-model$param[[4]]$value*y_1,0,model$param[[5]]$value)",
                              model.null = "(1-model$param[[4]]$value)*model$param[[2]]$value+model$param[[4]]$value*y_1+rnorm(1000,mean=0,sd=model$param[[5]]$value)",
                              model.alt = "(1-model$param[[4]]$value)*model$param[[3]]$value+model$param[[4]]$value*y_1+rnorm(1000,mean=0,sd=model$param[[5]]$value)"
                  ),
                  cnFctr=list(0.01,0.02,0.03),
                  trainN=oth$trN,
                  hist=oth$hist
    )
    return(bsquid)
  }
  mntbrk <- function(paramLst,df,trN,nSim,hist){
    oth <- list('param'=paramLst,
                'trN'=trN,
                'nSim'=nSim,
                'hist'=hist)
    lst <- lapply(df,crtBSquid,oth)
    opt <- mclapply(lst, BSquid, mc.cores = getOption("mc.cores", 10L))
    return(opt)
  }
  crtDfLst <- function(df){
    pk <- list()
    for(i in 2:length(df)){
      d <- df[,c(1,i)]
      pk[[i-1]] <- (d%>%drop_na())
    }
    return(pk)
  }
  crtLst <- function(l,bgnDate){
    return(l%>%dplyr::filter(d>=as.Date(bgnDate)))
  }
  
  
  crtMntDf <- function(df,strvn,endvn,bgnDate){
    lst <- df[strvn:endvn]
    l <- lapply(lst,crtLst,bgnDate=bgnDate)
    return(l)
  }
  source("/home/elrond/Dropbox/Research/datingRecession/cfnai/code/BSPT.R")
  #############################################################################################
  ################## prepare data #############################################################
  #############################################################################################
  dt <- read.csv("./data/cfnai-realtime-2012-2020-03.csv", header = TRUE)
  dt.ma3 <- dt[,seq(1,ncol(dt),by=2)]
  dt.ma3[,1]<-as.Date(dt.ma3[,1],format="%m/%d/%Y")
  cfnai.lst <- crtDfLst(dt.ma3) 
  #############################################################################################
  #############################################################################################
  ######## define parameters ##################################################################
  #############################################################################################
  paramLst_peak <- list(p11=list(prior = c(0.00,1.00), value = 0.95),
                        mu1=list(prior = c(0.00,0.50), value = 0.00),
                        mu2=list(prior = c(-2.00,-0.80), value = 0.00),
                        phi=list(prior = c(0.10,0.40), value = 0.00),
                        sigma=list(prior = c(0.50,0.80), value = 0.00)
  )
  
  paramLst_trough <- list(p11=list(prior = c(0.00,1.00), value = 0.95),
                          mu1=list(prior = c(-2.00,-1.00), value = 0.00),
                          mu2=list(prior = c(-0.40,0.40), value = 0.00),
                          phi=list(prior = c(0.60,0.90), value = 0.00),
                          sigma=list(prior = c(0.20,0.50), value = 0.00)
  )
  nSim = 10000
  #############################################################################################
  ################# real time vintage data ####################################################
  #############################################################################################
  lst <- crtMntDf(cfnai.lst,59,100,'2016-01-01')
  opt <- mntbrk(paramLst_peak,lst,trN=10,nSim,hist=24)
  save(opt,file="./output/peak2020.Rdata")
i <- 0
i <- i+1;View(drop_na(opt[[i]]))
