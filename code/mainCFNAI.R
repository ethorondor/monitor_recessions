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
  setwd("/home/elrond/Documents/Dropbox/Research/datingRecession/cfnai")
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
source("/home/elrond/Documents/Dropbox/Research/datingRecession/cfnai/code/BSPT.R")
#############################################################################################
################## prepare data #############################################################
#############################################################################################
dt <- read.csv("./data/cfnai-realtime-2001.csv", header = TRUE)
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
# creating 2001 vintage for all historical recessions 
cfnai2011 <- as.data.frame(cfnai.lst[132])
colnames(cfnai2011) <- c("d","vec")
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
df_p1969 <- (cfnai2011%>%dplyr::filter(d<=as.Date("1970-06-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p1969))
bsquid_p1969 <- crtBSquid(df_p1969,oth)
#############################################################################################
df_p1973 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1970-11-01"))
                      %>%dplyr::filter(d<=as.Date("1975-03-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p1973))
bsquid_p1973 <- crtBSquid(df_p1973,oth)
#############################################################################################
df_p1980 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1975-03-01"))
                      %>%dplyr::filter(d<=as.Date("1980-07-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p1980))
bsquid_p1980 <- crtBSquid(df_p1980,oth)
#############################################################################################
df_p1982 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1980-06-01"))
                      %>%dplyr::filter(d<=as.Date("1982-01-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p1982))
bsquid_p1982 <- crtBSquid(df_p1982,oth)
#############################################################################################
df_p1990 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1982-02-01"))
                      %>%dplyr::filter(d<=as.Date("1991-01-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p1990))
bsquid_p1990 <- crtBSquid(df_p1990,oth)
#############################################################################################
df_p2001 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1992-12-01"))
                      %>%dplyr::filter(d<=as.Date("2001-12-01")))
oth <- list('param'=paramLst_peak,
            'trN'=10,
            'nSim'=nSim,
            'hist'=nrow(df_p2001))
bsquid_p2001 <- crtBSquid(df_p2001,oth)
#############################################################################################

#############################################################################################
df_t1969 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1970-03-01"))
                      %>%dplyr::filter(d<=as.Date("1974-06-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t1969))
bsquid_t1969 <- crtBSquid(df_t1969,oth)
#############################################################################################
df_t1973 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1974-11-01"))
                      %>%dplyr::filter(d<=as.Date("1977-03-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t1973))
bsquid_t1973 <- crtBSquid(df_t1973,oth)
#############################################################################################
df_t1980 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1980-04-01"))
                      %>%dplyr::filter(d<=as.Date("1982-07-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t1980))
bsquid_t1980 <- crtBSquid(df_t1980,oth)
#############################################################################################
df_t1982 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1981-10-01"))
                      %>%dplyr::filter(d<=as.Date("1983-05-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t1982))
bsquid_t1982 <- crtBSquid(df_t1982,oth)
#############################################################################################
df_t1990 <- (cfnai2011%>%dplyr::filter(d>=as.Date("1990-11-01"))
                      %>%dplyr::filter(d<=as.Date("1993-01-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t1990))
bsquid_t1990 <- crtBSquid(df_t1990,oth)
#############################################################################################
df_t2001 <- (cfnai2011%>%dplyr::filter(d>=as.Date("2001-04-01"))
                      %>%dplyr::filter(d<=as.Date("2003-12-01")))
oth <- list('param'=paramLst_trough,
            'trN'=6,
            'nSim'=nSim,
            'hist'=nrow(df_t2001))
bsquid_t2001 <- crtBSquid(df_t2001,oth)
bsquid_lst = list('p1969'=bsquid_p1969,
                  't1969'=bsquid_t1969,
                  'p1973'=bsquid_p1973,
                  't1973'=bsquid_t1973,
                  'p1980'=bsquid_p1980,
                  't1980'=bsquid_t1980,
                  'p1982'=bsquid_p1982,
                  't1982'=bsquid_t1982,
                  'p1990'=bsquid_p1990,
                  't1990'=bsquid_t1990,
                  'p2001'=bsquid_p2001,
                  't2001'=bsquid_t2001)
opt_cfnai_hist = mclapply(bsquid_lst,BSquid,mc.cores = getOption("mc.cores", 12L))
save(opt_cfnai_hist,file="./output/opt_cfnai_hist.Rdata")
#############################################################################################
#############################################################################################
################# real time vintage data ####################################################
#############################################################################################
peak2008 <- crtMntDf(cfnai.lst,27,95,'2002-05-01')
p2008 <- mntbrk(paramLst_peak,peak2008,trN=10,nSim,hist=24)
save(p2008,file="./output/peak2008.Rdata")
#############################################################################################
trough2008 <- crtMntDf(cfnai.lst,95,115,'2008-06-01')
t2008 <- mntbrk(paramLst_trough,trough2008,trN=6,nSim,hist=24)
save(t2008,file="./output/trough2008.Rdata")
#############################################################################################
#############################################################################################
#############################################################################################

