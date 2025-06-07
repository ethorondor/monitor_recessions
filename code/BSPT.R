# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
#packages <- c("tidyverse", "astsa", "xts", "zoo", "forecast")
packages <- c("tidyverse", "xts", "zoo")
ipak(packages)

setClass(
  "BSquid",
  slots = c(
    ts = "list",
    nSim = "numeric",
    pi0 = "numeric",
    pi12 = "numeric",
    model = "list" ,
    cnFctr = "list",
    trainN = 'numeric',
    hist='numeric'
  )
)

setGeneric(name = "BSquid",
           def = function(theObject)
           {
             standardGeneric("BSquid")
           })
setMethod("BSquid",
          signature = "BSquid",
          definition = function(theObject)
          {
            Y = theObject@ts
            pi12 = theObject@pi12
            nsim = theObject@nSim
            pi0  = theObject@pi0
            model = theObject@model
            cnFctr = theObject@cnFctr
            trN = theObject@trainN
            hist=theObject@hist
            opt = list(0)
            vtg = list(0)
            seqY = Y[[2]]
            OT.output <- matrix(data=NA,nrow = length(seqY),ncol=10)
            for (tmC in  max(trN,length(seqY)-hist):length(seqY)){
              y_1  = seqY[1]
              vtgY = seqY[1:tmC]
              estModel <- paramEst(vtgY, pi12, y_1, nsim, pi0, model)
              OT.output[tmC,]=c(tmC,
                                estModel$output,
                                BSquid.OT(estModel,y_1,c=cnFctr[[1]]),
                                BSquid.OT(estModel,y_1,c=cnFctr[[2]]),
                                BSquid.OT(estModel,y_1,c=cnFctr[[3]]))
            }
            OT_output <- as.data.frame(OT.output)
            OT_tmp <- OT_output
            OT_output$date <- Y[[1]][1:length(seqY)]
            OT_output$vec <- Y[[2]][1:length(seqY)]
            names(OT_output) <- c("count",model$paramNames,"pi","c1","c2","c3","date","vec")
            return(OT_output)
          }
)

setGeneric(name = "BSquid.ML",
           def = function(theObject)
           {
             standardGeneric("BSquid.ML")
           })
setMethod("BSquid.ML",
          signature = "BSquid",
          definition = function(theObject)
          {
            Y = theObject@ts
            pi12 = theObject@pi12
            nsim = theObject@nSim
            pi0  = theObject@pi0
            model = theObject@model
            cnFctr = theObject@cnFctr
            trN = theObject@trainN
            opt = list(0)
            vtg = list(0)
            seqY = Y[[2]]
            OT.output <- matrix(data=NA,nrow = length(seqY),ncol=10)
            for (tmC in  max(trN,length(seqY)-24):length(seqY)){
              y_1  = seqY[1]
              vtgY = seqY[1:tmC]
              #if (!is.null(seed <- getOption("myseed")+tmC))
              if (!is.null(seed <- tmC))
                estModel <- paramEst.ML(vtgY, pi12, y_1, nsim, pi0, model)
              OT.output[tmC,]=c(tmC,
                                estModel$output,
                                BSquid.OT(estModel,y_1,c=cnFctr[[1]]),
                                BSquid.OT(estModel,y_1,c=cnFctr[[2]]),
                                BSquid.OT(estModel,y_1,c=cnFctr[[3]]))
            }
            OT_output <- as.data.frame(OT.output)
            OT_tmp <- OT_output
            OT_output$date <- Y[[1]][1:length(seqY)]
            OT_output$vec <- Y[[2]][1:length(seqY)]
            names(OT_output) <- c("count",model$paramNames,"pi","c1","c2","c3","date","vec")
            return(OT_output)
          }
)

setGeneric(name = "BSquid.main.simu",
           def = function(theObject)
           {
             standardGeneric("BSquid.main.simu")
           })

setMethod("BSquid.main.simu",
          signature = "BSquid",
          definition = function(theObject)
          {
            Y = theObject@ts
            pi12 = theObject@pi12
            y_1  = theObject@ts[1]
            nsim = theObject@nSim
            pi0  = theObject@pi0
            model = theObject@model
            OT.output <- matrix(data=NA,nrow = length(Y),ncol=10)
            for(temCount in 30:length(Y)){
              seqY = Y[1:temCount]
              estModel <- paramEst(seqY, pi12, y_1, nsim, pi0, model)
              OT.output[temCount,]=c(temCount,
                                     estModel$output,
                                     BSquid.OT(estModel,y_1,c=0.02),
                                     BSquid.OT(estModel,y_1,c=0.04),
                                     BSquid.OT(estModel,y_1,c=0.07))
            }
            OT_output <- as.data.frame(OT.output)
            names(OT_output) <- c("count",model$paramNames,"pi","c02","c04","c07")
            return(OT_output)
          }
)


ranGenParam <- function(pLst){
  for(i in 1:length(pLst)){
    pLst[[i]]$value = round(runif(1,min=pLst[[i]]$prior[1],max=pLst[[i]]$prior[2]),2)
  }
  return(pLst)
}


paramEst.ML = function(Y,pi12,y_1,nsim,pi0,model){
  pm <- matrix(data=0,nrow=nsim,ncol=length(model$param))
  temParam <- matrix(data=0,nrow=1,ncol=length(model$param))
  for(i in 1:nsim){
    model$param <- ranGenParam(model$param)
    likelihood <- mcLikelihood(model,Y,y_1,pi0,pi12)
    sumLogL <- sum(log(likelihood[1][[1]][2:length(Y)]))
    if(sumLogL == '-Inf' | sumLogL == 'NaN'){sumLogL = -99999}
    if(i==1){
      pSwch = 1
    }else{
      LLR = exp(sumLogL - sumLogL_0)
      pSwch <- min(1,LLR)
    }
    if(runif(1)<pSwch){
      for (j in 1:length(model$param)) {
        temParam[1,j] = model$param[[j]]$value
      }
      sumLogL_0 = sumLogL
    }
    pm[i,] <- temParam
  }
  pm <- as.data.frame(pm)
  colnames(pm) <- model$paramNames
  pmSpace <- group_by(pm,.dots=model$paramNames)%>%tally()
  pmSpace <- pmSpace[which(pmSpace$n>1),]
  pmSpace$freq <- pmSpace$n/sum(pmSpace$n)
  tmpPmSpace <- subset(pmSpace,select=-c(n,freq))
  mx <- which.max(pmSpace$freq)
  pmSpaceBar <-c(pmSpace[[1]][mx],pmSpace[[2]][mx],pmSpace[[3]][mx],pmSpace[[4]][mx],pmSpace[[5]][mx])
  #pmSpaceBar <-t( t(tmpPmSpace)%*%pmSpace$freq)
  for (j in 1:length(model$param)) {
    model$param[[j]]$value = pmSpaceBar[j]
    model$output[j] = pmSpaceBar[j]
  }
  estL <- mcLikelihood(model,Y,y_1,pi0,pi12)
  
  model$output[length(model$output)] <- estL[[2]][length(Y),2]
  return(model)
}

paramEst = function(Y,pi12,y_1,nsim,pi0,model){
  pm <- matrix(data=0,nrow=nsim,ncol=length(model$param))
  temParam <- matrix(data=0,nrow=1,ncol=length(model$param))
  for(i in 1:nsim){
    model$param <- ranGenParam(model$param)
  #  model$param <- lapply(model$param,ranGenParam)
    likelihood <- mcLikelihood(model,Y,y_1,pi0,pi12)
    sumLogL <- sum(log(likelihood[1][[1]][2:length(Y)]))
    if(sumLogL == '-Inf' | sumLogL == 'NaN'){sumLogL = -99999}
      if(i==1){
        pSwch = 1
      }else{
        LLR = exp(sumLogL - sumLogL_0)
        pSwch <- min(1,LLR)
      }
      if(runif(1)<pSwch){
        for (j in 1:length(model$param)) {
          temParam[1,j] = model$param[[j]]$value
        }
        sumLogL_0 = sumLogL
      }
     pm[i,] <- temParam
  }
  pm <- as.data.frame(pm)
  colnames(pm) <- model$paramNames
  pmSpace <- group_by(pm,.dots=model$paramNames)%>%tally()
  pmSpace <- pmSpace[which(pmSpace$n>1),]
  pmSpace$freq <- pmSpace$n/sum(pmSpace$n)
  tmpPmSpace <- subset(pmSpace,select=-c(n,freq))
  pmSpaceBar <-t( t(tmpPmSpace)%*%pmSpace$freq)
  for (j in 1:length(model$param)) {
    model$param[[j]]$value = pmSpaceBar[j]
    model$output[j] = pmSpaceBar[j]
  }
  estL <- mcLikelihood(model,Y,y_1,pi0,pi12)

  model$output[length(model$output)] <- estL[[2]][length(Y),2]
  return(model)
}

mcLikelihood = function(model, Y, y_1, pi0,p12){
  N <- matrix(data=0,nrow = length(Y),ncol = 2)
  pi <- matrix(data=0,nrow=length(Y),ncol=2)
  f <- vector(mode = 'numeric',length=length(Y))
  p <- matrix(c(model$param[[1]]$value,p12 ,1-model$param[[1]]$value,1-p12),nrow=2,ncol=2,byrow = TRUE)
  for(tmp.c in 1:length(Y)){
    if(tmp.c == 1){
      N[tmp.c,] <- c(returnErrorDnorm(model,Y[tmp.c],y_1,1),returnErrorDnorm(model,Y[tmp.c],y_1,2))
      f[tmp.c] <- c(1,1)%*%(p%*%pi0*N[tmp.c,])
      pi[tmp.c,] <- (p%*%pi0*N[tmp.c,])/f[tmp.c]
    }else{
      N[tmp.c,] <- c(returnErrorDnorm(model,Y[tmp.c],Y[tmp.c-1],1),returnErrorDnorm(model,Y[tmp.c],Y[tmp.c-1],2))
      f[tmp.c] <- c(1,1)%*%(p%*%pi[tmp.c-1,]*N[tmp.c,])
      pi[tmp.c,] <- (p%*%pi[tmp.c-1,]*N[tmp.c,])/f[tmp.c]
    }
  }
  return(list(f,pi))
}

returnErrorDnorm = function(model,y,y_1,state){
  if(state==1){
    fError <- eval(parse(text=model$model.null.erdis))
  }else{
    fError <- eval(parse(text=model$model.alt.erdis))
  }
  return(fError)
}
##there are two important spaces in this function: the data space and the pi space
BSquid.OT=function(model,y_1,c){
    rho <- 1-model$param[[1]]$value
#  #specify data space
    sim1 <- eval(parse(text=model$model.null))
    sim2 <- eval(parse(text=model$model.alt))
    sim <- c(sim1,sim2)
    lowBound <- min(sim)
    upperBound <- max(sim)
    inc = 0.01
    Y <- seq(from=lowBound, to=upperBound, by=inc)
    pi <- seq(from=0,to=1,by=0.01)
    pi<-pi[which(pi>0)]
#  #initialize vector and matrix
    Z1 <- vector(mode='numeric',length=length(Y))
    Z2<-Z1
    piPrime <- matrix(data=0,nrow=length(pi),ncol=length(Y))

    for(n in 1: length(Y)){
      Z1[n]<-returnErrorDnorm(model,Y[n],y_1,1)
      Z2[n]<-returnErrorDnorm(model,Y[n],y_1,2)
    }

    P1 <- Z1/sum(Z1)
    P2 <- Z2/sum(Z2)

    lr <- Z2/Z1
  #piPrime is a matrix with dimension: size of pi by size of data space
  #the matrix is defined as given pi what is pi_prime at each data point
  for(j in 1:length(pi)){
    like <- (lr*(pi[j]+rho*(1-pi[j])))/(lr*(pi[j]+rho*(1-pi[j]))+(1-rho)*(1-pi[j]))
    like[which(like=='inf')]<- 1
    piPrime[j,]=t(round(100*like))
  }
  #initialize matrix
  Pp1 <- matrix(0,nrow=length(Y),ncol=length(pi))
  Pp2 <- Pp1
  T1 <- matrix(0,nrow=length(pi),ncol = length(pi))
  T2 <- T1
  N_T <- T1
  for(j in 1:length(pi)){
    Pp1 <- matrix(0,nrow=length(Y),ncol=length(pi))
    Pp2 <- matrix(0,nrow=length(Y),ncol=length(pi))
    for(i in 1:length(Y)){
      #Pp1 is data space by pi space, it is defined as give pi, the probability of getting the pi_prime given state 1
      Pp1[i,piPrime[j,i]]=P1[i]
      #Pp1 is data space by pi space, it is defined as give pi, the probability of getting the pi_prime given state 2
      Pp2[i,piPrime[j,i]]=P2[i]
    }
    T1[j,]<-colSums(Pp1,na.rm = FALSE,dims = 1)
    T2[j,]<-colSums(Pp2,na.rm = FALSE,dims = 1)
  }

  for(j in 1:length(pi)){
    N_T[j,] <- pi[j]*T2[j,]+(1-(pi[j]))*(1-rho)*T1[j,]+(1-pi[j])*rho*T2[j]
  }

  T_tmp <- rep(1,length(pi))
  T_tmp[rowSums(N_T)>1] <- rowSums(N_T)[rowSums(N_T)>1]
  T <- N_T/T_tmp
  h <- c*T%*%pi+1+(c-1)*pi

  g <- as.matrix(1-pi)
  r <- cbind(g,h)
  Q <- pmin(g,h)
  ep = 1
  cnt = 0
  while(ep > 0.001 & cnt <500){
    Q1 <- Q
    cnt = cnt+1
    h <- T%*%Q+c*pi
    Q <- pmin(g,h)
    ep <- max(abs(Q1-Q),0,na.rm=TRUE)
    diff <- (g-h)^2
    if(cnt < 499){
      piStar <- pi[which.min(diff)]
    }else{
      piStar <- NA
    }

  }
  return(piStar)
}
