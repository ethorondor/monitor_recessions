options(scipen = 999, digits = 4)
require(tidyverse)
setwd("/home/elrond/Documents/Dropbox/Research/datingRecession/cfnai")
load("./output/opt_cfnai_hist.Rdata")
load("./output/peak2008.Rdata")
load("./output/trough2008.Rdata")
View(opt_cfnai_hist[[11]])
df <- t2008
i <- 0
i <- i+1;View(drop_na(df[[i]]))
  