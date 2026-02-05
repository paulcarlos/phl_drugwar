Sys.setLanguage("en")

# import packages
library(lubridate)
library(splines)
library(dlnm)
library(MASS)
library(pbs)
library(zoo)
library(scales)

# import qAIC function
source("qaicbic.R")

# generate weeks 
dut <- seq(ymd(20160630),ymd(20200126),1) # Duterte presidency without COVID time/week
dutwk <- unique(paste0(isoyear(dut),"-",sprintf("%02d",isoweek(dut))))
noy <- seq(ymd(20100630),ymd(20160630),1) # Aquino presidency
noywk <- unique(paste0(isoyear(noy),"-",sprintf("%02d",isoweek(noy))))
noywk <- noywk[noywk!=dutwk[1]]
twk <- c(noywk,dutwk)

# upload data
dat <- readRDS("weekly_mort_prov_2010-2020_v2.rds") # NOT SHAREABLE
dat <- dat[dat$yrwk%in%twk,]
colnames(dat)
grp <- c("fira","sep","cvd","res","unk")

# add Drug War Period
dat$dwar <- 0
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk))
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk))
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8))

# create TC wind indicator variable
summary(dat$tcwind)
dat$tcwind2 <- ifelse(dat$tcwind>=33,2,1)
dat$tcwind2 <- factor(dat$tcwind2)

# model specifications
nkpost <- seq(5,13,1) # number of knots for drug war weeks
dfseas <- 3:7 # degrees of freedom for seasonality

# create blank dataframe for model specs
prv <- unique(dat$provname)
mspec <- data.frame("prov"=rep(prv,each=length(nkpost)*length(dfseas)),
                    "nkpost"=rep(nkpost,times=length(prv),each=length(dfseas)),
                    "dfseas"=rep(dfseas,times=length(prv)*length(nkpost)),
                    "qaic_fira"=NA,"qaic_fira_18a"=NA,"qaic_fira_17b"=NA,
                    "qaic_sep"=NA,"qaic_sep_18a"=NA,"qaic_sep_17b"=NA,
                    "qaic_cvd"=NA,"qaic_cvd_18a"=NA,"qaic_cvd_17b"=NA,
                    "qaic_res"=NA,"qaic_res_18a"=NA,"qaic_res_17b"=NA,
                    "qaic_unk"=NA,"qaic_unk_18a"=NA,"qaic_unk_17b"=NA)
sum(duplicated(mspec))

# loop by combination of specs
for (i in 1:nrow(mspec)) {
  #i=1
  p1 <- mspec$prov[i]
  d1 <- dat[dat$provname==p1,]
  nkp <- mspec$nkpost[i]
  dfs <- mspec$dfseas[i]
  kpost <- equalknots(d1$dwar,nkp)
  tkn <- quantile(d1$t2m,c(0.2,0.8))
  
  bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots=kpost)
  #bpost <- onebasis(d1$dwar, fun="ns", knots=kpost)
  
  for (j in grp) {
    #j=grp[1]
    d1$out <- d1[,j]
    d1$out_18a <- d1[,paste0(j,"_18a")]
    d1$out_17b <- d1[,j]
    
    if (sum(d1$tcwind2)>0) {
      trc1 <- tryCatch(glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mod1 <- glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j)] <- qaicbic(mod1)[1]
      }
      
      trc2 <- tryCatch(glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mod2 <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j,"_18a")] <- qaicbic(mod2)[1]
      }
      
      trc3 <- tryCatch(glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc3)=="glm")) {
        mod3 <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j,"_17b")] <- qaicbic(mod3)[1]
      }
      
    } else {
      trc1 <- tryCatch(glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc1)=="glm")) {
        mod1 <- glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j)] <- qaicbic(mod1)[1]
      }
      
      trc2 <- tryCatch(glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc2)=="glm")) {
        mod2 <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j,"_18a")] <- qaicbic(mod2)[1]
      }
      
      trc3 <- tryCatch(glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                           family=quasipoisson,data=d1,na.action="na.exclude"),
                       error=function(e)FALSE,warning=function(w)FALSE)
      if (any(class(trc3)=="glm")) {
        mod3 <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                    family=quasipoisson,data=d1,na.action="na.exclude")
        mspec[i,paste0("qaic_",j,"_17b")] <- qaicbic(mod3)[1]
      }
      
    }
    
  }
  
  cat(i," ")
}
rm(i,p1,d1,nkp,dfs,kpost,bpost,tkn,mod1,mod2,mod3,trc1,trc2,trc3)
saveRDS(mspec,"qaic_allspecs_prov_v2.rds")

# find lowest qAIC values
colnames(mspec)
lwqaic <- data.frame("prov"=prv,"nkpost_fira"=NA,"dfseas_fira"=NA,"nkpost_fira_18a"=NA,"dfseas_fira_18a"=NA,"nkpost_fira_17b"=NA,"dfseas_fira_17b"=NA,
                     "nkpost_sep"=NA,"dfseas_sep"=NA,"nkpost_sep_18a"=NA,"dfseas_sep_18a"=NA,"nkpost_sep_17b"=NA,"dfseas_sep_17b"=NA,
                     "nkpost_cvd"=NA,"dfseas_cvd"=NA,"nkpost_cvd_18a"=NA,"dfseas_cvd_18a"=NA,"nkpost_cvd_17b"=NA,"dfseas_cvd_17b"=NA,
                     "nkpost_res"=NA,"dfseas_res"=NA,"nkpost_res_18a"=NA,"dfseas_res_18a"=NA,"nkpost_res_17b"=NA,"dfseas_res_17b"=NA,
                     "nkpost_unk"=NA,"dfseas_unk"=NA,"nkpost_unk_18a"=NA,"dfseas_unk_18a"=NA,"nkpost_unk_17b"=NA,"dfseas_unk_17b"=NA)
for (i in 1:nrow(lwqaic)) {
  #i=1
  p1 <- lwqaic$prov[i]
  d1 <- mspec[mspec$prov==p1,]
  
  for (j in grp) {
    #j=grp[1]
    pq <- d1[,paste0("qaic_",j)]
    if (!sum(is.na(pq))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j)] <- d1$nkpost[which.min(pq)][1]
      lwqaic[i,paste0("dfseas_",j)] <- d1$dfseas[which.min(pq)][1]
    }
    
    pq_18a <- d1[,paste0("qaic_",j,"_18a")]
    if (!sum(is.na(pq_18a))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j,"_18a")] <- d1$nkpost[which.min(pq_18a)][1]
      lwqaic[i,paste0("dfseas_",j,"_18a")] <- d1$dfseas[which.min(pq_18a)][1]
    }
    
    pq_17b <- d1[,paste0("qaic_",j,"_17b")]
    if (!sum(is.na(pq_17b))==nrow(d1)) {
      lwqaic[i,paste0("nkpost_",j,"_17b")] <- d1$nkpost[which.min(pq_17b)][1]
      lwqaic[i,paste0("dfseas_",j,"_17b")] <- d1$dfseas[which.min(pq_17b)][1]
    }
  }
  
}
rm(i,p1,d1)
saveRDS(lwqaic,"qaic_lowest_specs_prov_v2.rds")

# remove all files
rm(list=ls());gc()