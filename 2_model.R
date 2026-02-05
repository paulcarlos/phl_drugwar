Sys.setLanguage("en")

# import packages
library(lubridate)
library(splines)
library(dlnm)
library(MASS)
library(pbs)
library(zoo)
library(scales)

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
nm <- c("Firearms","Sepsis","Circulatory","Respiratory","Unknown")

# add Drug War Period
dat$dwar <- 0
dat$dwar[dat$yrwk%in%dutwk] <- sapply(dat$yrwk[dat$yrwk%in%dutwk],function(z) grep(z,dutwk))
dat$week <- sapply(dat$yrwk, function(z) grep(z,twk))
dat$isoweek <- as.numeric(substr(dat$yrwk,6,8))

# adjust TC wind
#summary(dat$tcwind)
dat$tcwind2 <- ifelse(dat$tcwind>=33,2,1)
dat$tcwind2 <- factor(dat$tcwind2)

# create blank dataframe
prv <- unique(dat$provname)
dt <- data.frame("prov"=rep(prv,each=length(dutwk)*length(grp)*3),
                 "cat"=rep(do.call(paste0,expand.grid(grp,c("","_18a","_17b"))),each=length(dutwk),times=length(prv)),
                 "week"=rep(dutwk,times=length(prv)*length(grp)*3),
                 "an"=NA,"lower"=NA,"upper"=NA)
sum(duplicated(dt))

# loop final model per province
lwqaic <- readRDS("0_data/4_classical_ITS/qaic_lowest_specs_prov_v2.rds")
z_score <- qnorm(0.975) 
nsim <- 1000 # number of Monte Carlo simulations
indwk <- unname(sapply(dutwk,function(z)grep(z,twk)))

pdf(file="plots_its_prov_rr.pdf",width=15,height=15) # save plots
par(mfrow=c(5,3),mar=c(3,4,3,3))

for (i in 1:nrow(lwqaic)) {
  #i=1L
  p1 <- lwqaic$prov[i]
  d1 <- dat[dat$provname==p1,]
  tkn <- quantile(d1$t2m,c(0.2,0.8))
  
  for (j in grp) {
    #j=grp[1]
    nkp <- lwqaic[i,paste0("nkpost_",j)]
    dfs <- lwqaic[i,paste0("dfseas_",j)]
    
    # All ages
    if (!is.na(nkp)) {
      kpost <- equalknots(d1$dwar,nkp)
      bpost <- onebasis(d1$dwar, fun="bs", degree=2, knots = kpost)
      
      d1$out <- d1[,j]
      if (sum(d1$tcwind2)>0) {
        mod <- glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                   family=quasipoisson,data=d1,na.action="na.exclude")
      } else {
        mod <- glm(out~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                   family=quasipoisson,data=d1,na.action="na.exclude")
      }
      
      cp <- crosspred(bpost,mod,cen=0,by=1)
      
      tb1 <- matrix(NA,nrow=nsim,ncol=length(dutwk))
      an1 <- (1-exp(-bpost%*%cp$coefficients))*d1$out
      #tb1[1,] <- an1[indwk]
      set.seed(20260113)
      cfsim <- mvrnorm(nsim,cp$coefficients,cp$vcov)
      for (s in 1:nsim) {
        #s=1
        ansim <- (1-exp(-bpost %*% cfsim[s,]))*d1$out
        tb1[s,] <- ansim[indwk]
      }
      dt$an[dt$prov==p1 & dt$cat==j] <- an1[indwk]
      dt$lower[dt$prov==p1 & dt$cat==j] <- apply(tb1,2,function(z)quantile(z,0.025,na.rm=TRUE))
      dt$upper[dt$prov==p1 & dt$cat==j] <- apply(tb1,2,function(z)quantile(z,0.975,na.rm=TRUE))
      
      
      if (sum(is.infinite(cp$allRRhigh))==0 & max(cp$allRRhigh[!is.infinite(cp$allRRhigh)])<21) {
        plot(cp,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",
             main=paste0(p1," - All (",nm[which(j==grp)],")"),lwd=2)
      } else {
        plot(cp,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),
             main=paste0(p1," - All (",nm[which(j==grp)],")"))
      }
      axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
      
    } else {
      plot(NA,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),xlim=c(1,length(dutwk)),
           main=paste0(p1," - All (",nm[which(j==grp)],")"))
      axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
    }
    
    
    # Adults
    nkp <- lwqaic[i,paste0("nkpost_",j,"_18a")]
    dfs <- lwqaic[i,paste0("dfseas_",j,"_18a")]
    if (!is.na(nkp)) {
      d1$out_18a <- d1[,paste0(j,"_18a")]
      
      if (sum(d1$tcwind2)>0) {
        trc_18a <- tryCatch(mod <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                                       family=quasipoisson,data=d1,na.action="na.exclude"),
                            error=function(e)FALSE,warning=function(w)FALSE)
      } else {
        trc_18a <- tryCatch(mod <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                                       family=quasipoisson,data=d1,na.action="na.exclude"),
                            error=function(e)FALSE,warning=function(w)FALSE)
      }
      
      
      if (any(class(trc_18a)=="glm")) {
        
        if (sum(d1$tcwind2)>0) {
          mod_18a <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                         family=quasipoisson,data=d1,na.action="na.exclude")
        } else {
          mod_18a <- glm(out_18a~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                         family=quasipoisson,data=d1,na.action="na.exclude")
        }
        
        cp_18a <- crosspred(bpost,mod_18a,cen=0,by=1)
        
        tb1 <- matrix(NA,nrow=nsim,ncol=length(dutwk))
        an1 <- (1-exp(-bpost%*%cp_18a$coefficients))*d1$out_18a
        #tb1[1,] <- an1[indwk]
        set.seed(20260113)
        cfsim <- mvrnorm(nsim,cp_18a$coefficients,cp_18a$vcov)
        for (s in 1:nsim) {
          #s=1
          ansim <- (1-exp(-bpost %*% cfsim[s,]))*d1$out_18a
          tb1[s,] <- ansim[indwk]
        }
        dt$an[dt$prov==p1 & dt$cat==paste0(j,"_18a")] <- an1[indwk]
        dt$lower[dt$prov==p1 & dt$cat==paste0(j,"_18a")] <- apply(tb1,2,function(z)quantile(z,0.025,na.rm=TRUE))
        dt$upper[dt$prov==p1 & dt$cat==paste0(j,"_18a")] <- apply(tb1,2,function(z)quantile(z,0.975,na.rm=TRUE))
        
        
        if (sum(is.infinite(cp_18a$allRRhigh))==0 & max(cp_18a$allRRhigh[!is.infinite(cp_18a$allRRhigh)])<21) {
          plot(cp_18a,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",
               main=paste0(p1," - Adults (",nm[which(j==grp)],")"),lwd=2)
        } else {
          plot(cp_18a,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),
               main=paste0(p1," - Adults (",nm[which(j==grp)],")"))
        }
        axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
        
      } else {
        plot(NA,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),xlim=c(1,length(dutwk)),
             main=paste0(p1," - Adults (",nm[which(j==grp)],")"))
        axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
      }
      
    } else {
      plot(NA,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),xlim=c(1,length(dutwk)),
           main=paste0(p1," - Adults (",nm[which(j==grp)],")"))
      axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
    }
    
    
    # Minors
    nkp <- lwqaic[i,paste0("nkpost_",j,"_17b")]
    dfs <- lwqaic[i,paste0("dfseas_",j,"_17b")]
    if (!is.na(nkp)) {
      d1$out_17b <- d1[,paste0(j,"_17b")]
      
      if (sum(d1$tcwind2)>0) {
        trc_17b <- tryCatch(mod <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                                       family=quasipoisson,data=d1,na.action="na.exclude"),
                            error=function(e)FALSE,warning=function(w)FALSE)
      } else {
        trc_17b <- tryCatch(mod <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                                       family=quasipoisson,data=d1,na.action="na.exclude"),
                            error=function(e)FALSE,warning=function(w)FALSE)
      }
      
      if (any(class(trc_17b)=="glm")) {
        
        if (sum(d1$tcwind2)>0) {
          mod_17b <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+tcwind2+offset(log(pop)),
                         family=quasipoisson,data=d1,na.action="na.exclude")
        } else {
          mod_17b <- glm(out_17b~bpost+week+pbs(isoweek,df=dfs)+ns(t2m,knots=tkn)+offset(log(pop)),
                         family=quasipoisson,data=d1,na.action="na.exclude")
        }
        
        cp_17b <- crosspred(bpost,mod_17b,cen=0,by=1)
        
        tb1 <- matrix(NA,nrow=nsim,ncol=length(dutwk))
        an1 <- (1-exp(-bpost%*%cp_17b$coefficients))*d1$out_17b
        #tb1[1,] <- an1[indwk]
        set.seed(20260113)
        cfsim <- mvrnorm(nsim,cp_17b$coefficients,cp_17b$vcov)
        for (s in 1:nsim) {
          #s=1
          ansim <- (1-exp(-bpost %*% cfsim[s,]))*d1$out_17b
          tb1[s,] <- ansim[indwk]
        }
        dt$an[dt$prov==p1 & dt$cat==paste0(j,"_17b")] <- an1[indwk]
        dt$lower[dt$prov==p1 & dt$cat==paste0(j,"_17b")] <- apply(tb1,2,function(z)quantile(z,0.025,na.rm=TRUE))
        dt$upper[dt$prov==p1 & dt$cat==paste0(j,"_17b")] <- apply(tb1,2,function(z)quantile(z,0.975,na.rm=TRUE))
        
        if (sum(is.infinite(cp_17b$allRRhigh))==0 & max(cp_17b$allRRhigh[!is.infinite(cp_17b$allRRhigh)])<21) {
          plot(cp_17b,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",
               main=paste0(p1," - Minors (",nm[which(j==grp)],")"),lwd=2)
        } else {
          plot(cp_17b,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),
               main=paste0(p1," - Minors (",nm[which(j==grp)],")"))
        }
        axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
        
      } else {
        plot(NA,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),xlim=c(1,length(dutwk)),
             main=paste0(p1," - Minors (",nm[which(j==grp)],")"))
        axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
      }
    } else {
      plot(NA,xaxt="n",ylab="Relative Risk (95%CI)",xlab="Drug War Weeks",ylim=c(0.8,20),xlim=c(1,length(dutwk)),
           main=paste0(p1," - Minors (",nm[which(j==grp)],")"))
      axis(1,grep("-26|-01",dutwk),labels=dutwk[grep("-26|-01",dutwk)])
    }
  }
  cat(i," ")
}
dev.off()
rm(i,p1,d1,tkn,j,nkp,dfs,kpost,bpost,mod,cp,cp_18a,cp_17b,tb1,an1,cfsim)

summary(dt)
saveRDS(dt,"an_prov_dutwk_v2.rds")


# remove files
rm(list=ls());gc()