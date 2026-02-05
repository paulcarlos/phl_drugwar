############# FIGURE 1 #############
Sys.setLanguage("en")
library(ggplot2)

# upload data
dat <- readRDS("an_prov_dutwk_v2.rds")

# ACLED drug kills
a1 <- readRDS("acled_daily_drugwar_2016-2020.rds")
acled <- aggregate(fatalities~admin2,data=a1,FUN="sum")
colnames(acled) <- c("prov","dkill")

# limit to significant values
excl <- c("Sulu","Tawi-Tawi","Maguindanao & Cotabato City","Basilan & City of Isabela","Lanao del Sur")
d1 <- dat[dat$lower>0 & !is.na(dat$lower) & !dat$prov%in%excl,]
#sum(d1$an[d1$cat%in%c("fira","sep","cvd","res","unk")])


# dataframe for all ages
cnm1 <- c("fira","sep","cvd","res","unk","fira_17b")
dt1 <- aggregate(an~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")
dt1$lower <- aggregate(lower~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")$lower
dt1$upper <- aggregate(upper~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")$upper
dt1$cat <- c("Circulatory","Firearms","Media reports\n(ACLED)",
             "Pneumonia","Sepsis","Deaths with\nunknown cause")
dt1$an[dt1$cat=="Media reports\n(ACLED)"] <- sum(acled$dkill[!acled$prov%in%excl])
dt1[dt1$cat=="Media reports\n(ACLED)",c("lower","upper")] <- NA
dt1$cat <- factor(dt1$cat,levels=c("Media reports\n(ACLED)","Firearms","Circulatory",
                                   "Pneumonia","Sepsis","Deaths with\nunknown cause"))
dt1$base <- 2100

# plot for all ages
#maxv <- round(max(dt1$upper,na.rm=TRUE),-4)
p1 <- ggplot(data=dt1,aes(y=cat,x=an,xmin=lower,xmax=upper)) +
  geom_col(fill="#076FA2",width=0.7) +
  scale_y_discrete(name="",limits=rev) +
  geom_errorbar(width=0.3,linewidth=0.5) +
  geom_text(aes(x=base,label=formatC(round(an),digits=5,big.mark=",")),size=4,color="white",fontface="bold") +
  geom_text(aes(x=upper,label=formatC(round(upper),digits=5,big.mark=",")),size=4,color="black",fontface="bold",nudge_x=3000) +
  scale_x_continuous(name="",breaks=seq(0,50000,5000),limits=c(0,50000+5000),expand=c(0,0)) +
  ggtitle("A. Total excess deaths in all ages") +
  theme_bw() +
  theme(plot.title = element_text(size=15,face="bold"),
        axis.title.y=element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# dataframe for minors
cnm1 <- c("fira","fira_17b","sep_17b","cvd_17b","res_17b","unk_17b")
dt2 <- aggregate(an~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")
dt2$lower <- aggregate(lower~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")$lower
dt2$upper <- aggregate(upper~cat,data=d1[d1$cat%in%cnm1,],FUN="sum")$upper
dt2$an[dt2$cat=="fira"] <- 122; dt2[dt2$cat=="fira",c("lower","upper")] <- NA # inserting OMCT number of drug war deaths
dt2$cat
dt2$cat <- c("Circulatory","Field reports\n(OMCT)","Firearms",
             "Pneumonia","Sepsis","Deaths with\nunknown cause")
dt2$cat <- factor(dt2$cat,levels=c("Field reports\n(OMCT)","Firearms","Circulatory",
                                   "Pneumonia","Sepsis","Deaths with\nunknown cause"))
dt2$base <- 30

# plot for minors
#maxv <- round(max(dt2$upper,na.rm=TRUE),-2)
p2 <- ggplot(data=dt2,aes(y=cat,x=an,xmin=lower,xmax=upper)) +
  geom_col(fill="#076FA2",width=0.7) +
  scale_y_discrete(name="",limits=rev) +
  geom_errorbar(width=0.3,linewidth=0.5) +
  geom_text(aes(x=base,label=formatC(round(an),digits=4,big.mark=",")),size=4,color="white",fontface="bold") +
  geom_text(aes(x=upper,label=formatC(round(upper),digits=4,big.mark=",")),size=4,color="black",fontface="bold",nudge_x=30) +
  scale_x_continuous(name="",breaks=seq(0,900,200),limits=c(0,900+100),expand=c(0,0)) +
  ggtitle("B. Total excess deaths in <18 years old") +
  theme_bw() +
  theme(plot.title = element_text(size=15,face="bold"),
        axis.title.y=element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# dataframe for all ages by year
ag1 <- aggregate(fatalities~year,data=a1,FUN="sum")
ag1$cat <- "Media reports\n(ACLED)"; ag1$an <- ag1$fatalities; ag1$upper <- ag1$lower <- NA
d1$year <- as.integer(substr(d1$week,1,4))
cnm1 <- c("fira","sep","cvd","res","unk")
dt3 <- aggregate(an~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")
dt3$lower <- aggregate(lower~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")$lower
dt3$upper <- aggregate(upper~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")$upper
#sum(dt3$an[dt3$year%in%2016:2017]); sum(dt3$lower[dt3$year%in%2016:2017]); sum(dt3$upper[dt3$year%in%2016:2017])
ag2 <- ag1[,colnames(dt3)]
dt3 <- rbind(dt3,ag2)

dt3$cat[dt3$cat=="fira"] <- "Firearms"
dt3$cat[dt3$cat=="cvd"] <- "Circulatory"
dt3$cat[dt3$cat=="res"] <- "Pneumonia"
dt3$cat[dt3$cat=="sep"] <- "Sepsis"
dt3$cat[dt3$cat=="unk"] <- "Unknown\ncause"
dt3$cat <- factor(dt3$cat,levels=c("Media reports\n(ACLED)","Firearms","Circulatory",
                                   "Pneumonia","Sepsis","Unknown\ncause"))
dt3$year <- factor(dt3$year)

# plot for all ages by year
col1 <- c("#A50F15","#DE2025","#FB5A4A","#FCAE91","#FEE5D9")
#maxv <- round(max(dt3$upper,na.rm=TRUE),-3)
p3 <- ggplot(data=dt3,aes(x=cat,y=an,ymin=lower,ymax=upper,fill=year)) +
  geom_bar(position=position_dodge(width=0.9),stat="identity") +
  geom_errorbar(position=position_dodge(width=0.9),width=0.3,linewidth=0.5,color="black") +
  scale_fill_manual(name="Year",values=col1) +
  scale_y_continuous(name="",breaks=seq(0,20000,4000),limits=c(0,20000),expand=c(0,0)) +
  ggtitle("C. Total excess deaths per year in all ages") +
  theme_bw() +
  theme(plot.title = element_text(size=15,face="bold",hjust=0),
        plot.title.position = "plot",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,face="bold"),
        #axis.title.y = element_text(size=12,face="bold"),
        panel.grid.major.x = element_blank()) 


# dataframe for minors by year
d1$year <- as.integer(substr(d1$week,1,4))
cnm1 <- c("fira_17b","sep_17b","cvd_17b","res_17b","unk_17b")
dt4 <- aggregate(an~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")
dt4$lower <- aggregate(lower~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")$lower
dt4$upper <- aggregate(upper~cat+year,data=d1[d1$cat%in%cnm1,],FUN="sum")$upper
dt4$cat[dt4$cat=="fira_17b"] <- "Firearms"
dt4$cat[dt4$cat=="cvd_17b"] <- "Circulatory"
dt4$cat[dt4$cat=="res_17b"] <- "Pneumonia"
dt4$cat[dt4$cat=="sep_17b"] <- "Sepsis"
dt4$cat[dt4$cat=="unk_17b"] <- "Unknown\ncause"
dt4$cat <- factor(dt4$cat,levels=c("Firearms","Circulatory",
                                   "Pneumonia","Sepsis","Unknown\ncause"))
dt4$year <- factor(dt4$year)

# plot for minors by year
col1 <- c("#A50F15","#DE2025","#FB5A4A","#FCAE91","#FEE5D9")
#maxv <- round(max(dt4$upper,na.rm=TRUE),-2)
p4 <- ggplot(data=dt4,aes(x=cat,y=an,ymin=lower,ymax=upper,fill=year)) +
  geom_bar(position=position_dodge(width=0.9),stat="identity") +
  geom_errorbar(position=position_dodge(width=0.9),width=0.3,linewidth=0.5,color="black") +
  scale_fill_manual(name="Year",values=col1) +
  scale_y_continuous(name="",breaks=seq(0,400,100),limits=c(0,400),expand=c(0,0)) +
  ggtitle("D. Total excess deaths per year in <18 years old") +
  theme_bw() +
  theme(plot.title = element_text(size=15,face="bold",hjust=0),
        plot.title.position = "plot",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,face="bold"),
        #axis.title.y = element_text(size=12,face="bold"),
        panel.grid.major.x = element_blank()) 

plt1 <- cowplot::plot_grid(p1,p2,nrow=1,align="h")
plt2 <- cowplot::plot_grid(p3,p4,nrow=1,align="h")
plt3 <- cowplot::plot_grid(plt1,plt2,nrow=2,rel_heights=c(0.6,0.7),greedy=FALSE)

sjPlot::save_plot("fig1_total_year.png",plt3,height=30,width=40,dpi=1200)


# remove all
rm(list=ls());gc()




############# FIGURE 2 #############
Sys.setLanguage("en")
library(ggplot2)
library(terra)
library(tidyterra)
library(classInt)


# upload data
dat <- readRDS("an_prov_dutwk_v2.rds")

# ACLED drug kills
a1 <- readRDS("0_data/4_classical_ITS/acled_daily_drugwar_2016-2020.rds")
acled <- aggregate(fatalities~admin2,data=a1,FUN="sum")
colnames(acled) <- c("prov","dkill")

# limit to significant values
excl <- c("Sulu","Tawi-Tawi","Maguindanao & Cotabato City","Basilan & City of Isabela","Lanao del Sur")
d1 <- dat[dat$lower>0 & !is.na(dat$lower) & !dat$prov%in%excl,]
#sum(d1$an[d1$cat%in%c("fira","sep","cvd","res","unk")])

# import provincial map
shp <- vect("phl_prov_v1_simplify.gpkg")

# get manila extent
mla <- shp[shp$ADM2_EN=="National Capital Region",]
extnt <- ext(mla)
extvec <- vect(extnt,crs=mla)


# tables and plots
colnames(d1)
tab1 <- data.frame("prov"=shp$ADM2_EN,"an"=NA,"grp1"=NA,"dkill"=NA,"diff_fira"=NA,"grp2"=NA,
                   "others"=NA,"grp3"=NA,"diff_all"=NA,"grp4"=NA)

ag1 <- aggregate(an~prov,data=d1[d1$cat=="fira",],FUN="sum")
#grp1 <- classIntervals(tab1$an,n=4,style="fisher")
#val1 <- round(grp1$brks)
val1 <- c(1,100,300,1500,3000)
lab1 <- c("0",paste0(val1[1],"-",val1[2]),paste0(val1[2],"-",val1[3]),
          paste0(val1[3],"-",val1[4]),paste0(val1[4],"-",val1[5]))
tab1$an <- ag1$an[match(tab1$prov,ag1$prov)]
tab1$an[is.na(tab1$an) & !tab1$prov%in%excl] <- 0
tab1$grp1 <- cut(tab1$an,breaks=c(0,val1),include.lowest=TRUE,labels=lab1)
tab1$grp1 <- factor(tab1$grp1,levels=lab1)
shp$fira <- tab1$grp1[match(shp$ADM2_EN,tab1$prov)]

tab1$dkill <- acled$dkill[match(tab1$prov,acled$prov)]
tab1$dkill[is.na(tab1$dkill) & !tab1$prov%in%excl] <- 0
tab1$diff_fira <- tab1$dkill-tab1$an
#grp2 <- classIntervals(tab1$diff_fira[tab1$diff_fira<=0 & !is.na(tab1$diff_fira)],n=4,style="fisher")
#val2 <- round(grp2$brks)
val2 <- c(-400,-200,-80,-25,-0.01,600)
lab2 <- c(paste0(val2[1]," to ",val2[2]),paste0(val2[2]," to ",val2[3]),
          paste0(val2[3]," to ",val2[4]),paste0(val2[4]," to ",0),">0")
tab1$grp2 <- cut(tab1$diff_fira,breaks=c(val2),labels=lab2)
tab1$grp2 <- factor(tab1$grp2,levels=c("0",lab2))
shp$diff_fira <- tab1$grp2[match(shp$ADM2_EN,tab1$prov)]

ag2 <- aggregate(an~prov,data=d1[d1$cat%in%c("fira","cvd","res","sep","unk"),],FUN="sum")
#grp3 <- classIntervals(ag2$an,n=4,style="fisher")
#grp3$brks
val3 <- c(1,800,3000,9000,16000)
lab3 <- c("0",paste0(val3[1],"-",val3[2]),paste0(val3[2],"-",val3[3]),
          paste0(val3[3],"-",val3[4]),paste0(val3[4],"-",val3[5]))
tab1$others <- ag2$an[match(tab1$prov,ag2$prov)]
tab1$others[is.na(tab1$others) & !tab1$prov%in%excl] <- 0
tab1$grp3 <- cut(tab1$others,breaks=c(0,val3),labels=lab3)
tab1$grp3 <- factor(tab1$grp3,levels=c(lab3))
shp$others <- tab1$grp3[match(shp$ADM2_EN,tab1$prov)]

tab1$diff_all <- tab1$dkill-tab1$others
classIntervals(tab1$diff_all,n=4,style="fisher")$brks
val4 <- c(-15000,-8000,-2000,-800,-1)
lab4 <- c(paste0(val4[1]," to ",val4[2]),paste0(val4[2]," to ",val4[3]),
          paste0(val4[3]," to ",val4[4]),paste0(val4[4]," to ",val4[5]))
tab1$grp4 <- cut(tab1$diff_all,breaks=c(val4),include.lowest=TRUE,labels=lab4)
tab1$grp4 <- factor(tab1$grp4,levels=c(lab4))
shp$diff_all <- tab1$grp4[match(shp$ADM2_EN,tab1$prov)]


# colors
col1 <- c("#A50F15","#DE2025","#FB5A4A","#FCAE91","#FEE5D9")

# other shapes
cebpt <- rbind(c(123.93,10.71),c(121.91,9.52))
ceblab <- vect(matrix(c(cebpt[2,1]-0.4,cebpt[2,2]),nrow=1),type="points",crs=crs(shp))
ceblin <- vect(cebpt,type="lines",crs=crs(shp))
bulpt <- rbind(c(121.13,15.10),c(119.35,15.50))
bullab <- vect(matrix(c(bulpt[2,1]-0.8,bulpt[2,2]),nrow=1),type="points",crs=crs(shp))
bullin <- vect(bulpt,type="lines",crs=crs(shp))
nuept <- rbind(c(121.09,15.74),c(119.30,17.50))
nuelab <- vect(matrix(c(nuept[2,1]-0.4,nuept[2,2]),nrow=1),type="points",crs=crs(shp))
nuelin <- vect(nuept,type="lines",crs=crs(shp))
mmlin <- vect(rbind(c(extnt$xmax,extnt$ymax),c(127,extnt$ymax)),type="lines",crs=crs(shp))
ddnpt <- rbind(c(125.66,7.43),c(123.57,7.07))
ddnlab <- vect(matrix(c(ddnpt[2,1]-0.6,ddnpt[2,2]),nrow=1),type="points",crs=crs(shp))
ddnlin <- vect(ddnpt,type="lines",crs=crs(shp))
compt <- rbind(c(125.92,7.86),c(124.53,8.96))
comlab <- vect(matrix(c(compt[2,1]-0.6,compt[2,2]),nrow=1),type="points",crs=crs(shp))
comlin <- vect(compt,type="lines",crs=crs(shp))
sulpt <- rbind(c(124.32,6.52),c(123.80,5.74))
sullab <- vect(matrix(c(sulpt[2,1]-0.8,sulpt[2,2]),nrow=1),type="points",crs=crs(shp))
sullin <- vect(sulpt,type="lines",crs=crs(shp))
panpt <- rbind(c(120.02,16.00),c(119.35,15.50))
panlab <- vect(matrix(c(panpt[2,1]-1.0,panpt[2,2]),nrow=1),type="points",crs=crs(shp))
panlin <- vect(panpt,type="lines",crs=crs(shp))

leypt <- rbind(c(124.67,11.14),c(123.96,11.56))
leylab <- vect(matrix(c(leypt[2,1]-0.4,leypt[2,2]),nrow=1),type="points",crs=crs(shp))
leylin <- vect(leypt,type="lines",crs=crs(shp))
ilopt <- rbind(c(122.42,10.78),c(121.05,11.33))
ilolab <- vect(matrix(c(ilopt[2,1]-0.7,ilopt[2,2]),nrow=1),type="points",crs=crs(shp))
ilolin <- vect(ilopt,type="lines",crs=crs(shp))
cavpt <- rbind(c(120.78,14.23),c(120.01,14.05))
cavlab <- vect(matrix(c(cavpt[2,1]-0.8,cavpt[2,2]),nrow=1),type="points",crs=crs(shp))
cavlin <- vect(cavpt,type="lines",crs=crs(shp))
cotpt <- rbind(c(124.88,6.32),c(124.17,5.67))
cotlab <- vect(matrix(c(cotpt[2,1]-0.7,cotpt[2,2]),nrow=1),type="points",crs=crs(shp))
cotlin <- vect(cotpt,type="lines",crs=crs(shp))

ddspt <- rbind(c(125.45,7.11),c(123.80,5.74))
ddslab <- vect(matrix(c(ddspt[2,1]-0.4,ddspt[2,2]),nrow=1),type="points",crs=crs(shp))
ddslin <- vect(ddspt,type="lines",crs=crs(shp))


# plot firearms
p1 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=fira)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  scale_fill_manual(name="Excess firearms deaths in all ages",values=rev(col1)) +
  geom_spatvector(data=bullin,color="black") +
  geom_spatvector_text(data=bullab,label="BULACAN",size=2) +
  geom_spatvector(data=nuelin,color="black") +
  geom_spatvector_text(data=nuelab,label="NUEVA\nECIJA",size=2,lineheight=0.8) +
  geom_spatvector(data=ceblin,color="black") +
  geom_spatvector_text(data=ceblab,label="CEBU",size=2) +
  geom_spatvector(data=mmlin,color="black") +
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,ncol=3,byrow=TRUE))

p2 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=fira)) +
  scale_fill_manual(name="",values=rev(col1)) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=6),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt1 <- cowplot::plot_grid(p1,p2,nrow=1,rel_widths=c(1,0.1),greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# plot difference
p3 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff_fira)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  scale_fill_manual(name="Relative media under-reporting for firearm deaths",values=col1) +
  geom_spatvector(data=ddnlin,color="black") +
  geom_spatvector_text(data=ddnlab,label="DAVAO\nDEL NORTE",size=2,lineheight=0.8) +
  geom_spatvector(data=comlin,color="black") +
  geom_spatvector_text(data=comlab,label="DAVAO\nDE ORO",size=2,lineheight=0.8) +
  geom_spatvector(data=sullin,color="black") +
  geom_spatvector_text(data=sullab,label="SULTAN\nKUDARAT",size=2,lineheight=0.8) +
  geom_spatvector(data=panlin,color="black") +
  geom_spatvector_text(data=panlab,label="PANGASINAN",size=2) +
  geom_spatvector(data=mmlin,color="black") +
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,ncol=3,byrow=TRUE))

p4 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff_fira)) +
  scale_fill_manual(name="",values=col1) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=6),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt2 <- cowplot::plot_grid(p3,p4,nrow=1,rel_widths=c(1,0.1),greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))



# plot others for all ages
p5 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=others)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  #scale_fill_discrete(name="Excess deaths from other causes in all ages",drop=FALSE) +
  scale_fill_manual(name="Excess deaths from all causes in all ages",values=rev(col1),drop=FALSE) +
  geom_spatvector(data=leylin,color="black") +
  geom_spatvector_text(data=leylab,label="LEYTE",size=2) +
  geom_spatvector(data=ilolin,color="black") +
  geom_spatvector_text(data=ilolab,label="ILOILO",size=2) +
  geom_spatvector(data=cavlin,color="black") +
  geom_spatvector_text(data=cavlab,label="CAVITE",size=2) +
  geom_spatvector(data=panlin,color="black") +
  geom_spatvector_text(data=panlab,label="PANGASINAN",size=2) +
  geom_spatvector(data=mmlin,color="black") +
  geom_spatvector(data=ddslin,color="black") +
  geom_spatvector_text(data=ddslab,label="DAVAO\nDEL SUR",size=2,lineheight=0.8) +
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,ncol=3,byrow=TRUE))

p6 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=others)) +
  scale_fill_manual(name="",values=rev(col1),drop=FALSE) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=6),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt3 <- cowplot::plot_grid(p5,p6,nrow=1,rel_widths=c(1,0.1),greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# plot difference for all causes
p7 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff_all)) +
  geom_spatvector(data=extvec,color="blue",lwd=0.5,fill=NA) +
  scale_fill_manual(name="Relative media under-reporting for all deaths",values=col1) +
  geom_spatvector(data=leylin,color="black") +
  geom_spatvector_text(data=leylab,label="LEYTE",size=2) +
  geom_spatvector(data=ddslin,color="black") +
  geom_spatvector_text(data=ddslab,label="DAVAO\nDEL SUR",size=2,lineheight=0.8) +
  geom_spatvector(data=ilolin,color="black") +
  geom_spatvector_text(data=ilolab,label="ILOILO",size=2) +
  geom_spatvector(data=cavlin,color="black") +
  geom_spatvector_text(data=cavlab,label="CAVITE",size=2) +
  geom_spatvector(data=panlin,color="black") +
  geom_spatvector_text(data=panlab,label="PANGASINAN",size=2) +
  geom_spatvector(data=mmlin,color="black") +
  coord_sf(xlim=c(116,127),
           ylim=c(4,21),expand=F) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_text(size=12),  
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5,"cm"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0.5,-0.5,0,0),"cm")) +  
  guides(fill=guide_legend(title.position="top",nrow=2,ncol=3,byrow=TRUE))

p8 <- ggplot() +
  geom_spatvector(data=shp,aes(fill=diff_all)) +
  scale_fill_manual(name="",values=col1) +
  coord_sf(xlim=c(extnt$xmin,extnt$xmax),
           ylim=c(extnt$ymin,extnt$ymax),expand=F) +
  labs(title="METRO MANILA") +
  theme_bw() +
  theme(plot.title = element_text(size=6),
        legend.position="none",
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0,0.5,3,-1),"cm"), #c(top,right,bottom,left)
        panel.border=element_rect(colour="blue",fill=NA,linewidth=2))

plt4 <- cowplot::plot_grid(p7,p8,nrow=1,rel_widths=c(1,0.1),greedy=FALSE) + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# combine all plots
plt <- cowplot::plot_grid(plt1,plt2,plt3,plt4,nrow=2,greedy=FALSE,labels=c("A","B","C","D"),label_size=16)

sjPlot::save_plot("fig2_map_prov_v2.png",plt,height=28,width=20,dpi=1500)


# remove all files
rm(list=ls());gc()