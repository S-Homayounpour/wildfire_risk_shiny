setwd("c:/wollongong/projects/ultimate/anal")
data<-read.csv("ult_anal_2014.csv")
attach(data)
library(nlme)
library(sciplot)
library(gam)
library(nlme)
logdist<-log(line_lengt)
totrain=pr+prx1
westwind<-windclass8=="W"
burnwt<-ifelse(burnt==1,1,0.044)
m0<-glm(burnt~1+fuelload, weights = burnwt, family=binomial)
m1<-glm(burnt~distance+ffdi+disruption+fuelload, weights = burnwt, family=binomial)
m1<-glm(burnt~logdist+ffdi+disruption+fuelload+forestp+elevation+totrain, weights = burnwt, family=binomial)
m1<-glm(burnt~logdist+ffdi+disruption+logdist*fuelload+logdist*forestp+elevation+totrain, weights = burnwt, family=binomial)
1 - exp(-2/10265*(logLik(m5)  -  logLik(m0))) [1]

windows(3.5,3.5)
boxplot(line_lengt~burnt,ylab="Distance (m)")
bargraph.CI(dirclass,burnt,ylab="Proportion Burnt")
burndat<-subset(data,burnt==1)
unburndat<-subset(data,burnt==0)
bhist<-hist(burndat$ffdi,plot=FALSE)
ubhist<-hist(unburndat$ffdi,plot=FALSE)
plot(ubhist$mids,ubhist$intensities,cex=0,xlab="FFDI",ylab="Frequency")
lines(bhist$mids,bhist$intensities,lwd=2)
lines(ubhist$mids,ubhist$intensities,lwd=2,lty=9)

#Final Model
m1<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi, weights = burnwt, family=binomial(link="logit"))
#new final model
m1<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi+westwind, weights = burnwt, family=binomial(link="logit"))
#Testing SLRV (non-significant)
m2<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi+westwind+neigb_wt, weights = burnwt, family=binomial(link="logit"))


library(tree)
tr1<-tree(burnt~forestp+tsf+line_lengt+ffdi_df1+kbdi+westwind)
plot(tr1); text(tr1,cex=0.8)
tr2<-prune.tree(tr1,best=6)
plot(tr2); text(tr2,cex=0.8)

#library(mgcv)
#gam1<-gam(burnt~s(forestp)+s(tsf)+s(line_lengt)+s(ffdi_df1)+s(kbdi),family=binomial)

#individual models
(1232.3-glm(burnt~logdist, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~disruption, weights = burnwt, family=binomial )$deviance)/12.323
(1232.3-glm(burnt~veg_dist, weights = burnwt, family=binomial) $deviance)/12.323
  (1232.3-glm(burnt~ffdi, weights = burnwt, family=binomial) $deviance)/12.323
 (1232.3-glm(burnt~ffdi_df1, weights = burnwt, family=binomial) $deviance)/12.323
  (1232.3-glm(burnt~df, weights = burnwt, family=binomial) $deviance)/12.323
  (1232.3-glm(burnt~kbdi, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~raincomb, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~rh, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~tmax, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~wspeed, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~wdir, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~wind_angle, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  tp, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  elevation, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  alt_std, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  asp_class, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  alt_change, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~  slope, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~forestp, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~vegpc, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~sparse, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~tsf, weights = burnwt, family=binomial) $deviance)/12.323
(1232.3-glm(burnt~fuelload, weights = burnwt, family=binomial) $deviance)/12.323



#logdist better than distance but logvegdist is worse than veg_dist
m1<-glm(burnt~logdist+disruption+veg_dist, weights = burnwt, family=binomial)
m2<-glm(burnt~logdist+disruption, weights = burnwt, family=binomial)
m3<-glm(burnt~logdist+veg_dist, weights = burnwt, family=binomial)
m4<-glm(burnt~disruption+veg_dist, weights = burnwt, family=binomial)
m5<-glm(burnt~logdist, weights = burnwt, family=binomial)
m6<-glm(burnt~disruption, weights = burnwt, family=binomial)
m7<-glm(burnt~veg_dist, weights = burnwt, family=binomial)

m5<-glm(burnt~logdist, weights = burnwt, family=binomial)


  #Set 1: FFDI aspect
  m1<-glm(burnt~ffdi+ffdi_df1+df+kbdi, weights = burnwt, family=binomial(link="logit"))
  
  m2<-glm(burnt~ffdi+ffdi_df1+df, weights = burnwt, family=binomial(link="logit"))
  m3<-glm(burnt~ffdi+ffdi_df1+kbdi, weights = burnwt, family=binomial(link="logit"))
  m4<-glm(burnt~ffdi+df+kbdi, weights = burnwt, family=binomial(link="logit"))
  m5<-glm(burnt~ffdi_df1+df+kbdi, weights = burnwt, family=binomial(link="logit"))
  
  m6<-glm(burnt~ffdi+ffdi_df1, weights = burnwt, family=binomial(link="logit"))
  m7<-glm(burnt~ffdi+df, weights = burnwt, family=binomial(link="logit"))
  m8<-glm(burnt~ffdi+kbdi, weights = burnwt, family=binomial(link="logit"))
  m9<-glm(burnt~ffdi_df1+df, weights = burnwt, family=binomial(link="logit"))
  m10<-glm(burnt~ffdi_df1+kbdi, weights = burnwt, family=binomial(link="logit"))
  m11<-glm(burnt~df+kbdi, weights = burnwt, family=binomial(link="logit"))
  
  m12<-glm(burnt~ffdi, weights = burnwt, family=binomial(link="logit"))
  m13<-glm(burnt~ffdi_df1, weights = burnwt, family=binomial(link="logit"))
  m14<-glm(burnt~df, weights = burnwt, family=binomial(link="logit"))
  m15<-glm(burnt~kbdi, weights = burnwt, family=binomial(link="logit"))
  
  m16<-glm(burnt~1, weights = burnwt, family=binomial(link="logit"))
  
  AIC1<-AIC(m1)
  AIC2<-AIC(m2)
  AIC3<-AIC(m3)
  AIC4<-AIC(m4)
  AIC5<-AIC(m5)
  AIC6<-AIC(m6)
  AIC7<-AIC(m7)
  AIC8<-AIC(m8)
  AIC9<-AIC(m9)
  AIC10<-AIC(m10)
  AIC11<-AIC(m11)
  AIC12<-AIC(m12)
  AIC13<-AIC(m13)
  AIC14<-AIC(m14)
  AIC15<-AIC(m15)
  AIC16<-AIC(m16)
  
  AICs<-rbind(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6,AIC7,AIC8,AIC9,AIC10,AIC11,AIC12,AIC13,AIC14,AIC15,AIC16)
  min<-min(AICs)
  
  dAICs<-AICs-min
  
  dAICs<-matrix(data=dAICs, nrow = 16, ncol = 1)
  L.gi.xf<-function (x) exp(x*-0.5)
  L.gi.x<-apply(dAICs,1,L.gi.xf)
  tot<-sum(L.gi.x)
  
  
  wi.f<- function(x) ((x)/tot)
  wi.l<-lapply(L.gi.x,wi.f)
  wi<-as.numeric(wi.l,length=17)
  table<-cbind(AICs,dAICs,wi)
  
  colnames(table)<-c("AIC","dAIC","wi")
  rownames(table)<-c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16")
  
  write.table(table, "routdec1.txt")

#Set 3: Raw weather

#aspect alt 2: raw: raincomb,rh,tmaxm,rh,wdir,wind_angle
#NB test raincomb v pr as alternatives
m1<-glm(burnt~raincomb+rh+tmax+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))

m2<-glm(burnt~raincomb+rh+tmax+wdir, weights = burnwt, family=binomial(link="logit"))
m3<-glm(burnt~raincomb+rh+tmax+wind_angle, weights = burnwt, family=binomial(link="logit"))
m4<-glm(burnt~raincomb+rh+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))
m5<-glm(burnt~raincomb+tmax+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))
m6<-glm(burnt~rh+tmax+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))


m7<-glm(burnt~raincomb+rh+tmax, weights = burnwt, family=binomial(link="logit"))
m8<-glm(burnt~raincomb+rh+wind_angle, weights = burnwt, family=binomial(link="logit"))
m9<-glm(burnt~raincomb+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))
m10<-glm(burnt~tmax+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))
m11<-glm(burnt~raincomb+rh+wdir, weights = burnwt, family=binomial(link="logit"))
m12<-glm(burnt~raincomb+tmax+wdir, weights = burnwt, family=binomial(link="logit"))
m13<-glm(burnt~rh+tmax+wdir, weights = burnwt, family=binomial(link="logit"))
m14<-glm(burnt~raincomb+tmax+wind_angle, weights = burnwt, family=binomial(link="logit"))
m15<-glm(burnt~rh+tmax+wind_angle, weights = burnwt, family=binomial(link="logit"))
m16<-glm(burnt~rh+wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))

m17<-glm(burnt~raincomb+rh, weights = burnwt, family=binomial(link="logit"))
m18<-glm(burnt~raincomb+tmax, weights = burnwt, family=binomial(link="logit"))
m19<-glm(burnt~raincomb+wdir, weights = burnwt, family=binomial(link="logit"))
m20<-glm(burnt~raincomb+wind_angle, weights = burnwt, family=binomial(link="logit"))
m21<-glm(burnt~rh+tmax, weights = burnwt, family=binomial(link="logit"))
m22<-glm(burnt~rh+wdir, weights = burnwt, family=binomial(link="logit"))
m23<-glm(burnt~rh+wind_angle, weights = burnwt, family=binomial(link="logit"))
m24<-glm(burnt~tmax+wdir, weights = burnwt, family=binomial(link="logit"))
m25<-glm(burnt~tmax+wind_angle, weights = burnwt, family=binomial(link="logit"))
m26<-glm(burnt~wdir+wind_angle, weights = burnwt, family=binomial(link="logit"))

m27<-glm(burnt~raincomb, weights = burnwt, family=binomial(link="logit"))
m28<-glm(burnt~rh, weights = burnwt, family=binomial(link="logit"))
m29<-glm(burnt~tmax, weights = burnwt, family=binomial(link="logit"))
m30<-glm(burnt~wdir, weights = burnwt, family=binomial(link="logit"))
m31<-glm(burnt~wind_angle, weights = burnwt, family=binomial(link="logit"))

m32<-glm(burnt~1, weights = burnwt, family=binomial(link="logit"))

AIC1<-AIC(m1)
AIC2<-AIC(m2)
AIC3<-AIC(m3)
AIC4<-AIC(m4)
AIC5<-AIC(m5)
AIC6<-AIC(m6)
AIC7<-AIC(m7)
AIC8<-AIC(m8)
AIC9<-AIC(m9)
AIC10<-AIC(m10)
AIC11<-AIC(m11)
AIC12<-AIC(m12)
AIC13<-AIC(m13)
AIC14<-AIC(m14)
AIC15<-AIC(m15)
AIC16<-AIC(m16)
AIC17<-AIC(m17)
AIC18<-AIC(m18)
AIC19<-AIC(m19)
AIC20<-AIC(m20)
AIC21<-AIC(m21)
AIC22<-AIC(m22)
AIC23<-AIC(m23)
AIC24<-AIC(m24)
AIC25<-AIC(m25)
AIC26<-AIC(m26)
AIC27<-AIC(m27)
AIC28<-AIC(m28)
AIC29<-AIC(m29)
AIC30<-AIC(m30)
AIC31<-AIC(m31)
AIC32<-AIC(m32)

AICs<-rbind(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6,AIC7,AIC8,AIC9,AIC10,AIC11,AIC12,AIC13,AIC14,AIC15,AIC16,AIC17,AIC18,AIC19,AIC20,AIC21,AIC22,AIC23,AIC24,AIC25,AIC26,AIC27,AIC28,AIC29,AIC30,AIC31,AIC32)
min<-min(AICs)

dAICs<-AICs-min

dAICs<-matrix(data=dAICs, nrow = 32, ncol = 1)
L.gi.xf<-function (x) exp(x*-0.5)
L.gi.x<-apply(dAICs,1,L.gi.xf)
tot<-sum(L.gi.x)


wi.f<- function(x) ((x)/tot)
wi.l<-lapply(L.gi.x,wi.f)
wi<-as.numeric(wi.l,length=17)
table<-cbind(AICs,dAICs,wi)

colnames(table)<-c("AIC","dAIC","wi")
rownames(table)<-c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32")

write.table(table, "routdec2.txt")


#Set 3: Topo
#Topo: tp, elevation, alt_std,aspect, alt_change, slope

m1<-glm(burnt~   tp+elevation+alt_std+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))

m2<-glm(burnt~  tp+elevation+alt_std+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m3<-glm(burnt~  tp+elevation+alt_std+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m4<-glm(burnt~  tp+elevation+alt_std+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m5<-glm(burnt~  tp+elevation+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m6<-glm(burnt~  tp+alt_std+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m7<-glm(burnt~  elevation+alt_std+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))

m8<-glm(burnt~  tp+elevation+alt_std+aspect, weights = burnwt, family=binomial(link="logit"))
m9<-glm(burnt~  tp+elevation+alt_std+alt_change, weights = burnwt, family=binomial(link="logit"))
m10<-glm(burnt~  tp+elevation+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m11<-glm(burnt~  tp+alt_std+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m12<-glm(burnt~  elevation+alt_std+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m13<-glm(burnt~  tp+elevation+alt_std+slope, weights = burnwt, family=binomial(link="logit"))
m14<-glm(burnt~  tp+elevation+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m15<-glm(burnt~  tp+alt_std+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m16<-glm(burnt~  elevation+alt_std+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m17<-glm(burnt~  tp+elevation+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m18<-glm(burnt~  tp+alt_std+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m19<-glm(burnt~  elevation+alt_std+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m20<-glm(burnt~  tp+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m21<-glm(burnt~  elevation+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m22<-glm(burnt~  alt_std+aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))

m23<-glm(burnt~  tp+elevation+alt_std, weights = burnwt, family=binomial(link="logit"))
m24<-glm(burnt~  tp+elevation+aspect, weights = burnwt, family=binomial(link="logit"))
m25<-glm(burnt~  tp+elevation+alt_change, weights = burnwt, family=binomial(link="logit"))
m26<-glm(burnt~  tp+elevation+slope, weights = burnwt, family=binomial(link="logit"))
m27<-glm(burnt~  tp+alt_std+aspect, weights = burnwt, family=binomial(link="logit"))
m28<-glm(burnt~  tp+alt_std+alt_change, weights = burnwt, family=binomial(link="logit"))
m29<-glm(burnt~  tp+alt_std+slope, weights = burnwt, family=binomial(link="logit"))
m30<-glm(burnt~  tp+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m31<-glm(burnt~  tp+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m32<-glm(burnt~  tp+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m33<-glm(burnt~  elevation+alt_std+aspect, weights = burnwt, family=binomial(link="logit"))
m34<-glm(burnt~  elevation+alt_std+alt_change, weights = burnwt, family=binomial(link="logit"))
m35<-glm(burnt~  elevation+alt_std+slope, weights = burnwt, family=binomial(link="logit"))
m36<-glm(burnt~  elevation+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m37<-glm(burnt~  elevation+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m38<-glm(burnt~  elevation+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m39<-glm(burnt~  alt_std+aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m40<-glm(burnt~  alt_std+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m41<-glm(burnt~  alt_std+alt_change+slope, weights = burnwt, family=binomial(link="logit"))
m42<-glm(burnt~  aspect+alt_change+slope, weights = burnwt, family=binomial(link="logit"))

m43<-glm(burnt~  tp+elevation, weights = burnwt, family=binomial(link="logit"))
m44<-glm(burnt~  tp+alt_std, weights = burnwt, family=binomial(link="logit"))
m45<-glm(burnt~  tp+aspect, weights = burnwt, family=binomial(link="logit"))
m46<-glm(burnt~  tp+alt_change, weights = burnwt, family=binomial(link="logit"))
m47<-glm(burnt~  tp+slope, weights = burnwt, family=binomial(link="logit"))
m48<-glm(burnt~  elevation+alt_std, weights = burnwt, family=binomial(link="logit"))
m49<-glm(burnt~  elevation+aspect, weights = burnwt, family=binomial(link="logit"))
m50<-glm(burnt~  elevation+alt_change, weights = burnwt, family=binomial(link="logit"))
m51<-glm(burnt~  elevation+slope, weights = burnwt, family=binomial(link="logit"))
m52<-glm(burnt~  alt_std+aspect, weights = burnwt, family=binomial(link="logit"))
m53<-glm(burnt~  alt_std+alt_change, weights = burnwt, family=binomial(link="logit"))
m54<-glm(burnt~  alt_std+slope, weights = burnwt, family=binomial(link="logit"))
m55<-glm(burnt~  aspect+alt_change, weights = burnwt, family=binomial(link="logit"))
m56<-glm(burnt~  aspect+slope, weights = burnwt, family=binomial(link="logit"))
m57<-glm(burnt~  alt_change+slope, weights = burnwt, family=binomial(link="logit"))

m58<-glm(burnt~  tp, weights = burnwt, family=binomial(link="logit"))
m59<-glm(burnt~  elevation, weights = burnwt, family=binomial(link="logit"))
m60<-glm(burnt~  alt_std, weights = burnwt, family=binomial(link="logit"))
m61<-glm(burnt~  aspect, weights = burnwt, family=binomial(link="logit"))
m62<-glm(burnt~  alt_change, weights = burnwt, family=binomial(link="logit"))
m63<-glm(burnt~  slope, weights = burnwt, family=binomial(link="logit"))

m64<-glm(burnt~  1, weights = burnwt, family=binomial(link="logit"))

AICc1<-AIC(m1)
AICc2<-AIC(m2)
AICc3<-AIC(m3)
AICc4<-AIC(m4)
AICc5<-AIC(m5)
AICc6<-AIC(m6)
AICc7<-AIC(m7)
AICc8<-AIC(m8)
AICc9<-AIC(m9)
AICc10<-AIC(m10)
AICc11<-AIC(m11)
AICc12<-AIC(m12)
AICc13<-AIC(m13)
AICc14<-AIC(m14)
AICc15<-AIC(m15)
AICc16<-AIC(m16)
AICc17<-AIC(m17)
AICc18<-AIC(m18)
AICc19<-AIC(m19)
AICc20<-AIC(m20)
AICc21<-AIC(m21)
AICc22<-AIC(m22)
AICc23<-AIC(m23)
AICc24<-AIC(m24)
AICc25<-AIC(m25)
AICc26<-AIC(m26)
AICc27<-AIC(m27)
AICc28<-AIC(m28)
AICc29<-AIC(m29)
AICc30<-AIC(m30)
AICc31<-AIC(m31)
AICc32<-AIC(m32)
AICc33<-AIC(m33)
AICc34<-AIC(m34)
AICc35<-AIC(m35)
AICc36<-AIC(m36)
AICc37<-AIC(m37)
AICc38<-AIC(m38)
AICc39<-AIC(m39)
AICc40<-AIC(m40)
AICc41<-AIC(m41)
AICc42<-AIC(m42)
AICc43<-AIC(m43)
AICc44<-AIC(m44)
AICc45<-AIC(m45)
AICc46<-AIC(m46)
AICc47<-AIC(m47)
AICc48<-AIC(m48)
AICc49<-AIC(m49)
AICc50<-AIC(m50)
AICc51<-AIC(m51)
AICc52<-AIC(m52)
AICc53<-AIC(m53)
AICc54<-AIC(m54)
AICc55<-AIC(m55)
AICc56<-AIC(m56)
AICc57<-AIC(m57)
AICc58<-AIC(m58)
AICc59<-AIC(m59)
AICc60<-AIC(m60)
AICc61<-AIC(m61)
AICc62<-AIC(m62)
AICc63<-AIC(m63)
AICc64<-AIC(m64)



AICcs<-rbind(AICc1,AICc2,AICc3,AICc4,AICc5,AICc6,AICc7,AICc8,AICc9,AICc10,AICc11,AICc12,AICc13,AICc14,AICc15,AICc16,AICc17,AICc18,AICc19,AICc20,AICc21,AICc22,AICc23,AICc24,AICc25,AICc26,AICc27,AICc28,AICc29,AICc30,AICc31,AICc32,AICc33,AICc34,AICc35,AICc36,AICc37,AICc38,AICc39,AICc40,AICc41,AICc42,AICc43,AICc44,AICc45,AICc46,AICc47,AICc48,AICc49,AICc50,AICc51,AICc52,AICc53,AICc54,AICc55,AICc56,AICc57,AICc58,AICc59,AICc60,AICc61,AICc62,AICc63,AICc64)
min<-min(AICcs)

dAICcs<-AICcs-min

dAICcs<-matrix(data=dAICcs, nrow = 64, ncol = 1) 
L.gi.xf<-function (x) exp(x*-0.5)
L.gi.x<-apply(dAICcs,1,L.gi.xf)
tot<-sum(L.gi.x)

library(nlme)

wi.f<- function(x) ((x)/tot)
wi.l<-lapply(L.gi.x,wi.f)
wi<-as.numeric(wi.l,length=17)
table<-cbind(AICcs,dAICcs,wi)

colnames(table)<-c("AICc","dAICc","wi")
rownames(table)<-c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32","m33","m34","m35","m36","m37","m38","m39","m40","m41","m42","m43","m44","m45","m46","m47","m48","m49","m50","m51","m52","m53","m54","m55","m56","m57","m58","m59","m60","m61","m62","m63","m64")
write.table(table, "routdec3.txt")


#Set 4: Fuel: Forestp, vegpc,sparse, tsf, fuelload
m1<-glm(burnt~forestp+vegpc+sparse+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))

m2<-glm(burnt~forestp+vegpc+sparse+tsf, weights = burnwt, family=binomial(link="logit"))
m3<-glm(burnt~forestp+vegpc+sparse+fuelload, weights = burnwt, family=binomial(link="logit"))
m4<-glm(burnt~forestp+vegpc+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))
m5<-glm(burnt~forestp+sparse+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))
m6<-glm(burnt~vegpc+sparse+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))


m7<-glm(burnt~forestp+vegpc+sparse, weights = burnwt, family=binomial(link="logit"))
m8<-glm(burnt~forestp+vegpc+fuelload, weights = burnwt, family=binomial(link="logit"))
m9<-glm(burnt~forestp+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))
m10<-glm(burnt~sparse+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))
m11<-glm(burnt~forestp+vegpc+tsf, weights = burnwt, family=binomial(link="logit"))
m12<-glm(burnt~forestp+sparse+tsf, weights = burnwt, family=binomial(link="logit"))
m13<-glm(burnt~vegpc+sparse+tsf, weights = burnwt, family=binomial(link="logit"))
m14<-glm(burnt~forestp+sparse+fuelload, weights = burnwt, family=binomial(link="logit"))
m15<-glm(burnt~vegpc+sparse+fuelload, weights = burnwt, family=binomial(link="logit"))
m16<-glm(burnt~vegpc+tsf+fuelload, weights = burnwt, family=binomial(link="logit"))

m17<-glm(burnt~forestp+vegpc, weights = burnwt, family=binomial(link="logit"))
m18<-glm(burnt~forestp+sparse, weights = burnwt, family=binomial(link="logit"))
m19<-glm(burnt~forestp+tsf, weights = burnwt, family=binomial(link="logit"))
m20<-glm(burnt~forestp+fuelload, weights = burnwt, family=binomial(link="logit"))
m21<-glm(burnt~vegpc+sparse, weights = burnwt, family=binomial(link="logit"))
m22<-glm(burnt~vegpc+tsf, weights = burnwt, family=binomial(link="logit"))
m23<-glm(burnt~vegpc+fuelload, weights = burnwt, family=binomial(link="logit"))
m24<-glm(burnt~sparse+tsf, weights = burnwt, family=binomial(link="logit"))
m25<-glm(burnt~sparse+fuelload, weights = burnwt, family=binomial(link="logit"))
m26<-glm(burnt~tsf+fuelload, weights = burnwt, family=binomial(link="logit"))

m27<-glm(burnt~forestp, weights = burnwt, family=binomial(link="logit"))
m28<-glm(burnt~vegpc, weights = burnwt, family=binomial(link="logit"))
m29<-glm(burnt~sparse, weights = burnwt, family=binomial(link="logit"))
m30<-glm(burnt~tsf, weights = burnwt, family=binomial(link="logit"))
m31<-glm(burnt~fuelload, weights = burnwt, family=binomial(link="logit"))

m32<-glm(burnt~1, weights = burnwt, family=binomial(link="logit"))

AIC1<-AIC(m1)
AIC2<-AIC(m2)
AIC3<-AIC(m3)
AIC4<-AIC(m4)
AIC5<-AIC(m5)
AIC6<-AIC(m6)
AIC7<-AIC(m7)
AIC8<-AIC(m8)
AIC9<-AIC(m9)
AIC10<-AIC(m10)
AIC11<-AIC(m11)
AIC12<-AIC(m12)
AIC13<-AIC(m13)
AIC14<-AIC(m14)
AIC15<-AIC(m15)
AIC16<-AIC(m16)
AIC17<-AIC(m17)
AIC18<-AIC(m18)
AIC19<-AIC(m19)
AIC20<-AIC(m20)
AIC21<-AIC(m21)
AIC22<-AIC(m22)
AIC23<-AIC(m23)
AIC24<-AIC(m24)
AIC25<-AIC(m25)
AIC26<-AIC(m26)
AIC27<-AIC(m27)
AIC28<-AIC(m28)
AIC29<-AIC(m29)
AIC30<-AIC(m30)
AIC31<-AIC(m31)
AIC32<-AIC(m32)

AICs<-rbind(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6,AIC7,AIC8,AIC9,AIC10,AIC11,AIC12,AIC13,AIC14,AIC15,AIC16,AIC17,AIC18,AIC19,AIC20,AIC21,AIC22,AIC23,AIC24,AIC25,AIC26,AIC27,AIC28,AIC29,AIC30,AIC31,AIC32)
min<-min(AICs)

dAICs<-AICs-min

dAICs<-matrix(data=dAICs, nrow = 32, ncol = 1)
L.gi.xf<-function (x) exp(x*-0.5)
L.gi.x<-apply(dAICs,1,L.gi.xf)
tot<-sum(L.gi.x)


wi.f<- function(x) ((x)/tot)
wi.l<-lapply(L.gi.x,wi.f)
wi<-as.numeric(wi.l,length=17)
table<-cbind(AICs,dAICs,wi)

colnames(table)<-c("AIC","dAIC","wi")
rownames(table)<-c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32")

write.table(table, "routdec4.txt")


#final models

m1<-glm(burnt~forestp+sparse+tsf+logdist++ffdi_df1+kbdi+tp+elevation+alt_std+aspect+slope, weights = burnwt, family=binomial(link="logit"))
m2<-step(m1)
m2<-glm(burnt~forestp*logdist+tsf+ffdi_df1+kbdi, weights = burnwt, family=binomial(link="logit"))
#by manually eliminating for
m3<-glm(burnt~forestp+tsf+logdist+ffdi_df1+kbdi+elevation, weights = burnwt, family=binomial(link="logit"))

1 - exp(-2/10265*(logLik(m1)  -  logLik(m0))) [1]


Intreractions
m3<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi, weights = burnwt, family=binomial(link="logit"))


windows(4,4)
library(gstat)
coordinates(data)=~final_x94+final_y94
vario<-variogram(burnt ~final_x94+final_y94,data=data)
vario_fit<-fit.variogram(vario,vgm(10,"Sph",40000,0))
plot(vario, col=1,pch=19)
plot(vario,model=vario_fit,col=1,pch=19, xlab="Distance (m)")

library(ape)
distmat<-1/(as.matrix(1+dist(cbind(final_x94/1000,final_y94/1000), diag=T, upper=T)))
diag(distmat)=0
MI1<-Moran.I(burnt,distmat,scaled=TRUE)


Mdata<-read.csv("moran_test.csv")
distmat<-1/(as.matrix(1+dist(cbind(Mdata$final_x94/1000,Mdata$final_y94/1000), diag=T, upper=T)))
diag(distmat)=0
MI1<-Moran.I(Mdata$burnt,distmat,scaled=TRUE)

m1<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi+westwind, weights = burnwt, family=binomial(link="logit"))

windows(4,4)
#plot for distance and ffdi
preddat1<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(0.28,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(6,51),"westwind"=rep(FALSE,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
#preddat2<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(4.74,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(130,51),"westwind"=rep(FALSE,51))
preddat2<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(6.5,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(130,51),"westwind"=rep(FALSE,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
plot(c(0,8000),c(0,1),cex=0,xlab="Distance (m)",ylab="P (burn)")
lines(exp(seq(0,10,by=0.2)), out1$fit,lwd=2,lty=1)
lines(exp(seq(0,10,by=0.2)), out2$fit,lwd=2,lty=8)
legend(4000,1,legend=c("Fire Weather","Low","Severe"),lty=c(0,1,8),col=c(0,1,1),lwd=2,cex=0.8)

 #plot for distance and tsf
preddat1<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(49,51),"tsf"=rep(1,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
preddat2<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(49,51),"tsf"=rep(20,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
plot(c(0,8000),c(0,1),cex=0,xlab="Distance (m)",ylab="P (burn)")
lines(exp(seq(0,10,by=0.2)), out1$fit,lwd=2,lty=1)
lines(exp(seq(0,10,by=0.2)), out2$fit,lwd=2,lty=8)
legend(5000,1,legend=c("TSF","1","20"),lty=c(0,1,8),col=c(0,1,1),lwd=2,cex=0.8)


#plot for distance and forestp
preddat1<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(11,51),"tsf"=rep(10,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
preddat2<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
preddat3<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(84,51),"tsf"=rep(10,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out3<-predict(m1, preddat3, "response", se.fit=T)
plot(c(0,8000),c(0,1),cex=0,xlab="Distance (m)",ylab="P (burn)")
lines(exp(seq(0,10,by=0.2)), out1$fit,lwd=2,lty=1)
lines(exp(seq(0,10,by=0.2)), out2$fit,lwd=2,lty=8)
lines(exp(seq(0,10,by=0.2)), out3$fit,lwd=2,lty=9)
legend(5000,1,legend=c("% Forest","11%","49%","84%"),lty=c(0,1,8,9),col=c(0,1,1,1),lwd=2,cex=0.8)

#plot for distance and westwind
preddat1<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(60,51),"westwind"=rep(FALSE,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
preddat2<-data.frame("logdist"=seq(0,10,by=0.2),"ffdi_df1"=rep(1.3,51),"forestp"=rep(49,51),"tsf"=rep(10,51),"kbdi"=rep(60,51),"westwind"=rep(TRUE,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
plot(c(0,8000),c(0,1),cex=0,xlab="Distance (m)",ylab="P (burn)")
lines(exp(seq(0,10,by=0.2)), out1$fit,lwd=2,lty=1)
lines(exp(seq(0,10,by=0.2)), out2$fit,lwd=2,lty=8)
legend(5300,1,legend=c("Wind","N","Y"),lty=c(0,1,8),col=c(0,1,1),lwd=2,cex=0.8); text(6100,0.92,"West",cex=0.8)





#plot for forestp and tsf
preddat1<-data.frame("logdist"=rep(8.3,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(1,51),"kbdi"=rep(60,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
preddat2<-data.frame("logdist"=rep(8.3,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(20,51),"kbdi"=rep(60,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
preddat3<-data.frame("logdist"=rep(6.9,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(1,51),"kbdi"=rep(60,51))
out3<-predict(m1, preddat3, "response", se.fit=T)
preddat4<-data.frame("logdist"=rep(6.9,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(20,51),"kbdi"=rep(60,51))
out4<-predict(m1, preddat4, "response", se.fit=T)
plot(c(0,100),c(0,1),cex=0,xlab="Forest Proportion",ylab="P (burn)")
lines(seq(0,100,by=2), out1$fit,lwd=2,lty=1)
lines(seq(0,100,by=2), out2$fit,lwd=2,lty=8)
lines(seq(0,100,by=2), out3$fit,lwd=2,lty=1,col=8)
lines(seq(0,100,by=2), out4$fit,lwd=2,lty=8,col=8)
legend(15,0.6,legend=c("Distance","1/4000","20/4000","1/1000","20/1000"),lty=c(0,1,8,1,8),col=c(0,1,1,8,8),lwd=2,cex=0.8)
text(25,0.52, "TSF /",cex=0.8)


#plot for forestp and logdist
preddat1<-data.frame("logdist"=rep(6.91,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(10,51),"kbdi"=rep(60,51))
out1<-predict(m1, preddat1, "response", se.fit=T)
preddat2<-data.frame("logdist"=rep(7.6,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(10,51),"kbdi"=rep(60,51))
out2<-predict(m1, preddat2, "response", se.fit=T)
preddat3<-data.frame("logdist"=rep(8.29,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(10,51),"kbdi"=rep(60,51))
out3<-predict(m1, preddat3, "response", se.fit=T)
preddat4<-data.frame("logdist"=rep(8.99,51),"ffdi_df1"=rep(1.3,51),"forestp"=seq(0,100,by = 2),"tsf"=rep(10,51),"kbdi"=rep(60,51))
out4<-predict(m1, preddat4, "response", se.fit=T)
plot(c(0,100),c(0,1),cex=0,xlab="Forest Proportion",ylab="P (burn)")
lines(seq(0,100,by=2), out1$fit,lwd=2,lty=1)
lines(seq(0,100,by=2), out2$fit,lwd=2,lty=8)
lines(seq(0,100,by=2), out3$fit,lwd=2,lty=1,col=8)
lines(seq(0,100,by=2), out4$fit,lwd=2,lty=8,col=8)
legend(0,0.6,legend=c("Distance","1000","2000","4000","8000"),lty=c(0,1,8,1,8),col=c(0,1,1,8,8),lwd=2,cex=0.8)


#heirarchical partitioning
library(gtools)
library(hier.part)
varlist<-data.frame(ffdi_df1,kbdi,logdist,forestp,tsf)
hp<-hier.part(burnt,varlist,family="binomial")

#Model accuracy
testdat<-read.csv("ult_testdat.csv")
attach(testdat)
logdist<-log(distance)
westwind<-windclass8=="W"
burnwt<-ifelse(burnt==1,1,0.044)
m1<-glm(burnt~forestp+tsf+logdist*forestp+ffdi_df1+kbdi+westwind, weights = burnwt, family=binomial(link="logit"))
preddat<-predict(m1,testdat,"response",se.fit=T)
outdat<-data.frame(testdat$objectid_1,preddat)
write.csv(outdat,"predicts.csv")

#need to swap to old version of r for this
library(gplots)
library(caTools)
library(ROCR)
library(bitops)

rocdata<-prediction(preddat$fit,burnt)
roc<-performance(rocdata,"tpr","fpr")
rocdata_cs<-prediction(pred_cs,cs)
roc_cs<-performance(rocdata_cs, "tpr","fpr")

plot(roc_cc,lwd=2)
lines(c(0,1),c(0,1),lty=9)
plot(roc_cs,lwd=2)
lines(c(0,1),c(0,1),lty=9)



setwd("c:/wollongong/newgis/risk")
data<-read.csv("risk_anal.csv")
attach(data)

mixdat<-subset(data,radeloff=="Intermix")
facedat<-subset(data,radeloff=="Interface")
