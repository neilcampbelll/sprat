# Clear data and graphics windows
rm(list=ls(pos=1))
graphics.off()
set.seed(1) 

## Sprat SWC survey index - ALK and Index final model from IBP6a7bcHer 2019
## Updated for 2019 indeces
## T. Buch
## Feb 2020
## Updated for 2020 indeces
## N. Campbell
## Mar 2021

## Run index models base on Berg et al. 2014
## output: ALK plots, Index, Index/model plots

## Set WD ======================================================================
setwd("C:/Work/WoS_Sprat/")


## libraries ===================================================================
# Install packages if needed
#library(devtools)
devtools::install_github("DTUAqua/DATRAS/DATRAS")
library(DATRAS)
remotes::install_github("casperwberg/surveyIndex/surveyIndex")

# load libraries
library(DATRAS)
library(surveyIndex)
library(mgcv)
library(plyr)
library(maps)
library(mapdata)
library(reshape2)
library(mapplots)
library(sp)
library(rgdal)


## Quarter 1 or 4
q<-"1"

## if retro run
RETRO<-FALSE

## Settings ====================================================================
## for each quarter
if(q=="1"){
  
  ## Data subset
  #  YEARS<-1994:2020
  
  YEARS<-2009:2020
  AREA<-c("VIa")
  QUARTER<-q
  DEPTH<-201       ## keep hauls below in border region
  
  # ALK
  ALKages<-1:3      ## ages to include in ALK
  ALKmodel<- 3      ## 3 = spatial ALK, 1= non-spatial ALK
  
  ## index model
  GRID<-30          ## recommened btw 20-40
  INDEXages<-0:4    ## ages to include in index, if same as ALK then plus group included
  CUTOFF <- 0.1     ## based on results from IBP
  
  RYEARS<-1994:2012 ## shortest timeseries - to be used for grid if retro run
  
}
if(q=="4"){
  
  ## Data subset
  YEARS<-1996:2020
  AREA<-c("VIa")
  QUARTER<-q
  DEPTH<-201        ## keep hauls below in border region
  
  # ALK
  ALKages<-0:4      ## ages to include in ALK
  ALKmodel<- 3      ## 3 = spatial ALK, 1= non-spatial ALK
  
  ## index model
  GRID<-30          ## recommened btw 20-40
  INDEXages<-0:4    ## ages to include in index, if same as ALK then plus group included
  CUTOFF <- 0.01    ## based on results from IBP
  
  RYEARS<-1996:2009 ## shortest timeseries - to be used for grid
  
}

## for both quarters
## output plots
pdf.plots<-TRUE
PNG<-FALSE

## File names
t.dataset<-"SWC" ## for output file names
t.yr<-paste(min(YEARS),"-",max(YEARS),sep="") ## assumed same for ALK and INDEX
t.ALKages<-paste("ages", min(ALKages),"-",max(ALKages),"+",sep="")
t.INDEXages<-paste("ages", min(INDEXages),"-",max(INDEXages),sep="")
t.add<-"" ## additional text to add to outputfiles

t.ALKoutput<-paste(t.dataset,"_ALK","_Q",QUARTER,"_",t.yr,"_",t.ALKages,sep="")
t.INDEXoutput<-paste(t.dataset,"_Idx","_Q",QUARTER,"_",t.yr,"_",t.INDEXages,sep="")


## load data, combine and add ICES areas========================================
#load survey data. Dowloaded from DATRAS 03.10.18

## SWC-IBTS for years 1985-2010
# Data from SWC-IBTS downloaded 01.03.2021
#WoS<-readExchange("data/Exchange_SWC_IBTS_Mar21",strict=TRUE)
WoS<-readExchange("data/Survey/Exchange_SWC_IBTS_Mar21.zip",strict=TRUE) ## use this format if reading in zip

## SCOWCGFS for years 2011-2020
# Data from SWC-IBTS downloaded 01.03.2021
WoS1<-readExchange("data/Survey/Exchange_SCO_GFS_Mar21_3.zip", strict = TRUE)

## combine two datasets, ekstra columns removed:
# in WoS2  one ekstra columns "Stratum", 
# in WoS three ekstra columsn "WgtGroundRope" "SpeedWater" "Roundfish"
SWC_d<-c(WoS,WoS1) # combine two datasets, ekstra columns removed: 
## change NoAtLngt to NoAtALK, due to changes in col names in exchange file. 
names(SWC_d[["CA"]])[names(SWC_d[["CA"]])=="NoAtLngt"] <-"NoAtALK"


## updated addSpatialData function #############################################
## updated november 2019
addSpatialData_update<-function (d, shape, select = NULL, ...) 
{
  require(rgdal)
  if (is.character(shape)) {
    if (file.exists(shape)) {
      #shape <- readShapeSpatial(shape)  ## from DATRAS library
      shape <- readOGR(dsn=shape,stringsAsFactors = FALSE) ## new code, replace readShapeSpatial
      
    }
  }
  i <- complete.cases(d[[2]][c("lon", "lat")])   ## index of complete lon/lat pairs
  tmp <- d[[2]][i, , drop = FALSE]               ## keep only cases with both lon and lat
  coordinates(tmp) <- ~lon + lat                 ## create SpatialPointsDataFrame
  proj4string(tmp)<-proj4string(shape)           ## new code, add proj4string from shape (needed to allow next step)
  xtra <- over(tmp, shape)                       ## extra columns (ICES_area and Area_km2)
  if (any(!i)) {                                 ## what to do for incomplete lon/lat pairs
    warning(paste(sum(!i), "incomplete lon/lat pairs will get NA as spatial data."))
    j <- 1:nrow(d[[2]])
    j[i] <- 1:sum(i)
    j[!i] <- NA
    xtra <- xtra[j, , drop = FALSE]
  }
  if (!is.null(select)) 
    xtra <- xtra[select]
  nm <- names(xtra)
  if (length(intnm <- intersect(names(d[[2]]), nm))) {
    print(intnm)
    stop("Some selected names from shapefile are already in DATRASraw")
  }
  d[[2]] <- cbind(d[[2]], xtra)
  d
}
########################################################################################

SWC_s<-addSpatialData_update(SWC_d,"data/ICES_areas/ICES_areas.shp")

## set up clyde ================================================================ 

clyde<-c("39E4","39E5","40E4","40E5")

## subset data =================================================================
Q<-QUARTER
if(QUARTER=="1"){Q<-c("1","2")} ## for 95 in Q1 some data in Q2


SWC_d<-subset(SWC_d,
              Species=="Sprattus sprattus",
              HaulVal=="V",
              !is.na(lon),
#              ICES_area %in% as.character(c(AREA)),
              StatRec %in% c(paste(39:43, "E2", sep=""), paste(39:45, "E3", sep=""), paste(41:46, "E4", sep="")),
              lon<(-4),
              Quarter %in% Q,
              Year %in% YEARS)


## remove clyde hauls in Clyde area
#SWC_d<-subset(SWC_d, (!StatRec %in% clyde) ) ## remove hauls in clyde

## hauls to remove based on datachecks:
if(QUARTER==1){
  if("1993" %in% YEARS){SWC_d<-subset(SWC_d, haul.id!="1993:1:SCO:SCO2:GOV:70:17")} ## etiher depth or location wrong
}

if (RETRO){
  dGRID<-subset(SWC_d, Year %in% c(RYEARS) )
}

## final dataset for quarter
SWCQ<-addSpectrum(SWC_d)

## data plots and  - not completed==============================================
## number of hauls used

summary(SWC_d) ## to check number of hauls - look back on previous years


## ALK =========================================================================
## check age data by year available for ALK ============================================

SWCQ_ya<-xtabs(NoAtALK~Year+Age,data=SWCQ[[1]])
## save table
write.csv(SWCQ_ya,file=paste(t.ALKoutput,"_ALKdata.csv",sep=""))
SWCQ_ya.df<-as.data.frame(SWCQ_ya)

for(a in ALKages){   ## check if data for each year and age
  if(any(SWCQ_ya.df$Freq[SWCQ_ya.df$Age==a]<1))
    SWCQ<-fixAgeGroup(SWCQ,age=a,fun=ifelse(a==min(ALKages),"min","mean"))
}
SWCQ<-subset(SWCQ,ages>=min(ages)) ## only if no data

## split DATRAS data into yrs ====================================================
SWCQ.ysplit<- split(SWCQ, SWCQ$Year)

## settings for ALK ============================================================

if(ALKmodel==1){ALK<-"oneALK"
ACK<-FALSE}
if(ALKmodel==3){ALK<-"spALK"
ACK<-TRUE}
gammas=NA
ack=ACK
useBICs=TRUE
varCofs=FALSE
maxKs=49

alkQ <- lapply(SWCQ.ysplit,
               fitALK,
               minAge=min(ALKages),
               maxAge=max(ALKages),
               method=ALKmodel,
               gamma=gammas,
               autoChooseK=ack,
               useBIC=useBICs,
               varCof=varCofs,
               maxK=maxKs)


plot(SWC_d)

## Numbers-at-age ==============================================================
Nage.Q<-lapply(alkQ,predict, type="Nage")
## at Nage to split dataset
for(i in 1:length(alkQ)) SWCQ.ysplit[[i]]$Nage=Nage.Q[[i]]
## combine split dataset into one
SWC.d<-do.call("c",SWCQ.ysplit)
ALKages


## ALK plots ===================================================================
if(pdf.plots) {
  pdf(file = paste(t.ALKoutput,"_",ALK,".pdf",sep=""), onefile = TRUE)
  par(mfrow = c(3,1),
      oma = c(3,3,1,1) + 0.1,
      mar = c(2,2,0,0) + 0.5, ann=F)
  #par(mfrow=c(1,1))
  for(i in c(YEARS)){
    y<-as.character(i)
    if(y %in% c(names(SWCQ.ysplit))){
      for(h in c(1:length(alkQ[[y]][[1]][["coefficients"]]))){
        ADD<-FALSE
        if(h %in% 2:length(alkQ[[y]][[1]][["coefficients"]])){ADD<-TRUE} ## plot on same plot 
        plotALKfit(alkQ[[y]],h,add=ADD,col=c(1:length(ALKages)),lty=c(1:length(ALKages)),xlab="length (cm)",ylab="Proportion")
        if (!ADD){mtext(paste(y," ",length(alkQ[[y]][[1]][["coefficients"]])," hauls",sep=""),cex=0.8)}
        legend("bottomleft",as.character(ALKages),col=c(1:length(ALKages)),lty=c(1:length(ALKages)),bty="n",inset=0.025,cex=1)
      }
    }
  }
  dev.off()
}
ALK.Q<-lapply(alkQ,predict,type="ALK")


## Index: Delta-lognormal model ================================================

## Model settings ==============================================================
if(RETRO){grid<-getGrid(dGRID,nLon=GRID)}  ## recommened btw 20-40
if(!RETRO){grid<-getGrid(SWC.d,nLon=GRID)}  ## recommened btw 20-40

## set max basis dim for spatial smooths by age, P=positive and Z=zero/presence.
kvP <- c(50,50,50,40,30,rep(10,length(INDEXages)-5))
kvZ <- kvP / 2;

## Set up model ==========================================================

## zero/presence part of model
mZ <- rep("Year+s(lon,lat,k=kvecZ[a],bs='ts')+s(Depth,bs='ts',k=6)+s(TimeShotHour,bs='cc')+offset(log(HaulDur))",length(INDEXages))

## Positive part of the model
mP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts')+s(Depth,bs='ts',k=6)+s(TimeShotHour,bs='cc')+offset(log(HaulDur))",length(INDEXages))

SI <- getSurveyIdx(SWC.d,
                   ages=INDEXages,
                   myids=grid[[3]],
                   cutOff=CUTOFF,
                   kvecP=kvP,
                   kvecZ=kvZ,
                   modelZ=mZ,
                   modelP=mP,
                   mc.cores=1,
                   fam=c("LogNormal")
)

## SI model plots =======================================================================

## plot indices, distribution map, and estimated depth effects
if (pdf.plots) pdf(file = paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,".pdf",sep=""), onefile = TRUE)
# par(mfrow = c(4,3),
#     oma = c(1,1,1,1) + 0.1,
#     mar = c(1,1,1,1) + 0.2, ann=F)
mfrow=c(4,3)

PLOTS<- c("index","map","1","2","3","residuals","fitVsRes") ## standard plotting options for surveyIdxPlots()
for(p in PLOTS){
  surveyIdxPlots(SI,SWC.d,
                 cols=1:length(SI$pModels),
                 alt.idx=NULL,
                 grid[[3]],
                 par=list(mfrow=c(4,3)),
                 legend=FALSE,
                 colors=rev(heat.colors(8)),
                 select=p,
                 plotByAge=FALSE)
}

IC<-internalCons(SI$idx, do.plot=F)
par(mfrow=c(1,1))
if(QUARTER == "1") {xaxis<-c("1vs2","2vs3","3vs4","4vs5","5vs6","6vs7","7vs8","8vs9")}
if(QUARTER == "4") {xaxis<-c("0vs1","1vs2","2vs3","3vs4","4vs5","5vs6","6vs7","7vs8","8vs9")}

plot(IC,type="l",ylab="Internal consistency",ylim=c(min(IC),max(IC)),xaxt="n")
axis(1,1:length(IC),xaxis)
dev.off()


if(PNG) {
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_maps.png",sep=""), width=2480, height=3508,res=300) 
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,colors=rev(heat.colors(8)),select="map",plotByAge=FALSE)
  dev.off()
  
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_Spline1.png",sep=""), width=2480, height=3508,res=300) 
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,select="1",plotByAge=FALSE)
  dev.off()
  
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_residuals.png",sep=""), width=2480, height=3508,res=300) 
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,select="residuals",plotByAge=FALSE)
  dev.off()
  
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_fittedvsres.png",sep=""), width=2480, height=3508,res=300) 
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,select="fitVsRes",plotByAge=FALSE)
  dev.off()
  
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_Spline2.png",sep=""), width=2480, height=3508,res=300 )
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,select="2",plotByAge=FALSE)
  dev.off()
  
  png(filename=paste(t.INDEXoutput,"_SI_",ALK,"_G",GRID,t.add,"_Spline3.png",sep=""), width=2480, height=3508,res=300) 
  surveyIdxPlots(SI,SWC.d,cols=1:length(SI$pModels),alt.idx=NULL,grid[[3]],par=list(mfrow=c(4,3)),legend=FALSE,select="3",plotByAge=FALSE)
  dev.off()
} 


if(PNG) {png(filename=paste(t.INDEXoutput,t.add,"_IC.png",sep=""), width=3508, height=2480,res=300) 
  par(mfrow=c(1,1))
  if(QUARTER == "1") {xaxis<-c("1vs2","2vs3","3vs4","4vs5","5vs6","6vs7","7vs8","8vs9")}
  if(QUARTER == "4") {xaxis<-c("0vs1","1vs2","2vs3","3vs4","4vs5","5vs6","6vs7","7vs8","8vs9")}
  plot(IC,type="l",ylab="Internal consistency",ylim=c(min(IC),max(IC)),xaxt="n")
  axis(1,1:length(IC),xaxis)
  dev.off()  
}


dev.off()

## Index csv and plot ==========================================================

idx<-SI[["idx"]]
rownames(idx)<-YEARS
colnames(idx)<-INDEXages

if(QUARTER=="4"){
  if("2013" %in% YEARS) {idx[c(18),]<-NA}
  if("2010" %in% YEARS){ idx[c(15),]<-NA }
}

write.csv(idx, file=paste(t.INDEXoutput,t.add,"SI.idx",".csv",sep=""))
write.csv(SI[["up"]], file=paste(t.INDEXoutput,t.add,"SI.upper",".csv",sep=""))  
write.csv(SI[["lo"]], file=paste(t.INDEXoutput,t.add,"SI.lower",".csv",sep="")) 

#  ,row.names=YEARS,col.names=as.character(INDEXages

## plot indices
## pdf
if (pdf.plots) pdf(file = paste(t.INDEXoutput,t.add,"SI.plot",".pdf",sep=""), onefile = T,paper = "a4r", width = 0, height = 0)

## bubble plots
x<-setNames(melt(idx),c('years','ages','idx'))

xlim<-c(min(YEARS),max(YEARS))
ylim<-c(min(x$ages),max(x$ages))
basemap(xlim,ylim,xlab="year",ylab="age (wr)",bg="white",main=paste("Quarter_",QUARTER,sep=""))
draw.bubble(x$years,x$ages,x$idx,maxradius=0.6,pch=21,bg="#00FF0050")

dev.off()
## png
if(PNG) {png(filename=paste(t.INDEXoutput,t.add,"SI.plot",".png",sep=""), width=3508, height=2480,res=300) 
  basemap(xlim,ylim,xlab="year",ylab="age (wr)",bg="white",main=paste("Quarter_",QUARTER,sep=""))
  draw.bubble(x$years,x$ages,x$idx,maxradius=0.6,pch=21,bg="#00FF0050")
  
  dev.off()
}


