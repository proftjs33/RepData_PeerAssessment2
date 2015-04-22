setwd("~/GitHub/RepData_PeerAssessment2")
# download the Storm Data file
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data/StormData.csv.bz2")

# download the documentation of the Storm Data file
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf","data/StormDataDoc.pdf")

# download the FAQ file
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf", "data/StormEvents-FAQ.pdf")

# read the data into R
data<-read.csv("data/StormData.csv")

# http://thmp.info/metadata/other-storms/Other%20Storm%20Events%201955-2003.html
# Attribute:
#     Attribute_Label: INJURIES
# Attribute:
#     Attribute_Label: INJURIES
# Attribute:
#     Attribute_Label: PROPDMG
#   Attribute_Definition:
#     Alias Label: Property Damage
# Attribute:
#     Attribute_Label: PROPDMGEXP
#   Attribute_Definition:
#     Alias Label: Prop. Damage Exp.; expressed in dollar unit type.
# Attribute:
#     Attribute_Label: CropDMG
#   Attribute_Definition:
#     Alias Label: Crop Damage
# Attribute:
#     Attribute_Label: CropDMGEXP
#   Attribute_Definition:
#     Alias Label: Crop Damage Exp.; expressed in dollar unit type.

# 1. Use 1986-1995 (10 years) data
require(lubridate)
# data10<-data[year(as.Date(data$BGN_DATE,"%m/%d/%y"))>1985,]
# getYears<-function(myData){
#     n<-length(myData)
#     myYear<-year(as.Date(myData[1],"%m/%d/%Y"))
#     years<-c(myYear)
#     for(i in 2:n){
#         myYear<-year(as.Date(myData[i],"%m/%d/%Y"))
#         years<-c(years,myYear)
#     }
#     years
# }
# year<-getYears(data$BGN_DATE)
allYears<-year(as.Date(data$BGN_DATE,"%m/%d/%Y"))
summary(allYears)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1950    1995    2002    1999    2007    2011 
# x<-allYears>1985
# data10<-data[x,]
# write.csv(data10,file="data/data10.csv",row.names=F)
keep<-c("BGN_DATE","COUNTYNAME", "STATE","EVTYPE","INJURIES","INJURIES","PROPDMG","PROPDMGEXP","CropDMG","CropDMGEXP")
data10_keep<-data10[,keep]
write.csv(data10_keep,file="data/data10_keep.csv", row.names=F)
data10_keep<-read.csv("data/data10_keep.csv")
sum(data10_keep$PROPDMGEXP==0)
data10_no.NA<-na.omit(data10_keep)
data10_keep$BGN_DATE<-as.Date(data10_keep$BGN_DATE, "%m/%d/%Y")
data10_keep$year<-year(data10_keep$BGN_DATE)
data10_keep[data10_keep$PROPDMG == "1.2",]
data10_keep[data10_keep$PROPDMGEXP == "4",]
#######################################################
data_years<-data10_keep[data10_keep$year > 2005,]
levels(data_years$PROPDMGEXP)
#[1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K" "m" "M"
levels(data_years$CropDMGEXP)
#[1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
head(data_years$PROPDMGEXP)
# [1]       K K  
# Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
head(data_years$CROPDMGEXP)
# [1]      
# Levels:  ? 0 2 B k K m M
######################################################
require(plyr)
replaceValues<-c("h"="100","H"="100","k"="1000","K"="1000","m"="1000000","M"="1000000","B"="1000000000","+"="1","-"="1","?"="1")
replaceValues<-c(replaceValues,"0"="1","1"="10","2"="100","3"="1000","4"="10000","5"="100000","6"="1000000","7"="10000000","8"="100000000")
data_years$PROPDMGEXP<-as.character(revalue(data_years$PROPDMGEXP, replaceValues))
data_years$CROPDMGEXP<-as.character(revalue(data_years$CROPDMGEXP, replaceValues))
data_years$PROPDMGEXP[data_years$PROPDMGEXP == ""]<-"1"
data_years$CROPDMGEXP[data_years$CROPDMGEXP == ""]<-"1"
data_years$PROPDMGEXP<-as.numeric(data_years$PROPDMGEXP)
data_years$CROPDMGEXP<-as.numeric(data_years$CROPDMGEXP)
data_years$PROPDMGEXP[is.na(data_years$PROPDMGEXP)]<-1
data_years$CROPDMGEXP[is.na(data_years$CROPDMGEXP)]<-1
for(i in 0:8) print(c(i,sum(data_years$PROPDMGEXP==i)))
for(i in 0:8) print(c(i,sum(data_years$CropDMGEXP==i)))
head(data_years$PROPDMGEXP)
# [1]    1    1    1 1000 1000    1
summary(data_years$PROPDMGEXP)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 1.000e+03 1.000e+03 4.537e+04 1.000e+03 1.000e+09 
summary(data_years$CropDMGEXP)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.000e+00 1.000e+03 1.000e+03 1.324e+04 1.000e+03 1.000e+09
head(data_years$CROPDMGEXP)
# [1] 1 1 1 1 1 1
summary(data_years$CROPDMGEXP)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.000e+00 1.000e+03 1.000e+03 1.324e+04 1.000e+03 1.000e+09 

######################################################
# data10[data10$PROPDMGEXP == 8,]
# data10_keep[data10_keep$COUNTYNAME == "CAMDEN",]
data_years$PropTot<-data_years$PROPDMG * data_years$PROPDMGEXP
summary(data_years$PropTot)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000e+00 0.000e+00 0.000e+00 5.972e+05 2.000e+03 1.150e+11
data_years$CropTot<-data_years$CROPDMG * data_years$CROPDMGEXP
summary(data_years$CropTot)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00e+00 0.00e+00 0.00e+00 3.48e+04 0.00e+00 1.00e+09

# eventTypes<-levels(data_years$EVTYPE)
# dataSnow<-data_years[grep("SNOW",data_years$EVTYPE,ignore.case=T),]
# levels(dataSnow$EVTYPE)
# dataWinterStorm<-data_years[grep("WINTER",data_years$EVTYPE,ignore.case=T),]
# dataWinter<-data_years[union(grep("WINTER",data_years$EVTYPE,ignore.case=T),grep("SNOW",data_years$EVTYPE,ignore.case=T)),]
# dataThunder<-data_years[union(grep("THUNDER",data_years$EVTYPE,ignore.case=T),grep("TSTM",data_years$EVTYPE,ignore.case=T)),]
# dataWind<-data_years[union(grep("wind",data_years$EVTYPE,ignore.case=T),grep("BLOW",data_years$EVTYPE,ignore.case=T)),]
# dataHail<-data_years[grep("hail",data_years$EVTYPE,ignore.case=T),]
# dataFlood<-data_years[union(grep("surf",data_years$EVTYPE,ignore.case=T),grep("flood",data_years$EVTYPE,ignore.case=T)),]

require(lattice)
# Property Damage ###############################################################
sumPropTotByFactor<-aggregate(PropTot ~ EVTYPE, sum, data = data_years,simplify=T)
sumPropTotByFactor$EVTYPE<-as.character(sumPropTotByFactor$EVTYPE)
quantile(sumPropTotByFactor$PropTot)
# 0%          25%          50%          75%         100% 
# 0       488300      7289000    252273803 130485411240 
maxPropTot<-max(sumPropTotByFactor$PropTot)
# [1] 130485411240
maxPropTotEVTYPE<-sumPropTotByFactor[sumPropTotByFactor$PropTot == maxPropTot,]
maxPropTotEVTYPE
# EVTYPE      PropTot
# 14  FLOOD 130485411240
#summary(sumPropTotByFactor)
Q3<-quantile(sumPropTotByFactor$PropTot,.9)
Q3PropType<-sumPropTotByFactor[sumPropTotByFactor$PropTot > Q3,]
sort(Q3PropType$PropTot)
# [1]   3382654440   4641188000   7177267260   7399567600  15381596740 130485411240
Q3PropType
# EVTYPE      PropTot
# 13       FLASH FLOOD   7177267260
# 14             FLOOD 130485411240
# 18              HAIL   7399567600
# 40  STORM SURGE/TIDE   4641188000
# 42 THUNDERSTORM WIND   3382654440
# 43           TORNADO  1538159674
x1<-Q3PropType$PropTot
names(x1)<-Q3PropType$EVTYPE
sort(x1,decreasing=TRUE)
# FLOOD           TORNADO              HAIL       FLASH FLOOD  STORM SURGE/TIDE THUNDERSTORM WIND 
# 130485411240       15381596740        7399567600        7177267260        4641188000        3382654440 
original.par<-par()
par(xaxt="n")
lablist<-as.vector(Q3PropType$EVTYPE)
plot(Q3PropType$PropTot,type="h",lwd=5,xlab="",ylab="PROPERTY DAMAGE",main="STORM PROPERTY DAMAGE IN THE U.S.")
axis(1,at=1:6,labels=F)
text(1:6,labels=lablist,srt=45,pos=1,xpd=T)
par(original.par)
#### Crop Damage #########################
sumCropTotByFactor<-aggregate(CropTot ~ EVTYPE, sum, data = data_years,simplify=T)
sumCropTotByFactor$EVTYPE<-as.character(sumCropTotByFactor$EVTYPE)
maxCropTot<-max(sumCropTotByFactor$CropTot)
maxCropTot
# [1] 2981275000
maxCropTotEVTYPE<-sumCropTotByFactor[sumCropTotByFactor$CropTot == maxCropTot,]
maxCropTotEVTYPE
# EVTYPE    CropTot
# 14  FLOOD 2981275000
quantile(sumCropTotByFactor$CropTot)
# 0%        25%        50%        75%       100% 
# 0          0      10000   28123975 2981275000 
Q90<-quantile(sumCropTotByFactor$CropTot,.9)
Q90CropType<-sumCropTotByFactor[sumCropTotByFactor$CropTot > Q90,]
x2<-Q90CropType$CropTot
names(x2)<-Q90CropType$EVTYPE
sort(x2,decreasing = TRUE)
# FLOOD        DROUGHT           HAIL   FROST/FREEZE    FLASH FLOOD EXCESSIVE HEAT 
# 2981275000     2801241000     1001730000      943741000      736955000      492400000
original.par<-par()
par(xaxt="n")
lablist<-as.vector(Q90CropType$EVTYPE)
plot(Q90CropType$CropTot,type="h",lwd=5,xlab="",ylab="CROP DAMAGE",main="STORM CROP DAMAGE IN THE U.S.")
axis(1,at=1:6,labels=F)
text(1:6,labels=lablist,srt=45,pos=1,xpd=T)
par(original.par)
### total #############################################################################
totalCost<-sumCropTotByFactor$CropTot + sumPropTotByFactor$PropTot
names(totalCost)<-sumPropTotByFactor$EVTYPE
totDamage<-sort(totalCost,decreasing=TRUE)
head(totDamage,6)
# FLOOD           TORNADO              HAIL       FLASH FLOOD  STORM SURGE/TIDE THUNDERSTORM WIND 
# 133466686240       15491243740        8401297600        7914222260        4642038000        3780985440
plot(totDamage[1:6],type='h',main="TOTAL COST DUE TO STORMS IN THE U.S.", ylab="TOTAL COST")
lablist<-as.vector(names(totDamage[1:6]))
axis(1,at=1:6,labels=F)
text(1:6,labels=lablist,srt=45,pos=1,xpd=T)
#===========================================================================
# FATALITIES ###############################################################
sumDeathByFactor<-aggregate(FATALITIES ~ EVTYPE, sum, data = data_years,simplify=T)
sumDeathByFactor$EVTYPE<-as.character(sumDeathByFactor$EVTYPE)
quantile(sumDeathByFactor$FATALITIES)
# 0%    25%    50%    75%   100% 
# 0.00   0.00   7.00  47.25 930.00
Q90<-quantile(sumDeathByFactor$FATALITIES,.9)
Q90death<-sumDeathByFactor[sumDeathByFactor$FATALITIES > Q90,]
x1<-Q90death$FATALITIES
names(x1)<-Q90death$EVTYPE
sort(x1,decreasing=TRUE)
# TORNADO    FLASH FLOOD EXCESSIVE HEAT    RIP CURRENT           HEAT      LIGHTNING 
# 930            352            324            233            229            204
original.par<-par()
par(xaxt="n")
lablist<-as.vector(Q90death$EVTYPE)
plot(Q90death$FATALITIES,type="h",lwd=5,xlab="",ylab="FATALITIES",main="STORM FATALITIES IN THE U.S.")
axis(1,at=1:6,labels=F)
text(1:6,labels=lablist,srt=45,pos=1,xpd=T)
par(original.par)
# injuries ###############################################################
require(stats)
sumHurtByFactor<-aggregate(INJURIES ~ EVTYPE, sum, data = data_years,simplify=T)
sumHurtByFactor$EVTYPE<-as.character(sumHurtByFactor$EVTYPE)
quantile(sumHurtByFactor$INJURIES)
# 0%     25%     50%     75%    100% 
# 0.0     0.0    13.5   145.5 10600.0
Q90<-quantile(sumHurtByFactor$INJURIES,.9)
Q90Hurt<-sumHurtByFactor[sumHurtByFactor$INJURIES > Q90,]
x1<-Q90Hurt$INJURIES
names(x1)<-Q90Hurt$EVTYPE
sort(x1,decreasing=TRUE)
# TORNADO    EXCESSIVE HEAT THUNDERSTORM WIND   HEAT         LIGHTNING          WILDFIRE 
# 10600              1873       1400            1222            1168               517
original.par<-par()
par(xaxt="n")
lablist<-as.vector(Q90Hurt$EVTYPE)
plot(Q90Hurt$INJURIES,type="h",lwd=5,xlab="",ylab="INJURIES",main="STORM INJURIES IN THE U.S.")
axis(1,at=1:6,labels=F)
text(1:6,labels=lablist,srt=45,pos=1,xpd=T)
par(original.par)
