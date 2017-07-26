library(foreign)

rm(list = ls(all = TRUE))
setwd("H:/")
setwd("~/Dropbox (VERG)/Pakistan_measles/")
setwd("~/Dropbox (VERG)/Pakistan_polioRI/Data (Punjab)/Pakistan (Punjab) MICS 2011 SPSS Datasets/")
#setwd("~/Dropbox/Dropbox/Ideas/Pakistan_IDPs/Data/Pakistan (Balochistan)_MICS4_Datasets/Pakistan (Balochistan) MICS 2010 SPSS Datasets/")

dhh <- read.spss("hh.sav")  # household survey
dch <- read.spss("ch.sav")  # child survey
dmo <- read.spss("wm.sav")  # women survey
dot <- read.spss("hl.sav")  # household members survey

names(dhh)
names(dot)

 
# what's cluster level?
dhh[[which(names(dhh)=="HH1A")]]
table(dhh[[which(names(dhh)=="HH1")]])  # cluster refers to a enumeration area and whether it is urban or rural
table(dhh[[which(names(dhh)=="HH2")]])  # a household within each enumeration area? (only goes up to 24)
table(dhh[[which(names(dhh)=="HH1A")]]) # district
table(dhh[[which(names(dhh)=="div")]],dhh[[which(names(dhh)=="HH1A")]])
table(dhh[[which(names(dhh)=="div")]])

table(dot[[which(names(dot)=="ED1")]])
table(dot[[which(names(dot)=="HL3")]]) # includes term "head"
table(dot[[which(names(dot)=="HL4")]])
table(dot[[which(names(dot)=="HL3")]],dot[[which(names(dot)=="HL4")]])
table(dot[[which(names(dot)=="ED1")]])

data.hl <- data.frame(district=dot[[which(names(dot)=="HH1A")]],
                      hh.num1=dot[[which(names(dot)=="HH1")]],  # cluster number
                      hh.num2=dot[[which(names(dot)=="HH2")]],  # household number
                      head=dot[[which(names(dot)=="HL3")]],     # status in household
                      gender=dot[[which(names(dot)=="HL4")]],   # gender
                      age=dot[[which(names(dot)=="HL6")]],      # age
                      edu=dot[[which(names(dot)=="ED4A")]]    # education
                      )
data.hl$hhID <- paste(data.hl$hh.num1,data.hl$hh.num2,sep="#")
data.hl$HCD_id <- paste(data.hl$district,data.hl$hh.num1,data.hl$hh.num2,sep="-")

data.hl2 <- subset(data.hl,head=="Head" & gender=="Male")

# we want to extract info for male HH, including age and education


data.hh <- data.frame(hh.num1=dhh[[which(names(dhh)=="HH1")]],  # cluster number
                      hh.num2=dhh[[which(names(dhh)=="HH2")]],  # household number
                   district=dhh[[which(names(dhh)=="HH1A")]],
                   floods=dhh[[which(names(dhh)=="HH7A")]],
                   district.division=dhh[[which(names(dhh)=="div")]],
			             rural=dhh[[which(names(dhh)=="HH6")]],
			             hhs=dhh[[which(names(dhh)=="HH11")]],  # househood size
                   cu5=dhh[[which(names(dhh)=="HH14")]],
                   HH9=dhh[[which(names(dhh)=="HH9")]],
                   WS1=dhh[[which(names(dhh)=="WS1")]],
                   WS6=dhh[[which(names(dhh)=="WS6")]],
			             WS8=dhh[[which(names(dhh)=="WS8")]],  # toilet
                   HW1=dhh[[which(names(dhh)=="HW1")]],
                   HW2=dhh[[which(names(dhh)=="HW2")]],
                   HW3A=dhh[[which(names(dhh)=="HW3A")]],
                   HW3B=dhh[[which(names(dhh)=="HW3B")]],
			             HC8A=dhh[[which(names(dhh)=="HC8A")]],
			             HC8B=dhh[[which(names(dhh)=="HC8B")]],  # radio
			             HC8C=dhh[[which(names(dhh)=="HC8C")]],  # tv
			             HC8D=dhh[[which(names(dhh)=="HC8D")]],
			             HC8E=dhh[[which(names(dhh)=="HC8E")]], # 
			             HC8G=dhh[[which(names(dhh)=="HC8E")]], # computer
			             HC11=dhh[[which(names(dhh)=="HC11")]],
			             HC13=dhh[[which(names(dhh)=="HC13")]],
			             HC14=dhh[[which(names(dhh)=="HC14A")]],
                   HW3D=dhh[[which(names(dhh)=="HW3D")]])
data.hh$hhID <- paste(data.hh$hh.num1,data.hh$hh.num2,sep="#")
data.hh$HCD_id <- paste(data.hh$district,data.hh$hh.num1,data.hh$hh.num2,sep="-")

dhh[[which(names(dhh)=="stratum")]]
table(data.hh$hh.num1,data.hh$district)
table(data.hh$district.division,data.hh$district)
table(data.hh$hh.num1,data.hh$district.division)
table(data.hh$HC8B,data.hh$HC8C)
head(data.hh)

# women survey
names(dmo)
data.wm <- data.frame(hh.num1=dmo[[which(names(dmo)=="HH1")]],
                      hh.num2=dmo[[which(names(dmo)=="HH2")]],
                      hhln = dmo[[which(names(dmo)=="LN")]],  # line in HH18
                      WB2=dmo[[which(names(dmo)=="WB2")]],  # womans age
                      WB3=dmo[[which(names(dmo)=="WB3")]],
                      WB4=dmo[[which(names(dmo)=="WB4")]],
                      WB7=dmo[[which(names(dmo)=="WB7")]],
                      CM5A=dmo[[which(names(dmo)=="CM5A")]],
                      CM5B=dmo[[which(names(dmo)=="CM5B")]],
                      CM7A=dmo[[which(names(dmo)=="CM7A")]],
                      CM7B=dmo[[which(names(dmo)=="CM7B")]],
                      parity=dmo[[which(names(dmo)=="CM5A")]]+dmo[[which(names(dmo)=="CM5B")]]+dmo[[which(names(dmo)=="CM7A")]]+dmo[[which(names(dmo)=="CM7B")]],
                      CM8=dmo[[which(names(dmo)=="CM8")]], # number of livebirths
                      MN1=dmo[[which(names(dmo)=="MN1")]], # antinatal care
                      MN2A=dmo[[which(names(dmo)=="MN2A")]],
                      MN2B=dmo[[which(names(dmo)=="MN2B")]],
                      MN2C=dmo[[which(names(dmo)=="MN2C")]],
                      MN2D=dmo[[which(names(dmo)=="MN2D")]],
                      MN2F=dmo[[which(names(dmo)=="MN2F")]],
                      MN2G=dmo[[which(names(dmo)=="MN2G")]],
                      MN5=dmo[[which(names(dmo)=="MN5")]],  # immunisation card?
                      MN6=dmo[[which(names(dmo)=="MN6")]],  # tetanus card?
                      #MN13=dmo[[which(names(dmo)=="MN13")]],
                      MN18=dmo[[which(names(dmo)=="MN18")]],  # place of delivery
                      IS2A=dmo[[which(names(dmo)=="IS2A")]],
                      HA1=dmo[[which(names(dmo)=="HA1")]],
                      HA3=dmo[[which(names(dmo)=="HA3")]],
                      HA6=dmo[[which(names(dmo)=="HA6")]],
                      HA8A=dmo[[which(names(dmo)=="HA8A")]],
                      HA8B=dmo[[which(names(dmo)=="HA8B")]],
                      HA8C=dmo[[which(names(dmo)=="HA8C")]]
                      )
data.wm$hhID <- paste(data.wm$hh.num1,data.wm$hh.num2,sep="#")
data.wm$hhIDLN <- paste(data.wm$hh.num1,data.wm$hh.num2,data.wm$hhln,sep="#")

head(data.wm) 
table(data.wm$MN18)
table(data.wm$parity)
names(dch)
table(dch[[which(names(dch)=="wscore")]])
hist(dch[[which(names(dch)=="wscore")]])  # I think this is the mulnutrition score? % below 2 SD
# Arsenault used -2 as a defintion of stunting
tmp <- dch[[which(names(dch)=="wscore")]]
sum(tmp < -2)/length(tmp)  # 1.1% or thereabouts (you would expect this to be linked to poverty, no?)
# mulnutrition?


data.ch <- data.frame(hh.num1=dch[[which(names(dch)=="HH1")]],
                      hh.num2=dch[[which(names(dch)=="HH2")]],
                      hh.mt=dch[[which(names(dch)=="UF6")]],
                      dob.d=dch[[which(names(dch)=="AG1D")]],
                      dob.m=dch[[which(names(dch)=="AG1M")]],
                      dob.y=dch[[which(names(dch)=="AG1Y")]],
                      age.y=dch[[which(names(dch)=="AG2")]],
                      BR1=dch[[which(names(dch)=="BR1")]],
                      CA1=dch[[which(names(dch)=="CA1")]],
                      CA4A=dch[[which(names(dch)=="CA4A")]],
                      CA4B=dch[[which(names(dch)=="CA4B")]],
                      CA4C=dch[[which(names(dch)=="CA4C")]],
                      CA6A=dch[[which(names(dch)=="CA6A")]],
                      CA6B=dch[[which(names(dch)=="CA6B")]],
                      CA6C=dch[[which(names(dch)=="CA6C")]],
                      CA6G=dch[[which(names(dch)=="CA6G")]],
                      CA6H=dch[[which(names(dch)=="CA6H")]],
                      CA6L=dch[[which(names(dch)=="CA6L")]],
                      CA6M=dch[[which(names(dch)=="CA6M")]],
                      CA6N=dch[[which(names(dch)=="CA6N")]],
                      CA6O=dch[[which(names(dch)=="CA6O")]],
                      CA6Q=dch[[which(names(dch)=="CA6Q")]],
                      CA6X=dch[[which(names(dch)=="CA6X")]],
                      IM1=dch[[which(names(dch)=="IM1")]],
                      IM2=dch[[which(names(dch)=="IM2")]],
                      dpt1d=dch[[which(names(dch)=="IM3T1D")]],
                      dpt1m=dch[[which(names(dch)=="IM3T1M")]],
                      dpt1y=dch[[which(names(dch)=="IM3T1Y")]],
                      dpt2d=dch[[which(names(dch)=="IM3T2D")]],
                      dpt2m=dch[[which(names(dch)=="IM3T2M")]],
                      dpt2y=dch[[which(names(dch)=="IM3T2Y")]],
                      dpt3d=dch[[which(names(dch)=="IM3T3D")]],
                      dpt3m=dch[[which(names(dch)=="IM3T3M")]],
                      dpt3y=dch[[which(names(dch)=="IM3T3Y")]],
                      vac.ever=dch[[which(names(dch)=="IM5")]],
                      vac.everb=dch[[which(names(dch)=="IM6")]],
                      vacc.ever.bcg=dch[[which(names(dch)=="IM7")]],
                      vac.ever.oral=dch[[which(names(dch)=="IM8")]],
                      vac.oral.bth=dch[[which(names(dch)=="IM9")]],
                      vac.oral.num=dch[[which(names(dch)=="IM10")]],
                      vac.ever.dpt=dch[[which(names(dch)=="IM11A")]],
                      dpt.num=dch[[which(names(dch)=="IM11B")]],
                      mmr3d=dch[[which(names(dch)=="IM3MD")]], 
                      mmr3m=dch[[which(names(dch)=="IM3MM")]], 
                      mmr3y=dch[[which(names(dch)=="IM3MY")]],
                      mmr.ever=dch[[which(names(dch)=="IM16")]], 
                      sex=dch[[which(names(dch)=="HL4")]], 
                      windex5=dch[[which(names(dch)=="windex5")]],
                      chweight=dch[[which(names(dch)=="chweight")]]
                      )
data.ch$hhID <- paste(data.ch$hh.num1,data.ch$hh.num2,sep="#")
data.ch$hhIDMT <- paste(data.ch$hh.num1,data.ch$hh.num2,data.ch$hh.mt,sep="#")

# add in the household info
oo <- match(data.ch$hhID,data.hh$hhID)
data.ch$matchID <- data.hh$hhID[oo]
data.ch$district <- data.hh$district[oo]
data.ch$district.division <- data.hh$district.division[oo]
data.ch$cu5 <- data.hh$cu5[oo]
data.ch$HH9 <- data.hh$HH9[oo]
data.ch$WS1 <- data.hh$WS1[oo]
data.ch$WS6 <- data.hh$WS6[oo]
data.ch$HW1 <- data.hh$HW1[oo]
data.ch$HW2 <- data.hh$HW2[oo]
data.ch$HW3A <- data.hh$HW3A[oo]
data.ch$HW3B <- data.hh$HW3B[oo]
data.ch$HW3D <- data.hh$HW3D[oo]
data.ch$rural <- data.hh$rural[oo]
data.ch$DCH_ID <- data.hh$HCD_id[oo]
data.ch$hhs <- data.hh$hhs[oo]
data.ch$HC8A <- data.hh$HC8A[oo]
data.ch$HC8B <- data.hh$HC8B[oo]  # radio
data.ch$HC8C <- data.hh$HC8C[oo]  # tv
data.ch$HC8D <- data.hh$HC8D[oo]
data.ch$HC8E <- data.hh$HC8E[oo]  # comp
data.ch$HC11 <- data.hh$HC11[oo]
data.ch$HC13 <- data.hh$HC13[oo]
data.ch$WS8 <- data.hh$WS8[oo]

 
# add in women info
# use linkage of hhIDMT
oo <- match(data.ch$hhIDMT,data.wm$hhIDLN)
sum(!is.na(oo))/length(oo) # 98.4% match
# (done incorrectly) oo <- match(data.ch$hhID,data.wm$hhID)
data.ch$WB3 <- data.wm$WB3[oo]
data.ch$WB4 <- data.wm$WB4[oo]
data.ch$WB7 <- data.wm$WB7[oo]
data.ch$MN1 <- data.wm$MN1[oo]    # antenatal care
data.ch$MN2A <- data.wm$MN2A[oo]
data.ch$MN2B <- data.wm$MN2B[oo]
data.ch$MN2C <- data.wm$MN2C[oo]
data.ch$MN2D <- data.wm$MN2D[oo]
data.ch$MN2F <- data.wm$MN2F[oo]
data.ch$MN2G <- data.wm$MN2G[oo]
data.ch$MN5 <- data.wm$MN5[oo]
data.ch$MN6 <- data.wm$MN6[oo]
data.ch$MN13 <- data.wm$MN13[oo]
data.ch$IS2A <- data.wm$IS2A[oo]
data.ch$HA1 <- data.wm$HA1[oo]
data.ch$HA3 <- data.wm$HA3[oo]
data.ch$HA6 <- data.wm$HA6[oo]
data.ch$HA8A <- data.wm$HA8A[oo]
data.ch$HA8B <- data.wm$HA8B[oo]
data.ch$HA8C <- data.wm$HA8C[oo]
data.ch$WB2 <- data.wm$WB2[oo]
data.ch$parity <- data.wm$parity[oo]
data.ch$MN18 <- data.wm$MN18[oo]
data.ch$CM8 <- data.wm$CM8[oo]
#data.ch$WS8 <- data.wm$WS8[oo]

# add in info about father
oo <- match(data.ch$hhID,data.hl2$hhID)
sum(!is.na(oo))/length(oo) # 98.4% match
data.ch$head <- data.hl2$head[oo]
data.ch$head.age <- data.hl2$age[oo]
data.ch$head.edu <- data.hl2$edu[oo]


head(data.ch)

table(data.ch$mmr.ever)
table(is.na(data.ch$mmr.ever))
table(data.ch$cu5)
table(data.ch$MN1)
table(data.ch$HC8B)
table(data.ch$WS6)
table(data.ch$MN18)

# remove incomplete surveys
#data <- subset(data.ch,HH9=="Completed" & !is.na(mmr.ever))
data <- subset(data.ch,HH9=="Completed" & !is.na(mmr.ever))
data <- subset(data.ch,HH9=="Completed" )

head(data)
table(data$hh.num1)

#add in a unique number for district-hh-cl
# *** Important ***
table(data$hh.num2)
data$DCH_IDNUM <- ((data$hh.num1-1)*25)+data$hh.num2  # up to 25 here because up to 25 hh are asked per enumeration 
tmp <- table(data$DCH_IDNUM,data$hh.num2)
table(data$DCH_IDNUM[data$hh.num1==4],data$hh.num2[data$hh.num1==4])
table(data$hh.num1)
data$dnum <- as.numeric(data$district.division)
table(data$dnum,data$district.division)
table(data$district,data$district.division)


setwd("~/Dropbox (VERG)/Dropbox/RI_Pakistan/data")
write.table(data,"punjab_child_data_mmr_7March2017.txt",col.names=T,row.names=F,sep=",")

#Bahawalpur D.G.Khan Faisalabad Gujranwala Lahore Multan Rawalpindi Sahiwal Sargodha
#1       2876        0          0          0      0      0          0       0        0
#2          0     2827          0          0      0      0          0       0        0
#3          0        0       3146          0      0      0          0       0        0
#4          0        0          0       3238      0      0          0       0        0
#5          0        0          0          0   2919      0          0       0        0
#6          0        0          0          0      0   2875          0       0        0
#7          0        0          0          0      0      0       1786       0        0
#8          0        0          0          0      0      0          0    1623        0
#9          0        0          0          0      0      0          0       0     1784

# end
