# read in data

library(ggplot2)
library(reshape2) # for "melt"
library(lme4)
library(car)  # for creating new variables
library(sjstats) # icc
library(piecewiseSEM)
library(survey) # for modelling

rm(list = ls(all = TRUE))

balo <- read.table("balochistan_child_data_mmr_7March2017.txt",header=T,sep=",")
punj <- read.table("punjab_child_data_mmr_7March2017.txt",header=T,sep=",")

head(balo)
head(punj)

# for consistency keep data with children aged <3 and yrbirth recorded
balo <- subset(balo,age.y<=2 & dob.y<9990) # 2268
punj <- subset(punj,!is.na(mmr.ever)) # 39202
punj <- subset(punj,age.y<=2 & dob.y<9990) # 39202

punj2 <- subset(punj,(OPV.ever=="Yes"|OPV.ever=="No") & (PENT.ever=="Yes"|PENT.ever=="No"))
punj2 <- subset(punj,(OPV.ever=="Yes"|OPV.ever=="No") & (PENT.ever=="Yes"|PENT.ever=="No") & (mmr.ever=="Yes"|mmr.ever=="No"))


#############################################################################################
#
# Regression Analysis
# - It's important to account for the sampling methodology used in the MICS surveys
# -- Not all districts / PSU were sampled equally
# -- Sampling without replacement was used, and this isn't really accounted for in a usual regression analysis
# -- Sampling weights are provided within the dataset, and we can use these to account for the sampling 
# -- in R, the package "survey" can be used to account for this
#############################################################################################

# Balochistan first

# use balo2 and punj2 for the analysis as they have been cleaned.
# observations within households, which are within clusters (hhID)
# 1530 hhID obs with valid observations
balo2$out.penta <- 0 
balo2$out.penta[balo2$PENT.num>=3] <- 1
table(balo2$out.penta)

# figure out the clustering
table(balo2$dd) # 6 regions
table(balo2$hh.num1,balo2$dd)
table(balo2$dd,balo2$hh.num2)
head(balo2)
balo2$dd <-  as.numeric(factor(balo2$district.division)) # convert character to numeric value
table(balo2$dd,balo2$district.division)

#m1 <- glmer(out.penta ~ 1 + rural + (1|hhID), family=binomial,data=balo2)
# I think this is the sensible structure
m1 <- glmer(out.penta ~ 1 + rural + (1|hhID) + (1|DISTRICT), family=binomial,data=balo2)

tmp <- summary(m1)#@REmat
names(tmp)

# variables - refer to dissertation


head(balo2)
table(balo2$DISTRICT)
# zone
balo2$zone <- "QUETTA"
balo2$zone[balo2$DISTRICT=="ZHOB"|balo2$DISTRICT=="SIBI"] <- "NORTH"
balo2$zone[balo2$DISTRICT=="NASIRABAD"|balo2$DISTRICT=="KALAT"|balo2$DISTRICT=="MEKRAN"] <- "SOUTH"
table(balo2$zone)
# immunisation card
table(balo2$IM1)
balo2$IM1 <- relevel(balo2$IM1,"No")
sum(is.na(balo2$IM1))
# Mother age
balo2$WB2b <- cut(balo2$WB2,c(0,20,25,30,35,40),right=F)
table(balo2$WB2,balo2$WB2b)
table(balo2$WB2b)
levels(balo2$WB2b) <- c(levels(balo2$WB2b),"Missing")
balo2$WB2b[is.na(balo2$WB2b)] <- "Missing"
balo2$WB2b <-   relevel(balo2$WB2b,"[25,30)")
table(balo2$WB2b)
# education
table(balo2$WB4)
sum(is.na(balo2$WB4)) # 1469 obs are missing 
balo2$WB4b <- recode(balo2$WB4,"c('Higher','Matric')='High'")  # using "car" package
levels(balo2$WB4b) <- c(levels(balo2$WB4b),"Missing")
balo2$WB4b[is.na(balo2$WB4)] <- "Missing"
balo2$WB4b <- relevel(balo2$WB4b,"High")
table(balo2$WB4b)
# literacy
table(balo2$WB7)
balo2$WB7b <- recode(balo2$WB7,"c('Able to read only parts of sentence','Blind / mute, visually / speech impaired','Missing','No sentence in required language Urdu')='Other'")
balo2$WB7b[is.na(balo2$WB7)] <- "Other"
table(balo2$WB7b)
# place of birth
table(balo2$MN18)
balo2$MN18b <- recode(balo2$MN18,"c('Government clinic / health centre','Government hospital')='Government hospital';c('Private clinic','Private hospital','Private maternity home')='Private hospital';c('Missing','Other','Other public')='Other'")
levels(balo2$MN18b) <- c(levels(balo2$MN18b),"Missing")
balo2$MN18b[balo2$MN18b=="Other"] <- "Missing"
balo2$MN18b[is.na(balo2$MN18)] <- "Missing"
#balo2$WB4b <- relevel(balo2$WB4b,"Missing")
table(balo2$MN18b)
# antenatal care sought
table(balo$MN1)
sum(is.na(balo2$MN1))
balo2$MN1[is.na(balo2$MN1)] <- "Missing"
balo2$MN1 <- relevel(balo2$MN1,"Yes")
# father's age'
balo2$head.ageb <- cut(balo2$head.age,c(0,30,35,40,45,50,100),right=F)
table(balo2$head.ageb)
#sum(!is.na(balo2$head.ageb))
levels(balo2$head.ageb) <- c(levels(balo2$head.ageb),"Missing")
balo2$head.ageb[is.na(balo2$head.ageb)] <- "Missing"
balo2$head.ageb <-   relevel(balo2$head.ageb,"[35,40)")
# father's education
table(balo2$head.edu)
sum(is.na(balo2$head.edu)) # 961 obs are missing 
balo2$head.edu2 <- recode(balo2$head.edu,"c('Higher','Matric')='High';c('DK','Middle')='Middle';c('Preschool')='Primary'")  # using "car" package
levels(balo2$head.edu2) <- c(levels(balo2$head.edu2),"Missing")
balo2$head.edu2[is.na(balo2$head.edu2)] <- "Missing"
balo2$head.edu2 <- relevel(balo2$head.edu2,"High")
table(balo2$head.edu2)
# access to info
table(balo2$HC8B)
balo2$HC8B[is.na(balo2$HC8B)] <- "Missing"
table(balo2$HC8C)
table(balo2$HC8E)
table(balo2$HC8B,balo2$HC8C)
table(balo$sex)

# examine whether they "missing at random"
tmp <- table(m=balo2$WB4b=="Missing",w=balo2$windex5)  # doesn't vary much
tmp <- table(m=balo2$MN18b=="Missing",w=balo2$windex5) # doesn't vary much
tmp <- table(m=balo2$WB7b=="Other",w=balo2$windex5) # this seems to increase with wealth
tmp <- table(m=balo2$WB2b=="Missing",w=balo2$windex5)  # doesn't vary much
prop.table(tmp,1)    # doesn't vary much

# wealth index
table(balo2$windex5)
table(balo2$DISTRICT)
table(balo2$dd,balo2$rural)
levels(balo2$WB2b)
balo2$WB2b <- relevel(balo2$WB2b,"[25,30)")
tapply(1/balo2$chweight,balo2$DISTRICTc,mean)

#save(balo2,file="balo2_7March17.Rdat")
load("balo2_7March17.Rdat")
write.csv(balo2,"balo2_7March17.csv",row.names=F)
write.csv(punj2,"punj2_7March17.csv",row.names=F)

hist(balo2$chweight)
balo2$chweight2 <- balo2$chweight/sum(balo2$chweight)
hist(tmp)
summary(tmp)
sum(balo2$chweight)

# accounting for survey design
# district divisio (dd) then cluster (hh.num1)
balo2$dum <-  1/0.02
dclus1 <- svydesign(id=~dd+hh.num1, weights=~chweight, data = balo2)
summary(dclus1)

# regression
# zone + IM1 + WB4b + MN18b + windex5 +
# variables: zone IM1 WB4b WB7b MN18b WB2b  windex5 head.ageb MN1 head.edu2 HC8B HC8C HC8E rural sex

glm1 <- svyglm(out.penta ~ DISTRICTc + age.y, 
              design=dclus1, family=quasibinomial())
glm1 <- svyglm(out.penta ~ factor(MN18c) + windex5 + age.y, 
              design=dclus1, family=quasibinomial())

summary(glm1)

table(balo2$MN18b)
balo2$MN18c <- balo2$MN18b
balo2$MN18c[balo2$MN18c=="Other home"] <- "Respondent's home"
balo2$MN18c <- as.numeric(factor(droplevels(balo2$MN18c)))
table(balo2$MN18c)

table(dclus1$variables$MN18b)
table(dclus1$variables$windex5)
table(balo2$dd,balo2$windex5)

# univariate
# things to force in : Age => linked because of age specific vaccines
#                      rural? The more I think about it the more this shouldn't be included (it's sig but but it's not really helpful)
m1 <- glmer(out.penta ~ 1 + age.y + (1|DISTRICT), 
            family=binomial,data=balo2)#[!is.na(balo2$WB7b),])
m1b <- glmer(out.penta ~ sex + age.y + (1|DISTRICT), 
            family=binomial,data=balo2)#[!is.na(balo2$WB7b),])
anova(m1,m1b)
summary(m1b)
# variables: zone IM1 WB4b WB7b MN18b WB2b  windex5 head.ageb MN1 head.edu2 HC8B HC8C HC8E rural sex
m2 <- glmer(out.penta ~ windex5 + age.y + (1|hhID) + (1|DISTRICT), 
            family=binomial,data=balo2)#[!is.na(balo2$WB7b),])
# anova
anova(m1b,m2)
summary(m2)

# glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))

m3 <- glmer(out.penta ~ zone + IM1 + windex5 + age.y + (1|hhID) + (1|DISTRICT), 
            family=binomial,data=balo2,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)))
m3a <- glmer(out.penta ~ zone*windex5 + IM1 + age.y + (1|hhID) + (1|DISTRICT), 
            family=binomial,data=balo2,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)))
m3b <- glmer(out.penta ~ zone*windex5 + IM1 + age.y + (1|hhID) + (1|DISTRICT), 
             family=binomial,data=balo2,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)))
m3c <- glmer(out.penta ~ zone + IM1 + WB4b + MN18b + age.y + (1|hhID) + (1|DISTRICT), 
             family=binomial,data=balo2,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)))

# notes: added all variables in
#        WB7 and WB2 had no sig values, removed
#        WB4==missing only sig, removed. MN18 borderline
#        final model: zone + IM1 + MN18(?) + windex
#        check IM18 non-sig, dropped
#        final model: zone + IM1 + windex5 + age.y
#        check interactions: zone*windex5 much smaller like interaction windex5:South
#
# alt model not including windex
#        WB2 & WB7 not sig, removed
#        final model: zone + IM1 + WB4b + MN18b + age.y
#        more variables but less of a fit
summary(m3)
summary(m3b)
anova(m3,m3a)
anova(m3b,m3c)
# all of the above a sig

###############################
# Punjab
###############################

punj2$out.penta <- 0 
punj2$out.penta[punj2$PENT.num>=3] <- 1
table(punj2$out.penta)
head(punj2)

#m1 <- glmer(out.penta ~ 1 + rural + (1|hhID), family=binomial,data=punj2)
# I think this is the sensible structure
m1 <- glmer(out.penta ~ 1 + rural + (1|hhID) + (1|DISTRICTc), family=binomial,data=punj2)

tmp <- summary(m1)#@REmat
names(tmp)

# variables - refer to dissertation


head(punj2)
table(punj2$DISTRICT)
# zone
punj2$zone <- "CENTRAL"
punj2$zone[punj2$DISTRICT=="GUJRANWALA"|punj2$DISTRICT=="RAWALPINDI"|punj2$DISTRICT=="SARGODHA"] <- "NORTH"
punj2$zone[punj2$DISTRICT=="D.G.KHAN"|punj2$DISTRICT=="MULTAN"|punj2$DISTRICT=="BAHAWALPUR"|punj2$DISTRICT=="SAHIWAL"] <- "SOUTH"
table(punj2$zone)
# immunisation card
table(punj2$IM1)
punj2$IM1[punj2$IM1=="Yes, seen"] <- "Yes, not seen"
punj2$IM1 <- relevel(punj2$IM1,"No")
sum(is.na(punj2$IM1))
# education
table(punj2$WB4)
sum(is.na(punj2$WB4)) # 11871 obs are missing 
punj2$WB4b <- recode(punj2$WB4,"c('Above Matric','Matric')='High'")  # using "car" package
punj2$WB4b[punj2$WB4b=="Preschool"] <- "Primary"
levels(punj2$WB4b) <- c(levels(punj2$WB4b),"Missing")
punj2$WB4b[is.na(punj2$WB4)] <- "Missing"
punj2$WB4b <- relevel(punj2$WB4b,"High")
table(punj2$WB4b)
# literacy
table(punj2$WB7)
punj2$WB7b <- recode(punj2$WB7,"c('Able to read only parts of sentence','Blind / mute, visually / speech impaired','Missing')='Other'")
punj2$WB7b[is.na(punj2$WB7)] <- "Other"
table(punj2$WB7b)
# place of birth
table(punj2$MN18)
punj2$MN18b <- recode(punj2$MN18,"c('Govt. Mother & Child Health centre','Other public')='Government hospital';c('Private clinic','Other private medical','Private maternity home')='Private hospital';c('Missing','Other','')='Other'")
levels(punj2$MN18b) <- c(levels(punj2$MN18b),"Missing")
punj2$MN18b[punj2$MN18b=="Other"] <- "Missing"
punj2$MN18b[is.na(punj2$MN18)] <- "Missing"
#punj2$WB4b <- relevel(punj2$WB4b,"Missing")
table(punj2$MN18b)
# Mother age
punj2$WB2b <- cut(punj2$WB2,c(0,20,25,30,35,40),right=F)
table(punj2$WB2,punj2$WB2b)
table(punj2$WB2b)
levels(punj2$WB2b) <- c(levels(punj2$WB2b),"Missing")
punj2$WB2b[is.na(punj2$WB2b)] <- "Missing"
punj2$WB2b <-   relevel(punj2$WB2b,"[25,30)")
table(punj2$WB2b)
table(punj2$windex5)
table(punj2$age.y)
# antenatal care sought
table(punj2$MN1)
sum(is.na(punj2$MN1))
punj2$MN1[is.na(punj2$MN1)] <- "Missing"
punj2$MN1 <- relevel(punj2$MN1,"Yes")
# father's age'
punj2$head.ageb <- cut(punj2$head.age,c(0,30,35,40,45,50,100),right=F)
table(punj2$head.ageb)
#sum(!is.na(punj2$head.ageb))
levels(punj2$head.ageb) <- c(levels(punj2$head.ageb),"Missing")
punj2$head.ageb[is.na(punj2$head.ageb)] <- "Missing"
punj2$head.ageb <-   relevel(punj2$head.ageb,"[35,40)")
# father's education
table(punj2$head.edu)
sum(is.na(punj2$head.edu)) # 961 obs are missing 
punj2$head.edu2 <- recode(punj2$head.edu,"c('Higher','Matric','Above Matric')='High';c('DK','Middle')='Middle';c('Pre-school')='Primary'")  # using "car" package
levels(punj2$head.edu2) <- c(levels(punj2$head.edu2),"Missing")
punj2$head.edu2[is.na(punj2$head.edu2)] <- "Missing"
punj2$head.edu2 <- relevel(punj2$head.edu2,"High")
table(punj2$head.edu2)
# access to info
table(punj2$HC8B)
punj2$HC8B[is.na(punj2$HC8B)] <- "Missing"
table(punj2$HC8C)
table(punj2$HC8E)
table(punj2$HC8B,punj2$HC8C)
table(punj2$sex)
table(punj2$rural)

# examine whether they "missing at random"
tmp <- table(m=punj2$WB4b=="Missing",w=punj2$windex5)  # doesn't vary much
tmp <- table(m=punj2$MN18b=="Missing",w=punj2$windex5) # doesn't vary much
tmp <- table(m=punj2$WB7b=="Other",w=punj2$windex5) # this seems to increase with wealth
tmp <- table(m=punj2$WB2b=="Missing",w=punj2$windex5)  # doesn't vary much
prop.table(tmp,1)    # doesn't vary much

# wealth index
table(punj2$windex5)
table(punj2$DISTRICT)

levels(punj2$WB2b)
punj2$WB2b <- relevel(punj2$WB2b,"[25,30)")

punj2$dd <- as.numeric(factor(punj2$district.division)) # convert character to numeric value
# account for sampling
save(punj2,file="punj2_7March17.Rdat")
load("punj2_7March17.Rdat")

punj2$chweight2 <- punj2$chweight/sum(punj2$chweight)
hist(tmp,xlim=c(0,0.0005))
summary(tmp)

names(balo2)
names(punj2)
balo3 <- subset(balo2,select=c("hh.num1","hh.num2","hh.mt","dob.d","dob.m","dob.y","age.y","BR1","CA1","CA4A","CA4B",
                              "CA4C","CA6A","CA6B","CA6C","CA6G","CA6H","CA6L","CA6M","CA6N","CA6O","CA6Q","CA6X","IM1",
                              "IM2","dpt1d","dpt1m","dpt1y","dpt2d","dpt2m","dpt2y","dpt3d","dpt3m","dpt3y","vac.ever",
                              "vac.everb","vacc.ever.bcg","vac.ever.oral","vac.oral.bth","vac.oral.num","vac.ever.dpt",
                              "dpt.num","mmr3d","mmr3m","mmr3y","mmr.ever","sex","windex5","chweight","hhID","hhIDMT",
                              "matchID","district.division","cu5","HH9","WS1","WS6","HW1","HW2","HW3A",
                              "HW3B","HW3D","rural","DCH_ID","hhs","HC8A","HC8B","HC8C","HC8D","HC8E","HC11","HC13",
                              "WB3","WB4","WB7","MN1","MN2A","MN2B","MN2C","MN2D","MN2F","MN2G","MN5","MN6","IS2A",
                              "HA1","HA3","HA6","HA8A","HA8B","HA8C","WB2","parity","MN18","CM8","head","head.age","head.edu",
                              "DCH_IDNUM","dnum","opvn","dtpn","OPV.num","OPV.ever","PENT.num","PENT.ever","DISTRICT","DISTRICTb",
                              "DISTRICTc","out.penta","zone","WB4b","WB7b","MN18b","WB2b","head.ageb","head.edu2","chweight2"))
punj3 <- subset(punj2,select=c("hh.num1","hh.num2","hh.mt","dob.d","dob.m","dob.y","age.y","BR1","CA1","CA4A","CA4B",
                               "CA4C","CA6A","CA6B","CA6C","CA6G","CA6H","CA6L","CA6M","CA6N","CA6O","CA6Q","CA6X","IM1",
                               "IM2","dpt1d","dpt1m","dpt1y","dpt2d","dpt2m","dpt2y","dpt3d","dpt3m","dpt3y","vac.ever",
                               "vac.everb","vacc.ever.bcg","vac.ever.oral","vac.oral.bth","vac.oral.num","vac.ever.dpt",
                               "dpt.num","mmr3d","mmr3m","mmr3y","mmr.ever","sex","windex5","chweight","hhID","hhIDMT",
                               "matchID","district.division","cu5","HH9","WS1","WS6","HW1","HW2","HW3A",
                               "HW3B","HW3D","rural","DCH_ID","hhs","HC8A","HC8B","HC8C","HC8D","HC8E","HC11","HC13",
                               "WB3","WB4","WB7","MN1","MN2A","MN2B","MN2C","MN2D","MN2F","MN2G","MN5","MN6","IS2A",
                               "HA1","HA3","HA6","HA8A","HA8B","HA8C","WB2","parity","MN18","CM8","head","head.age","head.edu",
                               "DCH_IDNUM","dnum","opvn","dtpn","OPV.num","OPV.ever","PENT.num","PENT.ever","DISTRICT","DISTRICTb",
                               "DISTRICTc","out.penta","zone","WB4b","WB7b","MN18b","WB2b","head.ageb","head.edu2","chweight2"))
balo3$Prov <- "BALOCHISTAN"
punj3$Prov <- "PUNJAB"
tmp <- rbind(balo3,punj3)
tmp$hh.num1[tmp$Prov=="BALOCHISTAN"] <- tmp$hh.num1[tmp$Prov=="BALOCHISTAN"]+8000
#check hhnum1 is specific to district
table(tmp$hh.num1,tmp$Prov)
table(tmp$hh.num1)
save(tmp,file="joint_surveys_13March.rdta")
write.csv(tmp,"joint_surveys_13March.csv")

# accounting for survey design
# district divisio (dd) then cluster (hh.num1)
#punj2$dum <-  1/0.02
dclus1 <- svydesign(id=~dd+hh.num1, weights=~chweight, data = punj2)
summary(dclus1)

# regression
# zone + IM1 + WB4b + MN18b + windex5 +
# variables: zone IM1 WB4b WB7b MN18b WB2b  windex5 head.ageb MN1 head.edu2 HC8B HC8C HC8E rural sex

glm1 <- svyglm(out.penta ~  age.y, 
               design=dclus1, family=quasibinomial())
glm2 <- svyglm(out.penta ~ IM1 + WB4b + MN18b + WB2b + windex5 + age.y, 
               design=dclus1, family=quasibinomial())

summary(glm2)
anova(glm1,glm2)
# univariate
# things to force in : Age => linked because of age specific vaccines
#                      rural? The more I think about it the more this shouldn't be included (it's sig but but it's not really helpful)
m1 <- glmer(out.penta ~ 1 + age.y + (1|DISTRICTc), 
            family=binomial,data=punj2)
m1b <- glmer(out.penta ~ sex + age.y + (1|DISTRICTc), 
             family=binomial,data=punj2)
# variables: zone IM1 WB4b WB7b MN18b WB2b  windex5 head.ageb MN1 head.edu2 HC8B HC8C HC8E rural sex
# anova
anova(m1,m1b)
summary(m1b)

m2 <- glmer(out.penta ~ IM1 + WB4b + MN18b + WB2b + windex5 + age.y + (1|hhID) + (1|DISTRICTc), 
            family=binomial,data=punj2,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 500000)))
# multivariable model - including wealth index
#     building: WB7b not sig, removed
#               and that seems to be a wrap!
#               wealth and other variables are sign.
summary(m2)

####################################################
# combine the datasets
####################################################

tmp1 <- subset(balo2,select=c("head.ageb","MN1","head.edu2","HC8B","HC8C","HC8E","rural","sex","IM1","WB4b","WB7b","MN18b","WB2b","windex5","age.y","out.penta","mmr.ever","hhID","DISTRICTc","zone"))
tmp2 <- subset(punj2,select=c("head.ageb","MN1","head.edu2","HC8B","HC8C","HC8E","rural","sex","IM1","WB4b","WB7b","MN18b","WB2b","windex5","age.y","out.penta","mmr.ever","hhID","DISTRICTc","zone"))
#"head.ageb","MN1","head.edu2","HC8B","HC8C","HC8E","rural","sex","IM1","WB4b","WB7b","MN18b","WB2b","windex5","age.y","out.penta","mmr.ever","hhID","DISTRICTc","zone"))

tmp1a <- cbind(tmp1,Province="BALO")
tmp2a <- cbind(tmp2,Province="PUNJ")

dataa <- rbind(tmp1a,tmp2a)
# need to add in wealth index changing
dataa$W2 <- as.factor(paste0(substr(dataa$Province,1,1),dataa$windex5))
table(dataa$W2)
dataa$W2 <- relevel(dataa$W2,"P1")
dataa$z2 <- as.factor(paste0(substr(dataa$Province,1,1),"-",dataa$zone))
table(dataa$z2)
dataa$z2 <- relevel(dataa$z2,"P-CENTRAL")
dataa$mmr2 <- 0
dataa$mmr2[dataa$mmr.ever=="Yes"] <- 1
table(dataa$mmr2)

table(dataa$HC8B)
dataa$HC8B <- relevel(dataa$HC8B,"No")
dataa$HC8C <- relevel(dataa$HC8C,"No")
dataa$HC8E <- relevel(dataa$HC8E,"No")
table(dataa$WB4b)
dataa$WB4b[dataa$WB4b=="Preschool"] <- "Primary"
# save this data
save(dataa,file="MICS_data_regression_23Feb17.rdata")

load(file="MICS_data_regression_23Feb17.rdata")

table(penta=dataa$out.penta,mmr=dataa$mmr2)

# test for ICC - ie. is a multilevel model necessary?

# unconditional model
m1 <- glmer(out.penta ~ 1 + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000)))
m1 <- glmer(mmr2 ~ 1 + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000)))

summary(m1)

# icc
icc(m1)  # from sjstats

# zone and wealth
m1 <- glmer(out.penta ~ z2*W2 + age.y + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000)))
summary(m1)
sem.model.fits(m1)
# ok - should add in zone really
# mega-model....
m2 <- glmer(out.penta ~ IM1 + WB4b + MN18b + WB2b + W2 + age.y + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000)))
m2c <- glmer(out.penta ~ IM1 + WB4b + MN18b + WB2b + W2*age.y + (1|hhID) + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000)))

# go through each variable and test the improvement 
# IM1 + WB4b + MN18b + WB2b + W2 + head.ageb + MN1 + head.edu2 + HC8B + HC8C + HC8E + rural + sex + age.y
m2d <- glmer(out.penta ~ IM1 + WB4b + WB2b + MN1 + head.edu2 + age.y + W2 + (1|DISTRICTc), 
             family=binomial,data=dataa,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000),calc.derivs = FALSE))
# check each in turn that have been removed
m2e <- glmer(out.penta ~ IM1 + WB4b + WB2b + MN1 + head.edu2 + age.y + W2 + HC8E + (1|DISTRICTc), 
             family=binomial,data=dataa,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000),calc.derivs = FALSE))

summary(m2d)
sem.model.fits(m2d)

anova(m2d,m2e)
#anova(m2,m2b)
summary(m2d)
summary(m2e)
# ok second round of analysis with more variables
# which are not sig...? 
# .................sex, rural, HC8E, HC8C, HC8B, head.ageb
# .................head.age - only missing is sig, remove
# checking their inclusion at the end 
# ................HC8E  p=0.70
# ................HC8C  p=0.23
# ................HC8B  p=0.21
# ................sex   p=0.65
# ................rural p=0.955
table(dataa$W2)

# we want to compare these results with measles
n1 <- glmer(mmr2 ~ 1 + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000),calc.derivs = FALSE))
sem.model.fits(n1)

n2 <- glmer(mmr2 ~ IM1 + WB4b + MN18b + WB2b + MN1 + head.edu2 + sex + W2 + age.y + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 5000),calc.derivs = FALSE))
summary(n2)
# IM1 + WB4b + WB2b + W2
n2b <- glmer(mmr2 ~ IM1 + WB4b + WB2b + MN1 + head.edu2 + sex + W2 + age.y + (1|DISTRICTc), 
            family=binomial,data=dataa,
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 6000),calc.derivs = FALSE))
summary(n2)
sem.model.fits(n2)
anova(n2,n2b)
# notes for final model:
# which are not sig...? 
# .................HC8B + HC8C + HC8E 
# .................head.ageb rural
# checking their inclusion at the end 
# ................ 

#save(m2,file="m2.rdata")
#save(m2b,file="m2b.rdata")

#load("m2.rdata")
#load("m2b.rdata")



summary(m2)
summary(m2b)
summary(m2c)
#  building.... comments
#               WB2b (age) seems to be laregly driven by the missing data (high Pr), should remove
#               MN18 only has "home births" as sig RF
#               WB7b (literacy) is not important, remove
# final model: IM1 + WB4b + MN18b + WB2b + windex5 + Province + age.y
# --          could explore for interactions. Looked at age*W2 and no interactions.  
# end
