
######## 
library(mtusRlocal) # load the package 
# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(knitr)
library(RColorBrewer)
library(TraMineR)
library(foreign)
library(ggplot2)
library(stargazer)
########
source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')
######## 
tb = function(x) table(x)
df = function(x) as.data.frame(x)
colnumber = function(x) t(data.frame(names(x))) 
recode_modalities <- function(variable, new_variable, var) { LeVelS <- levels(var); cat(paste(new_variable , '[', variable , '== "',LeVelS,'"', '] <- ', '\n',sep="")) }
######## 

# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dataWide3.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dta3.RData')
# 

########
# boys day sequences 
########
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaBoysDaysClust.RData') 

########
# girls day sequences 
########
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaGirlsDaysClust.RData') 

# 
source('/Users/giacomovagni/Rprojects/analysis_bcs/sourceglmsummary.R')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaBoysDaysClust2.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaGirlsDaysClust2.RData')
library(fmsb)
#

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeGirlsClusters_rec.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeBoysClusters_rec.RData') 

# recode_modalities('dataWide3$BD2SOC', new_variable = "dataWide3$BD2SOC_rec", var = dataWide3$BD2SOC) 
# table(dataWide3$BD4PSOC, dataWide3$BD2SOC)

dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "info missing"] <- "missing"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "student/voluntary work"] <- "missing"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "V unskilled"] <- "V unskilled"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "IV partly-skilled"] <- "IV partly-skilled"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "III manual"] <- "III manual"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "III non manual"] <- "III non manual"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "II managerial and Technical"] <- "II managerial and Technical"
dataWide3$BD2SOC_rec[dataWide3$BD2SOC== "I professional"] <- "I professional"

# missing 
dataWide3$BD2SOC_rec[is.na(dataWide3$BD2SOC_rec)] = "missing"
# sex factor 
dataWide3$SEX_rec = factor(dataWide3$SEX, labels = c('boys', 'girls'))
#  


dta3$withwhom[dta3$WHOWITH_orig== "1"] <- "partner"
dta3$withwhom[dta3$WHOWITH_orig== "10"] <- "partner"
dta3$withwhom[dta3$WHOWITH_orig== "11"] <- ""
dta3$withwhom[dta3$WHOWITH_orig== "12"] <- "siblings"
dta3$withwhom[dta3$WHOWITH_orig== "13"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "14"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "15"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "16"] <- "with children"
dta3$withwhom[dta3$WHOWITH_orig== "17"] <- "lover"
dta3$withwhom[dta3$WHOWITH_orig== "18"] <- "one friend"
dta3$withwhom[dta3$WHOWITH_orig== "19"] <- "more than one friend"
dta3$withwhom[dta3$WHOWITH_orig== "2"] <- "with children"
dta3$withwhom[dta3$WHOWITH_orig== "20"] <- "neighbours"
dta3$withwhom[dta3$WHOWITH_orig== "21"] <- "colleagues"
dta3$withwhom[dta3$WHOWITH_orig== "22"] <- "voluntary group"
dta3$withwhom[dta3$WHOWITH_orig== "24"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "25"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "26"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "27"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "28"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "29"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "3"] <- "siblings"
dta3$withwhom[dta3$WHOWITH_orig== "30"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "31"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "32"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "33"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "34"] <- "other/service"
dta3$withwhom[dta3$WHOWITH_orig== "35"] <- "parents"
dta3$withwhom[dta3$WHOWITH_orig== "36"] <- "alone"
dta3$withwhom[dta3$WHOWITH_orig== "4"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "5"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "6"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "7"] <- "misc"
dta3$withwhom[dta3$WHOWITH_orig== "8"] <- "kin"
dta3$withwhom[dta3$WHOWITH_orig== "9"] <- "misc"

kable(dta3 %>% group_by(SEX) %>% distinct(pid) %>% summarise(n = n()), caption = "Gender distribution") 

kable(dataWide3 %>% group_by(SEX_rec, BD2SOC_rec) %>% summarise(n = n()) %>% mutate(n = cp(n,NULL)) %>% spread(SEX_rec, n), caption = "ddistribution of parent's class by gender") 

n = n_distinct(dta3$pid)
dta3Summary = dta3 %>% group_by(diaryday, act_rec) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = sdur / n) %>% mutate(sum(meanduration))
# spread( select(dta3Summary, diaryday, act_rec, meanduration), act_rec, meanduration)

dta3SumSpread = df( spread( select(dta3Summary, diaryday, act_rec, meanduration), diaryday, meanduration) )
tclock = apply(X = dta3SumSpread[,-1], MARGIN = 2, FUN = TimeClock)
dta3SumSpread = as.data.frame( cbind(Activities = dta3SumSpread[,1], tclock)) 

kable(dta3SumSpread, caption = "Mean Time of Activities by Days")

kable(dta3 %>% group_by(pid, diaryday) %>% summarise(max_ep = max(epnum)) %>% group_by(diaryday) %>% summarise(mean_ep = sum(max_ep) / 4033), caption = "Mean number of episode per day") 

dta3Summary = dta3 %>% group_by(diaryday, withwhom) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = round( sdur / n, 2) ) %>% mutate(sum(meanduration))

dta3SumSpread = df( spread( select(dta3Summary, diaryday, withwhom, meanduration), diaryday, meanduration) )
tclock = apply(X = dta3SumSpread[,-1], MARGIN = 2, FUN = TimeClock)
dta3SumSpread = as.data.frame( cbind(Activities = dta3SumSpread[,1], tclock)) 
dta3SumSpread = filter(dta3SumSpread, Activities == "parents" | 
                         Activities == "siblings" | 
                         Activities == "kin" | 
                         # Activities == "neighbours" | 
                         Activities == "one friend" | 
                         Activities == "more than one friend" | 
                         Activities == "voluntary group" | 
                         Activities == "lover" | 
                         Activities == "colleagues" )

kable(dta3SumSpread, caption = "Mean Co-Presence Time by Days")

n = n_distinct(dta3$pid)

dta3Summary = dta3 %>% group_by(SEX) %>% mutate(n_sex = n_distinct(pid)) %>% group_by() %>% group_by(diaryday, act_rec, SEX, n_sex) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = sdur / n_sex) %>% group_by(diaryday, SEX) %>% mutate(sum(meanduration)) %>% group_by()

# spread( select(dta3Summary, diaryday, act_rec, meanduration), act_rec, meanduration)

dta3Summary = dta3Summary %>% mutate(meanduration = round(meanduration))
dta3SumSpreadSEX = df( spread( select(dta3Summary, SEX, diaryday, act_rec, meanduration), diaryday, meanduration) )

kol = brewer.pal(12,"Set3")

dta3Summary %>% ggplot(aes(x = SEX, fill = factor(act_rec), y = meanduration)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=c(kol))  + theme_minimal() + facet_grid(.~diaryday) 

kable( filter(dta3SumSpreadSEX, SEX == 1), caption = "Activities by Days - Boys")
kable( filter(dta3SumSpreadSEX, SEX == 2), caption = "Activities by Days - Girls") 

# melt 
dtaBoysDaysClust_melt = dtaBoysDaysClust %>%  melt(id.vars = c('pid', 'clusters'))

# spread 
dtaBoysDaysClust_spread = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = round( mean(value), 0)) %>% spread(clusters, meanv)
# dtaBoysDaysClust_spread

# re-melt 
dtaBoysDaysClust_melt_agg = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))

# heat-map 
kol = brewer.pal(6,"Oranges")
# ggplot(filter(dtaBoysDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal(

kable(dtaBoysDaysClust_spread, caption = "Mean Time by Clusters")

## girls 

# melt 
dtaGirlsDaysClust_melt = dtaGirlsDaysClust %>%  melt(id.vars = c('pid', 'clusters'))

# spread 
dtaGirlsDaysClust_spread = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = round( mean(value), 0)) %>% spread(clusters, meanv)
# dtaBoysDaysClust_spread

# re-melt 
dtaGirlsDaysClust_melt_agg = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))

# heat-map 
kol = brewer.pal(6,"Oranges")
# ggplot(filter(dtaBoysDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal()

kable(dtaGirlsDaysClust_spread, caption = "Mean Time by Clusters")

# Parents edu 
vec = vector('list', 6)

for(i in 1:6){
  vec [[i]] = glmSummary(glmodel = glm(dtaBoysDaysClust2$clusters == levels(dtaBoysDaysClust2$clusters)[i] ~ dtaBoysDaysClust2$parents_class))
}

names(vec) = levels(dtaBoysDaysClust2$clusters)
# as.data.frame(vec)

dtaBoysDaysClust2$BD2SOC = relevel(dtaBoysDaysClust2$BD2SOC, 'I professional')

summary( lm(dtaBoysDaysClust2$Culturalleisure ~ dtaBoysDaysClust2$BD2SOC) )
summary( lm(dtaBoysDaysClust2$Domesticwork ~ dtaBoysDaysClust2$BD2SOC) )
summary( lm(dtaBoysDaysClust2$Paidwork ~ dtaBoysDaysClust2$BD2SOC) )
summary( lm(dtaBoysDaysClust2$TV_radio ~ dtaBoysDaysClust2$BD2SOC) )
summary( lm(dtaBoysDaysClust2$School_Study ~ dtaBoysDaysClust2$BD2SOC) )

# Life Course Sequences 
########################################
Lifecourse = dataWide3[, grepl(pattern = 'y', x = colnames(dataWide3))] 
Lifecourse$pid = dataWide3$pid
########################################

########################################
#### #### #### ##### Sequence ##########
########################################
# colours 
kol = brewer.pal(10, 'Set3')
#
Lifecourse_seq = seqdef(Lifecourse[,-c(26)], cpal = kol)
#
rowboys = which(dataWide3$SEX == 1)
rowgirls = which(dataWide3$SEX == 2)

seqdplot(Lifecourse_seq, group = dataWide3$SEX, border = NA, title = "by Gender")

seqdplot(Lifecourse_seq[rowboys, ], group = LifeBoysClusters_rec$LifeBoysClusters_rec, border = NA, title = "Boys")

seqdplot(Lifecourse_seq[rowgirls, ], group = LifeGirlsClusters_rec$LifeGirlsClusters_rec, border = NA, title = "Girls")

stargazer(glm(dtaBoysDaysClust2$LifeBoysClusters_rec == "Long_Education" ~ 
                dtaBoysDaysClust2$Culturalleisure + 
                dtaBoysDaysClust2$Religionandvolunteering + 
                dtaBoysDaysClust2$Socialleisure +
                dtaBoysDaysClust2$School_Study + 
                dtaBoysDaysClust2$BD2SOC))

stargazer(glm(dtaBoysDaysClust2$LifeBoysClusters_rec == "Full_Time" ~ 
                dtaBoysDaysClust2$Culturalleisure + 
                dtaBoysDaysClust2$Religionandvolunteering + 
                dtaBoysDaysClust2$Socialleisure +
                dtaBoysDaysClust2$School_Study + 
                dtaBoysDaysClust2$BD2SOC))
# summary(glm(dtaBoysDaysClust2$LifeBoysClusters_rec == "Long_Education" ~ dtaBoysDaysClust2$Culturalleisure))
# summary(glm(dtaBoysDaysClust2$LifeBoysClusters_rec == "Long_Education" ~ dtaBoysDaysClust2$Culturalleisure))


