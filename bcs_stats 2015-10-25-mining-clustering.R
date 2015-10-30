# Creating the databases ! 

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
########
source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dataWide3.RData')

tb = function(x) table(x)
df = function(x) as.data.frame(x)
colnumber = function(x) t(data.frame(names(x))) 
recode_modalities <- function(variable, new_variable, var) { LeVelS <- levels(var); cat(paste(new_variable , '[', variable , '== "',LeVelS,'"', '] <- ', '\n',sep="")) }
######## 

########
# boys day sequences 
########

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaBoysDaysClust.RData') 

# melt 
dtaBoysDaysClust_melt = dtaBoysDaysClust %>%  melt(id.vars = c('pid', 'clusters'))

# spread 
dtaBoysDaysClust_spread = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% spread(clusters, meanv)
dtaBoysDaysClust_spread

# re-melt 
dtaBoysDaysClust_melt_agg = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))

# heat-map 
kol = brewer.pal(6,"Oranges")
ggplot(filter(dtaBoysDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal()

# barplots 
kol = brewer.pal(6,"Set3")
filter(dtaBoysDaysClust_melt, variable != 'Personal care') %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% ggplot(aes(x = clusters, fill = factor(clusters), y = meanv)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=c(kol))  + theme_minimal() + facet_grid(.~variable) 

########
# girls day sequences 
########

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaGirlsDaysClust.RData') 

# melt 
dtaGirlsDaysClust_melt = dtaGirlsDaysClust %>%  melt(id.vars = c('pid', 'clusters'))

# spread 
dtaGirlsDaysClust_spread = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% spread(clusters, meanv)
dtaGirlsDaysClust_spread

# re-melt 
dtaGirlsDaysClust_melt_agg = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))

# heat-map 
kol = brewer.pal(6,"Oranges")
ggplot(filter(dtaGirlsDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal()

# barplots 
kol = brewer.pal(6,"Set3")
filter(dtaGirlsDaysClust_melt, variable != 'Personal care') %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% ggplot(aes(x = clusters, fill = factor(clusters), y = meanv)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=c(kol))  + theme_minimal() + facet_grid(.~variable) 

#### 

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeGirlsClusters_rec.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/LifeBoysClusters_rec.RData') 

LifeBoysClusters_rec$pid = rownames(LifeBoysClusters_rec)
LifeGirlsClusters_rec$pid = rownames(LifeGirlsClusters_rec)

dtaGirlsDaysClust2 = merge(dtaGirlsDaysClust, LifeGirlsClusters_rec, by = 'pid') 
dtaBoysDaysClust2 = merge(dtaBoysDaysClust, LifeBoysClusters_rec, by = 'pid') 

dw3 = dataWide3[,c('pid', 'BD2SOC')]

# 
recode_modalities(variable = 'dw3$BD2SOC', new_variable = 'dw3$parents_class', var = dw3$BD2SOC)

dw3$parents_class[dw3$BD2SOC== "info missing"] <- 'missing'
  dw3$parents_class[dw3$BD2SOC== "student/voluntary work"] <- 'missing'
  dw3$parents_class[dw3$BD2SOC== "V unskilled"] <- 'V unskilled'
  dw3$parents_class[dw3$BD2SOC== "IV partly-skilled"] <- 'IV partly-skilled'
  dw3$parents_class[dw3$BD2SOC== "III manual"] <- 'III manual'
  dw3$parents_class[dw3$BD2SOC== "III non manual"] <- 'III non manual'
  dw3$parents_class[dw3$BD2SOC== "II managerial and Technical"] <- 'II managerial and Technical'
  dw3$parents_class[dw3$BD2SOC== "I professional"] <- 'I professional'
#

dtaGirlsDaysClust2 = merge(dtaGirlsDaysClust2, dw3, by = 'pid')
dtaBoysDaysClust2 = merge(dtaBoysDaysClust2, dw3, by = 'pid')

#  
dtaGirlsDaysClust2$parents_class = as.character(dtaGirlsDaysClust2$parents_class)
dtaGirlsDaysClust2$parents_class[is.na(dtaGirlsDaysClust2$parents_class)] = 'Missing'

dtaBoysDaysClust2$parents_class = as.character(dtaBoysDaysClust2$parents_class)
dtaBoysDaysClust2$parents_class[is.na(dtaBoysDaysClust2$parents_class)] = 'Missing'

# 
# save(dtaBoysDaysClust2, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaBoysDaysClust2.RData')
# save(dtaGirlsDaysClust2, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtaGirlsDaysClust2.RData')
#

colnames(dtaBoysDaysClust2) = gsub(pattern = "/", replacement = '_', colnames(dtaBoysDaysClust2)) 
colnames(dtaBoysDaysClust2) = gsub(pattern = "[[:blank:]]", replacement = '', colnames(dtaBoysDaysClust2)) 

summary( glm(dtaBoysDaysClust2$LifeBoysClusters_rec == 'Full_Time' ~ dtaBoysDaysClust2$clusters + dtaBoysDaysClust2$parents_class) )
summary( glm(dtaBoysDaysClust2$LifeBoysClusters_rec == 'Not_Working' ~ dtaBoysDaysClust2$clusters + dtaBoysDaysClust2$parents_class) )
summary( glm(dtaBoysDaysClust2$LifeBoysClusters_rec == 'Long_Education' ~ dtaBoysDaysClust2$clusters + dtaBoysDaysClust2$parents_class) )
summary( glm(dtaBoysDaysClust2$LifeBoysClusters_rec == 'Self_Emp' ~ dtaBoysDaysClust2$clusters + dtaBoysDaysClust2$parents_class ) )

summary( glm(dtaBoysDaysClust2$clusters == 'TV' ~  dtaBoysDaysClust2$parents_class, family = binomial))
summary( glm(dtaBoysDaysClust2$clusters == 'Religion' ~ dtaBoysDaysClust2$parents_class, family = binomial))
summary( glm(dtaBoysDaysClust2$clusters == 'Sport' ~ dtaBoysDaysClust2$parents_class, family = binomial))
summary( glm(dtaBoysDaysClust2$clusters == 'School' ~ dtaBoysDaysClust2$parents_class, family = binomial))
summary( glm(dtaBoysDaysClust2$clusters == 'Paid_Work' ~ dtaBoysDaysClust2$parents_class, family = binomial))
#

cp( table(dtaBoysDaysClust2$clusters, dtaBoysDaysClust2$parents_class), 1)
cp( table(dtaBoysDaysClust2$clusters, dtaBoysDaysClust2$LifeBoysClusters_rec), 2)

chisq.test( table(dtaBoysDaysClust2$clusters, dtaBoysDaysClust2$parents_class) )

library(FactoMineR)
CA(table(dtaBoysDaysClust2$clusters, dtaBoysDaysClust2$LifeBoysClusters_rec))

CA(table(dtaBoysDaysClust2$clusters, dtaBoysDaysClust2$parents_class))


summary( glm(dtaBoysDaysClust2$LifeBoysClusters_rec == 'Long_Education' ~ 
               dtaBoysDaysClust2$School_Study + 
               dtaBoysDaysClust2$TV_radio + 
               dtaBoysDaysClust2$Socialleisure + 
               dtaBoysDaysClust2$Domesticwork +
               dtaBoysDaysClust2$Culturalleisure + 
               dtaBoysDaysClust2$Religionandvolunteering + 
               dtaBoysDaysClust2$parents_class) )

# 

colnames(dtaGirlsDaysClust2) = gsub(pattern = "/", replacement = '_', colnames(dtaGirlsDaysClust2)) 
colnames(dtaGirlsDaysClust2) = gsub(pattern = "[[:blank:]]", replacement = '', colnames(dtaGirlsDaysClust2)) 

summary( glm(dtaGirlsDaysClust2$LifeGirlsClusters_rec == 'Looking_After_Home' ~ dtaGirlsDaysClust2$clusters ) )
summary( glm(dtaGirlsDaysClust2$LifeGirlsClusters_rec == 'Full_Time' ~ dtaGirlsDaysClust2$clusters ) )
summary( glm(dtaGirlsDaysClust2$LifeGirlsClusters_rec == 'Part_Time' ~ dtaGirlsDaysClust2$clusters ) )
summary( glm(dtaGirlsDaysClust2$LifeGirlsClusters_rec == 'Self_Emp' ~ dtaGirlsDaysClust2$clusters ) )

summary( glm(dtaGirlsDaysClust2$clusters == 'TV' ~ dtaGirlsDaysClust2$LifeGirlsClusters_rec, family = binomial))
summary( glm(dtaGirlsDaysClust2$clusters == 'Domestic' ~ dtaGirlsDaysClust2$LifeGirlsClusters_rec, family = binomial))
summary( glm(dtaGirlsDaysClust2$clusters == 'Paid_Work' ~ dtaGirlsDaysClust2$LifeGirlsClusters_rec, family = binomial))
summary( glm(dtaGirlsDaysClust2$clusters == 'School' ~ dtaGirlsDaysClust2$LifeGirlsClusters_rec, family = binomial))





