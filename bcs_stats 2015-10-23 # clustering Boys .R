
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

tb = function(x) table(x)
df = function(x) as.data.frame(x)
colnumber = function(x) t(data.frame(names(x))) 
######## 

# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dataWide3.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dta3.RData')
# 

######################################################################
################################### Boys ############################# 
######################################################################

# filter boys 
dtaBoys = filter(dataWide3, SEX == 1)
# diarys 
dtaBoysDays = dtaBoys[, grepl(pattern = 'DAY', x = names(dtaBoys))] 
# create id 
dtaBoysDays$pid = dtaBoys$pid
# melt 
dtaBoysDaysMelt = dtaBoysDays %>% group_by(pid) %>% melt(id.vars = 'pid')
# how many boys 
n_distinct(dtaBoysDaysMelt$pid)

# each episode is one minute - so counting solely works 
head(dtaBoysDaysMelt)
# 
dtaBoysDaysMean = dtaBoysDaysMelt %>% group_by(pid, value) %>% summarise(mean = n())
# remember that the 4 days are together 
dtaBoysDaysMean %>% mutate(total = sum(mean))
5760 / 4

# spread into a matrix format 
dtaBoysDaysMean = spread(dtaBoysDaysMean, value, mean, fill = 0) 
dtaBoysDaysMean
# log the data 
dtaBoysDaysLog = apply(dtaBoysDaysMean[,-1], 2, log)
# Inf to 0 
dtaBoysDaysLog [dtaBoysDaysLog == '-Inf'] = 0
# 
hist(dtaBoysDaysLog[,1])

colnumber( as.data.frame(dtaBoysDaysLog) )
cor(as.data.frame(dtaBoysDaysLog))

# id as rows 
row.names(dtaBoysDaysLog) = dtaBoysDaysMean$pid

########
library(FactoMineR)
########

# factor PCA 
head(dtaBoysDaysLog)
# 
dtaBoysDaysPCA = PCA(dtaBoysDaysLog[,c(1,2,5,6,8,9,10,11,12)]) 
quartz()
plot.PCA(dtaBoysDaysPCA)
plot.PCA(dtaBoysDaysPCA, choix = 'var')
plot.PCA(dtaBoysDaysPCA, choix = 'var', axes = c(2,3))

# HC 
dtaBoysDaysHC = HCPC(dtaBoysDaysPCA, nb.clust = 6, order = F)
# plots 
plot.HCPC(dtaBoysDaysHC, choice = 'map', draw.tree = F)
# variable description 
dtaBoysDaysHC$desc.var

# ind most closest to cluster center 
dtaBoysDaysHC$desc.ind$para

# Cluster 1 - Paid Work 
filter(dtaBoysDaysMean, pid == 'B19703Z')
filter(dtaBoysDaysMean, pid == 'B22087Z')

# Cluster 3 - School  
filter(dtaBoysDaysMean, pid == 'B21523U')
filter(dtaBoysDaysMean, pid == 'B27932H')

# Cluster 4 - TV
df( filter(dtaBoysDaysMean, pid == 'B28742H') )
df( filter(dtaBoysDaysMean, pid == 'B11598J') )

# Cluster 6 - Religion
df( filter(dtaBoysDaysMean, pid == 'B16848D') )
df( filter(dtaBoysDaysMean, pid == 'B23127U') )

########################################
########################################

clusters = dtaBoysDaysHC$data.clust$clust 
levels(clusters) = c('Paid_Work', 'Paid_Domestic', 'School', 'TV', 'Sport', 'Religion')
table(clusters, dtaBoysDaysHC$data.clust$clust )

# merge dataset  
dtaBoysDaysClust = cbind(dtaBoysDaysMean, clusters)
# melt 
dtaBoysDaysClust_melt = dtaBoysDaysClust %>%  melt(id.vars = c('pid', 'clusters'))
#
# spread 
dtaBoysDaysClust_spread = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% spread(clusters, meanv)
dtaBoysDaysClust_spread

# melt 
dtaBoysDaysClust_melt_agg = dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))
# rescaled 
library(scales)
# scale by clusters or variable ??    
dtaBoysDaysClust_melt_agg = dtaBoysDaysClust_melt_agg %>% group_by(clusters) %>% mutate(meanv_res = cp(meanv, NULL)) 
# check 
spread(select(dtaBoysDaysClust_melt_agg, -meanv), clusters, meanv_res)
###  
kol = brewer.pal(6,"Oranges")
quartz()
ggplot(filter(dtaBoysDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal()
dtaBoysDaysClust_melt_agg

#### ####
#### ggplot 
#### ####
quartz()
kol = brewer.pal(12, 'Set3')
kol = brewer.pal(6,"Blues")
dtaBoysDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% ggplot(aes(x = clusters, fill = factor(clusters), y = meanv)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=c(kol))  + theme_minimal() + facet_grid(.~variable) 

#########
#########
# save dataset 
# save(dtaBoysDaysClust, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaBoysDaysClust.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaBoysDaysClust.RData') 

########################################
########################################
########################################

# 
c('grey', 'purple', brewer.pal(12, 'Set3'))
seq = seqdef(dtaBoysDays[,-c(5761)], cpal = c(brewer.pal(12, 'Set3')))

row.names(seq)

quartz()
seqdplot(seq, border = NA, group = dtaBoysDaysHC$data.clust$clust)
quartz()
seqlegend(seq)

######################################################################
################################### Girls ############################ 
######################################################################

dataWide3[1:5,1:10]

# filter boys 
dtaGirls = filter(dataWide3, SEX == 2)
nrow(dtaGirls)

# diarys 
dtaGirlsDays = dtaGirls[, grepl(pattern = 'DAY', x = names(dtaGirls))] 
# create id 
dtaGirlsDays$pid = dtaGirls$pid
# melt 
dtaGirlsDaysMelt = dtaGirlsDays %>% group_by(pid) %>% melt(id.vars = 'pid')
# how many Girls 
n_distinct(dtaGirlsDaysMelt$pid)

# each episode is one minute - so counting solely works 
head(dtaGirlsDaysMelt)
# 
dtaGirlsDaysMean = dtaGirlsDaysMelt %>% group_by(pid, value) %>% summarise(mean = n())
# remember that the 4 days are together 
dtaGirlsDaysMean %>% mutate(total = sum(mean))
5760 / 4

# spread into a matrix format 
dtaGirlsDaysMean = spread(dtaGirlsDaysMean, value, mean, fill = 0) 
dtaGirlsDaysMean
# log the data 
dtaGirlsDaysLog = apply(dtaGirlsDaysMean[,-1], 2, log)
# Inf to 0 
dtaGirlsDaysLog [dtaGirlsDaysLog == '-Inf'] = 0
# 
hist(dtaGirlsDaysLog[,1])

colnumber( as.data.frame(dtaGirlsDaysLog) )
cor(as.data.frame(dtaGirlsDaysLog))

# id as rows 
row.names(dtaGirlsDaysLog) = dtaGirlsDaysMean$pid

########
library(FactoMineR)
########

# factor PCA 
head(dtaGirlsDaysLog)
# 

dtaGirlsDaysPCA = PCA(dtaGirlsDaysLog[,c(1,2,5,6,8,9,10,11,12)]) 
quartz()
plot.PCA(dtaGirlsDaysPCA)
plot.PCA(dtaGirlsDaysPCA, choix = 'var')

# HC 
dtaGirlsDaysHC = HCPC(dtaGirlsDaysPCA, nb.clust = 5, order = F)
# plots 
quartz()
plot.HCPC(dtaGirlsDaysHC, choice = 'map', draw.tree = F)
# variable description 
dtaGirlsDaysHC$desc.var

# ind most closest to cluster center 
dtaGirlsDaysHC$desc.ind$para

# Cluster 1 - Paid Work 
filter(dtaGirlsDaysMean, pid == 'B13253U')
filter(dtaGirlsDaysMean, pid == 'B12699S')

# Cluster 3 - School  
filter(dtaGirlsDaysMean, pid == 'B10648X')
filter(dtaGirlsDaysMean, pid == 'B27937N')

# Cluster 4 - TV
df( filter(dtaGirlsDaysMean, pid == 'B28742H') )
df( filter(dtaGirlsDaysMean, pid == 'B11598J') )

# Cluster 6 - Religion
df( filter(dtaGirlsDaysMean, pid == 'B16848D') )
df( filter(dtaGirlsDaysMean, pid == 'B23127U') )

########################################
########################################

clusters = dtaGirlsDaysHC$data.clust$clust 
levels(clusters) = c('Paid_Work', 'School', 'TV', 'Religion', 'Domestic')
table(clusters, dtaGirlsDaysHC$data.clust$clust )

# merge dataset  
dtaGirlsDaysClust = cbind(dtaGirlsDaysMean, clusters)
# melt 
dtaGirlsDaysClust_melt = dtaGirlsDaysClust %>%  melt(id.vars = c('pid', 'clusters'))
#
# spread 
dtaGirlsDaysClust_spread = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% spread(clusters, meanv)
dtaGirlsDaysClust_spread

# melt 
dtaGirlsDaysClust_melt_agg = dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value))
# rescaled 
library(scales)
# scale by clusters or variable ??    
dtaGirlsDaysClust_melt_agg = dtaGirlsDaysClust_melt_agg %>% group_by(clusters) %>% mutate(meanv_res = cp(meanv, NULL)) 
# check 
spread(select(dtaGirlsDaysClust_melt_agg, -meanv), clusters, meanv_res)
###  
kol = brewer.pal(6,"Oranges")
quartz()
ggplot(filter(dtaGirlsDaysClust_melt_agg, variable != 'Personal care'), aes(clusters, variable)) + geom_tile(aes(fill = meanv), colour = "white") + geom_text(aes(fill = meanv, label = round(meanv, 0))) + scale_fill_gradient(low = "white", high = "red") + theme_minimal()
dtaGirlsDaysClust_melt_agg

#### ####
#### ggplot 
#### ####
quartz()
kol = brewer.pal(12, 'Set3')
kol = brewer.pal(6,"Blues")
dtaGirlsDaysClust_melt %>% group_by(clusters, variable) %>% summarise(meanv = mean(value)) %>% ggplot(aes(x = clusters, fill = factor(clusters), y = meanv)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=c(kol))  + theme_minimal() + facet_grid(.~variable) 

#########
#########
# save dataset 
# save(dtaGirlsDaysClust, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaGirlsDaysClust.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dtaGirlsDaysClust.RData') 

