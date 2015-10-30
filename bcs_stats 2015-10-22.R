library(mtusRlocal) # load the package 

# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library('ggplot2')
library(knitr)
library(RColorBrewer)
library(TraMineR)
library(foreign)
######## 

# extract the most typical individual of the clusters ! 

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtBoysSpread_clus.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dataWide.RData') 

dataWide[1:5, 1:5]

# attention c'est les garcons ! 
# c'est pour il y a des missings ! 
dataWide$clusters = 'none'
dataWide$clusters [ dataWide$id %in% dtBoysSpread_clus$pid ] = dtBoysSpread_clus$clustersHC [ match( dataWide$id [ dataWide$id %in% dtBoysSpread_clus$pid ] , dtBoysSpread_clus$pid ) ] 

table(dataWide$clusters [ dataWide$id %in% dtBoysSpread_clus$pid ])
table(dataWide$clusters) 

dataWideBoys = filter(dataWide, clusters != 'none')
dataWideBoys[1:5, 1:5]

dtboys = dataWideBoys %>% melt(id.vars = c('id', 'clusters')) 
head(dtboys)

dtboysSummary = dtboys %>% group_by(clusters, value) %>% summarise(n=n()) %>% group_by() %>% spread(clusters, n) 
dtboysSummary = as.data.frame(dtboysSummary)
dtboysSummary 
  
kol = c('black', 'red', brewer.pal(12, 'Set3')) 
#
d1 = dataWideBoys[, grepl('DAY1', names(dataWideBoys)) ] 
d1_seq = seqdef(d1, cpal = kol)

quartz()
layout(rbind(c(1,1,2), c(1,1,2))) 
seqdplot(d1_seq, border = NA, withlegend = T, group = dataWideBoys$clusters)
# seqlegend(d1_seq)

# 
d2 = dataWideBoys[, grepl('DAY2', names(dataWideBoys)) ] 
d2_seq = seqdef(d2, cpal = kol)

seqdplot(d2_seq, border = NA, withlegend = T, group = dataWideBoys$clusters)

# 
d3 = dataWideBoys[, grepl('DAY3', names(dataWideBoys)) ] 
d3_seq = seqdef(d3, cpal = kol)

seqdplot(d3_seq, border = NA, withlegend = T, group = dataWideBoys$clusters)
seqlegend(d3_seq)

# day4
d4 = dataWideBoys[, grepl('DAY4', names(dataWideBoys)) ] 
d4_seq = seqdef(d4, cpal = kol)

seqdplot(d4_seq, border = NA, withlegend = T, group = dataWideBoys$clusters)
seqlegend(d4_seq)
