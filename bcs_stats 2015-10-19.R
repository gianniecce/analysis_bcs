library(mtusRlocal) # load the package 

# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library('ggplot2')
library(RColorBrewer)
require(gridExtra)
library(TraMineR)
########  
source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta1.RData')
dt = dta1

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData')
dt = dta2

nd = n_distinct(dt$pid)

dtaSummaryD1 = dt %>% filter(diaryday == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 1) 
dtaSummaryD2 = dt %>% filter(diaryday == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 2)
dtaSummaryD3 = dt %>% filter(diaryday == 3) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 3)
dtaSummaryD4 = dt %>% filter(diaryday == 4) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 4) 

dtaSummary = rbind(select(dtaSummaryD1, act_rec, meantime, day), 
                   select(dtaSummaryD2, act_rec, meantime, day),
                   select(dtaSummaryD3, act_rec, meantime, day), 
                   select(dtaSummaryD4, act_rec, meantime, day))

dtdt = dtaSummary %>% spread(day, meantime)
dtdt

# neat 
ggplot(dtaSummary, aes(day, meantime, fill = factor(act_rec))) + ylab('') +
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

# neat 
ggplot(dtaSummaryD1, aes(act_rec, meantime, fill = act_rec)) + ylab('') +
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

ggplot(dtaSummaryD2, aes(act_rec, meantime, fill = act_rec)) + ylab('') +
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

# boys 

dtaSummaryD1 = dt %>% filter(diaryday == 1 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 1) 
dtaSummaryD2 = dt %>% filter(diaryday == 2 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 2)
dtaSummaryD3 = dt %>% filter(diaryday == 3 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 3)
dtaSummaryD4 = dt %>% filter(diaryday == 4 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 4) 

# neat 
gboys = ggplot(dtaSummary, aes(act_rec, meantime, fill = factor(act_rec)) )  + ylab('') + facet_grid(.~day) + 
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

# girls 

dtaSummaryD1 = dt %>% filter(diaryday == 1 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 1) 
dtaSummaryD2 = dt %>% filter(diaryday == 2 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 2)
dtaSummaryD3 = dt %>% filter(diaryday == 3 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 3)
dtaSummaryD4 = dt %>% filter(diaryday == 4 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 4) 

# neat # 
ggirls = ggplot(dtaSummary, aes(act_rec, meantime, fill = factor(act_rec)) )  + ylab('') + facet_grid(.~day) + 
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

#
# grid.arrange(p2, p1, p3, ncol=3) 
#
 
quartz()
gboys
quartz()
ggirls

######
# Day 1
######
dataD1 = dt %>% filter(diaryday == 1) %>% mutate(time = as.numeric(duration)) %>% select(id, av = act_rec, time) %>% as.data.frame()
# long to wide 
dataWideD1 = TimeLongToWide(dta = dataD1)

######
# Day 2
######
dataD2 = dt %>% filter(diaryday == 2) %>% mutate(time = as.numeric(duration)) %>% select(id, av = act_rec, time) %>% as.data.frame()
# long to wide 
dataWideD2 = TimeLongToWide(dta = dataD2)

######
# Day 3
######
dataD3 = dt %>% filter(diaryday == 3) %>% mutate(time = as.numeric(duration)) %>% select(id, av = act_rec, time) %>% as.data.frame()
# long to wide 
dataWideD3 = TimeLongToWide(dta = dataD3)

######
# Day 4
######
dataD4 = dt %>% filter(diaryday == 4) %>% mutate(time = as.numeric(duration)) %>% select(id, av = act_rec, time) %>% as.data.frame()
# long to wide 
dataWideD4 = TimeLongToWide(dta = dataD4)
# 

nrow(dataWideD1)
nrow(dataWideD2)
nrow(dataWideD3)
nrow(dataWideD4)

dataWideD1[1:5, 1:5]
dataWideD2[1:5, 1:5]
dataWideD3[1:5, 1:5]
dataWideD4[1:5, 1:5]

# 
colnames(dataWideD1) = paste('DAY1_ep', colnames(dataWideD1), sep = '') 
colnames(dataWideD2) = paste('DAY2_ep', colnames(dataWideD2), sep = '') 
colnames(dataWideD3) = paste('DAY3_ep', colnames(dataWideD3), sep = '') 
colnames(dataWideD4) = paste('DAY4_ep', colnames(dataWideD4), sep = '') 

dataWideD1$id = row.names(dataWideD1)
dataWideD2$id = row.names(dataWideD2)
dataWideD3$id = row.names(dataWideD3)
dataWideD4$id = row.names(dataWideD4)

dataWideD1$id = substring(dataWideD1$id, 1, 7) 
dataWideD2$id = substring(dataWideD2$id, 1, 7) 
dataWideD3$id = substring(dataWideD3$id, 1, 7) 
dataWideD4$id = substring(dataWideD4$id, 1, 7) 

a = merge(dataWideD1, dataWideD2, "id")
b = merge(a, dataWideD3, "id")
dataWide = merge(b, dataWideD4, "id")
# dataWide = merge(c, dataWideD4, "id")

head(dataWide) 
# save(dataWide, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dataWide.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dataWide.RData') 

# 
dta2 = dt [dt$pid %in% dataWide$id, ] 
# 
save(dta2, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData') 

############################
############## Sequences ### 
############################

# 
kol = c('black', 'red', brewer.pal(12, 'Set3')) 
# 
dataWideSeq = seqdef(dataWide[,-1], cpal = kol ) 
# 
layout(rbind(c(1,1,2), 
       c(1,1,2))) 
seqdplot(dataWideSeq, border = NA, withlegend = F)
seqlegend(dataWideSeq)


table(dt_sub$month) 





