
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
######## 

source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

#
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dataWide.RData') 
# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData')
dt = dta2

nd = n_distinct(dt$pid)

dtboy = filter(dt, SEX == 1)
# boys 
nd = n_distinct(dtboy$pid)

# Boys 
dtaSummaryD1 = dt %>% filter(diaryday == 1 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 1) 
dtaSummaryD2 = dt %>% filter(diaryday == 2 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 2)
dtaSummaryD3 = dt %>% filter(diaryday == 3 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 3)
dtaSummaryD4 = dt %>% filter(diaryday == 4 & SEX == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 4) 

dtaSummaryBoys = rbind(select(dtaSummaryD1, act_rec, meantime, day),
                       select(dtaSummaryD2, act_rec, meantime, day),
                       select(dtaSummaryD3, act_rec, meantime, day),
                       select(dtaSummaryD4, act_rec, meantime, day))

kable(dtaSummaryBoys %>% spread(day, meantime), digits = 1, caption = 'Boys - mean time by act per day') 


dtgirl = filter(dt, SEX == 2)
# girls 
nd = n_distinct(dtgirl$pid)

# Girls 
dtaSummaryD1 = dt %>% filter(diaryday == 1 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 1) 
dtaSummaryD2 = dt %>% filter(diaryday == 2 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 2)
dtaSummaryD3 = dt %>% filter(diaryday == 3 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 3)
dtaSummaryD4 = dt %>% filter(diaryday == 4 & SEX == 2) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid) %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop), day = 4) 

dtaSummaryGirls = rbind(select(dtaSummaryD1, act_rec, meantime, day),
                        select(dtaSummaryD2, act_rec, meantime, day),
                        select(dtaSummaryD3, act_rec, meantime, day),
                        select(dtaSummaryD4, act_rec, meantime, day))

# sum(dtaSummaryGirls$meantime) /4

kable(dtaSummaryGirls %>% spread(day, meantime), digits = 1, caption = 'Girls - mean time by act per day') 

dtSum = rbind(cbind(dtaSummaryBoys, sex = 'boy'), cbind(dtaSummaryGirls, sex = 'girl'))

# sum( dtSum$meantime ) / 8

ggplot(dtSum, aes(sex, meantime, fill = factor(act_rec)) ) + ylab('') + facet_grid(.~day) + 
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 

# day 1 

kol = c('black', 'red', brewer.pal(12, 'Set3')) 
#
d1 = dataWide[, grepl('DAY1', names(dataWide)) ] 
d1_seq = seqdef(d1, cpal = kol)

layout(rbind(c(1,1,2), c(1,1,2))) 

seqdplot(d1_seq, border = NA, withlegend = F)
seqlegend(d1_seq)

# day2

kol = c('black', 'red', brewer.pal(12, 'Set3')) 
#
d2 = dataWide[, grepl('DAY2', names(dataWide)) ] 
d2_seq = seqdef(d2, cpal = kol)


layout(rbind(c(1,1,2), c(1,1,2))) 

seqdplot(d2_seq, border = NA, withlegend = F)
seqlegend(d2_seq)

# day3

kol = c('black', 'red', brewer.pal(12, 'Set3')) 
#
d3 = dataWide[, grepl('DAY3', names(dataWide)) ] 
d3_seq = seqdef(d3, cpal = kol)

layout(rbind(c(1,1,2), c(1,1,2))) 

seqdplot(d3_seq, border = NA, withlegend = F)
seqlegend(d3_seq)


# day4

kol = c('black', 'red', brewer.pal(12, 'Set3')) 
#
d4 = dataWide[, grepl('DAY4', names(dataWide)) ] 
d4_seq = seqdef(d4, cpal = kol)

layout(rbind(c(1,1,2), c(1,1,2))) 

seqdplot(d4_seq, border = NA, withlegend = F)
seqlegend(d4_seq)


