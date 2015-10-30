
library(mtusRlocal) # load the package 

# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library('ggplot2')
######## 

# source_https <- function(url, ...) { require(RCurl) sapply(c(url, ...), function(u) { eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv) }) }
# source_https('https://raw.githubusercontent.com/gianniecce/analysis_bcs/master/rscripts/source_timetolongfunction.R')
source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

#
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta.RData')
# 

actmatch = read.csv(file = "/Users/giacomovagni/Rprojects/analysis_bcs/doc/actmatch.csv")
actmatch$original = as.character(actmatch$original)
actmatch$recode = as.character(actmatch$recode)

#
actmatch$originalgood = gsub('\\D+','', actmatch$original)
actmatch

#
dta = dta %>% group_by()
# 
dt = dta
dt$act_rec = dt$MAINACT_orig  

#
dt$act_rec [ dt$MAINACT_orig %in% actmatch$originalgood ] = actmatch$recode [ match(dt$MAINACT_orig [dt$MAINACT_orig %in% actmatch$originalgood ], actmatch$originalgood ) ] 
head(select(dt, MAINACT_orig, main, av, act_rec))
#

# save(dta1, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta1.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta1.RData')
dt = dta1

n_distinct(dta1$pid)

day = 1

# 
dtaSummary = dt %>% filter(diaryday == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid)
dtaSummary 

# good 
sum( dtaSummary$sdur ) / n_distinct(dtaSummary$pid)

# divid = n_distinct(dta$pid) * n_distinct(dta$diaryday)
length(unique(dtaSummary$pid)) 

# check if everybody sums up to 1440 
# dt = dtaSummary %>% group_by(pid, diaryday) %>% summarise(sdt = sum(sdur))

nd = n_distinct(dtaSummary$pid)

dt = dtaSummary %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% 
  mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop) ) 

# dtaSummary = dt %>% filter(diaryday == 1) %>% group_by(act_rec, pid, diaryday) %>% summarise(sdur = sum(duration)) %>% arrange(pid)
# nd = n_distinct(dtaSummary$pid)
# dtaSummary %>% group_by(act_rec) %>% summarise(sdt = sum(sdur)) %>% mutate(meantime = sdt / nd) %>% mutate(total = sum(sdt) / nd ) %>% mutate(prop = cp(sdt, NULL, digits = 4) ) %>% mutate(totalprop = sum(prop) ) 

library(RColorBrewer)
col = c('black', 'white', brewer.pal(12, 'Set3')) 

dt = arrange(dt, meantime)

# neat 
ggplot(dt, aes(act_rec, meantime, fill = act_rec)) + ylab('') +
  geom_bar(stat='identity') + theme(legend.position="none") + theme_minimal()  + scale_fill_manual(values= c('black', 'red', brewer.pal(12, 'Set3')) ) 


### it works 
# Day 1 
d1 = filter(dta, diaryday == 1) 
d1 = select(d1, id, av = main, time = duration)

head(d1) 
dta = as.data.frame(d1)

# good 
#### TimeLongToWideMeanById(dta = d1, byAct = T)

######
###### Check for order of the days and if they all filled the 4 days # check the missing 
######

dta$avTV = ifelse(dta$av == 31, 1, 0)

# Day 1 
dataD1 = filter(dta, diaryday == 1)
dataD1_sex1 = filter(dataD1, SEX == '1')
dataD1_sex1 = as.data.frame(dataD1_sex1)

dataD1_sex1$time = as.numeric(dataD1_sex1$duration)
d1 = select(dataD1_sex1, id, av = avTV, time)

dataWideD1 = TimeLongToWide(dta = d1)

# Day 2
dataD2 = filter(dta, diaryday == 2)
dataD2_sex1 = filter(dataD2, SEX == '1')
dataD2_sex1 = as.data.frame(dataD2_sex1)

dataD2_sex1$time = as.numeric(dataD2_sex1$duration)
d2 = select(dataD2_sex1, id, av = avTV, time)

dataWideD2 = TimeLongToWide(dta = d2)

# Day 3
dataD3 = filter(dta, diaryday == 3)
dataD3_sex1 = filter(dataD3, SEX == '1')
dataD3_sex1 = as.data.frame(dataD3_sex1)

dataD3_sex1$time = as.numeric(dataD3_sex1$duration)
d3 = select(dataD3_sex1, id, av = avTV, time)

dataWideD3 = TimeLongToWide(dta = d3)

# Day 4
dataD4 = filter(dta, diaryday == 4)
dataD4_sex1 = filter(dataD4, SEX == '1')
dataD4_sex1 = as.data.frame(dataD4_sex1)

dataD4_sex1$time = as.numeric(dataD4_sex1$duration)
d4 = select(dataD4_sex1, id, av = avTV, time)

dataWideD4 = TimeLongToWide(dta = d4)

# merge 
colnames(dataWideD1) = paste(colnames(dataWideD1), 'DAY1', sep = '') 
dataWideD1$id = row.names(dataWideD1)
dataWideD1$id = substring(dataWideD1$id, 1, 7) 

colnames(dataWideD2) = paste(colnames(dataWideD2), 'DAY2', sep = '') 
dataWideD2$id = row.names(dataWideD2)
dataWideD2$id = substring(dataWideD2$id, 1, 7) 

colnames(dataWideD3) = paste(colnames(dataWideD3), 'DAY3', sep = '') 
dataWideD3$id = row.names(dataWideD3)
dataWideD3$id = substring(dataWideD3$id, 1, 7) 

colnames(dataWideD4) = paste(colnames(dataWideD4), 'DAY4', sep = '') 
dataWideD4$id = row.names(dataWideD4)
dataWideD4$id = substring(dataWideD4$id, 1, 7) 

dataWideD1 [1:5, 1:5] 
dataWideD2 [1:5, 1:5] 
dataWideD3 [1:5, 1:5] 
dataWideD4 [1:5, 1:5] 

#
dataWideD1D2 = merge( dataWideD1[, ], dataWideD2[, ], by = 'id') 
dataWideD1D2D3 = merge( dataWideD1D2[, ], dataWideD3[, ], by = 'id') 
dataWideD1D2D3D4 = merge( dataWideD1D2D3[, ], dataWideD4[, ], by = 'id') 

dataWideD1D2D3D4[1:5, 'id']
# 

# je pense que l'on peux très bien faire un clustering sur les durées moyennes par jour 
# et ensuite utiliser l'analyse de séquence pour entropie, etc ... 

#

library(TraMineR)
dataWideD1D2D3D4_seq = seqdef(dataWideD1D2D3D4[,-1], cpal = c('white', 'orange')) 
seqdplot(dataWideD1D2D3D4_seq, border = NA)

sample1 = dataWideD1D2D3D4_seq[ sample(size = 10, x = nrow(dataWideD1D2D3D4_seq)), ] 
seqdplot(sample1, border = NA)

dist.s1 = seqdist(sample1, method = 'OM', with.missing=TRUE, sm = 'CONSTANT')

sample1.clusterward <- hclust(as.dist(dist.s1), method = "ward")
plot(sample1.clusterward, labels = FALSE)

sample1.clust = cutree(sample1.clusterward, k = 3)

seqdplot(sample1, border = NA, group = sample1.clust)

