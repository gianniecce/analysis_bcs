
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
dta2 

#
as.data.frame(table(dta2$act_rec)) 
dta2$act_rec[dta2$act_rec == '3401'] = 'Cultural leisure'
dta2$act_rec[dta2$act_rec == 'School'] = 'Study'
# check 
as.data.frame(table(dta2$act_rec)) 

dtBoys = dta2 %>% 
  filter(SEX == 1 & diaryday == 1) %>% 
  group_by(act_rec, pid) %>% 
  summarise(sdur = sum(duration)) %>% 
  group_by() %>%
  arrange(pid) 

arrange(dtBoys, pid) %>% group_by(pid) %>% mutate(sum(sdur))

# 
dtBoysSpread = spread(dtBoys, act_rec, sdur, fill = 0) 
dtBoysSpread = dtBoysSpread[, -c(4,5,8)]
# 
dtboys = df(dtBoysSpread) 
dtboys
# 
dtboys = apply(dtboys[,-1], 2, log)
dtboys [dtboys == '-Inf'] = 0
# 
dtboys

library(FactoMineR)
pcBoysD1 = PCA(dtboys) 

plot.PCA(pcBoysD1, c(1,2), choix = 'var')
plot.PCA(pcBoysD1, c(1,2), choix = 'ind')

plot.PCA(pcBoysD1, c(2,3), choix = 'var')
plot.PCA(pcBoysD1, c(2,3), choix = 'ind')

#

dtBoys = dta2 %>% 
  filter(SEX == 1) %>% 
  group_by(act_rec, pid) %>% 
  summarise(sdur = sum(duration)) %>% 
  group_by() %>%
  arrange(pid) 

arrange(dtBoys, pid) 

dtBoys %>% group_by(pid) %>% summarise(sum(sdur) / 4)

# 
dtBoysSpread = spread(dtBoys, act_rec, sdur, fill = 0) 
dtBoysSpread = dtBoysSpread[, -c(4,5,8)]

dtBoysSpread

dtboys = apply(dtBoysSpread[,-1], 2, log)
dtboys [dtboys == '-Inf'] = 0
# 
dtboys

library(FactoMineR)
pcBoys = PCA(dtboys) 
hc = HCPC(pcBoys, order = F, nb.clust = 6)

clustersHC = hc$call$X$clust

dtBoysSpread_clus = cbind(dtBoysSpread, clustersHC)

plot.PCA(pcBoys, c(1,2), choix = 'var')
plot.PCA(pcBoys, c(1,2), choix = 'ind')

plot.PCA(pcBoys, c(2,3), choix = 'var')
plot.PCA(pcBoys, c(2,3), choix = 'ind')

dtBoysSpread_clus = as.data.frame(dtBoysSpread_clus)
head(dtBoysSpread_clus)

# save(dtBoysSpread_clus, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtBoysSpread_clus.RData')
# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtBoysSpread_clus.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/bc_seq.RData')
# 
head(bc_seq) 
bc_seq$id = row.names(bc_seq)

nrow(dtBoysSpread_clus)

bc_seq$clusters = 'none'
bc_seq$clusters [ bc_seq$id %in% dtBoysSpread_clus$pid ] = dtBoysSpread_clus$clustersHC [ match( bc_seq$id [ bc_seq$id %in% dtBoysSpread_clus$pid ] , dtBoysSpread_clus$pid ) ] 
# 
bc_seq2 = subset(bc_seq, clusters != 'none')
head(bc_seq2)
# 
bc_seq2_seqDef = seqdef(bc_seq2[,-c(26, 27)] )
# 
quartz()
seqdplot(bc_seq2_seqDef, border = NA, cex.legend = 0.5)
seqdplot(bc_seq2_seqDef, border = NA, cex.legend = 0.5, group = bc_seq2$clusters)

# 
bcs_dist = seqdist(bc_seq2_seq, method = 'OM', with.missing = T, sm = 'CONSTANT')
#
bcshclust = hclust(d = as.dist(bcs_dist), method = 'ward.D')
plot(bcshclust)

bcs_clust = cutree(bcshclust, 5)
bcs_clust

seqdplot(bc_seq2_seq, group = bcs_clust)


