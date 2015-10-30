
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

source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

######## 

# write.csv(as.data.frame(table(bcs70_actII$jactiv)), file = '/Users/giacomovagni/Rprojects/analysis_bcs/doc/jactiv_rec.csv') 
act = read.csv(file = '/Users/giacomovagni/Rprojects/analysis_bcs/doc/jactiv_rec.csv') 
act$recode = as.character(act$recode)
act$Var1 = as.character(act$Var1)

# read dta 
# bcs70_part = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/6941STATA9/stata9/bcs70_partner_histories.dta')
bcs70_act = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/6943STATA9/stata9/bcs70_activity_histories.dta')

bcs70_actI = bcs70_act %>% group_by(bcsid) %>% mutate(nepisode = n()) %>% filter(nepisode > 1) 

bcs70_actI = bcs70_actI %>% group_by(bcsid) %>% mutate(ep = 1:n())

bcs70_actII = bcs70_actI %>% group_by(bcsid) %>% mutate(yearstart = ifelse(jstyr[ep == 1] == 1986, 1, 0)) %>% filter(yearstart == 1)
bcs70_actII

table(bcs70_actII$yearstart) 
select(bcs70_actII, jstyr, jendyr, jactiv)

# recode 
bcs70_actII$jactiv_rec = as.character(bcs70_actII$jactiv)
# 
bcs70_actII$jactiv_rec [ bcs70_actII$jactiv %in% act$Var1 ] = act$recode [ match(bcs70_actII$jactiv [ bcs70_actII$jactiv %in% act$Var1 ], act$Var1 ) ] 

table(bcs70_actII$jactiv_rec)
table(bcs70_actII$jactiv)

bcs70_actII = bcs70_actII %>% group_by()

bcs70 = select(bcs70_actII, bcsid, jstyr, jendyr, jactiv_rec) %>% arrange(bcsid)
bcs70$jendyr [bcs70$jendyr == -6] = 2010

bc = bcs70[, ]
as.data.frame(bc)

# bc[ bc$jstyr > bc$jendyr, ] 

bc_seq = seqformat(bc, id = 'bcsid', begin = 'jstyr', end = 'jendyr', status = 'jactiv_rec', process = F, from = "SPELL", to = "STS", pvar = c("idpers")) 
nrow(bc_seq)
#
# save(bc_seq, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/bc_seq.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/bc_seq.RData')
# 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData')
# 

# id 
bc_seq$id = row.names(bc_seq)
#  

# subset the correct id 
bc_seq2 = bc_seq[ bc_seq$id %in% unique(dta2$pid), ] 
# 
nrow(bc_seq2)
# 
bc_seq2_seq = seqdef(bc_seq2[,-c(26)] )
# 
seqdplot(bc_seq2_seq, border = NA, cex.legend = 0.5)

# 
bcs_dist = seqdist(bc_seq2_seq, method = 'OM', with.missing = T, sm = 'CONSTANT')
#
bcshclust = hclust(d = as.dist(bcs_dist), method = 'ward.D')
plot(bcshclust)

bcs_clust = cutree(bcshclust, 5)
bcs_clust

seqdplot(bc_seq2_seq, group = bcs_clust)
# recoding employment according to class - by gender 

# parent's edu 

bcs70_class = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/bcs3derived0_16X.dta')

head(bcs70_class) 

