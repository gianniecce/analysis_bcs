

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


# Time Use Sequences 
########################################
time_use = dataWide3[, grepl(pattern = 'DAY', x = colnames(dataWide3))] 
time_use$pid = dataWide3$pid
########################################

# colours 
kol = brewer.pal(12, 'Set3')
#
time_seq = seqdef(time_use[,-c(5761)], cpal = kol)
# 
seqdplot(time_seq, border = NA)
# 
which(time_use$pid == 'B19703Z')
which(time_use$pid == 'B22087Z')
which(time_use$pid == 'B16848D')
which(time_use$pid == 'B23127U')

# 
quartz()
seqiplot(time_seq[2032, ], border = NA)
seqiplot(time_seq[c(2032,2555), ], border = NA)

seqiplot(time_seq[c(1442,2778), ], border = NA)

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
