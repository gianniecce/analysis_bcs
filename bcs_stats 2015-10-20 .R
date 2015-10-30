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

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta2.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dtBoysSpread_clus.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dataWide.RData') 

####
bcs70_parents = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/bcs3derived0_16X.dta')
####
# BD4PSOC "1986: Social class from fathers occup (or mothers if missing) (t11.2 + t11.9)   " 
# BD1PSOC "1970: Social class at birth: fathers occup or mothers  (vars a0014 + a0018) "
# BD2SOC "1975: Social class -fathers occup (or mothers if missing) (e197 + e206) " 

# library(Hmisc)
# bcs70_parentsHM = stata.get('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/bcs3derived0_16X.dta')
# attributes(bcs70_parentsHM)
# label(bcs70_parentsHM)


head(bcs70_parents) 

select(bcs70_parents, bcsid, BD4PSOC, BD1PSOC, BD2SOC)
# save(bcs70_parents, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/bcs70_parents.RData') 




