
library(mtusRlocal) # load the package 

# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library('RecordLinkage')
########

source_https <- function(url, ...) { 
  require(RCurl) # load package
  sapply(c(url, ...), function(u) { eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv) }) }

source_https('https://raw.githubusercontent.com/gianniecce/analysis_bcs/master/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

###
# the data 
load(file = '/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/RData/MainFinal/dataXI.RData')
####
# write.csv(dtBad, file = '/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/RData/dtDatesErrorCases.RData')

##
dta = dataXI
dta$duration = as.numeric(dta$duration)

df(filter(dta, pid == 'B11148L')) 
df(filter(dataXI, pid == 'B11148L')) 

# we subset the month and correct days 
cp( table(dta$month), NULL) 
# month  
plot(table(dta$month))
# days 
plot(table(dta$diaryday))

# how many kids sampled 
length(unique(dta$pid)) 

# diaries with less than 5 episodes 
# group must be id 
dta = dta %>% 
  group_by(id) %>% 
  mutate(baddiary = ifelse(maxep < 5, 1, 0)) 

# watch out the group ! 
# here is pid ! 
dta = dta %>% 
  group_by(pid) %>% 
  mutate(nbrdiaries = max(as.numeric(diaryday)) ) %>% # see if they filled the 4 days 
  mutate(diariesBeginFirstDay = ifelse(diaryday[1] == 1, 1, 0)) # begin first day or not 

# diaries with less than 5 episodes 
dta = dta %>% 
  group_by(id) %>% 
  filter(baddiary == 0) %>% 
  filter(nbrdiaries == 4) %>% 
  filter(diariesBeginFirstDay == 1) 

length(unique(dta$pid)) 

# subset the days and month 
days = c(1,2,3,4)
dta = dta[ dta$diaryday %in% days, ] 

# subset the days and month 
months = c(6,7,8)
dta = dta[ dta$month %in% months, ] 

# 
dta %>% group_by() %>% 
  summarise(mean(maxep), sd(maxep), median(maxep))

kable(  dta %>% group_by() %>% summarise(mean = mean(maxep), sd = sd(maxep), median = median(maxep)), caption = "Maximum Episodes")
kable(  dta %>% group_by() %>% mutate(duration = as.numeric(duration)) %>% summarise(mean = mean(duration), sd = sd(duration), median = median(duration)), caption = "Duration") 

kable(  dta %>% group_by(diaryday) %>% mutate(epnum = as.numeric(epnum)) %>% summarise(mean = mean(epnum), sd = sd(epnum), median = median(epnum)), caption = "Epnum by Days") 
kable(  dta %>% group_by(diaryday) %>% mutate(duration = as.numeric(duration)) %>% summarise(mean = mean(duration), sd = sd(duration), median = median(duration)), caption = "Duration by Days") 
# 

cp(table(dta$diaryday), NULL) 
table(dta$month)
table(dta$year)

#####
#####

dtaBad = dta %>% 
  group_by(id) %>% summarise(sdur = sum(duration)) %>% mutate(check = ifelse(sdur == 1440, 1, 0)) 

dtaGood = filter(dtaBad, check == 1)
dta = dta[ dta$id %in% dtaGood$id, ] 

dta = dta %>% group_by(pid) %>% mutate(nbrdiaries2 = max(as.numeric(diaryday)) ) %>% filter(nbrdiaries2 == 4)

table(dta$month) 
table(dta$diaryday) 
table(dta$nbrdiaries) 
table(dta$nbrdiaries2) 
table(dta$duration) 

check = dta %>% group_by(id) %>% summarise(sm = sum(duration))
table(check$sm) 

# 4505
length(unique(dta$pid))

# 2015-10-18 
# save(dta, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta.RData')
# load(dta, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/dta.RData')
