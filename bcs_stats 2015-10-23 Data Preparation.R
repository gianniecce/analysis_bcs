
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
########

source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)

################################################################################################
######################################### Data 1 : Time Use Data ############################### 
################################################################################################

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

# subset the days and month 
days = c(1,2,3,4)
dta = dta[ dta$diaryday %in% days, ] 

# subset the days and month 
months = c(6,7,8)
dta = dta[ dta$month %in% months, ] 

# diaries with less than 5 episodes 
# group must be id 
dta = dta %>% 
  group_by(id) %>% 
  mutate(baddiary = ifelse(maxep < 5, 1, 0)) 

# 
# K idea of checking for the 4 filled days 
# 

dta = dta %>% group_by()

dta$diarydayD1 = ifelse(dta$diaryday == 1, 1, 0)
dta$diarydayD2 = ifelse(dta$diaryday == 2, 1, 0)
dta$diarydayD3 = ifelse(dta$diaryday == 3, 1, 0)
dta$diarydayD4 = ifelse(dta$diaryday == 4, 1, 0)

select(dta, diaryday, diarydayD1, diarydayD2, diarydayD3, diarydayD4)

dta = dta %>% group_by(pid) %>% mutate(diarydayCheck4days = paste( max( diarydayD1), max(diarydayD2), max(diarydayD3), max(diarydayD4), sep = '-')) 
# check = select(dta, diaryday, diarydayD1, diarydayD2, diarydayD3, diarydayD4, diarydayCheck4days)
# table(check$diarydayCheck4days)
dta = filter(dta, diarydayCheck4days == '1-1-1-1')

# watch out the group ! 
# here is pid ! 
dta = dta %>% 
  group_by(pid) %>% 
  #  mutate(nbrdiaries = max(as.numeric(diaryday)) ) %>% # see if they filled the 4 days 
  mutate(diariesBeginFirstDay = ifelse(diaryday[1] == 1, 1, 0)) # begin first day or not 

# diaries with less than 5 episodes 
dta = dta %>% 
  group_by(id) %>% 
  filter(baddiary == 0) %>% 
  #  filter(nbrdiaries == 4) %>% 
  filter(diariesBeginFirstDay == 1) 

length(unique(dta$pid)) 

check = dta %>% group_by(id) %>% distinct(diaryday) %>% mutate(sm= sum( as.numeric(diaryday) )) %>% group_by(pid) %>% summarise(sum(sm))
table(check$`sum(sm)`)

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

# 4962
length(unique(dta$pid))

# 2015-10-18 
# save(dta, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta.RData')

################## 
################## Wide to Long 
################## 

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
# remove blank at the end of the word 
dt$act_rec = gsub(pattern = "[[:blank:]]$", '', dt$act_rec)
# 
head(select(dt, MAINACT_orig, main, av, act_rec))
#

cp(table(dt$act_rec), NULL) 

dta2 = dt

# save(dta2, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta2.RData')
n_distinct(dt$pid)

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
# save(dataWide, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide.RData')

nrow(dataWide)

########
# merge both dataset 
########  

dt_sub = distinct(select(dta2, pid, SEX))

nrow(dt_sub)
nrow(dataWide) 
# 
dataWide2 = merge(dt_sub, dataWide, by.x = 'pid', by.y = 'id') 
# 
n_distinct(dataWide2$pid) # 
# 
table(dataWide2$SEX)

# save(dataWide2, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide2.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide2.RData')

################################################################################################
######################################### Data 2 : employment traject ##########################
################################################################################################

# here watch out because you are also subsetting the individual that didnot begin filling ==== ifelse(jstyr[ep == 1] == 1986, 1, 0))

# write.csv(as.data.frame(table(bcs70_actII$jactiv)), file = '/Users/giacomovagni/Rprojects/analysis_bcs/doc/jactiv_rec.csv') 
act = read.csv(file = '/Users/giacomovagni/Rprojects/analysis_bcs/doc/jactiv_rec.csv') 
act$recode = as.character(act$recode)
act$Var1 = as.character(act$Var1)

# read dta 
# bcs70_part = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/6941STATA9/stata9/bcs70_partner_histories.dta')
bcs70_act = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/6943STATA9/stata9/bcs70_activity_histories.dta')

bcs70_actI = bcs70_act %>% group_by(bcsid) %>% mutate(nepisode = n()) %>% filter(nepisode > 1) 

bcs70_actI = bcs70_actI %>% group_by(bcsid) %>% mutate(ep = 1:n())

# susbet the people who beginning filling in 1986 ! 
bcs70_actII = bcs70_actI %>% group_by(bcsid) %>% mutate(yearstart = ifelse(jstyr[ep == 1] == 1986, 1, 0)) %>% filter(yearstart == 1)
bcs70_actII
# check 
table(bcs70_actII$yearstart) 
select(bcs70_actII, jstyr, jendyr, jactiv)

# recode the employment status ! 
bcs70_actII$jactiv_rec = as.character(bcs70_actII$jactiv)
# 
bcs70_actII$jactiv_rec [ bcs70_actII$jactiv %in% act$Var1 ] = act$recode [ match(bcs70_actII$jactiv [ bcs70_actII$jactiv %in% act$Var1 ], act$Var1 ) ] 

# check 
table(bcs70_actII$jactiv_rec)
table(bcs70_actII$jactiv)

bcs70 = select(bcs70_actII, bcsid, jstyr, jendyr, jactiv_rec) %>% arrange(bcsid)
bcs70$jendyr [bcs70$jendyr == -6] = 2010

bc = bcs70[, ]
as.data.frame(bc)

library(TraMineR)
bc_seq = seqformat(bc, id = 'bcsid', begin = 'jstyr', end = 'jendyr', status = 'jactiv_rec', process = F, from = "SPELL", to = "STS", pvar = c("idpers")) 
nrow(bc_seq)
#
# save(bc_seq, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/bc_seq.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/bc_seq.RData')
#  

#
head(bc_seq)
#
nrow(bc_seq)
#
bc_seq$id = row.names(bc_seq)

################################################################################################
######################################### Data 2 : employment traject ##########################
################################################################################################ 

# parent's edu 

bcs70_parents = read.dta('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/bcs3derived0_16X.dta')
head(bcs70_parents) 

library(Hmisc)
sg = stata.get('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/otherBCSdata/bcs3derived0_16X.dta')
label(sg)
attributes(sg)

bcs70_parents = select(bcs70_parents, bcsid, BD4PSOC, BD1PSOC, BD2SOC)
#
bcs70 = merge(bcs70_parents, bc_seq, by.x = 'bcsid', by.y = 'id')

# save(bcs70, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/bcs70.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/bcs70.RData') 

table(bcs70$BD4PSOC, bcs70$y1986) 
table(bcs70$y1986, bcs70$y2010)

summary( glm(bcs70$y1986 == 'Education' ~ bcs70$BD2SOC, binomial) )
summary( glm(bcs70$y1986 == 'Full Time' ~ bcs70$BD2SOC, binomial) )

summary( glm(bcs70$y2010 == 'Higher managerial and professional occupations' ~ bcs70$BD2SOC, binomial) )

t( as.data.frame(names(bcs70)) ) 

bcs70[,5:29]

################################################################################################
######################################### CLEANING ############################################# 
################################################################################################ 

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide2.RData') 
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/bcs70.RData') 

head(bcs70)
head(dataWide2)

bcs70_sub = bcs70[ bcs70$bcsid %in% dataWide2$pid, ] 
dataWide2 = merge(dataWide2, bcs70_sub, by.x = 'pid', by.y = 'bcsid')
# 
n_distinct(dataWide2$pid)

dataWide3 = dataWide2

# save(dataWide3, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide3.RData')

n_distinct(dataWide3$pid)
table(dataWide3$SEX)

################################################################################################
######################################### Merge with Original ################################## 
################################################################################################ 

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dataWide3.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta2.RData')

n_distinct(dataWide3$pid)
n_distinct(dta2$pid)

dta3 = dta2[dta2$pid %in% dataWide3$pid, ]
table(unique(dta3$pid) == dataWide3$pid) 

# save(dta3, file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta3.RData')
load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new2/dta3.RData')

###############################################################################
###### Now we want to create a more complex variable including class 
###### ########################################################################

t = select(bcs70_actII, J8NSSEC, jactiv_rec)
bcs70_actII %>% group_by()

# 
bcs70_actII$class = as.character(bcs70_actII$jactiv_rec )
bcs70_actII$J8NSSEC = as.character(bcs70_actII$J8NSSEC )
# 
table(bcs70_actII$jactiv_rec)
table(bcs70_actII$J8NSSEC)

head(bcs70_actII)

# education 
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Higher managerial and professional occupations'] = 'Higher managerial and professional occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Intermediate occupations'] = 'Intermediate occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Lower managerial and professional occupations'] = 'Lower managerial and professional occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Lower supervisory and technical occupations'] = 'Lower supervisory and technical occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Routine occupations'] = 'Routine occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC ==  'Semi-routine occupations'] = 'Semi-routine occupations'
bcs70_actII$class[ bcs70_actII$J8NSSEC == 'Small Employers and Own account workers'] = 'Small Employers and Own account workers'

table(bcs70_actII$class)
# 2 times ! 
bcs70_actII$class = gsub(pattern = "^[[:blank:]]$", replacement = '', x = bcs70_actII$class)

bcs70_actII$class[ bcs70_actII$class == 'Retired'] = 'Looking after home family'
bcs70_actII$class[ bcs70_actII$class == 'other' | bcs70_actII$class == 'bad' |  bcs70_actII$class == 'Sick' ] = 'unknown'
table(bcs70_actII$class)

select(bcs70_actII, class, jactiv_rec)

bcs70_actII = bcs70_actII %>% group_by()

bcs70 = select(bcs70_actII, bcsid, jstyr, jendyr, class) %>% arrange(bcsid)
bcs70$jendyr [bcs70$jendyr == -6] = 2010

bc = bcs70[, ]
as.data.frame(bc)

# bc[ bc$jstyr > bc$jendyr, ] 

# we must abandon our class idea and focus on employment 
