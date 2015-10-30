

```{r, echo=FALSE, message=FALSE, warning=FALSE}
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

# all days - boys 
dta = select(dta3, id = id, av = act_rec, time = duration)
dta = as.data.frame(dta)

# sequence generation 
seqDay = dta[rep(1:nrow(dta), dta[,'time'] ), -3] %>%
  group_by(id) %>% 
  mutate( Time = 1:n() ) 

# retreive pid and diaryday  
seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by() 
# seqDay2 = seqDay %>% group_by(Time, av, diaryday) %>% summarise(n = n())

# mean by individual 
seqDay %>% group_by(pid, av, diaryday) %>% summarise(meanpid = n()) %>% group_by(pid, diaryday) %>% mutate(sum(meanpid))
# aggregate mean - correct / divided by the boys number 
seqDay %>% group_by(av, diaryday) %>% summarise(meanav = n() / 4033) %>% group_by(diaryday) %>% mutate(sum(meanav))

# aggregate by minutes of sequences ! 
seqDay2 = select(seqDay, av, Time, diaryday, pid)
#
seqDay3 = seqDay2 %>% group_by(Time, diaryday, av) %>% summarise(n = n()) 
# 
seqDay4 = seqDay3 %>% group_by(Time, diaryday) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)
seqDay4

###
kol = brewer.pal(12, 'Set3')

# Friday
ggplot(filter(seqDay4, diaryday == 1), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Friday')
# Saturday
ggplot(filter(seqDay4, diaryday == 2), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Saturday')
# Sunday 
ggplot(filter(seqDay4, diaryday == 3), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Sunday')
# Monday 
ggplot(filter(seqDay4, diaryday == 4), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Monday')

# + facet_grid(.~diaryday) +


