
event = stata.get('/Users/giacomovagni/Dropbox/BCS70_86 to G/alldata/original/bcs4_time_diaries_event.dta')

dta3 %>% group_by(SEX) %>% mutate(n_sex = n_distinct(pid)) %>% group_by() %>% group_by(diaryday, act_rec, SEX, n_sex) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = sdur / n_sex) %>% group_by(diaryday, SEX) %>% mutate(sum(meanduration)) %>% group_by()


recode_modalities('dta3$WHOWITH_orig', variable = 'dta3$withwhom', var = factor(dta3$WHOWITH_orig))

table(dta3$WHOWITH_orig)

dta3$withwhom[dta3$WHOWITH_orig== "1"] <- "partner"
  dta3$withwhom[dta3$WHOWITH_orig== "10"] <- "partner"
  dta3$withwhom[dta3$WHOWITH_orig== "11"] <- ""
  dta3$withwhom[dta3$WHOWITH_orig== "12"] <- "siblings"
  dta3$withwhom[dta3$WHOWITH_orig== "13"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "14"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "15"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "16"] <- "with children"
  dta3$withwhom[dta3$WHOWITH_orig== "17"] <- "lover"
  dta3$withwhom[dta3$WHOWITH_orig== "18"] <- "one friend"
  dta3$withwhom[dta3$WHOWITH_orig== "19"] <- "more than one friend"
  dta3$withwhom[dta3$WHOWITH_orig== "2"] <- "with children"
  dta3$withwhom[dta3$WHOWITH_orig== "20"] <- "neighbours"
  dta3$withwhom[dta3$WHOWITH_orig== "21"] <- "colleagues"
  dta3$withwhom[dta3$WHOWITH_orig== "22"] <- "voluntary group"
  dta3$withwhom[dta3$WHOWITH_orig== "24"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "25"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "26"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "27"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "28"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "29"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "3"] <- "siblings"
  dta3$withwhom[dta3$WHOWITH_orig== "30"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "31"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "32"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "33"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "34"] <- "other/service"
  dta3$withwhom[dta3$WHOWITH_orig== "35"] <- "parents"
  dta3$withwhom[dta3$WHOWITH_orig== "36"] <- "alone"
  dta3$withwhom[dta3$WHOWITH_orig== "4"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "5"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "6"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "7"] <- "misc"
  dta3$withwhom[dta3$WHOWITH_orig== "8"] <- "kin"
  dta3$withwhom[dta3$WHOWITH_orig== "9"] <- "misc"
  
table(dta3$withwhom)

dta3Summary = dta3 %>% group_by(diaryday, withwhom) %>% summarise(sdur = sum(duration)) %>% mutate(meanduration = round( sdur / n, 2) ) %>% mutate(sum(meanduration))

dta3SumSpread = df( spread( select(dta3Summary, diaryday, withwhom, meanduration), diaryday, meanduration) )
tclock = apply(X = dta3SumSpread[,-1], MARGIN = 2, FUN = TimeClock)
dta3SumSpread = as.data.frame( cbind(Activities = dta3SumSpread[,1], tclock)) 
dta3SumSpread

# all days - boys 
dta = select(dta3, id = id, av = withwhom, time = duration)
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
kol = c(brewer.pal(3, 'Set1'), brewer.pal(12, 'Set3')) 

# Friday
ggplot(filter(seqDay4, diaryday == 1), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Friday')
# Saturday
ggplot(filter(seqDay4, diaryday == 2), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Friday')
# Sunday
ggplot(filter(seqDay4, diaryday == 3), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Friday')
# Monday
ggplot(filter(seqDay4, diaryday == 3), aes(x = Time, fill = factor(av), y = SharePerHour)) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + guides(fill = guide_legend(reverse = TRUE)) + ggtitle('Friday')
