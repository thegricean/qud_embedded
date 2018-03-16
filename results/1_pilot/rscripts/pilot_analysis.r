theme_set(theme_bw(18))
require(tidyverse)
library(stringr)

source("helpers.R")

# load data
# d = read.table(file="../data/qud_results.csv",sep=";",header=T,quote="\"") %>%
#   filter(!is.na(Eachsome.S2s2a4a4))
# 
# d$workerid = as.factor(as.numeric(d$Worker.ID))
# d$Worker.ID = NULL
# d$HIT.ID = NULL
# # 
# # # write anonymized results, to be loaded in the future  
# write.csv(d, file="../data/pilot_data.csv",row.names=F)

# load data
d = read.csv("../data/pilot_data.csv")

ggplot(d, aes(x=Gender)) +
  geom_histogram(stat="count")

ggplot(d, aes(x=Age)) +
  geom_histogram()

ggplot(d, aes(x=Eductation)) +
  geom_histogram(stat="count")

ggplot(d, aes(x=Lang)) +
  geom_histogram(stat="count")

unique(d$Comments)

# move from wide to long format so there's one data point per row
dd = d %>%
  select(-Lang,-Eductation,-Comments,-Age,-Gender,-Lifetime.Approval.Rate) %>%
  gather(Condition,Response,-workerid,-QuD,-Controlnumber,-Controlqud) %>%
  filter(!is.na(Response)) %>%
  mutate(condition = gsub("\\.","--",Condition)) %>%
  droplevels() 

dd$Utterance = sapply(strsplit(as.character(dd$condition),"--"), "[", 1)
dd$Scene = sapply(strsplit(as.character(dd$condition),"--"), "[", 2)

head(dd)

means = dd %>%
  group_by(Utterance,Scene,QuD) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

dodge = position_dodge(.9)

ggplot(means,aes(x=QuD,y=Mean,fill=Scene)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Utterance)
ggsave("../graphs/allmeans.pdf",width=13,height=10)

# for analysis, look only at "each...some" utterances and exclude floor/ceiling sanity checks
eachsome = dd %>% 
  filter(Utterance == "Eachsome") %>%
  droplevels() %>%
  mutate(Scene = as.factor(Scene),Response = as.factor(as.character(Response)))
summary(eachsome)

eachsomemeans = eachsome %>%
  group_by(Scene,QuD) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)
eachsomemeans[eachsomemeans$QuD == "anyany",c("Scene","Mean")]

ggplot(eachsomemeans,aes(x=QuD,y=Mean,fill=Scene)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) 

toanalyze = eachsome %>% 
  filter(! Scene %in% c("N0n0s2s3","S2s3s2s3")) %>% 
  droplevels()

contrasts(toanalyze$Scene) = cbind("weak.to.literal"=c(1,0))
contrasts(toanalyze$QuD) = cbind("allall.vs.anyany"=c(1,0,0,0),"anyall.vs.anyany"=c(0,1,0,0),"allany.vs.anyany"=c(0,0,0,1))

m = glmer(Response ~ Scene + QuD + (1|workerid),family="binomial",data=toanalyze)
summary(m)
