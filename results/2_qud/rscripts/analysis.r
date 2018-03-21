theme_set(theme_bw(18))
require(tidyverse)
library(stringr)
library(forcats)

source("helpers.R")

# load data
# d = read.table(file="../data/QUD_final_results.csv",sep=";",header=T,quote="\"") %>%
#   filter(!is.na(Eachsome.S2s2a4a4))
# 
# d$workerid = as.factor(as.numeric(d$Worker.ID))
# d$Worker.ID = NULL
# d$HIT.ID = NULL
# #
# # # write anonymized results, to be loaded in the future
# write.csv(d, file="../data/data.csv",row.names=F)

# load data
d = read.csv("../data/data.csv")

ggplot(d, aes(x=Gender)) +
  geom_histogram(stat="count")

ggplot(d, aes(x=Age)) +
  geom_histogram()

ggplot(d, aes(x=Education)) +
  geom_histogram(stat="count")

ggplot(d, aes(x=Lang)) +
  geom_histogram(stat="count")

unique(d$Comments)

d = d %>%
  mutate(QUD_annotated = as.character(QUD_annotated)) %>%
  replace_na(list(QUD_annotated="other"))
d$CorrectQUD = as.factor(ifelse(as.character(d$QUD) == as.character(d$QUD_annotated),"correct","incorrect"))
table(d$QUD,d$CorrectQUD)
prop.table(table(d$QUD,d$CorrectQUD),mar=c(1))

# move from wide to long format so there's one data point per row
dd = d %>%
  select(-Lang,-Education,-Comments,-Age,-Gender,-Lifetime.Approval.Rate) %>%
  gather(Condition,Response,-workerid,-QUD,-Controlnumber,-Controlqud,-QUD_annotated,-CorrectQUD,-Number_annotated) %>%
  filter(!is.na(Response)) %>%
  mutate(condition = gsub("\\.","--",Condition)) %>%
  droplevels() 

dd$Utterance = sapply(strsplit(as.character(dd$condition),"--"), "[", 1)
dd$Scene = sapply(strsplit(as.character(dd$condition),"--"), "[", 2)

head(dd)

# means by qud for all utterances
means = dd %>%
  group_by(Utterance,Scene,QUD) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

dodge = position_dodge(.9)

ggplot(means,aes(x=QUD,y=Mean,fill=Scene)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Utterance)
ggsave("../graphs/allmeans.pdf",width=8,height=6)

# means by qud for all utterances, conditioned on CorrectQUD
means = dd %>%
  group_by(Utterance,Scene,QUD,CorrectQUD) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

dodge = position_dodge(.9)

ggplot(means,aes(x=QUD,y=Mean,fill=Scene)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_grid(Utterance~CorrectQUD)
ggsave("../graphs/allmeans_correctQUD.pdf",width=9,height=12)

# for analysis, look only at "each...some" utterances and exclude floor/ceiling sanity checks
eachsome = dd %>% 
  filter(Utterance == "Eachsome") %>%
  droplevels() %>%
  mutate(Scene = as.factor(Scene),Response = as.factor(as.character(Response))) %>%
  mutate(Condition=recode(Scene, A4a4a4a4 = "literal", N0n0s2s3="false", S2s2a4a4 = "weak", S2s3s2s3 = "true"),
QUD=recode(QUD,eachany="all-any",anyany="any-any",allall="all-all",anyall="any-all")) %>%
  mutate(Condition = fct_relevel(Condition, "false", "literal", "weak", "true"),
         QUD=fct_relevel(QUD,"any-any","any-all","all-any","all-all"))

summary(eachsome)

# who responded TRUE on FALSE trials or FALSE on TRUE trials??
badfalsesubjects = eachsome %>%
  filter(Condition == "false") %>%
  select(workerid,Response) %>%
  mutate(IncorrectResponse=ifelse(Response == "1","bad subject", "good subject")) %>%
  filter(IncorrectResponse == "bad subject") %>%
  select(workerid)

badtruesubjects = eachsome %>%
  filter(Condition == "true") %>%
  select(workerid,Response) %>%
  mutate(IncorrectResponse=ifelse(Response == "0","bad subject", "good subject")) %>%
  filter(IncorrectResponse == "bad subject") %>%
  select(workerid)

badsubjects = union(badfalsesubjects,badtruesubjects)
nrow(badsubjects)

eachsome$IncorrectControlResponse = ifelse(as.character(eachsome$workerid) %in% as.character(badsubjects$workerid), "bad subject","good subject")

#eachsome means
eachsomemeans = eachsome %>%
  group_by(Condition,QUD) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)

ggplot(eachsomemeans,aes(x=QUD,y=Mean,fill=Condition)) +
  geom_bar(stat="identity",position=dodge) +
  ylab("% TRUE") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) #+ 
#  geom_point(data=eachsome,aes(y=as.numeric(as.character(Response)),color=Condition),position=dodge)
ggsave("../graphs/eachsomemeans.pdf",width=7,height=4)

# eachsome means conditioned on correctness of QUD
eachsomemeans = eachsome %>%
  group_by(Condition,QUD,CorrectQUD) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)

ggplot(eachsomemeans,aes(x=QUD,y=Mean,fill=Condition)) +
  geom_bar(stat="identity",position=dodge) +
  ylab("% TRUE") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~CorrectQUD)
#  geom_point(data=eachsome,aes(y=as.numeric(as.character(Response)),color=Condition),position=dodge)
ggsave("../graphs/eachsomemeans_correctQUD.pdf",width=12,height=4)

# eachsome means conditioned on correctly responding FALSE in false condition and TRUE in true condition
eachsomemeans = eachsome %>%
  filter(!is.na(IncorrectControlResponse)) %>%
  group_by(Condition,QUD,IncorrectControlResponse) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)

ggplot(eachsomemeans,aes(x=QUD,y=Mean,fill=Condition)) +
  geom_bar(stat="identity",position=dodge) +
  ylab("% TRUE") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~IncorrectControlResponse)
#  geom_point(data=eachsome,aes(y=as.numeric(as.character(Response)),color=Condition),position=dodge)
ggsave("../graphs/eachsomemeans_incorrectControlResponses.pdf",width=12,height=4)

# eachsome means conditioned on correctly responding FALSE in false condition and TRUE in true condition
# also conditioned on whether or not they responded correctly to the number of marbles control question

table(eachsome$Number_annotated)
table(eachsome$IncorrectControlResponse)

eachsomemeans = eachsome %>%
  filter(!is.na(IncorrectControlResponse)) %>%
  group_by(Condition,QUD,IncorrectControlResponse,Number_annotated) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)

ggplot(eachsomemeans,aes(x=QUD,y=Mean,fill=Condition)) +
  geom_bar(stat="identity",position=dodge) +
  ylab("% TRUE") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_grid(Number_annotated~IncorrectControlResponse)
#  geom_point(data=eachsome,aes(y=as.numeric(as.character(Response)),color=Condition),position=dodge)
ggsave("../graphs/eachsomemeans_incorrectControlResponses_number.pdf",width=12,height=7)

# eachsome means conditioned on empirical QUD and whether or not they responded correctly on control trials
eachsomemeans = eachsome %>%
  group_by(Condition,QUD_annotated,Number_annotated,IncorrectControlResponse) %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  summarize(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
dodge = position_dodge(.9)

ggplot(eachsomemeans,aes(x=QUD_annotated,y=Mean,fill=Condition)) +
  geom_bar(stat="identity",position=dodge) +
  ylab("% TRUE") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_grid(Number_annotated~IncorrectControlResponse) +
  theme(axis.text.x=element_text(angle=45, hjust=1,vjust=1))
#  geom_point(data=eachsome,aes(y=as.numeric(as.character(Response)),color=Condition),position=dodge)
ggsave("../graphs/eachsomemeans_empiricalQUDandNumber.pdf",width=12,height=8)

toanalyze = eachsome %>% 
  filter(Condition %in% c("weak","literal")) %>% 
  mutate(Condition=as.factor(Condition)) %>%
  droplevels()

contrasts(toanalyze$Condition) = cbind("weak.to.literal"=c(1,0))
# contrasts(toanalyze$QUD) = cbind("allall.vs.anyany"=c(1,0,0,0),"anyall.vs.anyany"=c(0,1,0,0),"allany.vs.anyany"=c(0,0,0,1))

table(toanalyze$QUD, toanalyze$CorrectQUD, toanalyze$IncorrectControlResponse, toanalyze$Number_annotated)

# Model (with interaction does not currently converge)
m = glmer(Response ~ Condition + QUD + (1|workerid),family="binomial",data=toanalyze)
summary(m)

m = glmer(Response ~ Condition + QUD + IncorrectControlResponse + Number_annotated + (1|workerid),family="binomial",data=toanalyze)
summary(m)

m = glmer(Response ~ Condition + QUD + CorrectQUD + IncorrectControlResponse + Number_annotated + (1|workerid),family="binomial",data=toanalyze)
summary(m)
