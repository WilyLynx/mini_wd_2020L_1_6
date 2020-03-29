library(readr)
library(dplyr)
library(ggplot2)

grants <-
  read_delim(
    "./data/grants.csv",
    "@",
    escape_double = FALSE,
    col_types = cols(
      announced = col_date(format = "%Y-%m-%d"),
      start = col_date(format = "%Y-%m-%d")
    ),
    trim_ws = TRUE
  )

data_global <-  grants %>% mutate(budget = as.numeric(gsub(
  pattern = "[A-Z ]",
  replacement = "",
  x = grants$budget
)))
  
data_2013 <- grants[format(grants$start, "%Y") == "2013", ]
data_2014 <- grants[format(grants$start, "%Y") == "2014", ]
data_2013 <-
  data_2013 %>% mutate(budget = as.numeric(gsub(
    pattern = "[A-Z ]",
    replacement = "",
    x = data_2013$budget
  )))
data_2014 <-
  data_2014 %>% mutate(budget = as.numeric(gsub(
    pattern = "[A-Z ]",
    replacement = "",
    x = data_2014$budget
  )))


# globalna popularnosc konkursow

data_global_mean_contest_budget <-
  data_global %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest)) %>% group_by(contest) %>% summarise(number = n())

data_global_mean_contest_budget %>%  ggplot(aes(x=contest,y=number)) +geom_bar(stat="identity")


# globalny sredni budzet 

data_global_mean_contest_budget <-
  data_global %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest)) %>% group_by(contest) %>% summarise(mean_budget = mean(budget))

data_global_mean_contest_budget %>%  ggplot(aes(x=contest,y=mean_budget)) +geom_bar(stat="identity")

# liczba projektow na przestrzeni lat

data_global_number_change <- data_global %>% select("id","start","contest") %>%  mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest), start = format(start,"%Y")) %>% filter(contest == "OPUS" | contest=="PRELUDIUM" | contest=="SYMFONIA" | contest=="MAESTRO" | contest =="SONATA" | contest =="SONATABIS") %>% group_by(contest,start) %>%  summarise(number=n())

data_global_number_change %>% ggplot(aes(x = start, y=number, group = contest , color = contest)) +  geom_line(size=1.2)  + geom_point()

# budzet na przestrzeni lat 

data_global_budget_change <- data_global %>% select("id","start","contest","budget") %>%  mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest), start = format(start,"%Y")) %>% filter(contest == "OPUS" | contest=="PRELUDIUM" | contest=="SYMFONIA" | contest=="MAESTRO" | contest =="SONATA" | contest =="SONATABIS") %>% group_by(contest,start) %>%  summarise(mean_budget= mean(budget))

data_global_budget_change %>% ggplot(aes(x = start, y=mean_budget, group = contest , color = contest)) +  geom_line(size=1.2)  + geom_point()

# mapa sredni budzet liczba

data_global_mean_contest_budget <-
  data_global %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest)) %>% group_by(contest) %>% summarise( number= n(),  mean_budget = mean(budget))

data_global_mean_contest_budget %>%  ggplot(aes(x=number,y=mean_budget,label = contest)) +geom_point() +   geom_label(size = 3)

# popularnosc  konkursow w roku 2013 2014

data_2013_contest_popularity <-
  data_2013 %>% select("id", "panel" , "contest") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_2013$contest)) %>% group_by(contest) %>% summarise( number=n())


data_2013_contest_popularity %>%  ggplot(aes(x=contest,y=number)) +geom_bar(stat="identity")


data_2014_contest_popularity <-
  data_2014 %>% select("id", "panel" , "contest") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_2014$contest)) %>% group_by(contest) %>% summarise( number=n())


data_2014_contest_popularity %>%  ggplot(aes(x=contest,y=number)) +geom_bar(stat="identity")

# sredni budzet w konkursach 2013
data_2013_mean_contest_budget <-
  data_2013 %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_2013$contest)) %>% group_by(contest) %>% summarise(mean_budget = mean(budget))

data_2013_mean_contest_budget %>%  ggplot(aes(x=contest,y=mean_budget)) +geom_bar(stat="identity")

  
# popularnosc subpaneli w konkursach opus preloudium sonata

data_2013_subpanel_popularity <-  data_2013 %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_2013$contest)) %>% filter(contest == "OPUS" | contest == "PRELUDIUM" | contest == "SONATA"  ) %>% group_by(contest,panel) %>% summarise(number=n(),mean_budget =mean(budget))  

  
data_2013_subpanel_popularity %>%  ggplot(aes(x=panel,y=mean_budget,fill = contest)) +geom_bar(stat="identity", position = "dodge")

  
data_2014_subpanel_popularity <-  data_2014 %>% select("id", "panel" , "contest","budget") %>% mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_2013$contest)) %>% filter(contest == "OPUS" | contest == "PRELUDIUM" | contest== "SONET"  ) %>% group_by(contest,panel) %>% summarise(number=n(),mean_budget =mean(budget))  


data_2014_subpanel_popularity %>%  ggplot(aes(x=panel,y=number,fill = mean_budget)) +geom_bar(stat="identity") +facet_wrap(~contest)

# liczba projektow z podzialem na panele dla opusa

data_global_number_change <- data_global %>% select("id","panel","start","contest") %>%
  mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest), start = format(start,"%Y"), panel= substr(panel,0,2)) %>%
  filter(contest == "OPUS") %>%
  group_by(panel,start) %>%  
  summarise(number=n())

data_global_number_change %>% ggplot(aes(x = start, y=number, group = panel , color = panel)) +  geom_line(size=1.2)  + geom_point()

# budzet projektow z podzialem na panele dla opusa

data_global_number_change <- data_global %>% select("id","panel","start","contest","budget") %>%
  mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest), start = format(start,"%Y"), panel= substr(panel,0,2)) %>%
  filter(contest == "OPUS") %>%
  group_by(panel,start) %>%  
  summarise(mean_budget=mean(budget))

data_global_number_change %>% ggplot(aes(x = start, y=mean_budget, group = panel , color = panel)) +  geom_line(size=1.2)  + geom_point()

# najpopularniejsze subpanele w opusie

data_global_number_budget_change <- data_global %>% select("id","panel","contest","budget") %>%
  mutate(contest = gsub(pattern = "[0-9 ]",replacement = "",data_global$contest), subpanel=panel,  panel= substr(panel,0,2)) %>%
  filter(contest == "OPUS") %>%
  group_by(panel,subpanel) %>%  
  summarise(number=n(), mean_budget=mean(budget))

data_global_number_budget_change %>%  ggplot(aes(x=number,y=mean_budget,label = subpanel, color=panel)) +geom_point() +   geom_label(size = 10)


#subpanele 

data_global_number_budget_change <- data_global 