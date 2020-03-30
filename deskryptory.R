library(readr)
library(dplyr)
library(tidyr)

grants_mapped <- read_csv("C:/Users/Łukasz/Desktop/wizualizacja danych/grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))
View(grants_mapped)

grants_mapped <- grants_mapped %>% mutate(subpanel = panel, panel =substr(panel,0,2))

grants_mapped <- grants_mapped %>% mutate( descriptors= strsplit(descriptors,"\\|"),) %>% unnest(c(descriptors)) 

# mapa deskryptorow

data_map_descriptors <- grants_mapped %>% select("subpanel","contest","budget","descriptors") %>%
  mutate(desc_subpanel = sapply(strsplit(descriptors,split = "_"),function(x){x[1]})) %>% 
  filter(desc_subpanel == "ST8") %>%
  group_by(descriptors) %>%  
  summarise(number=n(), mean_budget=mean(budget))

data_map_descriptors %>%  ggplot(aes(x=number,y=mean_budget,label = descriptors)) +geom_point() +   geom_label(size = 3)

# wykres sumy budzetu od subpanelu

data_subpanel_budget_sum <- grants_mapped %>% select("panel","subpanel","contest","budget") %>%
  group_by(panel,subpanel) %>%  
  summarise(sum_budget=sum(budget))

data_subpanel_budget_sum %>% ggplot(aes(x =subpanel, y= sum_budget,fill=panel)) + geom_bar(stat = "identity")


# wykres kolowy z sumarycznym budzetem

data_subpanel_budget_sum <- grants_mapped %>% select("panel","subpanel","contest","budget") %>%
  group_by(panel) %>%  
  summarise(sum_budget=sum(budget))

data_subpanel_budget_sum %>% ggplot(aes(x =" ", y= sum_budget,fill=panel)) + geom_bar(width = 1, stat = "identity") + coord_polar("y",start = 0)
 
# wykres kolowy z sumarycznym budzetem per rok
subpanel_per_year<- grants_mapped %>% select("panel","start","subpanel","contest","budget") %>%
  mutate(start = format(start,"%Y")) %>% 
  group_by(start) %>%  
  summarise(total_budget = sum(budget))

data_subpanel_budget_sum_per_year <- grants_mapped %>% select("panel","start","subpanel","contest","budget") %>%
  mutate(start = format(start,"%Y")) %>% 
  group_by(start,panel) %>%
  inner_join(subpanel_per_year,by = c("start"="start")) %>% 
  summarise(sum_budget=sum(budget)/total_budget[1])

data_subpanel_budget_sum_per_year %>% ggplot(aes(x =" ", y= sum_budget,fill=panel)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y",start = 0) +
  facet_wrap(~start)

# słupki deskryptorowe

data_bar_descriptors <- grants_mapped %>% select("subpanel","contest","budget","descriptors") %>%
  mutate(desc_subpanel = sapply(strsplit(descriptors,split = "_"),function(x){x[1]}),
         desc_panel = substr(descriptors,0,2)) %>% 
  group_by(desc_panel, desc_subpanel) %>%  
  summarise( mean_budget=n())

data_bar_descriptors %>%  ggplot(aes(x=desc_subpanel,y=mean_budget, fill=desc_panel)) +geom_bar(stat = "identity")


# słupki deskryptorowe per panel

data_bar_descriptors <- grants_mapped %>% select("panel","subpanel","contest","budget","descriptors") %>%
  mutate(desc_subpanel = sapply(strsplit(descriptors,split = "_"),function(x){x[1]}),
         desc_panel = substr(descriptors,0,2)) %>% 
  group_by(panel,desc_panel, desc_subpanel) %>%  
  summarise(mean_budget=n())

data_bar_descriptors %>%  ggplot(aes(x=desc_subpanel,y=mean_budget, fill=desc_panel)) + geom_bar(stat = "identity") +
  facet_wrap(~panel)


# słupki deskryptorowe per panel

data_bar_descriptors_per_panel <- grants_mapped %>% select("panel","subpanel","contest","budget","descriptors") %>%
  mutate(desc_subpanel = sapply(strsplit(descriptors,split = "_"),function(x){x[1]}),
         desc_panel = substr(descriptors,0,2)) %>% 
  group_by(panel,desc_panel, desc_subpanel) %>%  
  summarise(mean_budget=sum(budget))

data_bar_descriptors_per_panel %>%  ggplot(aes(x=desc_subpanel,y=mean_budget, fill=desc_panel)) + geom_bar(stat = "identity") +
  facet_wrap(~panel)

# słupki deskryptorowe per subpanel

data_bar_descriptors_per_panel <- grants_mapped %>% select("panel","subpanel","contest","budget","descriptors") %>%
  mutate(desc_subpanel = sapply(strsplit(descriptors,split = "_"),function(x){x[1]}),
         desc_panel = substr(descriptors,0,2)) %>% 
  group_by(subpanel,desc_panel, desc_subpanel) %>%  
  summarise(count=n())

data_bar_descriptors_per_panel %>%  ggplot(aes(x=desc_subpanel,y=count, fill=desc_panel)) + geom_bar(stat = "identity") +
  facet_wrap(~subpanel)


# budzet od liczby deskryptorow

grants_mapped <- read_csv("C:/Users/Łukasz/Desktop/wizualizacja danych/grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))

grants_mapped <- grants_mapped %>% mutate(subpanel = panel, panel =substr(panel,0,2), desc_count =sapply(strsplit(descriptors,split = "\\|"),function(x){length(x)}) )

data_bar_descriptors_per_panel <- grants_mapped %>% select("panel","subpanel","contest","budget","desc_count") %>%
  group_by(panel,desc_count) %>%  
  summarise(mean_budget = n())

data_bar_descriptors_per_panel %>%  ggplot(aes(x=desc_count,y=mean_budget, fill=panel)) + geom_bar(position ="dodge" ,stat = "identity")

# liczby deskryptorow  per rok

data_bar_descriptors_per_panel <- grants_mapped %>% select("start", "panel","subpanel","contest","budget","desc_count") %>%
  mutate(start = format(start,"%Y")) %>% 
  group_by(start,desc_count) %>%  
  summarise(count = n())

data_bar_descriptors_per_panel %>%  ggplot(aes(x=desc_count,y=count)) + geom_bar(position ="dodge" ,stat = "identity")+
  facet_wrap(~start)


# liczby deskryptorow  per rok

data_bar_descriptors_per_panel <- grants_mapped %>% select("start", "panel","subpanel","contest","budget","desc_count") %>%
  mutate(start = format(start,"%Y"),desc_count = factor(desc_count)) %>% 
  group_by(start,desc_count) %>%  
  summarise(mean_budget = mean(budget))

data_bar_descriptors_per_panel %>%  ggplot(aes(x=start,y=mean_budget,color=desc_count,group=desc_count))  +
  geom_line(size=1.2)  +
  geom_point()
