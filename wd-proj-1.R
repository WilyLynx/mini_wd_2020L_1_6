library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

inflation <- data.frame(
  rok = factor(as.character(seq(2011,2019,1))),
  val = c(1.0, 1.037, 1.009, 1.0, 0.991, 0.994, 1.02, 1.016, 1.023)
) %>% 
  mutate(
    cumulative_val = cumprod(val)
  )

month_shortcut <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")
grants_mapped <- read_csv("C:/Users/Łukasz/Desktop/wizualizacja danych/grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))

grants_mapped <- grants_mapped %>% mutate(subpanel = panel, panel =substr(panel,0,2), desc_count =sapply(strsplit(descriptors,split = "\\|"),function(x){length(x)}) )

#1
data_bar_descriptors_per_panel <- grants_mapped %>% select("start", "panel","subpanel","contest","budget","desc_count") %>%
  mutate(start = format(start,"%Y"),desc_count = factor(desc_count)) %>% 
  group_by(start,desc_count) %>%  
  summarise(mean_budget = mean(budget)) %>%
  inner_join(inflation,by = c("start"="rok")) %>% 
  mutate(budget_with_inflation = mean_budget/cumulative_val)
  
  
data_bar_descriptors_per_panel %>%  ggplot(aes(x=start,y=budget_with_inflation/1000,color=desc_count,group=desc_count))  +
  geom_line(size=1.2)  +
  geom_point() +
  xlab("Rok") +
  ylab("Średni budżet") +
  guides(color=guide_legend(title="Liczba deskryptorów")) +
  scale_color_manual(values=c("#9D47B3", "#D49837",'#0D9E61')) +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł"))


#2
mean_budget_per_month <- grants_mapped %>% select("start","panel","budget") %>%
  mutate(month = format(start,"%m"), year = format(start,"%Y")) %>% 
  inner_join(inflation,by = c("year"="rok")) %>% 
  mutate(budget_with_inflation = budget/cumulative_val) %>% 
  group_by(panel,month) %>%  
  summarise(mean_budget=mean(budget_with_inflation)) %>% 
  mutate(month_label = month_shortcut[month])

  
mean_budget_per_month %>% ggplot(aes(x=month,y=mean_budget/1000,color = panel, group=panel)) + 
  geom_line(size=1.2)  +
  geom_point() +
  ylab("Średni budżet") +
  xlab("Miesiąc") +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł")) +
  guides(color=guide_legend(title="Panel"))

#3
data_global_number_change <- grants_mapped %>% select("panel","start","contest","budget") %>%
  mutate(start = format(start,"%Y")) %>%
  group_by(panel,start) %>%  
  summarise(mean_budget=mean(budget))

data_global_number_change %>% ggplot(aes(x = start, y=mean_budget, group = panel , color = panel)) +  geom_line(size=1.2)  + geom_point()

#4 nie ma


#5

mean_budget_per_month <- grants_mapped %>% select("contest","panel","budget") %>%
  group_by(panel,contest) %>%  
  summarise(number=n()) %>% 
  top_n(8,number)

mean_budget_per_month %>% ggplot(aes(x=contest,y=number,fill = panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")


#6
mean_budget_per_month <- grants_mapped %>% select("contest","panel","budget") %>%
  group_by(panel,contest) %>%  
  summarise(number=n(), mean_budget=mean(budget)) %>% 
  top_n(8,number)

mean_budget_per_month %>% ggplot(aes(x=contest,y=mean_budget,fill = panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")
  

#7


grants_mapped <- read_csv("C:/Users/Łukasz/Desktop/wizualizacja danych/grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))

grants_mapped <- grants_mapped %>% mutate(subpanel = panel, panel =substr(panel,0,2))

grants_mapped <- grants_mapped %>% mutate( descriptors= strsplit(descriptors,"\\|"),) %>% unnest(c(descriptors)) 

mean_budget_per_month <- grants_mapped %>% select("contest","descriptors", "panel","budget") %>%
  group_by(panel,descriptors) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  arrange(desc(mean_budget) ) %>% 
  top_n(20,mean_budget) %>% 
  mutate(desc_panel = substr(descriptors,0,2))

mean_budget_per_month %>% ggplot(aes(x=descriptors,y=mean_budget,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity") +
  facet_wrap(~panel,scales="free")

#8

mean_budget_per_month <- grants_mapped %>% select("contest","subpanel", "panel","budget") %>%
  filter(contest %in% c('OPUS','SONATA','SONATABIS','PRELUDIUM','MAESTRO','HARMONIA','FUGA','ETIUDA')) %>% 
  group_by(contest,subpanel) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  top_n(3) %>% 
  mutate(panel = substr(subpanel,0,2),
         panel_color = case_when(
           panel == "HS" ~ "red",
           panel == "ST" ~ "blue",
           panel == "NZ" ~ "green",
         ))

mean_budget_per_month %>% ggplot(aes(x=contest,y=mean_budget, group=subpanel, fill=panel, label=subpanel)) + 
  coord_flip() +
  geom_bar(width = 0.8, position =position_dodge(width = 1) ,stat = "identity") +
  geom_text(size=3, position = position_dodge(width = 1)) +
  scale_fill_manual(values=c("#00ba38", "#619cff"))
