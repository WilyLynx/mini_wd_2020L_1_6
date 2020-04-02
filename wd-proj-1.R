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
grants_mapped <- read_csv("./grants_mapped.csv", 
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
ggsave(filename="../plots/plot1.svg")

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
  guides(color=guide_legend(title="Panel"))+
  scale_x_discrete(labels = c("01" = "Sty", "02" = "Lut", "03" = "Mar", "04" = "Kwi", "05" = "Maj", "06" = "Cze",
                              "07" = "Lip", "08" = "Sie", "09" = "Wrz", "10" = "Paź", "11" = "Lis", "12" = "Gru"))
ggsave(filename="../plots/plot2.svg")

#3
data_global_number_change <- grants_mapped %>% select("panel","start","contest","budget") %>%
  mutate(start = format(start,"%Y")) %>%
  group_by(panel,start) %>%  
  summarise(mean_budget=mean(budget))

data_global_number_change %>% ggplot(aes(x = start, y=mean_budget / 1000, group = panel , color = panel)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł")) +
  xlab("Rok") +
  ylab("Średni budżet") +
  guides(color=guide_legend(title="Panel"))
ggsave(filename="../plots/plot3.svg")
#4

grants_mapped %>% 
  mutate(
    budget = log10(budget),
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  ggplot(aes(x=budget))+
  geom_histogram(aes(fill=..count..,color="black"),bins=45)+
  scale_fill_gradient(low = "white", high = "brown")+
  xlab("Budżet")+
  ylab("Liczba projektów")+
  scale_x_continuous(breaks=c(4.301,4.6989,5,5.301,5.6989,6, 6.3979),
                     labels=c("20 tys. zł","50 tys. zł", "100 tys. zł","200 tys. zł","500 tys. zł", "1 mln zł", "2.5 mln zł"))+
  guides(fill=FALSE, color=FALSE)
ggsave(filename="../plots/plot4.svg")
#5

mean_budget_per_month <- grants_mapped %>% select("contest","panel","budget") %>%
  group_by(panel,contest) %>%  
  summarise(number=n()) %>% 
  top_n(8,number)

mean_budget_per_month %>% ggplot(aes(x=contest,y=number,fill = panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  xlab("Konkurs")+
  ylab("Liczba grantów") +
  guides(fill=guide_legend(title="Panel"))
ggsave(filename="../plots/plot5.svg")

#6
mean_budget_per_month <- grants_mapped %>% select("contest","panel","budget") %>%
  group_by(panel,contest) %>%  
  summarise(number=n(), mean_budget=mean(budget)) %>% 
  top_n(8,number)

mean_budget_per_month %>% ggplot(aes(x=contest,y=mean_budget/1e6,fill = panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  xlab("Konkurs")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł")) +
  guides(fill=guide_legend(title="Panel"))
  
ggsave(filename="../plots/plot6.svg")

#7

grants_mapped2 <- read_csv("./grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))

grants_mapped2 <- grants_mapped2 %>% mutate(subpanel = panel, panel =substr(panel,0,2))

grants_mapped2 <- grants_mapped2 %>% mutate( descriptors= strsplit(descriptors,"\\|"),) %>% unnest(c(descriptors)) 

mean_budget_per_month <- grants_mapped2 %>% select("contest","descriptors", "panel","budget") %>%
  group_by(panel,descriptors) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  arrange(desc(mean_budget) ) %>% 
  top_n(20,mean_budget) %>% 
  mutate(desc_panel = substr(descriptors,0,2))

mean_budget_per_month %>% 
  filter(panel == "HS")%>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
                                     y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#00ba38", "#619cff"))+
  labs(title="TOP 20 deskryptorów występujących w grantach panelu HS", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))

ggsave(filename="../plots/plot7.svg")

#8
mean_budget_per_month %>%
  filter(panel == "ST")%>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
             y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#00ba38", "#619cff"))+
  labs(title="TOP 20 deskryptorów występujących w grantach panelu ST", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))

ggsave(filename="../plots/plot8.svg")

#9
mean_budget_per_month %>%
  filter(panel == "NZ")%>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
             y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#f8766d","#00ba38", "#619cff"))+
  labs(title="TOP 20 deskryptorów występujących w grantach panelu NZ", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))

ggsave(filename="../plots/plot9.svg")

#10

mean_budget_per_month <- grants_mapped2 %>% select("contest","subpanel", "panel","budget") %>%
  filter(contest %in% c('OPUS','SONATA','SONATA BIS','PRELUDIUM','MAESTRO','HARMONIA','FUGA','ETIUDA')) %>%
  group_by(contest,subpanel) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  top_n(3) %>% 
  mutate(panel = substr(subpanel,0,2),
         panel_color = case_when(
           panel == "HS" ~ "red",
           panel == "ST" ~ "blue",
           panel == "NZ" ~ "green",
         ))

mean_budget_per_month %>% ggplot(aes(x=contest,y=mean_budget/1e6, group=subpanel, fill=panel, label=subpanel)) + 
  coord_flip() +
  geom_bar(width = 0.8, position =position_dodge(width = 1) ,stat = "identity") +
  geom_text(size=3, position = position_dodge2(width = 1)) +
  scale_fill_manual(values=c("#00ba38", "#619cff"))+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Konkurs")+
  guides(fill=guide_legend(title="Panel"))

ggsave(filename="../plots/plot10.svg")


#11

data_global_mean_contest_budget <-
  grants_mapped %>% select("panel" , "contest","budget") %>%
  filter(contest %in% c('OPUS','SONATA','SONATA BIS','PRELUDIUM','MAESTRO','HARMONIA','FUGA','ETIUDA')) %>%
  group_by(panel,contest) %>%
  summarise( number= n(),  mean_budget = mean(budget))

data_global_mean_contest_budget %>%  ggplot(aes(x=number,y=mean_budget/1000,color=panel,label=contest , shape=contest)) +geom_point(size=10, stroke=2.5) + geom_text(size=3,color="black",nudge_y = -0.05) +
  scale_shape_manual(values=c(15, 10, 17,18,19,2,7,22,23)) +
  scale_y_log10(labels = label_comma(suffix = " tys. zł"))+
  scale_x_log10()+
  xlab("Liczba projektów") +
  ylab("Średni budżet") +
  theme(legend.key.height=unit(3,"line"))

ggsave(filename="../plots/plot11.svg")


