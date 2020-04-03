##### LIBRARIES #####
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(stringr)

##### DATA #####
month_shortcut <- c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")
grants_mapped <- read_csv("./grants_mapped.csv", 
                          col_types = cols(announced = col_date(format = "%Y-%m-%d"), 
                                           start = col_date(format = "%Y-%m-%d")))

grants_mapped <- grants_mapped %>% 
  mutate(
    subpanel = panel, 
    panel =substr(panel,0,2), 
    desc_count =sapply(strsplit(descriptors,split = "\\|"),function(x){length(x)}),
    descriptors= strsplit(descriptors,"\\|"),) %>% unnest(c(descriptors)
    )




##### INFLATION #####
inflation <- data.frame(
  year = factor(as.character(seq(2011,2019,1))),
  val = c(1.0, 1.037, 1.009, 1.0, 0.991, 0.994, 1.02, 1.016, 1.023)
) %>% 
  mutate(
    cumulative_val = cumprod(val)
  )

##### THEME ####
theme_wd1 <- function () { 
  theme_bw(base_size=12, base_family="Avenir") %+replace% 
    theme(
      text = element_text(color = 'white',size=11),
      plot.background = element_rect(fill = 'black'),
      axis.text= element_text(colour = 'white'),
      legend.background = element_rect(fill = 'black'),
      axis.ticks = element_line(color = 'white'),
      legend.key = element_rect(color = 'black')
    )
}

##### 1 #####
mean_budget_per_month <- grants_mapped %>% 
  select("start","panel","budget") %>%
  mutate(month = format(start,"%m"), year = format(start,"%Y")) %>% 
  inner_join(inflation,by = c("year"="year")) %>% 
  mutate(budget_with_inflation = budget/cumulative_val) %>% 
  group_by(panel,month) %>%  
  summarise(mean_budget=mean(budget_with_inflation)) %>% 
  mutate(month_label = month_shortcut[month])

mean_budget_per_month %>% 
  ggplot(aes(x=month,y=mean_budget/1000,color = panel, group=panel)) + 
  geom_line(size=1.2)  +
  geom_point() +
  ylab("Średni budżet") +
  xlab("Miesiąc") +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł")) +
  guides(color=guide_legend(title="Panel"))+
  scale_x_discrete(labels = c("01" = "Sty", "02" = "Lut", "03" = "Mar", "04" = "Kwi", "05" = "Maj", "06" = "Cze",
                              "07" = "Lip", "08" = "Sie", "09" = "Wrz", "10" = "Paź", "11" = "Lis", "12" = "Gru"))+
  theme_wd1()

ggsave(filename="../plots/plot1.svg",
       width = 185,
       height = 130,
       units = "mm")



##### 2 #####
data_global_mean_contest_budget <-
  grants_mapped %>% 
  select("panel" , "contest","budget") %>%
  filter(contest %in% c('OPUS','SONATA','SONATA BIS','PRELUDIUM','MAESTRO','HARMONIA','FUGA','ETIUDA')) %>%
  group_by(panel,contest) %>%
  summarise( number= n(),  mean_budget = mean(budget))

data_global_mean_contest_budget %>%  
  ggplot(aes(x=number,y=mean_budget/1e3,color=panel, shape=contest)) +
  geom_point(size=4, stroke=1.5) + 
  scale_shape_manual(values=c(15, 10, 17,18,19,2,7,22,23)) +
  scale_y_log10(labels = label_comma(suffix = " tys. zł"))+
  scale_x_log10(limits=c(70,10000))+
  xlab("Liczba projektów") +
  ylab("Średni budżet") +
  guides(shape=guide_legend(title="Konkurs"),color=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot2.svg",
       width = 185,
       height = 130,
       units = "mm")
##### 3 #####
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
  guides(fill=FALSE, color=FALSE)+
  theme_wd1()

ggsave(filename="../plots/plot3.svg",
       width = 185,
       height = 130,
       units = "mm")


##### 4 #####

data_global_number_change <- grants_mapped %>% 
  select("panel","start","contest","budget") %>%
  mutate(start = format(start,"%Y")) %>%
  group_by(panel,start) %>%  
  summarise(mean_budget=mean(budget))

data_global_number_change %>% 
  ggplot(aes(x = start, y=mean_budget / 1000, group = panel , color = panel)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł")) +
  xlab("Rok") +
  ylab("Średni budżet") +
  guides(color=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot1.svg",
       width = 185,
       height = 130,
       units = "mm")


##### 5 #####
data_top3_contest_subpanel <- grants_mapped %>% 
  select("contest","subpanel", "panel","budget") %>%
  filter(contest %in% c('OPUS','SONATA','SONATA BIS','PRELUDIUM','HARMONIA','FUGA')) %>%
  group_by(contest,subpanel) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  top_n(3) %>% 
  mutate(panel = substr(subpanel,0,2),
         panel_color = case_when(
           panel == "HS" ~ "red",
           panel == "ST" ~ "blue",
           panel == "NZ" ~ "green",
         )) %>% 
  mutate(
    subpanel = case_when(
      subpanel == "NZ1" ~ " Biologia molekularna",
      subpanel == "NZ2" ~ " Genetyka",
      subpanel == "NZ3" ~ " Bio. na poz. komórki",
      subpanel == "NZ4" ~ " Bio. na poz. tkanki, narządów, organizmów",
      subpanel == "NZ5" ~ " Choroby niezakaźne",
      subpanel == "NZ6" ~ " Choroby zakaźne",
      subpanel == "NZ7" ~ " Leki i zdrowie publiczne",
      subpanel == "NZ8" ~ " Ekologia",
      
      subpanel == "ST2" ~ " Podstawowe składniki materii",
      subpanel == "ST9" ~ " Astronomia",
      
      TRUE ~ subpanel
  ))

data_top3_contest_subpanel %>% 
  ggplot(aes(x=contest,y=mean_budget/1e6, group=subpanel, fill=panel, label=subpanel)) + 
  geom_text(size=3, 
            position = position_dodge(width = 0.9), 
            hjust = 0) +
  coord_flip() +
  geom_bar(width = 0.8, position =position_dodge(width = 1) ,stat = "identity") +

  scale_fill_manual(values=c("#00ba38", "#619cff", "#ffffff"))+
  ylab("Średni budżet")+
  scale_y_continuous(limits = c(0,2.5),labels = label_comma(suffix = " mln zł"))+
  xlab("Konkurs")+
  guides(fill=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot5.svg",
       width = 185,
       height = 130,
       units = "mm")


##### 6 #####

data_bar_descriptors_per_panel <- grants_mapped %>% 
  select("start", "panel","subpanel","contest","budget","desc_count") %>%
  mutate(
    start = format(start,"%Y"),
    desc_count = factor(desc_count)) %>% 
  group_by(start,desc_count) %>%  
  summarise(mean_budget = mean(budget)) %>%
  inner_join(inflation,by = c("start"="year")) %>% 
  mutate(budget_with_inflation = mean_budget/cumulative_val)

data_bar_descriptors_per_panel %>%  
  ggplot(aes(x=start,y=budget_with_inflation/1000,color=desc_count,group=desc_count))  +
  geom_line(size=1.2)  +
  geom_point() +
  xlab("Rok") +
  ylab("Średni budżet") +
  guides(color=guide_legend(title="Liczba\ndeskryptorów")) +
  scale_color_manual(values=c("#9D47B3", "#D49837",'#0D9E61')) +
  scale_y_continuous(labels = label_comma(suffix = " tys. zł"))+
  theme_wd1()

ggsave(filename="../plots/plot6.svg",
       width = 185,
       height = 130,
       units = "mm")

##### DATA 7-9 #####
data_top_descriptors_per_panel <- grants_mapped %>% 
  select("contest","descriptors", "panel","budget") %>%
  group_by(panel,descriptors) %>%  
  summarise(mean_budget=mean(budget)) %>% 
  arrange(desc(mean_budget) ) %>% 
  mutate(desc_panel = substr(descriptors,0,2))
##### 7 #####
data_top_descriptors_per_panel %>% 
  filter(panel == "HS")%>% 
  top_n(5,mean_budget) %>% 
  mutate(
    descriptors = case_when(
      descriptors == "ST8_005" ~ "Energetyka",
      descriptors == "NZ9_003" ~ "Leśnictwo",
      descriptors == "NZ7_014" ~ "Farmacja",
      descriptors == "ST5_019" ~ "Met. badań\nwł. materiałów",
      descriptors == "ST7_007" ~ "Przetwarzanie\nsygnałów",
      TRUE ~ descriptors
    )
  ) %>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
                                     y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#00ba38", "#619cff", "#ffffff"))+
  labs(title="TOP 5 deskryptorów występujących w grantach panelu HS", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot7.svg",
       width = 185,
       height = 130,
       units = "mm")


##### 8 #####
data_top_descriptors_per_panel %>%
  filter(panel == "ST")%>% 
  top_n(5,mean_budget) %>%   
  mutate(
    descriptors = case_when(
      descriptors == "NZ4_016" ~ "Farmacja",
      descriptors == "NZ5_009" ~ "Prewencja\nchorób",
      descriptors == "NZ1_008" ~ "Inżynieria\ntkankowa",
      descriptors == "NZ3_003" ~ "Apoptoza",
      descriptors == "ST9_014" ~ "Astronomia fal\ngrawitacyjnych",
      TRUE ~ descriptors
    )
  ) %>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
             y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#00ba38", "#619cff", "#ffffff"))+
  labs(title="TOP 5 deskryptorów występujących w grantach panelu ST", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot8.svg",
       width = 185,
       height = 130,
       units = "mm")


##### 9 #####
data_top_descriptors_per_panel %>%
  filter(panel == "NZ")%>% 
  top_n(5,mean_budget) %>% 
  mutate(
    descriptors = case_when(
      descriptors == "HS6_005" ~ "Psychologia",
      descriptors == "NZ1_006" ~ "Biologia\nsyntetyczna",
      descriptors == "ST10_003" ~ "Klimatologia",
      descriptors == "ST10_008" ~ "Paleontologia",
      descriptors == "ST8_008" ~ "Inżynieria\nmateriałowa",
      TRUE ~ descriptors
    )
  ) %>% 
  ggplot(aes(x=reorder(descriptors,mean_budget,sum),
             y=mean_budget/1e6,fill=desc_panel)) + 
  coord_flip() +
  geom_bar(position ="dodge" ,stat = "identity")+
  ylab("Średni budżet")+
  scale_y_continuous(labels = label_comma(suffix = " mln zł"))+
  xlab("Deskryptor")+
  scale_fill_manual(values=c("#f8766d","#00ba38", "#619cff"))+
  labs(title="TOP 5 deskryptorów występujących w grantach panelu NZ", subtitle = "pod względem średniego budżetu")+
  guides(fill=guide_legend(title="Panel"))+
  theme_wd1()

ggsave(filename="../plots/plot9.svg",
       width = 185,
       height = 130,
       units = "mm")








