##### LIBRARIES #####
library(dplyr)
library(ggplot2)

##### DATA #####
grants_df <- read.csv("data/grants_mapped.csv",
                       stringsAsFactors = FALSE)

##### Clear data #####
grants_df <-mutate(grants_df,
                      year_annaunced = sapply(strsplit(grants_df$announced,'-'),
                                              function(i)i[1]),
                      year_start = sapply(strsplit(grants_df$start,'-'),
                                          function(i)i[1]),
                      subpanel = panel
) %>% 
  mutate(
    panel = gsub("[0-9]","",panel)
  )

##### Filter years 15 16 #####
grants_15_16 <- filter(grants_df, year_annaunced %in% c(2015,2016))


##### Panel popularity #####
grants_15_16 %>% 
  group_by(panel) %>% 
  summarise(n=n(), panel_budget=sum(budget)/1e6) %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  ggplot(aes(x=n,y=panel_budget,
             color=panel_color, label=panel))+
  geom_point() +
  geom_label(size = 4)+
  labs(title="Panel budget and project count in 2015-2016")+
  xlab("Project count")+
  ylab("Budget [mln zł]")+
  xlim(0,NA)+
  ylim(0,NA)+
  guides(fill=FALSE)

##### Panel mean budget boxplot #####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  group_by(subpanel,panel, panel_color) %>% 
  summarise(n=n(), subpanel_budget=sum(budget)/1e6) %>% 
  ggplot(aes(x=panel,y=subpanel_budget,
             color=panel_color, label=panel))+
  geom_boxplot()+
  labs(title="Mean subpanel budget in 2015-2016")+
  xlab("Panel")+
  ylab("Budget [mln zł]")+
  guides(color=FALSE)

##### Project budget boxplot ####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  mutate(
    budget = budget / 1e3
  ) %>% 
  ggplot(aes(x=panel, y=budget, color=panel_color))+
  geom_boxplot()+
  labs(title="Projects budget in 2015-2016")+
  xlab("Panel")+
  ylab("Budget [tys zł]")+
  ylim(0,2000)+
  guides(color=FALSE)


##### Subpanel popularity #####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  group_by(subpanel,panel, panel_color) %>% 
  summarise(n=n(), subpanel_budget=sum(budget)/1e6) %>% 
  mutate(
    budget_grt_100 = subpanel_budget > 100
  ) %>% 
  ggplot(aes(x=n,y=subpanel_budget,
             color=panel_color, label=subpanel))+
  geom_point() +
  geom_label(size = 3)+
  labs(title="Subanel budget and project count in 2015-2016")+
  xlab("Project count")+
  ylab("Budget [mln zł]")+
  xlim(0,NA)+
  ylim(0,NA)+
  guides(color=FALSE)+
  facet_grid(cols=vars(panel))

##### Bugdet split by years ####
grants_15_16 %>% 
  group_by(year_annaunced) %>% 
  summarise(budget_sum = sum(budget)/1e6) %>% 
  ggplot(aes(x=year_annaunced, y=budget_sum, label=budget_sum))+
  geom_col()+
  ylim(0,1300)+
  geom_text(size=4, vjust=-1)+
  labs(title="Overall budget in years 2015-2016")+
  ylab("Budget [mln zł]")+
  xlab("Year")

##### Panel budget split by years ####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  group_by(panel,year_annaunced,panel_color) %>% 
  summarise(budget_sum = round(sum(budget)/1e6,2))  %>% 
  mutate(
    panel_year = paste(panel,year_annaunced,sep="_")
  ) %>% 
  ggplot(aes(x=panel_year, y=budget_sum, fill = panel_color, label=budget_sum))+
  geom_col()+
  geom_text(vjust=-1)+
  ylim(0,600)+
  guides(fill=FALSE)+
  ylab("Panel budget [mln zł]")

##### Coinvestigators count ####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  group_by(panel,year_annaunced,panel_color) %>% 
  summarise(coinvestigators_sum = sum(coinvestigators))  %>% 
  mutate(
    panel_year = paste(panel,year_annaunced,sep="_")
  ) %>% 
  ggplot(aes(x=panel_year, y=coinvestigators_sum, fill = panel_color, label=coinvestigators_sum))+
  geom_col()+
  geom_text(vjust=-1)+
  ylim(0,4900)+
  guides(fill=FALSE)+
  ylab("Coinvestigators count")


##### Project count ####
grants_15_16 %>% 
  mutate(
    panel_color = case_when(
      panel == "HS" ~ "red",
      panel == "ST" ~ "blue",
      panel == "NZ" ~ "green",
    )
  ) %>% 
  group_by(panel,year_annaunced,panel_color) %>% 
  summarise(project_count = n())  %>% 
  mutate(
    panel_year = paste(panel,year_annaunced,sep="_")
  ) %>% 
  ggplot(aes(x=panel_year, y=project_count, fill = panel_color, label=project_count))+
  geom_col()+
  geom_text(vjust=-1)+
  ylim(0,1200)+
  guides(fill=FALSE)+
  ylab("Project count")


##### Budget histogram ####
grants_15_16 %>% 
  mutate(
    budget = log10(budget)
  ) %>% 
  ggplot(aes(budget))+
  geom_histogram(bins=45)+
  xlab("log10(budget)")+
  ylab("Project count")+
  scale_x_continuous(breaks=c(4.301,4.6989,5,5.301,5.6989,6, 6.3979),
                   labels=c("20 tys zł","50 tys zł", "100 tys zł","200 tys zł","500 tys zł", "1 mln zł", "2.5 mln zł"))


grants_15_16 %>% 
  mutate(
    budget = log10(budget)
  ) %>% 
  ggplot(aes(budget))+
  geom_histogram(bins=45)+
  xlab("log10(budget)")+
  ylab("Project count")+
  facet_grid(rows=vars(year_annaunced))+
  scale_x_continuous(breaks=c(4.301,4.6989,5,5.301,5.6989,6, 6.3979),
                     labels=c("20 tys zł","50 tys zł", "100 tys zł","200 tys zł","500 tys zł", "1 mln zł", "2.5 mln zł"))



#### Buget over years ####
grants_df %>% 
  group_by(year_annaunced) %>% 
  summarise(budget_sum = sum(budget)) %>% 
  ggplot(aes(x=year_annaunced,y=budget_sum))+
  geom_point()
