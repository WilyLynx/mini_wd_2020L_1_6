##### LIBRARIES #####
library(dplyr)
library(ggplot2)
library(scales)

##### DATA #####
grants_raw <- read.csv("data/grants.csv",sep='*',
                       stringsAsFactors = FALSE)
descriptors <- read.csv('../2020L-WizualizacjaDanych/data/descriptors.csv', 
                        stringsAsFactors = FALSE)


##### DATA CLEANING #####
grants_raw <- mutate(grants_raw,
  coinvestigators = sapply(coinvestigators, function(val) as.numeric(as.character(val))),
  budget = as.numeric(gsub(pattern = "[a-z: ]", replacement = "", 
                       budget,
                       ignore.case = TRUE)),
  subpanel = panel
) %>% 
  mutate(
    panel = gsub(pattern = "[0-9 ]", replacement = "",
                 subpanel,
                 ignore.case = TRUE)
  ) %>% 
  filter(
    panel %in% c("NZ", "ST", "HS")
  ) %>% 
  na.omit()


###### SUBPANELS PLOT ######
# BUDGET - PROJECT COUNT #

panel_df <- mutate(grants_raw,
                   panel_color = case_when(
                     panel == "HS" ~ "red",
                     panel == "TS" ~ "blue",
                     panel == "NZ" ~ "green",
                   ))

group_by(panel_df, panel, subpanel) %>% 
  summarise(budget = sum(budget), n = length(subpanel)) %>% 
  ggplot(aes(x = n, y = budget, color = panel, label = subpanel)) +
  geom_point() +
  geom_label(size = 4)


##### DESCRIPTORS IN ST5 ST8 #####
# TODO: Clear descriptors:
# leading 0 after _
# many unknown descriptors like ST5_23 

descriptors_in_proj_sub_ST5_ST8 <- descriptors %>% 
  filter(subpanel %in% c('ST5', 'ST8')) 

descriptors_in_proj_sub_ST5_ST8 %>% 
  group_by(id) %>% 
  count() %>% 
  ggplot(aes(x=n))+
  geom_histogram(bins = 10)+
  ylab("Project count") +
  xlab("Descriptors count")+
  labs(title = "Distribution of descriptors count used in",
       subtitle = 'projects from ST5 and ST8 subpanel')

descriptors_in_proj_sub_ST5_ST8 %>% 
  filter(grepl("ST5_|ST8_", descriptors)) %>% 
  group_by(id) %>% 
  count() %>% 
  ggplot(aes(x=n))+
  geom_histogram(bins = 10)+
  ylab("Project count") +
  xlab("Descriptors count")+
  labs(title = "Distribution of ST5_* and ST8_* descriptors count used in ",
       subtitle = 'projects from ST5 and ST8 subpanel')


###### ST5 ST8 DESCRIPTORS BUDGET ######

ST5_ST8_descriptors_budget <- descriptors_in_proj_sub_ST5_ST8 %>% 
  filter(grepl("ST5_|ST8_", descriptors)) %>% 
  inner_join(grants_raw, by='id') %>% 
  select(id, subpanel=subpanel.x, descriptor=descriptors.x, budget)


###### ST5 ST8 DESCRIPTORS PLOT ######
# BUDGET - PROJECT COUNT #

ST5_ST8_descriptors_budget %>% 
  group_by(descriptor) %>% 
  summarise(
    n = n(),
    budget = sum(budget)
  ) %>% 
  ggplot(aes(x=n, y=budget, label = descriptor))+
  geom_point()+
  geom_label(size=3) +
  xlab("Projects count")


##### ST8_8 BUDGET TO WHOLE ST8 #####
ST8_budget <- ST5_ST8_descriptors_budget %>% 
  filter(subpanel == "ST8")

ST8_8_projects <- ST8_budget %>% 
  filter(descriptor == "ST8_8") %>% 
  distinct(id, .keep_all = TRUE)

ST8_others_projects <- setdiff(ST8_budget,ST8_8_projects) %>% 
  distinct(id, .keep_all = TRUE)

bind_rows(
  ST8_8_projects %>% mutate(descriptor = "ST8_8"),
  ST8_others_projects %>% mutate(descriptor = "Others")
) %>%
  aggregate(budget~descriptor,data=.,FUN=sum) %>% 
  ggplot(aes(x="",y=budget, fill=descriptor))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(y = sum(budget)-c(0,cumsum(budget)[-length(budget)])-budget/2,
                label = percent(budget/sum(budget)/100)), size=5)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
       panel.grid = element_blank(),
       axis.ticks = element_blank())+
  labs(title="Money distribution in subpanel ST8",
         subtitle="projects with vs without ST8_8 descriptor")
  

##### ST8_8 BUDGET TO WHOLE ST8 #####
ST5_budget <- ST5_ST8_descriptors_budget %>% 
  filter(subpanel == "ST5")

ST5_23_projects <- ST5_budget %>% 
  filter(descriptor == "ST5_23") %>% 
  distinct(id, .keep_all = TRUE)

ST5_others_projects <- setdiff(ST5_budget,ST5_23_projects) %>% 
  distinct(id, .keep_all = TRUE)

bind_rows(
  ST5_23_projects %>% mutate(descriptor = "ST5_23"),
  ST5_others_projects %>% mutate(descriptor = "Others")
) %>%
  aggregate(budget~descriptor,data=.,FUN=sum) %>% 
  ggplot(aes(x="",y=budget, fill=descriptor))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(y = sum(budget)-c(0,cumsum(budget)[-length(budget)])-budget/2,
                label = percent(budget/sum(budget)/100)), size=5)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  labs(title="Money distribution in subpanel ST5",
       subtitle="projects with vs without ST5_23 descriptor")


##### ST8_8 BUDGET TO OTHERS IN ST8 and ST5  #####

ST5_ST8_budget <-  ST5_ST8_descriptors_budget %>% 
  filter(subpanel %in% c("ST5", "ST8"))

ST8_8_AND_ST5_23_projects <- ST5_ST8_budget %>% 
  filter(descriptor %in% c("ST8_8", "ST5_23")) %>% 
  group_by(id) %>% 
  summarise(n=n(),budget=sum(budget)) %>% 
  filter(n>1)
## ST8_8_AND_ST5_23_projects is empty ST8_8 and ST5_23 doesn't occure together


ST8_8_projects <- ST5_ST8_budget %>% 
  filter(descriptor == "ST8_8") %>% 
  distinct(id, .keep_all = TRUE)

ST5_23_projects <- ST5_ST8_budget %>% 
  filter(descriptor == "ST5_23") %>% 
  distinct(id, .keep_all = TRUE)

ST5_ST8_others_projects <- setdiff(ST5_ST8_budget, ST8_8_projects) %>% 
  setdiff(ST5_23_projects) %>% 
  distinct(id, .keep_all = TRUE)


bind_rows(
  ST8_8_projects %>% mutate(descriptor = "ST8_8"),
  ST5_23_projects %>% mutate(descriptor = "ST5_23"),
  ST5_ST8_others_projects %>% mutate(descriptor = "Others"),
  
) %>%
  aggregate(budget~descriptor,data=.,FUN=sum) %>% 
  ggplot(aes(x="",y=budget, fill=descriptor))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(y = sum(budget)-c(0,cumsum(budget)[-length(budget)])-budget/2,
                label = percent(budget/sum(budget)/100)), size=5)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())+
  labs(title="Money distribution in subpanel ST5 and ST8",
       subtitle="projects with descriptor ST8_8 or ST5_23 vs others")
