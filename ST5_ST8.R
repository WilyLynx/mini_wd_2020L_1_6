library(dplyr)
library(ggplot2)

grants_raw <- read.csv("data/grants.csv",sep='*',
                       stringsAsFactors = FALSE)

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

descriptors <- read.csv('../2020L-WizualizacjaDanych/data/descriptors.csv', stringsAsFactors = FALSE)


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


descriptors_in_proj_sub_ST5_ST8 %>% 
  filter(grepl("ST5_|ST8_", descriptors)) %>% 
  inner_join(grants_raw, by='id') %>% 
  select(id, subpanel=subpanel.x, descriptor=descriptors.x, budget) %>% 
  group_by(descriptor) %>% 
  summarise(
    n = n(),
    budget = sum(budget)
  ) %>% 
  ggplot(aes(x=n, y=budget, label = descriptor))+
  geom_point()+
  geom_label(size=3)
  
