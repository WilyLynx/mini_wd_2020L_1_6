library(dplyr)
library(ggplot2)


data <- read.csv('../2020L-WizualizacjaDanych/data/grants_larger.csv', stringsAsFactors = FALSE)


grands_df <- mutate(data,
                    subpanel = strsplit(subpanel, split = "-") %>% 
                      sapply(first) %>% 
                      gsub(pattern = "[ ]+$", replacement = "", x = .),
                    year = strsplit(id, split = "/") %>% 
                      sapply(first)
                      ) %>% 
                    mutate(
                      panel = gsub(pattern = "[0-9]", replacement = "", x=subpanel)
                    )

panels_over_years <- grands_df %>% 
  group_by(panel,year) %>% 
  summarise(n = n())


ggplot(panels_over_years, aes(x=year,y = n, color=panel, group=panel)) +
  geom_line()+
  labs(title = "Project count in panel from 2011 to 2019 ")
