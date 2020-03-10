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

library(glue)

plot_subpanels <- function(df,panel_name)
  df %>% 
    group_by(subpanel,year) %>% 
    filter(panel == panel_name) %>% 
    summarise(n = n()) %>% 
    ggplot(aes(x=year, y=n, color=subpanel, group=subpanel)) +
      geom_line() +
      labs(title = glue("Project count in {panel_name} panel from 2011 to 2019"))

plot_subpanels(grands_df,"HS")
plot_subpanels(grands_df,"NZ")
plot_subpanels(grands_df,"ST")



