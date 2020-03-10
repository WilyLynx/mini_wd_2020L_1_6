library(dplyr)
library(ggplot2)


data <- read.csv('../2020L-WizualizacjaDanych/data/grants_larger.csv', stringsAsFactors = FALSE)


contests_df <- mutate(data,
                      type = gsub(pattern = "Konkurs: ", replacement = "", x = type) %>% 
                        strsplit(split = " ") %>% 
                        sapply(first), 
                      panel = strsplit(subpanel, split = "-") %>% 
                        sapply(first) %>% 
                        gsub(pattern = "[0-9]", replacement = "", x = .))


panels_in_contests <- contests_df %>% group_by(panel, type) %>% summarise(number = n())


ggplot(data = panels_in_contests, 
       mapping = aes(x = panels_in_contests$type, y = log(panels_in_contests$number, base = 10), 
                     fill = panels_in_contests$panel)) +
  xlab("Contest type") + 
  ylab("Number") + 
  labs(fill = "Panel") + 
  geom_col()
