library(dplyr)
library(ggplot2)

data_source <- "../data/grants_mapped.csv"

data <- read.csv(data_source, stringsAsFactors = FALSE)

data <- mutate(data, subpanel = panel,
               panel = gsub(pattern = "[0-9]", replacement = "", x = panel),
               announced_year = strsplit(announced, split = "-") %>% 
                 sapply(first)) %>% 
  filter(announced_year %in% c(2017, 2018))

count_plot <- data %>% 
  ggplot(aes(x = contest, fill = panel)) + 
  geom_bar() +
  ylab('Number of projects') +
  xlab('Contest type')

count_plot

count_plot2 <- data %>% 
  ggplot(aes(x = contest, fill = panel)) + 
  geom_bar() +
  ylab('Number of projects') +
  xlab('Contest type') +
  facet_wrap(~announced_year) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

count_plot2

all_budget_plot <- data %>% 
  ggplot(aes(x = contest, y = budget, fill = panel)) + 
  geom_col() +
  ylab('Budget') +
  xlab('Contest type') +
  facet_wrap(~announced_year) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

all_budget_plot

favourite_contests <- data %>% group_by(contest) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(5)

budget_plot <- data %>% ggplot(aes(x = contest, y = budget, fill = panel)) +
  geom_col() +
  ylab('Budget') +
  xlab('Contest type')

budget_plot

# Zrobić dla budżetu facetę po roku

budget2_plot <- data[data$contest %in% favourite_contests$contest, ] %>% 
  ggplot(aes(x = panel, y = budget)) +
  geom_boxplot() +
  
  facet_wrap(~announced_year)

budget2_plot

data_subpanel <- data %>% group_by(panel, subpanel) %>% 
  summarise(mean_budget = mean(budget), sum_budget = sum(budget), number = n())

mean_budget <- data_subpanel %>% 
  ggplot(aes(x = number, y = mean_budget, label = subpanel, col = panel)) + 
  geom_point() +
  geom_label() +
  ylab('Mean budget') + 
  xlab('Number of projects')

mean_budget

sum_budget <- data_subpanel %>% 
  ggplot(aes(x = number, y = sum_budget, label = subpanel, col = panel)) + 
  geom_point() +
  geom_label() +
  ylab('Total budget') + 
  xlab('Number of projects')

sum_budget



#------------------------------------------------------------------------

raw_data <- read.csv(data_source, stringsAsFactors = FALSE)

raw_data <- mutate(raw_data, subpanel = panel,
       panel = gsub(pattern = "[0-9]", replacement = "", x = panel),
       announced_year = strsplit(announced, split = "-") %>% 
         sapply(first))

panel_sum_data <- raw_data %>% group_by(panel, announced_year) %>% 
  summarise(budget = sum(budget)) %>% 
  mutate(panel_over_year = paste(panel, '_', announced_year))

sum_budget_plot <- panel_sum_data %>% 
  ggplot(aes(x = panel_over_year, y = budget, fill = panel)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

sum_budget_plot
