library(dplyr)
library(ggplot2)


org_data <- read.csv('../2020L-WizualizacjaDanych/data/grants_larger.csv', stringsAsFactors = FALSE, encoding = "UTF-8")
descriptors <-read.csv('../2020L-WizualizacjaDanych/data/descriptors.csv', stringsAsFactors = FALSE)
org_data[["subpanel"]] <- c()
data <- merge(org_data,descriptors,by="id")

second <- function(x){ 
  x[2]
  }



contests_df <- mutate(data,
                      year = strsplit(type, split=" ") %>% 
                        sapply(last) %>% 
                        strsplit(split="-") %>% 
                        sapply(first),
                      month = strsplit(type, split=" ") %>% 
                        sapply(last) %>% 
                        strsplit(split="-") %>% 
                        sapply(second),
                      day =  strsplit(type, split=" ") %>% 
                        sapply(last) %>% 
                        strsplit(split="-") %>% 
                        sapply(last) ,
                      type = gsub(pattern = "Konkurs: ", replacement = "", x = type) %>% 
                        strsplit(split = " ") %>% 
                        sapply(first), 
                      panel = strsplit(subpanel, split = "-") %>% 
                        sapply(first) %>% 
                        gsub(pattern = "[0-9]", replacement = "", x = .),
                      budget =gsub(pattern = "[a-zA-z :]", replacement = "", x = budget) %>% 
                        as.numeric(),
                      duration = gsub(pattern = "[a-zA-Z¹ê: ]", replacement = "", x=duration) %>% 
                        as.numeric(),
                      coinvestigators = gsub(pattern = "[a-zA-Z¹êó: ]", replacement = "", x=coinvestigators) %>% 
                        as.numeric()
                      )


panels_in_contests <- contests_df %>% group_by(panel, type) %>% summarise(number = n())
panels_2 <- contests_df %>% group_by(type) %>% summarise(number = n())

logarithmic_with_error <- ggplot(data = panels_in_contests, 
       mapping = aes(x = reorder(panels_in_contests$type, -panels_in_contests$number), 
                     y = panels_in_contests$number,
                     fill=panel)) +
  xlab("Contest type") + 
  ylab("Number") + 
  labs(fill = "Panel") + 
  geom_col() + 
  scale_y_log10()
logarithmic_with_error

logarithimic_without_panels <- ggplot(data = panels_2, 
      mapping = aes(x = reorder(panels_2$type, -panels_2$number), 
                    y = log(panels_2$number, base=10))) +
  xlab("Contest type") + 
  ylab("Number") + 
  labs(fill = "Panel") + 
  geom_col()
logarithimic_without_panels


ggplot(data = contests_df, 
       aes(x=type,
        fill=panel)) + 
  geom_bar() + scale_y_log10()

#heatmapa korelacji 
numeric_columns <- select_if(contests_df, is.numeric)





cormat<-signif(cor(numeric_columns),2)
cormat


#ilosc deskryoptorw dla danego id (dla projektu)
descriptors_count_per_id <-descriptors %>% group_by(id) %>% summarise(number=n())
project_count_per_descriptors_count <- descriptors_count_per_id %>% group_by(number) %>%  summarise(number2 = n())
barplot(as.matrix(project_count_per_descriptors_count$number2), xlab = project_count_per_descriptors_count$number)
ggplot(project_count_per_descriptors_count,
       aes(x=number,
           y=number2)) + 
  geom_col()




  