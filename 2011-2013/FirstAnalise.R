# ====================================== LIBRARIES ========================================
library(dplyr)
library(ggplot2)
library(sqldf)
library(lubridate)
library(scales)

# ====================================== FUNCTIONS =======================================
statistics <- function(x) {
  a <- summarise(.data = x,
                 grants_count = n(),
                 budget_sum = sum(budget),
                 budget_mean = mean(budget),
                 monthly_budget_sum = sum(monthly_budget),
                 monthly_budget_mean = mean(monthly_budget),
                 monthly_budget_sd = sd(budget),
                 monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                 monthly_budget_per_person_mean = mean(monthly_budget_per_person),
                 monthly_budget_per_person_sd = sd(monthly_budget_per_person),
                 person_count_mean = mean(coinvestigators),
                 person_count_sd = sd(coinvestigators))
  a
}

# ======================================== DATA ==========================================
org_data <-
  read.csv('../data/grants_cleared.csv',
           stringsAsFactors = FALSE,
           encoding = "UTF-8")

descriptors_data <- 
  read.csv('../data/descriptors.csv',
           stringsAsFactors = FALSE,
           encoding = "UTF-8")

org_data_not_fixed <-
  read.csv('../data/grants_fixed.csv',
           stringsAsFactors = FALSE,
           encoding = "UTF-8",
           sep = "#")

all_contests_df <- mutate(org_data,
                          year =
                            strsplit(start, split = "-") %>%
                            sapply(first) %>%
                            as.integer(),
                          month =
                            strsplit(start, split = "-") %>%
                            sapply(nth, n=2) %>%
                            as.integer(),
                          day =
                            strsplit(start, split = "-") %>%
                            sapply(last) %>%
                            as.integer(),
                          monthly_budget =
                            budget / duration,
                          monthly_budget_per_person =
                            monthly_budget / coinvestigators,
                          descriptors_count =
                            strsplit(descriptors, split = "|", fixed = TRUE) %>%
                            sapply(unlist) %>%
                            sapply(length),
                          subpanel =
                            strsplit(descriptors, split = "|", fixed = TRUE) %>%
                            sapply(first) %>%
                            strsplit(split = "_", fixed = TRUE) %>%
                            sapply(first),
                          sorted_descriptors = 
                            strsplit(descriptors, split = "|", fixed = TRUE) %>% 
                            sapply(sort) %>%
                            sapply(unlist) %>% 
                            sapply(paste, collapse="|"),
                          start = 
                            as.Date(start),
                          announced = 
                            as.Date(announced)
                          
)


all_descriptors_df <- mutate(descriptors_data,
                             year =
                               strsplit(id, split = "/", fixed = TRUE) %>%
                               sapply(first) %>%
                               as.integer(),
                             month =
                               strsplit(id, split = "/", fixed = TRUE) %>%
                               sapply(nth, n=2) %>%
                               as.integer(),
                             panel = 
                               gsub(subpanel ,pattern = "[0-9]", replacement = "")
                           )

# =================================== VARIABLES ============================================
contest_df_years_range_sql = "select * from all_contests_df where year>=2011 and year <=2013 and coinvestigators IS NOT NULL"
contest_df_specific_day = "select * from all_contests_df where year = 2011 and month = 12 and day = 1 and coinvestigators IS NOT NULL"
contest_df_all_data_sql = "select * from all_contests_df"
descriptors_df_years_range_sql = " select * from all_descriptors_df where year >=2011 and year <=2013"
descriptors_df_all_data_sql = "select * from all_descriptors_df"



# =================================== CONFIG ==============================================
setwd("C:/Users/DAMIAN/Desktop/STUIDA/Magisterskie - In¿. i analiza danych/Semestr 1/Wizualizacja Danych/Repo/mini_wd_2020L_1_6/2011-2013")
contests_current_sql = contest_df_years_range_sql
descriptors_current_sql = descriptors_df_years_range_sql
contests_df <- sqldf(contest_df_years_range_sql)
descriptors_df <- sqldf(descriptors_current_sql)

# ================================ ANALYSIS DATA============================================
# --------------------- WITHOUT DIVISION TO PANELS OR CONTESTS -----------------------------
#statystyki w poszczególnych latach 
statistics_in_years <- 
  contests_df %>% group_by(year= floor_date(start, "year")) %>% statistics

#statystyki w poszczególnych miesi¹cach (typ miesi¹c na przestrzeni lat)
statistics_in_months <- 
  contests_df %>% group_by(month) %>% statistics

#statystyki w poszczególnych dniach (typ dzieñ na przestrzeni miesiêcy i lat)
statistics_in_days <- 
  contests_df %>% group_by(day) %>% statistics

#statystyki w poszczególnych miesi¹cach w poszczególnych latach 
statistics_in_years_and_months <- 
  contests_df %>% group_by(month = floor_date(start, "month")) %>%  statistics

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach (na przestrzeni lat)
statistics_in_months_and_days <- 
  contests_df %>% group_by(month, day) %>%  statistics

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach w poszczególnych latach
statistics_in_years_months_and_days <- 
  contests_df %>% group_by(day = floor_date(start, "day")) %>%  statistics

#statystyki dla poszczególnych iloœci deskryptorów 
statistics_descriptors_count <- 
  contests_df %>% group_by(descriptors_count) %>%  statistics

#statystyki dla poszczególnych konkursów
statistics_in_contests <- 
  contests_df %>% group_by(contest) %>% statistics

#statystyki dla poszczególnych paneli 
statistics_in_panels <-
  contests_df %>%  group_by(panel) %>%  statistics

#statystyki dla poszczególnych paneli w konkursach 
statistics_in_contests_and_panels  <- 
  contests_df %>%  group_by(contest, panel) %>%  statistics

#statystyki dla poszczególnych subpaneli 
statistics_in_subpanels <- 
  contests_df %>%  group_by(subpanel) %>% statistics

#iloœæ wniosków z podanymi deskryptorami 
descriptors_popularity <- 
  descriptors_df %>% group_by(descriptors) %>% summarise(count = n())

#bud¿et w poszczególnych deksryptorach (i grupach deskryptorów)
descriptors_budget <- 
  contests_df %>% group_by(sorted_descriptors) %>%  statistics

#statystyki wniosków w konkursach poszczególnych deksryptorów
decriptors_in_contests <- 
  contests_df %>% group_by(contest, sorted_descriptors) %>%  statistics

#statystyki wniosków w panelach poszczególnych deksryptorów 
descriptors_in_panels <-
  contests_df %>% group_by(panel, sorted_descriptors) %>% statistics

#statystyki wniosków po d³ugoœci trwania projektu 
statistics_in_duration <- 
  contests_df %>% group_by(duration) %>%  statistics

#statystyki wniosków po iloœci osób 
statistics_in_coinvestigators <- 
  contests_df %>% group_by(coinvestigators) %>% statistics

# ====================================== WYKRESY ====================================================================

#iloœæ wniosków w poszczególnych latach 
pl_grants_count_in_years =  
  ggplot(data = statistics_in_years,
         aes(x = year, 
             y = grants_count,
             fill = year)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%Y")) + 
  labs(title = " Grants number in years", 
       x = "Year",
       y = "Grants count")

pl_grants_count_in_years
  
#ca³kowity bud¿et w poszczególnych latach 
pl_grants_budget_in_years =  
  ggplot(data = statistics_in_years,
         aes(x = year, 
             y = budget_sum,
             fill = year)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%Y")) + 
  labs(title = " Grants total budget in years", 
       x = "Year",
       y = "Total budget")

pl_grants_budget_in_years

#œredni bud¿et w poszczególnych latach 


