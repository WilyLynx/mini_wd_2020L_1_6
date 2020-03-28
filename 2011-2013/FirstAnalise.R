# ====================================== LIBRARIES ========================================
library(dplyr)
library(ggplot2)
library(sqldf)

# ====================================== FUNCTIONS =======================================
second <- function(x) {
  x[2]
}

get_list <- function(x) {
  x[[1]]
}


# ======================================== DATA ==========================================
org_data <-
  read.csv('../data/grants_cleared.csv',
           stringsAsFactors = FALSE,
           encoding = "UTF-8")

all_contests_df <- mutate(org_data,
                      year =               
                        strsplit(start,split = "-") %>%    
                        sapply(first) %>%    
                        as.integer(),
                      month =   
                        strsplit(start,split = "-") %>%
                        sapply(second) %>%
                        as.integer(),
                      day =
                        strsplit(start,split = "-") %>%
                        sapply(last) %>%
                        as.integer(),
                      monthly_budget = 
                        budget/duration,
                      monthly_budget_per_person = 
                        monthly_budget/coinvestigators,
                      descriptors_count = 
                        strsplit(descriptors, split = "|", fixed=TRUE) %>% 
                        sapply(unlist) %>% 
                        sapply(length)
                        
)

# =================================== VARIABLES ============================================
years_range_sql = "select * from all_contests_df where year>=2011 and year <=2013 and coinvestigators IS NOT NULL"
all_data_sql = "select * from all_contests_df"



# =================================== CONFIG ==============================================
current_sql = years_range_sql
contests_df <- sqldf(current_sql)

contests_df[5693,]

# ================================ ANALYSIS DATA============================================
# --------------------- WITHOUT DIVISION TO PANELS OR CONTESTS -----------------------------
#statystyki w poszczególnych latach 
statistics_in_years <- 
  contests_df %>% group_by(year) %>% summarise(grants_count = n(), 
                                               budget_sum = sum(budget),
                                               budget_mean = mean(budget),
                                               monthly_budget_sum = sum(monthly_budget),
                                               monthly_budget_mean = mean(monthly_budget),
                                               monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                               monthly_budget_per_person_mean = mean(monthly_budget_per_person))

#statystyki w poszczególnych miesi¹cach (typ miesi¹c na przestrzeni lat)
statistics_in_months <- 
  contests_df %>% group_by(month) %>% summarise(grants_count = n(), 
                                                budget_sum = sum(budget),
                                                budget_mean = mean(budget),
                                                monthly_budget_sum = sum(monthly_budget),
                                                monthly_budget_mean = mean(monthly_budget),
                                                monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                                monthly_budget_per_person_mean = mean(monthly_budget_per_person))

#statystyki w poszczególnych dniach (typ dzieñ na przestrzeni miesiêcy i lat)
statistics_in_days <- 
  contests_df %>% group_by(day) %>% summarise(grants_count = n(), 
                                              budget_sum = sum(budget),
                                              budget_mean = mean(budget),
                                              monthly_budget_sum = sum(monthly_budget),
                                              monthly_budget_mean = mean(monthly_budget),
                                              monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                              monthly_budget_per_person_mean = mean(monthly_budget_per_person))

#statystyki w poszczególnych miesi¹cach w poszczególnych latach 
statistics_in_years_and_months <- 
  contests_df %>% group_by(year,month) %>%  summarise(grants_count = n(), 
                                                      budget_sum = sum(budget),
                                                      budget_mean = mean(budget),
                                                      monthly_budget_sum = sum(monthly_budget),
                                                      monthly_budget_mean = mean(monthly_budget),
                                                      monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                                      monthly_budget_per_person_mean = mean(monthly_budget_per_person))

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach (na przestrzeni lat)
statistics_in_months_and_days <- 
  contests_df %>% group_by(month, day) %>%  summarise(grants_count = n(), 
                                                      budget_sum = sum(budget),
                                                      budget_mean = mean(budget),
                                                      monthly_budget_sum = sum(monthly_budget),
                                                      monthly_budget_mean = mean(monthly_budget),
                                                      monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                                      monthly_budget_per_person_mean = mean(monthly_budget_per_person))

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach w poszczególnych latach
statistics_in_years_months_and_days <- 
  contests_df %>% group_by(year,month, day) %>%  summarise(grants_count = n(), 
                                                           budget_sum = sum(budget),
                                                           budget_mean = mean(budget),
                                                           monthly_budget_sum = sum(monthly_budget),
                                                           monthly_budget_mean = mean(monthly_budget),
                                                           monthly_budget_per_person_sum = sum(monthly_budget_per_person),
                                                           monthly_budget_per_person_mean = mean(monthly_budget_per_person))
