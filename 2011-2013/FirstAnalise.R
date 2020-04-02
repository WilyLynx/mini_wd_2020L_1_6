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
                 budget_sum = sum(as.numeric(budget)),
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
setwd("C:/Users/DAMIAN/Desktop/STUIDA/Magisterskie - In¿. i analiza danych/Semestr 1/Wizualizacja Danych/Repo/mini_wd_2020L_1_6/2011-2013")
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
                          panel = 
                            gsub(panel, pattern = "[0-9]", replacement = ""),
                          sorted_descriptors = 
                            strsplit(descriptors, split = "|", fixed = TRUE) %>% 
                            sapply(sort) %>%
                            sapply(unlist) %>% 
                            sapply(paste, collapse="|"),
                          start = 
                            as.Date(start),
                          announced = 
                            as.Date(announced),
                          year_date = 
                            floor_date(start, "year"),
                          month_date = 
                            floor_date(start, "month"),
                          day_date = 
                            floor_date(start, "day"),
                          season = 
                            case_when(
                              month %in%  9:11  ~ "Jesieñ",
                              month %in%  c(12, 1, 2) ~ "Zima",
                              month %in%  3:5   ~ "Wiosna",
                              TRUE ~ "Lato"
                            ) %>% 
                            factor(levels = c("Wiosna", "Lato", "Jesieñ", "Zima"))
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
contest_df_years_range_sql = "select * from all_contests_df where year>=2011 and year <=2012 and coinvestigators IS NOT NULL"
contest_df_specific_day = "select * from all_contests_df where year = 2011 and month = 12 and day = 1 and coinvestigators IS NOT NULL"
contest_df_all_data_sql = "select * from all_contests_df"
descriptors_df_years_range_sql = " select * from all_descriptors_df where year >=2011 and year <=2012"
descriptors_df_all_data_sql = "select * from all_descriptors_df"



# =================================== CONFIG ==============================================
setwd("C:/Users/DAMIAN/Desktop/STUIDA/Magisterskie - In¿. i analiza danych/Semestr 1/Wizualizacja Danych/Repo/mini_wd_2020L_1_6/2011-2013")
contests_current_sql = contest_df_years_range_sql
descriptors_current_sql = descriptors_df_years_range_sql
contests_df <- sqldf(contest_df_all_data_sql)
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

#statystyki w poszczególnych porach roku
statistics_in_seasons <- 
  contests_df %>% group_by(season = floor_date(start, "season")) %>% statistics

#statystyki w poszczególnych miesi¹cach w poszczególnych latach 
statistics_in_years_and_months <- 
  contests_df %>% group_by(month = floor_date(start, "month")) %>%  statistics

#statystyki w poszczególnych miesi¹cach w poszczególnych latach z panelami
statistics_in_years_and_months_in_panels <- 
  contests_df %>% group_by(month = floor_date(start, "month"), panel) %>%  statistics

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach (na przestrzeni lat)
statistics_in_months_and_days <- 
  contests_df %>% group_by(month, day) %>%  statistics

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach w poszczególnych latach
statistics_in_years_months_and_days <- 
  contests_df %>% group_by(day = floor_date(start, "day")) %>%  statistics

#statystyki w poszczególnych dniach w poszczególnych miesi¹cach w poszczególnych latach z podzia³em na panele
statistics_in_days_and_panels <- 
  contests_df %>% group_by(day = floor_date(start, "day"), panel) %>%  statistics

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
pl_grants_count_in_years <-  
  ggplot(data = statistics_in_years,
         aes(x = year, 
             y = grants_count,
             fill = year)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%Y"),
               breaks = unique(contests_df[["year_date"]])) + 
  labs(title = "Iloœæ wniosków", 
       x = "Rok",
       y = "Iloœæ wniosków") + 
  theme(legend.position = "none")

pl_grants_count_in_years
  
#ca³kowity bud¿et w poszczególnych latach 
pl_grants_budget_in_years <-
  ggplot(data = statistics_in_years,
         aes(x = year, 
             y = budget_sum/1000000,
             fill = year)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%Y"),
               breaks = unique(contests_df[["year_date"]])) + 
  scale_y_continuous(labels = label_comma(suffix = " mln")) +
  labs(title = "Ca³kowity bud¿et przeznaczony na realizacjê wniosków", 
       x = "Rok",
       y = "Ca³kowity bud¿et") + 
  theme(legend.position = "none")

pl_grants_budget_in_years

#œredni bud¿et w poszczególnych latach 
pl_grants_mean_budget_in_years <-  
  ggplot(data = statistics_in_years,
         aes(x = year, 
             y = budget_mean,
             fill = year)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%Y"),
               breaks = unique(contests_df[["year_date"]])) + 
  scale_y_continuous(labels = label_comma(suffix = " z³")) +
  labs(title = "Œredni bud¿et przeznaczony na realizacjê wniosków", 
       x = "Rok",
       y = "Œredni bud¿et") + 
  theme(legend.position = "none")

pl_grants_mean_budget_in_years


#bud¿et boxplot w poszczególnych latach 
pl_grants_budget_boxplot_in_years <-
  ggplot(data = contests_df, 
         aes(x = year(start),
             y = budget,
             group = year(start))) + 
  geom_boxplot() +
  scale_y_log10(labels = label_comma(suffix = " z³")) + 
  labs(title = "Bud¿et wniosków",
       x = "Rok",
       y = "Bud¿et") + 
  scale_x_continuous(labels = label_number(accuracy = 1, big.mark="",),
                     breaks = unique(contests_df[["year"]]))
  
pl_grants_budget_boxplot_in_years


#bud¿et boxplot w poszczególnych latach z podzia³em na panele
pl_grants_budget_in_panels_boxplot_in_years <-
  ggplot(data = contests_df, 
         aes(x = year(start),
             y = budget,
             group = year(start),
             fill = panel)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(labels = label_dollar(suffix = " z³")) + 
  labs(title = "Bud¿et wniosków w poszczególnych panelach",
       x = "Rok",
       y = "Bud¿et w z³.") + 
  scale_x_continuous(labels = label_number(accuracy = 1, big.mark=""),
                     breaks = unique(contests_df[["year"]])) + 
  facet_grid(. ~panel)

pl_grants_budget_in_panels_boxplot_in_years

#bud¿et boxplot w poszczególnych porach roku 
pl_grants_budget_in_panels_boxplot_in_seasons <-
  ggplot(data = contests_df, 
         aes(x = season,
             y = budget,
             fill = season)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(labels = label_dollar(suffix = " z³")) + 
  labs(title = "Bud¿et wniosków w poszczególnych porach roku w latach 2011-2012",
       x = "Pora roku",
       y = "Bud¿et w z³.") + 
  scale_x_discrete(breaks = c("Wiosna", "Lato", "Jesieñ", "Zima")) + 
  facet_grid(season ~ year  )

pl_grants_budget_in_panels_boxplot_in_seasons


#iloœæ wniosków w poszczególnych miesi¹cach 
pl_grants_count_in_months <-  
  ggplot(data = statistics_in_years_and_months,
         aes(x = month, 
             y = grants_count)) +
  geom_line() +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = breaks_pretty(n=10)) + 
  scale_y_log10() +
  labs(title = "Iloœæ wniosków w poszczególnych miesi¹cach 2011-2012", 
       x = "Miesi¹c",
       y = "Iloœæ wniosków") 

pl_grants_count_in_months


#iloœæ wniosków w poszczególnych miesi¹cach w panelach 
pl_grants_count_in_months_in_panels <-  
  ggplot(data = statistics_in_years_and_months_in_panels,
         aes(x = month, 
             y = grants_count,
             colour = panel)) +
  geom_line(show.legend = TRUE,
            size=1.3) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = breaks_pretty(n=5)) + 
  scale_y_log10() +
  labs(title = "Iloœæ wniosków w poszczególnych miesi¹cach 2011-2012", 
       x = "Miesi¹c",
       y = "Iloœæ wniosków") 

pl_grants_count_in_months_in_panels


#ca³kowity bud¿et w poszczególnych miesi¹cach
pl_grants_budget_in_years_and_months <-
  ggplot(data = statistics_in_years_and_months,
         aes(x = month,
             y = budget_sum,
             fill = year(month) )) +
  geom_bar(stat = "identity") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = unique(statistics_in_years_and_months[["month"]])) +
  scale_y_log10(labels = label_comma(suffix = " z³"),
                breaks = breaks_log(n=6)) +
  labs(title = "Ca³kowity bud¿et przeznaczony na realizacjê wniosków z poszczególnych miesiêcy 2011-2012",
       x = "Miesi¹c",
       y = "Ca³kowity bud¿et") + 
  facet_grid(. ~ year(month),
             space = "free",
             shrink = TRUE,
             scales="free_x")

pl_grants_budget_in_years_and_months


#œredni bud¿et w poszczególnych miesi¹cach
pl_grants_mean_budget_in_years_and_months <-
  ggplot(data = statistics_in_years_and_months,
         aes(x = month,
             y = budget_mean,
             fill = month)) +
  geom_bar(stat = "identity") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = unique(statistics_in_years_and_months[["month"]])) +
  scale_y_continuous(labels = label_comma(suffix = "z³")) +
  labs(title = "Œredni bud¿et przeznaczony na realizacjê wniosków z poszczególnych miesiêcy 2011-2012",
       x = "Miesi¹c",
       y = "Œredni bud¿et") 
pl_grants_mean_budget_in_years_and_months


#œredni bud¿et miesiêczny w poszczególnych miesi¹cach
pl_grants_mean_monthly_budget_in_years_and_months <-
  ggplot(data = statistics_in_years_and_months,
         aes(x = month,
             y = monthly_budget_mean,
             fill = month)) +
  geom_bar(stat = "identity") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = unique(statistics_in_years_and_months[["month"]])) +
  scale_y_continuous(labels = label_comma(suffix = "z³")) +
  labs(title = "Œredni bud¿et miesiêczny wniosków startuj¹cych w poszczególnych miesi¹cach 2011-2012",
       x = "Miesi¹c",
       y = "Œredni bud¿et miesiêczny")  


pl_grants_mean_monthly_budget_in_years_and_months


#œredni bud¿et miesiêczny w poszczególnych miesi¹cach w poszczególnych panelach
pl_grants_mean_monthly_budget_in_years_and_months_in_panels <-
  ggplot(data = statistics_in_years_and_months_in_panels,
         aes(x = month,
             y = monthly_budget_mean,
             fill = panel)) +
  geom_bar(stat = "identity",
           show.legend = TRUE) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_x_date(labels = date_format("%b %Y"),
               breaks = unique(statistics_in_years_and_months[["month"]])) +
  scale_y_continuous(labels = label_comma(suffix = "z³")) +
  labs(title = "Œredni bud¿et miesiêczny wniosków startuj¹cych w poszczególnych miesi¹cach",
       x = "Miesi¹c",
       y = "Œredni bud¿et miesiêczny") + 
  facet_grid(panel ~ .)

pl_grants_mean_monthly_budget_in_years_and_months_in_panels




#œredni bud¿et w poszczególnych dniach
pl_grants_mean_budget_in_years_and_months_and_days <-
  ggplot(data = statistics_in_years_months_and_days,
         aes(x = day,
             y = budget_mean)) +
  geom_line() +
  scale_x_date(labels = date_format("%d %b %Y"),
               breaks = breaks_pretty(n=20)) +
  scale_y_continuous(labels = label_comma(suffix = "z³")) +
  labs(title = "Œredni bud¿et przeznaczony na realizacjê projektów startuj¹cych w poszczególnych dniach 2011-2012",
       x = "Dzieñ",
       y = "Œredni bud¿et")  


pl_grants_mean_budget_in_years_and_months_and_days


#œredni bud¿et w poszczególnych dniach z podzia³em na panele
pl_grants_mean_budget_in_days_and_panels <-
  ggplot(data = statistics_in_days_and_panels,
         aes(x = day,
             y = budget_mean, 
             color= panel )) +
  geom_line(show.legend = TRUE,
            size = 1.3) +
  scale_x_date(labels = date_format("%d %b %Y"),
               breaks = breaks_pretty(n=20)) +
  scale_y_continuous(labels = label_comma(suffix = "z³")) +
  labs(title = "Œredni bud¿et przeznaczony na realizacjê wniosków startuj¹cych w poszczególne dni 2011-2012",
       x = "Dzieñ",
       y = "Œredni bud¿et") 

pl_grants_mean_budget_in_days_and_panels


#bud¿et boxplot w poszczególnych miesi¹cach 
pl_grants_budget_boxplot_in_years_and_months <-
  ggplot(data = contests_df, 
         aes(x = month_date,
             y = budget/1000,
             group = month)) + 
  geom_boxplot(show.legend = TRUE) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_y_log10(labels = label_comma(suffix = " tys. z³")) + 
  scale_x_date(labels = date_format("%b"),
               breaks = unique(contests_df[["month_date"]]))+
  labs(title = "Bud¿et wniosków w poszczególnych miesi¹cach",
       x = "Miesi¹c",
       y = "Bud¿et w tys. z³.") + 
  facet_grid(. ~ year,
             space = "free",
             shrink = TRUE,
             scales="free_x")
  

pl_grants_budget_boxplot_in_years_and_months



#bud¿et boxplot w poszczególnych miesi¹cach w poszczególnych panelach 
pl_grants_budget_boxplot_in_years_and_months_and_panels <-
  ggplot(data = contests_df, 
         aes(x = month_date,
             y = budget/1000,
             group = month,
             fill = panel)) + 
  geom_boxplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.3
    )
  ) +
  scale_y_log10(labels = label_comma(suffix = " tys. z³")) + 
  scale_x_date(labels = date_format("%b"),
               breaks = unique(contests_df[["month_date"]]))+
  labs(title = "Bud¿et wniosków w poszczególnych miesi¹cach z podzia³em na panele",
       x = "Miesi¹c",
       y = "Bud¿et w tys. z³.") + 
  facet_grid(panel ~ year,
             space = "free",
             shrink = TRUE,
             scales="free_x")


pl_grants_budget_boxplot_in_years_and_months_and_panels


pl_grants_and_coinvestigators <- 
  ggplot(data = statistics_in_coinvestigators,
         aes(
           x = coinvestigators, 
           y = monthly_budget_per_person_mean
         )) + 
  geom_bar(stat = "identity") 

pl_grants_and_coinvestigators




pl_grants_and_contests <- 
  ggplot(data = contests_df,
         aes(
           x = contest, 
           y = budget
         )) + 
  geom_bar(stat= "sum") + 
  facet_grid( season ~ .)

pl_grants_and_contests













