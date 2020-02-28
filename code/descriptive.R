library(tidyverse)
library(lubridate)
library(here)
here <- here::here

confirmed <- read_csv(here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Confirmed.csv"))
death <- read_csv(here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Deaths.csv"))


china_confirmed <- confirmed %>% 
  filter(`Country/Region` == "Mainland China") %>% 
  select(-c(`Country/Region`, Lat, Long)) %>% 
  pivot_longer(-`Province/State`,
               names_to = "date",
               values_to = "confirmed_cum") %>% 
  rename(province = `Province/State`) %>% 
  group_by(province) %>% 
  mutate(confirmed_daily = confirmed_cum - lag(confirmed_cum),
         confirmed_daily = replace_na(confirmed_daily, 0))

china_death <- death %>% 
  filter(`Country/Region` == "Mainland China") %>% 
  select(-c(`Country/Region`, Lat, Long)) %>% 
  pivot_longer(-`Province/State`,
               names_to = "date",
               values_to = "death_cum") %>% 
  rename(province = `Province/State`) %>% 
  group_by(province) %>% 
  mutate(death_daily = death_cum - lag(death_cum),
         death_daily = replace_na(death_daily, 0))

china <- left_join(
  china_confirmed,
  china_death,
  by = c("date", "province")
) %>% 
  mutate(date = mdy(date))

china1 <- china %>% 
  group_by(date) %>% 
  summarise(confirmed_cum = sum(confirmed_cum),
            confirmed_daily = sum(confirmed_daily),
            death_cum = sum(death_cum),
            death_daily = sum(death_daily)) %>% 
  ungroup() %>% 
  mutate(death_rate = death_cum/confirmed_cum)


ggplot() +
  geom_line(data = china1, aes(x = date, y = confirmed_daily)) +
  geom_line(data = china1, aes(x = date, y = death_daily))


ggplot() +
  geom_line(data = )

hubei
outside hubei
outside china

hubei <- china %>% 
  filter(`Province/State` == "Hubei")
