library(tidyverse)
library(tidylog)
library(lubridate)
library(here)
here <- here::here

confirmed <- read_csv(here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Confirmed.csv"))
death <- read_csv(here("csse_covid_19_data", "csse_covid_19_time_series", "time_series_19-covid-Deaths.csv"))

# China data ----

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
  mutate(hubei = ifelse(province == "Hubei", "Hubei", "Outside Hubei")) %>% 
  group_by(hubei, date) %>% 
  summarise(confirmed_cum = sum(confirmed_cum),
            confirmed_daily = sum(confirmed_daily),
            death_cum = sum(death_cum),
            death_daily = sum(death_daily)) %>% 
  ungroup() %>% 
  mutate(cfr = death_cum/confirmed_cum)

# China viz ----

hubei <- china1 %>% 
  filter(hubei == "Hubei")


ggplot(data = hubei) +
  geom_line(aes(x = date, y = confirmed_daily)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()
  

ggplot(data = hubei) +
  geom_line(aes(x = date, y = death_daily)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()

hubei_long <- hubei %>% 
  pivot_longer(col = c(confirmed_cum, death_cum),
               names_to = "Status",
               values_to = "cases") %>% 
  mutate(Status = case_when(Status == "confirmed_cum" ~ "Confirmed",
                            Status == "death_cum" ~ "Death"
  )
  )

ggplot(data = hubei_long) +
  geom_line(aes(x = date, y = cases, col = Status)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()

ggplot(data = hubei) +
  geom_line(aes(x = date, y = confirmed_cum)) +
  geom_line(aes(x = date, y = death_cum)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()

ggplot(data = hubei) +
  geom_line(aes(x = date, y = cfr)) +
  labs(x = "", y = "CFR", title = "Case Fatality Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  theme_light()

# Outside China data ----

out_china_confirmed <- confirmed %>% 
  filter(`Country/Region` != "Mainland China") %>% 
  select(-c(Lat, Long)) %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`),
               names_to = "date",
               values_to = "confirmed_cum") %>% 
  rename(country = `Country/Region`,
         province = `Province/State`) %>% 
  group_by(country) %>% 
  mutate(confirmed_daily = confirmed_cum - lag(confirmed_cum),
         confirmed_daily = replace_na(confirmed_daily, 0)) %>% 
  ungroup()

out_china_death <- death %>% 
  filter(`Country/Region` != "Mainland China") %>% 
  select(-c(Lat, Long)) %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`),
               names_to = "date",
               values_to = "death_cum") %>% 
  rename(country = `Country/Region`,
         province = `Province/State`) %>% 
  group_by(country) %>% 
  mutate(death_daily = death_cum - lag(death_cum),
         death_daily = replace_na(death_daily, 0)) %>% 
  ungroup()

out_china <- left_join(
  out_china_confirmed,
  out_china_death,
  by = c("date", "country", "province")
) %>% 
  mutate(date = mdy(date),
         country = case_when(country == "Italy" ~ "Italy",
                             country == "South Korea" ~ "South Korea",
                             TRUE ~ "Other")) %>% 
  select(-province)

out_china1 <- out_china %>% 
  group_by(date) %>% 
  summarise(confirmed_cum = sum(confirmed_cum),
            confirmed_daily = sum(confirmed_daily),
            death_cum = sum(death_cum),
            death_daily = sum(death_daily),
            cfr = death_cum/confirmed_cum) %>% 
  ungroup()

ggplot(data = out_china1) +
  geom_line(aes(x = date, y = confirmed_cum)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()

ggplot(data = out_china1) +
  geom_line(aes(x = date, y = cfr)) +
  labs(x = "", y = "", title = "CFR outside China") +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  theme_light()

out_china_country <- out_china %>% 
  group_by(country, date) %>% 
  summarise(confirmed_cum = sum(confirmed_cum),
            confirmed_daily = sum(confirmed_daily),
            death_cum = sum(death_cum),
            death_daily = sum(death_daily),
            cfr = death_cum/confirmed_cum) %>% 
  ungroup()

ggplot(data = out_china_country) +
  geom_line(aes(x = date, y = confirmed_daily, col = country)) +
  labs(x = "", y = "Cases", title = "Daily confirmed COVID-19 cases") +
  theme_light()