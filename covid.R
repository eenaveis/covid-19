# Data acquired from: https://thl.fi/fi/tilastot-ja-data/aineistot-ja-palvelut/avoin-data/varmistetut-koronatapaukset-suomessa-covid-19-

library(tidyverse)
library(ggmap)

# Google Maps API key
api_key <- Sys.getenv("GOOGLE_MAPS")
covid1 <- read_csv2("./data/covid19case.csv")

covid1

#################
# Data cleaning #
#################

covid2 <- covid1

covid2 <- covid1 %>%
  separate(Aika, c("y", "year", "w", "week"), sep =" ") %>%
  select(-y, -w) %>%
  unite(date, c(year, week), sep = "") %>%
  mutate(date = paste(date, "04", sep = "")) %>%
  transmute(date = as.Date(date, "%Y%U%w"), area = Alue, cases = val) %>%
  filter(!is.na(cases), !is.na(date))

####################
# Summarising data #
####################

covid2 %>% group_by(area) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))

######################
# Data visualization #
######################
# plot top 5 areas
covid3 <- covid2 %>%
  filter(area != "Kaikki Alueet") %>%
  group_by(area) %>%
  summarise(total_cases = sum(cases)) %>%
  top_n(5)
  
ggplot(data = covid3) +
  geom_col(aes(x = area, y = total_cases, fill = area)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# plot cumulative cases
covid4 <- covid2 %>%
  semi_join(covid3, by = "area") %>%
  group_by(area) %>%
  mutate(cumulative_cases = cumsum(cases)) %>%
  ungroup()

ggplot(data = covid4) +
  geom_line(aes(x = date, y = cumulative_cases, group = area, color = area))

# plot total cases in a map

# area coordinates
coordinates_HUS <- c(24.943536799, 60.166640739)
coordinates_PS <- c(23.761290078, 61.497742570)
coordinates_PPS <- c(25.472099070, 65.013784817)
coordinates_VS <- c(21.615874122, 63.092588875)
coordinates_VSS <- c(22.266866666, 60.451690351)
# center point coordinates (Jyväskylä)
coordinates_center <- c(lon = 25.72088, lat = 62.24147)

lon <- c(coordinates_HUS[1], 
         coordinates_PS[1],
         coordinates_PPS[1], 
         coordinates_VS[1],
         coordinates_VSS[1])

lat <- c(coordinates_HUS[2], 
         coordinates_PS[2],
         coordinates_PPS[2], 
         coordinates_VS[2],
         coordinates_VSS[2])

covid5  <- covid3 %>% add_column(lon, lat)

# Set API key
register_google(api_key)

map <- ggmap(get_googlemap(coordinates_center, zoom = 6))

map + 
  geom_point(aes(x = lon, y = lat, size = total_cases), 
             data = covid5, 
             color = "blue", 
             alpha = 0.5
            ) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  scale_size_continuous(range = c(5, 30))

