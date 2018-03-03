library(tidyverse)

# Most common types of eruptions in the last 100 years from of NOAA Significant
# Volcanic Eruption Database
volcanic_eruptions <- read_tsv("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$HAZ_EVENT_ID&t=102557&s=50&d=54&dfn=volerup.txt")

volcanic_eruptions <- volcanic_eruptions %>%
  rename(lat = Latitude, long = Longitude) %>%
  filter(Year > 1919)

# pull out most common types
common_types <- volcanic_eruptions %>% count(Type) %>%
  filter(n >10) %>% arrange(-n) %>% pull(Type)

# squash other types, factorize to make it easier to arrange them
volcanic_eruptions <- volcanic_eruptions %>%
  mutate(
    Type = ifelse(Type %in% common_types, Type, "other"),
    Type = factor(Type, levels = c(common_types, "other")))

devtools::use_data(volcanic_eruptions)

 
