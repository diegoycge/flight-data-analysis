library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(shiny)
library(forcats)
library(stringr)
library(marginaleffects)
library(lubridate)
library(tidyr)
library(ggalluvial)


# This function replaces original values of aircraft type with a standard form

normalize_aircraft <- function(x) {
  s <- str_to_upper(str_trim(x))
  
  s <- s %>%
    str_replace_all("[–—−]", "-") %>%   # normalize dashes
    str_replace_all("\\s+", " ") %>%
    str_replace_all("AIRBUS INDUSTRIE\\s+", "") %>%
    str_replace_all("BOEING\\s+", "") %>%
    str_replace_all("EMBRAER\\s+", "E") %>%
    str_replace_all("MCDONNELL DOUGLAS\\s+", "") %>%
    str_replace_all("AIRBUS\\s+", "") %>%
    str_replace_all("INDUSTRIE\\s+", "")
  
  out <- case_when(
    # Airbus A320 family
    str_detect(s, "^A319") ~ "A319",
    str_detect(s, "^A320") & str_detect(s, "200N|\\bN\\b") ~ "A20N",
    str_detect(s, "^A320") ~ "A320",
    str_detect(s, "^A321") & str_detect(s, "200N|\\bN\\b|LR") ~ "A21N",
    str_detect(s, "^A321") ~ "A321",
    
    # A220 (often shows as BD-500-1A10/1A11)
    str_detect(s, "^A220-?100|BD-500-1A10") ~ "A221",
    str_detect(s, "^A220-?300|BD-500-1A11") ~ "A223",
    
    # A330 (incl neo)
    str_detect(s, "^A330-?200") ~ "A332",
    str_detect(s, "^A330-?300") ~ "A333",
    str_detect(s, "^A330-?900") ~ "A339",
    
    # A350
    str_detect(s, "^A359|A350-?900") ~ "A359",
    str_detect(s, "^A35K|A350-?1000") ~ "A35K",
    
    # Boeing 717 / 737 / 757 / 767 / 747 / 777 / 787
    str_detect(s, "^717-?200") ~ "B712",
    
    # 737 variants
    str_detect(s, "^B737\\b") ~ "B737",
    str_detect(s, "737-?400") ~ "B734",
    str_detect(s, "737-?800|\\bB738\\b") ~ "B738",
    str_detect(s, "737-?900ER") ~ "B739ER",
    str_detect(s, "737-?900") ~ "B739",
    str_detect(s, "737 MAX 800") ~ "B38M",
    str_detect(s, "737 MAX 900") ~ "B39M",
    str_detect(s, "^737-300") ~ "B733",
    str_detect(s, "^737-500") ~ "B735",
    str_detect(s, "737-700") & str_detect(s, "MAX|MAX 7") ~ "B37M",
    
    # 757
    str_detect(s, "757-?200") ~ "B752",
    str_detect(s, "757-?300") ~ "B753",
    
    # 767
    str_detect(s, "767-?400") ~ "B764",
    str_detect(s, "^B767\\b") ~ "B767",
    str_detect(s, "^767\\b")  ~ "B767",
    
    # 747
    str_detect(s, "747SP") ~ "B74S",
    str_detect(s, "747-?400") ~ "B744",
    
    # 777
    str_detect(s, "777-?200|\\bB777-200\\b") ~ "B777-200",
    str_detect(s, "777-?300|\\bB777-300\\b") ~ "B777-300",
    
    # 787
    str_detect(s, "787-?800|\\bB787-800\\b") ~ "B787-800",
    str_detect(s, "787-?900|\\bB787-900\\b") ~ "B787-900",
    str_detect(s, "787-?10|\\bB787-10\\b") ~ "B787-10",
    
    # Embraer
    str_detect(s, "^E ?190|\\bE190\\b") ~ "E190",
    
    # MD-80 / MD-90 / DC-9
    str_detect(s, "DC9.*SUPER 80|MD8[1-9]|MD-?8[1-9]") ~ "MD80",
    str_detect(s, "MD-?90") ~ "MD90",
    str_detect(s, "DC-?9-?50") ~ "DC9-50",
    
    TRUE ~ NA_character_
  )
  
  out
}

# Enumerating source files into list

files <- list.files(
  path = "data",
  pattern = "^[0-9]{4}\\.csv$",
  full.names = TRUE)

# Combining all source files and reading into R as data frame

df_all <- lapply(files, read_csv) %>% 
  bind_rows()

# Reading original data mapping file of aircraft type and replacing values

aircraft_type_map <- read_csv("data/L_AIRCRAFT_TYPE.csv") %>% 
  rename(
    AIRCRAFT_TYPE = Code,
    AIRCRAFT = Description
  ) %>% 
  mutate(
    AIRCRAFT_NORMALIZED = normalize_aircraft(AIRCRAFT)
  )

