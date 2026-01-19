df_china <- df_all %>% 
  filter(
    ORIGIN_COUNTRY == "CN" | 
      DEST_COUNTRY == "CN"
  )

# Mainland China only

# Filter for scheduled passenger service, and filtering out non-relevant data fields
# Filter for more than 10 passengers on the route in the particular month
# Filter for more than 1 flight performed per month for regularly scheduled service
# Filtering out airport that we know to not have regularly scheduled U.S.-China passenger service that show up in the data set, like Harbin (HRB) and Pittsburgh (PIT)

no_known_service_airports <- c("HRB", "PIT", "ANC", "MSP", "TUL", "SYR",
                               "STL", "PHL", "PDX", "OKC", "MDT", "KMG",
                               "KHN", "IAG", "HSV", "AFW", "BHM", "BNA", "CDB",
                               "CGO", "CMH", "DLC", "ELP", "GEG", "GRB", "HAK", 
                               "HET", "OKC")

df_china_passenger <- df_china %>% 
  filter(
    CLASS == "F"
  ) %>% select(
    DEPARTURES_SCHEDULED, DEPARTURES_PERFORMED, SEATS, PASSENGERS, DISTANCE,
    AIR_TIME, CARRIER, ORIGIN, DEST, AIRCRAFT_TYPE, YEAR, MONTH
  ) %>% 
  filter(PASSENGERS > 10,
         DEPARTURES_PERFORMED > 1,
         !(ORIGIN %in% no_known_service_airports),
         !(DEST %in% no_known_service_airports)) %>% 
  left_join(aircraft_type_map, by = "AIRCRAFT_TYPE") %>% 
  select(!AIRCRAFT_TYPE) %>% 
  select(!AIRCRAFT) %>% 
  rename(AIRCRAFT = AIRCRAFT_NORMALIZED)

china_airports <- c("PVG", "PEK", "CTU", "HGH", "XIY", "CAN", "WUH",
                    "NKG", "CTU", "SZX", "CSX", "TNA", "TAO", "XIY", "XMN", 
                    "CKG", "FOC", "SHE", "TFU")

us_airports <- c("SFO", "LAX", "JFK", "SEA", "DTW", "EWR", "BOS", "ORD",
                 "SJC", "IAD", "IAH", "DFW", "LAS", "ATL")

# Continental U.S. only: HNL, GUM, SPN excluded.

df_china_passenger_route_departure_performed_by_year <- df_china_passenger %>% 
  group_by(YEAR, ORIGIN, DEST, AIRCRAFT, CARRIER, MONTH) %>% 
  summarise(
    departures = sum(DEPARTURES_PERFORMED, na.rm = TRUE),
    passengers = sum(PASSENGERS, na.rm = TRUE),
    seats = sum(SEATS, na.rm = TRUE)
  ) %>% 
  mutate(
    cn_airport = case_when(
      ORIGIN %in% china_airports ~ ORIGIN,
      DEST %in% china_airports ~ DEST,
      TRUE ~ NA_character_
    ),
    us_airport = case_when(
      ORIGIN %in% us_airports ~ ORIGIN,
      DEST %in% us_airports ~ DEST,
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(cn_airport), !is.na(us_airport)) %>% 
  group_by(YEAR, cn_airport, us_airport, AIRCRAFT, CARRIER, MONTH) %>% 
  summarise(
    departures = sum(departures),
    passengers = sum(passengers),
    seats = sum(seats),
    .groups = "drop"
  ) %>% 
  mutate(
    load_factor = passengers / seats * 100,
    capacity_per_departure = seats / departures,
    policy_regime = case_when(
      YEAR <= 2019 ~ "pre_covid",
      YEAR >= 2020 & YEAR <= 2022 ~ "china_lockdown",
      YEAR >= 2023 ~ "post_covid",
      TRUE ~ NA
    ),
    chinese_carrier = case_when(
      CARRIER %in% c("MF", "CZ", "MU", "3U", "CA", "HU") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  mutate(policy_regime = fct_relevel(policy_regime, "pre_covid"),
         route = paste(cn_airport, "-", us_airport),
         date = ymd(paste(YEAR, MONTH, "01")),
         us_airport_category = case_when(
           us_airport %in% c("SFO", "LAX", "SEA", "SJC", "LAS") ~ "West Coast",
           us_airport %in% c("JFK", "EWR", "BOS", "IAD") ~ "East Coast",
           us_airport %in% c("ATL", "DFW", "IAH") ~ "The South",
           us_airport %in% c("ORD", "DTW") ~ "Midwest",
           TRUE ~ NA
         ),
         cn_airport_category = case_when(
           cn_airport %in% c("PVG", "HGH", "NKG") ~ "Eastern China",
           cn_airport %in% c("CAN", "SZX", "XMN", "FOC") ~ "Southeastern China",
           cn_airport %in% c("PEK", "SHE", "TAO", "TNA", "XIY") ~ "Northern China",
           cn_airport %in% c("CKG", "CTU", "TFU", "WUH", "CSX") ~ "Central and Western China",
           TRUE ~ NA
         ))


