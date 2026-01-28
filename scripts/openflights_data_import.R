
# Not sure how to use this in the project yet, but looks like a good dataset

library(readr)
library(dplyr)

df_openflights_airports <- read_csv("data/airports.dat",
                                    col_names = c("airport_id",
                                                  "name",
                                                  "city",
                                                  "country",
                                                  "iata",
                                                  "icao",
                                                  "latitude",
                                                  "longitude",
                                                  "altitude",
                                                  "timezone",
                                                  "daylight_saving",
                                                  "timezone_olson",
                                                  "type",
                                                  "source"))

df_openflights_planes <- read_csv("data/planes.dat", 
                                  col_names = c("name", "iata_code", "icao_code"))

df_airlines <- read_csv("data/airlines.dat",
                        col_names = c("airline_id",
                                      "name",
                                      "alias",
                                      "iata_code",
                                      "icao_code",
                                      "call_sign",
                                      "country",
                                      "active"))


