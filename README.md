This project is programmed with R, using RStudio, Quarto and Shiny

Data used: all fields, 2015 - 2025 from DOT database, downloaded Jan 16, 2026

<https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FMG>

To import the dataset containing all original data, run `source("data/library_and_data_import.R")`. This should produce `df_all`, which has 5,225,379 observations and 50 variables. This will also import all the libraries you need for subsequent analysis.

OpenFlights data is used for a database of airport names and aircraft names:

<https://openflights.org/data.php>

(accessed Jan 20, 2026)
