# =============================================================================
# helpers.R — Utility functions and API fallback connectors
# =============================================================================
# This file is sourced automatically if you split the app into ui.R / server.R.
# With a single app.R file, these functions are embedded directly in app.R.
# Keep this file for reference, extension, or unit testing.
#
# HOW TO USE IN A SPLIT APP:
#   Add this line to both ui.R and server.R:
#     source("helpers.R")
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# =============================================================================
# WORLD BANK API — Historical time series
# =============================================================================
# Indicator codes (most useful for GDP analysis):
#   NY.GDP.MKTP.CD  — GDP, current USD (nominal)
#   NY.GDP.MKTP.PP.CD — GDP, PPP current international $
#   NY.GDP.PCAP.CD  — GDP per capita, current USD
#   NY.GDP.PCAP.PP.CD — GDP per capita, PPP
#   NY.GDP.MKTP.KD.ZG — GDP growth (annual %)
#
# <<MOD-WB-INDICATOR>> Change the default indicator here or pass it as argument
fetch_worldbank <- function(indicator  = "NY.GDP.MKTP.CD",
                            start_year = 2000,
                            end_year   = 2023,
                            per_page   = 5000) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/", indicator,
    "?date=", start_year, ":", end_year,
    "&format=json&per_page=", per_page
  )

  tryCatch({
    res  <- fromJSON(url, flatten = TRUE)
    data <- as_tibble(res[[2]])
    data %>%
      select(
        Country   = country.value,
        ISO3      = countryiso3code,
        Year      = date,
        Value     = value,
        Indicator = indicator.value
      ) %>%
      mutate(
        Year  = as.integer(Year),
        Value = as.numeric(Value)
      ) %>%
      filter(!is.na(Value), nchar(ISO3) == 3)  # removes regional aggregates
  }, error = function(e) {
    message("World Bank API error: ", e$message)
    NULL
  })
}

# =============================================================================
# IMF DataMapper API — Latest estimates by indicator
# =============================================================================
# Common IMF indicators:
#   NGDPD   — GDP nominal (billions USD)
#   PPPGDP  — GDP PPP (billions international $)
#   NGDPDPC — GDP per capita nominal
#   PPPPC   — GDP per capita PPP
#   NGDP_RPCH — Real GDP growth (% change)
#
# <<MOD-IMF-INDICATOR>> Change concept to switch measure
fetch_imf_latest <- function(concept = "NGDPD", base_url = "https://www.imf.org/external/datamapper/api/v1") {
  url <- paste0(base_url, "/", concept)
  tryCatch({
    res  <- fromJSON(url)
    vals <- res$values[[concept]]

    # vals is a named list: country_code -> list(year -> value)
    do.call(rbind, lapply(names(vals), function(iso) {
      country_vals <- unlist(vals[[iso]])
      data.frame(
        ISO3      = iso,
        Year      = as.integer(names(country_vals)),
        Value     = as.numeric(country_vals),
        Indicator = concept,
        stringsAsFactors = FALSE
      )
    })) %>% as_tibble()
  }, error = function(e) {
    message("IMF API error: ", e$message)
    NULL
  })
}

# =============================================================================
# ISO3 → Country Name lookup (for joining API and Wikipedia data)
# =============================================================================
# <<MOD-ISO-LOOKUP>> Extend this table if country names don't match Wikipedia
iso3_to_name <- function(iso3_vec) {
  lookup <- c(
    USA = "United States", CHN = "China", JPN = "Japan", DEU = "Germany",
    GBR = "United Kingdom", FRA = "France", IND = "India", ITA = "Italy",
    CAN = "Canada", KOR = "South Korea", RUS = "Russia", AUS = "Australia",
    BRA = "Brazil", ESP = "Spain", MEX = "Mexico", IDN = "Indonesia",
    NLD = "Netherlands", SAU = "Saudi Arabia", TUR = "Turkey", CHE = "Switzerland",
    TWN = "Taiwan", ARG = "Argentina", SWE = "Sweden", POL = "Poland",
    BEL = "Belgium", THA = "Thailand", NOR = "Norway", AUT = "Austria",
    ARE = "United Arab Emirates", NGA = "Nigeria", SGP = "Singapore",
    MYS = "Malaysia", ZAF = "South Africa", DNK = "Denmark", PHL = "Philippines",
    EGY = "Egypt", VNM = "Vietnam", BGD = "Bangladesh", FIN = "Finland",
    CHL = "Chile", IRN = "Iran", PAK = "Pakistan", COL = "Colombia",
    PRT = "Portugal", GRC = "Greece", NZL = "New Zealand", IRQ = "Iraq",
    PER = "Peru", ISR = "Israel", CZE = "Czechia", LKA = "Sri Lanka"
  )
  ifelse(iso3_vec %in% names(lookup), lookup[iso3_vec], iso3_vec)
}

# =============================================================================
# Number formatting helpers
# =============================================================================

# Auto-scale GDP value to T/B/M with dollar sign
format_gdp_auto <- function(x, is_per_capita = FALSE) {
  if (is.na(x)) return("N/A")
  if (is_per_capita) return(scales::dollar(x, accuracy = 1))
  if (x >= 1e6)     return(paste0("$", round(x / 1e6, 2), "T"))
  if (x >= 1e3)     return(paste0("$", round(x / 1e3, 1), "B"))
  return(paste0("$", round(x, 0), "M"))
}

# Return share of world total as percent string
share_of_world <- function(x, total) {
  if (is.na(x) || is.na(total) || total == 0) return("N/A")
  paste0(round(100 * x / total, 1), "%")
}

# =============================================================================
# Wikipedia table column name normalizer
# =============================================================================
# <<MOD-COL-DETECT>> Add additional patterns if Wikipedia renames columns
normalize_wiki_colnames <- function(df) {
  names(df) <- tolower(gsub("\\s+|/", "_", names(df)))
  names(df) <- gsub("\\[.*?\\]", "", names(df))
  names(df) <- trimws(names(df))
  df
}

# Detect which column is the country name column
detect_country_col <- function(df) {
  matches <- names(df)[grepl("country|nation|economy|name|territory", names(df), ignore.case = TRUE)]
  if (length(matches) > 0) return(matches[1])
  names(df)[1]  # fallback: assume first column
}

# Detect GDP value columns (excludes rank/notes)
detect_value_cols <- function(df) {
  names(df)[sapply(df, function(col) {
    num_vals <- suppressWarnings(as.numeric(gsub(",|\\[.*?\\]", "", as.character(col))))
    mean(!is.na(num_vals)) > 0.5
  })]
}
