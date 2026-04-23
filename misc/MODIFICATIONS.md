# 📖 MODIFICATIONS GUIDE — World GDP Wikipedia Explorer
*Last updated: 2025 | App version: 1.0*

This document is the complete reference for modifying every aspect of the app.
Search for `<<MOD>>` tags in `app.R` to jump directly to the relevant code.

---

## Table of Contents
1. [Running the App](#1-running-the-app)
2. [File Structure](#2-file-structure)
3. [Changing Wikipedia Sources](#3-changing-wikipedia-sources)
4. [Adding / Changing Year Widgets](#4-adding--changing-year-widgets)
5. [Modifying Regional Groupings](#5-modifying-regional-groupings)
6. [Changing the Theme & Colors](#6-changing-the-theme--colors)
7. [Adding a New Tab / Chart](#7-adding-a-new-tab--chart)
8. [Switching to API Data Sources](#8-switching-to-api-data-sources)
9. [Export Options](#9-export-options)
10. [Troubleshooting](#10-troubleshooting)

---

## 1. Running the App

### Prerequisites
```r
source("install_packages.R")   # run once
shiny::runApp("gdp_wiki_app/") # then launch
```

### RStudio
Open `app.R` → click **Run App** in the top-right of the editor pane.

### From the terminal
```bash
Rscript -e "shiny::runApp('gdp_wiki_app/', launch.browser=TRUE)"
```

### Deploying to shinyapps.io
```r
install.packages("rsconnect")
rsconnect::deployApp("gdp_wiki_app/")
```

---

## 2. File Structure

```
gdp_wiki_app/
├── app.R               ← Main app (UI + server in one file)
├── helpers.R           ← Utility functions and API connectors
├── install_packages.R  ← One-time package installer
├── MODIFICATIONS.md    ← This file
└── www/                ← (Optional) Static assets: CSS, images, JS
    └── custom.css      ← (Optional) Override styles here
```

**To split into ui.R + server.R** (preferred for larger apps):
1. Create `ui.R` — paste everything inside `ui <- page_navbar(...)` 
2. Create `server.R` — paste everything inside `server <- function(input, output, session) {...}`
3. Add `source("helpers.R")` at the top of both files
4. Remove the `shinyApp(ui, server)` call from `app.R` (it's auto-detected)

---

## 3. Changing Wikipedia Sources

### Tag: `<<MOD-CONFIG>>` in app.R

Wikipedia GDP pages change infrequently but may restructure tables. The relevant
block in `CONFIG$wiki_sources` looks like:

```r
imf_gdp = list(
  url         = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)",
  label       = "GDP Nominal (IMF, World Bank, UN) — USD billions",
  table_index = 1    # which <table> on the page (1-indexed)
)
```

**To add a new source:**
```r
gdp_debt = list(
  url         = "https://en.wikipedia.org/wiki/List_of_countries_by_public_debt",
  label       = "Public Debt (% of GDP)",
  table_index = 1
)
```
Then add `"gdp_debt"` to the `checkboxGroupInput("cmp_measures", ...)` choices.

**If data stops loading** — the most common cause is Wikipedia adding a new
table above the one you need. Increment `table_index` until data appears.
Confirm by running in console:
```r
library(rvest); library(httr)
page   <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)")
tables <- html_table(page, fill = TRUE)
length(tables)        # how many tables total
head(tables[[1]])     # inspect first table
```

---

## 4. Adding / Changing Year Widgets

### Tag: `<<MOD-YEARS>>` in app.R

The Snapshot tab currently shows the **latest available** data from Wikipedia
(Wikipedia tables are not historical — they reflect the current year's estimates).

**To add a year slider for historical data** (requires World Bank API):

**Step 1** — Add slider to sidebar:
```r
sliderInput("year_range", "Year Range",
  min   = 1990,
  max   = 2024,
  value = c(2010, 2023),
  sep   = "",         # removes comma from 2,010
  step  = 1
)
```

**Step 2** — Add reactive for API data:
```r
wb_data <- reactive({
  req(input$year_range)
  fetch_worldbank(
    indicator  = "NY.GDP.MKTP.CD",
    start_year = input$year_range[1],
    end_year   = input$year_range[2]
  )
})
```

**Step 3** — Use `wb_data()` in a `renderPlotly()` with Year on the x-axis
and animate by adding `frame = ~Year` in the `aes()`:
```r
ggplot(wb_data(), aes(x = Year, y = Value, color = Country, frame = Year)) +
  geom_line()
```

---

## 5. Modifying Regional Groupings

### Tag: `<<MOD-REGIONS>>` and `<<MOD-REGION-MAP>>` in app.R

**Region display names** (shown in dropdown):
```r
regions = list(
  "All Regions"          = "all",
  "East Asia & Pacific"  = "EAP",
  "Sub-Saharan Africa"   = "SSA"   # <<< add new region
)
```

**Country → region assignments:**
```r
region_map = c(
  # Existing ...
  "Kenya"    = "SSA",
  "Tanzania" = "SSA",
  "Uganda"   = "SSA"
)
```

**Important:** Country names must match **exactly** what Wikipedia uses.
Run `unique(snap_data()$Country)` in the R console after first data load to verify.

---

## 6. Changing the Theme & Colors

### Tag: `<<MOD-THEME>>` and `<<MOD-COLORS>>` in app.R

**Bootstrap theme:**
```r
bs_theme(
  version    = 5,
  bootswatch = "flatly",   # light; options: flatly, lumen, cosmo, sandstone, yeti
  primary    = "#005f73",
  font_scale = 1.0
)
```
Full list of themes: https://bootswatch.com/

**Chart color palette** (one color per region):
```r
palette = c(
  EAP   = "#0077b6",
  EUR   = "#2dc653",
  LAC   = "#f77f00",
  MEA   = "#e63946",
  NAM   = "#7b2d8b",
  SAS   = "#f4a261",
  Other = "#adb5bd"
)
```
Use any valid hex code. Ensure sufficient contrast for accessibility (WCAG AA: 4.5:1).

**Custom CSS** — add a `www/custom.css` file and reference it:
```r
# In UI header:
tags$link(rel = "stylesheet", href = "custom.css")
```

---

## 7. Adding a New Tab / Chart

**Step 1** — Add a `nav_panel()` in `page_navbar()`:
```r
nav_panel(
  title = "📉 Debt vs GDP",
  layout_sidebar(
    sidebar = sidebar( selectInput("debt_measure", ...) ),
    card( plotlyOutput("debt_plot") )
  )
)
```

**Step 2** — Add a reactive data source in `server`:
```r
debt_data <- reactive({
  fetch_wiki_table(
    "https://en.wikipedia.org/wiki/List_of_countries_by_public_debt", 1
  ) %>% parse_gdp_table("gdp_debt")
})
```

**Step 3** — Add render function:
```r
output$debt_plot <- renderPlotly({
  df <- debt_data(); req(df)
  p  <- ggplot(df, aes(x = reorder(Country, value), y = value)) +
    geom_col() + coord_flip()
  ggplotly(p)
})
```

---

## 8. Switching to API Data Sources

### World Bank API (historical, reliable)
See `helpers.R → fetch_worldbank()`.

Key indicators:
| Code | Measure |
|------|---------|
| `NY.GDP.MKTP.CD` | GDP nominal (USD) |
| `NY.GDP.MKTP.PP.CD` | GDP PPP |
| `NY.GDP.PCAP.CD` | GDP per capita |
| `NY.GDP.MKTP.KD.ZG` | GDP growth (%) |

### IMF DataMapper API (latest estimates, forecasts)
See `helpers.R → fetch_imf_latest()`.

Key concepts: `NGDPD`, `PPPGDP`, `NGDPDPC`, `NGDP_RPCH`

### Switching in server.R
Replace `cached_fetch(...)` calls with:
```r
snap_data <- reactive({
  fetch_worldbank("NY.GDP.MKTP.CD", input$year_range[1], input$year_range[2])
})
```

---

## 9. Export Options

### CSV (default)
Each tab has a `downloadButton` + `downloadHandler`. CSV is written with:
```r
write.csv(snap_data(), file, row.names = FALSE)
```

### Excel
Install `writexl`, then:
```r
writexl::write_xlsx(snap_data(), file)
# Change filename extension to ".xlsx"
```

### JSON
```r
jsonlite::write_json(snap_data(), file, pretty = TRUE)
```

### PNG chart export
Add a `downloadButton` and:
```r
output$download_plot <- downloadHandler(
  filename = "chart.png",
  content  = function(file) {
    p <- ggplot(...) + ...
    ggsave(file, plot = p, width = 10, height = 6, dpi = 150)
  }
)
```

---

## 10. Troubleshooting

| Problem | Likely Cause | Fix |
|---------|-------------|-----|
| "Failed to fetch data" | Network / Wikipedia down | Try again; check internet |
| Data loads but looks wrong | `table_index` is off | Increment `table_index` in CONFIG |
| Country not assigned to region | Missing from `region_map` | Add to `CONFIG$region_map` |
| Bubble chart shows empty | Join failed (name mismatch) | Check `unique(snap_data()$Country)` |
| App crashes on startup | Missing package | Re-run `install_packages.R` |
| Slow data loading | Wikipedia latency | Enable `CONFIG$use_session_cache = TRUE` |
| `bsicons` not found | Package not installed | `install.packages("bsicons")` |

**Debug mode** — add to server to print reactive data to console:
```r
observe({ print(snap_data()) })
```

---

*Questions? Check the `# <<MOD>>` inline comments in `app.R` for the exact
line of each modification point.*
