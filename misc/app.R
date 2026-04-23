# =============================================================================
# WORLD GDP WIKIPEDIA EXPLORER — Shiny App
# =============================================================================
# FILE: app.R
# PURPOSE: Single-file Shiny app that extracts and visualizes world GDP data
#          from Wikipedia using rvest. Supports multi-year comparison,
#          regional filtering, per-capita analysis, and CSV export.
#
# HOW TO RUN:
#   1. Install required packages (see MODIFICATIONS.md or run install_packages.R)
#   2. Open this file in RStudio and click "Run App", OR run from console:
#         shiny::runApp("path/to/gdp_wiki_app/")
#
# MODIFYING THIS APP:
#   - See MODIFICATIONS.md for a full annotated guide to every parameter
#   - Key modification points are marked with: # <<MOD>> comments below
#   - Wikipedia source URLs are in the CONFIG section near line 50
#   - UI theme/colors are in the CSS block inside ui()
#   - Chart defaults are in the CHART DEFAULTS section near line 220
# =============================================================================

# --- PACKAGE LOADING ---------------------------------------------------------
# <<MOD-PKG>> Add or remove packages here. All must be installed first.
library(shiny)
library(bslib)       # Bootstrap 5 theming
library(rvest)       # Web scraping from Wikipedia
library(dplyr)       # Data manipulation
library(tidyr)       # Reshaping data
library(ggplot2)     # Static plots
library(plotly)      # Interactive plots (wraps ggplot2)
library(DT)          # Interactive data tables
library(scales)      # Number formatting (trillions, billions)
library(stringr)     # String cleaning
library(httr)        # HTTP requests with user-agent control
library(jsonlite)    # JSON parsing for World Bank API fallback

# =============================================================================
# CONFIG — Edit this block to change data sources or app-level settings
# =============================================================================
# <<MOD-CONFIG>>

CONFIG <- list(

  # Wikipedia pages scraped by this app. Table indices (table_index) may shift
  # if Wikipedia editors add/remove tables — adjust if data stops loading.
  wiki_sources = list(
    imf_gdp = list(
      url         = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)",
      label       = "GDP Nominal (IMF, World Bank, UN) — USD billions",
      table_index = 1    # <<MOD>> Change if Wikipedia layout changes
    ),
    gdp_ppp = list(
      url         = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)",
      label       = "GDP by PPP (IMF, World Bank, CIA) — Int'l $ billions",
      table_index = 1
    ),
    gdp_per_capita = list(
      url         = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita",
      label       = "GDP per Capita Nominal (IMF, World Bank, UN) — USD",
      table_index = 1
    ),
    gdp_per_capita_ppp = list(
      url         = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita",
      label       = "GDP per Capita PPP (IMF, World Bank, CIA) — Int'l $",
      table_index = 1
    ),
    gdp_growth = list(
      url         = "https://en.wikipedia.org/wiki/List_of_countries_by_real_GDP_growth_rate",
      label       = "Real GDP Growth Rate (%) — IMF",
      table_index = 1
    )
  ),

  # IMF World Economic Outlook API — used as fallback / historical time series
  # <<MOD-API>> Change this base URL if IMF updates their API endpoint
  imf_api_base = "https://www.imf.org/external/datamapper/api/v1",

  # <<MOD-YEARS>> Default year range shown in sliders
  year_range_default = c(2010, 2023),
  year_min           = 1990,
  year_max           = 2024,

  # <<MOD-TOP-N>> Default number of countries in bar/bubble charts
  top_n_default = 20,

  # <<MOD-REGIONS>> Regional groupings — add/rename/reorder as needed
  regions = list(
    "All Regions"         = "all",
    "East Asia & Pacific" = "EAP",
    "Europe"              = "EUR",
    "Latin America"       = "LAC",
    "Middle East & Africa"= "MEA",
    "North America"       = "NAM",
    "South Asia"          = "SAS"
  ),

  # IMF DataMapper concept codes — used when snap_year < year_max (historical fetch)
  imf_concepts = list(
    imf_gdp            = "NGDPD",
    gdp_ppp            = "PPPGDP",
    gdp_per_capita     = "NGDPDPC",
    gdp_per_capita_ppp = "PPPPC",
    gdp_growth         = "NGDP_RPCH"
  ),

  # Countries assigned to each region code (used for filter)
  # <<MOD-REGION-MAP>> Add countries or reorganize groupings here
  region_map = c(
    "China" = "EAP", "Japan" = "EAP", "South Korea" = "EAP",
    "Australia" = "EAP", "Indonesia" = "EAP", "Taiwan" = "EAP",
    "Thailand" = "EAP", "Vietnam" = "EAP", "Philippines" = "EAP",
    "Malaysia" = "EAP", "Singapore" = "EAP", "New Zealand" = "EAP",
    "Germany" = "EUR", "France" = "EUR", "Italy" = "EUR",
    "United Kingdom" = "EUR", "Spain" = "EUR", "Russia" = "EUR",
    "Netherlands" = "EUR", "Switzerland" = "EUR", "Sweden" = "EUR",
    "Poland" = "EUR", "Belgium" = "EUR", "Norway" = "EUR",
    "Austria" = "EUR", "Denmark" = "EUR", "Finland" = "EUR",
    "Turkey" = "EUR", "Greece" = "EUR", "Portugal" = "EUR",
    "Brazil" = "LAC", "Mexico" = "LAC", "Argentina" = "LAC",
    "Colombia" = "LAC", "Chile" = "LAC", "Peru" = "LAC",
    "Venezuela" = "LAC", "Ecuador" = "LAC",
    "Saudi Arabia" = "MEA", "United Arab Emirates" = "MEA",
    "South Africa" = "MEA", "Nigeria" = "MEA", "Egypt" = "MEA",
    "Israel" = "MEA", "Iran" = "MEA", "Iraq" = "MEA",
    "United States" = "NAM", "Canada" = "NAM",
    "India" = "SAS", "Pakistan" = "SAS", "Bangladesh" = "SAS",
    "Sri Lanka" = "SAS"
  ),

  # <<MOD-COLORS>> Palette used in charts (one per region + default)
  palette = c(
    EAP = "#0077b6", EUR = "#2dc653", LAC = "#f77f00",
    MEA = "#e63946", NAM = "#7b2d8b", SAS = "#f4a261",
    Other = "#adb5bd"
  ),

  # <<MOD-CACHE>> Set to TRUE to cache Wikipedia fetches within a session
  use_session_cache = TRUE,

  # HTTP user-agent sent to Wikipedia (good practice)
  user_agent = "WikiGDP-ShinyApp/1.0 (Educational; contact: instructor@university.edu)"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

iso3_to_name <- function(iso3_vec) {
  lookup <- c(
    USA="United States", CHN="China", JPN="Japan", DEU="Germany",
    GBR="United Kingdom", FRA="France", IND="India", ITA="Italy",
    CAN="Canada", KOR="South Korea", RUS="Russia", AUS="Australia",
    BRA="Brazil", ESP="Spain", MEX="Mexico", IDN="Indonesia",
    NLD="Netherlands", SAU="Saudi Arabia", TUR="Turkey", CHE="Switzerland",
    TWN="Taiwan", ARG="Argentina", SWE="Sweden", POL="Poland",
    BEL="Belgium", THA="Thailand", NOR="Norway", AUT="Austria",
    ARE="United Arab Emirates", NGA="Nigeria", SGP="Singapore",
    MYS="Malaysia", ZAF="South Africa", DNK="Denmark", PHL="Philippines",
    EGY="Egypt", VNM="Vietnam", BGD="Bangladesh", FIN="Finland",
    CHL="Chile", IRN="Iran", PAK="Pakistan", COL="Colombia",
    PRT="Portugal", GRC="Greece", NZL="New Zealand", IRQ="Iraq",
    PER="Peru", ISR="Israel", CZE="Czechia", LKA="Sri Lanka"
  )
  ifelse(iso3_vec %in% names(lookup), lookup[iso3_vec], iso3_vec)
}

fetch_imf_latest <- function(concept, base_url = CONFIG$imf_api_base) {
  url <- paste0(base_url, "/", concept)
  tryCatch({
    res  <- fromJSON(url)
    vals <- res$values[[concept]]
    do.call(rbind, lapply(names(vals), function(iso) {
      country_vals <- unlist(vals[[iso]])
      data.frame(ISO3=iso, Year=as.integer(names(country_vals)),
                 Value=as.numeric(country_vals), stringsAsFactors=FALSE)
    })) %>% as_tibble()
  }, error = function(e) { message("IMF API error: ", e$message); NULL })
}

# Safe Wikipedia table fetch with retry and caching
fetch_wiki_table <- function(url, table_index = 1, ua = CONFIG$user_agent) {
  tryCatch({
    page <- GET(url, user_agent(ua), timeout(15))
    stop_for_status(page)
    html  <- read_html(content(page, as = "text", encoding = "UTF-8"))
    tables <- html_table(html, fill = TRUE)
    if (length(tables) < table_index) {
      stop(paste("Expected table", table_index, "but only", length(tables), "found."))
    }
    tables[[table_index]]
  }, error = function(e) {
    message("Wikipedia fetch failed: ", e$message)
    NULL
  })
}

# Clean a GDP numeric column: remove commas, footnotes, dashes, convert to numeric
clean_gdp_col <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, "\\[.*?\\]")   # remove [note] citations
  x <- str_remove_all(x, ",")           # remove thousand separators
  x <- str_replace_all(x, "—|–|-|N/A|n/a", NA_character_)
  x <- str_trim(x)
  suppressWarnings(as.numeric(x))
}

# Assign region label to a country vector
assign_region <- function(countries) {
  r <- CONFIG$region_map[countries]
  r[is.na(r)] <- "Other"
  unname(r)
}

# Format GDP value for display (trillions / billions auto-scaling)
format_gdp <- function(x, is_per_capita = FALSE) {
  if (is_per_capita) return(dollar(x, accuracy = 1))
  if (is.na(x))     return("N/A")
  if (x >= 1e6)     return(paste0("$", round(x / 1e6, 2), "T"))
  if (x >= 1e3)     return(paste0("$", round(x / 1e3, 1), "B"))
  return(paste0("$", x, "M"))
}

# Parse and standardize the raw Wikipedia GDP table
parse_gdp_table <- function(raw_df, source_key) {
  if (is.null(raw_df)) return(NULL)

  df <- raw_df

  # Standardize column names (Wikipedia tables vary by source)
  names(df) <- tolower(str_replace_all(names(df), "\\s+|/", "_"))
  names(df) <- str_remove_all(names(df), "\\[.*?\\]")

  # Try to find country and value columns heuristically
  country_col <- names(df)[str_detect(names(df), "country|nation|economy")][1]
  value_cols  <- names(df)[str_detect(names(df), "imf|world_bank|cia|estimate|forecast|gdp|value|2")]

  if (is.na(country_col) || length(value_cols) == 0) {
    # Fallback: first col = country, subsequent numeric cols = values
    country_col <- names(df)[1]
    value_cols  <- names(df)[-1]
  }

  # Build tidy data frame
  out <- df %>%
    select(all_of(c(country_col, value_cols))) %>%
    rename(Country = 1)

  # Pivot if multiple value columns (IMF / World Bank / UN side by side)
  if (length(value_cols) > 1) {
    out <- out %>%
      pivot_longer(-Country, names_to = "source_org", values_to = "value_raw") %>%
      mutate(
        value     = clean_gdp_col(value_raw),
        source_org = str_replace(source_org, "_", " ") %>% str_to_title()
      ) %>%
      filter(!is.na(value), Country != "", !str_detect(Country, "World|Total|Region"))
  } else {
    out <- out %>%
      rename(value_raw = 2) %>%
      mutate(
        value      = clean_gdp_col(value_raw),
        source_org = "IMF"
      ) %>%
      filter(!is.na(value), Country != "", !str_detect(Country, "World|Total|Region"))
  }

  out %>%
    mutate(
      Country     = str_remove_all(Country, "\\[.*?\\]") %>% str_trim(),
      Region      = assign_region(Country),
      source_key  = source_key
    ) %>%
    select(Country, Region, source_org, value, source_key)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

icon_text <- function(emoji, text) tags$span(emoji, " ", text)

mod_section <- function(title, description, code_example = NULL) {
  tagList(
    tags$div(
      style = "background:#111827; border:1px solid #1e2d3d; border-radius:10px; padding:20px; margin-bottom:18px;",
      tags$h4(title, style = "color:#0077b6; margin-top:0;"),
      tags$p(description, style = "color:#a8b8cc; line-height:1.65;"),
      if (!is.null(code_example))
        tags$pre(style = "background:#0d1b2a; border:1px solid #1e2d3d; border-radius:6px; padding:14px; color:#7dd3fc; font-size:.78rem; overflow-x:auto;",
                 code_example)
    )
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/Wikipedia-logo-v2.svg/40px-Wikipedia-logo-v2.svg.png",
             height = "28px", style = "margin-right:8px; vertical-align:middle;"),
    "World GDP Explorer"
  ),
  theme = bs_theme(
    version    = 5,
    bootswatch = "darkly",           # <<MOD-THEME>> Options: darkly, flatly, lumen, minty, sandstone, etc.
    primary    = "#0077b6",
    secondary  = "#023e8a",
    success    = "#2dc653",
    font_scale = 0.95
  ),
  navbar_options = navbar_options(bg = "#0a0e1a", fg = "#e8eaf0"),
  fillable  = TRUE,

  # ---- CUSTOM CSS -----------------------------------------------------------
  header = tags$head(
    tags$style(HTML("
      /* <<MOD-CSS>> Adjust colors, fonts, spacing below */
      body { background: #0a0e1a; font-family: 'IBM Plex Sans', sans-serif; }
      .value-box-card { border-radius: 12px !important; }
      .card { background: #111827 !important; border: 1px solid #1e2d3d !important; border-radius: 12px !important; }
      .card-header { background: #0d1b2a !important; border-bottom: 1px solid #1e2d3d !important; font-weight: 600; letter-spacing: .04em; }
      .sidebar { background: #0d1b2a !important; border-right: 1px solid #1e2d3d !important; }
      .sidebar .form-label { font-size: .78rem; font-weight: 600; text-transform: uppercase; letter-spacing: .08em; color: #7aa7c7; }
      .btn-primary { background: #0077b6 !important; border: none !important; border-radius: 8px !important; }
      .btn-success { border-radius: 8px !important; }
      .dataTables_wrapper { font-size: .82rem; }
      .shiny-notification { background: #111827; border-left: 4px solid #0077b6; color: #e8eaf0; }
      #status_banner { font-size:.78rem; padding:6px 14px; border-radius:6px; margin-top:4px; }
      .source-badge { display:inline-block; padding:2px 8px; border-radius:10px; font-size:.7rem;
                      font-weight:700; background:#1e2d3d; color:#7aa7c7; margin-left:6px; }
      .mod-hint { font-size:.72rem; color:#4a6780; margin-top:2px; }
    ")),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;600;700&display=swap", rel = "stylesheet")
  ),

  # ===========================================================================
  # TAB 1 — SNAPSHOT (current rankings)
  # ===========================================================================
  nav_panel(
    title = icon_text("📊", "Snapshot"),
    layout_sidebar(
      fillable = TRUE,
      sidebar  = sidebar(
        width = 280,
        # <<MOD-WIDGET-SOURCE>> Change default or add/remove choices
        selectInput("snap_source", "GDP Measure",
          choices  = setNames(names(CONFIG$wiki_sources),
                              sapply(CONFIG$wiki_sources, `[[`, "label")),
          selected = "imf_gdp"
        ),
        tags$div(class="mod-hint", "Latest year → Wikipedia. Past years → IMF API."),
        hr(),
        sliderInput("snap_year", "Data Year",
          min = CONFIG$year_min, max = CONFIG$year_max,
          value = CONFIG$year_max, step = 1, sep = ""
        ),
        selectInput("snap_source_org", "Reporting Organization",
          choices  = c("All", "IMF", "World Bank", "UN", "CIA"),
          selected = "IMF"
        ),
        # <<MOD-WIDGET-REGION>> Supports multiple selections; "All Regions" clears filter
        selectizeInput("snap_region", "Region Filter",
          choices  = CONFIG$regions,
          selected = "all",
          multiple = TRUE,
          options  = list(placeholder = "All regions")
        ),
        # <<MOD-WIDGET-COUNTRIES>> Populated after first fetch
        selectizeInput("snap_countries", "Country Filter",
          choices  = NULL,
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "All countries (no filter)", maxItems = 100)
        ),
        # <<MOD-WIDGET-TOPN>> Default and range
        sliderInput("snap_top_n", "Top N Countries (bar chart)",
          min = 5, max = 50, value = CONFIG$top_n_default, step = 5
        ),
        checkboxInput("snap_log_scale", "Log scale (Y axis)", FALSE),
        hr(),
        actionButton("snap_fetch", "🔄 Fetch / Refresh Data",
                     class = "btn-primary w-100"),
        tags$div(id = "status_banner", ""),
        br(),
        downloadButton("snap_download", "💾 Download CSV", class = "btn-success w-100")
      ),

      # Main panel
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Countries Loaded", value = textOutput("vb_n_countries"),
                  showcase = icon("globe"), theme = "primary"),
        value_box(title = "Largest Economy", value = textOutput("vb_top_country"),
                  showcase = icon("trophy"), theme = "success"),
        value_box(title = "Top GDP Value",  value = textOutput("vb_top_gdp"),
                  showcase = icon("dollar-sign"), theme = "info"),
        value_box(title = "Data Source",    value = textOutput("vb_source_label"),
                  showcase = icon("book"), theme = "secondary")
      ),
      br(),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("🏆 GDP Rankings — Bar Chart"),
          plotlyOutput("snap_bar", height = "420px")
        ),
        card(
          card_header("🌍 Regional Share — Pie Chart"),
          plotlyOutput("snap_pie", height = "420px")
        )
      ),
      br(),
      card(
        card_header("📋 Full Rankings Table",
                    tags$span(class = "source-badge", "Wikipedia")),
        DTOutput("snap_table")
      )
    )
  ),

  # ===========================================================================
  # TAB 2 — COMPARE (multi-country / multi-measure)
  # ===========================================================================
  nav_panel(
    title = icon_text("⚖️", "Compare"),
    layout_sidebar(
      fillable = TRUE,
      sidebar  = sidebar(
        width = 290,
        p(class = "mod-hint", "Compare up to 4 GDP measures side-by-side for selected countries."),
        hr(),
        # <<MOD-WIDGET-COUNTRIES>> Pre-selected countries
        selectizeInput("cmp_countries", "Select Countries",
          choices  = NULL,  # populated after data loads
          selected = NULL,
          multiple = TRUE,
          options  = list(placeholder = "Type to search…", maxItems = 15)
        ),
        checkboxGroupInput("cmp_measures", "GDP Measures to Compare",
          choices  = setNames(names(CONFIG$wiki_sources),
                              sapply(CONFIG$wiki_sources, `[[`, "label")),
          selected = c("imf_gdp", "gdp_ppp")
        ),
        checkboxInput("cmp_log_scale", "Log scale (Y axis)", FALSE),
        hr(),
        actionButton("cmp_fetch", "🔄 Load Comparison", class = "btn-primary w-100"),
        br(), br(),
        downloadButton("cmp_download", "💾 Download CSV", class = "btn-success w-100")
      ),
      card(
        card_header("📊 Side-by-Side Comparison"),
        plotlyOutput("cmp_bar", height = "480px")
      ),
      br(),
      card(
        card_header("📋 Comparison Table"),
        DTOutput("cmp_table")
      )
    )
  ),

  # ===========================================================================
  # TAB 3 — BUBBLE CHART (GDP vs GDP per capita)
  # ===========================================================================
  nav_panel(
    title = icon_text("🫧", "Bubble"),
    layout_sidebar(
      fillable = TRUE,
      sidebar  = sidebar(
        width = 270,
        p(class = "mod-hint", "X = GDP Nominal, Y = GDP per Capita, Size = GDP PPP"),
        hr(),
        sliderInput("bub_top_n", "Countries to Display",
          min = 10, max = 80, value = 40, step = 5
        ),
        selectizeInput("bub_region", "Region Filter",
          choices = CONFIG$regions, selected = "all",
          multiple = TRUE, options = list(placeholder = "All regions")
        ),
        checkboxInput("bub_label", "Show Country Labels", TRUE),
        checkboxInput("bub_log_x", "Log X axis", TRUE),
        checkboxInput("bub_log_y", "Log Y axis", FALSE),
        hr(),
        actionButton("bub_fetch", "🔄 Load Bubble Data", class = "btn-primary w-100")
      ),
      card(
        card_header("🫧 GDP Nominal vs. GDP per Capita (bubble = PPP size)"),
        plotlyOutput("bubble_plot", height = "580px")
      )
    )
  ),

  # ===========================================================================
  # TAB 4 — GROWTH RATES
  # ===========================================================================
  nav_panel(
    title = icon_text("📈", "Growth"),
    layout_sidebar(
      fillable = TRUE,
      sidebar  = sidebar(
        width = 270,
        p(class = "mod-hint", "Real GDP growth rates scraped from Wikipedia (latest year available)."),
        hr(),
        sliderInput("grw_top_n", "Top N Fastest-Growing",
          min = 5, max = 30, value = 15, step = 5
        ),
        selectizeInput("grw_region", "Region Filter",
          choices = CONFIG$regions, selected = "all",
          multiple = TRUE, options = list(placeholder = "All regions")
        ),
        checkboxInput("grw_bottom", "Show Slowest-Growing Instead", FALSE),
        checkboxInput("grw_log_scale", "Log scale (Y axis)", FALSE),
        hr(),
        actionButton("grw_fetch", "🔄 Load Growth Data", class = "btn-primary w-100"),
        br(), br(),
        downloadButton("grw_download", "💾 Download CSV", class = "btn-success w-100")
      ),
      card(
        card_header("📈 Real GDP Growth Rates (%)"),
        plotlyOutput("grw_plot", height = "500px")
      ),
      br(),
      card(
        card_header("📋 Growth Rate Table"),
        DTOutput("grw_table")
      )
    )
  ),

  # ===========================================================================
  # TAB 5 — DOCUMENTATION (how to modify)
  # ===========================================================================
  nav_panel(
    title = icon_text("📖", "How to Modify"),
    fillable = FALSE,
    tags$div(
      style = "max-width:860px; margin:0 auto; padding:30px 20px;",
      tags$h2("📖 App Modification Guide", style="color:#7aa7c7; border-bottom:1px solid #1e2d3d; padding-bottom:12px;"),
      tags$p("This tab documents every major modification point in the app. Look for ", tags$code("# <<MOD>>"), " comments in ", tags$code("app.R"), " to jump directly to that section."),

      mod_section("🔗 Changing Wikipedia Sources",
        "In the CONFIG block (near line 50), edit wiki_sources. Each entry needs:
        url (the Wikipedia page), label (shown in dropdowns), and table_index
        (which HTML table on the page to scrape — use Inspect Element to count tables).
        If Wikipedia restructures a page, increment table_index until data loads correctly.",
        'wiki_sources = list(
  my_new_source = list(
    url         = "https://en.wikipedia.org/wiki/...",
    label       = "My Custom Measure — Units",
    table_index = 2   # second table on that page
  )
)'
      ),

      mod_section("📅 Year Range Widget",
        "The Snapshot tab currently shows the latest-available data. To add a
        multi-year historical slider, set year_range_default, year_min, and year_max
        in CONFIG, then wire a sliderInput() to filter IMF API data returned by
        fetch_imf_historical() (skeleton provided in helpers.R).",
        'sliderInput("year_range", "Year Range",
  min   = CONFIG$year_min,
  max   = CONFIG$year_max,
  value = CONFIG$year_range_default,
  sep   = ""   # removes comma in year display
)'
      ),

      mod_section("🌍 Changing Regional Groupings",
        "Edit CONFIG$regions (display names → codes) and CONFIG$region_map
        (country → region code). Both must stay in sync. Add any country name
        exactly as it appears in the Wikipedia table (check the raw table with
        str(snap_data()) in the R console after first load).",
        'regions = list(
  "Sub-Saharan Africa" = "SSA"
),
region_map = c(
  "Kenya" = "SSA", "Ghana" = "SSA", "Ethiopia" = "SSA"
)'
      ),

      mod_section("🎨 Changing the Visual Theme",
        "The app uses bslib (Bootstrap 5). Change bootswatch in bs_theme() to
        any Bootswatch theme name: flatly, lumen, minty, sandstone, cosmo, yeti.
        Or set preset = NULL and fully customize primary/secondary/success colors.
        Fonts: add any Google Font link in the header and reference in CSS.",
        'theme = bs_theme(
  version    = 5,
  bootswatch = "flatly",  # light theme
  primary    = "#005f73",
  font_scale = 1.0
)'
      ),

      mod_section("📊 Adding a New Chart Type",
        "Add a new nav_panel() in the navbar. Inside, add a plotlyOutput() in the
        UI and a renderPlotly({}) in the server. Use snap_data() as your reactive
        data source — it returns a clean data frame with columns:
        Country, Region, source_org, value, source_key.",
        '# In UI:
plotlyOutput("my_new_chart", height = "400px")

# In server:
output$my_new_chart <- renderPlotly({
  df <- snap_data()
  req(df)
  p <- ggplot(df, aes(x = reorder(Country, value), y = value, fill = Region)) +
    geom_col() + coord_flip()
  ggplotly(p)
})'
      ),

      mod_section("💾 Export & Download",
        "Every tab has a downloadButton() wired to a downloadHandler(). The
        filename and content functions are in server.R logic. To add a new
        download, copy an existing downloadHandler block and change the
        reactive data source. To add Excel export, install the writexl package
        and change write.csv() to writexl::write_xlsx().",
        'output$my_download <- downloadHandler(
  filename = function() paste0("gdp_export_", Sys.Date(), ".csv"),
  content  = function(file) write.csv(my_reactive_data(), file, row.names = FALSE)
)'
      ),

      mod_section("🔌 Switching to the IMF or World Bank API",
        "helpers.R (included in this package) contains fetch_imf_historical()
        and fetch_worldbank() skeleton functions. These hit the IMF DataMapper
        and World Bank REST APIs respectively. To use them, call them inside a
        reactive() in server.R and replace wiki_data() references.",
        '# helpers.R — fetch_worldbank()
fetch_worldbank <- function(indicator = "NY.GDP.MKTP.CD",
                            start_year = 2000, end_year = 2023) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/", indicator,
    "?date=", start_year, ":", end_year,
    "&format=json&per_page=5000"
  )
  res <- fromJSON(url)
  as_tibble(res[[2]])
}'
      ),

      tags$hr(),
      tags$p(style="color:#4a6780; font-size:.8rem;",
        "App built with R Shiny + bslib + rvest. Data sourced from Wikipedia (live scrape). ",
        "For reproducibility, consider caching snapshots with pins or arrow.")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---- Session-level cache (avoids re-fetching same page twice) -------------
  wiki_cache <- reactiveVal(list())

  cached_fetch <- function(key, url, table_index) {
    cache <- wiki_cache()
    if (CONFIG$use_session_cache && !is.null(cache[[key]])) {
      return(cache[[key]])
    }
    raw  <- fetch_wiki_table(url, table_index)
    data <- parse_gdp_table(raw, key)
    if (!is.null(data)) {
      cache[[key]] <- data
      wiki_cache(cache)
    }
    data
  }

  # ---- SNAPSHOT reactive data -----------------------------------------------
  # snap_raw stores the fetched, org-filtered data; snap_data applies region/country filters
  snap_raw <- reactiveVal(NULL)

  snap_data <- reactive({
    df <- snap_raw(); req(df)
    regions   <- input$snap_region
    countries <- input$snap_countries
    if (!("all" %in% regions) && length(regions) > 0)
      df <- df %>% filter(Region %in% regions)
    if (length(countries) > 0)
      df <- df %>% filter(Country %in% countries)
    df
  })

  observeEvent(input$snap_fetch, {
    yr <- input$snap_year

    if (yr < CONFIG$year_max) {
      # Historical fetch via IMF API
      concept <- CONFIG$imf_concepts[[input$snap_source]]
      if (is.null(concept)) {
        showNotification("⚠️ Historical data not available for this measure.", type = "warning")
        return()
      }
      showNotification(paste0("⏳ Fetching IMF data for ", yr, "…"), id = "fetching", duration = NULL)
      imf_raw <- fetch_imf_latest(concept)
      removeNotification("fetching")
      if (is.null(imf_raw)) {
        showNotification("❌ IMF API fetch failed.", type = "error"); return()
      }
      df <- imf_raw %>%
        filter(Year == yr) %>%
        mutate(
          Country    = iso3_to_name(ISO3),
          Region     = assign_region(Country),
          source_org = "IMF",
          source_key = input$snap_source,
          value      = Value
        ) %>%
        filter(!is.na(value)) %>%
        select(Country, Region, source_org, value, source_key)
    } else {
      # Latest data via Wikipedia
      showNotification("⏳ Fetching Wikipedia data…", id = "fetching", duration = NULL)
      src <- CONFIG$wiki_sources[[input$snap_source]]
      df  <- cached_fetch(input$snap_source, src$url, src$table_index)
      removeNotification("fetching")
      if (is.null(df)) {
        showNotification("❌ Failed to fetch data. Check your internet connection.", type = "error")
        return()
      }
      if (input$snap_source_org != "All")
        df <- df %>% filter(str_detect(source_org, fixed(input$snap_source_org, ignore_case = TRUE)))
    }

    snap_raw(df)
    showNotification(paste0("✅ Data loaded (", yr, ")!"), type = "message", duration = 3)

    all_countries <- sort(unique(df$Country))
    updateSelectizeInput(session, "snap_countries",
      choices = all_countries, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "cmp_countries",
      choices  = all_countries,
      selected = c("United States", "China", "Germany", "India", "Japan"),
      server   = TRUE)
  })

  # ---- Value boxes ----------------------------------------------------------
  output$vb_n_countries <- renderText({
    df <- snap_data(); req(df); nrow(df) %>% format(big.mark = ",")
  })
  output$vb_top_country <- renderText({
    df <- snap_data(); req(df)
    df %>% arrange(desc(value)) %>% slice(1) %>% pull(Country)
  })
  output$vb_top_gdp <- renderText({
    df <- snap_data(); req(df)
    val <- df %>% arrange(desc(value)) %>% slice(1) %>% pull(value)
    format_gdp(val, is_per_capita = str_detect(input$snap_source, "per_capita"))
  })
  output$vb_source_label <- renderText({
    CONFIG$wiki_sources[[input$snap_source]]$label %>%
      str_extract("^[^—]+") %>% str_trim()
  })

  # ---- Snapshot bar chart ---------------------------------------------------
  output$snap_bar <- renderPlotly({
    df <- snap_data(); req(df)
    top_df <- df %>%
      group_by(Country, Region) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(value)) %>%
      slice_head(n = input$snap_top_n)

    is_pc <- str_detect(input$snap_source, "per_capita")

    p <- ggplot(top_df, aes(
      x    = reorder(Country, value),
      y    = value,
      fill = Region,
      text = paste0("<b>", Country, "</b><br>",
                    "Value: ", format_gdp(value, is_pc), "<br>",
                    "Region: ", Region)
    )) +
      geom_col(width = 0.75) +
      coord_flip() +
      scale_fill_manual(values = CONFIG$palette, na.value = "#adb5bd") +
      scale_y_continuous(
        labels = if (is_pc) dollar_format(accuracy = 1)
                 else function(x) paste0("$", x / 1e3, "B"),
        trans  = if (input$snap_log_scale) "log10" else "identity"
      ) +
      labs(x = NULL, y = NULL, fill = "Region") +
      theme_minimal(base_family = "IBM Plex Sans") +
      theme(
        plot.background  = element_rect(fill = "#111827", color = NA),
        panel.background = element_rect(fill = "#111827", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#1e2d3d"),
        axis.text  = element_text(color = "#a8b8cc", size = 8),
        legend.background = element_rect(fill = "#111827"),
        legend.text  = element_text(color = "#a8b8cc"),
        legend.title = element_text(color = "#7aa7c7")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#111827",
        plot_bgcolor  = "#111827",
        font          = list(color = "#a8b8cc"),
        legend        = list(font = list(color = "#a8b8cc"))
      )
  })

  # ---- Snapshot pie chart ---------------------------------------------------
  output$snap_pie <- renderPlotly({
    df <- snap_data(); req(df)
    reg_df <- df %>%
      group_by(Region) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total))

    plot_ly(
      reg_df, labels = ~Region, values = ~total, type = "pie",
      textinfo    = "label+percent",
      hoverinfo   = "label+value+percent",
      marker      = list(colors = CONFIG$palette[reg_df$Region],
                         line   = list(color = "#0a0e1a", width = 2))
    ) %>%
      layout(
        paper_bgcolor = "#111827",
        font          = list(color = "#a8b8cc", family = "IBM Plex Sans"),
        legend        = list(font = list(color = "#a8b8cc")),
        showlegend    = TRUE
      )
  })

  # ---- Snapshot table -------------------------------------------------------
  output$snap_table <- renderDT({
    df <- snap_data(); req(df)
    is_pc <- str_detect(input$snap_source, "per_capita")
    display <- df %>%
      group_by(Country, Region) %>%
      summarise(
        `Value (USD)` = mean(value, na.rm = TRUE),
        Sources       = paste(unique(source_org), collapse = ", "),
        .groups       = "drop"
      ) %>%
      arrange(desc(`Value (USD)`)) %>%
      mutate(
        Rank          = row_number(),
        `Formatted`   = sapply(`Value (USD)`, format_gdp, is_per_capita = is_pc)
      ) %>%
      select(Rank, Country, Region, `Value (USD)`, Formatted, Sources)

    datatable(
      display,
      options  = list(pageLength = 20, scrollX = TRUE, dom = "frtip"),
      rownames = FALSE,
      class    = "table-dark table-hover table-sm"
    ) %>%
      formatRound("`Value (USD)`", digits = 1) %>%
      formatStyle("Country",    color = "#7dd3fc") %>%
      formatStyle("Region",     color = "#2dc653") %>%
      formatStyle("Formatted",  color = "#f4a261", fontWeight = "bold")
  })

  # ---- Snapshot download ----------------------------------------------------
  output$snap_download <- downloadHandler(
    filename = function() paste0("gdp_", input$snap_source, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- snap_data()
      req(df)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ---- COMPARE reactive data ------------------------------------------------
  cmp_data <- reactiveVal(NULL)

  observeEvent(input$cmp_fetch, {
    req(input$cmp_countries, input$cmp_measures)
    showNotification("⏳ Loading comparison data…", id = "cmp_loading", duration = NULL)

    results <- lapply(input$cmp_measures, function(key) {
      src <- CONFIG$wiki_sources[[key]]
      df  <- cached_fetch(key, src$url, src$table_index)
      if (!is.null(df)) df$measure_label <- src$label
      df
    })

    combined <- bind_rows(results) %>%
      filter(Country %in% input$cmp_countries) %>%
      group_by(Country, measure_label) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    removeNotification("cmp_loading")
    cmp_data(combined)
  })

  output$cmp_bar <- renderPlotly({
    df <- cmp_data(); req(df)
    # Shorten measure labels
    df <- df %>%
      mutate(measure_short = str_extract(measure_label, "^[^(]+") %>%
                             str_trim() %>% str_trunc(25))
    p <- ggplot(df, aes(
      x    = Country,
      y    = value,
      fill = measure_short,
      text = paste0("<b>", Country, "</b><br>",
                    measure_short, ": $", round(value, 1), "B")
    )) +
      geom_col(position = "dodge", width = 0.7) +
      scale_y_continuous(
        labels = function(x) paste0("$", x / 1e3, "B"),
        trans  = if (input$cmp_log_scale) "log10" else "identity"
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = NULL, fill = "Measure") +
      theme_minimal(base_family = "IBM Plex Sans") +
      theme(
        plot.background  = element_rect(fill = "#111827", color = NA),
        panel.background = element_rect(fill = "#111827", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#1e2d3d"),
        axis.text  = element_text(color = "#a8b8cc", size = 9),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.background = element_rect(fill = "#111827"),
        legend.text  = element_text(color = "#a8b8cc"),
        legend.title = element_text(color = "#7aa7c7")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#a8b8cc"))
  })

  output$cmp_table <- renderDT({
    df <- cmp_data(); req(df)
    wide <- df %>%
      mutate(measure_short = str_extract(measure_label, "^[^(]+") %>% str_trim()) %>%
      select(Country, measure_short, value) %>%
      pivot_wider(names_from = measure_short, values_from = value)
    datatable(wide, rownames = FALSE, class = "table-dark table-hover table-sm",
              options = list(pageLength = 15, dom = "frtip"))
  })

  output$cmp_download <- downloadHandler(
    filename = function() paste0("gdp_comparison_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(cmp_data(), file, row.names = FALSE)
  )

  # ---- BUBBLE chart ---------------------------------------------------------
  bub_data <- reactiveVal(NULL)

  observeEvent(input$bub_fetch, {
    showNotification("⏳ Loading bubble chart data (3 sources)…", id = "bub_load", duration = NULL)
    nom <- cached_fetch("imf_gdp",         CONFIG$wiki_sources$imf_gdp$url,         CONFIG$wiki_sources$imf_gdp$table_index)
    ppp <- cached_fetch("gdp_ppp",         CONFIG$wiki_sources$gdp_ppp$url,         CONFIG$wiki_sources$gdp_ppp$table_index)
    pc  <- cached_fetch("gdp_per_capita",  CONFIG$wiki_sources$gdp_per_capita$url,  CONFIG$wiki_sources$gdp_per_capita$table_index)
    removeNotification("bub_load")

    if (any(sapply(list(nom, ppp, pc), is.null))) {
      showNotification("❌ One or more sources failed to load.", type = "error"); return()
    }

    nom_s <- nom %>% group_by(Country) %>% summarise(gdp_nom = mean(value, na.rm=TRUE))
    ppp_s <- ppp %>% group_by(Country) %>% summarise(gdp_ppp = mean(value, na.rm=TRUE))
    pc_s  <- pc  %>% group_by(Country) %>% summarise(gdp_pc  = mean(value, na.rm=TRUE))

    joined <- nom_s %>%
      inner_join(ppp_s, by = "Country") %>%
      inner_join(pc_s,  by = "Country") %>%
      mutate(Region = assign_region(Country)) %>%
      filter(!is.na(gdp_nom), !is.na(gdp_ppp), !is.na(gdp_pc))

    if (!("all" %in% input$bub_region) && length(input$bub_region) > 0)
      joined <- filter(joined, Region %in% input$bub_region)
    joined <- joined %>% arrange(desc(gdp_nom)) %>% slice_head(n = input$bub_top_n)
    bub_data(joined)
  })

  output$bubble_plot <- renderPlotly({
    df <- bub_data(); req(df)

    p <- ggplot(df, aes(
      x    = gdp_nom,
      y    = gdp_pc,
      size = gdp_ppp,
      color = Region,
      text = paste0("<b>", Country, "</b><br>",
                    "Nominal GDP: ", format_gdp(gdp_nom), "<br>",
                    "GDP per Capita: $", format(round(gdp_pc), big.mark=","), "<br>",
                    "GDP PPP: ", format_gdp(gdp_ppp))
    )) +
      geom_point(alpha = 0.75) +
      scale_color_manual(values = CONFIG$palette) +
      scale_size_continuous(range = c(3, 25), guide = "none") +
      scale_x_continuous(labels = function(x) paste0("$", x/1e3, "T"),
                         trans  = if (input$bub_log_x) "log10" else "identity") +
      scale_y_continuous(labels = dollar_format(accuracy = 1),
                         trans  = if (input$bub_log_y) "log10" else "identity") +
      labs(x = "GDP Nominal (USD billions)", y = "GDP per Capita (USD)", color = "Region") +
      theme_minimal(base_family = "IBM Plex Sans") +
      theme(
        plot.background  = element_rect(fill = "#111827", color = NA),
        panel.background = element_rect(fill = "#111827", color = NA),
        panel.grid       = element_line(color = "#1e2d3d"),
        axis.text        = element_text(color = "#a8b8cc"),
        legend.background = element_rect(fill = "#111827"),
        legend.text      = element_text(color = "#a8b8cc"),
        legend.title     = element_text(color = "#7aa7c7")
      )

    if (input$bub_label) {
      p <- p + ggrepel::geom_text_repel(
        aes(label = Country), size = 2.5, color = "#a8b8cc",
        max.overlaps = 20, segment.color = "#2a3f54"
      )
    }

    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#a8b8cc"))
  })

  # ---- GROWTH chart ---------------------------------------------------------
  grw_data <- reactiveVal(NULL)

  observeEvent(input$grw_fetch, {
    showNotification("⏳ Loading growth rate data…", id = "grw_load", duration = NULL)
    raw <- cached_fetch("gdp_growth", CONFIG$wiki_sources$gdp_growth$url,
                        CONFIG$wiki_sources$gdp_growth$table_index)
    removeNotification("grw_load")
    if (is.null(raw)) { showNotification("❌ Failed to load.", type = "error"); return() }

    df <- raw %>%
      group_by(Country, Region) %>%
      summarise(growth = mean(value, na.rm = TRUE), .groups = "drop")

    if (!("all" %in% input$grw_region) && length(input$grw_region) > 0)
      df <- filter(df, Region %in% input$grw_region)
    df <- if (input$grw_bottom)
            df %>% arrange(growth)  %>% slice_head(n = input$grw_top_n)
          else
            df %>% arrange(desc(growth)) %>% slice_head(n = input$grw_top_n)

    grw_data(df)
  })

  output$grw_plot <- renderPlotly({
    df <- grw_data(); req(df)
    p <- ggplot(df, aes(
      x    = reorder(Country, growth),
      y    = growth,
      fill = ifelse(growth >= 0, "Positive", "Negative"),
      text = paste0("<b>", Country, "</b><br>Growth: ", round(growth, 2), "%")
    )) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_fill_manual(values = c(Positive = "#2dc653", Negative = "#e63946"), guide = "none") +
      scale_y_continuous(
        labels = function(x) paste0(x, "%"),
        trans  = if (input$grw_log_scale) "log10" else "identity"
      ) +
      geom_hline(yintercept = 0, color = "#a8b8cc", linewidth = .4) +
      labs(x = NULL, y = "Real GDP Growth Rate (%)") +
      theme_minimal(base_family = "IBM Plex Sans") +
      theme(
        plot.background  = element_rect(fill = "#111827", color = NA),
        panel.background = element_rect(fill = "#111827", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#1e2d3d"),
        axis.text = element_text(color = "#a8b8cc", size = 8)
      )

    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#111827", plot_bgcolor = "#111827",
             font = list(color = "#a8b8cc"))
  })

  output$grw_table <- renderDT({
    df <- grw_data(); req(df)
    df <- df %>% arrange(desc(growth)) %>%
      mutate(Rank = row_number(), Growth = paste0(round(growth, 2), "%")) %>%
      select(Rank, Country, Region, Growth)
    datatable(df, rownames = FALSE, class = "table-dark table-hover table-sm",
              options = list(pageLength = 15, dom = "frtip")) %>%
      formatStyle("Growth", color = styleInterval(0, c("#e63946", "#2dc653")),
                  fontWeight = "bold")
  })

  output$grw_download <- downloadHandler(
    filename = function() paste0("gdp_growth_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(grw_data(), file, row.names = FALSE)
  )

}  # end server

# =============================================================================
# LAUNCH
# =============================================================================
shinyApp(ui, server)
