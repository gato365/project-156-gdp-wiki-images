# =============================================================================
# install_packages.R
# Run this ONCE before launching the app.
# Usage: source("install_packages.R")
# =============================================================================

pkgs <- c(
  "shiny",       # Core Shiny framework
  "bslib",       # Bootstrap 5 theming for Shiny
  "bsicons",     # Bootstrap icons used in value_box()
  "rvest",       # HTML/web scraping from Wikipedia
  "httr",        # HTTP requests (user-agent, timeout control)
  "dplyr",       # Data manipulation (filter, mutate, summarise)
  "tidyr",       # Pivot / reshape (pivot_longer, pivot_wider)
  "ggplot2",     # Base plotting
  "plotly",      # Interactive charts (wraps ggplot2)
  "DT",          # Interactive datatable widget
  "scales",      # Number formatting (dollar, percent, etc.)
  "stringr",     # String manipulation (str_detect, str_remove_all)
  "jsonlite",    # JSON parsing for API fallback
  "ggrepel",     # Non-overlapping text labels in bubble chart
  "shinyjs"      # JavaScript helpers (used for status banner)
)

# Install any missing packages
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cran.rstudio.com/")
} else {
  message("✅ All packages already installed.")
}

message("Done. You can now run: shiny::runApp('gdp_wiki_app/')")
