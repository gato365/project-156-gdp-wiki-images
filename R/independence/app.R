# =============================================================================
# app.R — Independence Through the Years (Gapminder Animated)
# =============================================================================
# Countries are drawn gray while under colonial rule.
# The frame they gain independence they FLASH gold (two-ring burst).
# From then on they glow in their continent's vivid colour.
#
# Axes: GDP per Capita (x) vs Life Expectancy (y), bubble size = population.
#
# Data:
#   gapminder R package (1952-2007, 5-year steps)
#   ../../Data/independence_years_full.csv
#
# HOW TO RUN from the project root:
#   shiny::runApp("R/independence")
#
# Packages required:
#   install.packages(c("shiny","bslib","gapminder","tidyverse",
#                      "gganimate","gifski","png","base64enc"))
# =============================================================================

library(shiny)
library(bslib)
library(gapminder)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(base64enc)

# =============================================================================
# COLOUR CONSTANTS
# =============================================================================
COLONIAL_COLOR <- "#546e7a"     # muted blue-gray — pre-independence
FLASH_COLOR    <- "#ffd700"     # gold burst — the year independence is gained

# Vivid post-independence colour per continent
CONT_VIVID <- c(
  Africa   = "#ff6b35",         # vivid orange
  Americas = "#06d6a0",         # vivid teal
  Asia     = "#118ab2",         # vivid blue
  Europe   = "#ef476f"          # vivid pink-red
)

CONT_CHOICES <- names(CONT_VIVID)
YEAR_MIN     <- 1952L
YEAR_MAX     <- 2007L

# =============================================================================
# DATA PREPARATION — runs once at startup
# =============================================================================

# Independence metadata
indep_raw <- read.csv(
  "../../Data/independence_years_full.csv",
  stringsAsFactors = FALSE
)

# Gapminder base: collapse Oceania into Asia (keeps country_colors working)
gap_base <- gapminder %>%
  mutate(
    continent   = fct_collapse(continent, Asia = c("Asia", "Oceania")),
    country_chr = as.character(country)
  ) %>%
  filter(continent %in% CONT_CHOICES)

# Join independence info onto every country-year row
gap_joined <- gap_base %>%
  left_join(
    indep_raw %>%
      select(country, independence_year,
             during_gapminder, pre_gapminder, colonial_power),
    by = c("country_chr" = "country")
  ) %>%
  mutate(
    cont_chr = as.character(continent),

    # "flashes" = country gained independence DURING the 1952-2007 window
    flashes = !is.na(independence_year) & during_gapminder == TRUE,

    # Point colour: colonial gray until free, then vivid continent colour
    pt_color = case_when(
      flashes & year < independence_year ~ COLONIAL_COLOR,
      TRUE                               ~ CONT_VIVID[cont_chr]
    ),

    # Colonial points are semi-transparent; independent ones are solid
    pt_alpha = if_else(flashes & year < independence_year, 0.40, 0.82)
  )

# For each flashing country, find the FIRST gapminder year >= independence_year.
# (Gapminder measures every 5 years, so 1960 independence → first frame is 1962, etc.)
first_free <- gap_joined %>%
  filter(flashes, year >= independence_year) %>%
  group_by(country_chr) %>%
  slice_min(year, n = 1L, with_ties = FALSE) %>%
  ungroup() %>%
  select(country_chr, flash_year = year)

gap_full <- gap_joined %>%
  left_join(first_free, by = "country_chr")

# Pre-filtered flash dataset: countries at their exact flash frame
flash_all <- gap_full %>%
  filter(!is.na(flash_year), year == flash_year)

# =============================================================================
# UI
# =============================================================================
ui <- page_fixed(
  theme = bs_theme(
    version    = 5,
    bootswatch = "darkly",
    font_scale = 0.95
  ),

  tags$head(tags$style(HTML("
    body            { background: #0d1117; }
    .app-title      { font-size: 1.5rem; font-weight: 700; color: #e6edf3;
                      text-align: center; margin-bottom: 4px; }
    .app-subtitle   { font-size: .82rem; color: #8b949e; text-align: center;
                      margin-bottom: 18px; }
    .card           { background: #161b22 !important;
                      border: 1px solid #30363d !important;
                      border-radius: 12px !important; }
    .ctrl-label     { font-size: .68rem; font-weight: 700; letter-spacing: .1em;
                      text-transform: uppercase; color: #8b949e; margin-bottom: 4px; }
    .btn-primary    { border-radius: 8px !important; background: #238636 !important;
                      border: none !important; }
    .btn-success    { border-radius: 8px !important; }
    img.gif-out     { border-radius: 10px; width: 100%; display: block; }
    .spinner-wrap   { text-align: center; padding: 60px 20px; color: #8b949e;
                      font-size: .9rem; }
    .export-bar     { display: flex; justify-content: flex-end;
                      padding-top: 10px; }
    .legend-wrap    { display: flex; flex-wrap: wrap; gap: 16px;
                      justify-content: center; margin-bottom: 18px;
                      font-size: .78rem; color: #8b949e; }
    .legend-item    { display: flex; align-items: center; gap: 6px; }
    .ldot           { width: 13px; height: 13px; border-radius: 50%;
                      flex-shrink: 0; }
  "))),

  br(),
  tags$div(class = "app-title",    "Independence Through the Years"),
  tags$div(class = "app-subtitle",
    "GDP per Capita vs Life Expectancy · ",
    "countries light up as they gain independence"
  ),

  # ---- Colour legend --------------------------------------------------------
  tags$div(
    class = "legend-wrap",
    tags$div(class = "legend-item",
      tags$div(class = "ldot",
               style = paste0("background:", COLONIAL_COLOR, ";")),
      "Pre-independence (colonial rule)"
    ),
    tags$div(class = "legend-item",
      tags$div(class = "ldot",
               style = paste0(
                 "background:", FLASH_COLOR, ";",
                 "box-shadow: 0 0 8px ", FLASH_COLOR, ";"
               )),
      "Year of independence ✨"
    ),
    tags$div(class = "legend-item",
      tags$div(class = "ldot", style = "background:#ff6b35;"),
      "Africa"
    ),
    tags$div(class = "legend-item",
      tags$div(class = "ldot", style = "background:#06d6a0;"),
      "Americas"
    ),
    tags$div(class = "legend-item",
      tags$div(class = "ldot", style = "background:#118ab2;"),
      "Asia"
    ),
    tags$div(class = "legend-item",
      tags$div(class = "ldot", style = "background:#ef476f;"),
      "Europe"
    )
  ),

  # ---- Controls card --------------------------------------------------------
  card(
    padding = "20px",

    # Row 1 — continent selector + animate button
    layout_columns(
      col_widths = c(8, 4),
      tags$div(
        tags$div(class = "ctrl-label", "Continent(s)"),
        selectizeInput("continent", label = NULL,
          choices  = CONT_CHOICES,
          selected = "Africa",
          multiple = TRUE,
          options  = list(placeholder = "Pick one or more…")
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", " "),
        actionButton("go", "▶︎  Animate",
                     class = "btn-primary w-100")
      )
    ),

    # Row 2 — year range + speed + pause
    layout_columns(
      col_widths = c(6, 3, 3),
      tags$div(
        tags$div(class = "ctrl-label", "Year Range"),
        sliderInput("year_range", label = NULL,
          min = YEAR_MIN, max = YEAR_MAX,
          value = c(YEAR_MIN, YEAR_MAX),
          step = 5L, sep = "", width = "100%"
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", "Speed (fps)"),
        sliderInput("fps", label = NULL,
          min = 2L, max = 20L, value = 8L, step = 1L, width = "100%"
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", "Pause per year (frames)"),
        sliderInput("pause_frames", label = NULL,
          min = 1L, max = 20L, value = 5L, step = 1L, width = "100%"
        )
      )
    ),

    # Row 3 — country filter + display options
    layout_columns(
      col_widths = c(8, 4),
      tags$div(
        tags$div(class = "ctrl-label",
                 "Country Filter (leave blank for all)"),
        selectizeInput("countries", label = NULL,
          choices = NULL, selected = NULL, multiple = TRUE,
          options = list(placeholder = "Type to search…", maxItems = 60)
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", "Display Options"),
        checkboxInput("log_x",
          "Log scale — X (GDP per capita)",  value = TRUE),
        checkboxInput("log_y",
          "Log scale — Y (Life expectancy)", value = FALSE),
        checkboxInput("show_lm",
          "Regression lines + correlations",  value = FALSE)
      )
    )
  ),

  br(),

  # ---- Chart card -----------------------------------------------------------
  card(
    padding = "16px",
    uiOutput("plot_area"),
    uiOutput("export_bar")
  ),

  br(),
  tags$p(
    style = "font-size:.7rem; color:#484f58; text-align:center;",
    "Sources: gapminder R package · independence_years_full.csv · ",
    "Oceania merged into Asia"
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  gif_path_rv <- reactiveVal(NULL)

  # Re-populate country list whenever continent changes
  observe({
    req(input$continent)
    avail <- gap_full %>%
      filter(cont_chr %in% input$continent) %>%
      pull(country_chr) %>%
      unique() %>%
      sort()
    updateSelectizeInput(session, "countries",
      choices = avail, selected = character(0L), server = TRUE)
  })

  output$plot_area <- renderUI({
    tags$div(class = "spinner-wrap",
      tags$p("Select a continent and click ▶︎ Animate."),
      tags$p(style = "font-size:.75rem; color:#484f58;",
        "Rendering takes ~15–30 seconds.")
    )
  })
  output$export_bar <- renderUI(NULL)

  observeEvent(input$go, {

    if (length(input$continent) == 0L) {
      showNotification("Select at least one continent.", type = "warning")
      return()
    }

    # Snapshot inputs before long render
    cont_sel  <- isolate(input$continent)
    ctry_sel  <- isolate(input$countries)
    yr_range  <- isolate(input$year_range)
    fps       <- isolate(input$fps)
    log_x        <- isolate(input$log_x)
    log_y        <- isolate(input$log_y)
    show_lm      <- isolate(input$show_lm)
    pause_frames <- isolate(input$pause_frames)

    output$plot_area <- renderUI({
      tags$div(class = "spinner-wrap",
        tags$div(class = "spinner-border text-success",
                 role  = "status",
                 style = "width:2.2rem; height:2.2rem;"),
        tags$p(style = "margin-top:14px;",
          paste0(
            "Animating — ",
            paste(cont_sel, collapse = " + "),
            " • ", yr_range[1], "–", yr_range[2],
            " • ", fps, " fps…"
          )
        )
      )
    })
    output$export_bar <- renderUI(NULL)

    # ---- Filter data --------------------------------------------------------
    df <- gap_full %>%
      filter(
        cont_chr %in% cont_sel,
        year     >= yr_range[1],
        year     <= yr_range[2]
      )
    if (length(ctry_sel) > 0L)
      df <- df %>% filter(country_chr %in% ctry_sel)

    fl <- flash_all %>%
      filter(
        cont_chr %in% cont_sel,
        year     >= yr_range[1],
        year     <= yr_range[2]
      )
    if (length(ctry_sel) > 0L)
      fl <- fl %>% filter(country_chr %in% ctry_sel)

    if (nrow(df) == 0L) {
      showNotification("No data matches the selected filters.",
                       type = "warning")
      output$plot_area <- renderUI({
        tags$div(class = "spinner-wrap",
          tags$p("No data matched the selected filters."))
      })
      return()
    }

    # ---- Per-continent correlations (computed on the displayed scale) --------
    # Label placed at the 85th-pct x of each continent-year, at the predicted
    # y on the regression line — so the label sits on the line itself.
    corr_df <- if (show_lm) {
      df %>%
        group_by(cont_chr, year) %>%
        group_modify(~ {
          sub   <- .x
          x_fit <- if (log_x) log10(sub$gdpPercap) else sub$gdpPercap
          r     <- cor(x_fit, sub$lifeExp, use = "complete.obs")
          x_lab <- quantile(sub$gdpPercap, 0.85, na.rm = TRUE)
          m <- if (log_x) {
            lm(lifeExp ~ log10(gdpPercap), data = sub)
          } else {
            lm(lifeExp ~ gdpPercap, data = sub)
          }
          y_lab <- as.numeric(
            predict(m, newdata = data.frame(gdpPercap = x_lab))
          )
          data.frame(r = r, x_lab = x_lab, y_lab = y_lab,
                     r_label = sprintf("r = %.2f", r))
        }) %>%
        ungroup() %>%
        filter(!is.na(r))
    } else {
      NULL
    }

    # ---- Build chart --------------------------------------------------------
    # {closest_state} is the gganimate glue variable for transition_states
    chart_title <- paste0(
      "Independence Through the Years — {closest_state}"
    )
    chart_sub <- paste(
      "Continent(s):", paste(cont_sel, collapse = " · ")
    )

    p <- ggplot(df, aes(x = gdpPercap, y = lifeExp)) +

      # Layer 1 — main bubbles (colonial gray or vivid continent colour)
      geom_point(
        aes(size = pop, color = pt_color, alpha = pt_alpha),
        show.legend = FALSE
      ) +
      scale_color_identity() +
      scale_alpha_identity() +
      scale_size_continuous(range = c(2, 20), guide = "none") +

      # Layer 2 — solid gold core at the independence moment
      geom_point(
        data        = fl,
        aes(x = gdpPercap, y = lifeExp),
        size        = 7,
        color       = FLASH_COLOR,
        alpha       = 0.95,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +

      # Layer 3 — soft outer glow ring (same layer, larger, very transparent)
      geom_point(
        data        = fl,
        aes(x = gdpPercap, y = lifeExp),
        size        = 13,
        color       = FLASH_COLOR,
        alpha       = 0.22,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +

      # X axis
      (if (log_x)
        scale_x_log10(
          breaks = c(300, 1000, 3000, 10000, 30000),
          labels = scales::dollar_format(accuracy = 1)
        )
       else
        scale_x_continuous(
          labels = scales::dollar_format(accuracy = 1)
        )
      ) +

      # Y axis
      (if (log_y)
        scale_y_log10(breaks = c(20, 30, 40, 50, 60, 70, 80))
       else
        scale_y_continuous(breaks = seq(20L, 85L, 10L))
      ) +

      labs(
        title    = chart_title,
        subtitle = chart_sub,
        x = if (log_x) "GDP Per Capita (log scale)" else "GDP Per Capita",
        y = if (log_y) "Life Expectancy (log scale)" else "Life Expectancy",
        caption  = paste0(
          "Source: gapminder R package · ",
          "independence_years_full.csv"
        )
      ) +

      transition_time(year) +
      ease_aes("linear") +

      theme_minimal(base_family = "sans") +
      theme(
        plot.background  = element_rect(fill = "#0d1117", color = NA),
        panel.background = element_rect(fill = "#0d1117", color = NA),
        panel.grid.major = element_line(color = "#21262d", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(
          size = 17, face = "bold", hjust = 0.5, color = "#e6edf3"
        ),
        plot.subtitle    = element_text(
          size = 11, hjust = 0.5, color = "#8b949e",
          margin = margin(b = 10)
        ),
        plot.caption     = element_text(size = 8, color = "#484f58"),
        axis.title       = element_text(size = 12, color = "#8b949e"),
        axis.text        = element_text(size = 9,  color = "#6e7681")
      )

    # ---- Render GIF ---------------------------------------------------------
    gif_file <- tempfile(fileext = ".gif")
    nframes  <- max(24L, as.integer(
      (yr_range[2] - yr_range[1]) / 5L * 12L
    ))

    anim_save(
      gif_file,
      animate(
        p,
        nframes  = nframes,
        fps      = fps,
        width    = 750L,
        height   = 500L,
        renderer = gifski_renderer()
      )
    )

    gif_path_rv(gif_file)

    b64 <- base64encode(gif_file)
    src <- paste0("data:image/gif;base64,", b64)

    output$plot_area <- renderUI({
      tags$img(src = src, class = "gif-out", alt = "Independence animation")
    })

    output$export_bar <- renderUI({
      tags$div(class = "export-bar",
        downloadButton("export_gif", "⬇ Download GIF",
                       class = "btn-success")
      )
    })
  })

  # Download handler
  output$export_gif <- downloadHandler(
    filename = function() {
      paste0(
        "independence_",
        paste(isolate(input$continent), collapse = "-"), "_",
        isolate(input$year_range)[1], "-",
        isolate(input$year_range)[2], ".gif"
      )
    },
    content = function(file) {
      path <- gif_path_rv()
      req(path)
      file.copy(path, file)
    },
    contentType = "image/gif"
  )
}

shinyApp(ui, server)
