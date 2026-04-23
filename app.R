# =============================================================================
# app.R — GDP Per Capita vs. Life Expectancy (Gapminder Animated)
# =============================================================================
# Controls: continent multi-select, country filter, year range, speed,
#           log-scale toggles, per-continent regression lines + correlations,
#           GIF export.
#
# HOW TO RUN:
#   install.packages(c("shiny", "gapminder", "tidyverse", "gganimate",
#                      "gifski", "png", "bslib", "base64enc"))
#   shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(gapminder)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(base64enc)

# Pre-process: collapse Oceania into Asia (following original chart)
gap_clean <- gapminder %>%
  mutate(continent = fct_collapse(continent, Asia = c("Asia", "Oceania"))) %>%
  filter(continent %in% c("Americas", "Africa", "Asia"))

CONTINENT_CHOICES <- c("Americas", "Africa", "Asia")
YEAR_MIN <- min(gapminder$year)   # 1952
YEAR_MAX <- max(gapminder$year)   # 2007

# One distinct colour per continent — used for regression lines and labels
cont_colors <- c(
  Americas = "#e63946",
  Africa   = "#f4a261",
  Asia     = "#457b9d"
)

# =============================================================================
# UI
# =============================================================================
ui <- page_fixed(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#3d6b8e",
    font_scale = 0.95
  ),
  tags$head(tags$style(HTML("
    body { background: #f7f5f2; font-family: 'Georgia', serif; }
    .app-title    { font-size: 1.35rem; font-weight: 700; color: #222;
                    text-align: center; margin-bottom: 2px; }
    .app-subtitle { font-size: .82rem; color: #777; text-align: center;
                    margin-bottom: 20px; }
    .card { border: 1px solid #ddd; border-radius: 10px; background: #fff;
            box-shadow: 0 2px 8px rgba(0,0,0,.06); }
    .ctrl-label { font-size: .72rem; font-weight: 700; letter-spacing: .1em;
                  text-transform: uppercase; color: #555; margin-bottom: 4px; }
    .form-select { border-radius: 8px !important; border: 1.5px solid #c8d8e8 !important;
                   font-size: .9rem !important; }
    .btn-primary  { border-radius: 8px !important; font-size: .85rem !important; }
    .btn-success  { border-radius: 8px !important; font-size: .85rem !important; }
    img.gif-out   { border-radius: 8px; width: 100%; display: block; }
    .spinner-wrap { text-align: center; padding: 60px 20px; color: #888;
                    font-size: .9rem; }
    .export-bar   { display: flex; justify-content: flex-end; padding-top: 10px; }
  "))),

  br(),
  tags$div(class = "app-title",    "The World Gets Better Every Year"),
  tags$div(class = "app-subtitle",
    "GDP Per Capita vs. Life Expectancy · Gapminder data · animated by year"),

  card(
    padding = "20px",

    # Row 1 — continent + animate button
    layout_columns(
      col_widths = c(8, 4),
      tags$div(
        tags$div(class = "ctrl-label", "Continent(s)"),
        selectizeInput("continent", label = NULL,
          choices  = CONTINENT_CHOICES,
          selected = "Americas",
          multiple = TRUE,
          options  = list(placeholder = "Pick one or more…")
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", " "),
        actionButton("go", "▶︎  Animate", class = "btn-primary w-100")
      )
    ),

    # Row 2 — year range + speed
    layout_columns(
      col_widths = c(8, 4),
      tags$div(
        tags$div(class = "ctrl-label", "Year Range"),
        sliderInput("year_range", label = NULL,
          min = YEAR_MIN, max = YEAR_MAX,
          value = c(YEAR_MIN, YEAR_MAX),
          step = 5, sep = "", width = "100%"
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", "Animation Speed (fps)"),
        sliderInput("fps", label = NULL,
          min = 2, max = 24, value = 10, step = 1, width = "100%"
        )
      )
    ),

    # Row 3 — country filter + display options
    layout_columns(
      col_widths = c(8, 4),
      tags$div(
        tags$div(class = "ctrl-label", "Country Filter (optional — leave blank for all)"),
        selectizeInput("countries", label = NULL,
          choices = NULL, selected = NULL, multiple = TRUE,
          options = list(placeholder = "Type to search countries…", maxItems = 50)
        )
      ),
      tags$div(
        tags$div(class = "ctrl-label", "Display Options"),
        checkboxInput("log_x",   "Log scale — X (GDP per capita)",  value = TRUE),
        checkboxInput("log_y",   "Log scale — Y (Life expectancy)", value = FALSE),
        checkboxInput("show_lm", "Regression lines + correlations",       value = FALSE)
      )
    )
  ),

  br(),

  card(
    padding = "16px",
    uiOutput("plot_area"),
    uiOutput("export_bar")
  ),

  br(),
  tags$p(style = "font-size:.72rem; color:#aaa; text-align:center;",
    "Source: gapminder R package · Oceania merged into Asia (following original chart)")
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  gif_path_rv <- reactiveVal(NULL)

  # Re-populate country list whenever continent changes
  observe({
    req(input$continent)
    available <- gap_clean %>%
      filter(continent %in% input$continent) %>%
      pull(country) %>%
      as.character() %>%
      unique() %>%
      sort()
    updateSelectizeInput(session, "countries",
      choices = available, selected = character(0), server = TRUE)
  })

  output$plot_area <- renderUI({
    tags$div(class = "spinner-wrap",
      tags$p("Select options above and click ▶︎ Animate to generate the chart."),
      tags$p(style = "font-size:.75rem; color:#bbb;",
        "(Animation takes ~10–20 seconds to render)")
    )
  })
  output$export_bar <- renderUI(NULL)

  observeEvent(input$go, {

    if (length(input$continent) == 0) {
      showNotification("Please select at least one continent.", type = "warning")
      return()
    }

    continent_sel <- isolate(input$continent)
    countries_sel <- isolate(input$countries)
    year_range    <- isolate(input$year_range)
    fps           <- isolate(input$fps)
    log_x         <- isolate(input$log_x)
    log_y         <- isolate(input$log_y)
    show_lm       <- isolate(input$show_lm)

    output$plot_area <- renderUI({
      tags$div(class = "spinner-wrap",
        tags$div(class = "spinner-border text-primary",
                 role = "status", style = "width:2rem;height:2rem;"),
        tags$p(style = "margin-top:14px;",
          paste0("Animating ", paste(continent_sel, collapse = " + "),
                 " • ", year_range[1], "–", year_range[2],
                 " • ", fps, " fps… (~15 sec)"))
      )
    })
    output$export_bar <- renderUI(NULL)

    # ---- Filter data ----------------------------------------------------------
    df <- gap_clean %>%
      filter(
        continent %in% continent_sel,
        year >= year_range[1],
        year <= year_range[2]
      )
    if (length(countries_sel) > 0)
      df <- df %>% filter(as.character(country) %in% countries_sel)

    if (nrow(df) == 0) {
      showNotification("No data for the selected filters.", type = "warning")
      output$plot_area <- renderUI({
        tags$div(
          class = "spinner-wrap",
          tags$p("No data matched the selected filters.")
        )
      })
      return()
    }

    # ---- Point alpha: more continents → more transparent ---------------------
    n_cont   <- length(continent_sel)
    pt_alpha <- max(0.25, 0.70 - (n_cont - 1) * 0.18)

    # ---- Pre-compute per-continent, per-year regression labels ---------------
    # Correlation is computed on the same scale as the displayed axes.
    # Label is placed at the 85th-percentile x of each continent-year,
    # at the predicted y of the regression line at that x — so it sits
    # on the line, not floating away from it.
    corr_df <- if (show_lm) {
      df %>%
        group_by(continent, year) %>%
        group_modify(~ {
          sub   <- .x
          x_raw <- sub$gdpPercap
          x_fit <- if (log_x) log10(x_raw) else x_raw
          r     <- cor(x_fit, sub$lifeExp, use = "complete.obs")

          x_lab <- quantile(x_raw, 0.85, na.rm = TRUE)
          m <- if (log_x) {
            lm(lifeExp ~ log10(gdpPercap), data = sub)
          } else {
            lm(lifeExp ~ gdpPercap, data = sub)
          }
          y_lab <- as.numeric(
            predict(m, newdata = data.frame(gdpPercap = x_lab))
          )

          data.frame(
            r       = r,
            x_lab   = x_lab,
            y_lab   = y_lab,
            r_label = sprintf("r = %.2f", r)
          )
        }) %>%
        ungroup() %>%
        filter(!is.na(r))
    } else {
      NULL
    }

    # ---- Build chart ---------------------------------------------------------
    chart_title    <- "The World Gets Better Every Year — {round(frame_time)}"
    chart_subtitle <- paste("Continent(s):", paste(continent_sel, collapse = " · "))

    p <- ggplot(df, aes(x = gdpPercap, y = lifeExp, size = pop, color = country)) +
      geom_point(alpha = pt_alpha, show.legend = FALSE) +
      scale_color_manual(values = country_colors) +
      (if (log_x)
        scale_x_log10(breaks = c(500, 1000, 5000, 10000, 50000),
                      labels = scales::dollar_format(accuracy = 1))
       else
        scale_x_continuous(labels = scales::dollar_format(accuracy = 1))
      ) +
      (if (log_y)
        scale_y_log10(breaks = c(20, 30, 40, 50, 60, 70, 80))
       else
        scale_y_continuous()
      ) +
      scale_size_continuous(range = c(2, 18), guide = "none") +
      labs(
        title    = chart_title,
        subtitle = chart_subtitle,
        x        = if (log_x) "GDP Per Capita (log scale)" else "GDP Per Capita",
        y        = if (log_y) "Life Expectancy (log scale)" else "Life Expectancy",
        caption  = "Source: gapminder package"
      ) +
      transition_time(year) +
      ease_aes() +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title       = element_text(size = 18, face = "bold",  hjust = 0.5),
        plot.subtitle    = element_text(size = 12, color = "#555", hjust = 0.5,
                                        margin = margin(b = 8)),
        axis.title.x     = element_text(size = 14),
        axis.title.y     = element_text(size = 14),
        panel.grid.minor = element_blank()
      )

    # ---- Per-continent regression lines + correlation labels -----------------
    # Each continent gets its own geom_smooth (fixed colour, avoids scale clash
    # with the country-colour scale) and a geom_label positioned ON the line.
    if (show_lm) {
      for (cont in continent_sel) {
        col      <- cont_colors[[cont]]
        df_cont  <- df %>% filter(continent == cont)
        lbl_cont <- corr_df %>% filter(continent == cont)

        p <- p +
          geom_smooth(
            data        = df_cont,
            aes(group   = 1),
            method      = "lm",
            se          = FALSE,
            color       = col,
            linewidth   = 1.15,
            show.legend = FALSE
          ) +
          geom_label(
            data        = lbl_cont,
            aes(x = x_lab, y = y_lab, label = r_label),
            color       = col,
            fill        = "white",
            size        = 3.4,
            fontface    = "bold",
            label.size  = 0.35,
            alpha       = 0.88,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }
    }

    # ---- Render GIF ----------------------------------------------------------
    gif_file <- tempfile(fileext = ".gif")
    nframes  <- max(20L, as.integer((year_range[2] - year_range[1]) / 5L * 10L))

    anim_save(gif_file,
              animate(p,
                      nframes  = nframes,
                      fps      = fps,
                      width    = 700,
                      height   = 480,
                      renderer = gifski_renderer()))

    gif_path_rv(gif_file)

    b64 <- base64encode(gif_file)
    src <- paste0("data:image/gif;base64,", b64)

    alt_text <- paste(
      "Animated gapminder:", paste(continent_sel, collapse = " + ")
    )
    output$plot_area <- renderUI({
      tags$img(src = src, class = "gif-out", alt = alt_text)
    })

    output$export_bar <- renderUI({
      tags$div(
        class = "export-bar",
        downloadButton("export_gif", "⬇ Download GIF", class = "btn-success")
      )
    })
  })

  output$export_gif <- downloadHandler(
    filename = function() {
      paste0("gapminder_",
             paste(isolate(input$continent), collapse = "-"), "_",
             isolate(input$year_range)[1], "-",
             isolate(input$year_range)[2], ".gif")
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
