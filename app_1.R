# =============================================================================
# app.R — GDP Per Capita vs. Life Expectancy (Gapminder Animated)
# =============================================================================
# Replicates Dasha Metropolitansky's animated gapminder chart.
# Only control: continent selector (Americas / Africa / Asia+Oceania).
#
# HOW TO RUN:
#   install.packages(c("shiny", "gapminder", "tidyverse", "gganimate",
#                      "gifski", "png", "bslib"))
#   shiny::runApp()
# =============================================================================

library(shiny)
library(bslib)
library(gapminder)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

# Pre-process data once (collapse Oceania into Asia, as in original)
gap_clean <- gapminder %>%
  mutate(continent = fct_collapse(continent, Asia = c("Asia", "Oceania"))) %>%
  filter(continent %in% c("Americas", "Africa", "Asia"))

CONTINENT_CHOICES <- c("Americas", "Africa", "Asia")

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
    .app-title { font-size: 1.25rem; font-weight: 700; color: #222; margin-bottom: 2px; }
    .app-subtitle { font-size: .82rem; color: #777; margin-bottom: 20px; }
    .card { border: 1px solid #ddd; border-radius: 10px; background: #fff; box-shadow: 0 2px 8px rgba(0,0,0,.06); }
    .continent-label { font-size: .72rem; font-weight: 700; letter-spacing: .1em;
                       text-transform: uppercase; color: #555; margin-bottom: 4px; }
    .form-select { border-radius: 8px !important; border: 1.5px solid #c8d8e8 !important;
                   font-size: .9rem !important; }
    .btn-primary { border-radius: 8px !important; font-size: .85rem !important; }
    img.gif-out { border-radius: 8px; width: 100%; }
    .spinner-wrap { text-align: center; padding: 60px 20px; color: #888; font-size: .9rem; }
  "))),

  br(),
  tags$div(class = "app-title", "The World Gets Better Every Year"),
  tags$div(class = "app-subtitle",
    "GDP Per Capita vs. Life Expectancy · Gapminder data · animated by year"),

  card(
    padding = "20px",
    tags$div(class = "continent-label", "Select Continent"),
    layout_columns(
      col_widths = c(6, 3, 3),
      selectInput("continent", label = NULL,
        choices  = CONTINENT_CHOICES,
        selected = "Americas"
      ),
      actionButton("go", "▶  Animate", class = "btn-primary w-100"),
      tags$div()   # spacer
    )
  ),

  br(),

  card(
    padding = "16px",
    uiOutput("plot_area")
  ),

  br(),
  tags$p(style = "font-size:.72rem; color:#aaa; text-align:center;",
    "Source: gapminder R package · Oceania merged into Asia (following original chart)")
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  output$plot_area <- renderUI({
    tags$div(class = "spinner-wrap",
      tags$p("Select a continent and click ▶ Animate to generate the chart."),
      tags$p(style = "font-size:.75rem; color:#bbb;",
        "(Animation takes ~10–20 seconds to render)")
    )
  })

  observeEvent(input$go, {

    # Show spinner while rendering
    output$plot_area <- renderUI({
      tags$div(class = "spinner-wrap",
        tags$div(class = "spinner-border text-primary",
                 role = "status", style = "width:2rem;height:2rem;"),
        tags$p(style = "margin-top:14px;",
          paste0("Animating ", input$continent, "… (~15 sec)"))
      )
    })

    # Build the animation (isolate to prevent re-trigger on other reactives)
    continent_sel <- isolate(input$continent)

    df <- gap_clean %>%
      filter(continent == continent_sel)

    p <- ggplot(df,
        aes(x = gdpPercap, y = lifeExp,
            size = pop, color = country)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_color_manual(values = country_colors) +
      scale_x_log10(
        breaks = c(1000, 10000),
        labels = c("$1,000", "$10,000")
      ) +
      labs(
        x       = "GDP Per Capita",
        y       = "Life Expectancy",
        caption = "Source: gapminder package"
      ) +
      transition_time(year) +
      ease_aes() +
      ggtitle(paste0("The World Gets Better Every Year: {frame_time}")) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title      = element_text(size = 18, face = "bold"),
        axis.title.x    = element_text(size = 14),
        axis.title.y    = element_text(size = 14),
        panel.grid.minor = element_blank()
      )

    # Render to a temp GIF file
    gif_path <- tempfile(fileext = ".gif")
    anim_save(gif_path,
              animate(p,
                      nframes   = 100,
                      fps       = 10,
                      width     = 700,
                      height    = 480,
                      renderer  = gifski_renderer()))

    # Encode GIF as base64 and show inline
    b64 <- base64enc::base64encode(gif_path)
    src <- paste0("data:image/gif;base64,", b64)

    output$plot_area <- renderUI({
      tags$img(src = src, class = "gif-out",
               alt = paste("Animated gapminder chart:", continent_sel))
    })
  })
}

shinyApp(ui, server)
