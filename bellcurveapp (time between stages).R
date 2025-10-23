# app.R
# Inquiry → Submit “Bell Curve” (Density) by Program — filterable in Shiny
# - Robust date parsing
# - Okabe–Ito palette + theme_tufte()
# - Single-select Program filter (plus "All Programs")
# - Optional Term filter
# - Median / Quartile reference lines
# - Download plot as PNG

# ---- Packages ----
library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggthemes)   # theme_tufte()
library(gt)

# ---- Config ----
# Use forward slashes on Windows or double backslashes
DATA_PATH <- "C:/Users/Todd/Dropbox/Daily Export/totalfunnelexport.csv"

# Okabe–Ito palette
okabe_ito <- c(
  "#E69F00","#56B4E9","#009E73","#F0E442",
  "#0072B2","#D55E00","#CC79A7","#000000"
)

# ---- Helpers ----
parse_any_date <- function(x) {
  suppressWarnings({
    y <- ymd(x, quiet = TRUE)
    y[is.na(y)] <- mdy(x[is.na(y)], quiet = TRUE)
    y[is.na(y)] <- ymd_hms(x[is.na(y)], quiet = TRUE)
    y[is.na(y)] <- mdy_hms(x[is.na(y)], quiet = TRUE)
    y[is.na(y)] <- parse_date_time(x[is.na(y)],
                                   orders = c("ymd","mdy","dmy","ymd HMS","mdy HMS","dmy HMS"),
                                   quiet = TRUE)
    as_date(y)
  })
}

load_data <- function(path) {
  raw <- readr::read_csv(path, show_col_types = FALSE) |> clean_names()
  
  # Expected columns after clean_names():
  #   date_of_inquiry, application_submit_date, active_major_calculated, active_term_calculated
  stopifnot(
    all(c("date_of_inquiry","application_submit_date") %in% names(raw))
  )
  
  df <- raw |> 
    mutate(
      .inquiry = parse_any_date(.data[["date_of_inquiry"]]),
      .submit  = parse_any_date(.data[["application_submit_date"]]),
      .program = as.character(.data[["active_major_calculated"]] %||% NA_character_),
      .term    = as.character(.data[["active_term_calculated"]] %||% NA_character_)
    ) |>
    filter(!is.na(.inquiry), !is.na(.submit)) |>
    mutate(days = as.integer(.submit - .inquiry)) |>
    filter(!is.na(days), days >= 0, days <=500)
  
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-output-error-validation{color:#D55E00;}"))),
  titlePanel("Inquiry → Submit: Bell Curve by Program"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "program",
        label = "Program (Active Major)",
        choices = NULL,  # populated server-side
        selected = NULL,
        multiple = FALSE
      ),
      selectInput(
        "term",
        label = "Active Term (optional)",
        choices = c("All Terms"),
        selected = "All Terms",
        multiple = FALSE
      ),
      sliderInput("bw", "Density bandwidth (days)", min = 2, max = 60, value = 14, step = 2),
      checkboxInput("show_median", "Show median line", TRUE),
      checkboxInput("show_quartiles", "Show quartile band (IQR)", TRUE),
      tags$hr(),
      downloadButton("dl_png", "Download Plot (PNG)")
    ),
    mainPanel(
      width = 9,
      plotOutput("density_plot", height = 420),
      tags$br(),
      gt_output("stats_table")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  df_all <- reactive({
    validate(need(file.exists(DATA_PATH), paste0("File not found: ", DATA_PATH)))
    load_data(DATA_PATH)
  })
  
  observe({
    df <- df_all()
    programs <- sort(unique(df$.program))
    updateSelectInput(session, "program", choices = c("All Programs", programs), selected = programs[1])
    
    terms <- sort(unique(na.omit(df$.term)))
    updateSelectInput(session, "term", choices = c("All Terms", terms), selected = "All Terms")
  })
  
  df_filtered <- reactive({
    d <- df_all()
    # Term filter (optional)
    if (!is.null(input$term) && input$term != "All Terms") {
      d <- d |> filter(.term == input$term)
    }
    d
  })
  
  # Summary stats for selected scope
  stats_tbl <- reactive({
    d <- df_filtered()
    if (!is.null(input$program) && input$program != "All Programs") {
      d <- d |> filter(.program == input$program)
    }
    d |>
      summarise(
        n = n(),
        median_days = median(days),
        p25 = quantile(days, 0.25),
        p75 = quantile(days, 0.75),
        iqr = p75 - p25,
        mean_days = mean(days)
      ) |>
      mutate(across(where(is.numeric), ~round(., 1)))
  })
  
  output$stats_table <- render_gt({
    st <- stats_tbl()
    gt(st) |>
      cols_label(
        n = "Count",
        median_days = "Median (days)",
        p25 = "P25",
        p75 = "P75",
        iqr = "IQR",
        mean_days = "Mean (days)"
      ) |>
      fmt_number(everything(), decimals = 1) |>
      tab_header(
        title = paste0("Summary • ", ifelse(input$program == "All Programs", "All Programs", input$program)),
        subtitle = ifelse(input$term == "All Terms", "All terms", input$term)
      )
  })
  
  output$density_plot <- renderPlot({
    d <- df_filtered()
    
    # If "All Programs": show overall density (filled) and optional median/IQR
    # If one Program selected: show that program density; also overlay the overall as outline for context
    if (is.null(input$program)) return(NULL)
    
    # Overall density for context
    dens_overall <- ggplot(d, aes(x = days)) +
      geom_density(fill = okabe_ito[2], alpha = 0.18, color = okabe_ito[2], adjust = 1, bw = input$bw) +
      theme_tufte(base_family = "sans") +
      labs(x = "Days from Inquiry → Submit", y = "Density",
           title = ifelse(input$program == "All Programs",
                          "Bell Curve: Inquiry → Submit (All Programs)",
                          paste0("Bell Curve: ", input$program)))
    
    if (input$program == "All Programs") {
      p <- dens_overall
      if (input$show_median) {
        med <- median(d$days)
        p <- p + geom_vline(xintercept = med, linetype = "dashed")
      }
      if (input$show_quartiles) {
        q1 <- quantile(d$days, 0.25)
        q3 <- quantile(d$days, 0.75)
        p <- p + annotate("rect", xmin = q1, xmax = q3, ymin = -Inf, ymax = Inf,
                          alpha = 0.08, fill = okabe_ito[3])
      }
      return(p)
    }
    
    # Single program view
    d_prog <- d |> filter(.program == input$program)
    validate(need(nrow(d_prog) > 1, "Not enough observations for this program."))
    
    med <- median(d_prog$days)
    q1  <- quantile(d_prog$days, 0.25)
    q3  <- quantile(d_prog$days, 0.75)
    
    p <- ggplot() +
      # overall outline for context
      geom_density(data = d, aes(x = days),
                   color = okabe_ito[2], linewidth = 0.8, alpha = 0.6, bw = input$bw) +
      # program fill
      geom_density(data = d_prog, aes(x = days),
                   fill = okabe_ito[1], alpha = 0.35, color = okabe_ito[1], bw = input$bw) +
      theme_tufte(base_family = "sans") +
      labs(x = "Days from Inquiry → Submit", y = "Density",
           title = paste0("Bell Curve: ", input$program))
    
    if (input$show_median) p <- p + geom_vline(xintercept = med, linetype = "dashed")
    if (input$show_quartiles) p <- p +
      annotate("rect", xmin = q1, xmax = q3, ymin = -Inf, ymax = Inf,
               alpha = 0.08, fill = okabe_ito[3])
    
    p
  })
  
  output$dl_png <- downloadHandler(
    filename = function() {
      prog <- ifelse(input$program == "All Programs", "all_programs", gsub("[^A-Za-z0-9]+","_", input$program))
      term <- ifelse(input$term == "All Terms", "all_terms", gsub("[^A-Za-z0-9]+","_", input$term))
      paste0("inquiry_to_submit_density_", prog, "_", term, ".png")
    },
    content = function(file) {
      # Re-render the same plot for saving
      png(file, width = 1200, height = 600, res = 144)
      print(isolate({ output$density_plot() }))
      dev.off()
    }
  )
}

shinyApp(ui, server)
