library(terra)
terraOptions(tempdir = "/srv/shiny-server/temp")


library(shiny)
library(leaflet)
library(dplyr)
library(googlesheets4)
library(htmltools)
library(tibble)

install.packages("rsconnect") # if not already
rsconnect::writeManifest()

# ---------------------------
# Google Sheet config
# ---------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1y8QkjZxZKbFMq-Vd3RIV6w6338l36axjV0OvdOMH9rc/edit?usp=sharing"
gs4_deauth()  # if your sheet is public; remove if private

# ---------------------------
# Helpers: safe reading + ensure columns exist
# ---------------------------
safe_read_sheet <- function(sheet_url, sheet_name) {
  tryCatch(
    googlesheets4::read_sheet(sheet_url, sheet = sheet_name, guess_max = 1000),
    error = function(e) {
      message(sprintf("Warning: could not read sheet '%s': %s", sheet_name, e$message))
      tibble()
    }
  )
}

ensure_columns <- function(df, expected) {
  existing <- names(df)
  for (col in expected) {
    if (!(col %in% existing)) df[[col]] <- NA
  }
  df <- df %>% select(all_of(expected), everything())
  df
}

expected_books_cols <- c(
  "title", "author", "year", "lat", "lon",
  "type_of_ban", "banning_institution_type", "jurisdiction",
  "cover_url", "source_url", "description"
)

expected_networks_cols <- c(
  "coalition_name", "institution_type", "year", "lat", "lon",
  "source_url", "description"
)

# ---------------------------
# Data loaders
# ---------------------------
load_books <- function(sheet_url) {
  df <- safe_read_sheet(sheet_url, "books")
  if (nrow(df) == 0) return(tibble())
  
  df <- df %>%
    rename_with(~tolower(gsub("\\s+", "_", .x))) %>%
    select(-any_of(c("timestamp")))
  
  df <- ensure_columns(df, expected_books_cols)
  
  df %>% mutate(
    id = as.character(row_number()),
    title = as.character(title),
    author = as.character(author),
    year = suppressWarnings(as.integer(as.numeric(year))),
    lat = suppressWarnings(as.numeric(lat)),
    lon = suppressWarnings(as.numeric(lon)),
    type_of_ban = as.character(type_of_ban),
    banning_institution_type = as.character(banning_institution_type),
    jurisdiction = as.character(jurisdiction),
    cover_url = as.character(cover_url),
    source_url = as.character(source_url),
    description = as.character(description)
  )
}

load_networks <- function(sheet_url) {
  df <- safe_read_sheet(sheet_url, "networks")
  if (nrow(df) == 0) return(tibble())
  
  df <- df %>%
    rename_with(~tolower(gsub("\\s+", "_", .x))) %>%
    select(-any_of(c("timestamp")))
  
  df <- ensure_columns(df, expected_networks_cols)
  
  df %>% mutate(
    id = as.character(row_number()),
    coalition_name = as.character(coalition_name),
    institution_type = as.character(institution_type),
    year = suppressWarnings(as.integer(as.numeric(year))),
    lat = suppressWarnings(as.numeric(lat)),
    lon = suppressWarnings(as.numeric(lon)),
    source_url = as.character(source_url),
    description = as.character(description)
  )
}

# ===========================
# UI
# ===========================
ui <- fluidPage(
  titlePanel("Global Banned Books Map"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      textInput("q", "Search (title / author / coalition)", value = ""),
      selectInput("ban_type", "Book: Ban type",
                  choices = c("All" = "", "official_gov_order","court_order","school_ban","retailer_removal","library_removal"),
                  selected = ""),
      selectInput("inst_type", "Book: Banning institution type",
                  choices = c("All" = "", "Government","Court","School","Retailer","Library","Other"),
                  selected = ""),
      selectInput("network_inst_type", "Network: Institution type",
                  choices = c("All" = "", "NGO","Political group","Religious org","Publisher","Other"),
                  selected = ""),
      checkboxInput("show_books", "Show books layer", TRUE),
      checkboxInput("show_networks", "Show networks layer", TRUE),
      uiOutput("year_slider_ui"),
      actionButton("reset", "Reset filters"),
      width = 3
    ),
    mainPanel(
      leafletOutput("map", height = "75vh"),
      hr(),
      uiOutput("details"),
      width = 9
    )
  )
)

# ===========================
# Server
# ===========================
server <- function(input, output, session) {
  
  books_raw <- reactive({
    invalidateLater(60000, session)
    load_books(sheet_url)
  })
  
  networks_raw <- reactive({
    invalidateLater(60000, session)
    load_networks(sheet_url)
  })
  
  output$year_slider_ui <- renderUI({
    b <- books_raw()
    n <- networks_raw()
    years <- c(b$year, n$year)
    years <- years[!is.na(years)]
    if (length(years) == 0) {
      sliderInput("year", "Year range", min = 1900, max = as.integer(format(Sys.Date(), "%Y")),
                  value = c(1900, as.integer(format(Sys.Date(), "%Y"))), sep = "")
    } else {
      sliderInput("year", "Year range", min = min(years), max = max(years), value = c(min(years), max(years)), sep = "")
    }
  })
  
  filtered_books <- reactive({
    df <- books_raw()
    if (nrow(df) == 0) return(df)
    if (!is.null(input$year)) df <- df %>% filter(is.na(year) | (year >= input$year[1] & year <= input$year[2]))
    if (!is.null(input$ban_type) && nzchar(input$ban_type)) df <- df %>% filter(tolower(type_of_ban) == tolower(input$ban_type))
    if (!is.null(input$inst_type) && nzchar(input$inst_type)) df <- df %>% filter(tolower(banning_institution_type) == tolower(input$inst_type))
    q <- trimws(input$q)
    if (!is.null(q) && nchar(q) > 0) df <- df %>% filter(grepl(q, paste0(coalesce(title, ""), " ", coalesce(author, "")), ignore.case = TRUE))
    df %>% filter(!is.na(lat) & !is.na(lon))
  })
  
  filtered_networks <- reactive({
    df <- networks_raw()
    if (nrow(df) == 0) return(df)
    if (!is.null(input$year)) df <- df %>% filter(is.na(year) | (year >= input$year[1] & year <= input$year[2]))
    if (!is.null(input$network_inst_type) && nzchar(input$network_inst_type)) df <- df %>% filter(tolower(institution_type) == tolower(input$network_inst_type))
    q <- trimws(input$q)
    if (!is.null(q) && nchar(q) > 0) df <- df %>% filter(grepl(q, paste0(coalesce(coalition_name, ""), " ", coalesce(description, "")), ignore.case = TRUE))
    df %>% filter(!is.na(lat) & !is.na(lon))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 0, lat = 20, zoom = 2)
  })
  
  observe({
    proxy <- leafletProxy("map") %>% clearMarkers()
    
    if (isTRUE(input$show_books)) {
      dfb <- filtered_books()
      if (nrow(dfb) > 0) {
        proxy %>% addCircleMarkers(
          data = dfb,
          lng = ~lon, lat = ~lat,
          layerId = ~paste0("book_", id),
          color = "red", radius = 6, fillOpacity = 0.9,
          label = ~title,
          popup = ~paste0(
            "<b>", coalesce(title, ""), "</b><br/>",
            "<i>", coalesce(author, ""), "</i><br/>",
            "Year: ", coalesce(as.character(year), "N/A"), "<br/>",
            "Ban type: ", coalesce(type_of_ban, ""), "<br/>",
            "Jurisdiction: ", coalesce(jurisdiction, ""), "<br/>",
            ifelse(!is.na(source_url) & nzchar(source_url),
                   paste0("<a href='", source_url, "' target='_blank'>Source</a>"), "")
          )
        )
      }
    }
    
    if (isTRUE(input$show_networks)) {
      dfn <- filtered_networks()
      if (nrow(dfn) > 0) {
        proxy %>% addCircleMarkers(
          data = dfn,
          lng = ~lon, lat = ~lat,
          layerId = ~paste0("network_", id),
          color = "blue", radius = 6, fillOpacity = 0.9,
          label = ~coalition_name,
          popup = ~paste0(
            "<b>", coalesce(coalition_name, ""), "</b><br/>",
            "Institution type: ", coalesce(institution_type, ""), "<br/>",
            "Year: ", coalesce(as.character(year), "N/A"), "<br/>",
            coalesce(description, ""), "<br/>",
            ifelse(!is.na(source_url) & nzchar(source_url),
                   paste0("<a href='", source_url, "' target='_blank'>Source</a>"), "")
          )
        )
      }
    }
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id)) return()
    
    if (startsWith(click$id, "book_")) {
      df <- filtered_books()
      book <- df %>% filter(paste0("book_", id) == click$id) %>% slice(1)
      if (nrow(book) == 1) {
        output$details <- renderUI({
          tags$div(
            tags$h3(book$title),
            tags$p(tags$strong("Author: "), book$author),
            tags$p(tags$strong("Jurisdiction / Ban type: "), book$jurisdiction, " — ", book$type_of_ban),
            tags$p(tags$strong("Year: "), book$year),
            if (!is.na(book$cover_url) && nzchar(book$cover_url))
              tags$img(src = book$cover_url, style="max-width:150px;float:right;margin-left:10px;"),
            tags$p(book$description),
            if (!is.na(book$source_url) & nzchar(book$source_url))
              tags$p(tags$a(href = book$source_url, target="_blank", "Primary source"))
          )
        })
      }
    } else if (startsWith(click$id, "network_")) {
      df <- filtered_networks()
      net <- df %>% filter(paste0("network_", id) == click$id) %>% slice(1)
      if (nrow(net) == 1) {
        output$details <- renderUI({
          tags$div(
            tags$h3(net$coalition_name),
            tags$p(tags$strong("Institution type: "), net$institution_type),
            tags$p(tags$strong("Year: "), net$year),
            tags$p(net$description),
            if (!is.na(net$source_url) && nzchar(net$source_url))
              tags$p(tags$a(href = net$source_url, target="_blank", "Primary source"))
          )
        })
      }
    }
  })
  
  observeEvent(input$reset, {
    updateTextInput(session, "q", value = "")
    updateSelectInput(session, "ban_type", selected = "")
    updateSelectInput(session, "inst_type", selected = "")
    updateSelectInput(session, "network_inst_type", selected = "")
    dfb <- books_raw()
    years <- dfb$year[!is.na(dfb$year)]
    updateSliderInput(session, "year", value = c(ifelse(length(years) > 0, min(years), 1900),
                                                 ifelse(length(years) > 0, max(years), as.integer(format(Sys.Date(), "%Y")))))
  })
}

# ===========================
# Run App
# ===========================
shinyApp(ui, server)


