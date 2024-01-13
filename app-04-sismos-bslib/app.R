library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(leaflet)
library(DT)
library(highcharter)
library(bslib)
library(bsicons)

get_sismos <- function(fecha = Sys.Date()) {
  url <- str_glue(
    "https://www.sismologia.cl/sismicidad/catalogo/{anio}/{mes}/{fecha}.html",
    anio = year(fecha),
    mes = format(fecha, "%m"),
    fecha = format(fecha, "%Y%m%d")
  )
  
  datos <- read_html(url) |>
    html_table() |>
    dplyr::nth(2) |>
    janitor::clean_names() |>
    tidyr::separate(
      latitud_longitud,
      into = c("latitud", "longitud"),
      sep = " ",
      convert = TRUE
    ) |>
    mutate(
      info = str_glue("Magnitud: {magnitud_2}<br>Profundidad: {profundidad}"),
      profundidad = readr::parse_number(profundidad),
      magnitud = readr::parse_number(magnitud_2),
      fecha_utc = readr::parse_datetime(fecha_utc),
      magnitud_cat = cut(magnitud, breaks = c(1:7, Inf))
    )
  datos
  
}

color <- "#0954e7" 

tema <- bs_theme(
  bg = "#FFF",
  fg = "#454546",
  primary = color,
  base_font = "Open Sans",
  heading_font = "Montserrat"
  )

ui <- page_navbar(
  title = tags$b("Sismos"),
  lang = "es",
  theme = tema,
  inverse = FALSE,
  sidebar = sidebar(
    bg = "#f9f8f8",
    width = 300,
    dateInput(
      "fecha",
      label = "Fecha",
      value = Sys.Date(),
      max = Sys.Date(),
      language = "es"
      ),
    ),
  nav_panel(
    title = tags$span("Inicio", class = "me-3"),
    icon  = icon("dashboard"),
    layout_column_wrap(
      width = 1/1,
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Mapa", popover(bs_icon("info-circle"), title = "Mapa", "Efectivamente es un mapa."),
          ),
          leafletOutput("mapa"),
          full_screen = TRUE
        ),
        card(
          card_header("Gráfico"),
          highchartOutput("grafico")
        ),
      ),
      card(
        card_header("Tabla"),
        dataTableOutput("tabla"),
        full_screen = TRUE
        ),
      )
  ),
  nav_panel(
    title = tags$span("Acerca de", class = "me-3"),
    icon  = icon("info-circle"),
    "Geodesia: Es la ciencia que mide la forma y dimensiones de la Tierra y su representación, incluyendo el campo de gravedad externo. De la medición continua o repetida de la forma de la Tierra, es posible extraer las variaciones en su forma..."
  )
)

server <- function(input, output) {
  sismos <- reactive({
    sismos <- get_sismos(input$fecha)
    sismos
  })
  
  output$mapa <- renderLeaflet({
    sismos <- sismos()
    leaflet(sismos) |>
      addTiles() |>
      addMarkers(
        lng = ~ longitud,
        lat = ~ latitud,
        popup = ~ as.character(info),
        label = ~ as.character(fecha_local_lugar)
      )
  })
  
  output$tabla <- renderDataTable({
    sismos()
  })
  
  output$grafico <- renderHighchart({
    sismos <- sismos()
    
    lvls <-
      c("(1,2]",
        "(2,3]",
        "(3,4]",
        "(4,5]",
        "(5,6]",
        "(6,7]",
        "(7,Inf]")
    
    d <- sismos |>
      count(magnitud_cat) |>
      complete(magnitud_cat = lvls) |>
      mutate(n = coalesce(n, 0),
             magnitud_cat = factor(magnitud_cat, levels = lvls)) |>
      arrange(magnitud_cat)
    
    hchart(d, "column", hcaes(magnitud_cat, n), name = "Conteo", color =  "#0954e7")
  })
  
}

shinyApp(ui = ui, server = server)
