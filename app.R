library(shiny)
library(shinythemes)
library(highcharter)
library(tidyverse)
library(geojsonio)
library(shinycssloaders)
library(rAmCharts)
library(xts)

#CSS style defined
css <- ''

#Functions to load the data
data_url_server <- "https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/data/"
data_url <- "./data/"
data_source <- "server"

get_path <- function(file_path) {
  if (data_source == "server") {
    return(url(paste0(data_url_server, file_path)))
  } else {
    return(paste0(data_url, file_path))
  }
}

#Load the data from github
##Data from spain about cases, death and comulative incidence
data.spain <- read_csv(get_path("spain_covid_dataset.csv"))
data.spain$Comunidad <- as.character(data.spain$Comunidad)
data.spain$Fecha <- as.Date(data.spain$Fecha, format="%Y-%m-%d")

##Data from the ucis of Spain
data.uci.spain <- read_csv(get_path("ucispain.csv"))

##Vaccunation data
data.spain.vac <- read_csv(get_path("vacspain.csv"))

##Spain map
spain.map <- geojson_read("https://raw.githubusercontent.com/SergioJuanes/coviddataspain/main/data/simple_spain.geojson")

##population of Spain
popcsv <- read_csv(get_path("population.csv"))
spainpopulation <- (popcsv %>% dplyr::filter(Country == "Spain"))$PopTotal

##population CCAA of Spain erxtracted from the INE 2020
pobcoms <- data.frame(Comunidad = c("Andalucía", "Aragón", "Principado de Asturias", "Islas Baleares", "Islas Canarias", "Cantabria", "Castilla y León", "Castilla-La Mancha" , "Cataluña", "Comunidad Valenciana", "Extremadura", "Galicia", "La Rioja", "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra", "País Vasco", "Ceuta", "Melilla"), stringsAsFactors = FALSE, Poblacion = c(8464411, 1329391, 1018784, 1171543, 2175952, 582905, 2394918, 2045221, 7780479, 5057353, 1063987, 2701819, 319914, 6779888, 1511251, 661197, 2220504, 84202, 87076))


#Every page separetly
general_page <- tabPanel("General",
                      fluidRow(
                        div(
                          column(8,
                                 highchartOutput("highchartincidenciaaucumulada", width="100%", height="700px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 h6("*Se calcula la Incidencia Acumulada para 14 días como: Casos durante 14 días/Población sana al inicio del periodo por 100000 habitantes."),
                                 h6("**Cálculo con respecto a la población por Comunidad Autónoma de acuerdo a los datos del INE en 2020.")
                          ),
                          column(4,
                                 highchartOutput("highchartnuevoscasosspain", width = "100%", height = "240px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 highchartOutput("highchartnuevosfallecidosspain", width = "100%", height = "240px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 #fluidRow(
                                 #  column(6,
                                 #         highchartOutput("highchartucispain", width = "100%", height = "220px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 #  ),
                                 #  column(6,
                                 #         highchartOutput("highcharthopitalspain", width = "100%", height = "220px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 #  )
                                 #)
                                 highchartOutput("highchartocupacioneshospital", width="100%", height="200px") %>% withSpinner(type = 6, color = "#000CCC")
                          
                                 
                          ),
                        )
                      )
                      )

detallado_page <- tabPanel("Detallado",
                         selectInput("ccaa", "Elige la CCAA", choices = unique((data.spain %>% dplyr::arrange(Comunidad))$Comunidad), selected = "España"),
                         highchartOutput("highchartincidenciadiarios", height = "500px") %>% withSpinner(type = 6, color = "#000CCC")
                         )

table_page <- tabPanel("Table",
                       h5("TextTable")
                       )

# Define UI for random distribution app ----
ui <- shinyUI(
  fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(css))),
  
  # App title ----
  titlePanel("Situación del COVID-19 en España"),
    
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  general_page,
                  detallado_page,
                  table_page
      )
      
    
  
)
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  output$highchartincidenciaaucumulada <- renderHighchart({
    myClickFunc <- JS("function(event) {Shiny.onInputChange('hcClickedspain', event.point.Comunidad);}")
    data <- data.spain %>% dplyr::filter(Fecha == max(unique(data.spain$Fecha)))
    data$tasacasos <- round(100*(data$Casos/spainpopulation), 2)
    data <- left_join(data, pobcoms, by = "Comunidad")
    data$tasamuertos <- round(100*(data$Fallecidos/data$Poblacion), 2)
    data <- data %>% dplyr::filter(Comunidad != "España")
    
    mapa <- highchart(type="map") %>%
      hc_add_series_map(spain.map, 
                        data,
                        "IA14", c("NAME", "Comunidad"), 
                        nullColor="#F0F0F0", borderColor="#fff",
                        borderWidth=1,
                        dataLabels = list(style = list(fontSize = '12'),enabled = TRUE, format = '{point.name}: {point.value}', color = "white", backgroundColor = 'rgba(0,0,0,0.5)', borderRadius = 7, padding = 5)) %>%
      hc_title(text = "Incidencia Acumulada a 14 días") %>%
      hc_credits(enabled = TRUE, text = 'Fuente: Ministerio de Sanidad', href = 'https://www.mscbs.gob.es/',  target = '_blank') %>%
      hc_tooltip(headerFormat="<b>{point.point.Comunidad}</b><br>",
                 pointFormat="Incidencia acumulada: <b>{point.value}*</b> <br> Número de casos: <b>{point.Casos}</b> <br> Porcentaje de casos: <b>{point.tasacasos}</b>%** <br> Número de fallecidos: <b>{point.Fallecidos}</b> <br> Porcentaje de fallecidos: <b>{point.tasamuertos}</b>%**") %>%
      hc_plotOptions(map = list(states = list(hover = list(color = "#00FFAA"))), series = list(stacking = FALSE, events = list(click = myClickFunc), cursor = "pointer"))
    hc_colorAxis(mapa, minColor = "#CC99FF", maxColor = "#6400C7")
    hc_colorAxis(mapa, min = 0, max = round(max(na.omit(data)$IA14)+50, -2), minColor = "#00CCFF", maxColor = "#0F00FF")  
    
  })
  
  data.spain.filtrado <- reactive({
    if(!is.null(input$hcClickedspain)){
      data.spain %>% dplyr::filter(Comunidad == input$hcClickedspain) %>% dplyr::select(Comunidad, Fecha, CasosDiarios, FallecidosDiarios)
    } else{
      data.spain %>% dplyr::filter(Comunidad == "España") %>% dplyr::select(Fecha, CasosDiarios, FallecidosDiarios)
    }
  })
  
  output$highchartnuevoscasosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Casos diarios en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Casos diarios en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = CasosDiarios), name = "Casos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  output$highchartnuevosfallecidosspain <- renderHighchart({
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Fallecidos diarios en ", unique(data.spain.filtrado()$Comunidad))
    } else {
      titulo <- "Fallecidos diarios en toda España"
    }
    data.spain.filtrado() %>%
      hchart("line", hcaes(x = Fecha, y = FallecidosDiarios), name = "Muertos", color = "#00FFAA") %>%
      hc_title(text = titulo) %>%
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
  })
  
  
  output$highchartocupacioneshospital <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Porcentaje de camas ocupadas en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Porcentaje de camas ocupadas en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    df <- data.frame(
      y = c(data$PercCamasCovid, data$PerCamasUCI),
      target = c(20,20),
      nombre = c("Hospital", "UCI")
    )
    
    hchart(df, "bullet", hcaes(x = nombre, y = y, target = target), color = "blue") %>%
      hc_chart(inverted = TRUE) %>%
      hc_title(text = titulo) %>%
      hc_yAxis(
        min = 0,
        max = 100,
        gridLineWidth = 0
      ) %>%
      hc_xAxis(
        title = list(enabled = FALSE),
        gridLineWidth = 15,
        gridLineColor = "white"
      ) %>% 
      hc_yAxis(
        title = list(enabled = FALSE),
        labels = list(format = "{value}%"),
        plotBands = list(
          list(from = 0, to = 20, color = "lightgreen"),
          list(from = 20, to = 50, color = "lightyellow"),
          list(from = 50, to = 100, color = "lightcoral")
        )
      ) %>%
      hc_tooltip(headerFormat="<b>{point.point.nombre}</b><br>",
                 pointFormat="Porcentaje de capas ocupadas: <b>{point.y}</b> %") %>%
      
      hc_plotOptions(
        series = list(
          stacking = FALSE,
          pointPadding = 0.25,
          pointWidth = 10,
          borderWidth = 0,
          targetOptions = list(width = '200%', color = "red")
        )
      ) 
  })
  
  output$highchartincidenciadiarios <- renderHighchart({
    
    data <- data.spain %>% dplyr::filter(Comunidad == input$ccaa)
    series1 <- xts(x = data$IA14, order.by = as.Date(data$Fecha, format = "%Y-%m-%d"))
    highchart(type = "stock") %>%
      hc_add_series(name = "Incidencia acumulada", series1, color = "#1DA3D5") %>%
      hc_title(text = paste0("Evolución de la incidencia acumulada a 14 días en ", input$ccaa)) %>%
      hc_xAxis(
        plotBands = list(
          list(
            label = list(
              text = "Primera Ola"
            ),
            from = datetime_to_timestamp(as.Date("2020-03-17", tz = "UTC")),
            to = datetime_to_timestamp(as.Date("2020-05-31", tz = "UTC")),
            color = "#FF6960"
          ),
          list(
            label = list(
              text = "Segunda Ola"
            ),
            from = datetime_to_timestamp(as.Date("2020-09-06", tz = "UTC")),
            to = datetime_to_timestamp(as.Date("2020-12-09", tz = "UTC")),
            color = "#EE635A"
          ),
          list(
            label = list(
              text = "Tercera Ola"
            ),
            from = datetime_to_timestamp(as.Date("2020-12-09", tz = "UTC")),
            to = datetime_to_timestamp(as.Date("2021-03-12", tz = "UTC")),
            color = "#DE5C54"
          ),
          list(
            label = list(
              text = "Cuarta Ola"
            ),
            from = datetime_to_timestamp(as.Date("2021-04-01", tz = "UTC")),
            to = datetime_to_timestamp(as.Date("2021-05-24", tz = "UTC")),
            color = "#FF624C"
          ),
          list(
            label = list(
              text = "Quinta Ola"
            ),
            from = datetime_to_timestamp(as.Date("2021-07-23", tz = "UTC")),
            to = datetime_to_timestamp(as.Date("2021-09-28", tz = "UTC")),
            color = "#FF6A4C"
          ),
          list(
            label = list(
              text = "Sexta Ola"
            ),
            from = datetime_to_timestamp(as.Date("2021-11-10", tz = "UTC")),
            to = datetime_to_timestamp(as.Date(Sys.Date(), tz = "UTC")),
            color = "#FF7A4C"
          )
        ))
  
  })
  
  
}

shinyApp(ui, server)