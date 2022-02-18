library(shiny)
library(shinythemes)
library(highcharter)
library(tidyverse)
library(geojsonio)
library(shinycssloaders)
library(rAmCharts)

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
data.spain <- read.csv(get_path("spain_covid_dataset.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
data.spain$Comunidad <- as.character(data.spain$Comunidad)
data.spain$Fecha <- as.Date(data.spain$Fecha, format="%Y-%m-%d")

##Data from the ucis of Spain
data.uci.spain <- read.csv(get_path("ucispain.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")

##Vaccunation data
data.spain.vac <- read.csv(get_path("vacspain.csv"), stringsAsFactors = FALSE)

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
                                 amChartsOutput("amchartucis", width="100%", height="150px") %>% withSpinner(type = 6, color = "#000CCC"),
                                 amChartsOutput("amcharthosp", width="100%", height="150px") %>% withSpinner(type = 6, color = "#000CCC"),
                          
                                 
                          ),
                        )
                      )
                      )

detallado_page <- tabPanel("Detallado",
                         h5("TextSummary")
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
                 pointFormat="Incidencia acumulada: <b>{point.value}*</b> <br> Número de casos: <b>{point.Casos}</b> <br> Porcentaje de casos: <b>{point.tasacasos}</b>%** <br> Número de fallecidos: <b>{point.Fallecidos}</b> <br> <br> Porcentaje de fallecidos: <b>{point.tasamuertos}</b>%**") %>%
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
  
  
  output$amchartucis <- renderAmCharts({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Porcentaje de camas UCI ocupadas en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Porcentaje de camas UCI ocupadas en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    
    amBullet(value = data$PerCamasUCI, steps = FALSE, limit = 20, val_color = "blue", rates=data.frame(name="Hola", min=0, max=100, color = "white")) %>% 
      amOptions( main = titulo)
  })
  
  output$amcharthosp <- renderAmCharts({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Porcentaje de camas de hospital ocupadas en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Porcentaje de camas de hospital ocupadas en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    
    amBullet(value = data$PercCamasCovid, steps = FALSE, limit = 20, val_color = "blue", rates=data.frame(name="Hola", min=0, max=100, color = "white")) %>% 
      amOptions( main = titulo)
  })
  
  
  output$highchartucispain <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Situación en las UCIS en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Situación en las UCIS en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '100%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = data$PerCamasUCI,
        dataLabels = list(
          y = -40,
          borderWidth = 0,
          useHTML = TRUE,
          format = paste('<p style="font-size:22px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:10px;margin-top:0px;margin-bottom:-10px">Camas UCI ocupadas</p>')#<p style="text-align:center;font-size:10px;margin-top:0px">', fecha, '</p>')
        )
      ) %>% 
      hc_size(height = 300)
  })
  
  output$highcharthopitalspain <- renderHighchart({
    data <- data.uci.spain %>% dplyr::filter(fecha == max(data.uci.spain$fecha))
    fecha <- unique(data$fecha)
    if(!is.null(input$hcClickedspain)){
      titulo <- paste0("Situación en las UCIS en ", input$hcClickedspain)
      data <- data %>% dplyr::filter(CCAA == input$hcClickedspain)
    } else {
      titulo <- "Situación en las UCIS en España"
      data <- data %>% dplyr::filter(CCAA == "España")
    }
    col_stops <- data.frame(
      q = c(0, 0.4, 0.7, 0.9),
      c = c('#55FF33', '#A3DF0D', '#DDDF0D', '#DF5353'),
      stringsAsFactors = FALSE
    )
    
    highchart() %>%
      hc_chart(type = "solidgauge") %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        center = list('50%', '70%'),
        size = '100%',
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>% 
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = data$PercCamasCovid,
        dataLabels = list(
          y = -40,
          borderWidth = 0,
          useHTML = TRUE,
          format = paste('<p style="font-size:22px;text-align:center;margin-bottom:0px;"> {point.y}% </p><p style="font-size:10px;margin-top:0px;margin-bottom:-10px">Camas de hospital ocupadas</p><br>')
        )
      ) %>% 
      hc_size(height = 300)
  })
}

shinyApp(ui, server)