library(shiny)
library(shinythemes)

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
data.spain$Fecha <- as.Date(data.spain$Fecha, format = "%Y-%m-%d")


#Every page separetly
plot_page <- tabPanel("Plot",
                      h5("TextPlot")
                      )

summary_page <- tabPanel("Summary",
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
  titlePanel("Tabsets"),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  plot_page,
                  summary_page,
                  table_page
      )
      
    )
  
)
)

# Define server logic for random distribution app ----
server <- function(input, output) {

}

shinyApp(ui, server)