library(shiny)

#CSS style defined
css <- '.nav-tabs>li>a {
  font-family: "Lucida Sans", sans-serif;
  color: green;
}'

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