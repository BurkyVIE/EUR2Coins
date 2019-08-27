library(shiny)

ui <- fluidPage(
  includeCSS(path = "style.css"),
  h1("Identifikation von EUR 2 Münzen"),
  textInput(inputId = "text", label = "Suche in ID"),
  p("Suchstrings beziehen sich auf eine beliebige Übereinstimmung mit deneindeutigen IDs, deren Aufbau wie folgt aussieht '",strong("J J J J L L A 0 0"), "'. Dabei steht ", em("JJJJ fürs Prägejahr"), ", ", em("LL fürs Land"), ", ", em("A für die Münzart"), " und ", em("0 für die fortlaufende Nummer"), "."),
  tableOutput(outputId = "auswahl")
)

server <- function(input, output) {
  output$auswahl <- renderTable({
      coins %>%
       filter(str_detect(ID, tolower(input$text))) %>% 
       arrange(ID) %>%
       unnest() %>% 
       mutate(Ausgabe = as.character(Ausgabe)) %>% 
       rename(Mzz = Münzzeichen) %>% 
       select(-Ausgabe)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

