library(shiny)

source("eur2coins.r")

ui <- fluidPage(
  includeCSS(path = "style.css"),
  h1("Identifikation von EUR 2 Münzen"),
  fluidRow(
    column(3, 
           textInput(inputId = "id", label = "Suche in ID")
           ),
    column(1,
           tags$label("Löschen"), br(),
           actionButton(inputId = "id_reset", label = "X")
    ),
    column(8,
           p("Suchstrings beziehen sich auf eine beliebige Übereinstimmung mit der eindeutigen ID. Der Aufbau der ID lautet: ", code("JJJJLLA00"),
             ", wobei ", code("JJJJ"), " fürs Prägejahr", ", ", code("LL"), " fürs Land", ", ", code("A"),
             " für die Münzart", " und ", code("0"), " für die fortlaufende Nummer stehen. Die Verwendung von ", code("."), " als Joker ist zulässig.")
           )
    ),
  fluidRow(
    column(3,
           textInput(inputId = "abb", label = "Suche in Abbildung")
           ),
    column(1,
           tags$label("Löschen"), br(),
           actionButton(inputId = "abb_reset", label = "X")
           )
    ),
  tableOutput(outputId = "auswahl")
)

server <- function(input, output, session) {
  output$auswahl <- renderTable({
      coins %>%
       filter(str_detect(ID, tolower(input$id)), str_detect(Abbildung, input$abb)) %>% 
       arrange(ID) %>%
       unnest() %>% 
       mutate(Ausgabe = as.character(Ausgabe)) %>% 
       rename(Mzz = Münzzeichen) %>% 
       select(-Ausgabe)
     })
  
  observeEvent(input$abb_reset, {
    updateTextInput(session, inputId = "abb", value = character(0))
    })

    observeEvent(input$id_reset, {
    updateTextInput(session, inputId = "id", value = character(0))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
