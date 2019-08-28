library(shiny)

source("eur2coins.r")

ui <- fluidPage(
  includeCSS(path = "style.css"),
  h1("Identifikation von EUR 2 Münzen"),
  fluidRow(
    column(3,
           h2("Suche"),
           h3("in ID"),
           wellPanel(
             fluidRow(
               column(9, textInput(inputId = "id", label = NULL)),
               column(3, actionButton(inputId = "id_reset", label = "X"))
               )
             ),
           p("Beliebige Übereinstimmung mit Feld", em("ID"), "; Aufbau ID: ", code("JJJJLLA00"),
             ", wobei ", code("JJJJ"), " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"),
             " = Münzart", " und ", code("0"), " = fortlaufende Nummer.", code("."), " als Joker ist zulässig."),
           h3("in Abbildung"),
           wellPanel(
             fluidRow(
               column(9, textInput(inputId = "abb", label = NULL)),
               column(3, actionButton(inputId = "abb_reset", label = "X"))
               )
             ),
           p("Beliebige Übereinstimmung mit Feld Abbildung. Groß-/ Kleinschreibung wird ignoriert.")
    ),
    column(9,
           h2("Suchergebnisse"),
           h3("Umlaufmünzen"),
           tableOutput(outputId = "auswahl_u"),
           h3("Gedenkmünzen"),
           tableOutput(outputId = "auswahl_g")
           )
    )
  )

server <- function(input, output, session) {
  output$auswahl_u <- renderTable({
      coins %>%
       filter(Münzart == "Umlaufmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
       arrange(ID) %>%
       unnest() %>% 
       mutate(Ausgabe = as.character(Ausgabe)) %>% 
       rename(Mzz = Münzzeichen) %>% 
       select(-Ausgabe, -Münzart)
     })
  
  output$auswahl_g <- renderTable({
    coins %>%
      filter(Münzart == "Gedenkmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
      arrange(ID) %>%
      unnest() %>% 
      mutate(Ausgabe = as.character(Ausgabe)) %>% 
      rename(Mzz = Münzzeichen) %>% 
      select(-Ausgabe, -Münzart)
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
