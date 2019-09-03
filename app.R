library(shiny)

source("eur2coins.r")

### UI
ui <- navbarPage(title = "EUR 2", id = "EUR2",
                 tabPanel("identifizieren",
                          includeCSS(path = "style.css"),
                          fluidPage(
                            h1("EUR 2 Münzen - Identifikation"),
                            fluidRow(
                              column(3,
                                     h2("Suche"),
                                     h3("in ID"),
                                     wellPanel(
                                       fluidRow(
                                         column(9, textInput(inputId = "id", label = NULL)),
                                         column(3, actionButton(inputId = "id_reset", label = "X"))
                                       ),
                                       actionButton(inputId = "id_ueber", label = "ID übernehmen")
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
                                     h3("Gedenkmünzen"),
                                     tableOutput(outputId = "suche_g"),
                                     h3("Umlaufmünzen"),
                                     tableOutput(outputId = "suche_u")
                              )
                            )
                          )
                 ),
                 tabPanel("sammeln", value = "sammeln",
                          fluidPage(
                            h1("EUR 2 Münzen - Sammlung"),
                            fluidRow(
                              column(3,
                                     h2("Verwaltung"),
                                     h3("Suche"),
                                     wellPanel(
                                       fluidRow(
                                         column(9, textInput(inputId = "coll_id", label = NULL)),
                                         column(3, actionButton(inputId = "coll_id_reset", label = "X"))
                                       )
                                     ),
                                     h3("Änderung"),
                                     wellPanel(
                                       p(em("eur2collection.txt"), " geändert und gespeichert"),
                                       actionButton(inputId = "coll_aend", label = "OK")
                                     )
                              ),
                              column(9,
                                     h2("Suchergebnisse"),
                                     tableOutput(outputId = "coll_suche")
                              )
                            )
                          )
                 ),
                 tabPanel("zusammenfassen",
                          fluidPage(
                            h1("EUR 2 Münzen - Übersicht Sammelerfolg"),
                            fluidRow(
                              column(4,
                                     h2("Prägejahr"),
                                     tableOutput(outputId = "zsf_jahr")
                              ),
                              column(4,
                                     h2("Land"),
                                     tableOutput(outputId = "zsf_land")
                              ),
                              column(4,
                                     h2("Qualität"),
                                     tableOutput(outputId = "zsf_qual")
                              )
                            )
                          )
                 )
)

### Server
server <- function(input, output, session) {
  output$suche_g <- renderTable(spacing = "xs", {
    coins %>%
      filter(Münzart == "Gedenkmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>"),
             ID = paste0("<div class='mono'>", ID, "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen) %>% 
      select(-Ausgabe, -Münzart)
  }, sanitize.text.function = function(x) x)
  
  output$suche_u <- renderTable(spacing = "xs", {
    coins %>%
      filter(Münzart == "Umlaufmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>"),
             ID = paste0("<div class='mono'>", ID, "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen) %>% 
      select(-Ausgabe, -Münzart)
  }, sanitize.text.function = function(x) x)
  
  observeEvent(input$abb_reset, {
    updateTextInput(session, inputId = "abb", value = character(0))
  })
  
  observeEvent(input$id_reset, {
    updateTextInput(session, inputId = "id", value = character(0))
  })
  
  observeEvent(input$id_ueber, {
    updateTextInput(session, inputId = "coll_id", value = input$id)
    writeClipboard(input$id)
    updateTabsetPanel(session, inputId = "EUR2", selected = "sammeln")
    updateTextInput(session, inputId = "id", value = character(0))
  })
  
  output$coll_suche <- renderTable(spacing = "xs", {coll_tbl()}, sanitize.text.function = function(x) x)
    coll_tbl <- eventReactive(c(input$coll_id, input$coll_aend), {
      source("eur2collection.r")
      collection %>% 
        filter(str_detect(ID, input$coll_id)) %>% 
        arrange(ID) %>% 
        mutate(Zeilennummer = paste0("<div class='mono'>", Zeilennummer, "</div>"),
               Jahr = str_sub(ID, 1, 4),
               Land = toupper(str_sub(ID, 5, 6)),
               ID = paste0("<div class='mono'>", ID, "</div>"),
               Qualität = case_when(Qualität == 0 ~ "<div style='color: goldenrod;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                    Qualität == 1 ~ "<div style='color: goldenrod;'>(1)&nbsp;&#9733;&#9733;</div>",
                                    Qualität == 2 ~ "<div style='color: green;'>(2)&nbsp;&#10004;&#10004;</div>",
                                    Qualität == 3 ~ "<div style='color: green;'>(3)&nbsp;&#10004;</div>",
                                    TRUE ~ "<div style='color: red;'>FEHLER</div>"),
               Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>")) %>% 
        rename(Mzz = Münzzeichen,
               ZNr = Zeilennummer) %>% 
        select(ID, Qualität, Jahr, Land, Abbildung, Mzz, ZNr)
      }, ignoreNULL = FALSE)
  
  observeEvent(input$coll_id_reset, {
    updateTextInput(session, inputId = "coll_id", value = character(0))
  })
  
  output$zsf_jahr <- renderTable(spacing = "xs", {zsf_tbl_jahr()}, sanitize.text.function = function(x) x)
  zsf_tbl_jahr <- eventReactive(input$coll_aend, {
    left_join(coins %>% group_by(Jahr = str_sub(ID, 1, 4)) %>% count(),
              collection %>% group_by(Jahr = str_sub(ID, 1, 4)) %>% count(),
              by = "Jahr") %>%
      transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
                vH = Erfolg %>% parse(text = .) %>% eval() %>% "*"(100),
                Graph = rep(HTML("&#9608;"), as.integer(vH %/% 12.5)) %>% paste(collapse = "")) %>% 
      ungroup() %>% 
      mutate(Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }, ignoreNULL = FALSE)
  
  output$zsf_land <- renderTable(spacing = "xs", {zsf_tbl_land()}, sanitize.text.function = function(x) x)
  zsf_tbl_land <- eventReactive(input$coll_aend, {
    left_join(coins %>% group_by(Land = str_sub(ID, 5, 6)) %>% count(),
              collection %>% group_by(Land = str_sub(ID, 5, 6)) %>% count(),
              by = "Land") %>%
      transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
                vH = Erfolg %>% parse(text = .) %>% eval() %>% "*"(100),
                Graph = rep(HTML("&#9608;"), as.integer(vH %/% 12.5)) %>% paste(collapse = "")) %>% 
      ungroup() %>% 
      mutate(Land = paste0("<img src='https://www.crwflags.com/fotw/images/", substr(Land, 1, 1), "/", tolower(Land), ".gif', height='14', alt='", toupper(Land), "'/>"),
             Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }, ignoreNULL = FALSE)

  output$zsf_qual <- renderTable(spacing = "xs", {zsf_tbl_qual()}, sanitize.text.function = function(x) x)
  zsf_tbl_qual <- eventReactive(input$coll_aend, {
    collection %>%
      group_by(Qualität = Qualität %>% ordered(levels = 0:3,
                                    labels = c("<div style='color: goldenrod;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                               "<div style='color: goldenrod;'>(1)&nbsp;&#9733;&#9733;</div>",
                                               "<div style='color: green;'>(2)&nbsp;&#10004;&#10004;</div>",
                                               "<div style='color: green;'>(3)&nbsp;&#10004;</div>")),
               .drop = FALSE) %>%
      count() %>%
      transmute(Erfolg = paste0(coalesce(n, 0L), " / ", count(collection)),
                vH = Erfolg %>% parse(text = .) %>% eval() %>% "*"(100),
                Graph = rep(HTML("&#9608;"), as.integer(vH %/% 12.5)) %>% paste(collapse = "")) %>% 
      ungroup() %>% 
      mutate(Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }, ignoreNULL = FALSE)

  }

# Run the application 
shinyApp(ui = ui, server = server)
