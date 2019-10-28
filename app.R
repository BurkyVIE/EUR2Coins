library(shiny)

source("eur2coins.r")
source("eur2collection.r")

# JS Funktion um Markierung zu kopieren
highlight <- '
function getSelectionText() {
var text = "";
if (window.getSelection) {
text = window.getSelection().toString();
} else if (document.selection) {
text = document.selection.createRange().text;
}
return text;
}

document.onmouseup = document.onkeyup = document.onselectionchange = function() {
var selection = getSelectionText();
Shiny.onInputChange("myselection", selection);
};
'

### UI
ui <- fluidPage(includeCSS(path = "style.css"),
                tabsetPanel(id = "EUR2",
                            tabPanel("identifizieren",
                                     tags$script(highlight),
                                     fluidPage(
                                       h1("EUR 2 Münzen"),
                                       fluidRow(
                                         column(width = 3,
                                                h2("Anzeige"),
                                                h3("Sammlung"),
                                                checkboxInput(inputId = "sammlung", label = "nur Sammlung"),
                                                h3("Münz ID"),
                                                fluidRow(
                                                  column(9, textInput(inputId = "id", label = NULL)),
                                                  column(3, offset = 0, actionButton(inputId = "id_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld", 
                                                  em("ID"), "; Aufbau ID: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"),
                                                  " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"),
                                                  " = Münzart", " und ", code("0"), " = fortlaufende Nummer.", code("."),
                                                  " als Joker ist zulässig.", HTML("</div>")),
                                                h3("Abbildung"),
                                                fluidRow(
                                                  column(9, textInput(inputId = "abb", label = NULL)),
                                                  column(3, actionButton(inputId = "abb_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld Abbildung. Groß-/ Kleinschreibung wird ignoriert.",
                                                  HTML("</div>")),
                                                h2("Anlage"),
                                                h3("Qualität"),
                                                fluidRow(
                                                  column(width = 3, actionButton(inputId = "q0", label = "0")),
                                                  column(width = 3, actionButton(inputId = "q1", label = "1")),
                                                  column(width = 3, actionButton(inputId = "q2", label = "2")),
                                                  column(width = 3, actionButton(inputId = "q3", label = "3"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Stelle Markierung ergänzt um Qualität ans Ende von ", em("eur2collection.txt"),
                                                  HTML("</div>")),
                                                h2("Änderung"),
                                                h3("eur2collection.txt"),
                                                actionButton(inputId = "aenderung", label = "Änderung durchgeführt"),
                                                p(HTML("<div class = 'beschr'>"), "Manuelle Änderung von ", em("eur2collection.txt"), ", zB Münztausch",
                                                  HTML("</div>"))
                                         ),
                                         column(width = 9,
                                                h2("Ergebnisse"),
                                                h3("Gedenkmünzen"),
                                                tableOutput(outputId = "suche_g"),
                                                h3("Umlaufmünzen"),
                                                tableOutput(outputId = "suche_u")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("ablegen",
                                     fluidPage(
                                       h1("EUR 2 Münzen - Ablage"),
                                       fluidRow(
                                         column(width = 3,
                                                h2("Auswahl"),
                                                h3("Box"),
                                                sliderInput(inputId = "box", label = NULL, min = 1, max = 2, step = 1, value = 1),
                                                h3("Tableau"),
                                                sliderInput(inputId = "tableau", label = NULL, min = 1, max = 6, step = 1,  value = 1),
                                                h2("Schnellwahl"),
                                                h3("Box, Tableau"),
                                                fluidRow(
                                                  column(9, textInput(inputId = "boxtab", label = NULL, value = pull(count(collection)))),
                                                  column(3, actionButton(inputId = "gehe_boxtab", label = ">>"))
                                                ),
                                                  p(HTML("<div class = 'beschr'>"), "Eingabe von Box und Tableau; Aufbau: ",
                                                    code("BT"), ", wobei ", code("B"), " = Box und", code("T"), " = Tableau.",
                                                    HTML("</div>")),
                                                h3("Zeilennummer"),
                                                fluidRow(
                                                  column(9, textInput(inputId = "znr", label = NULL, value = pull(count(collection)))),
                                                  column(3, actionButton(inputId = "gehe_znr", label = ">>"))
                                                )
                                         ),
                                         column(width = 9,
                                                h2("Ansicht"),
                                                h3(textOutput(outputId = "adresse")),
                                                tableOutput(outputId = "tableau")
                                         )
                                       )
                                     )
                            ),
                            tabPanel("zusammenfassen",
                                     fluidPage(
                                       h1("EUR 2 Münzen - Sammelerfolg"),
                                       fluidRow(
                                         column(width = 4,
                                                h2("Prägejahr"),
                                                tableOutput(outputId = "zsf_jahr")
                                         ),
                                         column(width = 4,
                                                h2("Land"),
                                                tableOutput(outputId = "zsf_land")
                                         ),
                                         column(width = 4,
                                                h2("Qualität"),
                                                tableOutput(outputId = "zsf_qual")
                                         )
                                       )
                                     )
                            )#,
                            #tabPanel("Test",
                            #         fluidPage(
                            #           h1("Test")
                            #         )
                            #)
                )
)

### Server
server <- function(input, output, session) {
  
  observeEvent(input$id_reset, {
    updateTextInput(session, inputId = "id", value = character(0))
  })
  
  observeEvent(input$abb_reset, {
    updateTextInput(session, inputId = "abb", value = character(0))
  })
  
  observeEvent(input$q0, {
    tmp <- paste0(input$myselection, "-0")
    write(tmp, file = "eur2collection.txt", append = TRUE)
  })
  observeEvent(input$q1, {
    tmp <- paste0(input$myselection, "-1")
    write(tmp, file = "eur2collection.txt", append = TRUE)
  })
  
  observeEvent(input$q2, {
    tmp <- paste0(input$myselection, "-2")
    write(tmp, file = "eur2collection.txt", append = TRUE)
  })
  
  observeEvent(input$q3, {
    tmp <- paste0(input$myselection, "-3")
    write(tmp, file = "eur2collection.txt", append = TRUE)
  })
  
  
  observeEvent(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    source("eur2collection.r")
  })
  
  output$suche_g <- renderTable(spacing = "xs", align = c("rllllrlr"), {tbl_g()}, sanitize.text.function = function(x) x)
  tbl_g <- eventReactive(c(input$sammlung, input$id, input$abb, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    coins %>%
      left_join(collection %>% 
                  select(ID, Qualität, Ablage),
                by = "ID") %>%
      filter((!is.na(Ablage) | !input$sammlung), Münzart == "Gedenkmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>"),
             ID = paste0("<div class='mono'>", ID, "</div>"),
             Qualität = case_when(is.na(Qualität) ~ "",
                                  Qualität == 0 ~ "<div style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                  Qualität == 1 ~ "<div style='color: #958746;'>(1)&nbsp;&#9733;&#9733;</div>",
                                  Qualität == 2 ~ "<div style='color: #51696c;'>(2)&nbsp;&#10004;&#10004;</div>",
                                  Qualität == 3 ~ "<div style='color: #0e4c92;'>(3)&nbsp;&#10004;</div>",
                                  TRUE ~ "<div style='color: red;'>FEHLER</div>"),
             Ablage = paste0("<div class='mono'>", replace_na(Ablage, " "), "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen,
             'Münz ID' = ID) %>% 
      select(-Ausgabe, -Münzart)
  })
  
  output$suche_u <- renderTable(spacing = "xs", align = c("rllllrlr"), {tbl_u()}, sanitize.text.function = function(x) x)
  tbl_u <- eventReactive(c(input$sammlung, input$id, input$abb, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    coins %>%
      left_join(collection %>% 
                  select(ID, Qualität, Ablage),
                by = "ID") %>%
      filter((!is.na(Ablage) | !input$sammlung), Münzart == "Umlaufmünze", str_detect(ID, tolower(input$id)), str_detect(tolower(Abbildung), tolower(input$abb))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>"),
             ID = paste0("<div class='mono'>", ID, "</div>"),
             Qualität = case_when(is.na(Qualität) ~ "",
                                  Qualität == 0 ~ "<div style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                  Qualität == 1 ~ "<div style='color: #958746;'>(1)&nbsp;&#9733;&#9733;</div>",
                                  Qualität == 2 ~ "<div style='color: #51696c;'>(2)&nbsp;&#10004;&#10004;</div>",
                                  Qualität == 3 ~ "<div style='color: #0e4c92;'>(3)&nbsp;&#10004;</div>",
                                  TRUE ~ "<div style='color: red;'>FEHLER</div>"),
             Ablage = paste0("<div class='mono'>", replace_na(Ablage, " "), "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen,
             'Münz ID' = ID) %>% 
      select(-Ausgabe, -Münzart)
  })
  
  output$zsf_jahr <- renderTable(spacing = "xs", align = c("rrrl"), {zsf_tbl_jahr()}, sanitize.text.function = function(x) x)
  zsf_tbl_jahr <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    left_join(coins %>% group_by(Jahr = str_sub(ID, 1, 4)) %>% count(),
              collection %>% group_by(Jahr = str_sub(ID, 1, 4)) %>% count(),
              by = "Jahr") %>%
      transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
                vH = Erfolg %>% parse(text = .) %>% eval() %>% "*"(100),
                Graph = rep(HTML("&#9608;"), as.integer(vH %/% 12.5)) %>% paste(collapse = "")) %>% 
      ungroup() %>% 
      mutate(Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }, ignoreNULL = FALSE)
  
  output$zsf_land <- renderTable(spacing = "xs", align = c("lrrl"), {zsf_tbl_land()}, sanitize.text.function = function(x) x)
  zsf_tbl_land <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
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
  
  output$zsf_qual <- renderTable(spacing = "xs", align = c("lrr"), {zsf_tbl_qual()}, sanitize.text.function = function(x) x)
  zsf_tbl_qual <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    collection %>%
      group_by(Qualität = Qualität %>% ordered(levels = 0:3,
                                               labels = c("<div style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                                          "<div style='color: #958746;'>(1)&nbsp;&#9733;&#9733;</div>",
                                                          "<div style='color: #51696c;'>(2)&nbsp;&#10004;&#10004;</div>",
                                                          "<div style='color: #0e4c92;'>(3)&nbsp;&#10004;</div>")),
               .drop = FALSE) %>%
      count() %>%
      transmute(Anzahl = n,
                Anteil = Anzahl / dim(collection)[1] * 100)
  }, ignoreNULL = FALSE)
  
  observeEvent(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    updateSliderInput(session, inputId = "box", value = (pull(count(collection)) -1) %/% 144 + 1)
    updateSliderInput(session, inputId = "tableau", value = (pull(count(collection)) -1) %/% 24 %% 6 + 1)
    updateTextInput(session, inputId = "boxtab", value = paste0(tail(collection$Box, 1), tail(collection$Tableau, 1)))
    updateTextInput(session, inputId = "znr", value = tail(collection$Zeilennummer, 1))
  })
  
  observeEvent(input$gehe_boxtab, {
    updateSliderInput(session, inputId = "box", value = as.integer(str_sub(input$boxtab, 1, 1)))
    updateSliderInput(session, inputId = "tableau", value = as.integer(str_sub(input$boxtab, 2, 2)))
    updateTextInput(session, inputId = "boxtab", value = paste0(tail(collection$Box, 1), tail(collection$Tableau, 1)))
    updateTextInput(session, inputId = "znr", value = tail(collection$Zeilennummer, 1))
  })
  
  observeEvent(input$gehe_znr, {
    updateSliderInput(session, inputId = "box", value = as.integer(input$znr) %/% 145 + 1)
    updateSliderInput(session, inputId = "tableau", value = (as.integer(input$znr) - 1) %% 144 %/% 24 + 1)
    updateTextInput(session, inputId = "boxtab", value = paste0(tail(collection$Box, 1), tail(collection$Tableau, 1)))
    updateTextInput(session, inputId = "znr", value = tail(collection$Zeilennummer, 1))
  })

    output$adresse <- renderText({
    paste0(input$box, input$tableau, "11x", sprintf("%04d", (input$box - 1) * 144 + (input$tableau - 1) * 24 + 1),
           " bis ",
           input$box, input$tableau, "64x", sprintf("%04d", (input$box - 1) * 144 + input$tableau * 24))
  })
  
  output$tableau <- renderTable({erst_tab()}, bordered = T, spacing = "l", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  erst_tab <- eventReactive(c(input$box, input$tableau, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    collection %>% 
      filter(Zeilennummer >= (input$box - 1) * 144 + (input$tableau - 1) * 24 + 1,
             Zeilennummer <= (input$box - 1) * 144 + input$tableau * 24) %>% 
      arrange(Zeilennummer) %>%
      mutate(Qualität = case_when(is.na(Qualität) ~ "",
                                  Qualität == 0 ~ "<div style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
                                  Qualität == 1 ~ "<div style='color: #958746;'>(1)&nbsp;&#9733;&#9733;</div>",
                                  Qualität == 2 ~ "<div style='color: #51696c;'>(2)&nbsp;&#10004;&#10004;</div>",
                                  Qualität == 3 ~ "<div style='color: #0e4c92;'>(3)&nbsp;&#10004;</div>",
                                  TRUE ~ "<div style='color: red;'>FEHLER</div>"),
             ID = paste0("<div class='mono'>", str_sub(ID, 1, 4), " ", toupper(str_sub(ID, 5, 6)), "<br>", toupper(str_sub(ID, 7, 7)), " ", str_sub(ID, 8, 9), "<br></div>", Qualität)) %>% 
      pull(ID) -> tmp
    if(length(tmp) < 24) tmp <- c(tmp, rep("", 24 - length(tmp)))
    matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
           dimnames = list(
             paste0("<b>..", 1:4, "<br>x<br>",
                    sprintf("%04d", (input$box - 1) * 144 + (input$tableau - 1) * 24 + (0:3) * 6, "</b>")
                    ),
             paste0(input$box, input$tableau, 1:6, "..<br>x<br>+", 1:6)
             )
           )
  }, ignoreNULL = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
