# GLOBAL ----
## Libraries ----
library(shiny)

## Externe Daten ----
source("eur2coins.r")
source("eur2collection.r")

## JS Funktion um Markierung zu kopieren ----
highlight <- 'function getSelectionText() {
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
};'

## Funktion zum Formatieren Qualität ----
form_quali <- function(x) {
  case_when(is.na(x) ~ "",
            x == 0 ~ "<div style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</div>",
            x == 1 ~ "<div style='color: #958746;'>(1)&nbsp;&#9733;&#9733;</div>",
            x == 2 ~ "<div style='color: #51696c;'>(2)&nbsp;&#10004;&#10004;</div>",
            x == 3 ~ "<div style='color: #0e4c92;'>(3)&nbsp;&#10004;</div>",
            TRUE ~ "<div style='color: red;'>FEHLER</div>")
}

## Funktion zur Darstellung Land ----
form_land <- function(txt) {
  txt <- tolower(txt) # jedenfalls Kleinbuchstaben
  paste0("<img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif', height='14', alt='", toupper(txt), "'/>(", toupper(txt), ")")
}

## Funktion zur Darstellung Statistik ----
form_stat <- function(val, von, bis) {
  left_join(coins |> group_by(Grp = str_sub(ID, von, bis)) |> count(),
            collection |> group_by(Grp = str_sub(ID, von, bis)) |> count(),
            by = "Grp") |> 
    transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
              vH = Erfolg |> (\(x) eval(parse(text = x)) * 100)(),
              Graph = c(rep(HTML("&#9608;"), as.integer(vH %/% 10)), if((vH %% 10) >= 5) HTML("&#9612;")) |>  paste(collapse = "")) |> 
    # ungroup() |> 
    rename(!!val := Grp) |> 
    mutate(Graph = paste0("<div class='bar'>", Graph, "</div>"))
}

## Funktion zum Erzeugen der Serien-Darstellung ----
displ_serie <- function(df, variation) {
  tmp <- left_join(df |> filter(!is.na(Amtsblatt)),
                   coins |> select(Amtsblatt, ID, Münzzeichen), by = 'Amtsblatt') |> 
    left_join(collection %>% select(ID, Qualität, Ablage), by = 'ID') |> 
    mutate(Jahr = str_sub(ID, 1, 4),
           Ablage = coalesce(Ablage, ""),
           Qualität = form_quali(Qualität),
           ID = paste0(Qualität, "<div class='mono'>", Ablage, "<br></div>")) |>  
    select(Jahr, Münzzeichen, Beschreibung, ID)
  
  # Variation Standard
  if(variation == "std") {
  res <- cbind(paste0("<b>", pull(tmp, Jahr), "</b>"),
               pull(tmp, Beschreibung), 
               pull(tmp, Münzzeichen),
               pull(tmp, ID)) |>  
    matrix(ncol = 4, dimnames = list(NULL, c("Jahr", "Bezeichnung", "Mzz", " ")))
  }
  
  # Variation Deutschland
  if(variation == "de") {
  res <-     cbind(paste0("<b>", tmp |> filter(Münzzeichen == "A") |> pull(Jahr), "</b>"),
                   tmp |> filter(Münzzeichen == "A") |> pull(Beschreibung),
                   matrix(tmp |> pull(ID), ncol = 5, byrow = TRUE)) |> 
    matrix(ncol = 7, dimnames = list(NULL, c("Jahr", "Bezeichnung", "A (Berlin)", "D (München)", "F (Stuttgart)", "G (Karlsruhe)", "J (Hamburg)")))  
  }

  return(res)
}



# UI (User Interface) ----
ui <- fluidPage(includeCSS(path = "style.css"),
                tabsetPanel(id = "EUR2",
                            tabPanel("Identifikation",
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
                                                  column(9, textInput(inputId = "id", value = "", label = NULL)),
                                                  column(3, offset = 0, actionButton(inputId = "id_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld", 
                                                  em("ID"), "; Aufbau ID: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"),
                                                  " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"),
                                                  " = Münzart", " und ", code("0"), " = fortlaufende Nummer.", code("."),
                                                  " als Joker ist zulässig.", HTML('</div>')),
                                                h3("Abbildung"),
                                                fluidRow(
                                                  column(9, textInput(inputId = "abb", value = "", label = NULL)),
                                                  column(3, actionButton(inputId = "abb_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld Abbildung. Groß-/ Kleinschreibung wird ignoriert.",
                                                  HTML('</div>')),
                                                h2("Anlage"),
                                                h3("Qualität"),
                                                fluidRow(
                                                  column(width = 3, actionButton(inputId = "q0", label = "0")),
                                                  column(width = 3, actionButton(inputId = "q1", label = "1")),
                                                  column(width = 3, actionButton(inputId = "q2", label = "2")),
                                                  column(width = 3, actionButton(inputId = "q3", label = "3"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Stelle Markierung ergänzt um Qualität ans Ende von ",em("eur2collection.txt"),
                                                  HTML('</div>')),
                                                h2("Änderung"),
                                                h3("eur2collection.txt"),
                                                actionButton(inputId = "aenderung", label = "Änderung durchgeführt"),
                                                p(HTML("<div class = 'beschr'>"), "Manuelle Änderung von ", em("eur2collection.txt"), ", zB Münztausch",
                                                  HTML('</div>'))
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
                            tabPanel("Ablage",
                                     fluidPage(
                                       h1("EUR 2 Münzen - Ablage"),
                                       fluidRow(
                                         column(width = 3,
                                                h2("Auswahl"),
                                                h3("Box"),
                                                sliderInput(inputId = "box", label = NULL, min = 1, max = 3, step = 1, value = 1),
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
                                                  HTML('</div>')),
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
                            tabPanel("Zusammenfassung",
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
                            ),
                            tabPanel("Serien",
                                     tabsetPanel(id = "Serien",
                                                 tabPanel("DE",
                                                          fluidPage(
                                                            h1("Deutschland"),
                                                            h2("Bundesländerserie I (2006-2022)"),
                                                            tableOutput(outputId = "debl1_tab"),
                                                            h2("Bundesländerserie II (2023-2038)"),
                                                            tableOutput(outputId = "debl2_tab")
                                                          )
                                                 ),
                                                 tabPanel("ES",
                                                          fluidPage(
                                                            h1("Spanien"),
                                                            h2("UNESCO-Welterbestätten"),
                                                            tableOutput(outputId = "esun_tab")
                                                          )
                                                 ),
                                                 tabPanel("FR",
                                                          fluidPage(
                                                            h1("Frankreich"),
                                                            h2("Olympische Sommerspiele 2024"),
                                                            tableOutput(outputId = "fros_tab")
                                                          )
                                                 ),tabPanel("LT",
                                                          fluidPage(
                                                            h1("Litauen"),
                                                            h2("Ethnographische Regionen"),
                                                            tableOutput(outputId = "lter_tab")
                                                          )
                                                 ),
                                                 tabPanel("LU",
                                                          fluidPage(
                                                            h1("Luxemburg"),
                                                            h2("Dynastieserie"),
                                                            tableOutput(outputId = "ludy_tab")
                                                          )
                                                 ),
                                                 tabPanel("LV",
                                                          fluidPage(
                                                            h1("Lettland"),
                                                            h2("Historische Regionen"),
                                                            tableOutput(outputId = "lvhr_tab")
                                                          )
                                                 ),
                                                 tabPanel("MT",
                                                          fluidPage(
                                                            h1("Malta"),
                                                            h2("Verfassungsgeschichte"),
                                                            tableOutput(outputId = "mtvg_tab"),
                                                            h2("Prähistorische Stätten"),
                                                            tableOutput(outputId = "mtps_tab"),
                                                            h2("Von Kindern mit Solidarität"),
                                                            tableOutput(outputId = "mtks_tab")
                                                          )
                                                 )#,
                                                 #tabPanel("...")
                                     )
                            )#,
                            #tabPanel("Test",
                            #         fluidPage(
                            #           h1("Test")
                            #         )
                            #)
                )
)


# Server ---
server <- function(input, output, session) {

  ## Reset Buttons ----  
  observeEvent(input$id_reset, {
    updateTextInput(session, inputId = "id", value = "")
  })
  
  observeEvent(input$abb_reset, {
    updateTextInput(session, inputId = "abb", value = "")
  })
  
  ## Bewertungs Buttons ----
  observeEvent(input$q0, {
    tmp <- paste0(input$myselection, "-0")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1.5)
  })
  
  observeEvent(input$q1, {
    tmp <- paste0(input$myselection, "-1")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1.5)
  })
  
  observeEvent(input$q2, {
    tmp <- paste0(input$myselection, "-2")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1.5)
  })
  
  observeEvent(input$q3, {
    tmp <- paste0(input$myselection, "-3")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1.5)
  })
  
  ## Reload ----
  observeEvent(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    source("eur2collection.r")
  })
  
  ## Ausgabe Gedenkmünzen ----
  output$suche_g <- renderTable(spacing = "xs", align = c("rllllrlr"), {tbl_g()}, sanitize.text.function = function(x) x)
  tbl_g <- eventReactive(c(input$sammlung, input$id, input$abb, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    coins %>%
      left_join(collection %>% 
                  select(ID, Qualität, Ablage),
                by = "ID") %>%
      filter((!is.na(Ablage) | !input$sammlung), Münzart == "Gedenkmünze", grepl(tolower(input$id), ID), grepl(tolower(input$abb), tolower(Abbildung))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             # Land = paste0("<img src='https://www.crwflags.com/fotw/images/", tolower(substr(Land, 1, 1)), "/", tolower(Land), ".gif', height='14', alt='", Land, "'/>"),
             Land = form_land(Land),
             ID = paste0("<div class='mono'>", ID, "</div>"),
             Qualität = form_quali(Qualität),
             Ablage = paste0("<div class='mono'>", replace_na(Ablage, " "), "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen,
             'Münz ID' = ID) %>% 
      select(-Ausgabe, -Münzart)
  })
  ## Ausgabe Umlaufmünzen ----
  output$suche_u <- renderTable(spacing = "xs", align = c("rllllrlr"), {tbl_u()}, sanitize.text.function = function(x) x)
  tbl_u <- eventReactive(c(input$sammlung, input$id, input$abb, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    coins %>%
      left_join(collection %>% 
                  select(ID, Qualität, Ablage),
                by = "ID") %>%
      filter((!is.na(Ablage) | !input$sammlung), Münzart == "Umlaufmünze", grepl(tolower(input$id), ID), grepl(tolower(input$abb), tolower(Abbildung))) %>% 
      arrange(ID) %>%
      mutate(Amtsblatt = paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/HTML/?uri=CELEX:", Amtsblatt, "', target = '_blank'>", Amtsblatt, "</a>"),
             Land = form_land(Land),
             ID = paste0("<div class='mono'>", ID, "</div>"),
             Qualität = form_quali(Qualität),
             Ablage = paste0("<div class='mono'>", replace_na(Ablage, " "), "</div>")) %>% 
      rename(Jahr = Prägejahr,
             Mzz = Münzzeichen,
             'Münz ID' = ID) %>% 
      select(-Ausgabe, -Münzart)
  })
  
  ## Ausgabe Zusammenfassung Jahr ----
  output$zsf_jahr <- renderTable(spacing = "xs", align = c("rrrl"), {zsf_tbl_jahr()}, sanitize.text.function = function(x) x)
  zsf_tbl_jahr <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    form_stat("Jahr", 1, 4)
  }, ignoreNULL = FALSE)
  
  ## Ausgabe Zusammenfassung Land ----
  output$zsf_land <- renderTable(spacing = "xs", align = c("lrrl"), {zsf_tbl_land()}, sanitize.text.function = function(x) x)
  zsf_tbl_land <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    form_stat("Land", 5, 6) |> 
      mutate(Land = form_land(Land))
  }, ignoreNULL = FALSE)
  
  ## Ausgabe Zusammenfassung Qualität ----
  output$zsf_qual <- renderTable(spacing = "xs", align = c("lrr"), {zsf_tbl_qual()}, sanitize.text.function = function(x) x)
  zsf_tbl_qual <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    collection %>%
      group_by(Qualität = Qualität %>% ordered(levels = 0:3, labels = form_quali(0:3)), .drop = FALSE) %>%
      count() %>%
      transmute(Anzahl = n,
                Anteil = Anzahl / dim(collection)[1] * 100)
  }, ignoreNULL = FALSE)
  
  ## Auswahl Ablage ----
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
  
  ## Ausgabe Ablage ----
  output$tableau <- renderTable({erst_tab()}, bordered = T, spacing = "l", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  erst_tab <- eventReactive(c(input$box, input$tableau, input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    collection %>% 
      filter(Zeilennummer >= (input$box - 1) * 144 + (input$tableau - 1) * 24 + 1,
             Zeilennummer <= (input$box - 1) * 144 + input$tableau * 24) %>% 
      arrange(Zeilennummer) %>%
      mutate(Qualität = form_quali(Qualität),
             ID = paste0("<div class='mono'>", str_sub(ID, 1, 4), " ", toupper(str_sub(ID, 5, 6)), "<br>", toupper(str_sub(ID, 7, 7)), " ", str_sub(ID, 8, 9), "<br></div>", Qualität)) %>% 
      pull(ID) -> tmp
    if(length(tmp) < 24) tmp <- c(tmp, rep("", 24 - length(tmp)))
    matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
           dimnames = list(
             paste0("<b>..", 1:4, "<br>x<br>",
                    sprintf("%04d", (input$box - 1) * 144 + (input$tableau - 1) * 24 + (0:3) * 6), "</b>"
             ),
             paste0(input$box, input$tableau, 1:6, "..<br>x<br>+", 1:6)
           )
    )
  }, ignoreNULL = FALSE)
  
  ## Darstellung Serien ----
  ### Deutschland - Bundesländerserie I ----
  output$debl1_tab <- renderTable({debl1_tab()}, bordered = T, spacing = "l", align = "clccccc", rownames = FALSE, sanitize.text.function = function(x) x)
  debl1_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    debl1 <- tribble(~Amtsblatt, ~Beschreibung,
                     'C2006/033/04', '<b>Schleswig-Holstein</b><br>(Lübecker Holstentor)',
                     'C2007/076/02', '<b>Mecklenburg-Vorpommern</b><br>(Schloss Schwerin)',
                     'C2008/013/02', '<b>Hamburg</b><br>(Hamburger Sankt-Michaelis-Kirche)',
                     'C2009/031/06', '<b>Saarland</b><br>(Saarbrücker Ludwigskirche)',
                     'C2010/012/05', '<b>Bremen</b><br>(Bremer Roland und Rathaus)',
                     'C2011/024/04', '<b>Nordrhein-Westfalen</b><br>(Kölner Dom)',
                     'C2012/010/02', '<b>Bayern</b><br>(Schloss Neuschwanstein)',
                     'C2013/379/08', '<b>Baden-Württemberg</b><br>(Kloster Maulbronn)',
                     'C2014/417/04', '<b>Niedersachsen</b><br>(St.-Michaelis-Kirche zu Hildesheim)',
                     'C2015/143/05', '<b>Hessen</b><br>(Frankfurter Paulskirche)',
                     'C2015/428/04', '<b>Sachsen</b><br>(Dresdner Zwinger)',
                     'C2017/023/04', '<b>Rheinland-Pfalz</b><br>(Porta Nigra)',
                     'C2018/400/05', '<b>Berlin</b><br>(Schloss Charlottenburg)',
                     'C2018/466/08', '<b>Sitz des Bundesrates</b><br>(Preußisches Herrenhaus, Sitz des Bundesrates)',
                     'C2020/049/11', '<b>Brandenburg</b><br>(Schloss Sanssouci)',
                     'C2021/020/04', '<b>Sachsen-Anhalt</b><br>(Magdeburger Dom)',
                     NA, '<b>Thüringen</b><br>(Wartburg)')

    displ_serie(debl1, "de")
    }, ignoreNULL = FALSE)
  
  ### Deutschland - Bundesländerserie II ----
  # output$debl2_tab <- renderTable({debl2_tab()}, bordered = T, spacing = "l", align = "clccccc", rownames = FALSE, sanitize.text.function = function(x) x)
  # debl2_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    # debl2 <- tribble(~Amtsblatt, ~Beschreibung,
    #                  NA, '<b>Hamburg</b><br>(Elbphilharmonie)',
    #                  NA, '<b>Mecklenburg-Vorpommern</b><br>(Königsstuhl)',
    #                  NA, '<b>Saarland</b><br>(Saarschleife)',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()',
    #                  NA, '<b>Unbekannt</b><br>()')
  #   
  #   displ_serie(debl1, "de")
  #   }, ignoreNULL = FALSE)

    ### Frankreich - Olympische Sommerspiele 2024 ----
  output$fros_tab <- renderTable({fros_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  fros_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    fros <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2021/470/07', '<b>Die sprintende Marianne</b>',
                    NA, '<b>Der Genius und das Diskuswerfen - Arc de Triomphe</b>',
                    NA, '<b>Die Säerin und der Faustkampf – Pont Neuf</b>',
                    NA, '<b>Herkules und der Ringkampf</b>')

    displ_serie(fros, "std")
    }, ignoreNULL = FALSE)
  
  ### Litauen - Ethnografische Regionen ----
  output$lter_tab <- renderTable({lter_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  lter_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    lter <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2019/351/10', '<b>Žemaitija</b><br>(Niederlittauen)',
                    'C2020/053/04', '<b>Aukschtaiten</b><br>(Oberlitauen)',
                    'C2021/473/05', '<b>Dzukija</b><br>(Mittellitauen)',
                    'C2022/484/25', '<b>Suvalkija</b><br>(Sudauen)',
                    NA, '<b>Mažoji Lietuva</b><br>(Kleinlitauen)')

    displ_serie(lter, "std")
    }, ignoreNULL = FALSE)
  
  ### Luxemburg - Dynastieserie ----
  output$ludy_tab <- renderTable({ludy_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  ludy_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    ludy <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2004/243/05', '<b>Monogramm Großherzog Henris</b>',
                    'C2005/011/03', '<b>50. Geburtstag und 5. Jahrestag der Thronbesteigung Großherzog Henris, 100. Todestag Großherzog Adolphs</b>',
                    'C2006/020/10', '<b>25. Geburtstag Erbgroßherzog Guillaumes</b>',
                    'C2007/053/02', '<b>Großherzoglicher Palast</b>',
                    'C2008/021/09', '<b>Schloss von Berg</b>',
                    'C2009/005/02', '<b>90. Jahrestag der Thronbesteigung Großherzogin Charlottes</b>',
                    'C2009/311/06', '<b>Wappen Großherzog Henris</b>',
                    'C2010/349/03', '<b>50. Jahrestag der Ernennung ihres Sohnes Jean zum Statthalter durch Großherzogin Charlotte</b>',
                    'C2011/373/06', '<b>100. Todestag Großherzog Wilhelms IV.</b>',
                    'C2013/021/05', '<b>Hochzeit Erbgroßherzog Guillaumes mit Gräfin Stéphanie de Lannoy</b>',
                    'C2013/219/06', '<b>Nationalhymne des Großherzogtums Luxemburg</b>',
                    'C2014/020/06', '<b>175 Jahre Unabhängigkeit des Großherzogtums Luxemburg</b>',
                    'C2014/262/05', '<b>50. Jahrestag der Thronbesteigung Großherzog Jeans</b>',
                    'C2015/086/03', '<b>15. Jahrestag der Thronbesteigung Großherzog Henris</b>',
                    'C2015/232/05', '<b>125. Jahrestag der Luxemburger Dynastie Nassau-Weilburg</b>',
                    'C2016/028/04', '<b>50-jähriges Bestehen der Großherzogin-Charlotte-Brücke</b>',
                    'C2017/023/07', '<b>50. Jahrestag der Gründung der Luxemburger Freiwilligenarmee</b>',
                    'C2017/320/04', '<b>200. Geburtstag Großherzog Wilhelms III.</b>',
                    'C2017/438/10', '<b>150 Jahre Luxemburgische Verfassung</b>',
                    'C2018/305/06', '<b>175. Todestag Großherzog Wilhelms I.</b>',
                    'C2018/466/11', '<b>100. Jahrestag der Thronbesteigung Großherzogin Charlottes</b>',
                    'C2019/352/13', '<b>100. Jahrestag der Einführung des allgemeinen Wahlrechts</b>',
                    'C2020/049/13', '<b>200. Geburtstag Heinrichs von Oranien-Nassau</b>',
                    'C2020/381/03', '<b>Geburt von Prinz Charles von Luxemburg</b>',
                    'C2020/444/04', '<b>100. Geburtstag Großherzog Jeans</b>',
                    'C2021/020/06', '<b>40. Hochzeitstag Großherzog Henris und Großherzogin Maria Teresas</b>',
                    'C2022/145/10', '<b>50. Jahrestag der Flagge Luxemburgs</b>',
                    'C2022/484/21', '<b>10. Hochzeitstag von Erbgroßherzog Guillaume und Erbgroßherzogin Stéphanie</b>',
                    NA, '<b>175. Jahrestag der Abgeordnetenkammer und der ersten Verfassung (1848)</b>',
                    NA, '<b>25. Jahrestag der Aufnahme von Großherzog Henri als Mitglied des Internationalen Olympischen Komitees</b>')
    
    displ_serie(ludy, "std")
    }, ignoreNULL = FALSE)
  
  ### Lettland - Historische Regionen ----
  output$lvhr_tab <- renderTable({lvhr_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  lvhr_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    lvhr <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2016/146/07', '<b>Vidzeme</b><br>(Zentral-Livland)',
                    'C2017/066/02', '<b>Kurzemen</b><br>(Kurland)',
                    'C2017/066/03', '<b>Latgale</b><br>(Lettgallen)',
                    'C2018/234/03', '<b>Zemgale</b><br>(Semgallen)')

    displ_serie(lvhr, "std")
    }, ignoreNULL = FALSE)
  
  ### Malta - Verfassungsgeschichte ----
  output$mtvg_tab <- renderTable({mtvg_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtvg_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtvg <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2011/299/08', '<b>Wahl der ersten Abgeordneten 1849</b>',
                    'C2012/375/06', '<b>Mehrheitswahlrecht 1887</b>',
                    'C2013/379/09', '<b>Einrichtung der Selbstverwaltung 1921</b>',
                    'C2014/383/05', '<b>Unabhängigkeit von Großbritannien 1964</b>',
                    'C2015/150/03', '<b>Ausrufung der Republik Malta 1974</b>')

    displ_serie(mtvg, "std")
    }, ignoreNULL = FALSE)
  
  ### Malta - Prähistorische Stätten ----
  output$mtps_tab <- renderTable({mtps_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtps_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtps <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2016/281/10', '<b>Tempel von Ggantija</b>',
                    'C2017/111/10', '<b>Tempel von Hagar Qim</b>',
                    'C2018/174/08', '<b>Tempel von Mnajdra</b>',
                    'C2019/352/15', '<b>Tempel von Ta’ Hagrat</b>',
                    'C2020/166/02', '<b>Tempel von Skorba</b>',
                    'C2021/473/08', '<b>Tempel von Tarxien</b>',
                    'C2022/484/22', '<b>Ħal-Saflieni-Hypogäum</b>')

    displ_serie(mtps, "std")
    }, ignoreNULL = FALSE)
  
  ### Malta - Von Kindern mit Solidarität ----
  output$mtks_tab <- renderTable({mtks_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtks_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtks <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2016/396/03', '<b>Solidarität durch Liebe</b>',
                    'C2017/386/03', '<b>Frieden</b>',
                    'C2018/401/07', '<b>Kulturelles Erbe</b>',
                    'C2019/352/16', '<b>Natur / Umwelt</b>',
                    'C2020/380/04', '<b>Kinderspiele</b>')

    displ_serie(mtks, "std")
    }, ignoreNULL = FALSE)
  
  ### Spanien - UNESCO Welterbestätten ----
  output$esun_tab <- renderTable({esun_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  esun_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    esun <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2010/047/07', '<b>Altstadt von Córdoba</b><br>(Innenraum der Mezquita de Córdoba)',
                    'C2011/050/02', '<b>Alhambra, Generalife und Albaicín in Granada</b><br>(Löwenhof der Alhambra)',
                    'C2012/057/03', '<b>Kathedrale von Burgos</b><br>(Obere Westfassade und Vierungsturm)',
                    'C2013/050/04', '<b>Königlicher Sitz Sankt Laurentius von El Escorial</b><br>(Südansicht der Klosterresidenz auf Glockentürme und Kirchenkuppel)',
                    'C2014/051/05', '<b>Arbeiten von Antoni Gaudí</b><br>(Park Güell in Barcelona)',
                    'C2014/397/04', '<b>Höhle von Altamira / Paläolithische Höhlenmalerei im Norden Spaniens</b><br>(Wisent, Wandmalerei in der Höhle von Altamira)',
                    'C2015/425/10', '<b>Altstadt und Aquädukt von Segovia</b><br>(Aquädukt von Segovia)',
                    'C2016/236/06', '<b>Monumente von Oviedo und des Fürstentums Asturien</b><br>(Santa María del Naranco)',
                    'C2018/014/04', '<b>Altstadt von Santiago de Compostela</b><br>(Detail der Westfassade der Kathedrale von Santiago de Compostela)',
                    'C2018/466/09', '<b>Altstadt von Ávila und Kirchen außerhalb der Stadtmauer</b><br>(Drei Wehrtürme der Stadtmauer Ávilas)',
                    'C2020/049/12', '<b>Architektur der Mudéjares in Aragon</b><br>(Turm von El Salvador in Teruel)',
                    'C2021/096/08', '<b>Historische Altstadt von Toledo</b><br>(Puerta del Sol und Detail der Synagoge El Tránsito in Toledo)',
                    'C2022/484/10', '<b>Nationalpark Garajonay auf La Gomera</b><br>(Roque de Agando mit Lorbeerwald)',
                    NA, '<b>Altstadt von Cáceres</b><br>(Plaza Mayor)',
                    NA, '<b>Kathedrale, Alcázar und Indienarchiv in Sevilla</b><br>()',
                    NA, '<b>Altstadt von Salamanca</b><br>()')

    displ_serie(esun, "std")
    }, ignoreNULL = FALSE)
  
}



# Run the application ----
shinyApp(ui = ui, server = server)
