# GLOBAL ----
## Libraries ----
library(shiny)

## Externe Daten ----
source("eur2coins.r")       #coins
source("eur2circulation.r") #circulation
source("eur2collection.r")  #collection

## Ergänzen und behübschen der Daten ----
all_data <- function() {
  left_join(coins, collection %>% select(ID, Qualität, Ablage), by = 'ID') |> 
    mutate(Ablage = coalesce(Ablage, " ")) |> 
    left_join(circulation, by = 'ID')
}

## JS Funktion um Markierung zu kopieren ----
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

## Userpath für Sammlung (extern)
addResourcePath("tmpuser", getwd())

# UI (User Interface) ----
ui <- fluidPage(includeCSS(path = "style.css"),
  tags$script(highlight),
  tabsetPanel(id = "Hauptmenu", type = "pills",
    ## Identifikation ----
    tabPanel("Identifikation",
      h1("🙤 Identifikation 🙧"),
      fluidRow(
        column(width = 3,
        h2("Filter"),
        h3("Münzen"),
        radioButtons(inputId = "samlg", label = NULL, inline = TRUE,
                     choices = c("Alle" = "alle",
                                 "Vorhandene" = "ja",
                                 "Fehlende" = "nein")),
        HTML("<div class = 'beschr'>"), "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"),
        "im Sinne von egal / vorhanden / leer", HTML('</div>'),
        fluidRow(
          column(width = 6,
            h3("Münz ID"),
            fluidRow(
              column(width = 8, textInput(inputId = "id", label = NULL, value = "", width = "100%")),
              column(width = 4, offset = 0, actionButton(inputId = "id_reset", label = "✗", width = "100%")) # &cross;
              ),
            HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"),
            ", wobei ", code("JJJJ"), " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ",
            code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen.", HTML('</div>')),
          column(width = 6,
            h3("Münzzeichen"),
            fluidRow(
              column(width = 8, selectInput(inputId = "mzz", label = NULL, choices = unique(c(c("", "A", "D", "F", "G", "J"), all_data()$Münzzeichen)), selected = NULL, width = "100%")),
              column(width = 4, actionButton(inputId = "mzz_reset", label = "✗", width = "100%")) # &cross;
              ),
            HTML("<div class = 'beschr'>"), "Auswahl aus Liste; Genaue Übereinstimmung mit Feld ", em("Mzz."), HTML('</div>'))
          ),
        h3("Abbildung"),
        fluidRow(
          column(width = 10, textInput(inputId = "abb", label = NULL, value = "", width = "100%")),
          column(width = 2, actionButton(inputId = "abb_reset", label = "✗", width = "100%")) # &cross;
          ),
        div(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld ", em("Abbildung."), " Groß-/ Kleinschreibung wird ignoriert.", HTML('</div>')),
        h2("Anlage / Änderung"),
        h3("Qualität"),
        fluidRow(
          column(width = 3, actionButton(inputId = "q0", label = "(0) ★★★", width = "100%")), # &starf;
          column(width = 3, actionButton(inputId = "q1", label = "(1) ★★", width = "100%")), # &starf;
          column(width = 3, actionButton(inputId = "q2", label = "(2) ✓✓", width = "100%")), # &check;
          column(width = 3, actionButton(inputId = "q3", label = "(3) ✓", width = "100%")) # &check;
          ),
        p(HTML("<div class = 'beschr'>"), "Übernimmt Markierung aus Feld ", em("Münz ID."), HTML('</div>')),
        h3("eur2collection.txt"),
        fluidRow(
          column(width = 5, actionButton(inputId = "aenderung", label = "Änderung durchgeführt", width = "100%")),
          column(width = 7)
          ),
        p(HTML("<div class = 'beschr'>"), "Manuelle Änderung von ", em("eur2collection.txt,"), " zB Münztausch", HTML('</div>')),
        ),
        column(width = 9,
          h2("Ergebnisse"),
          htmlOutput(outputId = "n_münzen"),
          tableOutput(outputId = "suche_"))
        )
     ),
    ## Ablage ----
    tabPanel("Ablage",
      h1("🙤 Ablage 🙧"),
      fluidRow(
        column(width = 3,
        h2("Auswahl Box und Tableau"),
          fluidRow(
            column(width = 6,
              h3("Box"),
              sliderInput(inputId = "box", label = NULL, min = 1, max = 4, value = 1, step = 1, width = "100%"),
              div(HTML("<div class = 'beschr'>"), "Auswahl der Ablagebox.", HTML('</div>'))),
            column(width = 6,
              h3("Tableau"),
              sliderInput(inputId = "tableau", label = NULL, min = 1, max = 6, value = 1, step = 1, width = "100%"),
              p(HTML("<div class = 'beschr'>"), "Auswahl des Tableaus in der gewählten Ablagebox.", HTML('</div>')))
          ),
        h2("Auswahl Münze"),
          h3("Ablagenummer"),
          fluidRow(
            column(width = 2, actionButton(inputId = "minus", label = "≺", width = "100%")), # &prec;
            column(width = 2, actionButton(inputId = "plus", label = "≻", width = "100%")), # &succ;
            column(width = 5, textInput(inputId = "znr", value = "", label = NULL, width = "100%")), #pull(count(collection))
            column(width = 3, actionButton(inputId = "get", label = "gehe zu", width = "100%"))
            ),
          div(HTML("<div class = 'beschr'>"), em("gehe zu"), " übernimmt Markierung des unterstrichenen Teils im Tableau oder springt zur letzten abgelegten Münze. ",
              em("≺"), " navigiert zur vorherigen (-1), ", em("≻"), " zur nächsten (+1) Münze.", HTML('</div>')),
          ),
        column(width = 9,
          h2("Ansicht"),
          h3(textOutput(outputId = "adresse")),
          tableOutput(outputId = "tableau"),
          h3("Gewählte Ablagenummer"),
          tableOutput(outputId = "suche_abl")))
      ),
    ## Statistik ----
    tabPanel("Statistik",
      h1("🙤 Statistik 🙧"),
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
      ),
    ## Nationale Serien ----
    tabPanel("Nat. Serien",
      h1("🙤 Nationale Serien 🙧"),
      tabsetPanel(id = "Serien", type = "pills",
        tabPanel("DE",
          fluidPage(
            h2("Deutschland"),
            tabsetPanel(id = "Serien_DE", type = "pills",
              tabPanel("Bundesländerserie I",
                h3("Bundesländerserie I (2006-2022)"),
                tableOutput(outputId = "debl1_tab")
                ),
              tabPanel("Bundesländerserie II",
                h3("Bundesländerserie II (2023-2038)"),
                tableOutput(outputId = "debl2_tab")
                ),
              tabPanel("Einigkeit und Recht und Freiheit",
                h3("Einigkeit und Recht und Freiheit (2025 - )"),
                tableOutput(outputId = "deerf_tab")
                )
              )
            )
          ),
        tabPanel("HR",
          fluidPage(
            h2("Kroatien"),
            h3("Kroatische Städte (2025 - )"),
            tableOutput(outputId = "hrks_tab")
            )
          ),
        tabPanel("EE",
          fluidPage(
            h2("Estland"),
            h3("Nationale Symbole (2021 - )"),
            tableOutput(outputId = "eens_tab")
            )
          ),
        tabPanel("ES",
          fluidPage(
            h2("Spanien"),
            h3("UNESCO-Welterbestätten (2010 - )"),
            tableOutput(outputId = "esun_tab")
            )
          ),
        tabPanel("FR",
          fluidPage(
            h2("Frankreich"),
            h3("Olympische Sommerspiele 2024 (2021 - 2024)"),
            tableOutput(outputId = "fros_tab")
            )
          ),
        tabPanel("LT",
          fluidPage(
            h2("Litauen"),
            h3("Ethnographische Regionen (2019 - 2022, 2025)"),
            tableOutput(outputId = "lter_tab")
            )
          ),
        tabPanel("LU",
          fluidPage(
            h2("Luxemburg"),
            h3("Dynastieserie (2004 - )"),
            tableOutput(outputId = "ludy_tab")
            )
          ),
        tabPanel("LV",
          fluidPage(
            h2("Lettland"),
            h3("Historische Regionen (2016 - 2018)"),
            tableOutput(outputId = "lvhr_tab")
            )
          ),
        tabPanel("MT",
          fluidPage(
            h2("Malta"),
            tabsetPanel(id = "Serien_MT", type = "pills",
              tabPanel("Verfassungsgeschichte",
                h3("Verfassungsgeschichte (2011- 2015)"),
                tableOutput(outputId = "mtvg_tab"),
                ),
              tabPanel("Prähistorische Stätten",
                h3("Prähistorische Stätten (2016 - 2022)"),
                tableOutput(outputId = "mtps_tab"),
                ),
              tabPanel("Von Kindern mit Solidarität",
                h3("Von Kindern mit Solidarität 2016 - 2020)"),
                tableOutput(outputId = "mtks_tab"),
                ),
              tabPanel("Einheimische Arten Maltas",
                h3("Einheimische Arten Maltas (2024 - )"),
                tableOutput(outputId = "mtea_tab"),
                ),
              tabPanel("Maltesische Städte mit Stadtmauern",
                h3("Maltesische Städte mit Stadtmauern (2024 - )"),
                tableOutput(outputId = "mtsm_tab"),
                )
              )
            )
          )#,
        #tabPanel("...")
        )
      ),
    ## Gemeinschaftsausgaben ----
    tabPanel("Gemeinschaftsausgaben",
      h1("🙤 Gemeinschaftsausgaben 🙧"),
      tabsetPanel(id = "Gemeinschaftsausgaben", type = "pills",
        tabPanel("Vertrag v. Rom",
        fluidPage(
          h2("50. Jahrestag der Unterzeichnung des Vertrags von Rom - 2007"),
          tableOutput(outputId = "vvr_tab")
          )
        ),
        tabPanel("WWU",
          fluidPage(
            h2("Zehnjähriges Bestehen der Wirtschafts- und Währungsunion (WWU) - 2009"),
            tableOutput(outputId = "wwu_tab")
            )
          ),
        tabPanel("Euro-Einführung",
          fluidPage(
            h2("10. Jahrestag der Einführung des Euro-Bargelds - 2012"),
            tableOutput(outputId = "eur_tab")
            )
          ),
        tabPanel("EU-Flagge",
          fluidPage(
            h2("Dreißigjähriges Bestehen der EU-Flagge - 2015"),
            tableOutput(outputId = "euf_tab")
            )
          ),
        tabPanel("Erasmus-Programm",
          fluidPage(
            h2("35-jähriges Bestehen des Erasmus-Programms - 2022"),
            tableOutput(outputId = "era_tab")
            )
          )
        )
      ),
    ## Liste ----
    tabPanel("Liste",
      h1("🙤 Liste 🙧"),
      h2("Gesammelte Münzen"),
      htmlOutput(outputId = "samml_ext")
    ),
    tabPanel("Auflage",
      h1("🙤 Auflage 🙧"),
      column(width = 3,
        h2("Bearbeiten"),
        h3("Erfassen"),
          fluidRow(
            column(width = 8, textAreaInput(inputId = "aufl_erf", label = NULL, rows = 11, resize = "none", width = "100%")),
            column(width = 4,
              textInput(inputId = "aufl_zahl", label = NULL, value = "", width = "100%"),
              htmlOutput(outputId = "zahl_form", inline = TRUE),
              actionButton(inputId = "aufl_uber", label = "Übernehmen", width = "100%"),
              p(HTML("<div class = 'beschr'>"), "Die obige Auflagenstärke wird gemeinsam mit der markierten ",
                  em("Münz ID"), " in das Textfeld übernommen.", HTML('</div>'))),
          ),
          h3("Verwalten"),
          fluidRow(
            column(width = 6,
              actionButton(inputId = "aufl_schrb", label = "Schreiben", width = "100%"),
              p(HTML("<div class = 'beschr'>"), "Die Eingaben aus dem Textfeld werden ins File ",
                em("eur2coins_circulation.txt"), "übernommen.", HTML('</div>'))),
            column(width = 6,
              # actionButton(inputId = "aufl_sort", label = "Sortieren", width = "100%")
            )
          )
        ),
        column(width = 9,
          h2("Unbekannte Auflagenstärke"),
          htmlOutput(outputId = "n_aufl"),
          tableOutput(outputId = "unbek_aufl")
          )
    )#,
    #tabPanel("Test",
    #    fluidPage(
    #       h1("Test")
    #    )
    #)
    )
  )

# Server ----
server <- function(input, output, session) {
  
  ## Funktion zum Formatieren Qualität ----
  form_quali <- function(x) {
    case_when(is.na(x) ~ "",
              x == 0 ~ "<span style='color: #daa520;'>(0)&nbsp;&#9733;&#9733;&#9733;</span>",
              x == 1 ~ "<span style='color: #5f9321;'>(1)&nbsp;&#9733;&#9733;</span>", # #958746;
              x == 2 ~ "<span style='color: #1b7547;'>(2)&nbsp;&#10004;&#10004;</span>", # #51696c;
              x == 3 ~ "<span style='color: #0e4c92;'>(3)&nbsp;&#10004;</span>",
              TRUE ~ "<span style ='color: red;'>FEHLER</span>")
  }
  
  ## Funktion zur Darstellung Land ----
  form_land <- function(txt) {
    txt <- tolower(txt) # jedenfalls Kleinbuchstaben
    paste0("<nobr style='font-size: 0.75em'><img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif', height='14', alt='", toupper(txt), "'>&nbsp;&nbsp;/&nbsp;", toupper(txt), "</nobr>")
  }
  
  ## Funktion zum Formatieren Amtsblatt ----
  form_amtsbl <- function(txt) {
    url <- paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:", txt, "', target = '_blank'>", txt, "</a>")
    url <- str_replace(url, "\\(", "%28")
    url <- str_replace(url, "\\)", "%29")
    return(url)
  }
  
  ## Funktion zu Formatieren der Art (Münzart) ----
  form_art <- function(txt) {
    txt[txt == "G"] <- "<span style='font-size: 1.1em'>Ⓖ</span>"
    txt[txt == "K"] <- "<span style='font-size: 1.1em'>Ⓚ</span>"
    return(txt)
  }
  
  ## Funktion zum Formatieren der Häufigkeit ----
  form_hfgkt <- function(txt) {
    c("<div style='font-size: 1.1em; background-color: #b22222; color: white'>⩔</div>",
      "<div style='font-size: 1.1em; background-color: #c56320; color: white'>˅</div>",
      "<div style='font-size: 1.1em; background-color: #daa520; color: white'>-</div>",
      "<div style='font-size: 1.1em; background-color: #7d9820; color: white'>˄</div>",
      "<div style='font-size: 1.1em; background-color: #228b22; color: white'>⩓</div>")[txt]
  }
  
  ## Funktion zur Darstellung der Daten ----
  displ_data <- function(df, variation) {
    df <- mutate(df,
                 Jahr = Prägejahr,
                 Land = form_land(Land),
                 Amtsblatt = form_amtsbl(Amtsblatt),
                 ID = paste0("<div class='mono'>", ID, "</div>"),
                 Qualität = form_quali(Qualität),
                 Ablage = paste0("<div class='mono'>", Ablage, "</div>"),
                 AQ = paste0(Ablage, Qualität),
                 Art = form_art(Art),
                 Hfgkt = form_hfgkt(Hfgkt)) |> 
      arrange(ID)
    
    switch(variation,
           ident = df |> transmute(Jahr,
                                   Land,
                                   Art,
                                   Hfgkt,
                                   Abbildung,
                                   Mzz = Münzzeichen,
                                   Amtsblatt,
                                   'Münz ID' = ID,
                                   Qualität,
                                   Ablage),
           ser = cbind(paste0("<b>", pull(df, Jahr), "</b>"),
                       pull(df, Beschreibung), 
                       pull(df, Münzzeichen),
                       pull(df, AQ)) |>
             matrix(ncol = 4, dimnames = list(NULL, c("Jahr", "Bezeichnung", "Mzz", " "))),
           serde = cbind(paste0("<b>", df |> filter(Münzzeichen == "A") |> pull(Jahr), "</b>"),
                         df |> filter(Münzzeichen == "A") |> pull(Beschreibung),
                         matrix(df |> pull(AQ), ncol = 5, byrow = TRUE)) |> 
             matrix(ncol = 7, dimnames = list(NULL, c("Jahr", "Bezeichnung", "A (Berlin)", "D (München)", "F (Stuttgart)", "G (Karlsruhe)", "J (Hamburg)"))),
           gem = cbind(pull(df, Land), 
                       pull(df, Münzzeichen),
                       pull(df, ID),
                       pull(df, AQ)) |>  
             matrix(ncol = 4, dimnames = list(NULL, c("Land", "Mzz", "Münz ID", " "))),
           aufl = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung) |> 
             arrange(Land)
    )
  }
  
  ## Funktion zur Darstellung Statistik ----
  form_stat <- function(val, von, bis) {
    left_join(coins |> group_by(Grp = str_sub(ID, von, bis)) |> count(),
              collection |> group_by(Grp = str_sub(ID, von, bis)) |> count(),
              by = "Grp") |> 
      transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
                vH = Erfolg |> (\(x) eval(parse(text = x)) * 100)(),
                Graph = c(rep(HTML("&#9608;"), vH %/% 5), if((vH %% 5) >= 2.5) HTML("&#9612;")) |>  paste(collapse = "")) |> 
      rename(!!val := Grp) |> 
      mutate(vH = formatC(vH, 2, format = "f", decimal.mark = ","),
             Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }
  
  ## Reset Buttons ----  
  observeEvent(eventExpr = input$id_reset, handlerExpr = updateTextInput(session, inputId = "id", value = ""))
  observeEvent(eventExpr = input$abb_reset, handlerExpr = updateTextInput(session, inputId = "abb", value = ""))
  observeEvent(eventExpr = input$mzz_reset, handlerExpr = updateTextInput(session, inputId = "mzz", value = ""))

  ## Reload (für Bewertungsbuttons über Funktion 'add_bew' oder bei Button Änderung direkt hier) ----
  reload <- function() {
    source("eur2collection.r")
    }
  observeEvent(eventExpr = input$aenderung, handlerExpr = reload())
  
  ## Funktion zum Schreiben der Bewertung
  add_bew <- function(qu) {
    tmp <- paste(input$myselection, qu, sep = "-")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1.5)
    reload()
  }
  
  ## Bewertungs Buttons ----
  observeEvent(eventExpr = input$q0, handlerExpr = add_bew(0))
  observeEvent(eventExpr = input$q1, handlerExpr = add_bew(1))
  observeEvent(eventExpr = input$q2, handlerExpr = add_bew(2))
  observeEvent(eventExpr = input$q3, handlerExpr = add_bew(3))
  
  
  ## Ausgabe Ergebnisse Münzen ----
  output$suche_ <- renderTable(expr = tbl_(), spacing = "xs", width = "100%", align = c("lllcllllll"), sanitize.text.function = function(x) x)
  tbl_ <- eventReactive(eventExpr = c(input$samlg, input$id, input$mzz, input$abb, input$q0, input$q1, input$q2, input$q3, input$aenderung),
                        valueExpr = {
                          # Anzuzeigende Münzen
                          show <- filter(all_data(), (Ablage != " " | input$samlg != "ja"), (Ablage == " " | input$samlg != "nein"), # Sammlung
                                         grepl(tolower(input$id), ID),                                                               # ID
                                         grepl(tolower(input$abb), tolower(Abbildung)),                                              # Abbildung
                                         grepl(paste0("\\b", input$mzz, "\\b"), Münzzeichen))                                        # Münzzeichen - exakte Übereinstimmung ('\\b', - Regex word boundary)
                          # Anzahl Münzen n (Überschrift inkl Plural)
                          output$n_münzen <- renderText(paste0("<h3>", format(dim(show)[1], big.mark = "&VeryThinSpace;"), " Münze", if(dim(show)[1] > 1) "n " else " ",
                                                               "&emsp;(", paste(unique(show$Art), collapse = ' + '), ")</h3>"))
                          # Ausgabe Ergebnisse Münzen
                          displ_data(df = show, variation = "ident")
                          }
                        )
  
  ## Schnellwahl Schritte ----
  observeEvent(eventExpr = input$minus, handlerExpr = updateTextInput(session, inputId = "znr", value = as.integer(input$znr) - 1))
  observeEvent(eventExpr = input$plus, handlerExpr = updateTextInput(session, inputId = "znr", value = as.integer(input$znr) + 1))
  
  ## Schnellwahl Markierung übernehmen ----
  observeEvent(eventExpr = input$get, handlerExpr = updateTextInput(session, inputId = "znr", value = input$myselection))
  
  ## Adressbereich - Überschrift ----
  output$adresse <- renderText(expr = paste0("Box ", input$box, ", Tableau ", input$tableau, ": Ablagenummern ",
                                             (input$box - 1) * 144 + (input$tableau - 1) * 24 + 1, " bis ", (input$box - 1) * 144 + input$tableau * 24)
  )
  
  ## Ausgabe Ablage ----
  output$tableau <- renderTable(expr = erst_tab(), bordered = T, spacing = "l", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  erst_tab <- eventReactive(eventExpr = c(input$box, input$tableau, input$znr, input$q0, input$q1, input$q2, input$q3, input$aenderung),
                            valueExpr = {
                              collection |> 
                                filter(Zeilennummer %in% (((input$box - 1) * 144 + (input$tableau - 1) * 24 + 1) + 0:23)) |> 
                                arrange(Zeilennummer) |>
                                mutate(Qualität = form_quali(Qualität),
                                       This_left = case_when(input$znr == Zeilennummer ~ "<span class = 'bar'>&#9612;&VeryThinSpace;</span>",
                                                             TRUE ~ "<b>&emsp;&VeryThinSpace;</b>"),
                                       This_right = case_when(input$znr == Zeilennummer ~ "<span class = 'bar'>&VeryThinSpace;&#9616;</span>",
                                                              TRUE ~ "<b>&VeryThinSpace;&emsp;</b>"), # kein nbsp wegen doppelklick-markierung
                                       Res = paste0("<div class='mono', align = 'center'>", This_left, str_sub(Ablage, 1, 4), "&VeryThinSpace;&times;&VeryThinSpace;", str_sub(Ablage, 6, 9 - nchar(Zeilennummer)), "&VeryThinSpace;", "<u><b>", str_sub(Ablage, 9 - nchar(Zeilennummer) + 1, 9), "</b></u>", This_right, "</div>",
                                                    "<div class='mono', align = 'center'><b>", This_left, str_sub(ID, 1, 4), "&VeryThinSpace;", (str_sub(ID, 5, 6)), "&VeryThinSpace;", (str_sub(ID, 7, 7)), "</b>&VeryThinSpace;", str_sub(ID, 8, 9), This_right, "</div>",
                                                    "<div align = 'center'>", Qualität, "</div>")) |>  
                                pull(Res) -> tmp
                              if(length(tmp) < 24) tmp <- c(tmp, rep("", 24 - length(tmp)))
                              matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
                                     dimnames = list(paste0("<br><b>", input$box, input$tableau, "&thinsp;", 1:4, "..", "</b>"),
                                                     paste0("..", 1:6)
                                     )
                              )
                            }, ignoreNULL = FALSE)
  
  ## Funktuion zur Gültigkeitsprüfung Eingabe Ablagenummer
  check_znr <- function(x) {
    x <- as.integer(x)
    na_chk <- is.na(x)
    maxi <- pull(count(collection))
    
    if(na_chk) x <- maxi
    x <- max(1, min(x, maxi))
    return(list(x, !na_chk))
  }
  
  ## Ausgabe Schnellwahl Ablage ----
  output$suche_abl <- renderTable(expr = tbl_abl(), spacing = "xs", width = "100%", align = c("lllcllllll"), sanitize.text.function = function(x) x)
  tbl_abl <- eventReactive(eventExpr = c(input$znr),
                           valueExpr = {
                             if(check_znr(input$znr)[[2]]) updateSliderInput(session, inputId = "box", value = (as.integer(input$znr) - 1) %/% 144 + 1)
                             if(check_znr(input$znr)[[2]]) updateSliderInput(session, inputId = "tableau", value = (as.integer(input$znr) - 1) %% 144 %/% 24 + 1)
                             updateTextInput(session, inputId = "znr", value = check_znr(input$znr)[[1]])
                             ## Anzuzeigende Münzdetails
                             show <- all_data() |> mutate(Zeile = as.integer(str_sub(Ablage, 6, 9))) |> filter(Ablage != " ", Zeile == input$znr)
                             # Ausgabe
                             displ_data(df = show, variation = "ident")
                           }
  )
  
  ## Ausgabe Zusammenfassung Jahr ----
  output$zsf_jahr <- renderTable(expr = zsf_tbl_jahr(), spacing = "xs", align = c("rrrl"), sanitize.text.function = function(x) x)
  zsf_tbl_jahr <- eventReactive(eventExpr = c(input$q0, input$q1, input$q2, input$q3, input$aenderung),
                                valueExpr = form_stat("Jahr", 1, 4),
                                ignoreNULL = FALSE)
  
  ## Ausgabe Zusammenfassung Land ----
  output$zsf_land <- renderTable(expr = zsf_tbl_land(), spacing = "xs", align = c("lrrl"), sanitize.text.function = function(x) x)
  zsf_tbl_land <- eventReactive(eventExpr = c(input$q0, input$q1, input$q2, input$q3, input$aenderung),
                                valueExpr = form_stat("Land", 5, 6) |> 
                                  mutate(Land = form_land(Land)),
                                ignoreNULL = FALSE)
  
  ## Ausgabe Zusammenfassung Qualität ----
  output$zsf_qual <- renderTable(expr = zsf_tbl_qual(), spacing = "xs", align = c("lrr"), sanitize.text.function = function(x) x)
  zsf_tbl_qual <- eventReactive(eventExpr = c(input$aenderung, input$q0, input$q1, input$q2, input$q3),
                                valueExpr = {
                                  collection|> 
                                    group_by(Qualität = Qualität |>  ordered(levels = 0:3, labels = form_quali(0:3)), .drop = FALSE) |> 
                                    count() |> 
                                    transmute(Anzahl = n,
                                              Anteil = formatC(Anzahl / dim(collection)[1] * 100, 2, format = "f", decimal.mark = ",")
                                              )
                                },
                                ignoreNULL = FALSE)
  
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
                     'C2022/145/08', '<b>Thüringen</b><br>(Wartburg)') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(debl1, "serde")
  }, ignoreNULL = FALSE)
  
  ### Deutschland - Bundesländerserie II ----
  output$debl2_tab <- renderTable({debl2_tab()}, bordered = T, spacing = "l", align = "clccccc", rownames = FALSE, sanitize.text.function = function(x) x)
  debl2_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    debl2 <- tribble(~Amtsblatt, ~Beschreibung,
                     'C2023/123/06', '<b>Hamburg</b><br>(Elbphilharmonie)',
                     'C/2024/02355', '<b>Mecklenburg-Vorpommern</b><br>(Königsstuhl)',
                     NA, '<b>Saarland</b><br>(Saarschleife)',
                     NA, '<b>Bremen</b><br>(Klimahaus Bremerhaven)',
                     NA, '<b>Nordrhein-Westfalen</b><br>()',
                     NA, '<b>Bayern</b><br>()',
                     NA, '<b>Baden-Württemberg</b><br>()',
                     NA, '<b>Niedersachsen</b><br>()',
                     NA, '<b>Hessen</b><br>()',
                     NA, '<b>Sachsen</b><br>()',
                     NA, '<b>Rheinland-Pfalz</b><br>()',
                     NA, '<b>Berlin</b><br>()',
                     NA, '<b>Schleswig-Holstein</b><br>()',
                     NA, '<b>Brandenburg</b><br>()',
                     NA, '<b>Sachsen-Anhalt</b><br>()',
                     NA, '<b>Thüringen</b><br>()') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |>
      filter(!is.na(Amtsblatt))
    
    displ_data(debl2, "serde")
  }, ignoreNULL = FALSE)
  
  ### Deutschland - Einigkeit und Recht und Freiheit ----
  output$erf_tab <- renderTable({deerf_tab()}, bordered = T, spacing = "l", align = "clccccc", rownames = FALSE, sanitize.text.function = function(x) x)
  deerf_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    deerf <- tribble(~Amtsblatt, ~Beschreibung,
                     NA, '<b>35 Jahre Deutsche Einheit</b>',
                     NA, '<b>Konrad Adenauer</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |>
      filter(!is.na(Amtsblatt))
    
    displ_data(deerf, "serde")
  }, ignoreNULL = FALSE)
  
  ### Estland - Nationale Symbole ----
  output$eens_tab <- renderTable({eens_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  eens_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    eens <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2021/059/05', '<b>Der Wolf</b>, das Nationaltier',
                    'C2023/264/07', '<b>Die Rauchschwalbe</b>, der Nationalvogel',
                    'C/2024/03965', '<b>Die Kornblume</b>, die Nationalblume') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(eens, "ser")
  }, ignoreNULL = FALSE)
  
  ### Frankreich - Olympische Sommerspiele 2024 ----
  output$fros_tab <- renderTable({fros_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  fros_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    fros <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2021/470/07', '<b>Marianne und der Wettlauf</b><br>(Eiffelturm)',
                    'C2023/014/04', '<b>Der Genius und der Diskuswurf</b><br>(Arc de Triomphe)',
                    'C2023/116/12', '<b>Die Säerin und der Faustkampf</b><br>(Pont Neuf)',
                    'C/2024/02468', '<b>Herkules und der Ringkampf</b><br>(Nortre Dame)',
                    'C/2024/03959', '<b>Olympische und Paralympische Spiele 2024 in Paris</b><br>(Notre-Dame de la Garde)') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(fros, "ser")
  }, ignoreNULL = FALSE)
  
  ### Kroatien - Kroatische Städte ----
  output$hrks_tab <- renderTable({hrks_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  hrks_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    hrks <- tribble(~Amtsblatt, ~Beschreibung,
                    NA, '<b>Pula</b><br>(Arena)') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(hrks, "ser")
  }, ignoreNULL = FALSE)
  
  ### Litauen - Ethnografische Regionen ----
  output$lter_tab <- renderTable({lter_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  lter_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    lter <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2019/351/10', '<b>Žemaitija</b><br>(Niederlittauen)',
                    'C2020/053/04', '<b>Aukschtaiten</b><br>(Oberlitauen)',
                    'C2021/473/05', '<b>Dzukija</b><br>(Mittellitauen)',
                    'C2022/484/25', '<b>Suvalkija</b><br>(Sudauen)',
                    NA, '<b>Mažoji Lietuva</b><br>(Kleinlitauen)') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(lter, "ser")
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
                    'C2023/123/05', '<b>175. Jahrestag der Abgeordnetenkammer und der ersten Verfassung (1848)</b>',
                    'C2023/122/05', '<b>25. Jahrestag der Aufnahme von Großherzog Henri als Mitglied des Internationalen Olympischen Komitees</b>',
                    'C/2024/02466', '<b>175. Todestag von Großherzog Guillaume II.</b>',
                    'C/2024/02467', '<b>100. Jahrestag der Unterzeichnung des Erlasses über die Ausgabe der „Feierstëppler“-Scheidemünze durch Großherzogin Charlotte</b>',
                    NA, '<b>25. Jahrestag der Thronbesteigung von Großherzog Henri</b>',
                    NA, '<b>75. Jahrestag der Schuman-Erklärung</b>',
                    NA, '<b>Verleihung des Internationalen Karlspreises an das luxemburgische Volk</b>',
                    NA, '<b>Luxemburgischer Nationalfeiertag / Groussherzogsgebuertsdag</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(ludy, "ser")
  }, ignoreNULL = FALSE)
  
  ### Lettland - Historische Regionen ----
  output$lvhr_tab <- renderTable({lvhr_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  lvhr_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    lvhr <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2016/146/07', '<b>Vidzeme</b><br>(Zentral-Livland)',
                    'C2017/066/02', '<b>Kurzemen</b><br>(Kurland)',
                    'C2017/066/03', '<b>Latgale</b><br>(Lettgallen)',
                    'C2018/234/03', '<b>Zemgale</b><br>(Semgallen)',
                    NA, '<b>Sēlija</b><br>(Selonien)') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(lvhr, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Verfassungsgeschichte ----
  output$mtvg_tab <- renderTable({mtvg_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtvg_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtvg <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2011/299/08', '<b>Wahl der ersten Abgeordneten 1849</b>',
                    'C2012/375/06', '<b>Mehrheitswahlrecht 1887</b>',
                    'C2013/379/09', '<b>Einrichtung der Selbstverwaltung 1921</b>',
                    'C2014/383/05', '<b>Unabhängigkeit von Großbritannien 1964</b>',
                    'C2015/150/03', '<b>Ausrufung der Republik Malta 1974</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtvg, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Verfassungsgeschichte ----
  output$mtvg_tab <- renderTable({mtvg_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtvg_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtvg <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2011/299/08', '<b>Wahl der ersten Abgeordneten 1849</b>',
                    'C2012/375/06', '<b>Mehrheitswahlrecht 1887</b>',
                    'C2013/379/09', '<b>Einrichtung der Selbstverwaltung 1921	</b>',
                    'C2014/383/05', '<b>Unabhängigkeit von Großbritannien 1964	</b>',
                    'C2015/150/03', '<b>Ausrufung der Republik Malta 1974</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtvg, "ser")
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
                    'C2022/484/22', '<b>Ħal-Saflieni-Hypogäum</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtps, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Von Kindern mit Solidarität ----
  output$mtks_tab <- renderTable({mtks_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtks_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtks <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2016/396/03', '<b>Solidarität durch Liebe</b>',
                    'C2017/386/03', '<b>Frieden</b>',
                    'C2018/401/07', '<b>Kulturelles Erbe</b>',
                    'C2019/352/16', '<b>Natur / Umwelt</b>',
                    'C2020/380/04', '<b>Kinderspiele</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtks, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Einheimische Arten Maltas ----
  output$mtea_tab <- renderTable({mtea_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtea_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtea <- tribble(~Amtsblatt, ~Beschreibung,
                    'C/2024/03949', 'Die maltesische <b>Honigbiene</b>',
                    NA, 'Der maltessiche <b>Ochse</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtea, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Maltesische Städte mit Stadtmauern ----
  output$mtsm_tab <- renderTable({mtsm_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtsm_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtsm <- tribble(~Amtsblatt, ~Beschreibung,
                    'C/2024/03946', '<b>Cittadella Gozo</b>',
                    NA, '<b>Mdina</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtsm, "ser")
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
                    'C2023/116/10', '<b>Altstadt von Cáceres</b><br>(Plaza Mayor)',
                    'C/2024/02354', '<b>Kathedrale, Alcázar und Indienarchiv in Sevilla</b><br>(Jungfrauenhof des Alcázar von Sevilla)',
                    NA, '<b>Altstadt von Salamanca</b>(Kirche und Kloster San Esteban des Dominikanerordens)<br>()',
                    NA, '<b>Kloster Poblet</b><br>()') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(esun, "ser")
  }, ignoreNULL = FALSE)
  
  ### Gemeinschaftsausgabe - Vertrag von Rom ----
  output$vvr_tab <- renderTable({vvr_tab()}, bordered = T, spacing = "l", align = "lccc", rownames = FALSE, sanitize.text.function = function(x) x)
  vvr_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    vvr <- tribble(~Amtsblatt, ~Beschreibung,
                   'C2007/065/04', '...') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(vvr, "gem")
  }, ignoreNULL = FALSE)
  
  ### Gemeinschaftsausgabe - WWU ----
  output$wwu_tab <- renderTable({wwu_tab()}, bordered = T, spacing = "l", align = "lccc", rownames = FALSE, sanitize.text.function = function(x) x)
  wwu_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    wwu <- tribble(~Amtsblatt, ~Beschreibung,
                   'C2008/315/04', '...') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(wwu, "gem")
  }, ignoreNULL = FALSE)
  
  ### Gemeinschaftsausgabe - Euro-Einführung ----
  output$eur_tab <- renderTable({eur_tab()}, bordered = T, spacing = "l", align = "lccc", rownames = FALSE, sanitize.text.function = function(x) x)
  eur_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    eur <- tribble(~Amtsblatt, ~Beschreibung,
                   'C2012/017/05', '...') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(eur, "gem")
  }, ignoreNULL = FALSE)
  
  ### Gemeinschaftsausgabe - EU-Flagge ----
  output$euf_tab <- renderTable({euf_tab()}, bordered = T, spacing = "l", align = "lccc", rownames = FALSE, sanitize.text.function = function(x) x)
  euf_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    euf <- tribble(~Amtsblatt, ~Beschreibung,
                   'C2015/253/07', '...',
                   'C2015/253/08', '...',
                   'C2015/253/09', '...',
                   'C2015/253/10', '...',
                   'C2015/255/03', '...',
                   'C2015/256/06', '...',
                   'C2015/257/04', '...',
                   'C2015/257/05', '...',
                   'C2015/257/06', '...',
                   'C2015/257/07', '...',
                   'C2015/257/08', '...',
                   'C2015/257/09', '...',
                   'C2015/290/04', '...',
                   'C2015/308/04', '...',
                   'C2015/327/05', '...',
                   'C2015/327/06', '...',
                   'C2015/327/07', '...',
                   'C2015/356/05', '...',
                   'C2015/356/07', '...') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(euf, "gem")
  }, ignoreNULL = FALSE)
  
  ### Gemeinschaftsausgabe - Erasmus-Programm ----
  output$era_tab <- renderTable({era_tab()}, bordered = T, spacing = "l", align = "lccc", rownames = FALSE, sanitize.text.function = function(x) x)
  era_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    era <- tribble(~Amtsblatt, ~Beschreibung,
                   'C2022/012/03', '...') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(era, "gem")
  }, ignoreNULL = FALSE)
  
  ## Sammlung (extern) ----
  output$samml_ext <- renderUI(tags$iframe(src = "tmpuser/sammlung.html", width = "50%", height = "750"))
  
  ## Auflage  Buttons ----
  observeEvent(eventExpr = input$aufl_uber, handlerExpr = updateTextInput(session, inputId = "aufl_erf", value = paste0(input$aufl_erf, input$myselection, "-", input$aufl_zahl, "\n")))
  observeEvent(eventExpr = input$aufl_schrb, handlerExpr = {
    out <- input$aufl_erf
    while(str_sub(out, -1) == "\n") out <- str_sub(out, 1, -2)
    write(out, file = "eur2coins_circulation.txt", append = TRUE)
    Sys.sleep(1.5)
    source("eur2circulation.r")
    reload()
    updateTextInput(session, inputId = "aufl_erf", value = "")
  })
  
  ## Ausgabe formtierte Zahl ----
  output$zahl_form <- renderText(expr = zahl_form())
  zahl_form <- eventReactive(eventExpr = input$aufl_zahl, valueExpr = paste0("<div style='text-align: center'>= ",format(as.numeric(input$aufl_zahl), big.mark = " ", scientific = FALSE), "<br>&nbsp;</div>"))
  
  ## Ausgabe Unbekannte Auflage ----
  output$unbek_aufl <- renderTable(expr = aufl_(), spacing = "xs", width = "100%", align = c("llllll"), sanitize.text.function = function(x) x)
  aufl_ <- eventReactive(eventExpr = c(input$aufl_schrb),
                        valueExpr = {
                          # Anzuzeigende Münzen
                          show <- filter(all_data(), is.na(Hfgkt), !str_starts(Abbildung, "~"))
                          # Anzahl Münzen n (Überschrift inkl Plural)
                          output$n_aufl <- renderText(paste0("<h3>", format(dim(show)[1], big.mark = "&VeryThinSpace;"), " Münze", if(dim(show)[1] > 1) "n " else " ", "</h3>"))
                          # Ausgabe Ergebnisse Münzen
                          displ_data(df = show, variation = "aufl")
                        }
  )
  
}

# Run the application ----
shinyApp(ui = ui, server = server)
