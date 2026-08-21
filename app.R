# GLOBAL ----
## Libraries ----
library(shiny)
library(bslib)
library(tidyverse)

## Laden der externen Daten ----
source("rd_celex.r")       # celex
source("rd_circulation.r") # circulation
source("rd_collection.r")  # collection
source("mk_cpic.r")        # cpic

## JS Funktion  ----
fkt_highlight <- '
// =============================================================================
// 1. TEXTMARKIERUNG IM BROWSER ERFASSEN
// =============================================================================

// Liest den aktuell vom Nutzer mit der Maus markierten Text im Browser aus
function getSelectionText() {
    var text = "";
    if (window.getSelection) {
        text = window.getSelection().toString();
    } else if (document.selection) { // Abwärtskompatibilität für ältere Internet Explorer
        text = document.selection.createRange().text;
    }
    return text;
}

// Reagiert auf Maus- und Tastaturaktionen sowie Änderung der Textauswahl.
// Übermittelt den markierten Text in Echtzeit an R/Shiny als "input$myselection".
document.onmouseup = document.onkeyup = document.onselectionchange = function() {
    var selection = getSelectionText();
    Shiny.onInputChange("myselection", selection);
};
'

# UI (User Interface) ----
ui <- page_fluid(
  includeCSS(path = "style_fwd.css"),
  tags$script(fkt_highlight),
  navset_pill(
    ## Identifikation ----
    nav_panel(title = "Identifikation",
              page_sidebar(
                sidebar = sidebar(width = "27%", position = "left", open = "always",
                                  h2("Filter"),
                                  fluidRow(
                                    h3("Münzen"),
                                    radioButtons(inputId = "in_smlg.ident", label = NULL, inline = TRUE,
                                                 choices = c("Alle" = "alle",
                                                             "Vorhandene" = "ja",
                                                             "Fehlende" = "nein")),
                                    div(class = 'beschr', "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"), "im Sinne von egal / vorhanden / leer")),
                                  fluidRow(
                                    column(width = 6,
                                           h3("Münz ID"),
                                           fluidRow(
                                             column(width = 8, textInput(inputId = "in_id.ident", label = NULL, value = "", width = "100%")),
                                             column(width = 4, actionButton(inputId = "bt_reset_id.ident", label = "✗", width = "100%", style = "padding:6px;"))),
                                           div(class = 'beschr', "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"), " = Prägejahr",
                                               ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen")),
                                    column(width = 6,
                                           h3("Münzzeichen"),
                                           fluidRow(
                                             column(width = 8, selectInput(inputId = "in_mzz.ident", label = NULL, 
                                                                           choices = unique(c("", "A", "D", "F", "G", "J", celex$Münzzeichen)), 
                                                                           selected = NULL, width = "100%")),
                                             column(width = 4, actionButton(inputId = "bt_reset_mzz.ident", label = "✗", width = "100%", style = "padding:6px;"))),
                                           div(class = 'beschr', "Auswahl aus Liste; Genaue Übereinstimmung mit ", em("Mzz")))),
                                  fluidRow(
                                    h3("Abbildung"),
                                    column(width = 10, textInput(inputId = "in_abb.ident", label = NULL, value = "", width = "100%")),
                                    column(width = 2, actionButton(inputId = "bt_reset_abb.ident", label = "✗", width = "100%", style = "padding:6px;")),
                                    div(class = 'beschr', "Beliebige Übereinstimmung mit ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert")),
                                  h2("Bearbeitung"),
                                  fluidRow(
                                    h3("Qualität"),
                                    column(width = 3, actionButton(inputId = "bt_write_q0.ident", label = "(0) ★★★", width = "100%", style = "padding:6px;")),
                                    column(width = 3, actionButton(inputId = "bt_write_q1.ident", label = "(1) ★★☆", width = "100%", style = "padding:6px;")),
                                    column(width = 3, actionButton(inputId = "bt_write_q2.ident", label = "(2) ★☆☆", width = "100%", style = "padding:6px;")),
                                    column(width = 3, actionButton(inputId = "bt_write_q3.ident", label = "(3) ☆☆☆", width = "100%", style = "padding:6px;")),
                                    p(div(class = 'beschr', "[...] Übernimmt markierte ", em("Münz ID"), "und ändert/ergänzt gewählte Qualität im File eur2collection.txt"))),
                                  fluidRow(
                                    column(width = 6,
                                           h3("eur2collection.txt"),
                                           actionButton(inputId = "bt_do_aend.ident", label = "Neu laden", width = "100%", style = "padding:6px;"),
                                           div(class = 'beschr', style = "margin-top: 4px;", "[Neu laden] liest Dateien von Festplatte neu ein.")
                                    ),
                                    column(width = 6,
                                           h3("sammlung.html"),
                                           downloadButton(outputId = "dl_sammlung_report.ident", label = "Erstellen", style = "width: 100%; padding: 6px; font-size: 14px;"),
                                           div(class = 'beschr', style = "margin-top: 4px;", "[Erstellen] knittet sammlung.Rmd in ein HTML-File zum Download.")))),
                ### Identifikation Main ----
                h2("Ergebnis entsprechend Filter", .noWS = "before"),
                  htmlOutput(outputId = "out_h3.ident"),
                fluidRow(
                  div(class = 'longtab', tableOutput(outputId = "out_table.ident"))))),
    ## Auflagenstärke ----
    nav_menu(title = "Auflage",
             nav_panel(title = "Auflagenstärke erfassen",
                       page_sidebar(
                         sidebar = sidebar(width = "27%", position = "left", open = "always",
                                           h2("Bearbeitung"),
                                           fluidRow(
                                             h3("Auflagenstärke"),
                                             column(width = 7, textInput(inputId = "in_aufl.erf", label = NULL, value = "", width = "100%")),
                                             column(width = 5, htmlOutput(outputId = "out_aufl.erf", inline = TRUE))),
                                           fluidRow(
                                             h3("Erfassen"),
                                             column(width = 6, textAreaInput(inputId = "in_erf.erf", label = NULL, rows = 11, resize = "none", width = "100%")),
                                             column(width = 6,
                                                    actionButton(inputId = "bt_do_erf.erf", label = "Erfassen", width = "100%", style = "padding:6px;"),
                                                    p(div(class = 'beschr', "[Erfassen] überträgt den Wert Auflagenstärke gemeinsam mit markierter ", em("Münz ID"), "in Eingabebereich")))),
                                           fluidRow(
                                             h3("Speichern"),
                                             column(width = 3),
                                             column(width = 6, actionButton(inputId = "bt_write_aufl.erf", label = "Speichern", width = "100%", style = "padding:6px;")),
                                             column(width = 3),
                                             p(div(class = 'beschr', "[Speichern] schreibt Werte aus Eingabebereich ins File eur2coins_circulation.txt")))),
                         ### Auflagenstärke erfassen Main ----
                         h2("Unbekannte Auflagenstärke", .noWS = "before"),
                           htmlOutput(outputId = "out_h3.erf"),
                         fluidRow(
                           div(class = 'longtab', tableOutput(outputId = "out_table.erf"))))),
             nav_panel(title = "Auflagenstärke korrigieren",
                       page_sidebar(
                         sidebar = sidebar(width = "27%", position = "left", open = "always",
                                           h2("Filter"),
                                           fluidRow(
                                             column(width = 6,
                                                    h3("Münz ID"),
                                                    fluidRow(
                                                      column(width = 8, textInput(inputId = "in_id.korr", label = NULL, value = "", width = "100%")),
                                                      column(width = 4, actionButton(inputId = "bt_reset_id.korr", label = "✗", width = "100%", style = "padding:6px;"))),
                                                    div(class = 'beschr', "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"), " = Prägejahr",
                                                        ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen")),
                                             column(width = 6,
                                                    h3("Münzzeichen"),
                                                    fluidRow(
                                                      column(width = 8, selectInput(inputId = "in_mzz.korr", label = NULL, 
                                                                                    choices = unique(c("", "A", "D", "F", "G", "J", celex$Münzzeichen)),
                                                                                    selected = NULL, width = "100%")),
                                                      column(width = 4, actionButton(inputId = "bt_reset_mzz.korr", label = "✗", width = "100%", style = "padding:6px;"))),
                                                    div(class = 'beschr', "Auswahl aus Liste; Genaue Übereinstimmung mit ", em("Mzz")))),
                                           fluidRow(
                                             h3("Abbildung"),
                                             column(width = 10, textInput(inputId = "in_abb.korr", label = NULL, value = "", width = "100%")),
                                             column(width = 2, actionButton(inputId = "bt_reset_abb.korr", label = "✗", width = "100%", style = "padding:6px;")),
                                             div(class = 'beschr', "Beliebige Übereinstimmung ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert")),
                                           h2("Bearbeitung"),
                                           fluidRow(
                                             h3("Auflagenstärke"),
                                             column(width = 7, textInput(inputId = "in_aufl.korr", label = NULL, value = "", width = "100%")),
                                             column(width = 5, htmlOutput(outputId = "out_aufl.korr", inline = TRUE))),
                                           fluidRow(
                                             h3("Speichern"),
                                             column(width = 3),
                                             column(width = 6, actionButton(inputId = "bt_write_aufl.korr", label = "Speichern", width = "100%", style = "padding:6px;")),
                                             column(width = 3),
                                             p(div(class = 'beschr', "Wert Auflagenstärke wird gemeinsam mit markierter ", em("Münz ID"), " im File eur2coins_circulation.txt geändert")))),
                         ### Auflagenstärke korrigieren Main ----
                         h2("Ergebnis entsprechend Filter", .noWS = "before"),
                           htmlOutput(outputId = "out_h3.korr"),
                         fluidRow(
                           div(class = 'longtab', tableOutput(outputId = "out_table.korr")))))),
    ## Ablage ----
    nav_panel(title = "Ablage",
              page_sidebar(
                sidebar = sidebar(width = "27%", position = "left", open = "always",
                                  h2("Auswahl Box und Tableau"),
                                  fluidRow(
                                    column(width = 6,
                                           h3("Box"),
                                           sliderInput(inputId = "in_box.abl", label = NULL, min = 1, max = 5, value = 1, step = 1, width = "100%"),
                                           HTML("<div class = 'beschr'>"), "Auswahl Ablagebox", HTML('</div>')),
                                    column(width = 6,
                                           h3("Tableau"),
                                           sliderInput(inputId = "in_tableau.abl", label = NULL, min = 1, max = 6, value = 1, step = 1, width = "100%"),
                                           div(class = 'beschr', "Auswahl Tableau in gewählter Ablagebox"))),
                                  h2("Auswahl Münze"),
                                  fluidRow(
                                    h3("Ablagenummer"),
                                    column(width = 2, actionButton(inputId = "bt_do_minus.abl", label = "≺", width = "100%", style = "padding:6px;")),
                                    column(width = 2, actionButton(inputId = "bt_do_plus.abl", label = "≻", width = "100%", style = "padding:6px;")),
                                    column(width = 5, textInput(inputId = "in_ablnr.abl", value = pull(count(collection)), label = NULL, width = "100%")),
                                    column(width = 3, actionButton(inputId = "bt_do_getablnr.abl", label = "gehe zu", width = "100%", style = "padding:6px;")),
                                    div(class = 'beschr', "[≺] navigiert zur vorherigen (-1), [≻] zur nächsten (+1) Münze; ",
                                        "[gehe zu] übernimmt markierten unterstrichenen Teil im Tableau oder springt zur letzten abgelegten Münze"))),
                ### Ablage Main ----
                h2("Aktives Tableau", .noWS = "before"),
                  htmlOutput(outputId = "out_h3tableau.abl"),
                fluidRow(
                  div(class = 'matrix', tableOutput(outputId = "out_tableau.abl"))),
                h2("Aktive Münze", .noWS = "before"),
                fluidRow(
                  column(width = 2, div(align = "center", imageOutput(outputId = "out_cpic.abl"))),
                  column(width = 10,
                         htmlOutput(outputId = "out_h3aktmz.abl"),
                         tableOutput(outputId = "out_aktmz.abl")))
              )),
    ## Statistik ----
    nav_panel(title = "Statistik",
              h2(HTML("&nbsp;")),
              fluidRow(
                column(width = 4,
                       h3("Prägejahr"),
                       tableOutput(outputId = "out_jahr.stat")),
                column(width = 4,
                       h3("Land"),
                       tableOutput(outputId = "out_land.stat")),
                column(width = 4,
                       h3("Münzart"),
                       tableOutput(outputId = "out_art.stat"),
                       HTML("<br>"),
                       h3("Qualität"),
                       tableOutput(outputId = "out_qual.stat"),
                       HTML("<br>"),
                       h3("Häufigkeit"),
                       tableOutput(outputId = "out_hfgkt.stat"))))))

# Server ----
server <- function(input, output, session) {
  
  ## Reaktive Speicherwerte ----
  val_collection  <- reactiveVal(collection)
  val_circulation <- reactiveVal(circulation)
  
  ## Reaktives Gecachtes ALL_DATA ----
  all_data <- reactive({
    Reduce(function(...) merge(..., by = "ID", all.x = TRUE, no.dups = TRUE),
           list(coins, 
                select(val_collection(), ID, Qualität, Ablage), 
                val_circulation(), 
                select(filter(cpic, Exists), ID, PicFile))) |> 
      as_tibble()
  })
  
  ## Formatierungs- und Hilfsfunktionen ----
  fkt_form_tsd <- function (x) format(as.numeric(x), big.mark = "&nbsp;", scientific = FALSE)
  
  fkt_form_land <- function(txt) {
    txt <- tolower(txt)
    paste0("<nobr class = 'flag'><img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif',
           height='15', alt='", toupper(txt), "'>&nbsp;&nbsp;(", toupper(txt), ")</nobr>")
  }
  
  fkt_form_amtsbl <- function(txt) {
    if(is_empty(txt)) return(NA)
    lexicon <- c(ORIG = "52001XC1228\\(04\\)", CELEX = "C\\d{4}/\\d{3}/\\d{2}", ELI = "C/\\d{4}/\\d{5}")
    work <- tibble(input = txt, class = sapply(txt, str_which, lexicon)) |>
      mutate(output = case_when(class == 1 ~ paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:52001XC1228%2804%29', target = '_blank'>", input, "</a>"),
                                class == 2 ~ paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:", input, "', target = '_blank'>", input, "</a>"),
                                class == 3 ~ paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=OJ:C_", str_replace_all(input, "[^0-9]", ""), "', target = '_blank'>", input, "</a>"),
                                TRUE ~ NA))
    return(work$output)
  }
  
  fkt_form_art <- function(txt) {
    txt[txt == "G"] <- "<span>&#10629;&#120022;&#10630;</span>"
    txt[txt == "K"] <- "<span>&#10629;&#120026;&#10630;</span>"
    return(txt)
  }
  
  fkt_form_hfgkt <- function(x) {
    c("<nobr class='rare1'>&emsp;&#9660;&emsp;</nobr>",
      "<nobr class='rare2'>&emsp;&#9661;&emsp;</nobr>",
      "<nobr class='rare3'>&emsp;&#9634;&emsp;</nobr>",
      "<nobr class='rare4'>&emsp;&#9651;&emsp;</nobr>",
      "<nobr class='rare5'>&emsp;&#9650;&emsp;</nobr>")[x]
  }
  
  fkt_form_quali <- function(x) {
    case_when(is.na(x) ~ "",
              x == 0 ~ "<nobr class = 'q0'>(0)&nbsp;&starf;&starf;&starf;</nobr>",
              x == 1 ~ "<nobr class = 'q1'>(1)&nbsp;&starf;&starf;&star;</nobr>",
              x == 2 ~ "<nobr class = 'q2'>(2)&nbsp;&starf;&star;&star;</nobr>",
              x == 3 ~ "<nobr class = 'q3'>(3)&nbsp;&star;&star;&star;</nobr>",
              TRUE ~ "<nobr class = 'qF'><i>&nbsp;FEHLER!&nbsp;<i></nobr>")
  }
  
  fkt_datadisplay <- function(df, variation) {
    df <- mutate(df,
                 Jahr = Prägejahr,
                 Land = fkt_form_land(Land),
                 Amtsblatt = fkt_form_amtsbl(Amtsblatt),
                 ID = paste0("<dbwert class='mono herv'>", ID, "</dbwert>"),
                 Qualität = fkt_form_quali(Qualität),
                 Ablage = case_when(is.na(Ablage) ~ "",
                                    TRUE ~ paste0("<dbwert class='mono herv'>", Ablage, "</dbwert>")),
                 AQ = paste0(Ablage, Qualität),
                 Art = fkt_form_art(Art),
                 Hfgkt = fkt_form_hfgkt(Hfgkt)) |> 
      arrange(ID)
    
    switch(variation,
           ident = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung, Hfgkt, Amtsblatt, Qualität, Ablage),
           ablage = df |> transmute(Jahr, Land, Art, Mzz = Münzzeichen, Abbildung, Hfgkt, Amtsblatt, Qualität),
           uaufl = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung) |> arrange(Land),
           eaufl = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung, Auflage = fkt_form_tsd(Auflage), Hfgkt) |> arrange('Münz ID'))
  }
  
  ### Fkt Darstellung Statistik ----
  fkt_form_stat <- function(val, von, bis) {
    left_join(coins |> group_by(Grp = str_sub(ID, von, bis)) |> count(),
              val_collection() |> group_by(Grp = str_sub(ID, von, bis)) |> count(), # MIT KLAMMERN
              by = "Grp") |> 
      transmute(Erfolg = paste0(coalesce(n.y, 0L), " / ", n.x),
                vH = Erfolg |> (\(x) eval(parse(text = x)) * 100)(),
                Graph = c(rep(HTML("&#9608;"), vH %/% 5), if((vH %% 5) >= 2.5) HTML("&#9612;")) |> paste(collapse = "")) |> 
      rename(!!val := Grp) |> 
      mutate(vH = formatC(vH, 2, format = "f", decimal.mark = ","),
             Graph = paste0("<div class='bar'>", Graph, "</div>"))
  }
  
  fkt_do_ungltg.mid <- function() {
    if(str_detect(input$myselection, "\\d{4}[a-z]{2}[g|k]\\d{2}")) return(FALSE)
    else {
      showModal(modalDialog(
        title = "Fehler",
        paste0("Keine gültige Münz ID markiert."),
        easyClose = TRUE, footer = NULL, size = "s"))
      return(TRUE)
    }
  }
  
  ### Fkt Schreiben/Ändern einer Bewertung im Speicher + Hintergrund-File ----
  fkt_write_bewertung <- function(qu) {
    if(fkt_do_ungltg.mid()) return()
    
    current_col <- val_collection()
    
    if(input$myselection %in% current_col$ID) {
      current_col[current_col$ID == input$myselection, "Qualität"] <- qu
    } else {
      current_col <- add_row(current_col, ID = input$myselection, Qualität = qu)
    }
    
    # Analoge Berechnung zu rd_collection.r anstossen
    current_col <- current_col |> 
      mutate(
        Zeilennummer = row_number(),
        Box = (Zeilennummer - 1) %/% 144 + 1,
        Tableau = (Zeilennummer - 1) %/% 24 %% 6 + 1,
        Zeile = (Zeilennummer - 1) %/% 6 %% 4 + 1,
        Spalte = (Zeilennummer - 1) %% 6 + 1,
        Ablage = paste0(Box, Tableau, Zeile, Spalte, "×", str_pad(Zeilennummer, 4, pad = "0"))
      )
    
    # 1. Speicher reaktiv updaten (stößt all_data() automatisch an)
    val_collection(current_col)
    
    # 2. Nur ID und Qualität ins Textfile schreiben (dein bisheriges Format)
    write_lines(paste(current_col$ID, current_col$Qualität, sep = "-"), "eur2coins_collection.txt")
  }
  
  form_aufl <- function(x) {
    paste0("<div style='text-align: left; margin-top: 7px'>=&nbsp;<b>", fkt_form_tsd(x), "</b>&nbsp;</div>")
  }
  
  ## Page Identifikation ----
  observeEvent(eventExpr = input$bt_reset_id.ident, handlerExpr = updateTextInput(session, inputId = "in_id.ident", value = ""))
  observeEvent(eventExpr = input$bt_reset_mzz.ident, handlerExpr = updateTextInput(session, inputId = "in_mzz.ident", value = ""))
  observeEvent(eventExpr = input$bt_reset_abb.ident, handlerExpr = updateTextInput(session, inputId = "in_abb.ident", value = ""))
  
  observeEvent(eventExpr = input$bt_write_q0.ident, handlerExpr = fkt_write_bewertung(0))
  observeEvent(eventExpr = input$bt_write_q1.ident, handlerExpr = fkt_write_bewertung(1))
  observeEvent(eventExpr = input$bt_write_q2.ident, handlerExpr = fkt_write_bewertung(2))
  observeEvent(eventExpr = input$bt_write_q3.ident, handlerExpr = fkt_write_bewertung(3))
  
  # Neu laden aus externen Dateien (falls außerhalb geändert)
  observeEvent(eventExpr = input$bt_do_aend.ident, handlerExpr = {
    tryCatch({
      source("rd_circulation.r")
      source("rd_collection.r")
      val_collection(collection)
      val_circulation(circulation)
      showNotification("Dateien erfolgreich neu eingelesen!", type = "message", duration = 2)
    }, error = function(e) {
      showNotification(paste("Fehler beim Einlesen:", e$message), type = "error", duration = 5)
    })
  })
                          
  # Erstelle sammlung.html
  output$dl_sammlung_report.ident <- downloadHandler(
    filename = function() {
      paste0("sammlung_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Benachrichtigung anzeigen
      id <- showNotification("Erstelle sammlung.html...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      # 1. Aktuelles Arbeitsverzeichnis der Shiny-App speichern
      app_dir <- getwd()
      
      # 2. Rmd-Datei in temporäres Verzeichnis kopieren
      tempReport <- file.path(tempdir(), "sammlung.Rmd")
      file.copy("sammlung.Rmd", tempReport, overwrite = TRUE)
      
      # 3. HTML rendern und explizit den App-Ordner als knit_root_dir festlegen
      rmarkdown::render(
        input = tempReport,
        output_file = file,
        knit_root_dir = app_dir, # Stellt sicher, dass Quell-Dateien wie mk_cpic.r gefunden werden
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$out_table.ident <- renderTable(expr = er_tabl.ident(), spacing = "xs", width = "100%", align = c("lllcrlclcl"), sanitize.text.function = function(x) x)
  er_tabl.ident <- reactive({
    show <- filter(all_data(), 
                   (Ablage != " " | input$in_smlg.ident != "ja"), 
                   (is.na(Ablage) | input$in_smlg.ident != "nein"),
                   grepl(input$in_id.ident, ID, ignore.case = TRUE, perl = TRUE),
                   grepl(paste0("\\b", input$in_mzz.ident, "\\b"), Münzzeichen),
                   grepl(input$in_abb.ident, Abbildung, ignore.case = TRUE))
    
    output$out_h3.ident <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ",
                                             "&nbsp;(", paste(unique(show$Art), collapse = ' + '), ")</h3>"))
    fkt_datadisplay(df = show, variation = "ident")
  })
  
  ## Unbekannte Auflagenstärke ----
  output$out_aufl.erf <- renderText(expr = er_auf.erf())
  er_auf.erf <- eventReactive(eventExpr = input$in_aufl.erf, valueExpr = form_aufl(input$in_aufl.erf))
  
  observeEvent(eventExpr = input$bt_do_erf.erf, handlerExpr = {
    if(fkt_do_ungltg.mid()) return()
    updateTextInput(session, inputId = "in_erf.erf", value = paste0(input$in_erf.erf, input$myselection, "-", input$in_aufl.erf, "\n"))
  })
  
  observeEvent(eventExpr = input$bt_write_aufl.erf, handlerExpr = {
    out <- input$in_erf.erf
    if(out == "") {
      showModal(modalDialog(title = "Fehler", paste0("Keine erfasste(n) Auflagenstärke(n)."), easyClose = TRUE, footer = NULL, size = "s"))
      return()
    }
    while(str_sub(out, -1) == "\n") out <- str_sub(out, 1, -2)
    
    write_lines(out, file = "eur2coins_circulation.txt", append = TRUE)
    
    new_rows <- read.table(text = out, sep = "-", col.names = c("ID", "Auflage"))
    val_circulation(bind_rows(val_circulation(), new_rows))
    
    updateTextInput(session, inputId = "in_erf.erf", value = "")
  })
  
  output$out_table.erf <- renderTable(expr = er_table.erf(), spacing = "xs", width = "100%", align = c("lllcrl"), sanitize.text.function = function(x) x)
  er_table.erf <- reactive({
    show <- filter(all_data(), is.na(Hfgkt))
    output$out_h3.erf <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
    fkt_datadisplay(df = show, variation = "uaufl")
  })
  
  ## Erfasste Auflagenstärke ----
  observeEvent(eventExpr = input$bt_reset_id.korr, handlerExpr = updateTextInput(session, inputId = "in_id.korr", value = ""))
  observeEvent(eventExpr = input$bt_reset_mzz.korr, handlerExpr = updateTextInput(session, inputId = "in_mzz.korr", value = ""))
  observeEvent(eventExpr = input$bt_reset_abb.korr, handlerExpr = updateTextInput(session, inputId = "in_abb.korr", value = ""))
  
  observeEvent(eventExpr = c(input$in_id.ident, input$in_mzz.ident, input$in_abb.ident), handlerExpr = {
    updateTextInput(session = session, inputId = "in_id.korr", value = input$in_id.ident)
    updateTextInput(session = session, inputId = "in_mzz.korr", value = input$in_mzz.ident)
    updateTextInput(session = session, inputId = "in_abb.korr", value = input$in_abb.ident)
  })
  
  output$out_aufl.korr <- renderText(expr = er_aufl.korr())
  er_aufl.korr <- eventReactive(eventExpr = input$in_aufl.korr, valueExpr = form_aufl(input$in_aufl.korr))
  
  observeEvent(eventExpr = input$bt_write_aufl.korr, handlerExpr = {
    if(fkt_do_ungltg.mid()) return()
    
    tmp <- val_circulation()
    tmp[tmp$ID == input$myselection, "Auflage"] <- as.numeric(input$in_aufl.korr)
    
    val_circulation(tmp)
    write_lines(paste(tmp$ID, tmp$Auflage, sep = "-"), "eur2coins_circulation.txt")
  })
  
  output$out_table.korr <- renderTable(expr = er_table.korr(), spacing = "xs", width = "100%", align = c("lllcrlrc"), sanitize.text.function = function(x) x)
  er_table.korr <- reactive({
    show <- filter(all_data(), !is.na(Hfgkt),
                   grepl(input$in_id.korr, ID, ignore.case = TRUE),
                   grepl(paste0("\\b", input$in_mzz.korr, "\\b"), Münzzeichen),
                   grepl(input$in_abb.korr, Abbildung, ignore.case = TRUE))
    output$out_h3.korr <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
    fkt_datadisplay(df = show, variation = "eaufl")
  })
  
  ## Ablage ----
  check_ablnr <- function(x) {
    maxi <- pull(count(val_collection())) # MIT KLAMMERN
    if (x == "bu") return (213L)
    if (x == "") return(1L)
    if (str_detect(x, "\\D+")) return (maxi)
    x <- as.integer(x)
    return(max(1, min(x, maxi)))
  }
  
  safe_ablnr <- reactiveVal()
  observeEvent(eventExpr = input$in_ablnr.abl, handlerExpr = {
    safe_ablnr(check_ablnr(input$in_ablnr.abl))
    updateSliderInput(session, inputId = "in_box.abl", value = (safe_ablnr() - 1) %/% 144 + 1)
    updateSliderInput(session, inputId = "in_tableau.abl", value = (safe_ablnr() - 1) %% 144 %/% 24 + 1)
    updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr())
  })
  
  observeEvent(eventExpr = input$bt_do_minus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr() - 1))
  observeEvent(eventExpr = input$bt_do_plus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr() + 1))
  observeEvent(eventExpr = input$bt_do_getablnr.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = check_ablnr(input$myselection)))
  
  output$out_tableau.abl <- renderTable(expr = er_tableau.abl(), spacing = "l", width = "90%", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  er_tableau.abl <- reactive({
    tmp <- val_collection() |> # MIT KLAMMERN
      filter(Zeilennummer %in% (((input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1) + 0:23)) |> 
      arrange(Zeilennummer) |>
      mutate(Qualität = fkt_form_quali(Qualität),
             Mark_start = case_when(input$in_ablnr.abl == Zeilennummer ~ "<div class = 'wahl'>", TRUE ~ ""),
             Mark_ende = case_when(input$in_ablnr.abl == Zeilennummer ~ "</div>", TRUE ~ ""),
             Res = paste0(Mark_start, "<dbwert><nobr class='mono'>", str_sub(Ablage, 1, 9 - nchar(Zeilennummer)), "&ZeroWidthSpace;", "<b><u>", str_sub(Ablage, 9 - nchar(Zeilennummer) + 1, 9), "</u></b>&ZeroWidthSpace;<br>",
                          "<b>", str_sub(ID, 1, 7), "</b>", str_sub(ID, 8, 9), "</nobr></dbwert><br>",
                          Qualität, Mark_ende)) |>
      pull(Res)
    
    if(length(tmp) < 24) tmp <- c(tmp, rep("<br><div class='mono'><i>l&nbsp;e&nbsp;e&nbsp;r</i></div><br>", 24 - length(tmp)))
    
    output$out_h3tableau.abl <- renderText(expr = paste0("<h3>Box ", input$in_box.abl, ", Tableau ", input$in_tableau.abl, ": Ablagenummern ",
                                                         (input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1, " bis ",
                                                         (input$in_box.abl - 1) * 144 + input$in_tableau.abl * 24, "</h3>"))
    output$out_h3aktmz.abl <- renderText(expr = paste0("<h3>Ablagenummer: ", input$in_ablnr.abl, "</h3>"))
    
    matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
           dimnames = list(paste0("<b>", input$in_box.abl, input$in_tableau.abl, 1:4, "&#0133;", "</b>"),
                           paste0("&#0133;", 1:6)))
  })
  
  output$out_aktmz.abl <- renderTable(expr = er_aktmz.abl(), spacing = "l", width = "90%", align = "c", rownames = FALSE, sanitize.text.function = function(x) x)
  er_aktmz.abl <- reactive({
    output$out_h3aktmz.abl <- renderText(expr = paste0("<h3>Ablagenummer: ", safe_ablnr(), "</h3>"))
    show <- all_data() |> mutate(Zeile = as.integer(str_sub(Ablage, 6, 9))) |> filter(Zeile == safe_ablnr())
    output$out_cpic.abl <- renderImage(list(src = show$PicFile, contentType = "image/png", width = 125), deleteFile = FALSE)
    fkt_datadisplay(df = show, variation = "ablage")
  })
  
  ## Statistik ----
  output$out_jahr.stat <- renderTable(expr = er_jahr.stat(), spacing = "xs", align = c("rrrl"), sanitize.text.function = function(x) x)
  er_jahr.stat <- reactive({ fkt_form_stat("Jahr", 1, 4) })
  
  output$out_land.stat <- renderTable(expr = er_land.stat(), spacing = "xs", align = c("lrrl"), sanitize.text.function = function(x) x)
  er_land.stat <- reactive({
    fkt_form_stat("Land", 5, 6) |> mutate(Land = fkt_form_land(Land))
  })
  
  output$out_art.stat <- renderTable(expr = er_art.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_art.stat <- reactive({
    filter(all_data(), !is.na(Ablage)) |> 
      group_by(Art = Art |> ordered(levels = c("G", "K"), labels = fkt_form_art(c("G", "K"))), .drop = FALSE) |> 
      count() |> 
      transmute(Anzahl = n,
                Anteil = formatC(Anzahl / dim(val_collection())[1] * 100, 2, format = "f", decimal.mark = ",")) # MIT KLAMMERN
  })
  
  output$out_qual.stat <- renderTable(expr = er_qual.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_qual.stat <- reactive({
    filter(all_data(), !is.na(Ablage)) |> 
      group_by(Qualität = Qualität |> ordered(levels = 0:3, labels = fkt_form_quali(0:3)), .drop = FALSE) |> 
      count() |> 
      transmute(Anzahl = n,
                Anteil = formatC(Anzahl / dim(val_collection())[1] * 100, 2, format = "f", decimal.mark = ",")) # MIT KLAMMERN
  })
  
  output$out_hfgkt.stat <- renderTable(expr = er_hfgkt.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_hfgkt.stat <- reactive({
    filter(all_data(), !is.na(Ablage)) |> 
      group_by(Häufigkeit = Hfgkt |> ordered(levels = 5:1, labels = fkt_form_hfgkt(5:1)), .drop = FALSE) |> 
      count() |> 
      transmute(Anzahl = n,
                Anteil = formatC(Anzahl / dim(val_collection())[1] * 100, 2, format = "f", decimal.mark = ",")) # MIT KLAMMERN
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
