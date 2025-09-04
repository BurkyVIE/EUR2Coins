# GLOBAL ----
## Libraries ----
library(shiny)
library(bslib)

## Laden der externe Daten ----
source("rd_celex.r")       #celex
source("rd_circulation.r") #circulation
source("rd_collection.r")  #collection
source("mk_cpic.r")        #cpic

## Zusammenführen und behübschen der Daten - all_data() ----
all_data <- function() {
  Reduce(function(...) merge(..., by = "ID", all.x = TRUE, no.dups = TRUE),
         list(coins, select(collection, ID, Qualität, Ablage), circulation, select(filter(cpic, Exists), ID, PicFile))) |> 
    as_tibble()
}

## JS Funktion um Markierung verfügbar zu machen ----
fkt_highlight <- '
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
# UI (User Interface) ----
ui <- page_fluid(includeCSS(path = "style_fwd.css"),
  tags$script(fkt_highlight),
  navset_pill(
    ## Identifikation ----
    nav_panel(title = "Identifikation",
      # h1("🙤 Identifikation 🙧"),
      ### Identifikation Sidebar ----
      page_sidebar(
        sidebar = sidebar(width = "27%", position = "left", open = "always",
          h2("Filter"),
          fluidRow(
            h3("Münzen"),
            radioButtons(inputId = "in_smlg.ident", label = NULL, inline = TRUE,
                         choices = c("Alle" = "alle",
                                     "Vorhandene" = "ja",
                                     "Fehlende" = "nein")),
            # HTML("<div class = 'beschr'>"), "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"), "im Sinne von egal / vorhanden / leer", HTML('</div>')),
            div(class = 'beschr', "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"), "im Sinne von egal / vorhanden / leer")),
          fluidRow(
            column(width = 6,
              h3("Münz ID"),
              fluidRow(
                column(width = 8, textInput(inputId = "in_id.ident", label = NULL, value = "", width = "100%")),
                column(width = 4, actionButton(inputId = "bt_reset_id.ident", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
              div(class = 'beschr', "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"), " = Prägejahr",
                  ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen")),
            column(width = 6,
              h3("Münzzeichen"),
              fluidRow(
                column(width = 8, selectInput(inputId = "in_mzz.ident", label = NULL, choices = unique(c(c("", "A", "D", "F", "G", "J"), all_data()$Münzzeichen)), selected = NULL, width = "100%")),
                column(width = 4, actionButton(inputId = "bt_reset_mzz.ident", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
              div(class = 'beschr', "Auswahl aus Liste; Genaue Übereinstimmung mit ", em("Mzz")))),
          fluidRow(
            h3("Abbildung"),
            column(width = 10, textInput(inputId = "in_abb.ident", label = NULL, value = "", width = "100%")),
            column(width = 2, actionButton(inputId = "bt_reset_abb.ident", label = "✗", width = "100%", style = "padding:6px;")), # &cross;
            div(class = 'beschr', "Beliebige Übereinstimmung mit ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert")),
          h2("Bearbeitung"),
          fluidRow(
            h3("Qualität"),
            column(width = 3, actionButton(inputId = "bt_write_q0.ident", label = "(0) ★★★", width = "100%", style = "padding:6px;")), # &starf;
            column(width = 3, actionButton(inputId = "bt_write_q1.ident", label = "(1) ☆★★", width = "100%", style = "padding:6px;")), # &star; 
            column(width = 3, actionButton(inputId = "bt_write_q2.ident", label = "(2) ☆☆★", width = "100%", style = "padding:6px;")), 
            column(width = 3, actionButton(inputId = "bt_write_q3.ident", label = "(3) ☆☆☆", width = "100%", style = "padding:6px;")), 
            p(div(class = 'beschr', "[...] Übernimmt markierte ", em("Münz ID"), "und ändert/ergänzt gewählte Qualität im File eur2collection.txt"))),
          fluidRow(
            h3("eur2collection.txt"),
            column(width = 3),
            column(width = 5, actionButton(inputId = "bt_do_aend.ident", label = "Neu laden", width = "100%", style = "padding:6px;")),
            column(width = 3),
            p(div(class = 'beschr', "[Neu laden] lädt File eur2collection.txt neu, z.B. nach manuellen Änderungen")))),
          ### Identifikation Main ----
          h2("Ergebnis entsprechend Filter", .noWS = "before"),
          fluidRow(
            htmlOutput(outputId = "out_h3.ident"),
            div(class = 'longtab', tableOutput(outputId = "out_table.ident"))))),
    ## Auflagenstärke ----
    nav_menu(title = "Auflage",
      # h1("🙤 Auflagenstärke 🙧"), # Könnte hier stehen, führt zu Warnung, daher 2x jeweils am Beginn einer page
      nav_panel(title = "Auflagenstärke erfassen",
        ### Auflagenstärke erfassen Sidebar ----
        page_sidebar(
          # h1("🙤 Auflagenstärke 🙧"),
          sidebar = sidebar(width = "27%", position = "left", open = "always",
            h2("Bearbeitung"),
            fluidRow(
              h3("Auflagenstärke"),
              column(width = 7,
                textInput(inputId = "in_aufl.erf", label = NULL, value = "", width = "100%")),
              column(width = 5,
              htmlOutput(outputId = "out_aufl.erf", inline = TRUE))),
            fluidRow(
              h3("Erfassen"),
              column(width = 6,
                textAreaInput(inputId = "in_erf.erf", label = NULL, rows = 11, resize = "none", width = "100%")),
              column(width = 6,
                actionButton(inputId = "bt_do_erf.erf", label = "Erfassen", width = "100%", style = "padding:6px;"),
                p(div(class = 'beschr', "[Erfassen] überträgt den Wert Auflagenstärke gemeinsam mit markierter ", em("Münz ID"), "in Eingabebereich")))),
            fluidRow(
              h3("Speichern"),
              column(width = 3),
              column(width = 6, actionButton(inputId = "bt_write_aufl.erf", label = "Speichern", width = "100%", style = "padding:6px;"),
              column(width = 3)),
              p(div(class = 'beschr', "[Speichern] schreibt Werte aus Eingabebereich ins File eur2coins_circulation.txt")))),
          ### Auflagenstärke erfassen Main ----
          h2("Unbekannte Auflagenstärke", .noWS = "before"),
          fluidRow(
            htmlOutput(outputId = "out_h3.erf"),
            div(class = 'longtab', tableOutput(outputId = "out_table.erf"))))),
      nav_panel(title = "Auflagenstärke korrigieren",
        ### Auflagenstärke korrigieren Sidebar ----
        page_sidebar(
          # h1("🙤 Auflagenstärke 🙧"),
          sidebar = sidebar(width = "27%", position = "left", open = "always",
            h2("Filter"),
            fluidRow(
              column(width = 6,
                h3("Münz ID"),
                fluidRow(
                  column(width = 8, textInput(inputId = "in_id.korr", label = NULL, value = "", width = "100%")),
                  column(width = 4, actionButton(inputId = "bt_reset_id.korr", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                div(class = 'beschr', "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"), " = Prägejahr",
                    ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen")),
              column(width = 6,
                h3("Münzzeichen"),
                fluidRow(
                  column(width = 8, selectInput(inputId = "in_mzz.korr", label = NULL, choices = unique(c(c("", "A", "D", "F", "G", "J"), all_data()$Münzzeichen)),
                                                selected = NULL, width = "100%")),
                  column(width = 4, actionButton(inputId = "bt_reset_mzz.korr", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                div(class = 'beschr', "Auswahl aus Liste; Genaue Übereinstimmung mit ", em("Mzz")))),
            fluidRow(
              h3("Abbildung"),
              column(width = 10, textInput(inputId = "in_abb.korr", label = NULL, value = "", width = "100%")),
              column(width = 2, actionButton(inputId = "bt_reset_abb.korr", label = "✗", width = "100%", style = "padding:6px;")), # &cross;
              div(class = 'beschr', "Beliebige Übereinstimmung ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert")),
            h2("Bearbeitung"),
            fluidRow(
              h3("Auflagenstärke"),
              column(width = 7,
                textInput(inputId = "in_aufl.korr", label = NULL, value = "", width = "100%")),
              column(width = 5,
                htmlOutput(outputId = "out_aufl.korr", inline = TRUE))),
            fluidRow(
              h3("Speichern"),
              column(width = 3),
              column(width = 6, actionButton(inputId = "bt_write_aufl.korr", label = "Speichern", width = "100%", style = "padding:6px;")),
              column(width = 3),
                p(div(class = 'beschr', "Wert Auflagenstärke wird gemeinsam mit markierter ", em("Münz ID"), " im File  eur2coins_collection.txt geändert")))),
        ### Auflagenstärke korrigieren Main ----
        h2("Ergebnis entsprechend Filter", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3.korr"),
          div(class = 'longtab', tableOutput(outputId = "out_table.korr")))))),
    ## Ablage ----
    nav_panel(title = "Ablage",
      # h1("🙤 Ablage 🙧"),
      ### Ablage Sidebar ----
      page_sidebar(
        sidebar = sidebar(width = "27%", position = "left", open = "always",
          h2("Auswahl Box und Tableau"),
          fluidRow(
            column(width = 6,
              h3("Box"),
              sliderInput(inputId = "in_box.abl", label = NULL, min = 1, max = 5, value = 1, step = 1, width = "100%"), # NEUE BOX HIER
              HTML("<div class = 'beschr'>"), "Auswahl Ablagebox", HTML('</div>')),
            column(width = 6,
              h3("Tableau"),
              sliderInput(inputId = "in_tableau.abl", label = NULL, min = 1, max = 6, value = 1, step = 1, width = "100%"),
              div(class = 'beschr', "Auswahl Tableau in gewählter Ablagebox"))),
          h2("Auswahl Münze"),
          fluidRow(
            h3("Ablagenummer"),
            column(width = 2, actionButton(inputId = "bt_do_minus.abl", label = "≺", width = "100%", style = "padding:6px;")), # &prec;
            column(width = 2, actionButton(inputId = "bt_do_plus.abl", label = "≻", width = "100%", style = "padding:6px;")), # &succ;
            column(width = 5, textInput(inputId = "in_ablnr.abl", value = pull(count(collection)), label = NULL, width = "100%")), #pull(count(collection))
            column(width = 3, actionButton(inputId = "bt_do_getablnr.abl", label = "gehe zu", width = "100%", style = "padding:6px;")),
            div(class = 'beschr', "[≺] navigiert zur vorherigen (-1), [≻] zur nächsten (+1) Münze; ",
                "[gehe zu] übernimmt markierten unterstrichenen Teil im Tableau oder springt zur letzten abgelegten Münze"))),
        ### Ablage Main ----
        h2("Aktives Tableau", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3tableau.abl"),
          div(class = 'matrix', tableOutput(outputId = "out_tableau.abl"))),
        h2("Aktive Münze", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3aktmz.abl"),
          tableOutput(outputId = "out_aktmz.abl")),
        fluidRow(
          div(align = "center", imageOutput(outputId = "out_cpic.abl"))
        ))),
    ## Statistik ----
    nav_panel(title = "Statistik",
      # h1("🙤 Statistik 🙧"),
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
  
  ## Funktionen ----
  ### Fkt Reload ----
  fkt_reload <- function() {
    # celex wird für gewöhnlich nicht im Laufen bearbeitet
    source("rd_circulation.r")
    source("rd_collection.r")
  }
  
  ### Fkt Formatieren Zahlen > 1k ----
  ### v.a. in den Unter-Überschriften und bei Auflagenstärken
  fkt_form_tsd <- function (x) format(as.numeric(x), big.mark = "&nbsp;", scientific = FALSE)
  
  ### Fkt Formatieren Land ----
  fkt_form_land <- function(txt) {
    txt <- tolower(txt) # jedenfalls Kleinbuchstaben
    paste0("<nobr class = 'flag'><img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif',
           height='15', alt='", toupper(txt), "'>&nbsp;&nbsp;(", toupper(txt), ")</nobr>")
  }
  
  ### Fkt Formatieren Amtsblatt ----
  fkt_form_amtsbl <- function(txt) {
    url <- paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:", txt, "', target = '_blank'>", txt, "</a>")
    url <- str_replace(url, "\\(", "%28")
    url <- str_replace(url, "\\)", "%29")
    return(url)
  }
  
  ### Fkt Formatieren Art (Münzart) ----
  fkt_form_art <- function(txt) {
    txt[txt == "G"] <- "<span>&#10629;&#120022;&#10630;</span>" # Ⓖ
    txt[txt == "K"] <- "<span>&#10629;&#120026;&#10630;</span>" # Ⓚ
    return(txt)
  }
  
  ### Fkt Formatieren Häufigkeit ----
  fkt_form_hfgkt <- function(txt) {
    c("<nobr class='rare1'>&emsp;&#9660;&emsp;</nobr>",
      "<nobr class='rare2'>&emsp;&#9661;&emsp;</nobr>",
      "<nobr class='rare3'>&emsp;&#9634;&emsp;</nobr>",
      "<nobr class='rare4'>&emsp;&#9651;&emsp;</nobr>",
      "<nobr class='rare5'>&emsp;&#9650;&emsp;</nobr>")[txt]
  }
  
  ### Fkt Formatieren Qualität ----
  fkt_form_quali <- function(x) {
    case_when(is.na(x) ~ "",
              x == 0 ~ "<nobr class = 'q0'>(0)&nbsp;&starf;&starf;&starf;</nobr>",
              x == 1 ~ "<nobr class = 'q1'>(1)&nbsp;&star;&starf;&starf;</nobr>",
              x == 2 ~ "<nobr class = 'q2'>(2)&nbsp;&star;&star;&starf;</nobr>",
              x == 3 ~ "<nobr class = 'q3'>(3)&nbsp;&star;&star;&star;</nobr>",
              TRUE ~ "<nobr class = 'qF'><i>&nbsp;FEHLER!&nbsp;<i></nobr>")
  }
  
  ### Fkt Darstellung Daten (switch)----
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
           ident = df |> transmute('Münz ID' = ID,
                                   Jahr,
                                   Land,
                                   Art,
                                   Mzz = Münzzeichen,
                                   Abbildung,
                                   Hfgkt,
                                   Amtsblatt,
                                   Qualität,
                                   Ablage),
           uaufl = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung) |> 
             arrange(Land),
           eaufl = df |> transmute('Münz ID' = ID, Jahr, Land, Art, Mzz = Münzzeichen, Abbildung, Auflage = fkt_form_tsd(Auflage), Hfgkt) |> 
             arrange('Münz ID'))
  }
  
  ### Fkt Darstellung Statistik ----
  fkt_form_stat <- function(val, von, bis) {
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
  
  ### Fkt Prüfe Gültigkeit markierter Münz ID und ggf Message ----
  fkt_do_ungltg.mid <- function()
    if(str_detect(input$myselection, "\\d{4}[a-z]{2}[g|k]\\d{2}")) return(FALSE)
  else {
    showModal(modalDialog(
    title = "Fehler",
    paste0("Keine gültige Münz ID markiert."),
    easyClose = TRUE,
    footer = NULL,
    size = "s"))
    return(TRUE)
    }
  
  ### Fkt Schreiben/Ändern einer Bewertung ----
  fkt_write_bewertung <- function(qu) {
    # Gültigkeit Münz ID
    if(fkt_do_ungltg.mid()) return()
    # Abbild des Files
    tmp <- select(collection, ID, Qualität)
    # Ändern oder Anfügen
    if(input$myselection %in% tmp$ID) {
      tmp[tmp$ID == input$myselection, "Qualität"] <- qu
      write_lines(
        paste(tmp$ID, tmp$Qualität, sep = "-"),
        "eur2coins_collection.txt")
    }
    else {
      tmp <- add_row(tmp, ID = input$myselection, Qualität = qu)
      write_lines(
        paste(tmp$ID, tmp$Qualität, sep = "-"),
        "eur2coins_collection.txt")
    }
    fkt_reload()
  }
  
  ### Fkt Formatieren Auflagenstärke ----
  form_aufl <- function(x)
    paste0("<div style='text-align: left; margin-top: 7px'>=&nbsp;<b>",fkt_form_tsd(x), "</b>&nbsp;</div>")
  
  ## Page Identifikation ----
  ### Reset Buttons ----  
  observeEvent(eventExpr = input$bt_reset_id.ident, handlerExpr = updateTextInput(session, inputId = "in_id.ident", value = ""))
  observeEvent(eventExpr = input$bt_reset_mzz.ident, handlerExpr = updateTextInput(session, inputId = "in_mzz.ident", value = ""))
  observeEvent(eventExpr = input$bt_reset_abb.ident, handlerExpr = updateTextInput(session, inputId = "in_abb.ident", value = ""))
  
  ### Bewertungs Buttons ----
  observeEvent(eventExpr = input$bt_write_q0.ident, handlerExpr = fkt_write_bewertung(0))
  observeEvent(eventExpr = input$bt_write_q1.ident, handlerExpr = fkt_write_bewertung(1))
  observeEvent(eventExpr = input$bt_write_q2.ident, handlerExpr = fkt_write_bewertung(2))
  observeEvent(eventExpr = input$bt_write_q3.ident, handlerExpr = fkt_write_bewertung(3))
  
  ### Reload Button ----
  observeEvent(eventExpr = input$bt_do_aend.ident, handlerExpr = fkt_reload())
  
  ### Ausgabe Ergebnis ----
  output$out_table.ident <- renderTable(expr = er_tabl.ident(), spacing = "xs", width = "100%", align = c("lllcrlclcl"), sanitize.text.function = function(x) x)
  er_tabl.ident <- eventReactive(eventExpr = c(input$in_smlg.ident, input$in_id.ident, input$in_mzz.ident, input$in_abb.ident,
                                               input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                 valueExpr = {
                                   # Anzuzeigende Münzen
                                   show <- filter(all_data(), (Ablage != " " | input$in_smlg.ident != "ja"), (is.na(Ablage) | input$in_smlg.ident != "nein"),
                                                  grepl(tolower(input$in_id.ident), ID),
                                                  grepl(paste0("\\b", input$in_mzz.ident, "\\b"), Münzzeichen), #('\\b', - Regex word boundary)
                                                  grepl(tolower(input$in_abb.ident), tolower(Abbildung)))
                                   # Anzahl Münzen n (Überschrift inkl Plural)
                                   output$out_h3.ident <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ",
                                                                            "&nbsp;(", paste(unique(show$Art), collapse = ' + '), ")</h3>"))
                                   # Ausgabe Ergebnisse Münzen
                                   fkt_datadisplay(df = show, variation = "ident")
                                   })

  ## Unbekannte Auflagenstärke ----
  ### Ausgabe Zahl Auflagenstärke ----
  output$out_aufl.erf <- renderText(expr = er_auf.erf())
  er_auf.erf <- eventReactive(eventExpr = input$in_aufl.erf, valueExpr = form_aufl(input$in_aufl.erf))
  
  ## Auflage  Buttons ----
  ### Vorerfassen Auflagenstärke ---
  observeEvent(eventExpr = input$bt_do_erf.erf,
               handlerExpr = {
                 # Gültigkeit Münz ID
                 if(fkt_do_ungltg.mid()) return()
                 updateTextInput(session, inputId = "in_erf.erf", value = paste0(input$in_erf.erf, input$myselection, "-", input$in_aufl.erf, "\n"))
               })
  
  ### Schreiben der vorerfassten Werte ----
  observeEvent(eventExpr = input$bt_write_aufl.erf, handlerExpr = {
    out <- input$in_erf.erf
    if(out == "") {
      showModal(modalDialog(
        title = "Fehler",
        paste0("Keine erfasste(n) Auflagenstärke(n)."),
        easyClose = TRUE,
        footer = NULL,
        size = "s"))
      return()
    }
    while(str_sub(out, -1) == "\n") out <- str_sub(out, 1, -2)
    write_lines(out, file = "eur2coins_circulation.txt", append = TRUE)
    fkt_reload()
    updateTextInput(session, inputId = "in_erf.erf", value = "")
  })
  
  ## Ausgabe Unbekannte Auflagenstärke ----
  output$out_table.erf <- renderTable(expr = er_table.erf(), spacing = "xs", width = "100%", align = c("lllcrl"), sanitize.text.function = function(x) x)
  er_table.erf <- eventReactive(eventExpr = c(input$bt_write_aufl.erf, input$bt_do_aend.ident),
                                  valueExpr = {
                                    # Anzuzeigende Münzen
                           show <- filter(all_data(), is.na(Hfgkt))
                           # Anzahl Münzen n (Überschrift inkl Plural)
                           output$out_h3.erf <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
                           # Ausgabe Ergebnisse Münzen
                           fkt_datadisplay(df = show, variation = "uaufl")
                         })
  
  ## Erfasste Auflagenstärke ----
  ### Reset Buttons ----  
  observeEvent(eventExpr = input$bt_reset_id.korr, handlerExpr = updateTextInput(session, inputId = "in_id.korr", value = ""))
  observeEvent(eventExpr = input$bt_reset_mzz.korr, handlerExpr = updateTextInput(session, inputId = "in_mzz.korr", value = ""))
  observeEvent(eventExpr = input$bt_reset_abb.korr, handlerExpr = updateTextInput(session, inputId = "in_abb.korr", value = ""))
  
  ### Synchonisiere Filter mit Identifikation ----
  observeEvent(eventExpr = c(input$in_id.ident, input$in_mzz.ident, input$in_abb.ident),
               handlerExpr = {
                 updateTextInput(session = session, inputId = "in_id.korr", value = input$in_id.ident)
                 updateTextInput(session = session, inputId = "in_mzz.korr", value = input$in_mzz.ident)
                 updateTextInput(session = session, inputId = "in_abb.korr", value = input$in_abb.ident)
                 })
  
  ### Ausgabe Zahl Auflagenstärke----
  output$out_aufl.korr <- renderText(expr = er_aufl.korr())
  er_aufl.korr <- eventReactive(eventExpr = input$in_aufl.korr, valueExpr = form_aufl(input$in_aufl.korr))

  ### Schreiben einer neuen/korrigierten Auflagenstärke ----
  observeEvent(eventExpr = input$bt_write_aufl.korr,
               handlerExpr = {
                 # Gültigkeit Münz ID
                 if(fkt_do_ungltg.mid()) return()
                 # Abbild des Files
                 tmp <- select(circulation, ID, Auflage)
                 # Ändern
                 tmp[tmp$ID == input$myselection, "Auflage"] <- as.numeric(input$in_aufl.korr)
                 # Schreiben
                 write_lines(paste(tmp$ID, tmp$Auflage, sep = "-"), "eur2coins_circulation.txt")
                 fkt_reload()
                 })
  
  ### Ausgabe Ergebnis ----
  output$out_table.korr <- renderTable(expr = er_table.korr(), spacing = "xs", width = "100%", align = c("lllcrlrc"), sanitize.text.function = function(x) x)
  er_table.korr <- eventReactive(eventExpr = c(input$in_id.korr, input$in_mzz.korr, input$in_abb.korr, input$bt_write_aufl.korr),
                                valueExpr = {
                                  # Anzuzeigende Münzen
                                  show <- filter(all_data(), !is.na(Hfgkt),
                                                 grepl(tolower(input$in_id.korr), ID),
                                                 grepl(paste0("\\b", input$in_mzz.korr, "\\b"), Münzzeichen),
                                                 grepl(tolower(input$in_abb.korr), tolower(Abbildung)))
                                  # Anzahl Münzen n (Überschrift inkl Plural)
                                  output$out_h3.korr <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
                                  # Ausgabe Ergebnisse Münzen
                                  fkt_datadisplay(df = show, variation = "eaufl")
                                  })
  
  ## Ablage ----
  ### Fkt Gültigkeitsprüfung Eingabe Ablagenummer ----
  check_ablnr <- function(x) {
    maxi <- pull(count(collection))
    if (x == "bu") return (213L)
    if (x == "") return(1L)
    if (str_detect(x, "\\D+")) return (maxi)
    x <- as.integer(x)
    
    return(max(1, min(x, maxi)))
  }
  
  ### Schieberegler anpassen und sichere Ablagenummer erzeugen ----
  safe_ablnr <- reactiveVal()
  observeEvent(eventExpr = input$in_ablnr.abl, handlerExpr = {
    # Sichere Ablagenummer
    safe_ablnr(check_ablnr(input$in_ablnr.abl))
    # Schieberegeler
    updateSliderInput(session, inputId = "in_box.abl", value = (safe_ablnr() - 1) %/% 144 + 1)
    updateSliderInput(session, inputId = "in_tableau.abl", value = (safe_ablnr() - 1) %% 144 %/% 24 + 1)
    # Zurückschreiben der sicheren Ablagenummer ins Eingabefeld
    updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr())
  })
  
  ### Schnellwahl Schritte ----
  observeEvent(eventExpr = input$bt_do_minus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr() - 1))
  observeEvent(eventExpr = input$bt_do_plus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = safe_ablnr() + 1))
  
  ### Schnellwahl Markierung übernehmen ----
  observeEvent(eventExpr = input$bt_do_getablnr.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = check_ablnr(input$myselection)))
  
  ### Ausgabe Ablage inkl Detail f Münze ----
  output$out_tableau.abl <- renderTable(expr = er_tableau.abl(), spacing = "l", width = "90%", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  er_tableau.abl <- eventReactive(eventExpr = c(input$in_box.abl, input$in_tableau.abl, input$in_ablnr.abl,
                                                input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                  valueExpr = {
                                    # Anzeige Tableau
                                    tmp <- collection |> 
                                      filter(Zeilennummer %in% (((input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1) + 0:23)) |> 
                                      arrange(Zeilennummer) |>
                                      mutate(Qualität = fkt_form_quali(Qualität),
                                             Mark_start = case_when(input$in_ablnr.abl == Zeilennummer ~ "<div class = 'wahl'>", TRUE ~ ""),
                                             Mark_ende = case_when(input$in_ablnr.abl == Zeilennummer ~ "</div>", TRUE ~ ""),
                                             Res = paste0(Mark_start, "<dbwert><nobr class='mono'>", str_sub(Ablage, 1, 9 - nchar(Zeilennummer)), "&ZeroWidthSpace;", "<b><u>", str_sub(Ablage, 9 - nchar(Zeilennummer) + 1, 9), "</u></b>&ZeroWidthSpace;<br>", # zeroWidht wegen Doppelklick-Markierung der Ablagenummer
                                                          "<b>", str_sub(ID, 1, 7), "</b>", str_sub(ID, 8, 9), "</nobr></dbwert><br>",
                                                          Qualität, Mark_ende)) |>
                                      pull(Res)
                                    if(length(tmp) < 24) tmp <- c(tmp, rep("<br><div class='mono'><i>l&nbsp;e&nbsp;e&nbsp;r</i></div><br>", 24 - length(tmp)))
                                    # Ablageadresse (Überschrift)
                                    output$out_h3tableau.abl <- renderText(expr = paste0("<h3>Box ", input$in_box.abl, ", Tableau ", input$in_tableau.abl, ": Ablagenummern ",
                                                                                         (input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1, " bis ",
                                                                                         (input$in_box.abl - 1) * 144 + input$in_tableau.abl * 24, "</h3>"))
                                    # Ablagenummer (Überschrift)
                                    output$out_h3aktmz.abl <- renderText(expr = paste0("<h3>Ablagenummer: ", input$in_ablnr.abl, "</h3>"))
                                    # Ausgabe
                                    matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
                                           dimnames = list(paste0("<b>", input$in_box.abl, input$in_tableau.abl, 1:4, "&#0133;", "</b>"),
                                                           paste0("&#0133;", 1:6)))
                                    },
                                  ignoreNULL = FALSE)

  ### Ausgabe aktive Münze ----
  output$out_aktmz.abl <- renderTable(expr = er_aktmz.abl(), spacing = "l", width = "90%", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  er_aktmz.abl <- eventReactive(eventExpr = c(safe_ablnr(), #input$in_ablnr.abl,
                                              input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                  valueExpr = {
                                    # Ablagenummer (Überschrift)
                                    output$out_h3aktmz.abl <- renderText(expr = paste0("<h3>Ablagenummer: ", safe_ablnr(), "</h3>"))
                                    # Anzuzeigende Münzdetails
                                    show <- all_data() |> mutate(Zeile = as.integer(str_sub(Ablage, 6, 9))) |> filter(Zeile == safe_ablnr())
                                    # Bild
                                    output$out_cpic.abl <- renderImage(list(src = show$PicFile, contentType = "image/png", width = 150), deleteFile = FALSE)
                                    # Ausgabe
                                    fkt_datadisplay(df = show, variation = "ident")
                                  },
                                  ignoreNULL = FALSE)
  
    
  ## Statistik ----
  ### Ausgabe Zusammenfassung Jahr ----
  output$out_jahr.stat <- renderTable(expr = er_jahr.stat(), spacing = "xs", align = c("rrrl"), sanitize.text.function = function(x) x)
  er_jahr.stat <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                valueExpr = fkt_form_stat("Jahr", 1, 4),
                                ignoreNULL = FALSE)
  
  ### Ausgabe Zusammenfassung Land ----
  output$out_land.stat <- renderTable(expr = er_land.stat(), spacing = "xs", align = c("lrrl"), sanitize.text.function = function(x) x)
  er_land.stat <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                valueExpr = fkt_form_stat("Land", 5, 6) |> 
                                  mutate(Land = fkt_form_land(Land)),
                                ignoreNULL = FALSE)
  
  ### Ausgabe Zusammenfassung Münzart ----
  output$out_art.stat <- renderTable(expr = er_art.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_art.stat <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                               valueExpr = {
                                 filter(all_data(), !is.na(Ablage)) |> 
                                   group_by(Art = Art |>  ordered(levels = c("G", "K"), labels = fkt_form_art(c("G", "K"))), .drop = FALSE) |> 
                                   count() |> 
                                   transmute(Anzahl = n,
                                             Anteil = formatC(Anzahl / dim(collection)[1] * 100, 2, format = "f", decimal.mark = ","))
                               },
                               ignoreNULL = FALSE)

  ### Ausgabe Zusammenfassung Qualität ----
  output$out_qual.stat <- renderTable(expr = er_qual.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_qual.stat <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                valueExpr = {
                                  filter(all_data(), !is.na(Ablage)) |> 
                                    group_by(Qualität = Qualität |>  ordered(levels = 0:3, labels = fkt_form_quali(0:3)), .drop = FALSE) |> 
                                    count() |> 
                                    transmute(Anzahl = n,
                                              Anteil = formatC(Anzahl / dim(collection)[1] * 100, 2, format = "f", decimal.mark = ","))
                                  },
                                ignoreNULL = FALSE)
  
  ### Ausgabe Zusammenfassung Häufigkeit ----
  output$out_hfgkt.stat <- renderTable(expr = er_hfgkt.stat(), spacing = "xs", align = c("crr"), sanitize.text.function = function(x) x)
  er_hfgkt.stat <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                 valueExpr = {
                                   filter(all_data(), !is.na(Ablage)) |> 
                                     group_by(Häufigkeit = Hfgkt |>  ordered(levels = 5:1, labels = fkt_form_hfgkt(5:1)), .drop = FALSE) |> 
                                     count() |> 
                                     transmute(Anzahl = n,
                                               Anteil = formatC(Anzahl / dim(collection)[1] * 100, 2, format = "f", decimal.mark = ","))
                                   },
                                 ignoreNULL = FALSE)

}

# Run the application ----
shinyApp(ui = ui, server = server)