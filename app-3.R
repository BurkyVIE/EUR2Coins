# GLOBAL ----
## Libraries ----
library(shiny)
library(bslib)

## Laden der externe Daten ----
source("rd_celex.r")       #celex
source("rd_circulation.r") #circulation
source("rd_collection.r")  #collection

## Zusammenführen und behübschen der Daten - all_data() ----
all_data <- function() {
  left_join(coins,
            collection %>% select(ID, Qualität, Ablage),
            by = 'ID') |> 
    mutate(Ablage = coalesce(Ablage, " ")) |> 
    left_join(circulation, by = join_by(ID))
}

## JS Funktion um Markierung verfügbar zu machen ----
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
# UI (User Interface) ----
ui <- page_fluid(includeCSS(path = "style_fwd.css"),
  tags$script(highlight),
  navset_pill(
    ## Identifikation ----
    nav_panel(title = "Identifikation",
      # h1("🙤 Identifikation 🙧"),
      ### Identifikation Sidebar ----
      page_sidebar(
        sidebar = sidebar(width = "30%", position = "left", open = "always",
          h2("Filter"),
          fluidRow(
            h3("Münzen"),
            radioButtons(inputId = "in_smlg.ident", label = NULL, inline = TRUE,
                         choices = c("Alle" = "alle",
                                     "Vorhandene" = "ja",
                                     "Fehlende" = "nein")),
            HTML("<div class = 'beschr'>"), "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"),
            "im Sinne von egal / vorhanden / leer", HTML('</div>')),
          fluidRow(
            column(width = 6,
              h3("Münz ID"),
              fluidRow(
                column(width = 8, textInput(inputId = "in_id.ident", label = NULL, value = "", width = "100%")),
                column(width = 4, actionButton(inputId = "bt_reset_id.ident", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
              HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"),
                ", wobei ", code("JJJJ"), " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", 
                code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen", HTML('</div>')),
            column(width = 6,
              h3("Münzzeichen"),
              fluidRow(
                column(width = 8, selectInput(inputId = "in_mzz.ident", label = NULL, choices = unique(c(c("", "A", "D", "F", "G", "J"), all_data()$Münzzeichen)), selected = NULL, width = "100%")),
                column(width = 4, actionButton(inputId = "bt_reset_mzz.ident", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
              HTML("<div class = 'beschr'>"), "Auswahl aus Liste; Genaue Übereinstimmung mit Feld ", em("Mzz"), HTML('</div>'))),
          fluidRow(
            h3("Abbildung"),
              column(width = 10, textInput(inputId = "in_abb.ident", label = NULL, value = "", width = "100%")),
              column(width = 2, actionButton(inputId = "bt_reset_abb.ident", label = "✗", width = "100%", style = "padding:6px;")), # &cross;
            HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert", HTML('</div>')),
          h2("Bearbeitung"),
          fluidRow(
            h3("Qualität"),
              column(width = 3, actionButton(inputId = "bt_write_q0.ident", label = "(0) ★★★", width = "100%", style = "padding:6px;")), # &starf;
              column(width = 3, actionButton(inputId = "bt_write_q1.ident", label = "(1) ★★", width = "100%", style = "padding:6px;")), # &starf;
              column(width = 3, actionButton(inputId = "bt_write_q2.ident", label = "(2) ✓✓", width = "100%", style = "padding:6px;")), # &check;
              column(width = 3, actionButton(inputId = "bt_write_q3.ident", label = "(3) ✓", width = "100%", style = "padding:6px;")), # &check;
            p(HTML("<div class = 'beschr'>"), "Übernimmt Markierung aus Feld ", em("Münz ID"), "und ändert/ergänzt", HTML('</div>'))),
          fluidRow(
            h3("eur2collection.txt"),
              column(width = 5, actionButton(inputId = "bt_do_aend.ident", label = "Neu laden", width = "100%", style = "padding:6px;")),
            p(HTML("<div class = 'beschr'>"), "Manuelle Änderung von ", em("eur2collection.txt,"), " zB Münztausch", HTML('</div>')))),
          ### Identifikation Main ----
          h2("Ergebniss", .noWS = "before"),
          fluidRow(
            htmlOutput(outputId = "out_h3.ident"),
            tableOutput(outputId = "out_table.ident")))),
    ## Auflagenstärke ----
    nav_menu(title = "Auflage",
      # h1("🙤 Auflagenstärke 🙧"), # Könnte hier stehen, führt zu Warnung, daher 2x jeweils am Beginn einer page
      nav_panel(title = "Unbekannte Auflagenstärke erfassen",
        ### Unbekannte Auflagenstärke Sidebar ----
        page_sidebar(
          # h1("🙤 Auflagenstärke 🙧"),
          sidebar = sidebar(width = "30%", position = "left", open = "always",
            h2("Bearbeitung"),
            fluidRow(
              h3("Auflagenstärke"),
              column(width = 7,
                textInput(inputId = "in_aufl.unbek", label = NULL, value = "", width = "100%")),
              column(width = 5,
              htmlOutput(outputId = "out_aufl.unbek", inline = TRUE))),
            fluidRow(
              h3("Erfassen"),
              column(width = 6,
                textAreaInput(inputId = "in_erf.unbek", label = NULL, rows = 11, resize = "none", width = "100%")),
              column(width = 6,
                actionButton(inputId = "bt_do_erf.unbek", label = "Erfassen", width = "100%", style = "padding:6px;"),
                p(HTML("<div class = 'beschr'>"), "Die oben stehende Auflagenstärke wird gemeinsam mit der markierten ",
                em("Münz ID"), " im Textfeld erfasst", HTML('</div>')))),
            fluidRow(
              h3("Speichern"),
              column(width = 6,
                actionButton(inputId = "bt_write_aufl.unbek", label = "Speichern", width = "100%", style = "padding:6px;"),
                p(HTML("<div class = 'beschr'>"), "Die Eingaben aus dem Textfeld werden ins File ",
                em("eur2coins_circulation.txt"), " übernommen", HTML('</div>'))),
              column(width = 6))),
          ### Unbekannte Auflagenstärke Main ----
          h2("Unbekannte Auflagenstärke", .noWS = "before"),
          fluidRow(
            htmlOutput(outputId = "out_h3.unbek"),
            tableOutput(outputId = "out_table.unbek")))),
      nav_panel(title = "Erfasste Auflagenstärke korrigieren",
        ### Erfasste Auflagenstärke Sidebar ----
        page_sidebar(
          # h1("🙤 Auflagenstärke 🙧"),
          sidebar = sidebar(width = "30%", position = "left", open = "always",
            h2("Filter"),
            fluidRow(
              column(width = 6,
                h3("Münz ID"),
                fluidRow(
                  column(width = 8, textInput(inputId = "in_id.erf", label = NULL, value = "", width = "100%")),
                  column(width = 4, actionButton(inputId = "bt_reset_id.erf", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"),
                  ", wobei ", code("JJJJ"), " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", 
                  code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen", HTML('</div>')),
              column(width = 6,
                h3("Münzzeichen"),
                fluidRow(
                  column(width = 8, selectInput(inputId = "in_mzz.erf", label = NULL, choices = unique(c(c("", "A", "D", "F", "G", "J"), all_data()$Münzzeichen)), selected = NULL, width = "100%")),
                  column(width = 4, actionButton(inputId = "bt_reset_mzz.erf", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                HTML("<div class = 'beschr'>"), "Auswahl aus Liste; Genaue Übereinstimmung mit Feld ", em("Mzz"), HTML('</div>'))),
            fluidRow(
              h3("Abbildung"),
              column(width = 10, textInput(inputId = "in_abb.erf", label = NULL, value = "", width = "100%")),
              column(width = 2, actionButton(inputId = "bt_reset_abb.erf", label = "✗", width = "100%", style = "padding:6px;")), # &cross;
              HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld ", em("Abbildung"), " Groß-/ Kleinschreibung wird ignoriert", HTML('</div>')),
            h2("Bearbeitung"),
            fluidRow(
              h3("Auflagenstärke"),
              column(width = 7,
                textInput(inputId = "in_aufl.erf", label = NULL, value = "", width = "100%")),
              column(width = 5,
                htmlOutput(outputId = "out_aufl.erf", inline = TRUE))),
            fluidRow(
              h3("Speichern"),
              column(width = 6,
                actionButton(inputId = "bt_write_aufl.erf", label = "Speichern", width = "100%", style = "padding:6px;"),
                p(HTML("<div class = 'beschr'>"), "Die eingegeben Auflagenstärke wird gemeinsam mit der markierten ",
                  em("Münz ID"), " als Korrektur übernommen", HTML('</div>'))))),
        ### Erfasst Auflagenstärke Main ----
        h2("Erfasste Auflagenstärke", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3.erf"),
          tableOutput(outputId = "out_table.erf"))))),
    ## Ablage ----
    nav_panel(title = "Ablage",
      # h1("🙤 Ablage 🙧"),
      ### Ablage Sidebar ----
      page_sidebar(
        sidebar = sidebar(width = "30%", position = "left", open = "always",
          h2("Auswahl Box und Tableau"),
          fluidRow(
            column(width = 6,
              h3("Box"),
              sliderInput(inputId = "in_box.abl", label = NULL, min = 1, max = 4, value = 1, step = 1, width = "100%"),
              HTML("<div class = 'beschr'>"), "Auswahl der Ablagebox", HTML('</div>')),
            column(width = 6,
              h3("Tableau"),
              sliderInput(inputId = "in_tableau.abl", label = NULL, min = 1, max = 6, value = 1, step = 1, width = "100%"),
              HTML("<div class = 'beschr'>"), "Auswahl des Tableaus in der gewählten Ablagebox", HTML('</div>'))),
          h2("Auswahl Münze"),
          fluidRow(
            h3("Ablagenummer"),
            column(width = 2, actionButton(inputId = "bt_do_minus.abl", label = "≺", width = "100%", style = "padding:6px;")), # &prec;
            column(width = 2, actionButton(inputId = "bt_do_plus.abl", label = "≻", width = "100%", style = "padding:6px;")), # &succ;
            column(width = 5, textInput(inputId = "in_ablnr.abl", value = "", label = NULL, width = "100%")), #pull(count(collection))
            column(width = 3, actionButton(inputId = "bt_do_getablnr.abl", label = "gehe zu", width = "100%", style = "padding:6px;")),
            HTML("<div class = 'beschr'>"), em("gehe zu"), " übernimmt Markierung des unterstrichenen Teils im Tableau oder springt zur letzten abgelegten Münze. ",
              em("≺"), " navigiert zur vorherigen (-1), ", em("≻"), " zur nächsten (+1) Münze", HTML('</div>'))),
        ### Ablage Main ----
        h2("Aktives Tableau", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3tableau.abl"),
          tableOutput(outputId = "out_tableau.abl")),
        h2("Aktive Münze", .noWS = "before"),
        fluidRow(
          htmlOutput(outputId = "out_h3aktmz.abl"),
          tableOutput(outputId = "out_aktmz.abl")))),
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
          h3("Qualität"),
          tableOutput(outputId = "out_qual.stat"))))))

# Server ----
server <- function(input, output, session) {
  
  ## Funktionen ----
  ### Fkt Reload ----
  fkt_reload <- function() {
    # celex wird für gewöhnlich nicht im Laufen bearbeitet
    source("rd_circulation.r")
    source("rd_collection.r")
  }
  observeEvent(eventExpr = input$bt_do_aend.ident, handlerExpr = fkt_reload())
  
  ### Fkt Formatieren Zahlen > 1k ----
  ### v.a. in den Unter-Überschriften und bei Auflagenstärken
  fkt_form_tsd <- function (x) format(as.numeric(x), big.mark = "&nbsp;", scientific = FALSE)
  
  ### Fkt Formatieren Qualität ----
  fkt_form_quali <- function(x) {
    case_when(is.na(x) ~ "",
              x == 0 ~ "<span style='color: #daa520'>(0)&nbsp;&#9733;&#9733;&#9733;</span>",
              x == 1 ~ "<span style='color: #5f9321'>(1)&nbsp;&#9733;&#9733;</span>", # #958746;
              x == 2 ~ "<span style='color: #1b7547'>(2)&nbsp;&#10004;&#10004;</span>", # #51696c;
              x == 3 ~ "<span style='color: #0e4c92'>(3)&nbsp;&#10004;</span>",
              TRUE ~ "<span style ='color: red'>FEHLER</span>")
  }
  
  ### Fkt Formatieren Land ----
  fkt_form_land <- function(txt) {
    txt <- tolower(txt) # jedenfalls Kleinbuchstaben
    paste0("<nobr style='font-size: 0.75em'><img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif', height='14', alt='", toupper(txt), "'>&nbsp;&nbsp;/&nbsp;", toupper(txt), "</nobr>")
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
    txt[txt == "G"] <- "<span style='font-size: 1.1em'>Ⓖ</span>"
    txt[txt == "K"] <- "<span style='font-size: 1.1em'>Ⓚ</span>"
    return(txt)
  }
  
  ### Fkt Formatieren Häufigkeit ----
  fkt_form_hfgkt <- function(txt) {
    c("<div class='rare' style='background-color: #b22222b5'>▼</div>",
      "<div class='rare' style='background-color: #c56320b5'>▽</div>",
      "<div class='rare' style='background-color: #daa520b5'>♢</div>",
      "<div class='rare' style='background-color: #7d9820b5'>△</div>",
      "<div class='rare' style='background-color: #228b22b5'>▲</div>")[txt]
  }
  
  ### Fkt Darstellung Daten (switch)----
  fkt_datadisplay <- function(df, variation) {
    df <- mutate(df,
                 Jahr = Prägejahr,
                 Land = fkt_form_land(Land),
                 Amtsblatt = fkt_form_amtsbl(Amtsblatt),
                 ID = paste0("<div class='mono'>", ID, "</div>"),
                 Qualität = fkt_form_quali(Qualität),
                 Ablage = paste0("<div class='mono'>", Ablage, "</div>"),
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
  
  
  ### Fkt Schreiben/Ändern einer Bewertung
  fkt_write_bewertung <- function(qu) {
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
  
  ### Fkt Formatieren Auflagenstärke
  form_aufl <- function(x)
    paste0("<div style='text-align: left'>= <b>",fkt_form_tsd(x), "</b><br>&nbsp;</div>")
  
  ## Fkt Gültigkeitsprüfung Eingabe Ablagenummer
  check_znr <- function(x) {
    x <- as.integer(x)
    na_chk <- is.na(x)
    maxi <- pull(count(collection))
    
    if(na_chk) x <- maxi
    x <- max(1, min(x, maxi))
    return(list(x, !na_chk))
  }
  
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
  
  ### Ausgabe Filter Münzen ----
  output$out_table.ident <- renderTable(expr = tbl_(), spacing = "xs", width = "100%", align = c("lllcrlclll"), sanitize.text.function = function(x) x)
  tbl_ <- eventReactive(eventExpr = c(input$in_smlg.ident, input$in_id.ident, input$in_mzz.ident, input$in_abb.ident, input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                        valueExpr = {
                          # Anzuzeigende Münzen
                          show <- filter(all_data(), (Ablage != " " | input$in_smlg.ident != "ja"), (Ablage == " " | input$in_smlg.ident != "nein"),
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
  output$out_aufl.unbek <- renderText(expr = zahl_form())
  zahl_form <- eventReactive(eventExpr = input$in_aufl.unbek, valueExpr = form_aufl(input$in_aufl.unbek))
  
  ## Auflage  Buttons ----
  observeEvent(eventExpr = input$bt_do_erf.unbek,
               handlerExpr = updateTextInput(session, inputId = "in_erf.unbek", value = paste0(input$in_erf.unbek, input$myselection, "-", input$in_aufl.unbek, "\n")))
  observeEvent(eventExpr = input$bt_write_aufl.unbek, handlerExpr = {
    out <- input$in_erf.unbek
    while(str_sub(out, -1) == "\n") out <- str_sub(out, 1, -2)
    write_lines(out, file = "eur2coins_circulation.txt", append = TRUE)
    fkt_reload()
    updateTextInput(session, inputId = "in_erf.unbek", value = "")
  })
  
  ## Ausgabe Unbekannte Auflagenstärke ----
  output$out_table.unbek <- renderTable(expr = uaufl_(), spacing = "xs", width = "100%", align = c("lllcrl"), sanitize.text.function = function(x) x)
  uaufl_ <- eventReactive(eventExpr = c(input$bt_write_aufl.unbek, input$bt_do_aend.ident),
                         valueExpr = {
                           # Anzuzeigende Münzen
                           show <- filter(all_data(), is.na(Hfgkt))
                           # Anzahl Münzen n (Überschrift inkl Plural)
                           output$out_h3.unbek <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
                           # Ausgabe Ergebnisse Münzen
                           fkt_datadisplay(df = show, variation = "uaufl")
                         })
  
  ## Erfasste AUflagenstärke ----
  ### Reset Buttons ----  
  observeEvent(eventExpr = input$bt_reset_id.erf, handlerExpr = updateTextInput(session, inputId = "in_id.erf", value = ""))
  observeEvent(eventExpr = input$bt_reset_mzz.erf, handlerExpr = updateTextInput(session, inputId = "in_mzz.erf", value = ""))
  observeEvent(eventExpr = input$bt_reset_abb.erf, handlerExpr = updateTextInput(session, inputId = "in_abb.erf", value = ""))
  
  ### Synchonisiere Filter mit Identifikation ----
  observeEvent(eventExpr = c(input$in_id.ident, input$in_mzz.ident, input$in_abb.ident),
               handlerExpr = {
                 updateTextInput(session = session, inputId = "in_id.erf", value = input$in_id.ident)
                 updateTextInput(session = session, inputId = "in_mzz.erf", value = input$in_mzz.ident)
                 updateTextInput(session = session, inputId = "in_abb.erf", value = input$in_abb.ident)
                 })
  
  ### Ausgabe Zahl Auflagenstärke----
  output$out_aufl.erf <- renderText(expr = zahl_korr_form())
  zahl_korr_form <- eventReactive(eventExpr = input$in_aufl.erf, valueExpr = form_aufl(input$in_aufl.erf))

  ### Schreiben einer neuen/korrigierten Auflagenstärke ----
  observeEvent(eventExpr = input$bt_write_aufl.erf,
               handlerExpr = {
                 # Abbild des Files
                 tmp <- select(circulation, ID, Auflage)
                 # Ändern
                 tmp[tmp$ID == input$myselection, "Auflage"] <- as.numeric(input$in_aufl.erf)
                 # Schreiben
                 write_lines(
                   paste(tmp$ID, tmp$Auflage, sep = "-"),
                   "eur2coins_circulation.txt")
                 fkt_reload()
               })
  
  ### Ausgabe Erfasste Auflagenstärke ----
  output$out_table.erf <- renderTable(expr = eaufl_(), spacing = "xs", width = "100%", align = c("lllcrlrc"), sanitize.text.function = function(x) x)
  eaufl_ <- eventReactive(eventExpr = c(input$in_id.erf, input$in_mzz.erf, input$in_abb.erf, input$bt_write_aufl.erf),
                          valueExpr = {
                            # Anzuzeigende Münzen
                            show <- filter(all_data(), !is.na(Hfgkt),
                                           grepl(tolower(input$in_id.erf), ID),
                                           grepl(paste0("\\b", input$in_mzz.erf, "\\b"), Münzzeichen),
                                           grepl(tolower(input$in_abb.erf), tolower(Abbildung)))
                            # Anzahl Münzen n (Überschrift inkl Plural)
                            output$out_h3.erf <- renderText(paste0("<h3>", fkt_form_tsd(dim(show)[1]), " Münze", if(dim(show)[1] != 1) "n " else " ", "</h3>"))
                            # Ausgabe Ergebnisse Münzen
                            fkt_datadisplay(df = show, variation = "eaufl")
                          })
  
  ## Ablage ---
  ### Schnellwahl Schritte ----
  observeEvent(eventExpr = input$bt_do_minus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = as.integer(input$in_ablnr.abl) - 1))
  observeEvent(eventExpr = input$bt_do_plus.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = as.integer(input$in_ablnr.abl) + 1))
  
  ### Schnellwahl Markierung übernehmen ----
  observeEvent(eventExpr = input$bt_do_getablnr.abl, handlerExpr = updateTextInput(session, inputId = "in_ablnr.abl", value = input$myselection))
  
  ### Ausgabe Ablage ----
  output$out_tableau.abl <- renderTable(expr = erst_tab(), spacing = "l", width = "90%", align = "c", rownames = TRUE, sanitize.text.function = function(x) x)
  erst_tab <- eventReactive(eventExpr = c(input$in_box.abl, input$in_tableau.abl, input$in_ablnr.abl, input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                            valueExpr = {
                              # Anzeige Tableau
                              tmp <- collection |> 
                                filter(Zeilennummer %in% (((input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1) + 0:23)) |> 
                                arrange(Zeilennummer) |>
                                mutate(Qualität = fkt_form_quali(Qualität),
                                  This_left = case_when(input$in_ablnr.abl == Zeilennummer ~ "<span class = 'bar'>&#9612;&thinsp;</span>",
                                                        TRUE ~ ""),
                                  This_right = case_when(input$in_ablnr.abl == Zeilennummer ~ "<span class = 'bar'>&thinsp;&#9616;</span>",
                                                         TRUE ~ ""), # kein nbsp wegen doppelklick-markierung
                                  Res = paste0("<div class='mono', align = 'center'>", This_left, str_sub(Ablage, 1, 5), str_sub(Ablage, 6, 9 - nchar(Zeilennummer)), "&middot;", "<u><b>", str_sub(Ablage, 9 - nchar(Zeilennummer) + 1, 9), "</b></u>", This_right, "</div>",
                                               "<div class='mono', align = 'center'><b>", This_left, str_sub(ID, 1, 7), "</b>&middot;", str_sub(ID, 8, 9), This_right, "</div>",
                                               "<div align = 'center'>", Qualität, "</div>")) |>
                                pull(Res)
                              if(length(tmp) < 24) tmp <- c(tmp, rep("<br><div class='mono'><i>< l e e r ></i></div><br>", 24 - length(tmp)))
                              # Ablageadresse (Überschrift)
                              output$out_h3tableau.abl <- renderText(expr = paste0("<h3>Box ", input$in_box.abl, ", Tableau ", input$in_tableau.abl, ": Ablagenummern ",
                                                                         (input$in_box.abl - 1) * 144 + (input$in_tableau.abl - 1) * 24 + 1, " bis ", (input$in_box.abl - 1) * 144 + input$in_tableau.abl * 24, "</h3>"))
                              # Ablagenummer (Überschrift)
                              output$out_h3aktmz.abl <- renderText(expr = paste0("<h3>Ablagenummer: ", input$in_ablnr.abl, "</h3>"))
                              # Ausgabe
                              matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
                                     dimnames = list(paste0("<br><b>", input$in_box.abl, input$in_tableau.abl, "&thinsp;", 1:4, "..", "</b>"),
                                                     paste0("..", 1:6)
                                     )
                              )
                            }, ignoreNULL = FALSE)
  
  ### Ausgabe Auswahl Zeilennummer ----
  output$out_aktmz.abl <- renderTable(expr = tbl_abl(), spacing = "xs", width = "100%", align = c("lllcrlclll"), sanitize.text.function = function(x) x)
  tbl_abl <- eventReactive(eventExpr = c(input$in_ablnr.abl),
                           valueExpr = {
                             if(check_znr(input$in_ablnr.abl)[[2]]) updateSliderInput(session, inputId = "in_box.abl", value = (as.integer(input$in_ablnr.abl) - 1) %/% 144 + 1)
                             if(check_znr(input$in_ablnr.abl)[[2]]) updateSliderInput(session, inputId = "in_tableau.abl", value = (as.integer(input$in_ablnr.abl) - 1) %% 144 %/% 24 + 1)
                             updateTextInput(session, inputId = "in_ablnr.abl", value = check_znr(input$in_ablnr.abl)[[1]])
                             ## Anzuzeigende Münzdetails
                             show <- all_data() |> mutate(Zeile = as.integer(str_sub(Ablage, 6, 9))) |> filter(Ablage != " ", Zeile == input$in_ablnr.abl)
                             # Ausgabe
                             fkt_datadisplay(df = show, variation = "ident")
                           }
  )

  ## Statistik ---
  ### Ausgabe Zusammenfassung Jahr ----
  output$out_jahr.stat <- renderTable(expr = fkt_stat_jahr(), spacing = "xs", align = c("rrrl"), sanitize.text.function = function(x) x)
  fkt_stat_jahr <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                valueExpr = fkt_form_stat("Jahr", 1, 4),
                                ignoreNULL = FALSE)
  
  ### Ausgabe Zusammenfassung Land ----
  output$out_land.stat <- renderTable(expr = fkt_stat_land(), spacing = "xs", align = c("lrrl"), sanitize.text.function = function(x) x)
  fkt_stat_land <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                 valueExpr = fkt_form_stat("Land", 5, 6) |> 
                                   mutate(Land = fkt_form_land(Land)),
                                 ignoreNULL = FALSE)
  
  ### Ausgabe Zusammenfassung Qualität ----
  output$out_qual.stat <- renderTable(expr = fkt_stat_qual(), spacing = "xs", align = c("lrr"), sanitize.text.function = function(x) x)
  fkt_stat_qual <- eventReactive(eventExpr = c(input$bt_write_q0.ident, input$bt_write_q1.ident, input$bt_write_q2.ident, input$bt_write_q3.ident, input$bt_do_aend.ident),
                                 valueExpr = {
                                   collection|> 
                                     group_by(Qualität = Qualität |>  ordered(levels = 0:3, labels = fkt_form_quali(0:3)), .drop = FALSE) |> 
                                     count() |> 
                                     transmute(Anzahl = n,
                                               Anteil = formatC(Anzahl / dim(collection)[1] * 100, 2, format = "f", decimal.mark = ",")
                                               )
                                   },
                                 ignoreNULL = FALSE)
  
}

# Run the application ----
shinyApp(ui = ui, server = server)