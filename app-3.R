# GLOBAL ----
## Libraries ----
library(shiny)
library(bslib)

## Externe Daten ----
source("rd_celex.r")       #celex
source("rd_circulation.r") #circulation
source("rd_collection.r")  #collection

## Ergänzen und behübschen der Daten ----
all_data <- function() {
  left_join(coins,
            collection %>% select(ID, Qualität, Ablage),
            by = 'ID') |> 
    mutate(Ablage = coalesce(Ablage, " ")) |> 
    left_join(circulation, by = join_by(ID))
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
# UI (User Interface) ----
ui <- page_fluid(includeCSS(path = "style_fwd.css"),
  tags$script(highlight),
  navset_pill(
    nav_panel(title = "Identifikation",
      ## Identifikation ----
      h1("🙤 Identifikation 🙧"),
      page_sidebar(
        sidebar = sidebar(width = "30%", position = "left", open = "always",
          h2("Filter"),
          fluidRow(
            h3("Münzen"),
            radioButtons(inputId = "samlg", label = NULL, inline = TRUE,
                         choices = c("Alle" = "alle",
                                     "Vorhandene" = "ja",
                                     "Fehlende" = "nein")),
            HTML("<div class = 'beschr'>"), "Auswahl einer Option; Genaue Übereinstimmung mit Feld", em("Qualität/Ablage"),
            "im Sinne von egal / vorhanden / leer", HTML('</div>')),
          fluidRow(
            column(width = 6,
                   h3("Münz ID"),
                   fluidRow(
                     column(width = 8, textInput(inputId = "id", label = NULL, value = "", width = "100%")),
                     column(width = 4, actionButton(inputId = "id_reset", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                   HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit", em("Münz ID;"), " Aufbau: ", code("JJJJLLA00"),
                   ", wobei ", code("JJJJ"), " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"), " = Münzart", " und ", 
                   code("0"), " = fortlaufende Nummer;", code("."), " = Jokerzeichen.", HTML('</div>')),
            column(width = 6,
                   h3("Münzzeichen"),
                   fluidRow(
                     column(width = 8, selectInput(inputId = "mzz", label = NULL, choices = unique(all_data()$Münzzeichen), selected = NULL, width = "100%")),
                     column(width = 4, actionButton(inputId = "mzz_reset", label = "✗", width = "100%", style = "padding:6px;"))), # &cross;
                   HTML("<div class = 'beschr'>"), "Auswahl aus Liste; Genaue Übereinstimmung mit Feld ", em("Mzz."), HTML('</div>'))),
          fluidRow(
            h3("Abbildung"),
              column(width = 10, textInput(inputId = "abb", label = NULL, value = "", width = "100%")),
              column(width = 2, actionButton(inputId = "abb_reset", label = "✗", width = "100%", style = "padding:6px;")), # &cross;
            HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld ", em("Abbildung."), " Groß-/ Kleinschreibung wird ignoriert.", HTML('</div>')),
          h2("Anlage / Änderung"),
          fluidRow(
            h3("Qualität"),
              column(width = 3, actionButton(inputId = "q0", label = "(0) ★★★", width = "100%", style = "padding:6px;")), # &starf;
              column(width = 3, actionButton(inputId = "q1", label = "(1) ★★", width = "100%", style = "padding:6px;")), # &starf;
              column(width = 3, actionButton(inputId = "q2", label = "(2) ✓✓", width = "100%", style = "padding:6px;")), # &check;
              column(width = 3, actionButton(inputId = "q3", label = "(3) ✓", width = "100%", style = "padding:6px;")), # &check;
            p(HTML("<div class = 'beschr'>"), "Übernimmt Markierung aus Feld ", em("Münz ID."), HTML('</div>'))),
          fluidRow(
            h3("eur2collection.txt"),
              column(width = 5, actionButton(inputId = "aenderung", label = "Änderung durchgeführt", width = "100%", style = "padding:6px;")),
            p(HTML("<div class = 'beschr'>"), "Manuelle Änderung von ", em("eur2collection.txt,"), " zB Münztausch", HTML('</div>')))),
          h2("Ergebnisse"),
          fluidRow(
            htmlOutput(outputId = "n_münzen"),
            tableOutput(outputId = "suche_")))),
    nav_panel(title = "Auflage",
      ## Auflage ----
      h1("🙤 Auflage 🙧"),
      page_sidebar(
        sidebar = sidebar(width = "30%", position = "left", open = "always",
          h2("Bearbeiten"),
          fluidRow(
            h3("Erfassen"),
            column(width = 7, textAreaInput(inputId = "aufl_erf", label = NULL, rows = 11, resize = "none", width = "100%")),
            column(width = 5,
              textInput(inputId = "aufl_zahl", label = NULL, value = "", width = "100%"),
              htmlOutput(outputId = "zahl_form", inline = TRUE),
              actionButton(inputId = "aufl_uber", label = "Übernehmen", width = "100%", style = "padding:6px;"),
              p(HTML("<div class = 'beschr'>"), "Die obige Auflagenstärke wird gemeinsam mit der markierten ",
              em("Münz ID"), " in das Textfeld übernommen.", HTML('</div>')))),
          fluidRow(
            h3("Verwalten"),
            column(width = 6,
              actionButton(inputId = "aufl_schrb", label = "Schreiben", width = "100%", style = "padding:6px;"),
              p(HTML("<div class = 'beschr'>"), "Die Eingaben aus dem Textfeld werden ins File ",
              em("eur2coins_circulation.txt"), "übernommen.", HTML('</div>'))),
            column(width = 6))),
        h2("Unbekannte Auflagenstärke"),
        fluidRow(
          htmlOutput(outputId = "n_aufl"),
          tableOutput(outputId = "unbek_aufl")))),
    nav_panel(title = "Ablage",
      ## Ablage ----
      h1("🙤 Ablage 🙧"),
      page_sidebar(
        sidebar = sidebar(width = "30%", position = "left", open = "always",
          h2("Auswahl Box und Tableau"),
          fluidRow(
            column(width = 6,
              h3("Box"),
              sliderInput(inputId = "box", label = NULL, min = 1, max = 4, value = 1, step = 1, width = "100%"),
              div(HTML("<div class = 'beschr'>"), "Auswahl der Ablagebox.", HTML('</div>'))),
            column(width = 6,
              h3("Tableau"),
              sliderInput(inputId = "tableau", label = NULL, min = 1, max = 6, value = 1, step = 1, width = "100%"),
              p(HTML("<div class = 'beschr'>"), "Auswahl des Tableaus in der gewählten Ablagebox.", HTML('</div>')))),
          h2("Auswahl Münze"),
          fluidRow(
            h3("Ablagenummer"),
            column(width = 2, actionButton(inputId = "minus", label = "≺", width = "100%", style = "padding:6px;")), # &prec;
            column(width = 2, actionButton(inputId = "plus", label = "≻", width = "100%", style = "padding:6px;")), # &succ;
            column(width = 5, textInput(inputId = "znr", value = "", label = NULL, width = "100%")), #pull(count(collection))
            column(width = 3, actionButton(inputId = "get", label = "gehe zu", width = "100%", style = "padding:6px;")),
            div(HTML("<div class = 'beschr'>"), em("gehe zu"), " übernimmt Markierung des unterstrichenen Teils im Tableau oder springt zur letzten abgelegten Münze. ",
              em("≺"), " navigiert zur vorherigen (-1), ", em("≻"), " zur nächsten (+1) Münze.", HTML('</div>')),)),
        h2("Ansicht"),
        fluidRow(
          h3(textOutput(outputId = "adresse")),
          tableOutput(outputId = "tableau"),),
        fluidRow(
          h3("Gewählte Ablagenummer"),
          tableOutput(outputId = "suche_abl")))),
    nav_panel(title = "Statistik",
      ## Statistik ----
      h1("🙤 Statistik 🙧"),
      h2(HTML("&nbsp;")),
      fluidRow(
        column(width = 4,
               h2("Prägejahr"),
               tableOutput(outputId = "zsf_jahr")),
        column(width = 4,
               h2("Land"),
               tableOutput(outputId = "zsf_land")),
        column(width = 4,
               h2("Qualität"),
               tableOutput(outputId = "zsf_qual"))))))

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
    c("<div style='background-color: #b22222b5; color: #faf0e6ff'>▼</div>",
      "<div style='background-color: #c56320b5; color: #faf0e6ff'>▽</div>",
      "<div style='background-color: #daa520b5; color: #faf0e6ff'>♢</div>",
      "<div style='background-color: #7d9820b5; color: #faf0e6ff'>△</div>",
      "<div style='background-color: #228b22b5; color: #faf0e6ff'>▲</div>")[txt]
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
    source("rd_collection.r")
  }
  observeEvent(eventExpr = input$aenderung, handlerExpr = reload())
  
  ## Funktion zum Schreiben der Bewertung
  add_bew <- function(qu) {
    tmp <- paste(input$myselection, qu, sep = "-")
    write(tmp, file = "eur2coins_collection.txt", append = TRUE)
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
                          output$n_münzen <- renderText(paste0("<h3>", format(dim(show)[1], big.mark = "&nbsp;"), " Münze", if(dim(show)[1] > 1) "n " else " ",
                                                               "&nbsp;(", paste(unique(show$Art), collapse = ' + '), ")</h3>"))
                          # Ausgabe Ergebnisse Münzen
                          displ_data(df = show, variation = "ident")
                        }
  )
  
  ## Ausgabe formatierte Zahl ----
  output$zahl_form <- renderText(expr = zahl_form())
  zahl_form <- eventReactive(eventExpr = input$aufl_zahl, valueExpr = paste0("<div style='text-align: center'>= ",format(as.numeric(input$aufl_zahl), big.mark = " ", scientific = FALSE), "<br>&nbsp;</div>"))
  
  ## Auflage  Buttons ----
  observeEvent(eventExpr = input$aufl_uber, handlerExpr = updateTextInput(session, inputId = "aufl_erf", value = paste0(input$aufl_erf, input$myselection, "-", input$aufl_zahl, "\n")))
  observeEvent(eventExpr = input$aufl_schrb, handlerExpr = {
    out <- input$aufl_erf
    while(str_sub(out, -1) == "\n") out <- str_sub(out, 1, -2)
    write(out, file = "eur2coins_circulation.txt", append = TRUE)
    Sys.sleep(1.5)
    source("rd_circulation.r")
    reload()
    updateTextInput(session, inputId = "aufl_erf", value = "")
  })
  
  ## Ausgabe Unbekannte Auflage ----
  output$unbek_aufl <- renderTable(expr = aufl_(), spacing = "xs", width = "100%", align = c("llllll"), sanitize.text.function = function(x) x)
  aufl_ <- eventReactive(eventExpr = c(input$aufl_schrb),
                         valueExpr = {
                           # Anzuzeigende Münzen
                           show <- filter(all_data(), is.na(Hfgkt)) # !str_starts(Abbildung, "~")
                           # Anzahl Münzen n (Überschrift inkl Plural)
                           output$n_aufl <- renderText(paste0("<h3>", format(dim(show)[1], big.mark = "&nbsp;"), " Münze", if(dim(show)[1] > 1) "n " else " ", "</h3>"))
                           # Ausgabe Ergebnisse Münzen
                           displ_data(df = show, variation = "aufl")
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
  
}

# Run the application ----
shinyApp(ui = ui, server = server)