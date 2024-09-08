# GLOBAL ----
## Libraries ----
library(shiny)

## Externe Daten ----
source("eur2coins.r")      #coins
source("eur2collection.r") #collection

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
  paste0("<nobr><img src='https://www.crwflags.com/fotw/images/", substr(txt, 1, 1), "/", txt, ".gif', height='14', alt='", toupper(txt), "'/>&nbsp;<font size = -3>/ ", toupper(txt), "</font></nobr>")
}

## Funktion zum Formatieren Amtsblatt ----
form_amtsbl <- function(txt) {
  url <- paste0("<a href='https://eur-lex.europa.eu/legal-content/DE/TXT/PDF/?uri=CELEX:", txt, "', target = '_blank'>", txt, "</a>")
  url <- str_replace(url, "\\(", "%28")
  url <- str_replace(url, "\\)", "%29")
  return(url)
}

## Ergänzen und behübschen der Daten ----
all_data <- function() {
  left_join(coins,
            collection %>% select(ID, Qualität, Ablage),
            by = 'ID') |> 
    mutate(Jahr = Prägejahr,
           Ablage = coalesce(Ablage, " "))
}

## Funktion zur Darstellung der Daten ----
displ_data <- function(df, variation) {
  df <- mutate(df,
               Land = form_land(Land),
               Amtsblatt = form_amtsbl(Amtsblatt),
               ID = paste0("<div class='mono'>", ID, "</div>"),
               Qualität = form_quali(Qualität),
               Ablage = paste0("<div class='mono'>", Ablage, "</div>"),
               AQ = paste0(Ablage, Qualität)) |> 
    arrange(ID)
  
  # Darstellung identifikation
  if(variation == "ident") {
    res <- df |> 
      transmute(Jahr,
                Land,
                Abbildung,
                Mzz = Münzzeichen,
                Amtsblatt,
                'Münz ID' = ID,
                Qualität,
                Ablage)
  }
  
  # Darstellung Serien - Standard
  if(variation == "ser") {
    res <- cbind(paste0("<b>", pull(df, Jahr), "</b>"),
                 pull(df, Beschreibung), 
                 pull(df, Münzzeichen),
                 pull(df, AQ)) |>  
      matrix(ncol = 4, dimnames = list(NULL, c("Jahr", "Bezeichnung", "Mzz", " ")))
  }
  
  # Darstellung serien - Deutschland
  if(variation == "serde") {
    res <- cbind(paste0("<b>", df |> filter(Münzzeichen == "A") |> pull(Jahr), "</b>"),
                 df |> filter(Münzzeichen == "A") |> pull(Beschreibung),
                 matrix(df |> pull(AQ), ncol = 5, byrow = TRUE)) |> 
      matrix(ncol = 7, dimnames = list(NULL, c("Jahr", "Bezeichnung", "A (Berlin)", "D (München)", "F (Stuttgart)", "G (Karlsruhe)", "J (Hamburg)")))  
  }
  
  # Variation Gemeinschaftsausgabe
  if(variation == "gem") {
    res <- cbind(pull(df, Land), 
                 pull(df, Münzzeichen),
                 pull(df, ID),
                 pull(df, AQ)) |>  
      matrix(ncol = 4, dimnames = list(NULL, c("Land", "Mzz", "Münz ID", " ")))
  }
  
  return(res)
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



# UI (User Interface) ----
ui <- fluidPage(includeCSS(path = "style_orig.css"),
                tabsetPanel(id = "EUR2",
                            tabPanel("Identifikation",
                                     tags$script(highlight),
                                     fluidPage(
                                       h1("EUR 2 Münzen"),
                                       fluidRow(
                                         column(width = 3,
                                                h2("Anzeige"),
                                                h3("Münzen"),
                                                radioButtons(inputId = "samlg", label = NULL, inline = TRUE,
                                                             choices = c("Alle" = "alle",
                                                                         "Vorhandene" = "ja",
                                                                         "Fehlende" = "nein")),
                                                h3("Münz ID"),
                                                fluidRow(
                                                  column(width = 9, textInput(inputId = "id", value = "", label = NULL)),
                                                  column(width = 3, offset = 0, actionButton(inputId = "id_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld", 
                                                  em("ID"), "; Aufbau ID: ", code("JJJJLLA00"), ", wobei ", code("JJJJ"),
                                                  " = Prägejahr", ", ", code("LL"), " = Land", ", ", code("A"),
                                                  " = Münzart", " und ", code("0"), " = fortlaufende Nummer.", code("."),
                                                  " als Joker ist zulässig.", HTML('</div>')),
                                                h3("Abbildung"),
                                                fluidRow(
                                                  column(width = 9, textInput(inputId = "abb", value = "", label = NULL)),
                                                  column(width = 3, actionButton(inputId = "abb_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Beliebige Übereinstimmung mit Feld Abbildung. Groß-/ Kleinschreibung wird ignoriert.",
                                                  HTML('</div>')),
                                                h3("Münzzeichen"),
                                                fluidRow(
                                                  column(width = 9, textInput(inputId = "mzz", value = "", label = NULL)),
                                                  column(width = 3, actionButton(inputId = "mzz_reset", label = "X"))
                                                ),
                                                p(HTML("<div class = 'beschr'>"), "Genaue Übereinstimmung mit Feld Münzzeichen.",
                                                  HTML('</div>')),
                                                h2("Anlage"),
                                                h3("Qualität"),
                                                fluidRow(
                                                  column(width = 3, actionButton(inputId = "q0", label = "0")),
                                                  column(width = 3, actionButton(inputId = "q1", label = "1")),
                                                  column(width = 3, actionButton(inputId = "q2", label = "2")),
                                                  column(width = 3, actionButton(inputId = "q3", label = "3"))
                                                ),
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
                                                sliderInput(inputId = "box", label = NULL, min = 1, max = 4, step = 1, value = 1),
                                                h3("Tableau"),
                                                sliderInput(inputId = "tableau", label = NULL, min = 1, max = 6, step = 1,  value = 1),
                                                h2("Schnellwahl"),
                                                h3("Ablagenummer"),
                                                fluidRow(
                                                  column(width = 2, actionButton(inputId = "minus", label = "<")),
                                                  column(width = 6, textInput(inputId = "znr", value = pull(count(collection)), label = NULL)),
                                                  column(width = 2, actionButton(inputId = "get", label = "get")),
                                                  column(width = 2, actionButton(inputId = "plus", label = ">"))
                                                )
                                         ),
                                         column(width = 9,
                                                h2("Ansicht"),
                                                h3(textOutput(outputId = "adresse")),
                                                tableOutput(outputId = "tableau"),
                                                h3("Gewählte Ablagenummer"),
                                                tableOutput(outputId = "suche_abl")
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
                                                 tabPanel("EE",
                                                          fluidPage(
                                                            h1("Estland"),
                                                            h2("Nationale Symbole"),
                                                            tableOutput(outputId = "eens_tab")
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
                                                          ),
                                                 tabPanel("LT",
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
                                                            tableOutput(outputId = "mtks_tab"),
                                                            h2("Einheimische Arten Maltas"),
                                                            tableOutput(outputId = "mtea_tab"),
                                                            h2("Maltesische Städte mit Stadtmauern"),
                                                            tableOutput(outputId = "mtsm_tab"),
                                                            )
                                                          )#,
                                                 #tabPanel("...")
                                     )
                            ),
                            tabPanel("Gemeinschaftsausgaben",
                                     tabsetPanel(id = "Gemeinschaftsausgaben",
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
  observeEvent(eventExpr = input$id_reset, handlerExpr = updateTextInput(session, inputId = "id", value = ""))
  observeEvent(eventExpr = input$abb_reset, handlerExpr = updateTextInput(session, inputId = "abb", value = ""))
  observeEvent(eventExpr = input$mzz_reset, handlerExpr = updateTextInput(session, inputId = "mzz", value = ""))
  
  ## Funktion zum Schreiben der Bewertung
  add_bew <- function(qu) {
    tmp <- paste(input$myselection, qu, sep = "-")
    write(tmp, file = "eur2collection.txt", append = TRUE)
    Sys.sleep(1)
  }
  
  ## Bewertungs Buttons ----
  observeEvent(eventExpr = input$q0, handlerExpr = add_bew(0))
  observeEvent(eventExpr = input$q1, handlerExpr = add_bew(1))
  observeEvent(eventExpr = input$q2, handlerExpr = add_bew(2))
  observeEvent(eventExpr = input$q3, handlerExpr = add_bew(3))
  
  ## Reload ----
  observeEvent(eventExpr = c(input$q0, input$q1, input$q2, input$q3, input$aenderung), 
               handlerExpr = source("eur2collection.r"))
  
  ## Funktion zur Anzeige Listendarstellung ----
  data_list <- function(page = NULL, art = NULL) {
    data <- all_data()
    
    if(page == "Ident")
      data <- filter(data, (Ablage != " " | input$samlg != "ja"), (Ablage == " " | input$samlg != "nein"), Münzart == art, grepl(tolower(input$id), ID), grepl(tolower(input$abb), tolower(Abbildung)), grepl(Münzzeichen, input$mzz))
    
    if(page == "Ablage")
      data <- mutate(data, Zeile = as.integer(str_sub(Ablage, 6, 9))) |> 
        filter(Ablage != " ", Zeile == input$znr)
    
    displ_data(data, variation = "ident")
  }
  
  ## Ausgabe Gedenkmünzen ----
  output$suche_g <- renderTable(expr = tbl_g(), spacing = "xs", width = "100%", align = c("llllllll"), sanitize.text.function = function(x) x)
  tbl_g <- eventReactive(eventExpr = c(input$samlg, input$id, input$abb, input$mzz, input$q0, input$q1, input$q2, input$q3, input$aenderung),
                         valueExpr = data_list(page = "Ident", art = "Gedenkmünze"))
  
  ## Ausgabe Umlaufmünzen ----
  output$suche_u <- renderTable(expr = tbl_u(), spacing = "xs", width = "100%", align = c("llllllll"), sanitize.text.function = function(x) x)
  tbl_u <- eventReactive(eventExpr = c(input$samlg, input$id, input$abb, input$mzz, input$q0, input$q1, input$q2, input$q3, input$aenderung),
                         valueExpr = data_list(page = "Ident", art = "Umlaufmünze"))
  
  ## Funktuion zur Gültigkeitsprüfung Eingabe Ablagenummer
  check_znr <- function(x) {
    x <- as.integer(x)
    maxi <- as.integer(tail(collection$Zeilennummer, 1))
    nachk <- is.na(x)
    
    if(nachk) x <- maxi
    x <- max(1, min(x, maxi))
    return(list(x, !nachk))
  }
  
  ## Auswahl Ablage ----
  observeEvent(eventExpr = input$znr, 
               handlerExpr = {
                 if(check_znr(input$znr)[[2]]) updateSliderInput(session, inputId = "box", value = (as.integer(input$znr) - 1) %/% 144 + 1)
                 if(check_znr(input$znr)[[2]]) updateSliderInput(session, inputId = "tableau", value = (as.integer(input$znr) - 1) %% 144 %/% 24 + 1)
                 updateTextInput(session, inputId = "znr", value = check_znr(input$znr)[[1]])
                 })
  
  ## Ausgabe Schnellwahl Ablage ----
  output$suche_abl <- renderTable(expr = tbl_abl(), spacing = "xs", width = "100%", align = c("llllllll"), sanitize.text.function = function(x) x)
  tbl_abl <- eventReactive(eventExpr = input$znr, valueExpr = data_list(page = "Ablage"))
  
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
  erst_tab <- eventReactive(eventExpr = c(input$box, input$tableau, input$znr),
                            valueExpr = {
                              collection |> 
                                filter(Zeilennummer %in% (((input$box - 1) * 144 + (input$tableau - 1) * 24 + 1) + 0:23)) |> 
                                arrange(Zeilennummer) |>
                                mutate(Qualität = form_quali(Qualität),
                                       This = case_when(input$znr == Zeilennummer ~ "<font color = 'red'><b>&#9679;</b></font>",
                                                        TRUE ~ ""),
                                       Res = paste0("<div class='mono'>", str_sub(Ablage, 1, 9 - nchar(Zeilennummer)), "&thinsp;", This, "<u><b>", str_sub(Ablage, 9 - nchar(Zeilennummer) + 1, 9), "</b></u>", This, "<br>",
                                                   "<b>", str_sub(ID, 1, 4), "&thinsp;", toupper(str_sub(ID, 5, 6)), "&thinsp;", toupper(str_sub(ID, 7, 7)), "</b>&thinsp;", str_sub(ID, 8, 9), "</div>",
                                                   Qualität)) |>  
                                pull(Res) -> tmp
                              if(length(tmp) < 24) tmp <- c(tmp, rep("", 24 - length(tmp)))
                              matrix(tmp, ncol = 6, nrow = 4, byrow = TRUE,
                                     dimnames = list(paste0("<br><b>", input$box, input$tableau, "&thinsp;", 1:4, "..", "</b>"),
                                                     paste0("..", 1:6)
                                                     )
                                     )
                              }, ignoreNULL = FALSE)
  
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
                                              Anteil = Anzahl / dim(collection)[1] * 100)
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

  ### Estland - Nationale Symbole ----
  output$eens_tab <- renderTable({eens_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  eens_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    eens <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2021/059/05', '<b>Der Wolf, das Nationaltier</b>',
                    'C2023/264/07', '<b>Die Rauchschwalbe, der Nationalvogel</b>',
                    'C/2024/03965', '<b>Die Kornblume, die Nationalblume</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(eens, "ser")
  }, ignoreNULL = FALSE)

  ### Frankreich - Olympische Sommerspiele 2024 ----
  output$fros_tab <- renderTable({fros_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  fros_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    fros <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2021/470/07', '<b>Marianne und der Wettlauf - Eiffelturm</b>',
                    'C2023/014/04', '<b>Der Genius und der Diskuswurf - Arc de Triomphe</b>',
                    'C2023/116/12', '<b>Die Säerin und der Faustkampf – Pont Neuf</b>',
                    'C/2024/02468', '<b>Herkules und der Ringkampf - Nortre Dame</b>',
                    'C/2024/03959', '<b>Olympische und Paralympische Spiele 2024 in Paris – Notre-Dame de la Garde</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(fros, "ser")
    }, ignoreNULL = FALSE)
  
  ### Litauen - Ethnografische Regionen ----
  output$lter_tab <- renderTable({lter_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  lter_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    lter <- tribble(~Amtsblatt, ~Beschreibung,
                    'C2019/351/10', '<b>Žemaitija</b><br>(Niederlittauen)',
                    'C2020/053/04', '<b>Aukschtaiten</b><br>(Oberlitauen)',
                    'C2021/473/05', '<b>Dzukija</b><br>(Mittellitauen)',
                    'C2022/484/25', '<b>Suvalkija</b><br>(Sudauen)') |>
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
                    'C/2024/02467', '<b>100. Jahrestag der Unterzeichnung des Erlasses über die Ausgabe der „Feierstëppler“-Scheidemünze durch Großherzogin Charlotte</b>') |>
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
                    'C2018/234/03', '<b>Zemgale</b><br>(Semgallen)') |>
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
                    'C/2024/03949', '<b>Die maltesische Honigbiene</b>') |>
      left_join(all_data(), by = "Amtsblatt", na_matches = "never") |> 
      filter(!is.na(Amtsblatt))
    
    displ_data(mtea, "ser")
  }, ignoreNULL = FALSE)
  
  ### Malta - Maltesische Städte mit Stadtmauern ----
  output$mtsm_tab <- renderTable({mtsm_tab()}, bordered = T, spacing = "l", align = "clcc", rownames = FALSE, sanitize.text.function = function(x) x)
  mtsm_tab <- eventReactive(c(input$aenderung, input$q0, input$q1, input$q2, input$q3), {
    mtsm <- tribble(~Amtsblatt, ~Beschreibung,
                    'C/2024/03946', '<b>Cittadella Gozo</b>') |>
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
                    NA, '<b>Altstadt von Salamanca</b><br>()',
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
}



# Run the application ----
shinyApp(ui = ui, server = server)
