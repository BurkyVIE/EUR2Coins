#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  h1("Identifikation von EUR 2 Münzen"),
  textInput(inputId = "text", label = "Suche in ID"),
  tableOutput(outputId = "auswahl")
)

server <- function(input, output) {
  output$auswahl <- renderTable({
      coins %>%
       filter(str_detect(ID, input$text)) %>% 
       arrange(ID) %>%
       unnest() %>% 
       mutate(Ausgabe = as.character(Ausgabe)) %>% 
       rename(Mzz = Münzzeichen) %>% 
       select(-Ausgabe)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

