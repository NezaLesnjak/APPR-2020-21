library(shiny)

fluidPage(
  selectInput(inputId = 'poklicna_skupina', label = 'Izberite poklicno skupino:',
              choices = unique(tabela_po_poklicni_skupini$poklicna_skupina), 
              selected = unique(tabela_po_poklicni_skupini$poklicna_skupina)[1]),
  plotOutput('skupine')
)
