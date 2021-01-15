library(shiny)

function(input, output) {
  output$skupine <- renderPlot({
    ggplot(tabela_po_poklicni_skupini %>%
             filter(poklicna_skupina == input$poklicna_skupina)) + 
      aes(x=leto, y=stevilo, fill=spol) + geom_bar(stat='identity', position='dodge') +
      labs(title='Delovno aktivni po izbrani poklicni skupini:', x='Leto', y = 'Število v 1000') + 
      scale_fill_manual(values=c('skyblue1', 'plum3')) + theme_minimal()
  })
}
