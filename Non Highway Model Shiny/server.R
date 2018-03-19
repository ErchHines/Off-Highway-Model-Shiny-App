library(reshape2)
library(DT)
library(stats)
library(dplyr)
#library(ggseas)

function(input, output, session) {
  
  combinedInput <- reactive({
    combine.model(
      input$Ag, input$AgC, input$Av, input$Bo,
      input$BoC, input$Fh, input$Ff, input$Ep
    )
  })
  
  fhwaIinput <- reactive({
    model.fhwa(
      input$Ag, input$AgC, input$Av, input$Bo,
      input$BoC, input$Fh, input$Ff, input$Ep
    )
  })
  
  epaInput <- reactive({
    get.epa.results(input$Ep)
  })
  
  
  # Output for Monthly Motor Fuel Tables
  output$mytable1 = DT::renderDataTable({
        DT::datatable(fhwaIinput(),
        filter = 'top',
        rownames = FALSE,
        extensions = 'FixedColumns',
        class = 'cell-border stripe',
        options = list(lengthMenu = c(15, 25, 51),
        pageLength = 15)) %>% formatCurrency(2:13, '', digits = 0) %>%
        formatStyle(1:13, 'vertical-align'='top') %>% 
        formatStyle(1:13, 'text-align' = 'right') 
  })
  
  output$mytable2 = DT::renderDataTable({
        DT::datatable(combinedInput(),
        filter = 'top',
        rownames = FALSE,
        extensions = 'FixedColumns',
        class = 'cell-border stripe',
        options = list(lengthMenu = c(15, 25, 51),
        pageLength = 15)) %>% formatCurrency(2:13, '', digits = 0) %>%
        formatStyle(1:13, 'vertical-align'='top') %>% 
        formatStyle(1:13, 'text-align' = 'right') 
  })

  output$mytable3 = DT::renderDataTable({
        DT::datatable(epaInput(),
        filter = 'top',
        extensions = 'FixedColumns',
        rownames = FALSE,
        class = 'cell-border stripe',
        options = list(lengthMenu = c(15, 25, 51),
        pageLength = 15)) %>% formatCurrency(2:13, '', digits = 0) %>%
        formatStyle(1:13, 'vertical-align'='top') %>% 
        formatStyle(1:13, 'text-align' = 'right') 
  })  
  
  output$downloadData1 <- downloadHandler(
    filename = paste("CombinedModel", ".csv", sep = ""),
    content = function(file) {
      write.csv(combinedInput(), file, row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = paste("FHWA_Model", ".csv", sep = ""),
    content = function(file) {
      write.csv(fhwaInput(), file, row.names = FALSE)
    }
  )

  output$downloadData3 <- downloadHandler(
    filename = paste("EPA_Model", ".csv", sep = ""),
    content = function(file) {
      write.csv(epaInput(), file, row.names = FALSE)
    }
  )

}

# %>% formatCurrency(1:12, '', digits = 0)