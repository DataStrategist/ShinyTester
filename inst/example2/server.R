# RStudio example based on: http://rstudio.github.io/shiny/tutorial/#reactivity

library(shiny)
library(dplyr)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  # By declaring datasetInput as a reactive expression we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it
  #     only executes a single time)
  #
  datasetInput <- reactive({
    # iris %>% filter(Species == input$dataset)
    iris[iris$Species == input$dataset,]
  })


  # The output$view depends on both the databaseInput reactive expression
  # and input$obs, so will be re-executed whenever input$dataset or
  # input$obs is changed.
  output$view <- renderTable({
    datasetInput <- datasetInput()
    datasetInput
  })
})
