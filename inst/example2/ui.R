# based on RStudio example from: http://rstudio.github.io/shiny/tutorial/#reactivity

library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Reactivity"),

  # Sidebar with controls to select a dataset, and
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    selectInput("dataset", "Choose a Species:",
                choices = c("setosa","versicolor","virginica" ))
  ),


  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    tableOutput("view")
  )
))
