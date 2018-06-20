library(shiny)
shinyUI(pageWithSidebar (
  headerPanel( "Optimizer"),
  sidebarPanel(
    numericInput("assets", label = "How many variables?", value="0 "),
    numericInput("constraints", label = "How many expressions?", value="0 "),
    helpText("Note: The last expression to be inserted is your Objective Function."),
    #checkboxInput("check",label="Check if to use Maximization. Uncheck if to use Minimization.",value = FALSE),
    selectInput("opt", 
                label = "Choose optimization type",
                choices = c("Minimization", "Maximization"),
                selected = "Maximization")
  ),
  mainPanel(
    uiOutput("vars"),
    uiOutput("answer"),
    actionButton( "OkButton" ,  "Move to next expression")
  
  )
))