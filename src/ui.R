library(shiny)

library(ggplot2)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Ecotox power estimation"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    sliderInput("nsims", "Number of simulations:", 
                min = 50, max = 500, value = 50, step = 50),
    sliderInput("muc", "Abundance in Control", value = 10,
                 min = 1, max = 500, step = 5),
    sliderInput('effsize', 'Abundance in Treatment (as proportion of control)',
                 value = 0.5, min = 0, max = 1, step = 0.1),
    textInput('N', 'Sample sizes (separated by comma or space',
              '3, 6, 9'),
    actionButton("goButton", "Go!")
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    plotOutput("powplot")
  )
)
) 
