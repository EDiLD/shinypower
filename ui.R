library(shiny)
library(ggplot2)

# Define UI for slider demo application
shinyUI(navbarPage("Powerapp",
          tabPanel("Design",
            sidebarLayout(
              # Sidebar with sliders that demonstrate various available options
              sidebarPanel(
                sliderInput("muc", "Abundance in Control:", value = 10,
                             min = 0.1, max = 200, step = 5),
                sliderInput('theta', 'Theta (=dispersion parameter): ', 
                            value = 4, min = 0.1, max = 200, step = 0.1),
                sliderInput('effsize', 'Abundance in Treatment (as proportion of control):',
                            value = 0.5, min = 0, max = 1, step = 0.1)
              ),
              
              # Show a table summarizing the values entered
              mainPanel(
                plotOutput("desplot"),
                dataTableOutput("destab")
              )
            )
          ), 
          tabPanel("Power",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("nsims", "Number of simulations:", 
                                   min = 50, max = 250, value = 50, step = 50),
                       textInput('N', 'Sample sizes (separated by comma or space): ',
                                 '3, 6, 9'),
                       actionButton("goButton", "Run Simulation!")
                     ),
                     mainPanel(
                       plotOutput("powplot"),
                       dataTableOutput("powtable"),
                       downloadButton('downloadData', 'Download dataset'),
                       downloadButton('downloadPlot', 'Download Plot')
                     )
                   )
          )
))
