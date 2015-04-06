library(shiny)
library(ggplot2)

# Define UI for slider demo application
shinyUI(navbarPage("Powerapp",
          tabPanel("Design",
            sidebarLayout(
              # Sidebar with sliders that demonstrate various available options
              sidebarPanel(
                tags$head(tags$script(src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript')
                ),
                sliderInput("muc", HTML("$$\\text{Abundance in Control } (\\mu_C)$$"), value = 10,
                             min = 0.1, max = 200, step = 5),
                sliderInput('theta', '$$\\text{Dispersion} (\\kappa)$$ ', 
                            value = 4, min = 0.1, max = 200, step = 0.1),
                sliderInput('effsize', 'Reduction in treatment (r)',
                            value = 0.5, min = 0, max = 1, step = 0.1)
              ),
              
              # Show a table summarizing the values entered
              mainPanel(
                tabsetPanel(
                  tabPanel('Readme', 
                    includeMarkdown("md/design.md")
                    ),
                  tabPanel('Summary', 
                    plotOutput("desplot"),
                    h3("Summary"), 
                    dataTableOutput("destab")
                  )
                )
              )
            )
          ), 
          tabPanel("Powerestimation",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("nsims", "Number of simulations:", 
                                   min = 50, max = 250, value = 50, step = 50),
                       textInput('N', 'Sample sizes (separated by comma or space; up to 5 entries): ',
                                 '3, 6, 9'),
                       actionButton("goButton", "Run Simulation!")
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Readme',
                            includeMarkdown("md/simulation.md")
                         ),
                         tabPanel('Results', 
                           plotOutput("powplot"),
                           h3("Summary"),
                           dataTableOutput("powtable"),
                           downloadButton('downloadData', 'Download dataset'),
                           downloadButton('downloadPlot', 'Download Plot')
                         )
                       )
                     )
                   )
          )
))
