library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for slider demo application
shinyUI(navbarPage("shinytox", theme = shinytheme("united"), 
          tabPanel("Power",
            sidebarLayout(
              sidebarPanel(
                tabsetPanel(
                  tabPanel('Effects',
                           tags$head(
                             tags$script(src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                                         type = 'text/javascript')
                             ),
                           sliderInput("muc", HTML("$$\\text{Abundance in Control } (\\mu_C)$$"), value = 10,
                                       min = 0.5, max = 200, step = 5),
                           sliderInput('theta', '$$\\text{Dispersion} (\\kappa)$$ ', 
                                       value = 4, min = 0.1, max = 200, step = 0.1),
                           sliderInput('effsize', 'Reduction in treatment (r)',
                                       value = 0.5, min = 0, max = 1, step = 0.1)
                           ), 
                  tabPanel('Simulations',
                           sliderInput("nsims", "Number of simulations:", 
                                       min = 50, max = 250, value = 50, step = 50),
                           textInput('N', 'Sample sizes (separated by comma or space; up to 5 entries): ',
                                     '3, 6, 9'),
                           selectInput("mct", "MCT:", 
                                       choices = c('Dunnett' = 'Dunnett',
                                                   'Williams' = 'Williams')),
                           selectInput("alt", "Alternative:", 
                                       choices = c('two.sided' = 'two.sided',
                                                   'greater' = 'greater',
                                                   'less' = 'less')),
                           actionButton("goButton", "Run Simulation!")
                           ), 
                  tabPanel('Readme', 
                           includeMarkdown("md/design.md")
                           ),
                  tabPanel('Readme2',
                           includeMarkdown("md/simulation.md")
                           )
              )),
              mainPanel(
                tabsetPanel(
                  tabPanel("Global test", 
                   plotOutput("powplot"),
                   h3("Summary"),
                   dataTableOutput("powtable"),
                   downloadButton('downloadpowtable', 'Download table'),
                   downloadButton('downloadpowplot', 'Download plot')
                   ), 
                  tabPanel('LOEC', 
                   plotOutput("loecplot"),
                   h3("Summary"),
                   dataTableOutput("loectable"),
                   downloadButton('downloadloectable', 'Download table'),
                   downloadButton('downloadloecplot', 'Download plot')
                   ),
                  tabPanel('Design',
                           plotOutput("desplot"),
                           h3("Summary"), 
                           dataTableOutput("destab")
                  )
                )
              )
            )
          ), 
          tabPanel("Community - PRC",
                   sidebarLayout(
                     sidebarPanel(
                       tabsetPanel(
                         tabPanel('Data'
                                  ),
                         tabPanel('Transformation'
                                  ),
                         tabPanel('Settings'
                                  ),
                         tabPanel('README'
                                  )
                       )
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Plot'
                         ),
                         tabPanel('Summary'
                         ),
                         tabPanel('Tests'
                         ),
                         tabPanel('Summary (per sampling)'
                         ),
                         tabPanel('Tests (per sampling)'
                         )
                       )
                     )
                     )
          ),
          tabPanel("Community - GLM",
                   sidebarLayout(
                     sidebarPanel(
                       tabsetPanel(
                         tabPanel('Data'
                         ),
                         tabPanel('Settings'
                         ),
                         tabPanel('README'
                         )
                       )
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel('Plot'
                         ),
                         tabPanel('Summary'
                         ),
                         tabPanel('Tests'
                         ),
                         tabPanel('Summary (per sampling)'
                         ),
                         tabPanel('Tests (per sampling)'
                         )
                       )
                     )
                   )
          ),
          tabPanel("Population",
                   sidebarLayout(
                     sidebarPanel(
                       tabsetPanel(
                         tabPanel('Data'
                         ),
                         tabPanel('Settings'
                         ),
                         tabPanel('README'
                         )
                       )
                     ),
                     mainPanel(
                       
                     )
                   )
          )
))
