library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for slider demo application
shinyUI(navbarPage("shinytox (alpha)", theme = shinytheme("united"), 
          tabPanel("Power",
            sidebarLayout(
              sidebarPanel(
                tabsetPanel(
                  tabPanel('Settings-Effects',
                           tags$head(
                             tags$script(src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                                         type = 'text/javascript')
                             ),
                           sliderInput("muc", HTML("$$\\text{Abundance in Control } (\\mu_C)$$"), value = 10,
                                       min = 0.5, max = 200, step = 2),
                           sliderInput('theta', '$$\\text{Dispersion} (\\kappa)$$ ', 
                                       value = 4, min = 0.1, max = 200, step = 0.1),
                           sliderInput('effsize', 'Reduction in treatment (r)',
                                       value = 0.5, min = 0, max = 1, step = 0.1),
                           p("See `Simulation-Design` tab for a live graphical representation of the simulated data."),
                           actionButton("goButton", "Run Simulation!")
                           ), 
                  tabPanel('Settings-Simulation',
                           sliderInput("nsims", "Number of simulations:", 
                                       min = 50, max = 250, value = 50, step = 50),
                           textInput('N', 'Sample sizes (separated by comma or space; up to 5 entries): ',
                                     '3, 6, 9'),
                           selectInput("mct", "Multiple comparison contrasts:", 
                                       choices = c('Dunnett contrasts' = 'Dunnett',
                                                   'Williams contrasts' = 'Williams')),
                           selectInput("alt", "Hypothesis:", 
                                       choices = c('two sided' = 'two.sided',
                                                   'one sided - increase' = 'greater',
                                                   'one sided - decrease' = 'less'))
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
                   # textOutput("test"),
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
                  tabPanel('Simulation-Design',
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
                         tabPanel('Diagnostics'
                         ),
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
          tabPanel("Population-GLM",
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
                         tabPanel('Diagnostics'
                         ),
                         tabPanel('Summary'
                         ),
                         tabPanel('Effect Class'
                         )
                       )
                     )
                   )
          )
))
