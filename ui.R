library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for slider demo application
shinyUI(navbarPage("shinytox (alpha)", theme = shinytheme("united"), 
          tabPanel("Power", 
            sidebarLayout(
              sidebarPanel(
                h2('Settings'),
                tags$head(
                  tags$script(src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                              type = 'text/javascript')
                  ),
                   wellPanel(tags$details(tags$summary("Effects"),
                                          sliderInput("muc", HTML("$$\\text{Abundance in Control } (\\mu_C)$$"), value = 10,
                                                      min = 0.5, max = 200, step = 1),
                                          sliderInput('theta', '$$\\text{Dispersion} (\\kappa)$$ ', 
                                                      value = 4, min = 0.1, max = 250, step = 0.1),
                                          sliderInput('effsize', 'Reduction in treatment (r)',
                                                      value = 0.5, min = 0, max = 1, step = 0.01)
                                          )
                             ),
                   wellPanel(tags$details(tags$summary("Simulations"),
                                          sliderInput("nsims", "Number of simulations:", 
                                                      min = 50, max = 250, value = 50, step = 10),
                                          textInput('N', 'Sample sizes (separated by comma or space; up to 5 entries): ',
                                                     '3, 6, 9')
                                          )
                             ),
                    wellPanel(tags$details(
                      tags$summary("Models")
                    )
                    ),
                   wellPanel(tags$details(tags$summary("Inference"),
                                           selectInput("mct", "Multiple comparison contrasts:", 
                                                       choices = c('Dunnett contrasts' = 'Dunnett')),
                                           selectInput("alt", "Hypothesis:", 
                                                       choices = c('one sided' = 'less',
                                                                   'two sided' = 'two.sided'))
                                          )
                             ),
                   actionButton("goButton", "Run Simulation!")
                   ),
              mainPanel(
                tabsetPanel(id='main', 
                  tabPanel('Simulation-Design',  value='sd',
                           p("1000 draws from the specified design:"),
                           plotOutput("desplot"),
                           h3("Summary"), 
                           dataTableOutput("destab")
                  ), 
                  tabPanel("Global test", value = 'gt',
                   plotOutput("powplot"),
                   h3("Summary"),
                   # textOutput("test"),
                   dataTableOutput("powtable"),
                   downloadButton('downloadpowtable', 'Download table'),
                   downloadButton('downloadpowplot', 'Download plot')
                   ), 
                  tabPanel('LOEC',   value = 'loec',
                   plotOutput("loecplot"),
                   h3("Summary"),
                   dataTableOutput("loectable"),
                   downloadButton('downloadloectable', 'Download table'),
                   downloadButton('downloadloecplot', 'Download plot')
                   ),
                  tabPanel('Readme', value = 'rm',
                           includeMarkdown("md/README_power.md")
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
          ),
          tabPanel("About"
                  
          )
))
