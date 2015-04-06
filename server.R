library(shiny)
library(plyr)
library(reshape2)
library(multcomp)
library(ggplot2)

shinyServer(function(input, output) {
  ## --- Shiny functions -------------------------------------------------------
  # ggplot2 theme
  mytheme <- theme_gray(base_size = 12, base_family = "Helvetica") + 
    theme(
          text = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
          axis.title.y = element_text(size = 14, face = "bold", vjust = 1),
          legend.key = element_blank())
  
  # parse textInput
  numextractall <- function(string) { # http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
    as.numeric(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)), 
                      use.names = FALSE))
  } 
  
  ### --- Design functions -----------------------------------------------------
  des_sim <- function(N = 1000, ctrl, theta, effsize){
    mus <- rep(c(rep(ctrl, each = 2), rep(ctrl * effsize, each = 2)), each = N)
    thetas <- rep(theta, 4 * N) 
    x      <- factor(rep(1:4, each = N)) 
    y      <- rnegbin(N * 4, mus, thetas)
    return(data.frame(x = factor(x), y = y))
  }
  #   plot(des_sim(1000, 1000, 4, 0.1))
  #   df <- des_sim(1000, 1000, 4, 0.1)

  plot_des <- function(df) {
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_boxplot()
    return(p)
  }
  
  df_des <- function(df){
    ddply(df, .(x), summarize, mean = mean(y), variance = var(y))
  }
  
  ### --- Simulation functions -------------------------------------------------
  # simulation from negative binomial
  sim_negbin <- function(N, ctrl, theta, effsize, nsims){
    mus <- rep(c(rep(ctrl, each = 2), rep(ctrl * effsize, each = 2)), each = N)
    thetas <- rep(theta, 4 * N) 
    x      <- factor(rep(1:4, each = N)) 
    y      <- replicate(nsims, rnegbin(N * 4, mus, thetas))
    return(list(x = x, y = y))
  }
  # sim_negbin(10, 100, 4, 0.5, 10)
  
  # simulate scenarios
  sim_fun <- function(N, ctrl, theta, effsize, nsims){
    sims <- vector("list", length(N)) 
    # withProgress(message = 'Generating data', detail = "Scenario 0", value = 0, {
      for (i in seq_along(N)) {
        # incProgress(1 / run, detail = paste("part", i))
        sims[[i]] <- sim_negbin(N = N[i], ctrl, theta, effsize, nsims)
      }
    # })
    return(list(sims = sims, meta = N))
  }
  # sims <- sim_fun(5, 100, 4, 0.1, 100)

  # compare methods
  get_result <- function(z){
    ana <- function(y, x){
      # -------------
      # Transformations
      # ln(ax + 1) transformation
      A <- 1 / min(y[y != 0])         
      yt <- log(A * y + 1)
      df <- data.frame(x, y, yt)
      
      # -------------
      # gaussian
      modlm <- lm(yt ~ x, data = df)
      modlm.null <- lm(yt ~ 1, data = df)
      # quasipoisson (to tackle down convergence problems)
      modqglm <- glm(y ~ x, data = df, family = 'quasipoisson')
      modqglm.null <-  glm(y ~ 1, data = df, family = 'quasipoisson')
      
      # ------------- 
      # Test of effects
      # F Tests
      p_lm_f <- anova(modlm, modlm.null, test = 'F')[2, 'Pr(>F)']
      p_qglm_f <- anova(modqglm, modqglm.null, test = 'F')[2, 'Pr(>F)']
      
#       # ----------------
#       # LOEC
#       mc_lm <- summary(glht(modlm, linfct = mcp(x = 'Dunnett'),  
#                             alternative = 'less'), test = adjusted('holm'))$test$pvalues
#       suppressWarnings( # intended warnings about no min -> no LOEC
#         loeclm <- min(which(mc_lm < 0.05))
#       )
#       # quasi
#       mc_qglm <- summary(glht(modqglm, linfct = mcp(x = 'Dunnett'),  
#                               alternative = 'less'), test = adjusted('holm'))$test$pvalues
#       suppressWarnings( # intended warnings about no min -> no LOEC
#         loecqglm <- min(which(mc_qglm < 0.05))
#       ) 
      
      # ---------
      # return object
      return(list(p_lm_f = p_lm_f, p_qglm_f = p_qglm_f
#                   ,
#                   loeclm = loeclm, loecqglm = loecqglm
      )
      )
    }
    withProgress(message = 'Analysing simulation', detail = "0", value = 0, {
    res <- vector('list', ncol(z$y))
    for (i in seq_len(ncol(z$y))) {
      incProgress(1 / ncol(z$y), detail = i)
      res[[i]] <- ana(z$y[ , i], z$x)
    }
    })
    return(res)
  }
  # res1 <- get_result(sims$sims[[1]])
  
  # exract power
  get_power <- function(z){ 
    take <- c('p_lm_f', 'p_qglm_f')
    ps <- ldply(z, function(w) as.numeric(unlist(w[take])))
    names(ps) <- take
    ps <- melt(ps)
    out <- ddply(ps, .(variable), summarize,
                 power = sum(value < 0.05, na.rm = TRUE) / sum(!is.na(value))
                 )
    return(out)
  }
  # p1 <- get_power(res1)

  # plot power
  plot_power <- function(z){
    meta <- z$meta
    z <-  z$pow
    z$N <- rep(meta, each = 2)
    z$variable <-  factor(z$variable, unique(z$variable)[1:2], 
                                   labels = c('LM', 'QP'))
    
    out <- ggplot(z) +
      geom_line(aes(y = power, x = N, group = variable, linetype = variable)) +
      geom_point(aes(y = power, x = N, shape = variable), color = 'black', size = 4) +
      geom_hline(aes(yintercept = 0.8), linetype = 'dotted') +
      # axes
      labs(x = 'N', 
           y = expression(paste('Power (global test , ', alpha, ' = 0.05)'))) +
      # appearance
      mytheme + 
      # legend title
      scale_shape_manual('Method', values = c(16, 2, 4), 
                         labels = c('LM', 'QP')) +
      scale_linetype_discrete('Method', 
                              labels = c('LM', 'QP')) +
      ylim(c(0, 1))
    return(out)
  }


  # simulate and analyze
  res_fun <- function(sims){
    meta <- sims$meta
    sims <- sims$sims
    withProgress(message = 'Analysing data', detail = "N = 0", value = 0, {
      res <- vector("list", length(sims)) 
      for (i in seq_along(sims)) {
        incProgress(1 / length(sims), detail = paste("N = ", i))
        res[[i]] <- get_result(sims[[i]])
      }
    })
    out <- list(meta = meta, res = res)
    return(out)
  }

  
  ### --- design ---------------------------------------------------------------
  desdata <- reactive({
    des_sim(ctrl = input$muc, 
            theta  = input$theta,
            effsize = input$effsize)
  })
  output$desplot <- renderPlot({
    print(plot_des(desdata()))
  })
  
  output$destab <- renderDataTable({
    df_des(desdata())
    },
  options = list(paging = FALSE, searching = FALSE)
  )
  
  ### --- simulations
  resdata <- eventReactive(input$goButton, {
    res_fun(
      sim_fun(N = numextractall(input$N),
                         ctrl = input$muc,
                         nsims = input$nsims,
                         theta  = input$theta,
                         effsize = input$effsize
              )
      )
    })
  
  get_data <- eventReactive(input$goButton, {
    pow <- list(pow = ldply(resdata()$res, get_power), meta = resdata()$meta)
    return(pow)
  })
  
  output$powplot <- renderPlot({
    print(plot_power(get_data()))
    })
  
  output$powtable <- renderDataTable({
    df <- cbind(get_data()$pow, get_data()$meta[rep(seq_along(get_data()$meta), each = 2)])
    df$variable <-  factor(df$variable, unique(df$variable)[1:2], 
                          labels = c('lm', 'qp'))
    df <- df[ , -4]
    names(df) <- c('Model', 'Power', 'N')
    df}, 
    options = list(paging = FALSE, searching = FALSE)
    )
  
  # Downloads
  output$downloadData <- downloadHandler(
    filename = function() 'test.csv', 
    content = function(file) {
      outdata <- cbind(get_data()$pow, get_data()$meta[rep(seq_len(nrow(get_data()$meta)), each = 3), ])
      write.csv(outdata, file)
      })
  
  output$downloadPlot <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file)
      print(plot_power(get_data()))
      dev.off()
      })
})
