library(shiny)
library(plyr)
library(reshape2)
library(MASS)
library(multcomp)
library(ggplot2)

shinyServer(function(input, output) {
  
sim_negbin <- function(N, mu, theta, nsims){
  Nj     <- rep(N, times = 6)                # 6 groups
  mus    <- rep(mu, times = Nj)             # vector of mus
  thetas <- rep(theta, times = Nj)            # vector of thetas
  x      <- factor(rep(1:6, times = Nj))      # factor
  y      <- replicate(nsims, rnegbin(sum(Nj), mus, thetas)) # draw from negative binomial distribution
  return(list(x = x, y = y))
}


#' pairwise wilcox.test
#' @param y numeric; vector of data values
#' @param g factor; grouping vector
#' @param dunnett logical; if TRUE dunnett contrast, otherwise Tukey-contrasts
#' @param padj character; method for p-adjustment, see ?p.adjust.
pairwise_wilcox <- function(y, g, dunnett = TRUE, padj = 'holm', alternative = 'less'){
  tc <- t(combn(nlevels(g), 2))
  # take on dunnett comparisons
  if (dunnett) {
    tc <- tc[tc[ ,1] == 1, ]
  }
  pval <- numeric(nrow(tc))
  # use wilcox.exact (for tied data)
  for (i in seq_len(nrow(tc))) {
    pval[i] <- wilcox.test(y[as.numeric(g) == tc[i, 2]], 
                            y[as.numeric(g) == tc[i, 1]], 
                            alternative = alternative)$p.value
  }
  pval <- p.adjust(pval, padj)
  names(pval) = paste(levels(g)[tc[,1]], levels(g)[tc[,2]], sep = ' vs. ')
  return(pval)
}


get_result <- function(z, verbose = TRUE){
  if (verbose) {
    message('n: ', length(z$x) / 6, '; muc = ', mean(z$y[,1][z$x == 1]))
  }
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
    # negative binomial 
    modglm <- try(glm.nb(y ~ x, data = df), silent = TRUE)
    modglm.null <- try(glm.nb(y ~ 1, data = df), silent = TRUE)
    # quasipoisson (to tackle down convergence problems)
    modqglm <- glm(y ~ x, data = df, family = 'quasipoisson')
    modqglm.null <-  glm(y ~ 1, data = df, family = 'quasipoisson')
    
    # ------------- 
    # Test of effects
    # check convergence
    if (inherits(modglm, "try-error") | inherits(modglm.null, "try-error")) {
      p_glm_lr <- 'convergence error'
      p_glm_lrpb <- 'convergence error'
    } else {
      if (!is.null(modglm[['th.warn']]) | !is.null(modglm.null[['th.warn']])) {
        p_glm_lr <- 'convergence error'
        p_glm_lrpb <- 'convergence error'
      } else {
        p_glm_lr <- anova(modglm, modglm.null, test = 'Chisq')[2 , 'Pr(Chi)']
      }
    }
    # F Tests
    p_lm_f <- anova(modlm, modlm.null, test = 'F')[2, 'Pr(>F)']
    p_qglm_f <- anova(modqglm, modqglm.null, test = 'F')[2, 'Pr(>F)']
    
    # ----------------
    # LOEC
    mc_lm <- summary(glht(modlm, linfct = mcp(x = 'Dunnett'),  
                          alternative = 'less'), test = adjusted('holm'))$test$pvalues
    suppressWarnings( # intended warnings about no min -> no LOEC
      loeclm <- min(which(mc_lm < 0.05))
    )
    # negbin
    if (inherits(modglm, "try-error")) {
      loecglm <- 'convergence error'
    } else {
      if (!is.null(modglm[['th.warn']])) {
        loecglm <- 'convergence error'
      } else {
        mc_glm <- summary(glht(modglm, linfct = mcp(x = 'Dunnett'),  
                               alternative = 'less'), test = adjusted('holm'))$test$pvalues
        suppressWarnings(
          loecglm <- min(which(mc_glm  < 0.05))
        )
      }
    }
    # quasi
    mc_qglm <- summary(glht(modqglm, linfct = mcp(x = 'Dunnett'),  
                            alternative = 'less'), test = adjusted('holm'))$test$pvalues
    suppressWarnings( # intended warnings about no min -> no LOEC
      loecqglm <- min(which(mc_qglm < 0.05))
    ) 
    
    # ---------
    # return object
    return(list(p_lm_f = p_lm_f, p_glm_lr = p_glm_lr, p_qglm_f = p_qglm_f,
                loeclm = loeclm, loecglm = loecglm, loecqglm = loecqglm
    )
    )
  }
  res <- apply(z$y, 2, ana, x = z$x)
  return(res)
}

get_power <- function(z){ 
  # extract p-values
  take <- c('p_lm_f', 'p_glm_lr', 'p_qglm_f')
  ps <- ldply(z, function(w) as.numeric(unlist(w[take])))
  names(ps) <- take
  ps <- melt(ps)
  out <- ddply(ps, .(variable), summarize,
               power = sum(value < 0.05, na.rm = TRUE) / sum(!is.na(value)),
               conv = sum(!is.na(value)) / length(value))
  return(out)
}

plot_power <- function(z){
  meta = z$meta
  z <-  z$pow
  z$muc <- rep(meta$ctrl, each  = 3)
  z$N <- rep(meta$N, each = 3)
  z$variable <-  factor(z$variable, unique(z$variable)[1:5], 
                                 labels = c('lm', 'glm_nb', 'glm_qp'))
  
  out <- ggplot(z) +
    geom_line(aes(y = power, x = N, group = variable, linetype = variable)) +
    geom_point(aes(y = power, x = N, shape = variable), color = 'black', size = 4) +
    # axes
    labs(x = 'N', 
         y = expression(paste('Power (global test , ', alpha, ' = 0.05)'))) +
#     # appearance
#     mytheme + 
    # legend title
    scale_shape_manual('Method', values = c(16, 2, 4), 
                       labels = c('LM', expression(GLM[nb]), expression(GLM[qp]))) +
    scale_linetype_discrete('Method', 
                            labels = c('LM', expression(GLM[nb]), expression(GLM[qp]))) +
    ylim(c(0, 1))
  return(out)
}

# parse textInput
numextractall <- function(string) { # http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
  as.numeric(unlist(regmatches(string, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", string)), 
         use.names = FALSE))
} 


foo <- function(N, ctrl, nsims, theta, effsize){
  todo <- expand.grid(N = N, ctrl = ctrl)
  sims <- vector("list", nrow(todo)) 
  withProgress(message = 'Generating data', detail = "Scenario 0", value = 0, {
    run <- seq_len(nrow(todo))
    for (i in run) {
      incProgress(1 / run, detail = paste("part", i))
      N <- todo[i, 'N']
      takectrl <- todo[i, 'ctrl']
      # reduce t2-t5 to 50%
      taketrt <- takectrl * effsize
      mu <- c(rep(takectrl, each = 2), rep(taketrt, each = 4))
      sims[[i]] <- sim_negbin(N = N, mu = mu, nsims = nsims, theta = theta)
    }
  })
  withProgress(message = 'Analysing data', detail = "Scenario 0", value = 0, {
    res <- vector("list", length(sims)) 
    for (i in seq_along(sims)) {
      incProgress(1 / length(sims), detail = paste("Scenario", i))
      res[[i]] <- get_result(sims[[i]])
    }
  })
  
  out <- list(meta = todo, res = res)
  return(out)
}
# 
# res <- foo(N = c(3, 6 , 9),
#            ctrl = 16,
#            nsims = 10,
#            theta  <- rep(4, 6),
#            effsize = 0.5)
# pow <- list(pow = ldply(res$res, get_power), meta = res$meta)
# print(plot_power(pow))
  

  
  mydata <- eventReactive(input$goButton, {
    foo(N = numextractall(input$N),
                         ctrl = input$muc,
                         nsims = input$nsims,
                         theta  = rep(4, 6),
                         effsize = input$effsize)
    })
  
  plotInput <- reactive(function() {
    pow <- list(pow = ldply(mydata()$res, get_power), meta = mydata()$meta)
    print(plot_power(pow))
  })
  
  output$powplot <- renderPlot({
    input$goButton
    print(plotInput())
    })
})
