library(shiny)
library(MASS)
library(lme4)


## Fix manual sampling for dependency 

##############################
############### FUNCTIONS ############
####################################

numformat <- function(x, digits = 2) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}

ANOVA_dat_norm <- function(total.n, group.n, eta.sq, kurt = 0, skw = 0){
  ss.total <- total.n - 1
  ss.between <- eta.sq * ss.total
  ss.between.nn <- ss.between / group.n
  random.group <- sample(1:3, 1)
  pattern <- sample(1:2, 1)
  random.sign <- sample(c(-1,1), 1)
  
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  
  data[,1] <- as.factor(rep(c(1:3), each = group.n))
  if(kurt == 0){
    if( skw == 0){
      data[,2] <- rnorm(n = total.n, mean = 0, sd = sqrt(1-eta.sq))
    } else if (skw != 0){
      # mean equals alpha/beta, variance equals alpha / (beta^2). So to get a varaince of 1, beta has to equal sqrt(alpha)
      # skewness equals 2 / sqrt(alpha), so skewed for small values  
      data[,2] <- rgamma(n = total.n, shape = (2/skw)^2, rate = 2/skw) # variance equals 1
      data[,2] <- data[,2]  - 2/skw   # transform data such that mean equals 0
    }
  } else if (kurt != 0){
    if(skw == 0){
      if (kurt == 1){
        data[,2] <- c(rnorm(floor(total.n/2)), rnorm(ceiling(total.n/2), mean = 0, sd = .5))
      } else if (kurt == 2){
        data[,2] <- c(rnorm(floor(total.n/2)), rnorm(ceiling(total.n/2), mean = 0, sd = .2))
      } else if (kurt == 4){
        data[,2] <- c(rnorm(floor(total.n/2)), rnorm(ceiling(total.n/2), mean = 0, sd = .05))
      }
    } else if (skw != 0){
      if (kurt == 1){
        data[,2] <- c(rgamma(n = floor(total.n/2), shape = (2/skw)^2, rate = 2/skw) , rnorm(ceiling(total.n/2), mean = 0, sd = .5))
      } else if (kurt == 2){
        data[,2] <- c(rgamma(n = floor(total.n/2), shape = (2/skw)^2, rate = 2/skw) , rnorm(ceiling(total.n/2), mean = 0, sd = .2))
      } else if (kurt == 4){
        data[,2] <- c(rgamma(n = floor(total.n/2), shape = (2/skw)^2, rate = 2/skw) , rnorm(ceiling(total.n/2), mean = 0, sd = .05))
      }
      data[,2] <- data[,2]  - 2/skw   # transform data such that mean equals 0
    }
  }
  
  if(pattern == 1){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * 0.5) * random.sign
    data[data[,1] == sample((unique(data[,1])[-random.group]),1),2] <- data[data[,1] == sample((unique(data[,1])[-random.group]),1),2] - sqrt(ss.between.nn * 0.5) * random.sign
  } else if (pattern == 2){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * (2/3)) * random.sign
    data[data[,1] != random.group,2] <- data[data[,1] != random.group,2] - sqrt(ss.between.nn * (2/3))/2 * random.sign
  }
  data[,2] <- data[,2] * 2
  data[,2] <- data[,2] + 5
  return(data)
}

ANOVA_dat_man_norm <- function(total.n, group.n, mu1, mu2, mu3, sd.overall, kurt = 0, skw = 0){
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  
  ############ SD DOES NOT MATCH INPUT SD NOW< ADJUST!! 
  data[,1] <- as.factor(rep(c(1:3), each = group.n))
  if(kurt == 0){
    if( skw == 0){
      data[,2] <- rnorm(n = total.n, mean = 0, sd = sd.overall)
    } else if (skw != 0){
      # mean equals alpha/beta, variance equals alpha / (beta^2). So to get a varaince of 1, beta has to equal sqrt(alpha)
      # skewness equals 2 / sqrt(alpha), so skewed for small values  
      data[,2] <- rgamma(n = total.n, shape = (2/skw)^2, rate = 2/skw) # variance equals 1
      data[,2] <- data[,2]  - 2/skw   # transform data such that mean equals 0
    }
  } else if (kurt != 0){
    if(skw == 0){
      if (kurt == 1){
        data[,2] <- c(rnorm(floor(total.n/2), sd = sd.overall), rnorm(ceiling(total.n/2), mean = 0, sd = sd.overall/2))
      } else if (kurt == 2){
        data[,2] <- c(rnorm(floor(total.n/2), sd = sd.overall), rnorm(ceiling(total.n/2), mean = 0, sd = sd.overall/5))
      } else if (kurt == 4){
        data[,2] <- c(rnorm(floor(total.n/2), sd = sd.overall), rnorm(ceiling(total.n/2), mean = 0, sd = sd.overall/20))
      }
    } else if (skw != 0){
      if (kurt == 1){
        data[,2] <- c(rgamma(n = total.n/2, shape = (2/skw)^2, rate = 2/skw) , rnorm(total.n/2, mean = 0, sd = .5))
      } else if (kurt == 2){
        data[,2] <- c(rgamma(n = total.n/2, shape = (2/skw)^2, rate = 2/skw) , rnorm(total.n/2, mean = 0, sd = .2))
      } else if (kurt == 4){
        data[,2] <- c(rgamma(n = total.n/2, shape = (2/skw)^2, rate = 2/skw) , rnorm(total.n/2, mean = 0, sd = .05))
      }
      data[,2] <- data[,2]  - 2/skw   # transform data such that mean equals 0
    }
  }
  data[data[,1] == 1,2] <- data[data[,1] == 1,2] + mu1
  data[data[,1] == 2,2] <- data[data[,1] == 2,2] + mu2
  data[data[,1] == 3,2] <- data[data[,1] == 3,2] + mu3
  return(data)
}

Bootstr_Fdistr_norm <- function(meanstar, R, act_data, group.n){
  Fstar <- numeric(R)
  gr1 <- act_data[act_data[,1] == 1,2] - meanstar[1]
  gr2 <- act_data[act_data[,1] == 2,2] - meanstar[2]
  gr3 <- act_data[act_data[,1] == 3,2] - meanstar[3]
  simdata <- as.data.frame(matrix(,ncol = 2, nrow = 3*group.n))
  colnames(simdata) <- c("group", "outcome")
  simdata[,1] <- act_data[,1]
  for(i in 1:R){
    simdata[simdata[,1] == 1,2] <- sample(gr1, size = group.n, replace = T)
    simdata[simdata[,1] == 2,2] <- sample(gr2, size = group.n, replace = T)
    simdata[simdata[,1] == 3,2] <- sample(gr3, size = group.n, replace = T)
    Fstar[i] <- anova(lm(outcome~group, data = simdata))[1,4]
  }
  return(Fstar)
}

ANOVA_dat_outl <- function(total.n, group.n, eta.sq){
  ss.total <- total.n - 1
  ss.between <- eta.sq * ss.total
  ss.between.nn <- ss.between / group.n
  random.group <- sample(1:3, 1)
  pattern <- sample(1:2, 1)
  random.sign <- sample(c(-1,1), 1)
  
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  
  data[,1] <- as.factor(rep(c(1:3), each = group.n))
  data[,2] <- rnorm(n = total.n, mean = 0, sd = sqrt(1-eta.sq))
  if(pattern == 1){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * 0.5) * random.sign
    data[data[,1] == sample((unique(data[,1])[-random.group]),1),2] <- data[data[,1] == sample((unique(data[,1])[-random.group]),1),2] - sqrt(ss.between.nn * 0.5) * random.sign
  } else if (pattern == 2){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * (2/3)) * random.sign
    data[data[,1] != random.group,2] <- data[data[,1] != random.group,2] - sqrt(ss.between.nn * (2/3))/2 * random.sign
  }
  data[,2] <- data[,2] * 2
  data[,2] <- data[,2] + 5
  return(data)
}

ANOVA_dat_man_outl <- function(total.n, group.n, mu1, mu2, mu3, sd.overall){
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  
  data[,1] <- as.factor(rep(c(1:3), each = group.n))
  data[,2] <- rnorm(n = total.n, mean = 0, sd = sd.overall)
  data[data[,1] == 1,2] <- data[data[,1] == 1,2] + mu1
  data[data[,1] == 2,2] <- data[data[,1] == 2,2] + mu2
  data[data[,1] == 3,2] <- data[data[,1] == 3,2] + mu3
  return(data)
}

ANOVA_dat_var <- function(total.n, group.n, eta.sq, var.rat, var.aloc = 0, ss.rat){
  # var.aloc: 0 = group sizes are equal, not activate var aloc
  #           1 = smallest variance belongs to smallest group size
  #           2 = smallest variance belongs to largest group size
  ss.total <- total.n - 1
  ss.between <- eta.sq * ss.total
  ss.between.nn <- ss.between / group.n
  random.group <- sample(1:3, 1)
  random.group2 <- sample(c(1:3)[-random.group], 1)
  pattern <- sample(1:2, 1)
  random.sign <- sample(c(-1,1), 1)
  group.ss <- sample(c(1:3), 3, replace = FALSE)
  if(var.aloc == 0){
    group.var <- sample(c(1:3), 3, replace = FALSE)
  } else if (var.aloc == 1){
    group.var <- group.ss
  } else if (var.aloc == 2){
    group.var <- numeric(3)
    group.var[group.ss == 3] <- 1
    group.var[group.ss == 2] <- 2
    group.var[group.ss == 1] <- 3
  }
  
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  group.size <- rep(group.n, 3)
  if(ss.rat == 1){
    group.size <- rep(group.n,3)
  } else if(ss.rat == 4){
    group.size <- c(round(total.n * 1/7,0), round(total.n * 2/7,0), round(total.n * 4/7,0))
  } else if (ss.rat == 8){
    group.size <- c(round(total.n * 1/12,0), round(total.n * 3/12,0), round(total.n * 8/12,0))
  }
  var <- 1-eta.sq
  ##### INSERT GROUP SIZE DEVISION! 
  data[,1] <- as.factor(rep(c(1:3), times = c(group.size[group.ss])))
  data[data[,1] == which(group.var == 1),2] <- rnorm(n = group.size[group.ss][which(group.var == 1)], 
                                                     mean = 0, sd = sqrt(var / sqrt(var.rat)))
  data[data[,1] == which(group.var == 2),2] <- rnorm(n = group.size[group.ss][which(group.var == 2)], 
                                                     mean = 0, sd = sqrt(var))
  data[data[,1] == which(group.var == 3),2] <- rnorm(n = group.size[group.ss][which(group.var == 3)], 
                                                     mean = 0, sd = sqrt(var * sqrt(var.rat)))
  act_ss.within <- sum(data[,2]^2)
  var.factor <- act_ss.within/(ss.total - ss.between)
  data[,2] <- data[,2] / sqrt(var.factor) 
  if(pattern == 1){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * 0.5) * random.sign
    data[data[,1] == random.group2,2] <- data[data[,1] == random.group2,2] - sqrt(ss.between.nn * 0.5) * random.sign
  } else if (pattern == 2){
    data[data[,1] == random.group,2] <- data[data[,1] == random.group,2] + sqrt(ss.between.nn * (2/3)) * random.sign
    data[data[,1] != random.group,2] <- data[data[,1] != random.group,2] - sqrt(ss.between.nn * (2/3))/2 * random.sign
  }
  data[,2] <- data[,2] * 2
  data[,2] <- data[,2] + 5
  return(data)
}

ANOVA_dat_man_var <- function(total.n, group.n, mu1, mu2, mu3, sd.overall, var.rat, var.aloc = 0, ss.rat){
  data <- as.data.frame(matrix(, ncol = 2, nrow = total.n))
  colnames(data) <- c("group", "outcome")
  group.ss <- sample(c(1:3), 3, replace = FALSE)
  if(var.aloc == 0){
    group.var <- sample(c(1:3), 3, replace = FALSE)
  } else if (var.aloc == 1){
    group.var <- group.ss
  } else if (var.aloc == 2){
    group.var <- numeric(3)
    group.var[group.ss == 3] <- 1
    group.var[group.ss == 2] <- 2
    group.var[group.ss == 1] <- 3
  }
  group.size <- rep(group.n, 3)
  if(ss.rat == 1){
    group.size <- rep(group.n,3)
    } else if(ss.rat == 4){
      group.size <- c(round(total.n * 1/7,0), round(total.n * 2/7,0), round(total.n * 4/7,0))
    } else if (ss.rat == 8){
      group.size <- c(round(total.n * 1/12,0), round(total.n * 3/12,0), round(total.n * 8/12,0))
    }
  data[,1] <- as.factor(rep(c(1:3), times = c(group.size[group.ss])))
  data[data[,1] == which(group.var == 1),2] <- rnorm(n = group.size[group.ss][which(group.var == 1)], 
                                                       mean = 0, sd = sqrt(sd.overall^2 / sqrt(var.rat)))
  data[data[,1] == which(group.var == 2),2] <- rnorm(n = group.size[group.ss][which(group.var == 2)], 
                                                       mean = 0, sd = sd.overall)
  data[data[,1] == which(group.var == 3),2] <- rnorm(n = group.size[group.ss][which(group.var == 3)], 
                                                       mean = 0, sd = sqrt(sd.overall^2 * sqrt(var.rat)))
 # act_ss.within <- sum(data[,2]^2)
 #var.factor <- act_ss.within/(ss.total - ss.between)
 # data[,2] <- data[,2] / sqrt(var.factor) 
    
  data[data[,1] == 1,2] <- data[data[,1] == 1,2] + mu1
  data[data[,1] == 2,2] <- data[data[,1] == 2,2] + mu2
  data[data[,1] == 3,2] <- data[data[,1] == 3,2] + mu3
  return(data)
}

Bootstr_Fdistr_var <- function(meanstar, R, act_data, group.size){
  Fstar <- numeric(R)
  gr1 <- act_data[act_data[,1] == 1,2] - meanstar[1]
  gr2 <- act_data[act_data[,1] == 2,2] - meanstar[2]
  gr3 <- act_data[act_data[,1] == 3,2] - meanstar[3]
  simdata <- as.data.frame(matrix(,ncol = 2, nrow = sum(group.size)))
  colnames(simdata) <- c("group", "outcome")
  simdata[,1] <- act_data[,1]
  for(i in 1:R){
    simdata[simdata[,1] == 1,2] <- sample(gr1, size = group.size[1], replace = T)
    simdata[simdata[,1] == 2,2] <- sample(gr2, size = group.size[2], replace = T)
    simdata[simdata[,1] == 3,2] <- sample(gr3, size = group.size[3], replace = T)
    Fstar[i] <- anova(lm(outcome~group, data = simdata))[1,4]
  }
  return(Fstar)
}

ML_dat <- function(GS, NG, ICC, d, fixed.int = 4, scale = 1){
  # GS = number of observations per cell	
  # NG = number of cells		
  # ICC = Intraclass correlation (this determines the intercept variance of full model as well)
  # d = Cohen's difference between groups
  Ngr <- rep(GS, NG)
  ntot <- sum(Ngr)
  int.var <- -ICC/(ICC-1)
  # Based on intercept variance and what second level predictor explains, obtain intercept variance full model
  v <- matrix(rnorm(NG, mean = 0, sd = sqrt(int.var)), ncol = 1)
  # simulate for 2nd level variance and covariance terms
  ve <- 1 - (d/(sqrt(d^2+4)))^2 	
  # Based on standardized predictor, obtain residual error to put in simulation
  Dist <- c(rep(0,(GS*(NG/2))), rep(1,(GS*(NG/2))))
  # Simulate predictor variable, syntax also works for unequal group sizes
  
  Ngr2 <- c(0, Ngr)
  dat=matrix(0,ntot,3)		# create matrix for standardized predictor variables, outcomes and group membership
  dat[,1] <- Dist
  for (ic in 1:NG) {			# obtain outcome values
    ic1=ic-1
    ist= 1 + (sum(Ngr2[1:ic]))
    iend= sum(Ngr2[1:(ic + 1)])
    dat[ist:iend,2]= d*Dist[ist:iend] + v[ic,1] + rnorm(Ngr[ic],0,sqrt(ve))
    dat[ist:iend,2]= dat[ist:iend,2] * (scale) + fixed.int
    dat[ist:iend,3]=ic1+1		# put cell id in last column
  }
  
  dat=as.data.frame(dat)
  colnames(dat)=c("Condition", "Outcome", "ClusterID")
  return(dat)	
}


############################
########## SERVER
#######################

server <- function(input, output) {
  
  rv <- reactiveValues(data =  numeric(10))

  ################
  ################ NORMALITY##########
  ################

      observeEvent(input$runNORM, {
    if(input$EffSize1 == 'No effect'){
      rv$effect.size <- 0
    } else if(input$EffSize1 == 'Small'){
      rv$effect.size <- 0.02
    } else if(input$EffSize1 == 'Medium'){
      rv$effect.size <- 0.13
    } else if(input$EffSize1 == 'Large'){
      rv$effect.size <- 0.26
    }
    
    if(input$skew == 'No skewness (= 0.0)'){
      rv$skew <- 0
    } else if(input$skew == 'Small skewness (= 1.0)'){
      rv$skew <- 1
    } else if(input$skew == 'Medium skewness (= 2.0)'){
      rv$skew <- 2
    } else if(input$skew == 'Large skewness (= 4.0)'){
      rv$skew <- 4
    }
    
    if(input$kurt == 'No kurtosis (= 0.0)'){
      rv$kurt <- 0
    } else if(input$kurt == 'Small positive kurtosis (= 1.0)'){
      rv$kurt <- 1
    } else if(input$kurt == 'Medium positive kurtosis (= 2.0)'){
      rv$kurt <- 2
    } else if(input$kurt == 'Large positive kurtosis (= 4.0)'){
      rv$kurt <- 4
    }
    
    if(input$EffSize1 !='Manual'){
      rv$data <- ANOVA_dat_norm(total.n = input$groupsizeNORM*3, group.n = input$groupsizeNORM, eta.sq = rv$effect.size, skw = rv$skew, kurt = rv$kurt)
    } else if (input$EffSize1 =='Manual'){
      rv$data <- ANOVA_dat_man_norm(total.n = input$groupsizeNORM*3, group.n = input$groupsizeNORM, 
                                    mu1 = input$mu1NORM, mu2 = input$mu2NORM, mu3 = input$mu3NORM, 
                                    sd.overall = input$sigmaNORM, skw = rv$skew, kurt = rv$kurt)
    }
    
    output$boxplNORM<- renderPlot({
      if(input$EffSize1 !='Manual'){
        boxplot(outcome ~ group, data = rv$data[order(rv$data[,1]),], names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), ylim = c(-2,12), boxwex = 0.25)
      } else {
        boxplot(outcome ~ group, data = rv$data[order(rv$data[,1]),], names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), boxwex = 0.25)
      }
      p.color2 <- c("cadetblue", "coral", "darkolivegreen4")
      for(i in 1:3){
        points(x = jitter(unclass(factor(rv$data[,1])[rv$data[,1] == sort(unique(rv$data[,1]))[i]]), amount = 0.01), 
               y = rv$data[rv$data[,1] == sort(unique(rv$data[,1]))[i],2], col = p.color2[i], pch = 1, lwd = 3)
      }
    })
    output$normplNORM <- renderPlot({
      max.dens <- numeric(3)
      for(i in 1:3){
        max.dens[i] <- max(density(rv$data[rv$data[,1] == i,2], adjust = 2)$y)
      }
      max.dens2 <- max(max.dens)
      plot(density(rv$data[rv$data[,1] == 1,2], adjust = 2), col = "cadetblue4", ylab = "Density", type = "l", yaxt = "n", lwd = 2, 
           xlab = "Outcome", main = "", xlim = c(-5, 15), ylim = c(0, max.dens2))
      lines(density(rv$data[rv$data[,1] == 2,2], adjust = 2), col = "coral2", ylab = "Density", type = "l", lwd = 2)
      lines(density(rv$data[rv$data[,1] == 3,2], adjust = 2), col = "darkolivegreen", ylab = "Density", type = "l", lwd = 2)
      polygon(density(rv$data[rv$data[,1] == 1,2], adjust = 2), col = rgb(t(col2rgb("aliceblue")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(density(rv$data[rv$data[,1] == 2,2], adjust = 2), col = rgb(t(col2rgb("mistyrose")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(density(rv$data[rv$data[,1] == 3,2], adjust = 2), col = rgb(t(col2rgb("darkseagreen1")), maxColorValue = 255, alpha = 0.4*255), border = NA)
    }) 
    
    output$aovSummary1NORM <- renderTable(rownames = TRUE, na = "",{
      return(anova(lm(outcome ~ group, data = rv$data)))
    })
    output$descriptives1NORM <- renderTable(rownames = TRUE,{
      means <- round(tapply(rv$data[,2], rv$data[,1], mean),2)
      sds <-  round(tapply(rv$data[,2], rv$data[,1], sd),2)
      vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)
      ss <- as.vector(table(rv$data[,1]))
      descr <- as.data.frame(matrix(,ncol = 3, nrow = 4))
      colnames(descr) <- paste("Group", 1:3)
      rownames(descr) <- c("Mean", "Sd", "Variance", "Sample size")
      descr[1,] <- round(means,2)
      descr[2,] <- round(sds,2)
      descr[3,] <- round(vars,2)
      descr[4,] <- ss
      return(descr)
    })
    
    
    output$p_val1NORM <- renderText({
      paste("P-value ANOVA: ",
            formatC(numformat(round(anova(lm(outcome ~ group, data = rv$data))[1,5], 3), digits = 3), format='f', digits=3)
      )      
    })
    
    output$p_val2NORM <- renderText({
      isolate({
        means <- tapply(rv$data[,2], rv$data[,1], mean)
        Fdist_act <- Bootstr_Fdistr_norm(meanstar = means, R = 1000, act_data = rv$data, group.n = input$groupsizeNORM)
        realFstar <- anova(lm(outcome ~ group, data = rv$data))[1,4]
        boot_p <- mean(Fdist_act >= realFstar)
        paste("P-value bootstrapped ANOVA: ",
              formatC(numformat(round(boot_p, 3), digits = 3), format='f', digits=3)
        )
      })
    }) 
    
  }
  )  
 
  ################
  ################ OUTLIERS ##########
  ################
  
  observeEvent(input$runOUTL, {
    if(input$EffSize2 == 'No effect'){
      rv$effect.size <- 0
    } else if(input$EffSize2 == 'Small'){
      rv$effect.size <- 0.02
    } else if(input$EffSize2 == 'Medium'){
      rv$effect.size <- 0.13
    } else if(input$EffSize2 == 'Large'){
      rv$effect.size <- 0.26
    }
    
    rv$outl.val <- input$outl.val
    rv$outl.group <- 1
    if(input$group.outl == 'Group 2'){
      rv$outl.group <- 2
    } else if (input$group.outl == 'Group 3'){
      rv$outl.group <- 3
    }
    
    if(input$EffSize2 !='Manual'){
      rv$data <- ANOVA_dat_outl(total.n = input$groupsizeOUTL*3, group.n = input$groupsizeOUTL, eta.sq = rv$effect.size)
    } else if (input$EffSize2 =='Manual'){
      rv$data <- ANOVA_dat_man_outl(total.n = input$groupsizeOUTL*3, group.n = input$groupsizeOUTL, 
                                    mu1 = input$mu1OUTL, mu2 = input$mu2OUTL, mu3 = input$mu3OUTL, sd.overall = input$sigmaOUTL )
    }
    
    if(input$TypeOutl == 'Specify outlier'){
      if (rv$outl.val > 5){
        rv$data <- rv$data[-c(which.max(rv$data[rv$data[,1] == rv$outl.group,2]) + ((rv$outl.group-1)*input$groupsizeOUTL)),]
        rv$data <- rbind(c(rv$outl.group, rv$outl.val), rv$data)
      } else if (rv$outl.val <= 5) {
        rv$data <- rv$data[-c(which.min(rv$data[rv$data[,1] == rv$outl.group,2]) + ((rv$outl.group-1)*input$groupsizeOUTL)),]
        rv$data <- rbind(c(rv$outl.group, rv$outl.val), rv$data)      }
    }  
    
    output$boxplOUTL <- renderPlot({
      if(input$EffSize2 !='Manual'){
        boxplot(outcome ~ group, data = rv$data[order(rv$data[,1]),], names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), ylim = c(-5,15), boxwex = 0.25)
      } else {
        boxplot(outcome ~ group, data = rv$data[order(rv$data[,1]),], names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), boxwex = 0.25)
      }
      p.color2 <- c("cadetblue", "coral", "darkolivegreen4")
      for(i in 1:3){
        points(x = jitter(unclass(factor(rv$data[,1])[rv$data[,1] == sort(unique(rv$data[,1]))[i]]), amount = 0.01), 
               y = rv$data[rv$data[,1] == sort(unique(rv$data[,1]))[i],2], col = p.color2[i], pch = 1, lwd = 3)
      }
      if(input$TypeOutl != 'No Outlier'){
        points(x = rv$outl.group, y = rv$data[1,2], pch = 17, col = p.color2[ rv$outl.group], cex = 2)
        legend("topright", pch = 17, legend = "Outlier", col = p.color2[rv$outl.group], cex = 1.5)
      }
      
    })
    output$normplOUTL <- renderPlot({
      means <- tapply(rv$data[,2],rv$data[,1], mean)
      sds <- tapply(rv$data[,2],rv$data[,1], sd)
      if(input$EffSize2 !='Manual'){
        x <- seq(-5, 15, length = 200)
      } else {
        x <- seq(min(rv$data[,2])-5, max(rv$data[,2])+5, length = 200)
      }  
      hx <- cbind(dnorm(x, mean = means[1], sd = sds[1]),
                  dnorm(x, mean = means[2], sd = sds[2]),
                  dnorm(x, mean = means[3], sd = sds[3]))
      max.hx = max(hx)
      plot(x, hx[,1], col = "cadetblue4", ylab = "Density", type = "l", yaxt = "n", xlim = c(-5,15), ylim = c(0,max.hx), lwd = 2, xlab = "Outcome")
      lines(x, hx[,2], col = "coral2", ylab = "Density", type = "l", lwd = 2)
      lines(x, hx[,3], col = "darkolivegreen", ylab = "Density", type = "l", lwd = 2)
      polygon(x, hx[,1], col = rgb(t(col2rgb("aliceblue")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(x, hx[,2], col = rgb(t(col2rgb("mistyrose")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(x, hx[,3], col = rgb(t(col2rgb("darkseagreen1")), maxColorValue = 255, alpha = 0.4*255), border = NA)
    }) 
    
    output$aovSummary1OUTL <- renderTable(rownames = TRUE, {
      return(anova(lm(outcome ~ group, data = rv$data)))
    })
    output$descriptives1OUTL <- renderTable(rownames = TRUE,{
      means <- round(tapply(rv$data[,2], rv$data[,1], mean),2)
      sds <-  round(tapply(rv$data[,2], rv$data[,1], sd),2)
      vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)
      ss <- as.vector(table(rv$data[,1]))
      descr <- as.data.frame(matrix(,ncol = 3, nrow = 4))
      colnames(descr) <- paste("Group", 1:3)
      rownames(descr) <- c("Mean", "Sd", "Variance", "Sample size")
      descr[1,] <- round(means,2)
      descr[2,] <- round(sds,2)
      descr[3,] <- round(vars,2)
      descr[4,] <- ss
      return(descr)
    })
    
    output$aovSummary2OUTL <- renderTable(rownames = TRUE, na = "",{
      if(input$TypeOutl != 'No Outlier'){
        return(anova(lm(outcome ~ group, data = rv$data[-1,])))
      } else {
        return(anova(lm(outcome ~ group, data = rv$data)))
      }
      
    })
    output$descriptives2OUTL <- renderTable(rownames = TRUE,{
      descr <- as.data.frame(matrix(,ncol = 3, nrow = 2))
      colnames(descr) <- paste("Group", 1:3)
      rownames(descr) <- c("Means", "Sds")
      if(input$TypeOutl != 'No Outlier'){
        means <- round(tapply(rv$data[-1,2], rv$data[-1,1], mean),2)
        sds <-  round(tapply(rv$data[-1,2], rv$data[-1,1], sd),2)
        vars <-  round(tapply(rv$data[-1,2], rv$data[-1,1], var),2)
        ss <- as.vector(table(rv$data[-1,1]))
      } else {
        means <- round(tapply(rv$data[,2], rv$data[,1], mean),2)
        sds <-  round(tapply(rv$data[,2], rv$data[,1], sd),2)    
        vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)
        ss <- as.vector(table(rv$data[,1]))
      }
      descr <- as.data.frame(matrix(,ncol = 3, nrow = 4))
      colnames(descr) <- paste("Group", 1:3)
      rownames(descr) <- c("Mean", "Sd", "Variance", "Sample size")
      descr[1,] <- round(means,2)
      descr[2,] <- round(sds,2)
      descr[3,] <- round(vars,2)
      descr[4,] <- ss
      return(descr)
    })
    
    output$p_val1OUTL <- renderText({
      paste("P-value ANOVA with outlier: ",
            formatC(numformat(round(anova(lm(outcome ~ group, data = rv$data))[1,5], 3), digits = 3), format='f', digits=3 )
      )      
    })
    
    output$p_val2OUTL <- renderText({
      if(input$TypeOutl != 'No Outlier'){
        paste("P-value ANOVA without outlier: ",
              formatC(numformat(round(anova(lm(outcome ~ group, data = rv$data[-1,]))[1,5], 3), digits = 3), format='f', digits=3)
        )
      } else if(input$TypeOutl == 'No Outlier'){
        paste("P-value ANOVA without outlier: ",
              formatC(numformat(round(anova(lm(outcome ~ group, data = rv$data))[1,5], 3), digits = 3), format='f', digits=3)
        )
      }
    })
  })  
  
  
  ################
  ################ HOMOGENEITY OF VARIANCE ##########
  ################
  
  observeEvent(input$runVAR, {
    if(input$EffSize3 == 'No effect'){
      rv$effect.size <- 0
    } else if(input$EffSize3 == 'Small'){
      rv$effect.size <- 0.02
    } else if(input$EffSize3 == 'Medium'){
      rv$effect.size <- 0.13
    } else if(input$EffSize3 == 'Large'){
      rv$effect.size <- 0.26
    }
    
    if(input$TypeVar == '1:1'){
      rv$var.ratio <- 1
    } else if(input$TypeVar == '1:5'){
      rv$var.ratio <- 5
    } else if(input$TypeVar == '1:10'){
      rv$var.ratio <- 10
    } else if(input$TypeVar == '1:20'){
      rv$var.ratio <- 20
    }
    
    
    if(input$ssRat == '1:1'){
      rv$ss.rat <- 1
    } else if(input$ssRat == '1:4'){
      rv$ss.rat <- 4
    } else if(input$ssRat == '1:8'){
      rv$ss.rat <- 8
    } 
    
    rv$var.aloc <- 0
    if(input$varAloc == 'group with smallest sample size'){
      rv$var.aloc <- 1
    } else if(input$varAloc == 'group with largest sample size'){
      rv$var.aloc <- 2
    } 
    
    
    if(input$EffSize3 !='Manual'){
      rv$data <- ANOVA_dat_var(total.n = input$groupsizeVAR*3, group.n = input$groupsizeVAR, 
                           eta.sq = rv$effect.size, var.rat = rv$var.ratio, 
                           ss.rat = rv$ss.rat, var.aloc = rv$var.aloc)
    } else if (input$EffSize3 =='Manual'){
      rv$data <- ANOVA_dat_man_var(total.n = input$groupsizeVAR*3, group.n = input$groupsizeVAR, 
                                   mu1 = input$mu1VAR, mu2 = input$mu2VAR, mu3 = input$mu3VAR, 
                                   sd.overall = input$sigmaVAR, var.rat = rv$var.ratio,
                                   ss.rat = rv$ss.rat, var.aloc = rv$var.aloc)
    }
    
    
    output$boxplVAR <- renderPlot({
      if(input$EffSize3 !='Manual'){
        boxplot(outcome ~ group, data = rv$data, names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), ylim = c(-2,12), boxwex = 0.25)
      } else {
        boxplot(outcome ~ group, data = rv$data, names = c("Group 1", "Group 2", "Group 3"), ylab = "Outcome",
                col = c("aliceblue", "mistyrose", "darkseagreen1"), border = c("cadetblue4", "coral2", "darkolivegreen"), boxwex = 0.25)
      }
      p.color2 <- c("cadetblue", "coral", "darkolivegreen4")
      for(i in 1:3){
        points(x = jitter(unclass(factor(rv$data[,1])[rv$data[,1] == unique(rv$data[,1])[i]]), amount = 0.01), 
               y = rv$data[rv$data[,1] == unique(rv$data[,1])[i],2], col = p.color2[i], pch = 1, lwd = 3)
      }
    })
    output$normplVAR <- renderPlot({
      means <- tapply(rv$data[,2],rv$data[,1], mean)
      sds <- tapply(rv$data[,2],rv$data[,1], sd)
      if(input$EffSize3 !='Manual'){
        x <- seq(-5, 15, length = 100)
      } else {
        x <- seq(min(rv$data[,2])-5, max(rv$data[,2])+5, length = 200)
      }  
      hx <- cbind(dnorm(x, mean = means[1], sd = sds[1]),
                  dnorm(x, mean = means[2], sd = sds[2]),
                  dnorm(x, mean = means[3], sd = sds[3]))
      max.hx = max(hx)
      plot(x, hx[,1], col = "cadetblue4", ylab = "Density", type = "l", yaxt = "n", ylim = c(0,max.hx), lwd = 2, xlab = "Outcome")
      lines(x, hx[,2], col = "coral2", ylab = "Density", type = "l", lwd = 2)
      lines(x, hx[,3], col = "darkolivegreen", ylab = "Density", type = "l", lwd = 2)
      polygon(x, hx[,1], col = rgb(t(col2rgb("aliceblue")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(x, hx[,2], col = rgb(t(col2rgb("mistyrose")), maxColorValue = 255, alpha = 0.4*255), border = NA)
      polygon(x, hx[,3], col = rgb(t(col2rgb("darkseagreen1")), maxColorValue = 255, alpha = 0.4*255), border = NA)
    }) 
    
    output$aovSummary1VAR <- renderTable(rownames = TRUE, na = "", {
      return(anova(lm(outcome ~ group, data = rv$data)))
    })
    output$descriptives1VAR <- renderTable(rownames = TRUE,{
      means <- round(tapply(rv$data[,2], rv$data[,1], mean),2)
      sds <-  round(tapply(rv$data[,2], rv$data[,1], sd),2)
      vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)
      ss <- as.vector(table(rv$data[,1]))
      descr <- as.data.frame(matrix(,ncol = 3, nrow = 4))
      colnames(descr) <- paste("Group", 1:3)
      rownames(descr) <- c("Mean", "Sd", "Variance", "Sample size")
      descr[1,] <- round(means,2)
      descr[2,] <- round(sds,2)
      descr[3,] <- round(vars,2)
      descr[4,] <- ss
      return(descr)
    })
    
    output$true.rat <- renderText({
      vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)    
      paste(
        "1 :", formatC(round(max(vars)/min(vars),2), format='f', digits=2 )
      )
    })
    
    output$p_val1VAR <- renderText({
      paste("P-value ANOVA: ",
            formatC(numformat(round(anova(lm(outcome ~ group, data = rv$data))[1,5], 3),digits = 3), format='f', digits=3 )
      )      
    })
    
    output$p_val2VAR <- renderText({
      isolate({
        means <- tapply(rv$data[,2], rv$data[,1], mean)
        Fdist_act <- Bootstr_Fdistr_var(meanstar = means, R = 1000, act_data = rv$data, group.size = table(rv$data[,1]))
        realFstar <- anova(lm(outcome ~ group, data = rv$data))[1,4]
        boot_p <- mean(Fdist_act >= realFstar)
        paste("P-value bootstrapped ANOVA: ",
              formatC(numformat(round(boot_p, 3), digits = 3), format='f', digits=3)
        )
      })
    })
  })  
  
  
  ##########################
  ############## iNDEPENDENCE
  #######################
  
  observeEvent(input$runIND, {
    if(input$EffSize4 == 'No effect'){
      rv$d.val <- 0
    } else if(input$EffSize4 == 'Small'){
      rv$d.val <- 0.2
    } else if(input$EffSize4 == 'Medium'){
      rv$d.val <- 0.5
    } else if(input$EffSize4 == 'Large'){
      rv$d.val <- 0.8
    } else if(input$EffSize4 == 'Manual'){
      rv$d.val <- (input$mu2 - input$mu1)/ input$sigma
    }
    
    if (input$TypeDep == 'Siblings (ICC = .30)'){
      if(input$EffSize4 != 'Manual'){
        rv$data <- ML_dat(GS = 2, NG= input$Nclusters, ICC= .30, d = rv$d.val)
      } else {
        rv$data <- ML_dat(GS = 2, NG= input$Nclusters, ICC= .30, d = rv$d.val, fixed.int = input$mu1, scale = input$sigma)
      }
      rv$title <- "Sibling ID"
    } else if (input$TypeDep == 'Twins (ICC = .50)'){
      if(input$EffSize4 != 'Manual'){
        rv$data <- ML_dat(GS = 2, NG= input$Nclusters, ICC= .50, d = rv$d.val)
      } else {
        rv$data <- ML_dat(GS = 2, NG= input$Nclusters, ICC= .50, d = rv$d.val, 
                          fixed.int = input$mu1, scale = input$sigma)
      }
      rv$title <- "Twin ID"
    } else if (input$TypeDep =='Classmates (ICC = .10)'){
      if(input$EffSize4 != 'Manual'){
        rv$data <- ML_dat(GS = 30, NG= input$Nclusters, ICC= .10, d = rv$d.val)
      } else {
        rv$data <- ML_dat(GS = 30, NG= input$Nclusters, ICC= .10, d = rv$d.val, 
                          fixed.int = input$mu1, scale = input$sigma)
      }
      rv$title <- "Class ID"
    }
    else if (input$TypeDep =='None (ICC = .00)'){
      if(input$EffSize4 != 'Manual'){
        rv$data <- ML_dat(GS = 10, NG= input$Nclusters, ICC= .0, d = rv$d.val)
      } else {
        rv$data <- ML_dat(GS = 10, NG= input$Nclusters, ICC= .0, d = rv$d.val, 
                          fixed.int = input$mu1, scale = input$sigma)
      }
      rv$title <- "Cluster ID"
    }
    else if (input$TypeDep =='Manual'){
      if(input$EffSize4 != 'Manual'){
        rv$data <- ML_dat(GS = input$Csize, NG= input$Nclusters, ICC= input$ICC, d = rv$d.val)
      } else {
        rv$data <- ML_dat(GS = input$Csize, NG= input$Nclusters, ICC= input$ICC, d = rv$d.val, 
                          fixed.int = input$mu1IND, scale = input$sigmaIND)
      }
      rv$title <- "Cluster ID"
    }
    
    
    output$scatterIND <- renderPlot({
      p.color <- c("cadetblue", "coral")
      if(input$EffSize4 !='Manual'){
        plot(x = rv$data[,3], y = rv$data[,2], type = "n", ylab = "Outcome", xlab = rv$title, xaxt = "n", ylim = c(-2,12))
      } else {
        plot(x = rv$data[,3], y = rv$data[,2], type = "n", ylab = "Outcome", xlab = rv$title, xaxt = "n")
      }
      axis(1, at = unique(rv$data[,3]))
      abline(h = mean(rv$data[,2]), lty = 2, col = "grey")
      for(i in 1:2){
        points(x = rv$data[rv$data[,1] == (i-1),3], y = rv$data[rv$data[,1] == (i-1),2], col = p.color[i], pch = 16)
        # below does not look very good, lines are not wide enough. look at segments if we want to implement this idea. 
        # points(x = mean(rv$data[rv$data[,3] == i,3]), y = mean(rv$data[rv$data[,3] == i,2]), col = p.color[i], pch = "-")
      }
      legend("topright", bty = "n", legend = c("Group 1", "Group 2"), pch = 16, col = p.color)
    })
    output$boxplIND <- renderPlot({
      if(input$EffSize4 !='Manual'){
        boxplot(Outcome ~ Condition ,data = rv$data, names = c("Group 1", "Group 2"), 
                col = c("aliceblue", "mistyrose"), border = c("cadetblue4", "coral2"), ylim = c(-2,12), boxwex = 0.2)
      } else {
        boxplot(Outcome ~ Condition ,data = rv$data, names = c("Group 1", "Group 2"), 
                col = c("aliceblue", "mistyrose"), border = c("cadetblue4", "coral2"), boxwex = 0.2)
      }
      p.color2 <- c("cadetblue", "coral")
      for(i in 1:2){
        points(x = jitter(unclass(factor(rv$data[,1])[rv$data[,1] == unique(rv$data[,1])[i]]), amount = 0.03), 
               y = rv$data[rv$data[,1] == unique(rv$data[,1])[i],2], col = p.color2[i], pch = 1, lwd = 3)
      }
    })
    output$normplIND <- renderPlot({
      means <- tapply(rv$data[,2],rv$data[,1], mean)
      sds <- tapply(rv$data[,2],rv$data[,1], sd)
      if(input$EffSize4 !='Manual'){
        x <- seq(-5, 15, length = 100)
      } else {
        x <- seq(min(rv$data[,2])-5, max(rv$data[,2])+5, length = 200)
      }  
      hx <- cbind(dnorm(x, mean = means[1], sd = sds[1]),
                  dnorm(x, mean = means[2], sd = sds[2]))
      max.hx = max(hx)
      plot(x, hx[,1], col = "cadetblue4", ylab = "Density", type = "l", yaxt = "n", ylim = c(0,max.hx), lwd = 2, xlab = "Outcome")
      lines(x, hx[,2], col = "coral2", ylab = "Density", type = "l", lwd = 2)
      polygon(x, hx[,1], col = rgb(t(col2rgb("aliceblue")), maxColorValue = 255, alpha = 0.6*255), border = NA)
      polygon(x, hx[,2], col = rgb(t(col2rgb("mistyrose")), maxColorValue = 255, alpha = 0.4*255), border = NA)
    })
    
    output$aovSummary1IND <- renderTable(rownames = TRUE, na = "", {
      return(anova(lm(Outcome ~ Condition, data = rv$data)))
    })
    
    output$descriptives1IND <- renderTable(rownames = TRUE,{
      means <- round(tapply(rv$data[,2],rv$data[,1], mean),2)
      sds <-  round(tapply(rv$data[,2], rv$data[,1], sd),2)
      vars <-  round(tapply(rv$data[,2], rv$data[,1], var),2)
      ss <- as.vector(table(rv$data[,1]))
      descr <- as.data.frame(matrix(,ncol = 2, nrow = 4))
      colnames(descr) <- paste("Group", 1:2)
      rownames(descr) <- c("Mean", "Sd", "Variance", "Sample size")
      descr[1,] <- round(means,2)
      descr[2,] <- round(sds,2)
      descr[3,] <- round(vars,2)
      descr[4,] <- ss
      return(descr)
    })
    
    output$true.icc <- renderText({
      model_condition <- lmer(Outcome ~ Condition + (1 | ClusterID), data = rv$data, REML = FALSE)
      int.var <- as.data.frame(VarCorr(model_condition, comp="Variance"))[1,4]
      res.var <- as.data.frame(VarCorr(model_condition, comp="Variance"))[2,4]
      print(
        formatC(round(int.var/(int.var + res.var),2), format='f', digits=2 )
      )
    })
    output$p_val1IND <- renderText({
      paste("P-value ANOVA: ",
            formatC(numformat(round(summary(lm(Outcome ~ Condition, data = rv$data))[[4]][2,4], 3), digits = 3), format='f', digits=3 )
      )      
    })
    output$p_val2IND <- renderText({
      intercept_only <- lmer(Outcome ~ 1 + (1 | ClusterID), data = rv$data, REML = FALSE)
      model_condition <- lmer(Outcome ~ Condition + (1 | ClusterID), data = rv$data, REML = FALSE)
      paste("P-value multilevel analysis: ",
            formatC(numformat(round(anova(intercept_only,model_condition)[[8]][2],3), digits = 3), format='f', digits=3 )
      )
    })
  })  
  
      
}