#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ### ONE SAMPLE ###
  
  data <- reactive({
    
    print(input$h0_mean)
    
    if (is.na(input$h0_mean) | is.na(input$ha_mean)) {
      return(NULL)
    } else {
    
    # Retrieve the inputs
    n <- input$sample_size
    simulations <- input$simulations
    h0_mean <- input$h0_mean 
    ha_mean <- input$ha_mean
    sd <- input$sd
    alpha <- as.numeric(input$alpha)
    skew <- input$skewness
    
    # Take the samples
    if (skew == 'Right skewed') {
      means_null <- replicate(simulations, mean(rsnorm(n, mean=h0_mean, sd=sd, xi=20)))
      means_alternative <- replicate(simulations, mean(rsnorm(n, mean=ha_mean, sd=sd, xi=20)))
      
    } else if (skew == 'Left skewed') {
      means_null <- replicate(simulations, mean(rsnorm(n, mean=h0_mean, sd=sd, xi=-20)))
      means_alternative <- replicate(simulations, mean(rsnorm(n, mean=ha_mean, sd=sd, xi=-20)))
      
    } else if(skew == 'No skewed') {
      means_null <- replicate(simulations, mean(rnorm(n, mean=h0_mean, sd=sd)))
      means_alternative <-  replicate(simulations, mean(rnorm(n, mean=ha_mean, sd=sd)))
      
    }
    
    # Dataframe with the means
    df_means <- data.frame(
      means_null = means_null,
      means_alternative = means_alternative
    )
    
    upper_rejection_zone <- quantile(means_null, c(1-alpha/2))
    lower_rejection_zone <- quantile(means_null, alpha/2)
    
    power <- sum(means_alternative > upper_rejection_zone)/simulations + sum(means_alternative < lower_rejection_zone)/simulations
    
    beta <- 1-power
    
    values <- list(df_means, power, upper_rejection_zone, lower_rejection_zone)
    }
  })
  
  output$power <- renderPlot({
    
    if (is.na(input$h0_mean) | is.na(input$ha_mean)) {
      return(NULL)
    } else {
      # Retrieve the inputs
      n <- input$sample_size
      simulations <- input$simulations
      h0_mean <- input$h0_mean 
      ha_mean <- input$ha_mean
      sd <- input$sd
      alpha <- as.numeric(input$alpha)
      skew <- input$skewness
      
      df_means <- data()[[1]]
      upper_rejection_zone <- data()[[3]]
      lower_rejection_zone <- data()[[4]]
      
      # Density curve of the sampling distribution
      dat <- with(density(df_means$means_null, n = 1000), data.frame(x, y))
      dat2 <- with(density(df_means$means_alternative, n = 1000), data.frame(x, y))
      
      # Create the plot
      # if (h0_mean < ha_mean) {
        ggplot(dat = dat2, mapping = aes(x = x, y = y)) +
          geom_line(data = dat, mapping = aes(x = x, y = y,
                                              color = "Null distribution"), 
                    size = 1) +
          geom_line(mapping = aes(color = "Alternative distribution"),
                    size = 1) +
          geom_area(mapping = aes(x = ifelse(x>=upper_rejection_zone, x, upper_rejection_zone)),
                    fill = "coral", alpha = 0.3) +
          geom_area(mapping = aes(x = ifelse(x<lower_rejection_zone, x, lower_rejection_zone)),
                    fill = "coral", alpha = 0.3) +
          labs(x = "x",
               y = "density") +
          geom_vline(xintercept = c(upper_rejection_zone, lower_rejection_zone),
                     linetype = 2) +
          scale_y_continuous(limits = c(0, max(c(dat$y, dat2$y)))) +
          scale_color_manual('',values=c("coral", 'steelblue')) +
          labs(title = "Sampling distributions") + 
          theme_bw() +
          theme(legend.title = element_blank(),
                legend.position = c(0.9, 0.9),
                legend.background = element_rect(fill = "white", color = "black"))
        }
    
  })
  
  output$power_analysis <- renderTable(hover = TRUE, align = 'c', width = '100%',{
    
    if (is.na(input$h0_mean) | is.na(input$ha_mean)) {
      return(NULL)
    } else {
    
    power <- data()[[2]]
    
    power_df <- data.frame(Power = round(power, 2),
                           Type.II.error = round(1-power, 2))
    
    print(power_df)
    
    }
  })
  
  output$random_sample <- renderPlot({
    
    if (is.na(input$h0_mean) | is.na(input$ha_mean)) {
      return(NULL)
    } else {
    
    # Retrieve the inputs
    n <- input$sample_size
    simulations <- input$simulations
    h0_mean <- input$h0_mean 
    ha_mean <- input$ha_mean
    sd <- input$sd
    alpha <- as.numeric(input$alpha)
    skew <- input$skewness
    
    # Generate one random sample
    if (skew == 'Right skewed') {
      random_sample_df <- data.frame(sample = rsnorm(n, mean=h0_mean, sd=sd, xi=20))
    } else if (skew == 'Left skewed') {
      random_sample_df <- data.frame(sample = rsnorm(n, mean=h0_mean, sd=sd, xi=-20))
    } else if (skew == 'No skewed') {
      random_sample_df <- data.frame(sample = rnorm(n, mean=h0_mean, sd=sd))
    }
  
    ggplot(random_sample_df, aes(x=sample)) +
      geom_boxplot(color='darkgrey', fill='dodgerblue', alpha=0.4) +
      geom_jitter(aes(y=0), shape=16, size=2, color='green', width = 0, height = 0.1) +
      labs(x='X', title='Distribution of one random sample') +
      theme_bw() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    }
  })
  
  ### TWO SAMPLES ###
  
  data2 <- reactive({
    
    if (is.na(input$mean1) | is.na(input$mean2)) {
      return(NULL)
    } else {
      
      # Retrieve the inputs
      n1 <- input$sample_size1
      n2 <- input$sample_size2
      simulations <- input$simulations2
      mean1 <- input$mean1 
      mean2 <- input$mean2
      sd1 <- input$sd1
      sd2 <- input$sd2
      alpha <- as.numeric(input$alpha2)
      skew1 <- input$skewness1
      skew2 <- input$skewness2
      
      # https://stackoverflow.com/questions/62998486/simulation-rstudio
      # Define the skewness parameter xi
      if (skew1 == 'No skewed'){
        xi1 <- 1
      } else if (skew1 == 'Right skewed'){
        xi1 <- 20
      } else if (skew1 == 'Left skewed') {
        xi1 <- -20
      }
      
      if (skew2 == 'No skewed'){
        xi2 <- 1
      } else if (skew2 == 'Right skewed'){
        xi2 <- 20
      } else if (skew2 == 'Left skewed') {
        xi2 <- -20
      }
      
      # Take the samples
      diffs_null <- replicate(simulations, 
                              mean(rsnorm(n2, mean=mean1, sd=sd2, xi=xi2)) - mean(rsnorm(n1, mean=mean1, sd=sd1, xi=xi1)))
      diffs_alternative <- replicate(simulations, 
                                     mean(rsnorm(n2, mean=mean2, sd=sd2, xi=xi2)) - mean(rsnorm(n1, mean=mean1, sd=sd1, xi=xi1)))
      
      # Dataframe with the means
      df_diffs <- data.frame(
        diffs_null = diffs_null,
        diffs_alternative = diffs_alternative
      )
      
      upper_rejection_zone <- quantile(diffs_null, c(1-alpha/2))
      lower_rejection_zone <- quantile(diffs_null, alpha/2)
      
      power <- sum(diffs_alternative > upper_rejection_zone)/simulations + sum(diffs_alternative < lower_rejection_zone)/simulations
      
      beta <- 1-power
      
      values <- list(df_diffs, power, upper_rejection_zone, lower_rejection_zone)
    }
  })
  
  output$power2 <- renderPlot({
    
    if (is.na(input$mean1) | is.na(input$mean2)) {
      print("bla")
      return(NULL)
    } else {
      # Retrieve the inputs
      n1 <- input$sample_size1
      n2 <- input$sample_size2
      simulations <- input$simulations2
      mean1 <- input$mean1 
      mean2 <- input$mean2
      sd1 <- input$sd1
      alpha <- as.numeric(input$alpha2)
      skew1 <- input$skewness1
      skew2 <- input$skewness2
      
      # https://stackoverflow.com/questions/62998486/simulation-rstudio
      # Define the skewness parameter xi
      if (skew1 == 'No skewed'){
        xi1 <- 1
      } else if (skew1 == 'Right skewed'){
        xi1 <- 20
      } else if (skew1 == 'Left skewed') {
        xi1 <- -20
      }
      
      if (skew2 == 'No skewed'){
        xi2 <- 1
      } else if (skew2 == 'Right skewed'){
        xi2 <- 20
      } else if (skew2 == 'Left skewed') {
        xi2 <- -20
      }
      
      print(xi1)
      print(xi2)
      
      df_diffs <- data2()[[1]]
      upper_rejection_zone <- data2()[[3]]
      lower_rejection_zone <- data2()[[4]]
      
      # Density curve of the sampling distribution
      dat <- with(density(df_diffs$diffs_null, n = 1000), data.frame(x, y))
      dat2 <- with(density(df_diffs$diffs_alternative, n = 1000), data.frame(x, y))
      
      # Create the plot
      # if (mean1 <= mean2) {
        ggplot(dat = dat2, mapping = aes(x = x, y = y)) +
          geom_line(data = dat, mapping = aes(x = x, y = y,
                                              color = "Null distribution"), 
                    size = 1) +
          geom_line(mapping = aes(color = "Alternative distribution"),
                    size = 1) +
          geom_area(mapping = aes(x = ifelse(x>upper_rejection_zone, x, upper_rejection_zone)),
                    fill = "coral", alpha = 0.3) +
          geom_area(mapping = aes(x = ifelse(x<lower_rejection_zone, x, lower_rejection_zone)),
                    fill = "coral", alpha = 0.3) +
          labs(x = "x",
               y = "density") +
          geom_vline(xintercept = c(upper_rejection_zone, lower_rejection_zone),
                     linetype = 2) +
          scale_y_continuous(limits = c(0, max(c(dat$y, dat2$y)))) +
          scale_x_continuous(limits = c(min(c(dat$x, dat2$x)), max(c(dat$x, dat2$x)))) +
          scale_color_manual('',values=c("coral", 'steelblue')) +
          labs(title = "Sampling distributions") +
          theme_bw() +
          theme(legend.title = element_blank(),
                legend.position = c(0.9, 0.9),
                legend.background = element_rect(fill = "white", color = "black"))
      
    }
    
  })
  
  output$power_analysis2 <- renderTable(hover = TRUE, align = 'c', width = '100%',{
    
    if (is.na(input$mean1) | is.na(input$mean2)) {
      return(NULL)
    } else {
      
      power <- data2()[[2]]
      
      power_df <- data.frame(Power = round(power, 2),
                             Type.II.error = round(1-power, 2))
      
      print(power_df)
      
    }
  })
  
  output$random_sample2 <- renderPlot({
    
    if (is.na(input$mean1) | is.na(input$mean2)) {
      return(NULL)
    } else {
      
      if (is.na(input$mean1) | is.na(input$mean2)) {
        return(NULL)
      } else {
        # Retrieve the inputs
        n1 <- input$sample_size1
        n2 <- input$sample_size2
        simulations <- input$simulations2
        mean1 <- input$mean1 
        mean2 <- input$mean2
        sd1 <- input$sd1
        alpha <- as.numeric(input$alpha2)
        skew1 <- input$skewness1
        skew2 <- input$skewness2
        
        # https://stackoverflow.com/questions/62998486/simulation-rstudio
        # Define the skewness parameter xi
        if (skew1 == 'No skewed'){
          xi1 <- 1
        } else if (skew1 == 'Right skewed'){
          xi1 <- 20
        } else if (skew1 == 'Left skewed') {
          xi1 <- -20
        }
        
        if (skew2 == 'No skewed'){
          xi2 <- 1
        } else if (skew2 == 'Right skewed'){
          xi2 <- 20
        } else if (skew2 == 'Left skewed') {
          xi2 <- -20
        }
      
      # Generate one random sample
      random_sample_df <- data.frame(sample = rsnorm(n1, mean=mean1, sd=sd1, xi=xi1))
      
      ggplot(random_sample_df, aes(x=sample)) +
        geom_boxplot(color='darkgrey', fill='dodgerblue', alpha=0.4) +
        geom_jitter(aes(y=0), shape=16, size=2, color='green', width = 0, height = 0.1) +
        labs(x='X', title='Distribution of one random sample from group 1') +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      
      }
    }
  })
  
  output$random_sample22 <- renderPlot({
    
    if (is.na(input$mean1) | is.na(input$mean2)) {
      return(NULL)
    } else {
      
      if (is.na(input$mean1) | is.na(input$mean2)) {
        return(NULL)
      } else {
        # Retrieve the inputs
        n1 <- input$sample_size1
        n2 <- input$sample_size2
        simulations <- input$simulations2
        mean1 <- input$mean1 
        mean2 <- input$mean2
        sd1 <- input$sd1
        alpha <- as.numeric(input$alpha2)
        skew1 <- input$skewness1
        skew2 <- input$skewness2
        
        # https://stackoverflow.com/questions/62998486/simulation-rstudio
        # Define the skewness parameter xi
        if (skew1 == 'No skewed'){
          xi1 <- 1
        } else if (skew1 == 'Right skewed'){
          xi1 <- 20
        } else if (skew1 == 'Left skewed') {
          xi1 <- -20
        }
        
        if (skew2 == 'No skewed'){
          xi2 <- 1
        } else if (skew2 == 'Right skewed'){
          xi2 <- 20
        } else if (skew2 == 'Left skewed') {
          xi2 <- -20
        }
        
        # Generate one random sample
        random_sample_df <- data.frame(sample = rsnorm(n2, mean=mean2, sd=sd1, xi=xi2))
        
        ggplot(random_sample_df, aes(x=sample)) +
          geom_boxplot(color='darkgrey', fill='dodgerblue', alpha=0.4) +
          geom_jitter(aes(y=0), shape=16, size=2, color='green', width = 0, height = 0.1) +
          labs(x='X', title='Distribution of one random sample from group 2') +
          theme_bw() +
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        
      }
    }
      })
  

})
