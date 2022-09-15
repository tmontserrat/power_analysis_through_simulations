# Libraries required
library(shiny)
library(ggplot2)
library(pracma)
library(fGarch)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("paper"),
  
  # Application title
  titlePanel("Power analysis through simulations"),
  tabsetPanel(type="pills",
    tabPanel(title="One sample",
         # Sidebar with sliders input for different parameters 
         sidebarLayout(
           # Show a plot
           mainPanel(
             br(),
             plotOutput("power"),
             tableOutput("power_analysis"),
             plotOutput("random_sample")
             
           ),
           sidebarPanel(
             numericInput(inputId="sample_size",
                          label="Sample size",
                          min=2,
                          max=1000,
                          value=45),
             sliderInput(inputId="simulations",
                          label="Number of simulations",
                          min=2,
                          max=10000,
                          value=5000),
             numericInput(inputId="h0_mean",
                          label="Null hypothesis mean",
                          value=0),
             numericInput(inputId="ha_mean",
                          label="Alternative hypothesis mean",
                          value=2),
             sliderInput(inputId="sd",
                         label="Standard deviation",
                         min=0,
                         max=10,
                         value=5),
             radioButtons(inputId="alpha",
                          label="Alpha",
                          choices=c(0.01, 0.05, 0.1),
                          selected="0.05"),
             selectInput(inputId="skewness",
                         label="Select the population distribution skewness",
                         choices=c("Left skewed", "Right skewed", "No skewed"),
                         selected="No skewed")
           ),
           
           
         )
         ),
    tabPanel(title="Two samples",
             # Sidebar with sliders input for different parameters 
             sidebarLayout(
               # Show a plot
               mainPanel(
                 br(),
                 plotOutput("power2"),
                 tableOutput("power_analysis2"),
                 plotOutput("random_sample2"),
                 plotOutput("random_sample22")
                 
               ),
               sidebarPanel(
                 numericInput(inputId="sample_size1",
                             label="Sample 1 size",
                             min=2,
                             max=1000,
                             value=45),
                 numericInput(inputId="sample_size2",
                             label="Sample 2 size",
                             min=2,
                             max=1000,
                             value=45),
                 sliderInput(inputId="simulations2",
                             label="Number of simulations",
                             min=2,
                             max=10000,
                             value=5000),
                 numericInput(inputId="mean1",
                              label="Population 1 mean",
                              value=5),
                 numericInput(inputId="mean2",
                              label="Population 2 mean",
                              value=7),
                 sliderInput(inputId="sd1",
                             label="Population 1 standard deviation",
                             min=0,
                             max=10,
                             value=5),
                 sliderInput(inputId="sd2",
                             label="Population 2 standard deviation",
                             min=0,
                             max=10,
                             value=5),
                 radioButtons(inputId="alpha2",
                              label="Alpha",
                              choices=c(0.01, 0.05, 0.1),
                              selected="0.05"),
                 selectInput(inputId="skewness1",
                             label="Select the population 1 distribution skewness",
                             choices=c("Left skewed", "Right skewed", "No skewed"),
                             selected="No skewed"),
                 selectInput(inputId="skewness2",
                             label="Select the population 1 distribution skewness",
                             choices=c("Left skewed", "Right skewed", "No skewed"),
                             selected="No skewed")
               ),
               
               
             ))
    ),

  hr(),
  div(
    includeHTML("footer.html")
  )
  
)




