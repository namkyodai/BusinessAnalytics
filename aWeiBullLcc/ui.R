library(shiny)
library(ggplot2)
library(WeibullR)

survi=function(t,eta,beta){exp(-(1/eta*t)^beta)} #this is survival function

breakpoint=10


# Define UI ----
ui <- fluidPage(
  titlePanel("Reliability and Optimal Preventive Intervention - A Weibull case"),
  
  helpText("Author: ", a("Nam Le (PhD)",  href="http://4analyst.wordpress.com"), "- Email address: ", a("namlt@protonmail.com",  href="namlt@protonmail.com")),
  
  helpText("This app can be extended for reliability estimation and determination of optimal intervention strategy in various real life cases (e.g. determination of preventive maintenance program of water and wastewater ultilities, facilities of power stations and industrial plants). "),
  
  
  helpText("References:"),  
  
  helpText(a("Naoto Kaio, Shunji Osaki, 'Extended block replacement models', AFCET 1984",  href="https://doi.org/10.1051/ro/1984180100591")),  
  
 # helpText(a("Nam Le, 'Stochastic Optimization Methods for Infrastructure Management with Incomplete Monitoring', PhD Dissertation, Kyoto University, (2009)")),  
  
 helpText("Nam et al. Reports of the Plant Audit for various Pump Stations and Reservoirs - Maynilad Water Services Inc. (2019) "), 
 
  sidebarLayout(
    sidebarPanel(
    
      helpText("Inputs to estimate Reliability"),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 10, width="40%"),
    
      
      
      sliderInput("range", 
                  label = "Range of duration between failure",
                  min = 0, max = 10000, value = c(3000,6000), step=200),#,
  
    #  actionButton("generateBt", "Generate Numbers")
      
  #  helpText("Weibull distribution"),
    
 #   withMathJax(),

    helpText("Inputs for Determination of Optimal Time Windows of Preventive Intervention."),
    
    sliderInput("cipiratio", "Ratio CI/PI:",  
                min = 1, max = 20, value = 5, step=0.5),
 
 sliderInput("interest", "Interest (Discount for NPV):",  
             min = 0, max = 20, value = 8.5, step=0.5)
        ),
  



    
    
    
    
    
    
#This is the main panel
    mainPanel(
      
    helpText("Display the data in tabular format and graphically."),
    # Output: Header + summary of distribution ----
    
    # Output: Header + table of distribution ----
    h4("Observations"),
    
    
    verbatimTextOutput("result"),
    h4("Summary"),
    
    verbatimTextOutput("summary"),
    
    h4("Graph"),
    plotOutput('distPlot', height = 300, width = 500 ),

    h4("Estimation of Weibull Parameters"),
    
    helpText("Estimation of model's parameters can be done with Maximum Likelihood Estimation (MLE) approach. The below graph represents the fitted linear line to the failure data."),
    
    plotOutput('weibullplot', height = 300, width = 500 ),
    
    h5("Value of Weibull's parameters"),
    verbatimTextOutput("weibullvalue"),
    
    h4("Reliability curve"),
    
    plotOutput('weibullreli', height = 300, width = 500 ),
    
    helpText("The red curve is the reliability curve. Reliability of an asset will gradually decrease over time"),
    
    h4("Life Cycle Costing"),
    
    plotOutput('lccplot', height = 300, width = 500 ),
    
    helpText("The curve represents Net Present Value (NPV) of the total cost if a Preventive Intervention is executed at any arbitrary time along the horizontal axis. It is clear from the graph that there will be a point, at which, the total impact is minimum. This minimum point corresponds to an optimal time window to execute Preventive Intervention.")
    
    )

    

)
)


