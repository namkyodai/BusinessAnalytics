library(shiny)
library(ggplot2)



# Define UI ----
ui <- fluidPage(
  titlePanel("A Simple Return on Investment Analysis for Pumps"),
  
  helpText("Author: ", a("Nam Le (PhD)",  href="http://4analyst.wordpress.com"), "- Email address: ", a("namlt@protonmail.com",  href="namlt@protonmail.com")),
  
  helpText("This app is used to demonstrate the benefits of replacing or rehabilatating a pump once the pump experiences more frequent random failure and the asset managers do not have a rich set of failure data to determinte optimal intervention strategy using reliability model (Click", a("HERE to refer to an Example)", href="https://4analyst.shinyapps.io/aWeiBullLcc/")),
  
 helpText("Nam et al. Reports of the Plant Audit for various Pump Stations and Reservoirs - Maynilad Water Services Inc. (2019) "), 
 
  sidebarLayout(
    sidebarPanel(
    
    #  helpText("We start with selecting parameter values of the model"),
      
      # Input: Specify the number of observations to view ----
    #  numericInput("obs", "Number of observations to view:", 10, width="40%"),
    
    h3("Inputs"),
      
      sliderInput("duration", 
                  label = "Investigated Duration (Years)",
                  min = 0, max = 50, value = 10, step=1),#,
  
  
    
    sliderInput("rho", 
                label = "Discount Factor (Interest Rate)",
                min = 0, max = 20, value = 8.5, step=0.5),#,
    
    
    sliderInput("pricewater", 
                label = "Price of 1 cubic meter of water (Peso)",
                min = 5, max = 15, value = 6.8, step=0.1),#,
    

    sliderInput("pricewater", 
                label = "Price of 1 cubic meter of water (Peso)",
                min = 5, max = 15, value = 6.8, step=0.1),#,
   
    sliderInput("horsepower", 
                label = "Pump capacity in horse power (HP)",
                min = 10, max = 600, value = 300, step=1),#,
    
    sliderInput("utilization", 
                label = "Utilization level of pump (%)",
                min = 1, max = 100, value = 98, step=1),#, 
    
    sliderInput("enew", 
                label = "Annual decrease in efficiency (%) of new pump",
                min = 0, max = 5, value = 0.5, step=0.1),#, 
   
    sliderInput("eold", 
                label = "Annual decrease in efficiency (%) of old pump",
                min = 0, max = 5, value = 1, step=0.1),#, 
    
    sliderInput("capexinvestment", 
                label = "Cost to install new pump (Peso)",
                min = 10000, max = 5000000, value = 2000000, step=100000),#, 
    
 #   helpText("Inputs for Determination of Optimal Time Windows of Preventive Intervention."),
    
  #  sliderInput("cipiratio", "Ratio CI/PI:",  
                min = 1, max = 20, value = 5, step=0.5),
 
 #sliderInput("interest", "Interest (Discount for NPV):",  
   #          min = 0, max = 20, value = 8.5, step=0.5)
   #     ),
  


#This is the main panel
    mainPanel(
  
    h3("Outputs"),
    # Output: Header + summary of distribution ----
    
 #   h4("Yearly Cost Distribution"),
#    plotOutput('roiplot', height = 300, width = 500 ),

    h4("check"),
   
    verbatimTextOutput("check")
    )


)
)


