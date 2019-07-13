# Define server logic ----
server <- function(input, output) {
  
  

  
  
  output$check <- renderPrint({

    Time <-input$duration
    enew<-matrix(nrow=Time) 
    eold<-matrix(nrow=Time) 
    capex<-matrix(nrow=Time) 
    opex<-matrix(nrow=Time) 
    u<-matrix(nrow=Time) 
    delta<-matrix(nrow=Time) 
    IS1<-matrix(nrow=Time)
    
    
    
    
    for (t in 1: Time){
      if (t<2){
          eold[t]<-input$eold
        } else {
           eold[t]<-eold[t-1]*input$eold
        }
      
      delta[]<-0
      delta[1]<-1
      enew[1]<-input$enew
      capex[t]<-input$capexinvestment
      u[t]<-input$utilization
      opex[t]<- input$pricewater*input$horsepower*365*24*0.746*u[t] #peso
      rho<-input$rho
      
      
        if (delta[t]==1){
          enew[t]<-1
        } else if (t==1){
          enew[t]<-1
        } else if (t>1){
          enew[t]<-enew[t-1]*input$enew
        }
      
      
      enew[is.na(enew)] <- 0
      
        if (enew[t]<1){
          eold[t]<-eold[t]
        } else if (enew[t]==1){
          eold[t]<-0
        }
      
      IS1[t]<-(((capex[t]*delta[t]+opex[t]*enew[t]))+(1-delta[t])*opex[t]*eold[t])/(1+rho)**t
      
    }
    

    
    
    IS1
    
    


  })
  
  
 
}



