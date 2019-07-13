# Define server logic ----
server <- function(input, output) {
  
  #define the object
  
#  d<-reactive({
  
  #dist <- reactive(sample(input$range[1]:input$range[2], input$obs, replace=FALSE))
  
  d<-reactive(    
    #if (input$generateBt > 0 ) 
    #isolate(
      floor(runif(input$obs, min = input$range[1], max = input$range[2]))
    )
  #)
  
  l=reactive(length(d()))
  
  
  #-------Weibull function
  median_percentile_ranks<-reactive(getPPP(d()))
  #print(median_percentile_ranks)#[,2]
  MLEfit<-reactive(mlefit(mleframe(d())))
  MLE_Unbiased<-reactive(c(MLEfit()[1],MLEfit()[2]*hrbu(l())))
  
  eta<-reactive(MLE_Unbiased()[[1]])
  beta<-reactive(MLE_Unbiased()[[2]])
  
  da1 <- reactive(data.frame(
    serial=as.character(c(1:l())),
    time=c(d()),
    event=rep(1,l())))
  
  da2 <- reactive(wblr(da1(), col="red"))

  da3 <- reactive(wblr.fit(da2(), col="darkgreen"))
  
  da4 <- reactive(wblr.conf(da3(), col="blue"))
  
  #----------------
  
  

  
  
  
  output$result <- renderPrint({ 
  d()
  })
  
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- d()
    summary(dataset)
  })
  
### display histograph
  
  output$distPlot <- renderPlot({
    dataset <- d()
    barplot(dataset,col="turquoise3",ylim=c(0,10000))
  })
 
 
 
### Display the estimation for alpha and eta

  output$weibullplot <- renderPlot({
    plot(da4())
  })
  
  
## Print value of Alpha and eta
  
  output$weibullvalue <- renderPrint({ 
    c(da4()$fit[[1]]$beta,da4()$fit[[1]]$eta)
  })
  
## Reliability curve
  
  output$weibullreli <- renderPlot({
    eta<-da4()$fit[[1]]$eta
    beta<-da4()$fit[[1]]$beta
    survi=function(t,eta,beta){exp(-(1/eta*t)^beta)}
    breakpoint=10
    plot(survi(c(0:10000),eta,beta),pch=14,axes=TRUE,ylim=c(0,1),ylab="Reliability", xlab="Time (hours)",col="red",type="l",lwd=2,cex=1)
  #  ggplot(x = data.frame(c(0, 10000)), aes(x = x)) 
 #      + stat_function(fun = survi)
    
  })
  


  
  eta<-reactive(da4()$fit[[1]]$eta)
  beta<-reactive(da4()$fit[[1]]$beta)
  
  rho<-reactive(input$interest/(100*365*24))
  ci<-reactive(input$cipiratio)
  
  #--survival function
  survi=function(t,eta,beta){exp(-(1/eta*t)^beta)} #this is survival function
  
  #--probability density function
  
  f=function(t,eta,beta){beta/eta*(t/eta)^(beta-1)*exp(-(t/eta)^beta)} #probability of failure
  
  #--First part of the cost function
  f_cost=function(t,eta,beta,rho){
    ((beta/eta)*(t/eta)^(beta-1)*exp(-(t/eta)^beta))*exp(-rho*t)
  }
  
  #FF<-matrix(nrow=10000)
  FF<-matrix(nrow=10000)
  F_cost<-matrix(nrow=10000)
  S<-matrix(nrow=10000)
  E<-matrix(nrow=10000)
  omega<-matrix(nrow=10000)
  
  
#  output$Check01 <- renderPrint({ 
#     for (t in 1: 10000){
#      F_cost[t]=integrate(f_cost,0,t,eta(),beta(),rho())$val
#      FF[t]<- 1-exp(-(1/eta()*t)^beta()) 
#      S[t]=exp(-(1/eta()*t)^beta())
#      E[t]=integrate(survi,0,t,eta(),beta())$val
#      omega[t]=(F_cost[t]+S[t]*(1/(input$cipiratio))*exp(-rho()*t))/E[t]
#    }
#    which.min(omega)
#   })
  
  output$lccplot <- renderPlot({

    for (t in 1: 10000){
      F_cost[t]=integrate(f_cost,0,t,eta(),beta(),rho())$val
      FF[t]<- 1-exp(-(1/eta()*t)^beta()) 
      S[t]=exp(-(1/eta()*t)^beta())
      E[t]=integrate(survi,0,t,eta(),beta())$val
      omega[t]=(F_cost[t]+S[t]*(1/(input$cipiratio))*exp(-rho()*t))/E[t]
    }
    unit=min(omega)*3
    
    #which.min(omega)
   # plot(omega,type="l",lwd=2,col="red",ylab="",xlab="",xlim=c(0,10000),ylim=c(0,unit),axes=TRUE,lty=1)
    
    p=ggplot(data.frame(omega), aes(x=seq_along(omega),y=omega)) + 
      geom_line(colour="#009999", lwd=1) #(stat="identity")
    p+scale_y_continuous(limits = c(0, unit))+xlab("Time (hours)")+ ylab("Impacts")+geom_point(aes(x=which.min(omega), y=min(omega)), colour="red",lwd=3)+geom_segment(aes(x = which.min(omega), y = min(omega), xend = which.min(omega), yend = 0),col= 'darkviolet',lty=1,lwd=1)+geom_segment(aes(x = 0, y = min(omega), xend = which.min(omega), yend = min(omega)),col= 'darkviolet',lty=1,lwd=1)+geom_text(x=which.min(omega)*0.9, y=min(omega)*0.8, label=paste("(", format(which.min(omega), 2),",", format(round(min(omega), 5), nsmall = 2),")"))
    
  })
  
  
}



