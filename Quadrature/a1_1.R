library(shiny)
library(ggplot2)
library(statmod)
library(tidyr)
f<-function(x){
  exp(-x**2)
}
z<-1/2*sqrt(pi)
fc<-function(x){
  f(x/(1-x**2))*(1+x**2)/(1-x**2)**2
}

reimann<-function(n,e){
  w=1/n
  qua=0
  for (i in 1:n){
    t<-1/n*(i-1+e)
    qua<-qua+fc(t)*w
  }
  qua
}
#********************SHINY*************
ui <- fluidPage(
  titlePanel("Quadrature by Different Method"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num","Number of Nodes:",min=1,max=1000,value=5),
      sliderInput("e","Node Position:",min=0,max=1,step=0.1,value=0.5),
      selectInput("poly","Polynomial Function:",
                   choices = list("legendre"="legendre","jacobi"="jacobi"),
                   selected = 1)
    ),
    mainPanel(
    plotOutput("error")
    )
  )
)

server <- function(input, output){

  output$error<-renderPlot({
    num<-input$num
    lst1<-rep(0,num)
    lst2<-rep(0,num)
    lst3<-rep(0,num)
    
    
    for (n in 1:num){
      #left 
      
      lst1[n]<-reimann(n,0)
      
      #mid
      e<-input$e
      lst2[n]<-reimann(n,e)
      
      #gaussian
      out<-gauss.quad(n,kind=input$poly)
      nodes<-out$nodes
      weights<-out$weights
      qua3<-0
      qua3<-sum(fc(nodes)*weights)/2
      lst3[n]<-qua3
    } 
    err1<-abs(lst1-z)
    err2<-abs(lst2-z)
    err3<-abs(lst3-z)
    m<-cbind(1:length(err1),err1,err2,err3)
    daf<-data.frame(m)
    colnames(daf)<-c("N","Reimann","ModiReimann","Gaussian")
    tidy<-gather(daf,key=Method,value=Error,-N)
    
    ggplot(tidy,aes(x=N,y=Error,col=Method))+geom_line()
    
  })
  
}
shinyApp(ui = ui, server = server)