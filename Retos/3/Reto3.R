#Felipe becerra 
#Julian Rizo

library(shiny)
library (shinydashboard) 
library(shinyjs)
library(PolynomF)
library(deSolve)
library(phaseR)
#datos 
kasp<-c(2481667,2402646,2442653,2584573,2501995,2747072,2740103,
        10805670,5817167,6384624,15171063,14331878,11909716,11759059,
        14595563,14626680,14530705,14530705,11347762,19363886,18961818,
        18340856,17038822,20291509,18754639,18203043,19297302,18655692,
        24020341,20930352,20779337,20243631,16447359,11256883,10329849,13970758)

dias<-seq(1, length(kasp), by = 1)
total<-c(kasp[1])
for(i in 1:(length(kasp)-1)){
  total <- c(total, total[i] + kasp[i+1])
}
total <- total/1000000

p <- 0
pr <- 0
cont <- 0
for(i in 1:(length(total)-1)){
  p <- (total[i+1]-total[i]) + p
  cont <- cont + 1
}
p<- p/cont
p <- p/100
p 

ui <- dashboardPage(
  dashboardHeader (), 
  dashboardSidebar (), 
  dashboardBody () 
)
header <- dashboardHeader(title = "Aplicacion de las Ecuaciones diferenciales")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("SI", tabName = "SI", icon = icon("caret-right")),
    menuItem("SIR", tabName = "SIR", icon = icon("caret-right") )
  )
  
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

TabSI <- fluidRow(
  
  box(
    title = "Modelo SI.", 
    width = 8,
    shinyjs::useShinyjs(),
    
    sliderInput("infectadosSI", "Infectados:", 1, 1000, 14, step = 1),
    
    sliderInput("suceptiblesSI", "Suceptibles:", 12, 1000, 600, step = 1),
    
    sliderInput("diasSI", "Dias:", 30, 50, 35, step = 1),

    sliderInput("bethaSI", "Beta:", 0.1000, 0.2000, round(p, 4), step = 0.0001),
    actionButton("botonCalcularSI", "Procesar")
  ),
  tabBox(
    title = "Metodos",
    width = 10,
    title = "RK4",
    width = 360,
    plotOutput("plot1SI", height = 380),
    plotOutput("plot1ErrorSI", height = 360, width = 360),
    dataTableOutput("tablaErrorR")
    ,
    title = "Adams",
    width = 360,
    plotOutput("plot2SI", height = 380),
    plotOutput("plot2ErrorSI", height = 360, width = 360),
    dataTableOutput("tablaErrorA")
    
  )
  
  
)



TabSIR <- fluidPage(
  box(
    title = "Modelo SIR.", 
    width = 10,
    shinyjs::useShinyjs(),
    
    sliderInput("infectadosSIR", "Infectados:", 1, 1000, 14, step = 10),
    
    #suceptibles es numero total de usuarios del antivirus kaspersky
    sliderInput("suceptiblesSIR", "Suceptibles:", 12, 1000, 600, step = 10),
    
    sliderInput("diasSIR", "Dias:", 30, 50, 35, step = 1),
    
    sliderInput("bethaSIR", "Beta:", 0.1000, 0.2000, 0.1351, step = 0.0001),
    
    sliderInput("gammaSIR", "Gamma:", 0.2500, 0.3500, 0.2800, step = 0.0001),
    actionButton("botonCalcularSIR", "Calcular")
  ),
  tabBox(
    title = "Metodos",
    width = 8,
    tabPanel("RK4",
             width = 400,
             plotOutput("plot1SIR", height = 380),
             plotOutput("plot1ErrorSIR", height = 400, width = 400),
             dataTableOutput("tablaErrorR2")
    ),
    tabPanel("Adams",
             width = 400,
             plotOutput("plot2SIR", height = 380),
             plotOutput("plot2ErrorSIR", height = 400, width = 400),
             dataTableOutput("tablaErrorA2")
    )
  )
)


body <- dashboardBody(
  tabItems(
    tabItem("SI",frow1, TabSI),
    tabItem("SIR",TabSIR)
  )
  
)

ui <- fluidPage(title = 'Reto 3', header, sidebar, body, theme = "bootstrap.css")

# Define server logic to show current time, update every second ----
server <- function(input, output, session) {
  CalcularSI <- function(){
    
    
    
    output$plot1SI <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          #dR <-                 gamma * I
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
      out <- as.data.frame(out*N) 
      attach(out)
      
      plot(dias, total, pch = 19, ylim = c(0, 800), xlim = c(0, input$diasSI),cex=1, col = "pink")
      splineAjuste=splinefun(dias, total) # frontera natural
      curve(splineAjuste,add=T,lty=1,lwd=2, ylim = c(0, 800), xlim = c(0, input$diasSI), col= "yellow")
      par(new=T)
      plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Infectados",main = "Runge Kutta 4")
      par(new=T)
      plot(out$time, out$I ,col="pink", type="l", xlab="Dias", ylab="Infectados", ylim = c(0,800))
      legend(x = "topright", legend=c("Susceptibles", "Infectados aproximado", "infectados real"), col=c("black", "pink", "yellow"), lty=rep(1, 2)) 
    })
    
    output$plot1ErrorSI <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
      out <- as.data.frame(out*N) 
      attach(out)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSI+1, by = 1)){
        error <- c(error, (abs(total[i] - out$I[i]))/total[i])
      }
      plot(times, error, col = "black", lwd = "2",type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
    })
    
    output$tablaErrorR <- renderDataTable({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
      out <- as.data.frame(out*N)
      attach(out)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSI+1, by = 1)){
        error <- c(error, (abs(total[i] - out$I[i]))/total[i])
      }
      
      valores <- data.frame(
        "Dia" = (1:(input$diasSI + 1 )),
        "Real" = round(total, 4),
        "Aproximado" = round(out$I, 4),
        "Error" = round(error,4)
      )
    })
    
    

    output$plot2SI <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
      out <- as.data.frame(out*N) 
      attach(out)
      plot(dias, total, pch = 19, ylim = c(0, 800), xlim = c(0, input$diasSI),cex=1, col = "pink")
      splineAjuste=splinefun(dias, total) 
      curve(splineAjuste,add=T,lty=1,lwd=2, ylim = c(0, 800), xlim = c(0, input$diasSI), col= "yellow")
      par(new=T)
      plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Infectados",main = "Adams")
      par(new=T)
      plot(out$time, out$I ,col="pink", type="l", xlab="Dias", ylab="Infectados", ylim = c(0,800))
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("black", "pink"), lty=rep(1, 2)) 
    })
    
    
    
    output$plot2ErrorSI <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
      out <- as.data.frame(out*N)
      attach(out)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSI+1, by = 1)){
        error <- c(error, (abs(total[i] - out$I[i]))/total[i])
      }
      plot(times, error, col = "black", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
    })
    
    output$tablaErrorA <- renderDataTable({
      N = 1
      init <- c(S = input$suceptiblesSI,
                I = input$infectadosSI)
      param <- c(beta = input$bethaSI,
                 gamma = input$gammaSI)
      
      si <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I / (S + I)
          dI <-  beta * S * I / (S + I)
          return(list(c(dS, dI)))
        })
      }
      times <- seq(0,input$diasSI, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
      out <- as.data.frame(out*N)
      attach(out)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSI+1, by = 1)){
        error <- c(error, (abs(total[i] - out$I[i]))/total[i])
      }
      
      valores <- data.frame(
        "Dia" = (1:(input$diasSI + 1 )),
        "Real" = round(total, 4),
        "Aproximado" = round(out$I, 4),
        "Error" = round(error,4)
      )
    })
    
  }
  
  
  CalcularSIR <- function(){
    

    output$plot1SIR <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR/100,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
      out <- as.data.frame(out*N) 
      attach(out)
      
      plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Infectados",main = "Runge Kutta 4")
      par(new=T)
      plot(out$time, out$I ,col="pink", type="l", xlab="Dias", ylab="Infectados", ylim = c(0,800))
      par(new=T)
      plot(out$time, out$R ,type = "l", col="yellow", xlab="Dias", ylab="Infectados", ylim = c(0, 800))
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("black", "pink", "yellow"), lty=rep(1, 2))
      
    })                                                                                    
    
    
    
    output$plot1ErrorSIR <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
   
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
      out2 <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
      out <- as.data.frame(out*N) 
      out2 <- as.data.frame(out2*N)
      attach(out)
      attach(out2)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSIR+1, by = 1)){
        error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])
      }
      plot(times, error, col = "purple", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
    })
    
    output$tablaErrorR2 <- renderDataTable({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
      out2 <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
      out <- as.data.frame(out*N) 
      out2 <- as.data.frame(out2*N)
      attach(out)
      attach(out2)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSIR+1, by = 1)){
        error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])
      }
      
      valores <- data.frame(
        "Dia" = (1:(input$diasSIR + 1 )),
        "Real" = round(out$I, 4),
        "Aproximado" = round(out2$I, 4),
        "Error" = round(error,4)
      )
    })
    

    output$plot2SIR <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR/100,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
      out <- as.data.frame(out*N) 
      attach(out)
      
      plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Infectados",main = "Adams")
      par(new=T)
      plot(out$time, out$I ,col="pink", type="l", xlab="Dias", ylab="Infectados", ylim = c(0,800))
      par(new=T)
      plot(out$time, out$R ,type = "l", col="yellow", xlab="Dias", ylab="Infectados", ylim = c(0, 800))
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("black", "pink", "yellow"), lty=rep(1, 2))
      
    })
    
    
    
    output$plot2ErrorSIR <- renderPlot({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
      out <- as.data.frame(out*N) 
      attach(out)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSIR+1, by = 1)){
        error <- c(error, (abs(total[i] - out$I[i]))/total[i])
      }
      plot(times, error, col = "purple", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
    })
    
    output$tablaErrorA2 <- renderDataTable({
      N = 1
      init <- c(S = input$suceptiblesSIR,
                I = input$infectadosSIR,
                R = 0)
      param <- c(beta = input$bethaSIR,
                 gamma = input$gammaSIR)
      
      sir <- function(times, init, param) {
        with(as.list(c(init, param)), {
          dS <- -beta * S * I 
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          return(list(c(dS, dI, dR)))
        })
      }
      times <- seq(0,input$diasSIR, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
      out2 <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
      out <- as.data.frame(out*N) 
      out2 <- as.data.frame(out2*N)
      attach(out)
      attach(out2)
      
      error <- c()
      i <- 1
      for(i in seq(1,input$diasSIR+1, by = 1)){
        error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])
      }
      
      valores <- data.frame(
        "Dia" = (1:(input$diasSIR + 1 )),
        "Real" = round(out$I, 4),
        "Aproximado" = round(out2$I, 4),
        "Error" = round(error,4)
      )
    })
  }
  
  
  observeEvent(input$botonCalcularSI, {
    CalcularSI()
  })
  
  observeEvent(input$botonCalcularSIR, {
    CalcularSIR()
  })
}


# Create Shiny app ----
shinyApp(ui, server)