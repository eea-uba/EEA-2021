#######                                                #######
####### Basado en https://github.com/apapiu/Shiny-Apps #######
#######                                                #######
####### Dejamos link al material sobre cómo implementar una shiny app #########
#### https://shiny.rstudio.com/tutorial/ ####### 

library(polynom)
library(ggplot2)
library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("paper"),
                
                titlePanel("Overfitting"),
                
                sidebarLayout(
                  sidebarPanel("Seleccionar parámetros deseados",
                               "",
                               "",
                               # checkboxInput(inputId = "seed", label = "Mantener datos",value = TRUE),
                               sliderInput(inputId = "deg1", label = "Grado del modelo", value = 2,
                                           min = 1, max = 20,step = 1),
                               "Datos",
                               sliderInput(inputId = "n", label = "Cantidad de observaciones", value = 50,
                                           min = 10, max = 200,step = 1),
                               sliderInput(inputId = "q", label = "Grado del target", value = 1,
                                           min = 1, max = 5,step = 1),
                               sliderInput(inputId = "s", label = "Cantidad de ruido", value = .5,
                                           min = 0, max = 3,step = .1), width = 3), 
                  
                  
                  mainPanel(
                    h3(textOutput("text1", container = span)),
                    h6(textOutput("text2", container = span)),
                    withMathJax(),
                    uiOutput('eq0'),
                    uiOutput('eq1'),
                    plotOutput("plot", width = "800px", height = "600px"),
                    "Diego Kozlowski, Juan Manuel Barriola y Sofia Perini, basado en https://github.com/apapiu/Shiny-Apps.",
                    "Compartimos el link al material para constuir sus propias shiny apps para el que le interese https://shiny.rstudio.com/tutorial/"
                    )
                  
                  
                  
                  
                )
)

server <- function (input, output) {
  
  data = reactive({
    
    n = input$n
    q = input$q
    s = input$s
    
    observeEvent(input$seed,{set.seed(1234)}) #sample(1:1000, 1)
    x <- runif(n, min = -1, max = 1)
    epsi <- rnorm(n, mean = 0,sd = s) #ruido
    poly <- polynom::polynomial(rnorm(n = q+1)) #polinomio
    y <- predict(poly, x) + epsi #valores con ruido 
    
    return(list(data.frame(x, y), poly))
  })
  
  output$text1 <- renderText({
    paste0("El concepto de overfitting en el modelo lineal")})
  
  output$text2 <- renderText({
    paste0("El modelo SIEMPRE es lineal en los coeficientes y sus efectos SIEMPRE son aditivos.")})
  
  output$eq0 <- renderUI({
    eq <- paste0("y  = \\beta_0")
    
    for (i in c(1:input$q)) {
      eq <- paste0(eq, '+ \\beta_{',i,'}','x^{',i,'}')
      
    }
    withMathJax(
      h6(paste0('Modelo Real $$',eq,'+ \\epsilon$$',
                      'con $$\\epsilon\\sim\\mathcal{N}(0,\\,',input$s,')$$')))
  })
  
  
  
  output$eq1 <- renderUI({
    eq <- paste0("\\hat{y}  = \\hat{\\beta_0}")
    
    for (i in c(1:input$deg1)) {
      eq <- paste0(eq, '+ \\hat{\\beta_{',i,'}}','x^{',i,'}')
      
    }
    withMathJax(
      h6(paste0('Modelo Propuesto $$',eq,'$$')))
  })
  
  output$plot =  renderPlot({
    
    polynomial <- as.function(data()[[2]])
    
    ggplot(data()[[1]]) +
      aes(x, y) +
      geom_point(alpha = 0.3, size = 3, color='forestgreen') +
      theme_minimal() +
      stat_function(fun = polynomial, size = 1.25, alpha = 0.9) +
      geom_smooth(method = "lm", formula = y ~ poly(x,input$deg1), 
                  color = "steelblue", se = FALSE, size = 1.25, alpha = 0.9) +
      theme(panel.background = element_rect(color = "black"), axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
  })
}

shinyApp(ui, server)

