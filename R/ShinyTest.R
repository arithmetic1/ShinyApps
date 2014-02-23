library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$caption <- renderText({
    input$caption
  })
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    #dist <- rnorm(input$obs)
    #plot(dist)
    # Code from: https://www.rmetrics.org/files/Meielisalp2007/Presentations/Pfaff.pdf
    # http://wps.ablongman.com/wps/media/objects/2829/2897573/ch18.pdf
    # http://www.statmethods.net/advgraphs/probability.html
    # 
    set.seed(12345)
    gwn <- rnorm( input$obs ) #^input$exp
    layout(matrix(1:4, ncol = 2, nrow = 2))
    plot.ts(gwn, xlab = "", ylab = "")
    abline(h = 0, col = "red")
    acf(gwn, main = "ACF")
    qqnorm(gwn)
    pacf(gwn, main = "PACF")
    
    
  })
})
