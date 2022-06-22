library(shiny) # 파일이 따로따로이기 때문에 로드 필요

# Define server logic required to draw a histogram
server <- function(input, output) { # 건드리기 금지
  
  output$d <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- rnorm(input$obs)
    
    
    # draw the histogram with the specified number of bins
    hist(x, main = "정규분포 히스토그램", col = 'blue', border = 'white', xlab="x", ylab="빈도")
  })
}