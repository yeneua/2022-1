library(shiny)
if(!require(bslib)){
  install.packages('bslib')
  require(bslib)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "darkly"),
  
  # Application title
  headerPanel("2017447 김예나"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "Number of observations:",
                  min = 1,
                  max = 1000,
                  value = 500),
      hr(), #hr은 그냥 작성해도 됨
      tags$img(src="donga.jpg", width = "200px", height = "200px") #속성이기 때문에 인용부호
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("d")
    )
  )
)
