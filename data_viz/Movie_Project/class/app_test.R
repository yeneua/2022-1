getwd()
library(shiny)
load("titles-1.rda")
load("predict.rda")
load("reviews-1.rda")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title을 입력함 
    titlePanel("영화평론 감성분석"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("title", "영화제목:",
                         c("범죄도시2"="A",
                           "닥터 스트레인지: 대혼돈의 멀티버스" = "B",
                           "그대가 조국"="C")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3(textOutput("caption")),
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # reactive function 
    dataInput <- reactive({
      temp = switch(input$title,
                    "A" = "범죄도시2",
                    "B" = "닥터 스트레인지: 대혼돈의 멀티버스",
                    "C" = "그대가 조국")
      t = which(titles == temp)
      predict[t]
    
    })
    #
    output$caption <- renderText({
      s = dataInput()
      p = length(s[s==1])
      n = length(s[s==-1])
      polarity = (p-n)/(p+n)
      paste("Polarity :", round(polarity, 3)*100)
       })
        
    output$moviePlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      temp = table(dataInput())
      names(temp) =c("부정","중립","긍정")
      pie(temp, main="Sentiment Analysis on Movie Revies(ref: naver)",
          col = c("red","green","blue"))
       

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
