library(shiny)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
setwd("C:/Users/yena/Documents/GitHub/schoolWorks/data_viz/shiny Project/exShiny/lab")
load(file="data/data1.rda")

ui <- fluidPage(
    
    # Application title
    headerPanel("Lotte DataViz"), 
    
    
    # Sidebar with a slider input a variable
    sidebarLayout(
        sidebarPanel(
            # radio button위젯 사용
            radioButtons("spot", "Branch Office of Lotte : ",
                         c("AA지점" = "AA",
                           "BB지점" = "BB",
                           "CC지점" = "CC",
                           "DD지점" = "DD")),
            hr(),
            
            
            
           
            tags$image(src="donga.jpg", width="200px", height="200px")),
        
        # Show output 
        mainPanel(
          h2(textOutput("caption")),
          plotOutput("plot"),
          tableOutput("table")
            
            
            
            
            )
    )  
     
)
#### server

server <- function(input, output) {
    # reactive function(title과 dataInput)
  dataInput <- reactive({
    temp <- input$spot
    subset(data1, 점포ID==temp)
    
  })
  title <- reactive(
    paste(input$spot, "지점 매출현황")
  )
    
    # title을 renderText에 전송
  output$caption <- renderText(title())
    
    
    # polt을 renderPlot에 전송
    output$plot <- renderPlot({
      ggplot(dataInput(), aes(거래월, 상품대분류명)) +
        geom_tile(aes(fill=amount))+
        scale_fill_gradientn(colors=brewer.pal(n=5, name="RdBu"))+
        ggtitle("월별 거래처에 대한 매출액")+
        xlab("거래월")+
        ylab("상품대분류품목")
    })
    
    #지점에 대한 매출액과 거래건수를 table형태로 rendering하여 전송 
    output$table <- renderTable({
      temp <- as.data.frame(dataInput() %>% 
        group_by(거래월) %>% 
        summarise(amount=sum(amount), 건수=sum(cnt)))
      temp
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
