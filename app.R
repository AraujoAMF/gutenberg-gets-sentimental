library(shiny)
source("/home/allan/Documentos/gutenberg_vis/processing.R")

ui <- fluidPage(
  titlePanel("Gutenberg gets sentimental"),
  dataTableOutput("gutenberg_base"),
  textInput("id_user_input", "Gutenberg Id"),
  verbatimTextOutput("value"),
  plotOutput("top_25_words"),
  plotOutput("top_25_2grams"),
  plotOutput("top_25_3grams"),
  plotOutput("narrative_arc"),
  plotOutput("treemap"),
  plotOutput("dendo")
  
)

server <- function(input, output) {
  
  output$gutenberg_base <-  renderDataTable(gutenberg_base, options  = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  output$value <- renderText({ input$id_user_input })
  
  raw_text <-reactive({ gutenberg_download2(input$id_user_input)})
  output$raw_text <- renderDataTable({raw_text})
  text1 <- reactive({preprocess_gutenberg(raw_text())})
  
  output$top_25_words <- renderPlot({make_most_frequent_ngrams(text1())})
  output$top_25_2grams <- renderPlot({make_most_frequent_ngrams(text1(), 2)})
  output$top_25_3grams <- renderPlot({make_most_frequent_ngrams(text1(), 3)})
  output$treemap <- renderPlot ({make_treemap(text1())})
  output$narrative_arc <- renderPlot ({make_narrative_arc(text1())})
  output$dendo <- renderPlot(make_dendogram(text1()))
}

shinyApp(ui = ui, server = server)

