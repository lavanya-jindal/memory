library(shiny)
ui <- navbarPage("YTD Metrics",
                 tabPanel("Index",
                           fileInput(inputId = "file1",label=NULL,
                                      multiple = FALSE,
                                      placeholder="No file selected"),
                          tableOutput('contents')
                          
                     )
                   ,
                   
                 tabPanel("YTD Yearly"),
                 
                 tabPanel("YTD by Brand"),
                 tabPanel("YTD by Tier")
                 #navbarMenu("More",
                            #tabPanel("Summary"),
                            #"----",
                            #"Section header",
                            #tabPanel("Table")
                 #)
)


# server <- function(input, output) {
#   output$file1 <- renderTable({
#     if(is.null(input$file1)) {return()}
#     input$file1$datapath
#   })

server <- function(input, output) {
  output$contents <- renderTable({
    
    req(input$file1)
    df <- read.csv(input$file1$datapath) #read_excel
    return(df)
    
  })
  
  
} 
  
  

shinyApp(ui, server)

