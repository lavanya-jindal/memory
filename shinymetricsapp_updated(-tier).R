library(shiny)
#install.packages("readxl")
library("readxl")
library("shinythemes")
library("plotly")
library("ggplot2")
library("colorspace") 
#Stored column names for tier and brand resp
sheet5 <-  read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet=5)
choices2 <- c(unique({sheet5$Tier}))

# sheet6 <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet=3)
# choices1 <- c(unique({sheet6$Brand}))


#Creating navbarPage( ) tabs
#ui <- fluidpage( fluidrow(tags$img(src ="Hilton_logo.png",height="150px" ), 


#))
ui <- fluidPage( title = "Test Page" ,theme = shinytheme('united'),
                 fluidRow( column(width=1,tags$img(src ="Hilton_logo.png", height="30%" )),
                           column(  width = 5, 
                                    offset = 3, tags$strong("Yearly Statistics Report (2017-2019)")
                           ), style = "color:white  ;background-color: #87ceeb; font-size: 15px;
                                                  font-family:georgia;"), br(), style = "font-family:georgia;", #font-weight: bold;",
                 
                 
                 navbarPage (tags$style(HTML(".navbar-default .navbar-nav > li > a {font-weight :bold;}")), 
                             tabPanel("Index", style = " font-style:calibri;", 
                                      
                                      #Index contents 
                                      
                                      h4("1. Yearly --> Stayers, Stays, Nights,
                                         Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018) " , 
                                         style = "color: black; font-family: calibri;font-style:italic; "),
                                      br() ,
                                      br(),
                                      h4(" 2. YTD Yearly --> Stayers, Stays, Nights,
                                         Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)", 
                                         style = "color: black; font-family: calibri;font-style:italic;"),
                                      br(),
                                      br(),
                                      
                                      h4(" 3. Brand-wise Yearly --> Stayers, Stays, Nights,
                                         Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018)", style = "color:black;
                                         font-family: calibri;font-style:italic;"),
                                      br() ,
                                      br(),
                                      
                                      h4(" 4. Brand-wise YTD --> Stayers, Stays, Nights,
                                         Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)",
                                         style = "color:black;font-family: calibri;font-style:italic;"),
                                      br() ,
                                      br(),
                                      
                                      h4(" 5. Tier-wise Yearly --> Stayers, Stays, Nights,
                                         Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018)", style = "color:black;
                                         font-family: calibri; 
                                         font-style:italic;"),
                                      br() ,
                                      br(),
                                      
                                      h4( " 6. Tier-wise YTD --> Stayers, Stays, Nights,
                                          Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)" ,
                                          style = "color:black; font-family:calibri;font-style:italic; ")
                                      
                                      
                                      ), 
                             
                             
                             
                             # tags$style(HTML(".navbar-default .navbar-nav > li > a {font-weight: bold;}")) ,
                             
                             
                             
                             
                             
                             # ), 
                             
                             tabPanel("Yearly", style = " font-style:calibri;font-size:16px;", dataTableOutput('contents1')),
                             tabPanel("YTD Yearly", style = " font-style:calibri;font-size: 16px;", dataTableOutput('contents2'),
                                      
                                      plotlyOutput("plot1", height ="300px", width ="400px") ),
                             
                             tabPanel("By Brand", style = " font-style:calibri;font-size: 16px;",dataTableOutput('contents3')),
                             #selectInput(inputId = 'select1', label = "Select box",
                             #choices = choices1)),
                             tabPanel("YTD by Brand",style = " font-style:calibri;font-size: 16px;", dataTableOutput('contents4'),
                                      plotlyOutput("plot2", height="300px", width="900px")),
                             
                             
                             #dropdown for brand
                             # selectInput(inputId = "select1", label = "Select box",
                             #               choices = data()$choices1)),
                             
                             #tabPanel("By Tier", style = " font-style:calibri;font-size: 16px;",dataTableOutput('contents5')),
                             # selectInput(inputId = "select2", label = "Select box",
                             #              choices = choices2)),
                             tabPanel("YTD by Tier", style = " font-style:calibri;font-size: 16px;", dataTableOutput('contents6'),
                                      plotlyOutput("plot3", height="300px", width="900px")),
                             tabPanel("HH Occupancy", style = " font-style:calibri;font-size: 16px;",
                                       dataTableOutput('contents7')
                             )))

# tabPanel("Yearly HH Occupancy", style = " font-style:calibri;font-size: 16px;", 
#                                           dataTableOutput('contents8'))))
# ,selectInput(inputId = 'select2', label = "Select box",
#              choices = choices2)
# ,verbatimTextOutput("summ")
# font-weight: bold;") 




#navbarMenu("More",
#tabPanel("Summary"),
#"----",
#"Section header",
#tabPanel("Table")
#)



#linking excel sheets to contents to display as tables

server <- function(input, output, session) {
  output$contents1 <- renderDataTable ({ filter = "top"
  
  #req(input$file1)
  df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 1)
  df <- df[rowSums(is.na(df))==0,]
  df$Folio <- paste("$", format(round(df$Folio/ 1e6, 1), trim = TRUE, big.mark=","), "M")
  df$`Room Revenue` <- paste("$",format(round(df$`Room Revenue`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
  df$Nights <- paste(format(round(as.numeric(df$Nights)/1e6, 1), nsmall=0, big.mark=","), "M")
  df$Stayers <- paste(format(round(as.numeric(df$Stayers)/1e6, 1), nsmall=0, big.mark=","),"M")
  df$Stays <- paste(format(round(as.numeric(df$Stays)/1e6, 1), nsmall=0, big.mark=","), "M")
  df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
  df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
  df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
  df$`Folio/Stayer` <- paste("$",format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))
  #df$Enrollments <- paste(format(round(as.numeric(df$`Enrollments`), 2), nsmall=0, big.mark= ","))
  style='font-family:calibri;' 
  
  #}
  return(df)
  
  })
  
  
  # output$summ <- renderPrint({colnames(data()$sheet6)})
  
  data = reactive({
    sheet6 <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet=3)
    choices1 <- c(unique({sheet6$Brand}))
    list(choices1 = choices1,sheet6 = sheet6)
  })
  
  
  output$contents2 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 2) 
    df <- df[rowSums(is.na(df))==0,]
    df$Folio <- paste("$", format(round(df$Folio/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$Enrollments <- paste(format(round(as.numeric(df$`Enrollments`), 2), nsmall=0 , big.mark= ","))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2,big.mark=","))
    df$`HH occupancy` <- paste(format(round(as.numeric(df$`HH occupancy`), 2), nsmall=2))
    return(df)
    
  })
  
  output$contents3 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 3) 
    #df$Year <- as.Date(df$Year)
    df$Year <- substr(df$Year, 1, 4)
    df$Folio <- paste("$", format(round(df$Folio/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))
    #df[,6]     <- as.character(df[,6])
    return(df)
    
  })
  
  output$contents4 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 4) 
    df$Folio <- paste("$", format(round(df$`Folio`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))
    return(df)
    
  })
  
  # output$contents5 <- renderDataTable({
  #   
  #   
  #   df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 5) 
  #   df$Year <- substr(as.character(df$Year), 1, 4)
  #   df$`Folio (in $)` <- paste("$", format(round(df$`Folio (in $)`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
  #   df$`Room Revenue (in $)` <- paste("$", format(round(df$`Room Revenue (in $)`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
  #   df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
  #   df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
  #   df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
  #   df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
  #   df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
  #   df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
  #   df$`Folio/Stayer  (in $)` <- paste("$", format(round(as.numeric(df$`Folio/Stayer  (in $)`), 2), nsmall=2, big.mark=","))
  #   return(df)
  #   
  # })
  # 
  # 
  # 
  # 
  # 
  # 
  output$contents6 <- renderDataTable({


    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 5)

    df$Folio <- paste("$", format(round(df$Folio/ 1e6, 0), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 1), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))

    return(df)

  })
  
  output$contents7 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 6)
    df$`HH occupancy` <- paste(format(round(as.numeric(df$`HH occupancy`), 3), nsmall=3))
    
    return(df)
    
    
  })
  
  
  data_react1 <- reactive({
    
    data_sheet2 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet=2)
    return(data_sheet2)
  })
  
  
  data_react2 <- reactive({
    
    data_sheet4 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 4)
    return(data_sheet4)
  })
  
  # data_react3 <- reactive({
  #   
  #   data_sheet6 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 6)
  #   return(data_sheet6)
  # })
  
  
  output$plot1 <- renderPlotly({
    
    # ggplot(data = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet=2),aes(x=c(data$Year), y=c(data$Nights))) +
    # geom_bar(stat="identity", fill="steelblue")
    # geom_text(aes(label=len), vjust=-0.3, size=3.5)
    
    
    
    
    p1 <- ggplot(data = data_react1(),aes(x=c(data_react1()$Year), y=c(data_react1()$Nights))) +
      geom_bar(stat="identity", fill="steelblue")
    p1+labs(x = "Year", y = "Nights") 
    
  })
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(data = data_react2(),aes(x=c( data_react2()$Brand), y=c(data_react2()$Stayers)), #yaxis = list(title="ts")
      plotly(layout
             (xaxis = 'Brand' ,
               yaxis = 'Year' ))) +
      
      geom_bar(stat="identity", fill="steelblue")  #,color = ~variable # , position = 'stack')
    p2 <- p2 + geom_bar(stat = "identity" #, position = "stack"
    )
    
    p2 +labs(x = "Brand", y="Stayers") #,add_lines(x = ~ date, y = ~ median)
    
  })
  
  # output$plot3 <- renderPlotly({
    # p3 <- ggplot(data = data_react3(),aes(fill = data_react3()$Tier,x=c(#data_react2()$Year, 
    # data_react3()$Year), y = c(data_react3()$Stayers)), 
    #fill = data_react2()$Tier), yaxis = list(title="ts")
    # plotly(
    # layout
    #       (xaxis = 'Tier',
    #       yaxis = 'Year', type = 'bar' ))) +
    #   
    # geom_bar(stat="identity",  aes(colour=data_react3()$Tier),     #aes(position_stack(vjust = 1)),
    #          fill="steelblue" ) 
    # p3 <- p3 + geom_bar(stat = "identity" )
    #                     
    # 
    # p3 +labs(x = "Tier", y ="Stayers")
    # 
    # data_plot <- data_react3()
    # 
    # print(colnames(data_plot))
    # 
    # ggplot(data_plot, aes(fill=data_plot$Tier, y=data_plot$Stayers, x=data_plot$`YTD Year`)) + 
    #   geom_bar(position="dodge", stat="identity")
    # 
    # 
    # ggplot(data_react3(), aes(fill=data_plot$Tier, y=data_plot$Stayers, x=data_plot$`YTD Year`)) + 
    #   geom_bar( stat="identity", position="fill")
    
    
    #sheetu <-  read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet=5)
    
    # hist(x = sheetu$Year, y= sheetu$Nights, breaks=3, freq=FALSE,
    #      col="blue", border='black', density=15, ylab="Number of nights",
    #      xlab="Year"
    
    
    #)
    
    #})
  
  
  
  # output$contents7 <- renderDataTable({
  #   
  #   
  #   df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet = 7)
  #   df$Year <- substr(as.character(df$Year), 1, 4)
  #   # df$`HH Nights` <- paste(format(round(as.numeric(df$`HH Nights`), 5), nsmall=0, big.mark=","))
  #   # df$`Total Nights` <- paste(format(round(as.numeric(df$`Total Nights`), 5), nsmall=0, big.mark=","))
  #   # df$`HH Occupancy` <-  paste(format(round(as.numeric(df$`HH Occupancy`), 5), nsmall=5))
  #   return(df)
  #   
  # })
  # 
  # renderDataTable({ df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet = 8)
  # df$Year <- substr(as.character(df$Year), 1, 4)
  # return (df)
  # })
  
  #to add reactive event and then observeEvent() which triggers corresponding records
  
  
  
  #observeEvent(input$select1, )
  
  # output$select1 <- updateSelectInput(session, 'select1',  choices = choices2,
  #                   selected = NULL)
  
}


# output$text1 <- renderPrint({ "1. Yearly --> Stayers, Stays, Nights, 
# Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018)"
#   
#    })

shinyApp(ui, server)


#Notes:
#treatment() 
#use reactive()
#store column names and then refer to it

