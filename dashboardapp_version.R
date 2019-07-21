
library(shinydashboard)
library(shiny)
library("plotly")
library("ggplot2")
library("readxl")


  ui <- dashboardPage(skin="blue",
    dashboardHeader(
      title = "Project: Picture This!",
      titleWidth = 350
    ),
    dashboardSidebar(
      width = 200,
      sidebarMenu(
        menuItem("Index", tabName = "Index",icon=icon("clipboard")),
        menuItem("Yearly data",tabName = "Yearly", icon=icon("thumbtack")),
        menuItem("YTD Yearly",tabName = "YTD_Yearly",icon=icon("bookmark")),
        menuItem("By Brand",tabName = "By_Brand",icon=icon("thumbtack")),
        menuItem("YTD By Brand",tabName = "YTD_By_Brand",icon=icon("bookmark")),
        menuItem("YTD By Tier",tabName = "YTD_By_Tier",icon=icon("thumbtack"))
        # ,
        # menuItem("HH Occupancy",tabName = "HH_Occupancy",icon=icon("bookmark"))


    )),

    dashboardBody(
      tabItems(
      tabItem(tabName = "Index", 
              h3("INDEX"), br(),br(),
              h4("1. Yearly --> Stayers, Stays, Nights,
                  Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018) " , 
                 style = "color: black; font-family: calibri;font-style:italic; "),
              
              br(),
              h4(" 2. YTD Yearly --> Stayers, Stays, Nights,
                 Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)", 
                 style = "color:black;font-family: calibri;font-style:italic;"), 
                 
              br(),
              
              
              h4(" 3. Brand-wise Yearly --> Stayers, Stays, Nights,
                 Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (2017-2018)", style = "color:black;
                 font-family: calibri;font-style:italic;"),
                
              br(),
             
              
              h4(" 4. Brand-wise YTD --> Stayers, Stays, Nights,
                 Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)",
                 style = "color:black;font-family: calibri;font-style:italic;"),
              
              br(),
             
              h4(" 5. Tier-wise YTD --> Stayers, Stays, Nights,
                      Room Revenue, Folio, ALOS, ADR, Nights/stayer, Folio/stayer (Jan-May'17 to Jan-May'19)", 
                 style = "color:black; font-family: calibri; font-style:italic;"),
              br(),br(),br(),
                box(width=9,collapsible=TRUE,
                h5("NOTES:", style="colour:green;font-style:italic;font-family:calibri"), br(), 
                   h5("Tables used: ims_user_work.pp_f_stay, revmgmt.d_property_h, hilton_c360.d_customer_tier_base,
                   hilton_c360.d_olson_member", 
                    style="color:green;"))),
              
      tabItem(tabName = "Yearly", h2("Yearly Data"), dataTableOutput("contents1")
              ),
      
      tabItem(tabName = "YTD_Yearly",h2("YTD Yearly"),box(
        title = "Bar Chart", width="5",status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("plot1", height ="300px", width ="400px")),
        dataTableOutput("contents2")),
              
      tabItem(tabName = "By_Brand",h2("By Brand"), dataTableOutput("contents3")),
      tabItem(tabName = "YTD_By_Brand", h2("YTD By Brand"),
              box(
                title = "Bar Chart", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width="10",plotlyOutput("plot2", height ="400px", width ="850px"))
              ,dataTableOutput("contents4")),
              
      tabItem(tabName = "YTD_By_Tier",h2("YTD By Tier"),
              box(
                title = "Bar Chart", status = "primary", solidHeader = TRUE,width=10,
                collapsible = TRUE,plotlyOutput("plot3", height="350px", width="850px")),dataTableOutput("contents5"))
      # ,
      # tabItem(tabName = "HH_Occupancy", h2("HH Occupancy"),dataTableOutput("contents6"))
      
      
      
          )))
  
  server <- function(input, output) {
  
  output$contents1 <- renderDataTable ({ filter = "top"
  df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 1)
  df <- df[rowSums(is.na(df))==0,]
  df$Folio <- paste("$", format(round(df$Folio/ 1e6, 1), trim = TRUE, big.mark=","), "M")
  df$`Room Revenue` <- paste("$",format(round(df$`Room Revenue`/ 1e6, 0), trim = TRUE, big.mark=","), "M")
  df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
  df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
  df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
  df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
  df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
  df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
  df$`Folio/Stayer` <- paste("$",format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))
  #df$Enrollments <- paste(format(round(as.numeric(df$`Enrollments`), 2), nsmall=0, big.mark= ","))
  style='font-family:calibri;' 
  
  #}
  return(df)
  
  })
  
  
  
  
  data = reactive({
    sheet6 <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet=3)
    choices1 <- c(unique({sheet6$Brand}))
    list(choices1 = choices1,sheet6 = sheet6)
  })
  
  
  output$contents2 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 2) 
    df <- df[rowSums(is.na(df))==0,]
    df$Folio <- paste("$", format(round(df$Folio/ 1e6, 0), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 0), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$Enrollments <- paste(format(round(as.numeric(df$`Enrollments`), 2), nsmall=0 , big.mark= ","))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2,big.mark=","))
    df$`HH Occupancy` <- paste(format(round(as.numeric(df$`HH Occupancy`), 2), nsmall=2))
    return(df)
    
  })
  
  output$contents3 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 3) 
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
    return(df)
    
  })
  
  output$contents4 <- renderDataTable({
    
    
    df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 4) 
    df$Folio <- paste("$", format(round(df$`Folio`/ 1e6, 0), trim = TRUE, big.mark=","), "M")
    df$`Room Revenue` <- paste("$", format(round(df$`Room Revenue`/ 1e6, 0), trim = TRUE, big.mark=","), "M")
    df$Nights <- paste(format(round(as.numeric(df$Nights), 1), nsmall=0, big.mark=","))
    df$Stayers <- paste(format(round(as.numeric(df$Stayers), 1), nsmall=0, big.mark=","))
    df$Stays <- paste(format(round(as.numeric(df$Stays), 1), nsmall=0, big.mark=","))
    df$ALOS <- paste(format(round(as.numeric(df$ALOS), 2), nsmall=2, big.mark=","))
    df$ADR <- paste(format(round(as.numeric(df$ADR), 2), nsmall=2, big.mark=","))
    df$`Nights/Stayer` <- paste(format(round(as.numeric(df$`Nights/Stayer`), 2), nsmall=2))
    df$`Folio/Stayer` <- paste("$", format(round(as.numeric(df$`Folio/Stayer`), 2), nsmall=2, big.mark=","))
    return(df)
    
  })
  output$contents5 <- renderDataTable({
    
    
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
  
  # output$contents6 <- renderDataTable({
  #   
  #   
  #   df <- read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 5)
  #   df$`HH occupancy` <- paste(format(round(as.numeric(df$`HH occupancy`), 3), nsmall=3))
  #   
  #   return(df)
  #   
  #   
  # })
  
  
  data_react1 <- reactive({
    
    data_sheet2 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet=2)
    return(data_sheet2)
  })
  
  
  data_react2 <- reactive({
    
    data_sheet7 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 7)
    return(data_sheet7)
  })
  
  data_react3 <- reactive({
    
    data_sheet6 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 5)
    return(data_sheet6)
  })
  
  data_react4 <- reactive({
    
    data_sheet6 = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_updated_metrics.xlsx", sheet = 6)
    return(data_sheet6)
  })
  
  
  output$plot1 <- renderPlotly({
    
    # ggplot(data = read_excel("C://Users//lavanya.jindal//Desktop//Hilton Yearly View//project1_excel_metrics.xlsx", sheet=2),aes(x=c(data$Year), y=c(data$Nights))) +
    # geom_bar(stat="identity", fill="steelblue")
    # geom_text(aes(label=len), vjust=-0.3, size=3.5)
    
    
    
    
    p1 <- ggplot(data = data_react1(),aes(x=c(data_react1()$Year), y=c(data_react1()$Nights))) +
      geom_bar(stat="identity", fill="steelblue")
    p1+labs(x = "Year", y = "Nights") 
    
  })
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(data = data_react2(),aes(fill=c(data_react2()$Year),x=c( data_react2()$Brand), y=c(data_react2()$Percentage)), 
                 plotly(layout
                        (xaxis = 'Brand' ,
                          yaxis = 'Percentage of Stays' ))) +
      
      geom_bar(stat="identity", fill="steelblue")  
    p2 <- p2 + geom_bar(stat = "identity")
    
    
    p2 +labs(x = "Brand", y="Percentage of Stays", fill="Year")
    
  })

  output$plot3 <- renderPlotly({
    p2 <- ggplot(data = data_react3(),aes(fill = c(data_react4()$Tier),x=c( data_react4()$Year), y=c(data_react4()$Percentage)), 
                 plotly(layout
                        (xaxis = 'Tier' ,
                          yaxis = 'Stayers' ))) +
      
      geom_bar(position="dodge",stat="identity", fill="steelblue") #+ geom_text(aes(label=round(data_react4()$Percentage,0))
                                                              #,position=position_dodge(width=0.3))
    p2 <- p2 + geom_bar(stat = "identity") 
    
    
    p2 +labs(x = "Tier", y="Percentage of Stayers", fill="Year") 
    
    
  })
  
  
    }
 
 shinyApp(ui, server)

#graphics package is masked within shinydashboard - not available for this R version - cannot install