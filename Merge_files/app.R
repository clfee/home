


library(shiny)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("CSV File Merger"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_a", "Upload Transaction CSV"),
      fileInput("file_b", "Upload Sales CSV"),
      fileInput("file_c", "Upload Funded Deposit CSV"),
      uiOutput("merge_vars_a_b"),
      uiOutput("merge_vars_b_c"),
      downloadButton("downloadData", "Download Merged File")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Transaction List", DT::dataTableOutput("table_a")),
        tabPanel("Sales List", DT::dataTableOutput("table_b")),
        tabPanel("Funded Deposit List", DT::dataTableOutput("table_c")),
        tabPanel("Merged File", DT::dataTableOutput("table_d"))
        #tabPanel("Transaction List", tableOutput("table_a")),
        #tabPanel("Sales List", tableOutput("table_b")),
        #tabPanel("Funded Deposit List", tableOutput("table_c")),
        #tabPanel("Merged File", tableOutput("table_d"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_a <- reactive({
    req(input$file_a)
    read.csv(input$file_a$datapath)
  })
  
  data_b <- reactive({
    req(input$file_b)
    read.csv(input$file_b$datapath)
  })
  
  data_c <- reactive({
    req(input$file_c)
    read.csv(input$file_c$datapath)
  })
  
  output$merge_vars_a_b <- renderUI({
    req(data_a(), data_b())
    var_names <- intersect(names(data_a()), names(data_b()))
    selectInput("var_ab", "Select merge variable for Transaction and Sales", choices = var_names)
  })
  
  output$merge_vars_b_c <- renderUI({
    req(data_b(), data_c())
    var_names <- intersect(names(data_b()), names(data_c()))
    selectInput("var_bc", "Select merge variable for Sales and Funded Depos it", choices = var_names)
  })
  

  
  #output$table_a <- renderTable({
  #  req(data_a())
  #  data_a()[1:100,]  
  #}#)
  
 
  output$table_a <- DT::renderDataTable({
    req(data_a())
    DT::datatable(data = data_a(),
                  options = list(pageLength = 100, rownames = FALSE) 
    )  
  })

  output$table_b <- DT::renderDataTable({
    req(data_b())
    DT::datatable(data = data_b(),
                  options = list(pageLength = 100, rownames = FALSE) 
    )  
  })
  
  output$table_c <- DT::renderDataTable({
    req(data_c())
    DT::datatable(data = data_c(),
                  options = list(pageLength = 100, rownames = FALSE) 
    )  
  })
  
  
  merged_data <- reactive({
    req(data_a(), data_b(), data_c(), input$var_ab, input$var_bc)
    #select rows contain summary slip
    #[1:(length(na.omit(data_c()[,1]))),]
    temp_ab <- dplyr::full_join(data_a(), data_b(), by = input$var_ab)
    dplyr::full_join(temp_ab, data_c(), by = input$var_bc)
  })
  
  output$table_dx <- renderTable({
    req(merged_data())
    merged_data()[1:100,]  
  })
  
  output$table_d <- DT::renderDataTable({
    req(merged_data())
    DT::datatable(data = merged_data(),
                  options = list(pageLength = 100, rownames = FALSE) 
    )  
  })

  

  
  output$downloadData <- downloadHandler(
    filename = function() {
     paste0(Sys.Date(), "merged_data.csv")
    },
    content = function(file) {
      write.csv(unique(merged_data()), file, row.names = FALSE)
    }
  )

  }
shinyApp(ui, server)

