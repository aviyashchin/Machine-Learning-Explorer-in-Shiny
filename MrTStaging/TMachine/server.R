source("helpers.R")

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output) {

  incolumn <- reactive({
    if(is.null(inFile())){
      return (NULL)
    }else{
      print(inFile())
      return (read.csv(inFile()$datapath, header = input$header,
                       sep = input$sep, quote = input$quote))
    }
  })
  
  inFile <- reactive({
      if(is.null(input$file1)){
        return (NULL)
      }else{
        return (input$file1)
      }
    })
  
  
  # Ask Zheyu For Help
  
  output$linPlot <- renderUI({
    if(is.null(inFile())){
      return (NULL)
    }else{
      x <- input$columns1
      y <- input$columns2
      plot(x, y)
    }
  })
  
#   output$dataset1 = renderUI({
#     data <- incolumn()
#     selectInput('columns1', 'Columns', names(data))
#   })
#   
#   output$dataset2 = renderUI({
#     data <- incolumn()
#     selectInput('columns2', 'Columns', names(data))
#   })
  
  output$dataset3 = renderUI({
    data <- incolumn()
    checkboxGroupInput('show_vars', 'Columns To Select', names(data), select = names(data))
  })
  
  output$dataset4 = renderUI({
    data <- fileInput()
    data[, input$show_vars, drop = FALSE]
  })
  
  output$contents2 <- renderTable({
    datasetInput <- reactive({
      switch(input$show_vars)
    })
  })
  

  output$contents <- renderTable({
    datasetInput <- reactive({
      switch(input$dataset3)
    })
    
    read.csv(inFile()$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })

})