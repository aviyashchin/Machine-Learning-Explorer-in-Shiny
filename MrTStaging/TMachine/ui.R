require(shiny);
require(shinyIncubator);

shinyUI(pageWithSidebar(
  headerPanel("T-Machine"),
  sidebarPanel(fileInput('rawInputFile','Upload Data File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
               
               uiOutput("labelSelectUI"),
               
               checkboxInput('headerUI','Header',TRUE),
               radioButtons('sepUI','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
               radioButtons('quoteUI','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote'),
               uiOutput("dummyTagUI")
  ),
  mainPanel(
    
            tabsetPanel(
              tabPanel("Model Selection Tab",radioButtons("crossFoldTypeUI","Cross Validation Type",c("K-Fold CV"='cv',"Repeated KFold CV"="repeatedcv"),"K-Fold CV"),
                       numericInput("foldsUI","Number of Folds(k)",5),
                       conditionalPanel(condition="input.crossFoldTypeUI == repeatedcv",numericInput("repeatUI","Number of Repeats",5)),
                       uiOutput("CVTypeUI"),
                       radioButtons("preprocessingUI","Pre-processing Type",c('No Preprocessing'="",'PCA'="pca",'ICA'="ica"),'No Preprocessing'),
                       uiOutput("ppUI"),
                       selectInput("modelSelectionUI","Select Model",
                                   c('Elastic Net'="en",'Neural Network'="nn",
                                     'Random Forest'="rf"),"Elastic Net"),
                       uiOutput("modelParametersUI"),
                       tags$hr(),
                       actionButton("runAnalysisUI","Run Analysis")),
              tabPanel("Model Results View",h4("Best Fit Model"),tableOutput("bestResultsUI"),h4("Full Model Output"),tableOutput("trainResultsUI"),plotOutput("finalPlotUI")),
              tabPanel("Data Table View",dataTableOutput("rawDataView")),
              tabPanel("Caret Feature View",plotOutput("caretPlotUI"))
              ,id="mainTabUI"))))