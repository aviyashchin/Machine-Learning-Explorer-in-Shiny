require(shiny);
require(shinyIncubator);


shinyUI(navbarPage(
  title = 'T-Machine',
  tabPanel('Summary'),
  tabPanel('Preparation',
           fluidPage(
             titlePanel("Data Preparation"),
             sidebarLayout(
               sidebarPanel(fileInput('rawInputFile','Upload Data File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                            uiOutput("labelSelectUI"),
                            checkboxInput('headerUI','Header',TRUE),
                            radioButtons('sepUI','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Comma'),
                            radioButtons('quoteUI','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
                            
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Analysis",
                 
                 column(6,
                        verbatimTextOutput("textmissing"),
                        
                        dataTableOutput("colmissing"))
                 
                   ),
                 tabPanel("Data Table",
                          column(6,dataTableOutput("pre.data"))
                          
                   ),
                 
                 tabPanel("Preprocessing",
                  radioButtons("missingUI","What to do with missing values?",c("Average"='mean',"K Nearest Neighbors"="knn"),"Average"),
                  numericInput("MISSING_COLS_FOR_REMOVAL","If more than x% of col data is missing, delete the column",5),
                  numericInput("MISSING_ROWS_FOR_REMOVAL","If more than x% of row data is missing, delete the row",5)
                  ),

                 tabPanel("Processed Data", 

                          column(6, dataTableOutput("test.clean")))
                 
                 
                 )
                 
               )
               
               
             )
             
             
             
             
             
             
           )),
  tabPanel('Modeling',
           
           fluidPage(
             titlePanel("T-Machine"),
             sidebarPanel('nuthing', 
                          uiOutput("dummyTagUI")),
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
                 ,id="mainTabUI")))),
  navbarMenu('More',
             tabPanel("Contact"),
             tabPanel("Acknowledgements")
  )
))