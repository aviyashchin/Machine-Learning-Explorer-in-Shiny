library(markdown)
source("helpers.R")

shinyUI(fluidPage(navbarPage("Navbar",
                             tabPanel("Summary"),
                             tabPanel("Data",
  titlePanel("T-Machine"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      uiOutput("dataset1"),
      uiOutput("dataset2"),
      uiOutput("dataset3"),
      column(6,
             verbatimTextOutput("dateText"),
             verbatimTextOutput("dateText2"),
             verbatimTextOutput("dateRangeText"),
             verbatimTextOutput("dateRangeText2")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Preprocessed",tableOutput("contents")),
        tabPanel("Processed", tableOutput("contents2"))
        
      ))
  )),
  tabPanel("Preparation"),
  tabPanel("Model"),
  tabPanel("Results")
  )
))