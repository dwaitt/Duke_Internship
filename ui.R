
library(shiny)
library(ggplot2)
library(writexl)
library(data.table)
library(DT)
library(readxl)
source('Functions.R')
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinyFiles)
library(shinyalert)




options(shiny.maxRequestSize = 100 * 1024^2)


sidebar <- dashboardSidebar(disable = TRUE, width = 165,
                            useShinyjs(),
                            sidebarMenu(
                              menuItem("Upload File", tabName = "load", selected = TRUE)
                            )
)

body <- dashboardBody(
  useShinyjs(),
  
  # Make label text bigger
  tags$style(HTML("
    label.control-label {
      font-size: 20px !important;
    }
  ")),
  
  
  tabItems(
    
    tabItem(tabName = "load",
            fluidRow(
              
              column(width=2,
                  fluidRow(
                    br(),
                    fileInput('file', "Input File", accept = c('.tsv'), placeholder = "No file selected",
                              width = "100%"),
                    actionButton('add_row', "Add to Table")
                  )
                ),
                column(width=10,  
                  tabBox(id = 'table', width = 12, height = 750,
                         tabPanel("Table",
                                  fluidRow(
                                    box(title = "Quartile Ratio Over Time", width = 12, status = "primary", solidHeader = TRUE,
                                        DTOutput("mytable", height = "600px"))
                                  )
                         ),
                         tabPanel("Quartile Graph",
                                  fluidRow(box(title = "Sum of Last Quartile", width = 12, status = "primary", solidHeader = TRUE,
                                               plotOutput("ratioPlot", height = "600px", brush = 'ratio_brush'))),
                                  fluidRow(
                                    column(width = 5, tags$b(tags$i('Rows corresponding to datapoints selected')), tableOutput('ratio_brush_table'))
                                  )),
                         
                         tabPanel("Protein Graph",
                                  fluidRow(box(title = "Total Proteins", width = 12, status = "primary", solidHeader = TRUE,
                                               plotOutput("proteinPlot", height = "600px")))),
                         
                         tabPanel("Peptide Graph",
                                  fluidRow(box(title = "Total Peptides", width = 12, status = "primary", solidHeader = TRUE,
                                               plotOutput("peptidePlot", height = "600px")))),
                         
                         tabPanel("Precursor Graph",
                                  fluidRow(box(title = "Total Precursors", width = 12, status = "primary", solidHeader = TRUE,
                                               plotOutput("precursorPlot", height = "600px")))),
                         tabPanel("PeakWidth Graph",
                                  fluidRow(box(title = "Ratio of Ideal Peak Width", width = 12, status = "primary", solidHeader = TRUE,
                                               plotOutput("peakwidthPlot", height = "600px"))))
                         
                  )
                )
          )
        )
      )
    )
dashboardPage(
  dashboardHeader(title = "Duke Proteomics Astral Quality Control", titleWidth = 325),
  sidebar,
  body
)



