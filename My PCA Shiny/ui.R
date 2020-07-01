library(shiny)
library(stats)
library(calibrate)
library(reporttools)
library(MASS)
library(matlib)
library(shinydashboard)

ui <- dashboardPage(skin="red",
  dashboardHeader(title = "PCA Demonstration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data Via CSV File", tabName = "maindashboard2", icon = icon("file-excel-o")),
      menuItem("Mean Subtraction & Scaling", tabName = "links", icon = icon("scale",lib="glyphicon")),
      menuItem("Scatter Plots of the Data", tabName = "patterns", icon = icon("line-chart")),
      menuItem("Compute Covariance Matrix (C)", tabName = "communities", icon = icon("th")),
      menuItem("Eigenvalues/vectors of C", tabName = "pathways", icon = icon("line-chart")),
      menuItem("Displaying D, V, V^-1", tabName = "pathways1", icon = icon("th")),
      menuItem("2 PC Retention Criteria", tabName = "pathways2", icon = icon("info")),
      menuItem("Select Number of PCs to Retain", tabName = "pathways3", icon = icon("hand-pointer-o")),
      menuItem("Biplot", tabName = "pathways4", icon = icon("line-chart"))
      #menuItem("Download Scores", tabName = "pathways5", icon = icon("th"))
    )
  ),
  ##K Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "maindashboard2",
              fluidPage(theme = "bootstrap.css",
                titlePanel("Upload a CSV File of the Data To Perform PCA On"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput('datafile', 'Choose CSV file',
                              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                    #fileInput('file1', 'Choose CSV File',
                    #         accept=c('text/csv', 
                    #'text/comma-separated-values,text/plain', 
                    #                 '.csv')),
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
                                 '"')
                  ),
                  mainPanel(
                    tableOutput('contents')
                  )
                )
              )
      ),
      tabItem(tabName = "links",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Mean Subtraction and Scaling by Standard Deviation"),
                            tableOutput('meancentered'),
                            tableOutput('stand')
                          )
                        ),
      tabItem(tabName = "patterns",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Plots of Original Data and Mean Centered+Scaled Data"),
                        plotOutput("plot1", click = "plot_click"),
                        plotOutput("plot2", click = "plot_click")
              )
      ),
      tabItem(tabName = "communities",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Covariance Matrix"),
                        tableOutput('covari')
              )
      ),
      tabItem(tabName = "pathways",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Computing the Eigenvalues and Eigenvectors of Covariance Matrix"),
                        tableOutput("printeigs1"),
                        #textOutput("printeigs2"),
                        tableOutput('eigenv1'),
                        plotOutput("plot3", click = "plot_click")
              )
      ),
      tabItem(tabName = "pathways1",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Eigenvalue Matrix, Eigenvector Matrix and Inverse of Eigenvector Matrix"),
                        tableOutput("eigenvMat1"),
                        tableOutput("matofeigenvec"),
                        tableOutput("inveigmat")
              )
      ),
      tabItem(tabName = "pathways2",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Scree Plot and Table of Cumulative Proportion of Variance Retained"),
                        plotOutput("screeplot"),
                        tableOutput("cumulprop")
              )
      ),
      tabItem(tabName = "pathways3",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Select Number of PCs to Retain"),
                        uiOutput("inputcomp"),
                        textOutput("printText"),
                        tableOutput("changedloadings"),
                        textOutput("printText2"),
                        tableOutput("changedscores")
              )
      ),
      tabItem(tabName = "pathways4",
              fluidPage(theme = "bootstrap.css",
                        titlePanel("Biplot showing Scores and Loadings"),
                        #plotOutput("plot4", click = "plot_click"),
                        #plotOutput("plot5", click = "plot_click"),
                        textOutput("printText55"),
                        plotOutput("biplot", click = "plot_click")
              )
      )
      
              )
      )
      
    
      
      
      
      )
                
  
      



#shinyUI(fluidPage(
 # titlePanel("Uploading Files"),
  #sidebarLayout(
   # sidebarPanel(
    #  fileInput('datafile', 'Choose CSV file',
     #           accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      #fileInput('file1', 'Choose CSV File',
      #         accept=c('text/csv', 
      #'text/comma-separated-values,text/plain', 
      #                 '.csv')),
      #tags$hr(),
      #checkboxInput('header', 'Header', TRUE),
      #radioButtons('sep', 'Separator',
      #             c(Comma=',',
       #              Semicolon=';',
        #             Tab='\t'),
         #          ','),
      #radioButtons('quote', 'Quote',
       #            c(None='',
        #             'Double Quote'='"',
         #            'Single Quote'="'"),
          #         '"')
    #),
    #mainPanel(
     # tableOutput('contents'),
    #  tableOutput('meancentered'),
     # tableOutput('stand'),
      #plotOutput("plot1", click = "plot_click"),
      #plotOutput("plot2", click = "plot_click"),
      #tableOutput('covari'),
      #textOutput("printeigs1"),
      #textOutput("printeigs2"),
      #tableOutput('eigenv1'),
      #plotOutput("plot3", click = "plot_click"),
      #tableOutput("eigenvMat1"),
      #tableOutput("matofeigenvec"),
      #tableOutput("inveigmat"),
      #plotOutput("screeplot"),
      #tableOutput("cumulprop"),
      #uiOutput("inputcomp"),
      #textOutput("printText"),
      #tableOutput("changedloadings"),
      #textOutput("printText2"),
      #tableOutput("changedscores"),
      #plotOutput("plot4", click = "plot_click"),
      #plotOutput("plot5", click = "plot_click"),
      #textOutput("printText55"),
      #plotOutput("biplot", click = "plot_click")
    #)
  #)
#))
