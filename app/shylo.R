#!/usr/bin/env Rscript

library(shiny)
library(stylo)
library(explor)
library(scatterD3)
library(ggiraph)
library(networkD3)


ui <- fluidPage(
   
   titlePanel("Shylo: A Shiny GUI for Stylo"),
   
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "corpus",
                  label = "Corpus",
                  multiple = TRUE,
                  accept = c('.txt', 'text/plain')),
        
        selectInput(inputId = "language",
                    label = "Language",
                    choices = list("CJK" = "CJK",
                                   "Dutch" = "Dutch",
                                   "English" = "English",
                                   "English (contr.)" = "English.contr",
                                   "English (all)" = "English.all",
                                   "French" = "French",
                                   "German" = "German",
                                   "Hungarian" = "Hungarian",
                                   "Italian" = "Italian",
                                   "Latin" = "Latin",
                                   "Latin (u/v > u)" = "Latin.corr",
                                   "Polish" = "Polish",
                                   "Spanish" = "Spanish",
                                   "Other" = "Other"),
                    selected = "English"),
        
        tags$hr(),
        
        selectInput(inputId = "distance",
                    label = "Distance",
                    choices = list("Argamon's Delta" = "argamon",
                                   "Canberra" = "canberra",
                                   "Classic Delta" = "delta",
                                   "Cosine" = "cosine",
                                   "Eder’s Delta" = "eder",
                                   "Eder’s Simple" = "simple",
                                   "Euclidean" = "euclidean",
                                   "Manhattan" = "manhattan"),
                    selected = "delta"),
        
        numericInput(inputId = "mfw",
                     label = "Most frequent words",
                     value = "100"),
        
        conditionalPanel(condition="input.panels == 'PCA (Individuals)' | input.panels == 'PCA (Variables)'", 
                         tags$hr(),
                         uiOutput("axes")),
        
        conditionalPanel(condition="input.panels != 'About' && input.panels != 'PCA (Individuals)'", 
                         tags$hr(),
                         sliderInput(inputId = "fontsize",
                                     label = "Label size",
                                     min = 4,
                                     max = 20,
                                     value = 12)),
        
        conditionalPanel(condition="input.panels == 'Heatmap'", 
                         checkboxInput(inputId = "include.dendrogram",
                                       label = "Include dendrogram",
                                       value = FALSE)),
        
        conditionalPanel(condition="input.panels == 'Dendrogram'", 
                         radioButtons(inputId = "tree.orientation",
                                      label = "Tree orientation",
                                      choices = list("Horizontal" = "horizontal",
                                                   "Vertical" = "vertical"),
                                      selected = "horizontal"),
                         radioButtons(inputId = "link.type",
                                      label = "Link type",
                                      choices = list("Elbow" = "elbow",
                                                     "Diagonal" = "diagonal"),
                                      selected = "elbow")
                         ),
        
        conditionalPanel(condition="input.panels == 'PCA (Individuals)'", 
                         sliderInput(inputId = "pointsize",
                                     label = "Point size",
                                     min = 8,
                                     max = 128,
                                     value = 64)),
        
        conditionalPanel(condition="input.panels == 'PCA (Individuals)'", 
                         checkboxInput(inputId = "labels",
                                       label = "Show labels",
                                       value = FALSE)),
        
        conditionalPanel(condition="input.panels == 'PCA (Individuals)' && input.labels == true", 
                         sliderInput(inputId = "fontsize.individuals",
                                     label = "Label size",
                                     min = 4,
                                     max = 20,
                                     value = 12))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(title = "About",
                   includeHTML("about.html")),
          
          tabPanel(title = "Network",
                   simpleNetworkOutput("network")),
          
          tabPanel(title = "Dendrogram",
                   dendroNetworkOutput("dendrogram")),
          
          tabPanel(title = "Heatmap",
                   ggiraphOutput("heatmap")),
          
          tabPanel(title = "PCA (Variables)",
                   scatterD3Output("pca.variables")),
          
          tabPanel(title = "PCA (Individuals)",
                   scatterD3Output("pca.individuals")),
          
          id = "panels"                
        )
      )  
   ))

server <- function(input, output) {
  files <- reactive({
    corpus <- input$corpus
    corpus$basename <- lapply(as.vector(corpus$datapath), basename)
    corpus$dirname <- lapply(as.vector(corpus$datapath), dirname)
    corpus$rename <- sapply(1:nrow(corpus), function(x) gsub(corpus$basename[x], corpus$name[x], corpus$datapath[x]))
    file.rename(as.vector(corpus$datapath), as.vector(corpus$rename))
    return(corpus)
  })

  # Stylo
  stylo.results <- reactive({
    req(input$corpus)
    corpus.dir <- files()[[1, "dirname"]]
    distance.measure <- input$distance
    mfw <- input$mfw
    language <- input$language


    results <- stylo(gui = FALSE,
                     corpus.dir = corpus.dir,
                     language = language,
                     distance.measure = distance.measure,
                     mfw.min = mfw,
                     mfw.max = mfw,
                     display.on.screen = FALSE)
    return(results)
  })
   
  # PCA
  pca.results <- reactive({
   req(stylo.results()$distance.table)
   distance.table <- stylo.results()$distance.table
   
   results <- prcomp(x = distance.table,
                     center = TRUE,
                     scale. = TRUE)
   return(prepare_results(results))
  })
  
  # Network
  output$network <- renderForceNetwork({
    list.of.edges <- stylo.results()$list.of.edges
    nodes <- unique(as.data.frame(list.of.edges$Source))
    nodes$ID <- seq.int(nrow(nodes))
    network <- simpleNetwork(Data = list.of.edges,
                             Source = "Source",
                             Target = "Target",
                             fontSize = input$fontsize,
                             fontFamily = "Arial",
                             zoom = TRUE)
    return(network)
  })
  
  # Heatmap
  output$heatmap <- renderggiraph({
    plot.heatmap <- dget("heatmap.R")
    distance.table <- stylo.results()$distance.table
    include.dendrogram <- input$include.dendrogram
    label.size <- input$fontsize
    
    return(plot.heatmap(distance.table, include.dendrogram, label.size))
  })
  
  # PCA
  output$pca.variables <- renderScatterD3({
    req(input$xaxis)
    req(input$yaxis)
    PCA_var_plot(res = pca.results(),
                 xax = input$xaxis,
                 yax = input$yaxis,
                 scale_unit = TRUE,
                 labels_size = input$fontsize,
                 menu = FALSE)
    
  })
  
  output$pca.individuals <- renderScatterD3({
    req(input$xaxis)
    req(input$yaxis)
    lab.var <- if(input$labels){"Lab"} else NULL
    
    PCA_ind_plot(res = pca.results(),
                 xax = input$xaxis,
                 yax = input$yaxis,
                 labels_size = input$fontsize.individuals,
                 point_opacity = 0.5,
                 lab_var = lab.var,
                 point_size = input$pointsize,
                 menu=FALSE)
  })
  
  # Dendrogram
  output$dendrogram <- renderDendroNetwork({
    distance.table <- stylo.results()$distance.table
    
    hc <- hclust(as.dist(distance.table),
                 method = "ward.D")
    
    dendroNetwork(hc = hc,
                  fontSize = input$fontsize,
                  treeOrientation = input$tree.orientation,
                  linkType = input$link.type,
                  zoom = TRUE)
    
  })
  
  output$axes <- renderUI({
    if(is.null(pca.results())){return ()
    } else tagList(
      selectInput(inputId = "xaxis", 
                  label = "X axis",
                  choices = pca.results()$axes,
                  selected = "1"),
      
      selectInput(inputId = "yaxis", 
                  label = "Y axis",
                  choices = pca.results()$axes,
                  selected = "2")
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
