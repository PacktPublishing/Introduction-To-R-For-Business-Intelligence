# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 8, UI.R file - Marketing Campaign Creator App

# every user interface starts with shinyUI
shinyUI(
  
  # fluidPage allows for a format that 
  # stretches or narrows elements relative
  # to the size of the browser window
  fluidPage(
    
    # all resources typically put into an HTML head tag
    # can be specified using tags$head
    tags$head(
      
      # adding a title makes for a more identifiable
      # tab in the brower window for a user
      tags$title("Campaign Creator"),
      
      # load the Sans Pro Font for cleaner look
      # consistent with RStudio projects
      tags$link(rel="stylesheet", 
                type = "text/css", 
                href = "http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,600"
                ),
      
      # load a style sheet specific to this application
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "app-styling.css"
                ),
      
      # include a browser shortcut icon
      # to make the app more visible in the 
      # browser window and bookmarks folder
      tags$link(rel = "shortcut icon", 
                type = "image/x-icon", 
                href = "bike-shortcut-icon.ico"
                )
      ),
    
    # include a progress wheel whenever the app
    # is performing a calculation longer than 1 second
    busyIndicator(text = "Calculation in progress ... ", wait = 0),
    
    # create a row in the ui devoted to welcoming the user
    # and telling them how to choose different cluster sizes
    # using the slider input
    # NOTE: adding the class'vertical-align' ensures that
    # the slider input is centered vertically in the row
    # next to the block of text
    fluidRow(
      class="vertical-align",
      column(3, 
        p(strong("Hello"), ", this application allows you to specify different clusters of customers", 
          "based on age and income to determine more targeted marketing segments. Simply, ", 
          "move the slider at right for more or less clusters and chose a clustering method."), 
        p("After choosing a cluster scheme", 
          "you can filter and download customer data based on those clusters to run a campaign.")
      ),
      # allow the user to pick a total number of
      # clusters to create
      column(3,
             sliderInput("cluster_count",
                         label = "How Many Clusters?",
                         min = 2, max = 10, 
                         value = 6, step = 1)
      ),
      # allow the user to pick a clustering method
      # default to K-means
      column(6,
             radioButtons("cluster_method",
                          label = "Clustering Method?",
                          choices = c("K-means", "Hierarchical"), 
                          selected = "K-means", 
                          inline = FALSE)
      )
    ),
    
    # create a row specifically for showing the results
    # and diagnostics of the user's cluster count choice
    fluidRow(
      column(7, 
             # display the ggplot of clusters
             plotOutput("cluster_viz", height = "500px")
             ),
      column(5, 
             h3("Cluster Summary Table"),
             # display the table of cluster summaries of age and income
             DT::dataTableOutput("campaign_summary_table", width = "100%")
             )
    ),
    
    # create a separate part of the ui
    # by dividing the page with a horizontal rule hr()
    # and showing them how to download groups of potential
    # campaign members based on the cluster membership and
    # other factors in the table of data
    hr(),
    h3("Potential Campaign Members"),
    fluidRow(
      column(4,
             p("The data in the table below shows all potential customers, their cluster ", 
                        "membership, and their relative percentile of age and income in that cluster. ", 
                        "Use the filter boxes at the top of the table to select even more granular ", 
                        "sets of customers.")
             ),
      column(4,
             p("For example, if you select the 75th percentile and above ", 
               "for 'Income %Tile within Cluster', then you will be selecting only the top 25% ", 
               "of incomes from each cluster, which might be a nicely stratified sample of customers ", 
               "with disposable income relative to their peer group.")
             ),
      column(4,
             # add a button to download the data in the table
             tags$div(class = "itemright",
                      downloadButton("downloadDataFromTable",
                                     "Download Table Data"))
             )
    ),
    
    fluidRow(
      column(12, 
             # display the table of potential campaign members
             DT::dataTableOutput("campaign_table", width = "100%")
             )
    )
    
  ) # this parenthesis closes the fluidPage
) # this parenthesis closes the UI
    