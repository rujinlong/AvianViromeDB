#' module1 UI Function
#'
#' @description A shiny Module for the Avian Virome Database interface.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem tabItems tabItem
#' @importFrom DT DTOutput renderDT
#' @importFrom plotly plotlyOutput renderPlotly
mod_module1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::dashboardPage(
      # Header with logo and database title
      shinydashboard::dashboardHeader(
        title = tagList(
          div(class = "logo-container",
              tags$img(src = "www/logo.png", height = "50px"),
              span(class = "logo-text", "AvianViromeDB")
          )
        ),
        titleWidth = 230
      ),
      # Sidebar with navigation menu
      shinydashboard::dashboardSidebar(
        width = 230,
        shinydashboard::sidebarMenu(
          id = ns("sidebar"),
          shinydashboard::menuItem("Home", tabName = "home", icon = icon("home")),
          shinydashboard::menuItem("Browse Database", tabName = "browse", icon = icon("database")),
          shinydashboard::menuItem("Search", tabName = "search", icon = icon("search")),
          shinydashboard::menuItem("Taxonomy", tabName = "taxonomy", icon = icon("sitemap")),
          shinydashboard::menuItem("Genome Browser", tabName = "genome", icon = icon("dna")),
          shinydashboard::menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
          shinydashboard::menuItem("About", tabName = "about", icon = icon("info-circle")),
          shinydashboard::menuItem("Download", tabName = "download", icon = icon("download"))
        )
      ),
      
      # Main body content
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          # Home tab
          shinydashboard::tabItem(
            tabName = "home",
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Welcome to the Avian Virome Database",
                status = "primary",
                solidHeader = TRUE,
                h4("A comprehensive resource for avian bacteriophage genomics"),
                p("This database contains bacteriophage sequences identified from avian samples through next-generation sequencing and advanced bioinformatics analysis.")
              )
            ),
            fluidRow(
              shinydashboard::box(
                width = 4,
                title = "Database Summary",
                status = "info",
                solidHeader = TRUE,
                p("Quick statistics about the database content:"),
                uiOutput(ns("db_summary"))
              ),
              shinydashboard::box(
                width = 8,
                title = "Featured Visualization",
                status = "info",
                solidHeader = TRUE,
                plotly::plotlyOutput(ns("featured_plot"))
              )
            )
          ),
          
          # Browse tab
          shinydashboard::tabItem(
            tabName = "browse",
            h2("Browse Database"),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Phage Sequences",
                status = "primary",
                solidHeader = TRUE,
                DT::DTOutput(ns("phage_table"))
              )
            )
          ),
          
          # Search tab
          shinydashboard::tabItem(
            tabName = "search",
            h2("Search"),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Search Options",
                status = "primary",
                solidHeader = TRUE,
                fluidRow(
                  column(4, 
                    selectInput(ns("search_field"), "Search Field", 
                      choices = c("Sequence ID", "Taxonomy", "Host", "Sample Origin", "Gene")
                    )
                  ),
                  column(6, 
                    textInput(ns("search_query"), "Search Query", placeholder = "Enter search terms...")
                  ),
                  column(2, 
                    actionButton(ns("search_button"), "Search", icon = icon("search"), 
                                 style = "margin-top: 25px; width: 100%;")
                  )
                )
              )
            ),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Search Results",
                status = "info",
                solidHeader = TRUE,
                DT::DTOutput(ns("search_results"))
              )
            )
          ),
          
          # Taxonomy tab
          shinydashboard::tabItem(
            tabName = "taxonomy",
            h2("Taxonomic Classification"),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Taxonomy Explorer",
                status = "primary",
                solidHeader = TRUE,
                plotly::plotlyOutput(ns("taxonomy_plot"), height = "600px")
              )
            )
          ),
          
          # Additional tabs content placeholder
          shinydashboard::tabItem(
            tabName = "genome",
            h2("Genome Browser"),
            p("Genome visualization feature will be implemented here.")
          ),
          shinydashboard::tabItem(
            tabName = "stats",
            h2("Database Statistics"),
            fluidRow(
              shinydashboard::box(
                width = 6,
                title = "Sequence Length Distribution",
                status = "primary",
                solidHeader = TRUE,
                plotly::plotlyOutput(ns("length_dist"))
              ),
              shinydashboard::box(
                width = 6,
                title = "Taxonomic Distribution",
                status = "primary",
                solidHeader = TRUE,
                plotly::plotlyOutput(ns("tax_dist"))
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "about",
            h2("About the Database"),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Project Information",
                status = "primary",
                solidHeader = TRUE,
                div(
                  style = "text-align: center; margin-bottom: 20px;",
                  img(src = "www/logo1.png", height = "200px", style = "margin-bottom: 5px;"),
                  h3("AvianViromeDB")
                ),
                p("The Avian Virome Database is a comprehensive collection of bacteriophage sequences identified from avian samples."),
                p("Our database is built from public NGS sequencing data that has been processed through a rigorous bioinformatics pipeline:"),
                tags$ol(
                  tags$li("Collection of raw sequencing data from various avian samples"),
                  tags$li("Quality control and preprocessing of sequencing reads"),
                  tags$li("Assembly of reads into contigs"),
                  tags$li("Identification of viral sequences, particularly bacteriophages"),
                  tags$li("Annotation of viral genomes"),
                  tags$li("Taxonomic classification and database integration"),
                  tags$li("Prediction of host range of bacteriophages")
                ),
                h4("Citation"),
                p("If you use this database in your research, please cite: [Citation information]")
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "download",
            h2("Download Data"),
            fluidRow(
              shinydashboard::box(
                width = 12,
                title = "Available Datasets",
                status = "primary",
                solidHeader = TRUE,
                p("Download can be found in the [Zenodo repository](https://zenodo.org/records/xxx).")
              )
            )
          )
        )
      )
    )
  )
}

#' module1 Server Functions
#'
#' @noRd
mod_module1_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Database summary stats for home page
    output$db_summary <- renderUI({
      # Use tryCatch to handle potential database connection errors
      stats <- tryCatch({
        get_db_stats()
      }, error = function(e) {
        # Return placeholder data if database connection fails
        list(
          total_sequences = "XX,XXX",
          phage_families = "XX",
          host_species = "XX",
          sample_sources = "XX"
        )
      })
      
      tagList(
        tags$ul(
          tags$li(strong("Total sequences: "), stats$total_sequences),
          tags$li(strong("Unique phage families: "), stats$phage_families),
          tags$li(strong("Host species: "), stats$host_species),
          tags$li(strong("Sample sources: "), stats$sample_sources)
        )
      )
    })
    
    # Featured plot for home page
    output$featured_plot <- plotly::renderPlotly({
      # Try to get real data, fall back to placeholders
      tax_dist <- tryCatch({
        get_taxonomy_dist()
      }, error = function(e) {
        data.frame(
          family = c("Siphoviridae", "Myoviridae", "Podoviridae", "Other"),
          count = c(45, 30, 15, 10)
        )
      })
      
      # Create the plot
      plotly::plot_ly(x = tax_dist$family, 
                      y = tax_dist$count, 
                      type = "bar") %>%
        plotly::layout(title = "Phage Family Distribution",
                       xaxis = list(title = "Family"),
                       yaxis = list(title = "Count"))
    })
    
    # Phage table for browse tab
    output$phage_table <- DT::renderDT({
      # Try to get real data, fall back to placeholders
      phage_data <- tryCatch({
        get_sequences(limit = 100, offset = 0)
      }, error = function(e) {
        # Return placeholder data if database connection fails
        data.frame(
          sequence_id = paste0("Phage_", 1:10),
          length = sample(5000:50000, 10),
          family = sample(c("Siphoviridae", "Myoviridae", "Podoviridae"), 10, replace = TRUE),
          host = sample(c("Chicken", "Duck", "Turkey", "Goose"), 10, replace = TRUE),
          source = sample(c("Fecal", "Gut", "Respiratory", "Environmental"), 10, replace = TRUE),
          gene_count = sample(10:100, 10)
        )
      })
      
      DT::datatable(
        phage_data,
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        filter = 'top',
        class = 'cell-border stripe'
      )
    })
    
    # Handle search button clicks
    observeEvent(input$search_button, {
      # Get search parameters
      query <- input$search_query
      field <- input$search_field
      
      # Perform search
      search_results <- tryCatch({
        search_db(query, field)
      }, error = function(e) {
        # Return placeholder data if search fails
        data.frame(
          sequence_id = paste0("Phage_", 1:5),
          description = rep("Matching result", 5),
          family = sample(c("Siphoviridae", "Myoviridae"), 5, replace = TRUE),
          host = sample(c("Chicken", "Duck"), 5, replace = TRUE),
          length = sample(10000:40000, 5)
        )
      })
      
      # Update the search results table
      output$search_results <- DT::renderDT({
        DT::datatable(
          search_results,
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
      })
    })
    
    # Taxonomy visualization
    output$taxonomy_plot <- plotly::renderPlotly({
      # Try to get real data, fall back to placeholders
      tax_hierarchy <- tryCatch({
        get_taxonomy_hierarchy()
      }, error = function(e) {
        # Create placeholder data with hierarchical structure
        data.frame(
          order = c(rep("Caudovirales", 9)),
          family = c(rep("Siphoviridae", 2), rep("Myoviridae", 2), rep("Podoviridae", 5)),
          genus = c("Sipho-A", "Sipho-B", "Myo-A", "Myo-B", paste0("Podo-", LETTERS[1:5])),
          count = c(25, 20, 15, 15, 5, 5, 5, 5, 5)
        )
      })
      
      # Create labels and parents for the sunburst plot
      tax_orders <- unique(tax_hierarchy$order)
      tax_families <- unique(tax_hierarchy$family)
      tax_genera <- unique(tax_hierarchy$genus)
      
      # Create labels vector with all taxonomic levels
      labels <- c(tax_orders, tax_families, tax_genera)
      
      # Create parents vector
      parents <- c(
        rep("", length(tax_orders)),  # Orders have no parent
        sapply(tax_families, function(f) {  # Families have order as parent
          tax_hierarchy$order[tax_hierarchy$family == f][1]
        }),
        sapply(tax_genera, function(g) {  # Genera have family as parent
          tax_hierarchy$family[tax_hierarchy$genus == g][1]
        })
      )
      
      # Create values vector
      values <- c(
        sapply(tax_orders, function(o) {  # Total counts for orders
          sum(tax_hierarchy$count[tax_hierarchy$order == o])
        }),
        sapply(tax_families, function(f) {  # Total counts for families
          sum(tax_hierarchy$count[tax_hierarchy$family == f])
        }),
        sapply(tax_genera, function(g) {  # Counts for genera
          sum(tax_hierarchy$count[tax_hierarchy$genus == g])
        })
      )
      
      # Create the sunburst plot
      plotly::plot_ly(
        labels = labels,
        parents = parents,
        values = values,
        type = 'sunburst',
        branchvalues = 'total'
      ) %>% 
        plotly::layout(title = "Taxonomic Hierarchy")
    })
    
    # Sequence length distribution
    output$length_dist <- plotly::renderPlotly({
      # Try to get real data, fall back to placeholders
      length_data <- tryCatch({
        get_length_dist()
      }, error = function(e) {
        data.frame(length = rnorm(1000, 30000, 10000))
      })
      
      # Create the histogram
      plotly::plot_ly(
        x = length_data$length, 
        type = "histogram",
        nbinsx = 30
      ) %>%
        plotly::layout(title = "Sequence Length Distribution",
                       xaxis = list(title = "Sequence Length (bp)"),
                       yaxis = list(title = "Count"))
    })
    
    # Taxonomic distribution
    output$tax_dist <- plotly::renderPlotly({
      # Try to get real data, fall back to placeholders
      tax_dist <- tryCatch({
        get_taxonomy_dist()
      }, error = function(e) {
        data.frame(
          family = c("Siphoviridae", "Myoviridae", "Podoviridae", "Unclassified"),
          count = c(45, 30, 15, 10)
        )
      })
      
      # Create the pie chart
      plotly::plot_ly(
        labels = tax_dist$family,
        values = tax_dist$count,
        type = 'pie'
      ) %>%
        plotly::layout(title = "Taxonomic Distribution")
    })
    
    # Download handler for the Download tab
    observeEvent(input$download_button, {
      # Display a notification that download preparation is in progress
      shinyjs::showNotification(
        "Preparing download. This may take a moment...",
        type = "message",
        duration = 5
      )
      
      # In a real application, you would prepare the requested files here
      # and provide a download link or initiate the download
    })
  })
}

## To be copied in the UI
# mod_module1_ui("module1_1")

## To be copied in the server
# mod_module1_server("module1_1")
