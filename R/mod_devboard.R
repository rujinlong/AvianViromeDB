#' devboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom reactable reactableOutput renderReactable
mod_devboard_ui <- function(id) {
  ns <- NS(id)

  # Connect to the SQLite database and load the data
  db_path <- app_sys("analyses/pc028e3.sqlite")

  # Use placeholders in case database connection fails
  df_ctganno <- tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    df <- DBI::dbGetQuery(con, "SELECT * FROM contig_annotation")
    DBI::dbDisconnect(con)
    df
  }, error = function(e) {
    # If any error occurs, return placeholder data
    message("Error loading data: ", e$message)
  })

  tagList(
    # create a page with a sidebar
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("contig_length"), "Contig length",
                   min = 0,
                   max = max(df_ctganno$contig_length, 1000000),
                   value = c(0, max(df_ctganno$contig_length, 1000000))),
        sliderInput(ns("checkv_completeness"), "CheckV completeness",
                   min = 0, max = 100, value = c(0, 100)),
        sliderInput(ns("checkv_quality_score"), "CheckV quality score",
                   min = 0, max = 100, value = c(0, 100)),
        sliderInput(ns("checkv_contamination"), "CheckV contamination",
                   min = 0, max = 100, value = c(0, 100)),
        sliderInput(ns("virsorter2_score"), "VirSorter2 score",
                   min = 0, max = 100, value = c(0, 100)),
        selectInput(ns("checkv_provirus"), "CheckV provirus",
                   choices = c("All", unique(df_ctganno$checkv_provirus)),
                   selected = "All"),
        selectInput(ns("lifestyle"), "Lifestyle",
                   choices = c("All", unique(df_ctganno$lifestyle)),
                   selected = "All"),
        selectInput(ns("realm"), "Realm",
                   choices = c("All", unique(df_ctganno$realm)),
                   selected = "All"),
        selectInput(ns("kingdom"), "Kingdom",
                   choices = c("All", unique(df_ctganno$kingdom)),
                   selected = "All"),
        selectInput(ns("phylum"), "Phylum",
                   choices = c("All", unique(df_ctganno$phylum)),
                   selected = "All"),
        selectInput(ns("class"), "Class",
                   choices = c("All", unique(df_ctganno$class)),
                   selected = "All"),
        selectInput(ns("order"), "Order",
                   choices = c("All", unique(df_ctganno$order)),
                   selected = "All"),
        selectInput(ns("family"), "Family",
                   choices = c("All", unique(df_ctganno$family)),
                   selected = "All"),
        selectInput(ns("genus"), "Genus",
                   choices = c("All", unique(df_ctganno$genus)),
                   selected = "All"),
        selectInput(ns("species"), "Species",
                   choices = c("All", unique(df_ctganno$species)),
                   selected = "All"),
        selectInput(ns("virsorter2_group"), "VirSorter2 group",
                   choices = c("All", unique(df_ctganno$virsorter2_group)),
                   selected = "All"),
        selectInput(ns("vc_novel_genus"), "VC novel genus",
                   choices = c("All", unique(df_ctganno$vc_novel_genus)),
                   selected = "All"),
        selectInput(ns("vc_with_ref"), "VC with reference",
                   choices = c("All", unique(df_ctganno$vc_with_ref)),
                   selected = "All")
      ),
      mainPanel(
        reactable::reactableOutput(ns("table"))
      )
    )
  )
}

#' devboard Server Functions
#'
#' @noRd
mod_devboard_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Connect to the SQLite database
    db_data <- reactive({
      db_path <- app_sys("analyses/pc028e3.sqlite")

      tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        df <- DBI::dbGetQuery(con, "SELECT * FROM contig_annotation")
        DBI::dbDisconnect(con)
        df
      }, error = function(e) {
        # If any error occurs, return placeholder data
        warning("Error loading data: ", e$message)
      })
    })

    # Filter data based on inputs
    filtered_data <- reactive({
      data <- db_data()

      # Apply filters
      if (!is.null(input$contig_length)) {
        data <- data[data$contig_length >= input$contig_length[1] &
                     data$contig_length <= input$contig_length[2], ]
      }

      if (!is.null(input$checkv_completeness)) {
        data <- data[data$checkv_completeness >= input$checkv_completeness[1] &
                     data$checkv_completeness <= input$checkv_completeness[2], ]
      }

      if (!is.null(input$checkv_quality_score)) {
        data <- data[data$checkv_quality_score >= input$checkv_quality_score[1] &
                     data$checkv_quality_score <= input$checkv_quality_score[2], ]
      }

      if (!is.null(input$checkv_contamination)) {
        data <- data[data$checkv_contamination >= input$checkv_contamination[1] &
                     data$checkv_contamination <= input$checkv_contamination[2], ]
      }

      if (!is.null(input$virsorter2_score)) {
        data <- data[data$virsorter2_score >= input$virsorter2_score[1] &
                     data$virsorter2_score <= input$virsorter2_score[2], ]
      }

      # Apply dropdown filters
      if (!is.null(input$checkv_provirus) && input$checkv_provirus != "All") {
        data <- data[data$checkv_provirus == input$checkv_provirus, ]
      }

      if (!is.null(input$lifestyle) && input$lifestyle != "All") {
        data <- data[data$lifestyle == input$lifestyle, ]
      }

      if (!is.null(input$realm) && input$realm != "All") {
        data <- data[data$realm == input$realm, ]
      }

      if (!is.null(input$kingdom) && input$kingdom != "All") {
        data <- data[data$kingdom == input$kingdom, ]
      }

      if (!is.null(input$phylum) && input$phylum != "All") {
        data <- data[data$phylum == input$phylum, ]
      }

      if (!is.null(input$class) && input$class != "All") {
        data <- data[data$class == input$class, ]
      }

      if (!is.null(input$order) && input$order != "All") {
        data <- data[data$order == input$order, ]
      }

      if (!is.null(input$family) && input$family != "All") {
        data <- data[data$family == input$family, ]
      }

      if (!is.null(input$genus) && input$genus != "All") {
        data <- data[data$genus == input$genus, ]
      }

      if (!is.null(input$species) && input$species != "All") {
        data <- data[data$species == input$species, ]
      }

      if (!is.null(input$virsorter2_group) && input$virsorter2_group != "All") {
        data <- data[data$virsorter2_group == input$virsorter2_group, ]
      }

      if (!is.null(input$vc_novel_genus) && input$vc_novel_genus != "All") {
        data <- data[data$vc_novel_genus == input$vc_novel_genus, ]
      }

      if (!is.null(input$vc_with_ref) && input$vc_with_ref != "All") {
        data <- data[data$vc_with_ref == input$vc_with_ref, ]
      }

      data
    })

    # Render the table
    output$table <- reactable::renderReactable({
      reactable::reactable(
        filtered_data(),
        filterable = TRUE,
        searchable = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 10,
        wrap = FALSE,
        pageSizeOptions = c(10, 25, 50, 100)
      )
    })
  })
}

## To be copied in the UI
# mod_devboard_ui("devboard_1")

## To be copied in the server
# mod_devboard_server("devboard_1")
