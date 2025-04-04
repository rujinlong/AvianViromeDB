#' vOTU Browser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 p
#' @importFrom DT dataTableOutput
mod_votu_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- English Translation ---
    shiny::h1("Browse vOTUs"),
    shiny::p("The table below displays the viral operational taxonomic units (vOTUs) identified in the AVID database. Use the filter boxes above each column to search and click column headers to sort."),
    # --- End English Translation ---
    DT::dataTableOutput(ns("votu_table"))
  )
}

#' vOTU Browser Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive req isolate
#' @importFrom DT renderDataTable datatable JS
#' @importFrom dplyr select rename mutate case_when filter arrange %>%
#' @importFrom tibble tibble
#' @importFrom stringr str_pad
mod_votu_browser_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Mock vOTU Data ---
    # NOTE: In a real application, replace this with code to read from your SQLite database
    # using DBI::dbConnect, RSQLite::SQLite(), DBI::dbReadTable or dplyr::tbl
    # Example structure for fetching and processing:
    # con <- DBI::dbConnect(RSQLite::SQLite(), "path/to/your/avid_annotations.sqlite.db")
    # votu_data_reactive <- reactive({
    #   DBI::dbReadTable(con, "contig_annotation_filtered_vOTU_infomap") %>% # Or relevant table
    #     # Select, rename, and calculate necessary columns (e.g., gc_content, quality, predicted_host, counts, source_host)
    #     dplyr::select(
    #         votu_id = vOTU_id,
    #         contig_length,
    #         # gc_content = ... , # Needs calculation or retrieval
    #         quality = checkv_quality, # Assuming checkv_quality exists, or derive from completeness/contamination
    #         taxonomy = class, # Example, might need concatenation
    #         # predicted_host = ... , # Needs join with host_filtered table
    #         lifestyle = lifestyle, # Or bacphlip_lifestyle
    #         # amg_count = ..., # Needs aggregation
    #         # arg_count = ..., # Needs aggregation
    #         source_host = dplyr::case_when(
    #             host_Chicken > 0 ~ "Chicken",
    #             host_Duck > 0 ~ "Duck",
    #             host_Goose > 0 ~ "Goose",
    #             # Add other hosts...
    #             TRUE ~ "Other/Mixed"
    #         )
    #         # ... other required columns
    #     ) %>%
    #     # Ensure column names match DT::datatable colnames argument
    #     dplyr::rename(
    #         `vOTU ID` = votu_id,
    #         `Length (bp)` = contig_length,
    #         # `GC (%)` = gc_content,
    #         `Quality` = quality,
    #         `Taxonomy` = taxonomy,
    #         `Predicted Host` = predicted_host,
    #         `Lifestyle` = lifestyle,
    #         `AMG Count` = amg_count,
    #         `ARG Count` = arg_count,
    #         `Source Host` = source_host
    #     )
    # })
    # DBI::dbDisconnect(con) # Ensure connection is closed

    # Temporary mock data
    mock_votu_data <- shiny::reactive({
      tibble::tibble(
        votu_id = paste0("vOTU_", stringr::str_pad(1:20, 4, pad = "0")),
        contig_length = sample(5000:150000, 20, replace = TRUE),
        gc_content = round(runif(20, 30, 65), 1),
        quality = sample(c("Complete", "High-quality", "Medium-quality"), 20, replace = TRUE, prob = c(0.2, 0.4, 0.4)),
        taxonomy = sample(c("Caudoviricetes; Herelleviridae", "Caudoviricetes; Straboviridae", "Caudoviricetes; Drexlerviridae", "Unclassified"), 20, replace = TRUE),
        predicted_host = sample(c("Bacillota; Escherichia", "Bacteroidota; Bacteroides", "Bacillota_A; Phocaeicola", "Actinomycetota; Bifidobacterium", "Unknown"), 20, replace = TRUE),
        lifestyle = sample(c("Virulent", "Temperate"), 20, replace = TRUE),
        amg_count = sample(0:10, 20, replace = TRUE),
        arg_count = sample(0:2, 20, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
        source_host = sample(c("Chicken", "Duck", "Goose", "Egret"), 20, replace = TRUE)
      ) %>%
        # Rename for UI display (English)
        dplyr::rename(
          `vOTU ID` = votu_id,
          `Length (bp)` = contig_length,
          `GC (%)` = gc_content,
          `Quality` = quality,
          `Taxonomy` = taxonomy,
          `Predicted Host` = predicted_host,
          `Lifestyle` = lifestyle,
          `AMG Count` = amg_count,
          `ARG Count` = arg_count,
          `Source Host` = source_host
        )
    })


    output$votu_table <- DT::renderDataTable({
      shiny::req(mock_votu_data()) # Ensure data is loaded

      DT::datatable(
        mock_votu_data(),
        rownames = FALSE,
        filter = 'top',
        extensions = c('Buttons', 'Scroller'),
        options = list(
          # --- Basic Options ---
          # server = TRUE, # Recommended for large datasets, requires server-side logic
          # --- English Language Options ---
          language = list(search = "Global Search:", lengthMenu = "Show _MENU_ entries per page",
                          zeroRecords = "No matching records found", info = "Showing _START_ to _END_ of _TOTAL_ entries",
                          infoEmpty = "Showing 0 to 0 of 0 entries", infoFiltered = "(filtered from _MAX_ total entries)",
                          paginate = list(first = "First", previous = "Previous", `next` = "Next", last = "Last")),
          # --- End English Language Options ---
          pageLength = 10,
          lengthMenu = c(10, 20, 50, 100),
          autoWidth = TRUE,

          # --- Extension Options ---
          dom = 'Bfrtip', # Define control layout ('B' for buttons)
          buttons = list(
            list(extend = 'copy', text = 'Copy'),
            list(extend = 'csv', text = 'Export CSV', filename = 'avid_votus_export'),
            list(extend = 'excel', text = 'Export Excel', filename = 'avid_votus_export'),
            list(extend = 'colvis', text = 'Column Visibility') # Column visibility control
          ),
          # Scroller extension for virtual scrolling performance with many rows
          deferRender = TRUE,
          scrollY = 500,
          scrollCollapse = TRUE,
          scroller = TRUE
          # columnDefs = list(list(targets = c(4, 5), searchable = FALSE)) # Example: disable search on specific columns
        )
      )
    })

  })
}

## To be copied in the UI
# mod_votu_browser_ui("votu_browser_1")

## To be copied in the server
# mod_votu_browser_server("votu_browser_1")
