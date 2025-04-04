#' Predicted Host Browser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 p
#' @importFrom DT dataTableOutput
mod_host_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- English Translation ---
    shiny::h1("Browse Predicted Hosts"),
    shiny::p("Potential bacterial hosts for viruses in the avian virome, based on computational predictions. The table shows the number of associated vOTUs, viral lifestyle distribution, and average genome size for each predicted host taxon."),
    # --- End English Translation ---
    DT::dataTableOutput(ns("host_table"))
  )
}

#' Predicted Host Browser Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr select rename group_by summarise n_distinct mean case_when filter arrange %>%
#' @importFrom tibble tibble
mod_host_browser_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Mock Host Data ---
    # NOTE: In real application, read from host_filtered table and aggregate with contig_annotation_filtered_vOTU table
    # Example structure for fetching and processing:
    # con <- DBI::dbConnect(RSQLite::SQLite(), "path/to/your/avid_annotations.sqlite.db")
    # host_data_raw <- DBI::dbReadTable(con, "host_filtered")
    # votu_info_simple <- DBI::dbReadTable(con, "contig_annotation_filtered_vOTU_infomap") %>% select(contig_id = vOTU_id, lifestyle, contig_length) # Simplified info
    #
    # host_data_reactive <- reactive({
    #   host_data_raw %>%
    #     dplyr::left_join(votu_info_simple, by = "contig_id") %>% # Assuming contig_id exists in host_filtered
    #     dplyr::group_by(host_taxonomy = `Host.taxonomy`) %>% # Note the dot in column name
    #     dplyr::summarise(
    #       votu_count = dplyr::n_distinct(contig_id, na.rm = TRUE),
    #       virulent_count = sum(lifestyle == "Virulent", na.rm = TRUE),
    #       temperate_count = sum(lifestyle == "Temperate", na.rm = TRUE),
    #       avg_genome_size = round(mean(contig_length, na.rm = TRUE)),
    #       .groups = 'drop'
    #     ) %>%
    #     dplyr::mutate(
    #       total_lifestyle = virulent_count + temperate_count,
    #       lifestyle_dist = dplyr::case_when(
    #         total_lifestyle > 0 ~ paste0(
    #           "Virulent: ", round(virulent_count / total_lifestyle * 100), "%, ",
    #           "Temperate: ", round(temperate_count / total_lifestyle * 100), "%"
    #         ),
    #         TRUE ~ "N/A"
    #       )
    #     ) %>%
    #     dplyr::select(host_taxonomy, votu_count, lifestyle_dist, avg_genome_size) %>%
    #     dplyr::rename(
    #         `Predicted Host Taxonomy` = host_taxonomy,
    #         `Associated vOTU Count` = votu_count,
    #         `Lifestyle Distribution` = lifestyle_dist,
    #         `Avg. Genome Size (bp)` = avg_genome_size
    #     ) %>%
    #     dplyr::arrange(dplyr::desc(`Associated vOTU Count`))
    # })
    # DBI::dbDisconnect(con)

    # Temporary mock data
    mock_host_data <- shiny::reactive({
      tibble::tibble(
        host_taxonomy = c("Bacillota; Escherichia", "Bacteroidota; Bacteroides", "Bacillota_A; Phocaeicola", "Pseudomonadota; Escherichia", "Actinomycetota; Bifidobacterium", "Bacillota; Lactobacillus"),
        votu_count = c(1500, 1200, 950, 800, 500, 450),
        lifestyle_dist = c("Virulent: 80%, Temperate: 20%", "Virulent: 40%, Temperate: 60%", "Virulent: 60%, Temperate: 40%", "Virulent: 90%, Temperate: 10%", "Virulent: 10%, Temperate: 90%", "Virulent: 95%, Temperate: 5%"),
        avg_genome_size = c(58000, 95000, 42000, 75000, 35000, 45000)
      ) %>%
        # Rename for UI display (English)
        dplyr::rename(
          `Predicted Host Taxonomy` = host_taxonomy,
          `Associated vOTU Count` = votu_count,
          `Lifestyle Distribution` = lifestyle_dist,
          `Avg. Genome Size (bp)` = avg_genome_size
        )
    })

    output$host_table <- DT::renderDataTable({
      shiny::req(mock_host_data())
      DT::datatable(
        mock_host_data(),
        rownames = FALSE,
        filter = 'top',
        extensions = c('Buttons', 'Scroller'),
        options = list(
          # English Language Options
          language = list(search = "Global Search:", lengthMenu = "Show _MENU_ entries per page",
                          zeroRecords = "No matching records found", info = "Showing _START_ to _END_ of _TOTAL_ entries",
                          infoEmpty = "Showing 0 to 0 of 0 entries", infoFiltered = "(filtered from _MAX_ total entries)",
                          paginate = list(first = "First", previous = "Previous", `next` = "Next", last = "Last")),
          pageLength = 10,
          lengthMenu = c(10, 20, 50, 100),
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'copy', text = 'Copy'),
            list(extend = 'csv', text = 'Export CSV', filename = 'avid_hosts_export'),
            list(extend = 'excel', text = 'Export Excel', filename = 'avid_hosts_export'),
            list(extend = 'colvis', text = 'Column Visibility')
          ),
          deferRender = TRUE,
          scrollY = 400,
          scrollCollapse = TRUE,
          scroller = TRUE
        )
      )
    })

  })
}

## To be copied in the UI
# mod_host_browser_ui("host_browser_1")

## To be copied in the server
# mod_host_browser_server("host_browser_1")
