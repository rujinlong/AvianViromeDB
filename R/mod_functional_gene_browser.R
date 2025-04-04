#' Functional Gene Browser UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 p fluidRow column strong icon tabsetPanel tabPanel textOutput # Removed card imports
#' @importFrom bslib card card_header card_body # Import bslib functions explicitly
#' @importFrom DT dataTableOutput
mod_functional_gene_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- English Translation ---
    shiny::h1("Browse Functional Genes (AMGs & ARGs)"),
    shiny::p("Explore Auxiliary Metabolic Genes (AMGs) and Antibiotic Resistance Genes (ARGs) identified within the avian virome."),

    # Functional category summaries
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header(shiny::strong(shiny::icon("gears"), " Auxiliary Metabolic Genes (AMGs)")),
          bslib::card_body(
            shiny::p("Viral-encoded genes that potentially influence host bacterial metabolism, providing advantages for viral replication or impacting ecosystem functions."),
            shiny::p(shiny::strong("Key Enriched Areas:"), " Nucleotide metabolism, amino acid metabolism, carbohydrate metabolism, etc."),
            shiny::p(shiny::strong("Total Distinct Types Found (Example):"), shiny::textOutput(ns("amg_count_summary"), inline = TRUE))
          )
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header(shiny::strong(shiny::icon("shield-halved"), " Antibiotic Resistance Genes (ARGs)")),
          bslib::card_body(
            shiny::p("Genes carried by viruses that may confer antibiotic resistance to host bacteria, potentially facilitating resistance spread."),
            shiny::p(shiny::strong("Examples Found:"), " Erm (MLSB resistance), Lnu (Lincosamide resistance), Aac (Aminoglycoside resistance), CfxA (β-lactam resistance)."),
            shiny::p(shiny::strong("Total Distinct Types Found (Example):"), shiny::textOutput(ns("arg_count_summary"), inline = TRUE))
          )
        )
      )
    ),

    # Tabset for switching between AMGs and ARGs
    shiny::tabsetPanel(
      id = ns("gene_tabs"),
      type = "tabs",
      shiny::tabPanel("AMGs", DT::dataTableOutput(ns("amg_table"))),
      shiny::tabPanel("ARGs", DT::dataTableOutput(ns("arg_table")))
    )
    # --- End English Translation ---
  )
}

#' Functional Gene Browser Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactiveVal observeEvent reactive req textOutput renderText
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr select rename count pull %>%
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @importFrom stringr str_pad
mod_functional_gene_browser_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Mock AMG Data ---
    # NOTE: In real application, read and integrate from anno_prot_eggnog, amg_dramv_filtered, amg_vibrant_filtered, etc.
    mock_amg_data <- shiny::reactive({
      # Simulate some AMG data
      amg_ids <- paste0("AMG_", stringr::str_pad(1:15, 3, pad = "0"))
      kos <- sample(paste0("K", stringr::str_pad(1:5000, 5, pad = "0")), 15, replace = TRUE)
      functions <- paste("Function description for", kos)
      pathways <- sample(c("Nucleotide metabolism", "Carbohydrate metabolism", "Amino acid metabolism", "Energy metabolism", "Glycolysis"), 15, replace = TRUE)
      # Simulate associated vOTU IDs (comma-separated)
      associated_votus <- purrr::map_chr(1:15, ~ paste(sample(paste0("vOTU_", stringr::str_pad(1:20, 4, pad = "0")), sample(1:5, 1)), collapse = ","))

      tibble::tibble(
        amg_id = amg_ids,
        gene_ko = kos,
        function_desc = functions,
        pathway = pathways,
        associated_votu_ids = associated_votus
      ) %>%
        # Rename for UI display (English)
        dplyr::rename(
          `AMG ID` = amg_id,
          `KO (KEGG)` = gene_ko,
          `Function Description` = function_desc,
          `Pathway` = pathway,
          `Associated vOTU IDs` = associated_votu_ids
        )
    })

    # --- Mock ARG Data ---
    # NOTE: In real application, organize based on external annotations (e.g., CARD, ResFinder) linked to contigs/proteins
    mock_arg_data <- shiny::reactive({
      tibble::tibble(
        arg_id = paste0("ARG_", stringr::str_pad(1:5, 3, pad = "0")),
        gene_name = c("Erm(X)", "Lnu(C)", "Aac(6')-Im", "CfxA4", "Erm(B)"),
        mechanism = c("Methylates 23S rRNA", "Modifies lincosamides", "Acetylates antibiotics", "Hydrolyzes cephalosporins", "Methylates 23S rRNA"),
        resistance_class = c("MLSB", "Lincosamides", "Aminoglycosides", "Cephalosporins", "MLSB"),
        associated_votu_ids = c("vOTU_0002", "vOTU_0002,vOTU_0006", "", "vOTU_0004", "vOTU_0001") # Example
      ) %>%
        # Rename for UI display (English)
        dplyr::rename(
          `ARG ID` = arg_id,
          `Gene Name` = gene_name,
          `Mechanism` = mechanism,
          `Resistance Class` = resistance_class,
          `Associated vOTU IDs` = associated_votu_ids
        )
    })

    # --- Render Tables ---
    output$amg_table <- DT::renderDataTable({
      shiny::req(mock_amg_data())
      DT::datatable(
        mock_amg_data(),
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
            list(extend = 'csv', text = 'Export CSV', filename = 'avid_amgs_export'),
            list(extend = 'excel', text = 'Export Excel', filename = 'avid_amgs_export'),
            list(extend = 'colvis', text = 'Column Visibility')
          ),
          deferRender = TRUE,
          scrollY = 400, # Adjust scroll height
          scrollCollapse = TRUE,
          scroller = TRUE
        )
      )
    })

    output$arg_table <- DT::renderDataTable({
      shiny::req(mock_arg_data())
      DT::datatable(
        mock_arg_data(),
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
            list(extend = 'csv', text = 'Export CSV', filename = 'avid_args_export'),
            list(extend = 'excel', text = 'Export Excel', filename = 'avid_args_export'),
            list(extend = 'colvis', text = 'Column Visibility')
          ),
          deferRender = TRUE,
          scrollY = 400, # Adjust scroll height
          scrollCollapse = TRUE,
          scroller = TRUE
        )
      )
    })

    # --- Update Summary Counts ---
    output$amg_count_summary <- shiny::renderText({
      nrow(mock_amg_data())
    })
    output$arg_count_summary <- shiny::renderText({
      nrow(mock_arg_data())
    })

  })
}

## To be copied in the UI
# mod_functional_gene_browser_ui("functional_gene_browser_1")

## To be copied in the server
# mod_functional_gene_browser_server("functional_gene_browser_1")
