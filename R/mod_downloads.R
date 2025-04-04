#' Downloads UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 p div strong downloadButton icon hr
mod_downloads_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- English Translation ---
    shiny::h1("Data Downloads"),
    shiny::p("You can download the complete AVID database datasets here for local analysis."),
    shiny::hr(),

    shiny::div(
      class = "card mb-3",
      shiny::div(
        class = "card-body",
        shiny::h2(shiny::icon("database"), " Annotation Data (SQLite)"),
        shiny::p("Contains detailed annotation information for all vOTUs, including taxonomy, predicted hosts, functional genes (AMGs/ARGs), genome quality, source information, etc. Recommended for use with DB Browser for SQLite or R/Python libraries."),
        shiny::downloadButton(ns("download_sqlite"), "Download SQLite File (Example)", class = "btn-primary"),
        shiny::p(class="text-muted small mt-2", "Note: Provides the complete annotation database file.")
      )
    ),

    shiny::div(
      class = "card",
      shiny::div(
        class = "card-body",
        shiny::h2(shiny::icon("dna"), " vOTU Genome Sequences (FASTA)"),
        shiny::p("Contains representative FASTA sequences for all vOTUs in the AVID database (typically compressed, e.g., .gz). Suitable for local BLAST searches or other sequence analyses."),
        shiny::downloadButton(ns("download_fasta"), "Download FASTA File (Placeholder)", class = "btn-primary"),
        shiny::p(class="text-muted small mt-2", "Note: Provides the compressed file of all representative vOTU sequences.")
      )
    ),
    shiny::hr(),
    shiny::p(shiny::strong("Please Note:"), " The files linked here need to be prepared and made available on the server side.")
    # --- End English Translation ---
  )
}

#' Downloads Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer downloadHandler
mod_downloads_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- SQLite Download Handler ---
    output$download_sqlite <- shiny::downloadHandler(
      filename = function() {
        # Define the downloaded filename
        "avid_annotations.sqlite.db"
      },
      content = function(file) {
        # Specify the actual path to your SQLite file here
        # This file needs to be accessible by the Shiny app.
        # e.g., place it in inst/extdata/ and use system.file()
        # sqlite_file_path <- system.file("extdata", "avid_annotations.sqlite.db", package = "YourGolemPackageName") # Replace YourGolemPackageName

        # --- Temporary Placeholder Logic ---
        # If the file doesn't exist or path isn't set, create a placeholder or show an error
        sqlite_file_path <- "path/to/your/avid_annotations.sqlite.db" # <--- !!! CHANGE THIS TO YOUR ACTUAL PATH !!!
        if (!file.exists(sqlite_file_path)) {
          # Create a small text file as a placeholder
          writeLines(c("SQLite database file not found at:", sqlite_file_path, "Please configure the correct path in mod_downloads_server."), file)
          warning(paste("SQLite file not found at:", sqlite_file_path))
        } else {
          # Copy the actual file to the temporary path provided by Shiny (file)
          file.copy(sqlite_file_path, file)
        }
        # --- End Placeholder Logic ---
      },
      contentType = "application/octet-stream" # Or "application/vnd.sqlite3"
    )

    # --- FASTA Download Handler ---
    output$download_fasta <- shiny::downloadHandler(
      filename = function() {
        # Define the downloaded filename
        "avid_votus.fasta.gz" # Assuming compressed
      },
      content = function(file) {
        # Specify the actual path to your FASTA file here
        # fasta_file_path <- system.file("extdata", "avid_votus.fasta.gz", package = "YourGolemPackageName") # Replace

        # --- Temporary Placeholder Logic ---
        fasta_file_path <- "path/to/your/avid_votus.fasta.gz" # <--- !!! CHANGE THIS TO YOUR ACTUAL PATH !!!
        if (!file.exists(fasta_file_path)) {
          writeLines(c(">Placeholder_Sequence", "ACGT"), file) # Create small placeholder FASTA
          warning(paste("FASTA file not found at:", fasta_file_path))
        } else {
          file.copy(fasta_file_path, file)
        }
        # --- End Placeholder Logic ---
      },
      contentType = "application/gzip" # If .gz file
      # contentType = "application/octet-stream" # If uncompressed .fasta
    )

  })
}

## To be copied in the UI
# mod_downloads_ui("downloads_1")

## To be copied in the server
# mod_downloads_server("downloads_1")
