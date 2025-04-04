#' Help/Documentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 h2 h3 p strong hr code verbatimTextOutput tags # Removed ul, li from here
mod_help_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- English Translation ---
    shiny::h1("Help & Documentation"),
    shiny::hr(style = "border: 0; margin: 2rem 0;"),

    shiny::h2("About AVID"),
    shiny::p("The Avian Virome Database (AVID) is a comprehensive resource aimed at collecting, organizing, and analyzing global avian gut virome data. We integrate metagenomic sequencing data from public databases, covering various poultry and wild bird species."),

    shiny::h2("Database Construction Methods"),
    shiny::p("(Detailed description of data collection, quality control, metagenomic assembly, virus identification, dereplication, quality assessment, taxonomic annotation, host prediction, functional gene annotation, etc., should be provided here, potentially referencing your publication)"),
    shiny::h3("Key Steps & Tools:"),
    # Using shiny::tags$ul and shiny::tags$li directly
    shiny::tags$ul(
      shiny::tags$li(shiny::strong("Data Sources:"), " NCBI SRA, ENA, and other public databases"),
      shiny::tags$li(shiny::strong("QC Tool:"), " fastp"),
      shiny::tags$li(shiny::strong("Assembly Tool:"), " MEGAHIT"),
      shiny::tags$li(shiny::strong("Viral Identification:"), " VirSorter2, VIBRANT, GeNomad (Consensus)"),
      shiny::tags$li(shiny::strong("Quality Assessment:"), " CheckV"),
      shiny::tags$li(shiny::strong("Dereplication:"), " CheckV (95% ANI, 85% AF)"),
      shiny::tags$li(shiny::strong("Taxonomic Annotation:"), " VITAP, geNomad"),
      shiny::tags$li(shiny::strong("Host Prediction:"), " iPHoP"),
      shiny::tags$li(shiny::strong("Functional Annotation:"), " EggNOG-mapper, VIBRANT, DRAM-v, CARD, ResFinder, etc."),
      shiny::tags$li(shiny::strong("Lifestyle Prediction:"), " BACPHLIP")
    ),

    shiny::h2("How to Use This Website"),
    shiny::p("You can navigate to different data browsing pages using the top navigation bar:"),
    shiny::tags$ul(
      shiny::tags$li(shiny::strong("Browse vOTUs:"), " View all identified viral operational taxonomic units. The table supports per-column filtering and sorting."),
      shiny::tags$li(shiny::strong("Browse Functional Genes:"), " View predicted auxiliary metabolic genes (AMGs) and antibiotic resistance genes (ARGs). Filtering and sorting are also supported."),
      shiny::tags$li(shiny::strong("Browse Predicted Hosts:"), " View information on predicted viral bacterial hosts."),
      shiny::tags$li(shiny::strong("Downloads:"), " Access the complete annotation data (SQLite format) and vOTU sequences (FASTA format).")
    ),
    shiny::p(shiny::strong("Filtering Tip:"), " Type keywords into the input boxes located above each column header to filter the table content in real-time."),
    shiny::p(shiny::strong("Sorting Tip:"), " Click on column headers to sort the data in ascending or descending order."),

    shiny::h2("Data Citation"),
    shiny::p("If you use the AVID database in your research, please cite our publication:"),
    # Using verbatimTextOutput to display preformatted text
    shiny::verbatimTextOutput(ns("citation_text")),

    shiny::h2("Contact Us"),
    shiny::p("If you have any questions, suggestions, or find errors, please contact us:"),
    shiny::verbatimTextOutput(ns("contact_text")),
    shiny::hr(style = "border: 0; margin: 2rem 0;")
    # --- End English Translation ---
  )
}

#' Help/Documentation Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer renderPrint
mod_help_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Set citation and contact information (English) ---
    output$citation_text <- shiny::renderPrint({
      # Replace this with your actual publication citation
      cat("Please paste your publication citation here.\nExample:\nAuthor A, Author B, et al. (Year). Comprehensive Exploration of Avian Gut Virome Reveals Viral Diversity, Functional Insights, and Ecological Impacts. Journal Name, Volume(Issue), Pages. DOI: xxx")
    })

    output$contact_text <- shiny::renderPrint({
      # Replace this with your contact information
      cat("Please enter your contact email or lab website here.\nExample:\nyour.email@example.com\nhttps://your-lab-website.com")
    })

  })
}

## To be copied in the UI
# mod_help_ui("help_1")

## To be copied in the server
# mod_help_server("help_1")
