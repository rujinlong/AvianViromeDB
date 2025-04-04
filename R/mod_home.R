#' Home Page UI Function - Enhanced Landing Page Style
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h1 h2 p div strong fluidRow column hr HTML ul li icon tags actionButton
#' @importFrom bslib card card_header card_body value_box layout_column_wrap
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  # 创建一个白色图标函数
  white_icon <- function(name) {
    shiny::tags$div(
      class = "white-icon-container",
      shiny::tags$i(
        class = paste0("fa fa-", name),
        style = "color: #ffffff !important; font-size: 4rem; text-shadow: 0 1px 2px rgba(0,0,0,0.2);"
      )
    )
  }

  # 创建一个白色标题图标函数（稍小一些）
  white_header_icon <- function(name) {
    shiny::tags$i(
      class = paste0("fa fa-", name),
      style = "color: #ffffff !important; font-size: 1.2rem; text-shadow: 0 1px 1px rgba(0,0,0,0.15);"
    )
  }

  shiny::tagList(
    # --- Hero Section ---
    shiny::div(
      class = "hero-section",
      shiny::h1("Avian Virome Database (AVID)"),
      shiny::p(
        class = "lead",
        "Explore the first comprehensive catalog of the avian gut virome, revealing vast viral diversity, functional insights, and potential ecological impacts."
      ),
      # Optional: Add a button to guide users
      # shiny::actionButton(ns("explore_button"), "Start Exploring vOTUs", icon = icon("arrow-right"), class = "btn-primary btn-lg mt-3")
    ),

    # --- Key Statistics Section (Using Gradient Professional Palette with White Icons) ---
    shiny::h2("Database at a Glance", class="section-title"),
    bslib::layout_column_wrap(
      width = 1/3,
      fixed_width = TRUE,
      heights_equal = "row",
      gap = "1.5rem",

      bslib::value_box(
        title = "Samples Processed",
        value = "2,692",
        showcase = white_icon("database"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #1C4E80 0%, #2D6DA8 100%); color: white;"
      ),
      bslib::value_box(
        title = "vOTUs Identified",
        value = "61,608",
        showcase = white_icon("virus"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #488A99 0%, #5BA7B8 100%); color: white;"
      ),
      bslib::value_box(
        title = "Complete Genomes",
        value = "> 5,800",
        showcase = white_icon("check-circle"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #6AB187 0%, #7FCDA4 100%); color: white;"
      ),
      bslib::value_box(
        title = "Novelty (vs Public DBs)",
        value = "95.31%",
        showcase = white_icon("lightbulb"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #0091D5 0%, #37AEEF 100%); color: white;"
      ),
      bslib::value_box(
        title = "Auxiliary Metabolic Genes",
        value = "> 10,000",
        showcase = white_icon("gears"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #A5BE00 0%, #C4DC1A 100%); color: white;"
      ),
      bslib::value_box(
        title = "Potential New Lineages",
        value = "95%",
        showcase = white_icon("magnifying-glass-chart"),
        fill = TRUE,
        style = "background: linear-gradient(135deg, #7E909A 0%, #98ADB7 100%); color: white;"
      )
    ),

    shiny::hr(style="margin-top: 3rem; margin-bottom: 3rem;"), # Increased spacing

    # --- Getting Started Section (With White Icons) ---
    shiny::h2("Getting Started", class="section-title"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        bslib::card(
          class = "h-100", # Ensure cards in the same row have equal height
          bslib::card_header(
            shiny::strong(
              white_header_icon("compass"),
              " Navigate & Explore"
            ),
            style="background: linear-gradient(135deg, #1C4E80 0%, #2D6DA8 100%); color: white;"
          ),
          bslib::card_body(
            shiny::p("Use the top navigation bar to access different data views: vOTUs, Functional Genes (AMGs/ARGs), and Predicted Hosts.")
          )
        )
      ),
      shiny::column(
        width = 4,
        bslib::card(
          class = "h-100",
          bslib::card_header(
            shiny::strong(
              white_header_icon("table-list"),
              " Interact with Data"
            ),
            style="background: linear-gradient(135deg, #488A99 0%, #5BA7B8 100%); color: white;"
          ),
          bslib::card_body(
            shiny::p("Filter tables using the search boxes above columns, sort by clicking headers, and use the buttons for copying or exporting data.")
          )
        )
      ),
      shiny::column(
        width = 4,
        bslib::card(
          class = "h-100",
          bslib::card_header(
            shiny::strong(
              white_header_icon("download"),
              " Access Full Data"
            ),
            style="background: linear-gradient(135deg, #6AB187 0%, #7FCDA4 100%); color: white;"
          ),
          bslib::card_body(
            shiny::p("Download the complete annotation database (SQLite) and all vOTU sequences (FASTA) from the 'Downloads' page for your own analyses.")
          )
        )
      )
    ),

    shiny::hr(style="margin-top: 3rem; margin-bottom: 3rem;"),

    # --- Significance Section ---
    shiny::h2("Why AVID?", class="section-title"),
    shiny::fluidRow(
      shiny::column(
        width = 10, offset = 1, # Center the content slightly
        bslib::card(
          bslib::card_body(
            shiny::p(shiny::strong("Addressing a Critical Knowledge Gap:"), " The avian gut virome is significantly understudied compared to mammals. AVID provides the first large-scale, unified resource, cataloging tens of thousands of novel viral sequences."),
            shiny::p(shiny::strong("Understanding Host-Microbe Interactions:"), " Explore predicted virus-host relationships and the potential roles of viruses (via AMGs and ARGs) in shaping gut microbial communities and influencing host metabolism (e.g., carbohydrate breakdown)."),
            shiny::p(shiny::strong("Enabling Comparative Genomics:"), " Facilitates comparative studies across different bird species (poultry vs. wild), geographical locations, and potentially other animal viromes."),
            shiny::p(shiny::strong("Foundation for Future Research:"), " Offers a crucial dataset for investigating viral evolution, ecology, and potential applications in areas like poultry health management and disease diagnostics.")
          )
        )
      )
    )
  )
}

#' Home Page Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Server logic remains the same
  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
