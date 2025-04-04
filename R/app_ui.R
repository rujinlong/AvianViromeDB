#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom htmltools tags HTML
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # Apply a base theme using colors derived from the logo
      theme = bslib::bs_theme(
        version = 5,
        # bootswatch = "minty", # Can remove bootswatch if defining main colors
        primary = "#005f73",   # Darker teal from logo for better contrast
        secondary = "#6c757d", # Standard gray for secondary elements
        success = "#94d2bd",   # A lighter shade for success messages (derived)
        info = "#1d3557",      # Dark blue from logo for info elements
        warning = "#e9c46a",   # A complementary yellow/gold for warnings
        danger = "#e76f51",    # A complementary reddish-orange for danger
        bg = "#ffffff",        # White background
        fg = "#212529",        # Dark foreground text
        base_font = bslib::font_google("Inter"),
        heading_font = bslib::font_google("Inter") # Use same font for headings
      ),
      # Add custom CSS for further styling
      tags$head(
        tags$style(HTML(
          "
          /* Custom CSS for further enhancements */
          .navbar {
             box-shadow: 0 2px 4px rgba(0,0,0,.1);
          }
          /* Style navbar brand (where logo might go) */
          .navbar-brand {
             font-weight: bold;
             letter-spacing: 0.5px;
             padding-top: 0.5rem; /* Adjust padding if logo image is added */
             padding-bottom: 0.5rem;
          }
           /* Ensure logo image fits well if added via title */
          .navbar-brand img {
             max-height: 50px; /* Adjust max height of logo */
             margin-right: 0.5rem;
             vertical-align: middle; /* Align logo vertically */
          }

          .hero-section {
            /* Use a subtle gradient derived from logo's teal */
            background: linear-gradient(135deg, #e0f2f7 0%, #f8f9fa 100%); /* Light teal to light gray */
            padding: 5rem 1.5rem;
            margin-bottom: 3rem;
            border-radius: 0.5rem;
            text-align: center;
            border: 1px solid #dee2e6;
          }
          .hero-section h1 {
            font-size: 3rem;
            font-weight: 700;
            margin-bottom: 1rem;
            color: #1d3557; /* Use dark blue from logo */
          }
          .hero-section .lead {
            font-size: 1.3rem;
            font-weight: 300;
            max-width: 850px;
            margin-left: auto;
            margin-right: auto;
            color: #495057;
          }
          .section-title {
            text-align: center;
            margin-bottom: 2rem;
            font-weight: 600;
            color: #1d3557; /* Use dark blue for section titles */
          }
          .value-box {
             box-shadow: 0 4px 8px rgba(0,0,0,0.12);
             border-radius: 8px;
             border: none;
             transition: all 0.25s ease-in-out;
             overflow: hidden;
             position: relative;
          }
          .value-box:hover {
             transform: translateY(-5px);
             box-shadow: 0 8px 16px rgba(0,0,0,0.18);
          }
          /* Value box text styling */
          .value-box .value-box-title {
             font-weight: 500;
             letter-spacing: 0.5px;
             opacity: 0.95;
             margin-bottom: 0.3rem;
             font-size: 0.9rem;
             text-transform: uppercase;
          }
          .value-box .value-box-value {
             font-weight: 700;
             font-size: 2.1rem;
             letter-spacing: 0.5px;
             margin-top: 0.2rem;
          }

          /* --- Showcase icon color - Changed to solid white --- */
          .value-box .value-box-area .r-showcase {
            color: #ffffff !important;
            opacity: 0.85;
            filter: drop-shadow(1px 1px 1px rgba(0,0,0,0.2));
          }

          .card {
             box-shadow: 0 1px 3px rgba(0,0,0,.06);
             border: 1px solid #e9ecef;
             margin-bottom: 1.5rem;
          }
          .card-header {
             background-color: #f8f9fa;
             font-weight: 600;
             border-bottom: 1px solid #e9ecef;
          }
           .card-body li { margin-bottom: 0.6rem; }
           /* Use the new primary color for highlighted text */
           .card-body strong {
             color: #005f73; /* Use darker teal for strong text */
             font-weight: 600;
           }

          /* --- Footer Styling --- */
          .footer {
            background-color: #f8f9fa;
            color: #6c757d;
            padding: 1.5rem 0;
            margin-top: 2rem;
            border-top: 1px solid #dee2e6;
            text-align: center;
            font-size: 0.9em;
          }
          /* Link styling */
          a {
             color: #005f73; /* Darker teal for better readability and professionalism */
             text-decoration: none;
             transition: color 0.2s ease;
          }
          a:hover {
             color: #0a9396; /* Lighter teal on hover */
          }
          .footer a {
             color: #005f73; /* Consistent link color */
             text-decoration: none;
          }

          /* Remove problematic ::after pseudo-element */
          /*
          .value-box::after {
             content: '';
             position: absolute;
             top: 0;
             left: 0;
             width: 100%;
             height: 100%;
             background: linear-gradient(135deg, rgba(255,255,255,0.1) 0%, rgba(0,0,0,0.1) 100%);
             pointer-events: none;
          }
          */
        "
        ))
      ),

      # Wrap main content in a div with class "main-content"
      div(
        class = "main-content",
        # --- Navbar with Custom Theme derived from Logo ---
        navbarPage(
          # Apply a specific theme to the navbar only
          theme = bslib::bs_theme(
            version = 5,
            # Use dark teal/blue-green from logo for background
            bg = "#005f73",
            fg = "#ffffff", # White text
            primary = "#ffffff", # Make active link white
            base_font = bslib::font_google("Inter")
          ),
          # --- Display Logo and Text without Link ---
          # Use tags$span or tags$div instead of tags$a
          title = tags$span(class = "navbar-brand", # Apply class for styling
                            tags$img(src="www/logo2.png", height="50px", alt="AVID Logo", style="margin-top: -10px;margin-right: 5px;margin-bottom: -10px;width: 50px;"), # Logo image
                            "AVID Database" # Text next to logo
          ),
          # --- End Logo Display ---
          tabPanel("Home", icon = icon("home"), mod_home_ui("home_1")),
          tabPanel("Browse vOTUs", icon = icon("filter"), mod_votu_browser_ui("votu_browser_1")),
          tabPanel("Functional Genes", icon = icon("gears"), mod_functional_gene_browser_ui("functional_gene_browser_1")),
          tabPanel("Predicted Hosts", icon = icon("bacteria"), mod_host_browser_ui("host_browser_1")),
          tabPanel("Downloads", icon = icon("download"), mod_downloads_ui("downloads_1")),
          tabPanel("Help", icon = icon("circle-question"), mod_help_ui("help_1"))
        ), # End navbarPage

        # Before the footer
        shiny::hr(class = "footer-separator"),

        # Add the footer spacer before the footer
        footer_spacer(),

        # Footer
        tags$footer(
          class = "footer",
          div(
            class = "container",
            p(
              "Developed by ",
              tags$a(href = "https://your-lab-website.com", target = "_blank", "Your Lab Name"),
              " at Your Institution."
            ),
            p(
              paste0("© ", format(Sys.Date(), "%Y"), " AVID Database. All Rights Reserved.")
            )
          )
        ) # End Footer
      ) # End div with class "main-content"
    ) # End fluidPage
  ) # End tagList
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    # Custom favicon from www directory
    tags$link(rel = "icon", href = "www/favicon.ico", type = "image/x-icon"),

    # Include custom CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),

    # Include JavaScript files
    tags$script(src = "www/script.js"),
    tags$script(src = "www/handlers.js"),

    # Include other resources from bundle
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AVID Database"
    ),

    # Add other external CDN resources
    tags$link(rel = "stylesheet", type = "text/css", href = "//cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,600,700,300italic,400italic,600italic"),
    tags$link(rel = "stylesheet", type = "text/css", href = "//cdn.datatables.net/1.10.25/css/dataTables.bootstrap.min.css"),
  )
}
