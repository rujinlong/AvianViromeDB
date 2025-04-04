#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # mod_devboard_server("devboard_1")
  # mod_module1_server("module1_1")
  mod_home_server("home_1")
  mod_votu_browser_server("votu_browser_1")
  mod_functional_gene_browser_server("functional_gene_browser_1")
  mod_host_browser_server("host_browser_1")
  mod_downloads_server("downloads_1")
  mod_help_server("help_1")
}
