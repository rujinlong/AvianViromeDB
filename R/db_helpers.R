#' Database Connection Helper Functions
#'
#' @description Functions to handle database connections and queries
#'
#' @import DBI
#' @import RSQLite
#' @importFrom golem get_golem_options
#' @noRd

#' Connect to the SQLite database
#'
#' @return A database connection object
#' @noRd
connect_to_db <- function() {
  db_path <- get_golem_options("db_path")

  if (is.null(db_path)) {
    # Default to app/data directory if not specified
    db_path <- app_sys("app/data/avian_virome.sqlite")
  }

  # Check if database exists
  if (!file.exists(db_path)) {
    stop("Database file not found at: ", db_path)
  }

  # Connect to the database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

#' Execute a query and return results
#'
#' @param query SQL query string
#' @param params Named list of parameters for parameterized queries
#'
#' @return Data frame with query results
#' @noRd
query_db <- function(query, params = NULL) {
  con <- connect_to_db()
  on.exit(DBI::dbDisconnect(con))

  if (is.null(params)) {
    result <- DBI::dbGetQuery(con, query)
  } else {
    result <- DBI::dbGetQuery(con, query, params)
  }

  return(result)
}

#' Get database summary statistics
#'
#' @return Named list with database statistics
#' @noRd
get_db_stats <- function() {
  # Connect to the database
  con <- connect_to_db()
  on.exit(DBI::dbDisconnect(con))

  # Query for total sequences
  total_seq <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS count FROM sequences")$count

  # Query for unique phage families
  families <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT family) AS count FROM taxonomy")$count

  # Query for host species
  hosts <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT host) AS count FROM hosts")$count

  # Query for sample sources
  samples <- DBI::dbGetQuery(con, "SELECT COUNT(DISTINCT source) AS count FROM samples")$count

  # Return as a list
  return(list(
    total_sequences = total_seq,
    phage_families = families,
    host_species = hosts,
    sample_sources = samples
  ))
}

#' Get sequences data for browse table
#'
#' @param limit Maximum number of rows to return
#' @param offset Offset for pagination
#'
#' @return Data frame with sequence information
#' @noRd
get_sequences <- function(limit = 100, offset = 0) {
  query <- paste0("
    SELECT
      s.sequence_id,
      s.length,
      t.family,
      h.host,
      sm.source,
      COUNT(g.gene_id) AS gene_count
    FROM sequences s
    LEFT JOIN taxonomy t ON s.sequence_id = t.sequence_id
    LEFT JOIN hosts h ON s.sequence_id = h.sequence_id
    LEFT JOIN samples sm ON s.sequence_id = sm.sequence_id
    LEFT JOIN genes g ON s.sequence_id = g.sequence_id
    GROUP BY s.sequence_id
    LIMIT ", limit, " OFFSET ", offset
  )

  result <- query_db(query)
  return(result)
}

#' Search database based on query and field
#'
#' @param search_query Search query string
#' @param search_field Field to search in
#'
#' @return Data frame with search results
#' @noRd
search_db <- function(search_query, search_field) {
  # Base query structure
  base_query <- "
    SELECT
      s.sequence_id,
      s.description,
      t.family,
      h.host,
      s.length
    FROM sequences s
    LEFT JOIN taxonomy t ON s.sequence_id = t.sequence_id
    LEFT JOIN hosts h ON s.sequence_id = h.sequence_id
    WHERE "

  # Modify WHERE clause based on search field
  where_clause <- switch(search_field,
    "Sequence ID" = "s.sequence_id LIKE ?",
    "Taxonomy" = "t.family LIKE ? OR t.genus LIKE ? OR t.species LIKE ?",
    "Host" = "h.host LIKE ?",
    "Sample Origin" = "sm.source LIKE ?",
    "Gene" = "EXISTS (SELECT 1 FROM genes g WHERE g.sequence_id = s.sequence_id AND g.gene_name LIKE ?)",
    "s.description LIKE ?" # Default case
  )

  # Complete the query
  query <- paste0(base_query, where_clause)

  # Prepare search parameter
  search_param <- paste0("%", search_query, "%")

  # Execute the query with the appropriate number of parameters
  if (search_field == "Taxonomy") {
    result <- query_db(query, list(search_param, search_param, search_param))
  } else {
    result <- query_db(query, list(search_param))
  }

  return(result)
}

#' Get taxonomy distribution data
#'
#' @return Data frame with taxonomy counts
#' @noRd
get_taxonomy_dist <- function() {
  query <- "
    SELECT
      family,
      COUNT(*) as count
    FROM taxonomy
    GROUP BY family
    ORDER BY count DESC
  "

  result <- query_db(query)
  return(result)
}

#' Get sequence length distribution data
#'
#' @return Data frame with sequence lengths
#' @noRd
get_length_dist <- function() {
  query <- "SELECT length FROM sequences"
  result <- query_db(query)
  return(result)
}

#' Get taxonomy hierarchy data for visualization
#'
#' @return Data frame with taxonomy hierarchy
#' @noRd
get_taxonomy_hierarchy <- function() {
  query <- "
    SELECT
      t.order,
      t.family,
      t.genus,
      COUNT(*) as count
    FROM taxonomy t
    GROUP BY t.order, t.family, t.genus
    ORDER BY t.order, t.family, t.genus
  "

  result <- query_db(query)
  return(result)
}
