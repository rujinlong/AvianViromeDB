% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{df_to_sankey}
\alias{df_to_sankey}
\title{Convert a taxonomy data frame to a Sankey plot data frame}
\usage{
df_to_sankey(df_taxonomy, level_cols)
}
\arguments{
\item{df_taxonomy}{A data frame containing taxonomy information with
columns representing different taxonomic levels.}

\item{level_cols}{A character vector specifying the taxonomic levels
to include in the Sankey plot.}
}
\value{
A list containing two data frames:
  - `links`: A data frame with source and target columns for the Sankey plot.
  - `nodes`: A data frame with node names and group assignments for coloring nodes.
}
\description{
This function converts a taxonomy data frame into a Sankey plot data frame,
which can be used to create a Sankey plot using the `networkD3` package.
}
\examples{
\dontrun{
# Assuming 'df_taxonomy' is the taxonomy data frame
df_sankey <- df_to_sankey(df_taxonomy, c("realm", "kingdom", "phylum", "class", "order", "family", "genus", "species"))
}
}
