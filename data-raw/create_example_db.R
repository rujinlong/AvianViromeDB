# Script to create an example SQLite database for the Avian Virome Database

library(DBI)
library(RSQLite)
library(dplyr)

# Create database connection
db_path <- file.path("inst", "app", "data", "avian_virome.sqlite")
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

# Connect to SQLite database (will create it if it doesn't exist)
con <- dbConnect(SQLite(), db_path)

# Create tables
dbExecute(con, "
CREATE TABLE IF NOT EXISTS sequences (
  sequence_id TEXT PRIMARY KEY,
  description TEXT,
  length INTEGER,
  sequence TEXT
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS taxonomy (
  taxonomy_id INTEGER PRIMARY KEY AUTOINCREMENT,
  sequence_id TEXT,
  'order' TEXT,
  family TEXT,
  genus TEXT,
  species TEXT,
  FOREIGN KEY (sequence_id) REFERENCES sequences(sequence_id)
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS hosts (
  host_id INTEGER PRIMARY KEY AUTOINCREMENT,
  sequence_id TEXT,
  host TEXT,
  host_taxonomy TEXT,
  FOREIGN KEY (sequence_id) REFERENCES sequences(sequence_id)
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS samples (
  sample_id INTEGER PRIMARY KEY AUTOINCREMENT,
  sequence_id TEXT,
  source TEXT,
  location TEXT,
  collection_date TEXT,
  FOREIGN KEY (sequence_id) REFERENCES sequences(sequence_id)
);")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS genes (
  gene_id INTEGER PRIMARY KEY AUTOINCREMENT,
  sequence_id TEXT,
  gene_name TEXT,
  start_pos INTEGER,
  end_pos INTEGER,
  strand TEXT,
  product TEXT,
  func TEXT,
  FOREIGN KEY (sequence_id) REFERENCES sequences(sequence_id)
);")

# Generate synthetic data
set.seed(42) # For reproducibility

# Create sample data for sequences
n_sequences <- 200
sequences <- data.frame(
  sequence_id = paste0("AvianPhage_", sprintf("%03d", 1:n_sequences)),
  description = paste0("Avian bacteriophage isolate ", 1:n_sequences),
  length = sample(5000:50000, n_sequences, replace = TRUE),
  sequence = replicate(n_sequences, paste0(sample(c("A", "C", "G", "T"), 50, replace = TRUE), collapse = ""))
)

# Create sample data for taxonomy
orders <- c("Caudovirales", "Petitvirales")
families <- list(
  "Caudovirales" = c("Siphoviridae", "Myoviridae", "Podoviridae", "Autographiviridae", "Demerecviridae"),
  "Petitvirales" = c("Microviridae", "Inoviridae")
)
genera <- list(
  "Siphoviridae" = c("Lambda-like", "T5-like", "Phi29-like"),
  "Myoviridae" = c("T4-like", "P2-like", "Mu-like"),
  "Podoviridae" = c("P22-like", "N4-like"),
  "Autographiviridae" = c("SP6-like", "T7-like"),
  "Demerecviridae" = c("P1-like"),
  "Microviridae" = c("PhiX174-like"),
  "Inoviridae" = c("Fd-like")
)

taxonomy <- data.frame(
  sequence_id = sequences$sequence_id,
  order = sample(orders, n_sequences, replace = TRUE, prob = c(0.9, 0.1))
)
# Assign families based on order
taxonomy$family <- sapply(taxonomy$order, function(ord) {
  sample(families[[ord]], 1)
})
# Assign genera based on family
taxonomy$genus <- sapply(taxonomy$family, function(fam) {
  if (fam %in% names(genera)) {
    sample(genera[[fam]], 1)
  } else {
    "Unknown"
  }
})
taxonomy$species <- paste0(taxonomy$genus, " sp. ", sample(1:100, n_sequences, replace = TRUE))
taxonomy$taxonomy_id <- 1:n_sequences

# Create sample data for hosts
hosts <- data.frame(
  sequence_id = sequences$sequence_id,
  host = sample(c("Chicken", "Duck", "Turkey", "Goose", "Quail", "Pigeon", "Ostrich"), 
                n_sequences, replace = TRUE),
  host_taxonomy = rep("Aves", n_sequences)
)
hosts$host_id <- 1:n_sequences

# Create sample data for samples
samples <- data.frame(
  sequence_id = sequences$sequence_id,
  source = sample(c("Fecal", "Gut", "Respiratory", "Environmental", "Cloacal", "Oral"), 
                  n_sequences, replace = TRUE),
  location = sample(c("USA", "China", "Brazil", "Germany", "Australia", "South Africa", "Canada"), 
                    n_sequences, replace = TRUE),
  collection_date = sample(seq(as.Date("2010-01-01"), as.Date("2023-12-31"), by = "day"), 
                           n_sequences, replace = TRUE)
)
samples$sample_id <- 1:n_sequences

# Create sample data for genes
# Function to generate genes for a sequence
generate_genes <- function(seq_id, seq_length) {
  n_genes <- sample(5:20, 1)
  gene_positions <- sort(sample(1:(seq_length-500), n_genes*2, replace = FALSE))
  start_pos <- gene_positions[seq(1, length(gene_positions), 2)]
  end_pos <- pmin(start_pos + sample(300:2000, length(start_pos), replace = TRUE), seq_length)
  
  products <- c("capsid protein", "tail protein", "portal protein", "terminase", 
               "DNA polymerase", "helicase", "lysin", "holin", "recombinase", 
               "integrase", "hypothetical protein")
  
  data.frame(
    sequence_id = rep(seq_id, length(start_pos)),
    gene_name = paste0(gsub("AvianPhage_", "gene_", seq_id), "_", 1:length(start_pos)),
    start_pos = start_pos,
    end_pos = end_pos,
    strand = sample(c("+", "-"), length(start_pos), replace = TRUE),
    product = sample(products, length(start_pos), replace = TRUE),
    func = ifelse(runif(length(start_pos)) > 0.7, 
                  sample(c("structural", "replication", "packaging", "lysis", "lysogeny"), 
                         length(start_pos), replace = TRUE), 
                  "unknown")
  )
}

# Generate genes for all sequences
genes_list <- Map(generate_genes, sequences$sequence_id, sequences$length)
genes <- do.call(rbind, genes_list)
genes$gene_id <- 1:nrow(genes)

# Write data to database
dbWriteTable(con, "sequences", sequences, append = TRUE, row.names = FALSE)
dbWriteTable(con, "taxonomy", taxonomy, append = TRUE, row.names = FALSE)
dbWriteTable(con, "hosts", hosts, append = TRUE, row.names = FALSE)
dbWriteTable(con, "samples", samples, append = TRUE, row.names = FALSE)
dbWriteTable(con, "genes", genes, append = TRUE, row.names = FALSE)

# Create indices for better performance
dbExecute(con, "CREATE INDEX idx_taxonomy_sequence_id ON taxonomy(sequence_id)")
dbExecute(con, "CREATE INDEX idx_hosts_sequence_id ON hosts(sequence_id)")
dbExecute(con, "CREATE INDEX idx_samples_sequence_id ON samples(sequence_id)")
dbExecute(con, "CREATE INDEX idx_genes_sequence_id ON genes(sequence_id)")

# Close the connection
dbDisconnect(con)

cat("Example database created at:", db_path, "\n")
cat("Database contains:", n_sequences, "sequences with annotations\n") 