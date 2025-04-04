# Avian Virome Database

This directory contains the SQLite database file for the Avian Virome Database application.

## Database Setup

The application expects a SQLite database file named `avian_virome.sqlite` in this directory. 

The database should have the following structure:

### Tables

1. **sequences** - Main table containing sequence information
   - sequence_id (PRIMARY KEY)
   - description
   - length
   - sequence (BLOB or TEXT)

2. **taxonomy** - Taxonomic classification of sequences
   - taxonomy_id (PRIMARY KEY)
   - sequence_id (FOREIGN KEY to sequences.sequence_id)
   - order
   - family
   - genus
   - species

3. **hosts** - Host organism information
   - host_id (PRIMARY KEY)
   - sequence_id (FOREIGN KEY to sequences.sequence_id)
   - host
   - host_taxonomy

4. **samples** - Information about sample sources
   - sample_id (PRIMARY KEY)
   - sequence_id (FOREIGN KEY to sequences.sequence_id)
   - source
   - location
   - collection_date

5. **genes** - Gene annotations
   - gene_id (PRIMARY KEY)
   - sequence_id (FOREIGN KEY to sequences.sequence_id)
   - gene_name
   - start_pos
   - end_pos
   - strand
   - product
   - func (functional category)

## Database Creation

You can create the database and import your data using the scripts in the `data-raw` directory of this project. Once created, place the database file in this directory.

## Example Data

For testing purposes, a small example database with synthetic data can be created using the `create_example_db.R` script in the `data-raw` directory. 