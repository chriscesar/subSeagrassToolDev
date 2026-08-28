# echsnd_files_02_process_id_vars.R ----

# AUTHOR: Dr Christopher Cesar
# Last edited: 11/08/2026

# AIM:  Further processing of copied candidate files

# SCRIPT EXPLAINER ------------------------------------------------------------
#
# PURPOSE
# This script performs the final identification and extraction stage of the
# echosounder file processing workflow. It processes candidate Excel files
# identified by earlier scripts, locates the true table header row within each
# workbook, extracts the associated data tables, and combines all records into
# a single standardised dataset.
#
# CONTEXT
# Candidate files have already been identified and copied into a dedicated
# working directory. However, variation in workbook formatting means the table
# header row may not occur at a consistent position across files. This script
# determines the correct header row for each workbook before extracting data.
#
# APPROACH
# Files are validated by searching for a set of expected echosounder table
# headers:
#
#   • From pos
#   • Center pos
#   • To pos
#   • % Ar.Inh.
#
# The first row containing all required headers is assumed to represent the
# start of the data table. Once identified, the script reconstructs column
# names, extracts the tabular data beneath the header, and applies consistent
# variable naming conventions across all files.
#
# PROCESS
#   1. Load candidate workbook metadata and file look-up information.
#   2. Search each workbook for the row containing the required headers.
#   3. Record the detected header row and flag valid files.
#   4. Reconstruct column names using the two-row header structure commonly
#      used in echosounder output files.
#   5. Extract data records below the header rows.
#   6. Stop extraction at the first completely empty row.
#   7. Standardise column names using janitor::make_clean_names().
#   8. Append source file metadata to each record.
#   9. Combine data from all valid files into a single dataset.
#
# OUTPUTS
# The script produces:
#
#   • combined_data
#       Combined dataset containing records extracted from all valid
#       echosounder workbooks.
#
#   • candidate_echsnd_combined.csv
#       CSV export of the combined dataset for subsequent analysis and QA.
#
# QUALITY ASSURANCE
# Additional (currently disabled) audit code is included to:
#
#   • Compare column names across workbooks.
#   • Identify files with inconsistent variable structures.
#   • Detect unexpected or missing variables.
#   • Assess whether all files conform to a common schema.
#
# These checks were used during development to verify consistency in file
# structure and can be re-enabled if further validation is required.
#
# WORKFLOW POSITION
# This script represents the final stage of the echosounder file discovery and
# extraction process:
#
#   echsnd_files_00_find.R
#       Identify candidate files based on expected header patterns.
#
#   echsnd_files_01_copy.R
#       Copy candidate files into a dedicated processing workspace.
#
#   echsnd_files_02_process_id_vars.R
#       Validate file structure, extract data, standardise variables, and
#       create a consolidated dataset.
#
# -----------------------------------------------------------------------------

# load packages & data ----
ld_pkgs <- c("tidyverse","tictoc","readxl","dplyr","purrr","tidyr",
             "stringr","janitor")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)
tictoc::tic.clearlog() ##clear log

# load folder locations
source("R/folders.R")
destdir <- "data/candidate_echsnd/"

file_lookup <- read.csv("data/possible_echosounder_files_lookup.csv")

# for every file in the destination folder, we need to see what row the
# variable names start and extract the variable names for further comparison

tictoc::tic("Define function for identifying starter row")

# Headers that must all be present on the same row
required_headers <- c(
  "From pos",
  "Center pos",
  "To pos",
  "% Ar.Inh."
)

find_header_row <- function(file,
                            required_headers,
                            n_min = 17,
                            n_max = 25) {
  
  # Read enough rows to cover the search range
  preview <- read_excel(
    file,
    sheet = 1,
    col_names = FALSE,
    n_max = n_max
  )
  
  # Ensure we don't check beyond the number of rows read
  rows_to_check <- seq(
    from = n_min,
    to = min(n_max, nrow(preview))
  )
  
  header_row <- NA_integer_
  
  for (i in rows_to_check) {
    
    row_values <- preview[i, ] %>%
      unlist(use.names = FALSE) %>%
      as.character() %>%
      str_squish()
    
    # Case-insensitive matching
    row_values <- tolower(row_values)
    headers <- tolower(required_headers)
    
    if (all(headers %in% row_values)) {
      header_row <- i
      break
    }
  }
  
  tibble(
    file_name = basename(file),
    header_row = header_row,
    headers_found = !is.na(header_row)
  )
}

# Get all xlsx files
files <- list.files(
  path = destdir,
  pattern = "\\.xlsx$",
  full.names = TRUE
  )
tictoc::toc(log=TRUE)

tictoc::tic("Check all files")
# Check all files
results <- map_dfr(
  files,
  find_header_row,
  required_headers = required_headers,
  n_min = 17,
  n_max = 25
  )

results
table(results$headers_found)
tictoc::toc(log=TRUE)

tictoc::tic("Extract data")
# Extract data ----
results_enriched <- results %>%
  left_join(
    file_lookup,
    by = c("file_name" = "renamed_filename")
  )

extract_file_data <- function(file,
                              header_row,
                              original_filepath = NA_character_,
                              original_filename = NA_character_) {
  
  x <- read_excel(
    file,
    sheet = 1,
    col_names = FALSE
  )
  
  # Build column names
  header1 <- x[header_row, ] |> unlist() |> as.character()
  header2 <- x[header_row + 1, ] |> unlist() |> as.character()
  
  col_names <- paste(
    dplyr::coalesce(header1, ""),
    dplyr::coalesce(header2, "")
  ) |>
    stringr::str_squish()
  
  col_names[col_names == ""] <- paste0(
    "unnamed_",
    seq_len(sum(col_names == ""))
  )
  
  data_start <- header_row + 3
  
  data <- x[data_start:nrow(x), ]
  
  # Stop at first fully empty row
  empty_rows <- apply(
    data,
    1,
    function(r)
      all(is.na(r) | stringr::str_squish(as.character(r)) == "")
  )
  
  if (any(empty_rows)) {
    data <- data[seq_len(which(empty_rows)[1] - 1), ]
  }
  
  names(data) <- janitor::make_clean_names(col_names)
  
  # Duplicate filename value down all rows
  if ("file_name" %in% names(data)) {
    
    first_file_name <- data$file_name[
      which(
        !is.na(data$file_name) &
          stringr::str_squish(as.character(data$file_name)) != ""
      )[1]
    ]
    
    data <- data %>%
      dplyr::mutate(
        file_name = first_file_name
      )
  }
  
  data %>%
    dplyr::mutate(
      excel_file = basename(file),
      original_filename = original_filename,
      original_filepath = original_filepath,
      .before = 1
    )
}

valid_results <- results_enriched %>%
  filter(headers_found)

combined_data <- purrr::pmap_dfr(
  list(
    file = file.path(destdir, valid_results$file_name),
    header_row = valid_results$header_row,
    original_filepath = valid_results$original_filepath,
    original_filename = valid_results$original_filename
    ),
  extract_file_data
  )

write.csv(combined_data,file = "data/candidate_echsnd_combined.csv",
          row.names = FALSE)
tictoc::toc(log=TRUE)

# Audit column name consistency ----
get_column_names <- function(file, header_row) {
  
  x <- readxl::read_excel(
    file,
    sheet = 1,
    col_names = FALSE
  )
  
  header1 <- x[header_row, ] |> unlist() |> as.character()
  header2 <- x[header_row + 1, ] |> unlist() |> as.character()
  
  col_names <- paste(
    dplyr::coalesce(header1, ""),
    dplyr::coalesce(header2, "")
  ) |>
    stringr::str_squish() |>
    janitor::make_clean_names()
  
  tibble::tibble(
    column_name = col_names
  )
}

## build an audit table ----
# Aim to ensure all files have consistent variable names
# header_audit <- purrr::map2_dfr(
#   file.path(destdir, valid_results$file_name),
#   valid_results$header_row,
#   ~ get_column_names(.x, .y) |>
#     dplyr::mutate(file_name = basename(.x))
#   )
# 
## Identify columns that don't occur in every file ----
# column_frequency <- header_audit %>%
#   count(column_name, sort = TRUE) %>% 
#   filter(n < n_distinct(header_audit$file_name))
# 
## Find files with unique column structures ----
# ## Create a signature for each file
# file_structures <- header_audit %>%
#   arrange(file_name, column_name) %>%
#   group_by(file_name) %>%
#   summarise(
#     structure = paste(column_name, collapse = "|"),
#     .groups = "drop"
#   )
# 
# # Count unique structures
# file_structures %>%
#   count(structure, sort = TRUE)
# 
## Compare against a reference file ----
# ## Use first file as the standard
# reference_cols <- combined_data %>%
#   names()
# 
# ## Find files containing unexpected columns
# header_audit %>%
#   count(file_name) %>%
#   arrange(n) %>% View()
# 
# ^ Checked and cleared

# Tidy up

rm(combined_data,file_lookup,file_structures,header_audit,
   results, results_enriched, valid_results,
   datfol, destdir, files, reference_cols, column_frequency,
   required_headers,
   extract_file_data, find_header_row, get_column_names,
   )

ld_pkgs <- c("tidyverse","tictoc","readxl","dplyr","purrr","tidyr",
             "stringr","janitor")

detach("package:tictoc", unload = TRUE)
detach("package:readxl", unload = TRUE)
detach("package:tidyr", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:janitor", unload = TRUE)
detach("package:stringr", unload = TRUE)
detach("package:purrr", unload = TRUE)
detach("package:tidyverse", unload = TRUE)
