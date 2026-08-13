# echsnd_files_process_02_id_vars.R ----

# AUTHOR: Dr Christopher Cesar
# Last edited: 11/08/2026

# AIM:  Further processing of copied candidate files

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

# for every file in the destination folder, we need to see what row the variable names start
# and extract the variable names for further comparison

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
  
  data %>%
    mutate(
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
