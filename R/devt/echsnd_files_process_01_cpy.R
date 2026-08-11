# echsnd_files_process_01_cpy.R ----

# AUTHOR: Dr Christopher Cesar
# Last edited: 11/08/2026

# AIM:  Open candidate echosounder files identified in the script in
#       echsnd_files_find.R, copy files to local storage to speed up processing
#       time and allow for exploration to identify. Further investigation will
#       be carried out in echsnd_files_process_02_id_vars.R

# load packages & data ----
ld_pkgs <- c("tidyverse","tictoc","fs", "purrr","stringr", "progress")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)
tictoc::tic.clearlog() ##clear log

# load folder locations
source("R/folders.R")

# load candidate list
matching_files <- readRDS("data/possible_echosounder_files.Rdat")

dir.create("data/candidate_echsnd", showWarnings = FALSE)

# Copy files from network to local storage, maintaining directory structure
## This minimises the risks of duplicated file names (e.g. `Results.xlsx`)

# Identify duplicated filenames ----
file_names <- basename(matching_files)

# Number of duplicated files (excluding first occurrence)
sum(duplicated(file_names))

# Number of unique filenames that occur more than once
sum(table(file_names) > 1)

# Which filenames are duplicated?

duplicate_files <- tibble(
  file_name = basename(matching_files)
) |>
  count(file_name, sort = TRUE) |>
  filter(n > 1)

duplicate_files
# write.csv(duplicate_files,file="data/echsnd_duplicate_files.csv",row.names = FALSE)

## These duplicate files have been examined and a new spreadsheet has been
## created, with additional flag variables:
## Duplicate - Y OR N
## Action - For Duplicate = Y, identifies whether to Keep or Remove
## Year - extracted the survey year from the file path - possibly appending to file names?

## Aim is to keep all Duplicate = N files
## For when Duplicate = Y, keep those flagged with Action = Keep

file_xchk0 <- read.csv("data/possible_echosounder_files_kps.csv")

## filter to retain only those we want
file_xchk0 %>% 
  dplyr::filter(Action == "Keep") %>% 
  dplyr::select(value, Year)-> file_xchk

tictoc::tic("Copy files")
# Copy files we want ----

# Destination folder
dest_dir <- "data/candidate_echsnd"

# Create destination folder if required
dir.create(
  dest_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Construct destination file paths
dest_files <- file.path(
  dest_dir,
  paste0(
    file_xchk$Year,
    "_",
    basename(file_xchk$value)
    )
  )

# Optional safety check for duplicate destination names
stopifnot(!anyDuplicated(basename(dest_files)))

# Create progress bar
pb <- progress_bar$new(
  format = "Copying files [:bar] :percent | :current/:total files",
  total = nrow(file_xchk),
  clear = FALSE,
  width = 60
  )

# Copy files and track success
copy_success <- logical(length(dest_files))

for(i in seq_along(dest_files)) {
  
  copy_success[i] <- file.copy(
    from = file_xchk$value[i],
    to = dest_files[i],
    overwrite = FALSE
    )
  pb$tick()
  }

# Summary
cat(
  "\n",
  sum(copy_success), "files copied successfully\n",
  sum(!copy_success), "files failed to copy\n"
  )

# Review failures
failed_files <- file_xchk |>
  mutate(
    destination_file = dest_files,
    copied = copy_success
  ) |>
  filter(!copied)

failed_files

tictoc::toc(log=TRUE)

## Create lookup for candidate files:
dest_dir <- "data/candidate_echsnd"

file_lookup <- file_xchk |>
  transmute(
    original_filepath = value,
    original_filename = basename(value),
    renamed_filename = paste0(
      Year,
      "_",
      basename(value)
    ),
    destination_filepath = file.path(
      dest_dir,
      renamed_filename
    )
  )

write.csv(file_lookup, file = "data/possible_echosounder_files_lookup.csv",
          row.names = FALSE)
