# echsnd_files_00_find.R ----

# AUTHOR: Dr Christopher Cesar
# Last edited: 11/08/2026

# AIM:  Search the folders containing echosounder data analysis outputs &
#       identify CANDIDATE echosounder analysis files.  The list of files will
#       be further explored in the the script: echsnd_files_process.R

#   COPILOT PROMPT:
#   I want to use R to identify files located within multiple directories and
#    subdirectories.  There will be multiple files within these directories,
#    but I only want those based on the following criteria:
#   
#   
# 1.  All files must be Excel files (mostly .xlsx, but likely some .xls too)
# 2.  Within each file, there will be cells containing the following contents:
#      "From pos", "Center pos", "To pos", "% Ar.Inh."
# 3.  The above cell contents represent column headers in a data table.  These
#      are typically found at approximately row 20, but there is variability due
#      to the amount of metadata information in the rows above
# 4.  I want the output to be an object containing the complete file paths
#      for matching files
# 5.  The folders are likely to contain lots of files, so this needs to be
#      as computationally-efficient as possible

# SCRIPT EXPLAINER ------------------------------------------------------------
#
# PURPOSE
# This script performs the first stage of the echosounder file discovery
# workflow. It searches a specified directory tree for Excel workbooks that
# are likely to contain processed echosounder analysis outputs.
#
# APPROACH
# Rather than reading entire workbooks, the script scans a restricted range of
# rows and columns within each worksheet to improve performance when processing
# large numbers of files. Candidate files are identified by the presence of a
# set of expected column headers:
#
#   • From pos
#   • Center pos
#   • To pos
#   • % Ar.Inh.
#
# A workbook is considered a candidate if all required headers occur on the
# same row within at least one worksheet. This pattern is characteristic of
# echosounder analysis summary tables and is used as a proxy for identifying
# relevant files.
#
# PROCESS
#   1. Recursively locate all .xlsx and .xls files within the target folder.
#   2. Inspect each worksheet within each workbook.
#   3. Read only the specified cell range likely to contain table headers.
#   4. Test whether all required header fields occur on the same row.
#   5. Record the full file path for matching workbooks.
#
# OUTPUTS
# The script produces:
#   • matching_files: Character vector containing full paths to all candidate
#     echosounder files.
#   • possible_echosounder_files.Rdat: Serialized R object containing the file
#     list.
#   • possible_echosounder_files.csv: CSV export of the file list for review.
#
# NOTES
# This script is intentionally conservative and identifies candidate files
# based solely on header structure. Files identified here should be regarded
# as potential matches and undergo further validation in:
#
#   echsnd_files_process.R
#
# -----------------------------------------------------------------------------

# Load packages ----
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(tictoc)
library(progress)

tictoc::tic.clearlog()
tictoc::tic("Load folder info & define column headers")
# load data folder location ----
source("R/folders.R")

# Required column headers
required_headers <- c(
  "From pos",
  "Center pos",
  "To pos",
  "% Ar.Inh."
)

# Check whether all required headers occur on the same row
header_row_match <- function(dat, required_headers) {
  
  apply(dat, 1, function(x) {
    
    vals <- as.character(x)
    
    positions <- match(required_headers, vals)
    
    all(!is.na(positions))
    
  }) |>
    any()
  }

find_matching_excel_files <- function(
    root_dir,
    start_row = 10,
    end_row = 35,
    start_col = 1,
    end_col = 30
) {
  
  # Find Excel files
  excel_files <- list.files(
    path = root_dir,
    pattern = "\\.(xlsx|xls)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  n_files <- length(excel_files)
  
  # Progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent | File :current/:total | Matches: :matches",
    total = n_files,
    clear = FALSE,
    width = 80
  )
  
  matching_files <- character(0)
  n_matches <- 0
  
  for (file in excel_files) {
    
    file_matches <- FALSE
    
    sheets <- tryCatch(
      readxl::excel_sheets(file),
      error = function(e) NULL
    )
    
    if (!is.null(sheets)) {
      
      for (sheet in sheets) {
        
        dat <- tryCatch(
          readxl::read_excel(
            path = file,
            sheet = sheet,
            range = readxl::cell_limits(
              c(start_row, start_col),
              c(end_row, end_col)
            ),
            col_names = FALSE
          ),
          error = function(e) NULL
        )
        
        if (is.null(dat)) {
          next
        }
        
        if (header_row_match(dat, required_headers)) {
          
          matching_files <- c(matching_files, file)
          
          n_matches <- n_matches + 1
          file_matches <- TRUE
          
          # No need to check remaining sheets
          break
        }
      }
    }
    
    pb$tick(tokens = list(matches = n_matches))
  }
  
  message(
    "\nFinished. ",
    n_matches,
    " matching files found from ",
    n_files,
    " Excel files scanned."
  )
  
  matching_files
}


# Run function to identify candidate files ----
tictoc::tic("Identify candidate files")
matching_files <- find_matching_excel_files(
  root_dir = datfol,
  start_row = 17,
  end_row = 25,
  start_col = 5,
  end_col = 20
  )
tictoc::toc(log=TRUE)

saveRDS(matching_files, file = "data/possible_echosounder_files.Rdat")

write.csv(as_tibble(matching_files), file = "data/possible_echosounder_files.csv",
          row.names = FALSE)
