# turbidityDataFromWims.R ####
## extract turbidity and suspended solids data from WIMS

# steps:
# Bring up list of water bodies visited in the camera data
# Filter WIMS site IDs from each visited water body
# Generate vector of turbidity and suspended solids determinands
# Produce SQL query to extract these values from WIMS
# ...?
# Profit?

## load packages
ld_pkgs <- c("DBI", "odbc", "dplyr", "stringr", "glue", "lubridate",
             "tictoc", "arrow", "here", "readxl", "purrr", "beepr")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

# Data already loaded:
# 'tmp' = camera imagery data generated in 'subSeagrassCameraDataFormat.R'

# Bring up list of water bodies visited in the camera data####
tic("Bring up list of water bodies visited in the camera data")
readxl::read_excel("reference/all.saline.sites_WBs.xlsx",
                   sheet="SalineSitesJoined",
                   guess_max = 20000
                   ) -> tmp_sites_all
toc(log = TRUE)

#Filter WIMS site IDs from each visited water body####
tic("Filter WIMS site IDs from each visited water body")
tmp %>% dplyr::select(wbid) %>% distinct() -> tmp_wbs

tmp_sites_all %>% 
  filter(.,WBID %in% tmp_wbs$wbid) %>%
  dplyr::select(.,
                wims_region,
                SMPT_USER_REFERENCE,
                SMPT_SHORT_NAME,
                SMPT_EASTING,
                SMPT_NORTHING,
                WBID,
                WBName
                ) %>% 
  mutate(.,region = toupper(substr(.$wims_region,start=5,stop=6))) -> tmp_sites_filt

rm(tmp_wbs,tmp_sites_all)
toc(log=TRUE)

# Generate vector of turbidity and suspended solids determinands ####
tic("Generate vector of turbidity and suspended solids determinands")
## load code set
tmp_codes <- readxl::read_excel("reference/National Code Set.xlsx",
                                sheet = "DETS Codes",
                                skip = 2,
                                guess_max = 20000) %>% 
  set_names(c("det_code",
              "desc",
              "short_desc",
              "unit_code",
              "unit",
              "limit_low",
              "limit_upper")) %>% 
  filter(.,
         str_starts(desc, "Solids, Sus") | str_starts(desc, "Turbid"))
toc(log=TRUE)  

# Extract data from WIMS ####
tic("Extract data from WIMS")
source("R/functions/get_and_save1.R")
## define wims.regions for extract:
wims.regions <- tibble(db = c("wimsanpr","wimsmipr","wimsnepr","wimsnwpr","wimssopr", "wimsswpr", "wimsthpr"),
                       reg = c('ANGLIAN', 'MIDLANDS', 'NORTH EAST', 'NORTH WEST', 'SOUTHERN', 'SOUTH WEST' , 'THAMES'),
                       res = c('AN', 'MI', 'NE', 'NW', 'SO', 'SW', 'TH'))

# x <- get_and_save1(wimsdb = tmp_sites_filt$wims_region[1],
#                    site.list = tmp_sites_filt %>% filter(.,wims_region=="wimsanpr") %>% 
#                      dplyr::select(SMPT_USER_REFERENCE),
#                    det.list = unique(tmp_codes$det_code))

# Initialize an empty list to store results
all_results <- list()

# Loop through each DSN level in tmp_query$dsn
for (i in seq_along(levels(as.factor(tmp_sites_filt$wims_region)))) {
  
  # Extract the current DSN
  current_dsn <- levels(as.factor(tmp_sites_filt$wims_region))[i]
  
  # Extract the corresponding sites for the current DSN
  current_sites <- tmp_sites_filt$SMPT_USER_REFERENCE[tmp_sites_filt$wims_region == current_dsn]
  
  # Call the function for the current DSN with the appropriate sites and codes
  cat("Querying DSN:", current_dsn, "\n")
  result <- get_and_save1(
    wimsdb = current_dsn,      # DSN from tmp_query
    site.list = current_sites, # Sites for current DSN
    det.list = unique(tmp_codes$det_code)       # The same det_list for all DSNs
  )
  
  # Store the result in a list
  all_results[[current_dsn]] <- result
}

# If you want to combine all results into one data frame:
turb_ssol_extracts <- dplyr::bind_rows(all_results);rm(all_results)
toc(log=TRUE)
