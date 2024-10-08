# subSeagrassCameraDataFormat.R ####
# Import and format camera data

# load packages ####
ld_pkgs <- c("tidyverse", "tictoc","janitor")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

# Load camera survey analysis data
tictoc::tic.clearlog()
tic("Load camera survey analysis data")
df_camera <- readxl::read_excel("data/Subtidal seagrass drop camera data - MASTER_CLEANED.xlsm",
                                sheet="Clean_Trim",
                                guess_max = 20000
                                )
df_camera <- janitor::clean_names(df_camera)
toc(log=TRUE)

tic("Append WB name data")
ref_wbs <- janitor::clean_names(as_tibble(read.csv("reference/wbs.csv", header=TRUE)))

df_camera %>% 
  mutate(.,waterbody = tolower(waterbody),
         waterbody = case_when(
         waterbody  == "dorset/hampshire" ~ "dorset / hampshire",
         waterbody  == "carrick road inner" ~ "carrick roads inner",
         waterbody  == "carrick road outer" ~ "carrick roads outer",
         waterbody  == "fal/helford" ~ "fal / helford",
         waterbody  == "torbay" ~ "tor bay",
         TRUE ~ waterbody)) %>% #keep other values
  filter(waterbody != "waterbody") %>% 
  left_join(ref_wbs %>%
              dplyr::select(wbid, wb_name) %>% 
              mutate(wb_name = tolower(wb_name)),
            by = c("waterbody" = "wb_name")) %>%
  relocate(wbid) %>% 
  mutate(wb_code = paste0(wbid,"_",bed_id),
         date=as_datetime(as_date(
           as.numeric(go_pro_image_date_time),
           origin = "1899-12-30")),
         year=lubridate::year(date),
         areal_cover=1,
         quadrat_number = go_pro_image_label,
         percentage_cover = zostera_percent_cover,
         # areal_cover to do!
         zostera_marina_present = "Yes",
         zostera_marina = 1,
         depth_cd = depth_cd
  ) %>% #names()
  dplyr::select(wbid,waterbody,wb_code, bed_name, year,areal_cover,
                quadrat_number,percentage_cover,
                zostera_marina_present, # not sure if actually required
                zostera_marina,depth_cd # not sure if actually required
                ) -> tmp
toc(log=TRUE)

# Extract all turbidity data from WIMS for WBs we have image data for####
tic("Extract all turbidity data from WIMS for WBs we have image data for")
source("R/devt/turbidityDataFromWims.R")
toc(log = TRUE)

### TO DO ###
# * Generate data of extents by survey_year and survey_bed.
# * Append extent data to output data
# * Extract & append turbidity data to image files (wait for Todd to respond to Nina
#   with update on whether turbidity measurements are actually being gathered during
#   camera surveys.  As seagrass beds aren't tied to WIMS sites, they are not analysed
#   through CLICK & not uploaded to WIMS)
# * Investigate potential for seagrass depths to be estimated using either:
#     - digital Admiralty charts; or
#     - DEM values generated by GEOMATICS
