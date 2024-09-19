# subSeagrassCameraDataFormat.R ####
# Import and format camera data

# load packages ####
## use pacman package to install/import/load packages
ld_pkgs <- c("tidyverse", "tictoc","janitor")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

# load data ####
tictoc::tic.clearlog()
tic("Load & format camera survey analysis data")
df_camera <- readxl::read_excel("data/Subtidal seagrass drop camera data - MASTER_CLEANED.xlsm",
                                sheet="Clean_Trim",
                                guess_max = 20000
                                )
df_camera <- janitor::clean_names(df_camera)
toc(log=TRUE)

tic("Load & append WB name data")
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
           as.numeric(tmp$go_pro_image_date_time),
           origin = "1899-12-30")),
         year=lubridate::year(date),
         areal_cover=1,
         quadrat_number = go_pro_image_label,
         percentage_cover = zostera_percent_cover,
         # areal_cover to do!
         zostera_marina_present = "Yes",
         zostera_marina = 1,
         depth_cd = depth_cd
  ) %>%
  dplyr::select(wb_code, bed_name, year,areal_cover,
                quadrat_number,percentage_cover,
                zostera_marina_present,zostera_marina,depth_cd)

### TO DO ###
# * Generate data of extents by survey_year and survey_bed.
# * Append extent data to output data
