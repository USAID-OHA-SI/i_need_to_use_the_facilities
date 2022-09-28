# PROJECT:  i_need_to_use_the_facilities
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  API pull of site level data
# REF ID:   8c5a97cb 
# LICENSE:  MIT
# DATE:     2022-09-01
# UPDATED:  2022-09-28

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(grabr)
  library(glue)
  library(googledrive)
  library(lubridate)

  source("Scripts/99_utility_functions.R")  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()
  

# DOWNLOAD VCPOLYGONS -----------------------------------------------------

  #shapefile for all PEPFAR countries and PSNUs
  # df_zip <- drive_ls(as_id("1cvNxiRxl-g3qEP_ON8FT_FxmHsHblDuz"), 
  #                    "VcPepfarPolygons") %>%
  #   mutate(created_time = map_chr(drive_resource, "createdTime") %>%
  #            ymd_hms(tz = "EST")) %>%
  #   slice_max(order_by = created_time, n = 1)
  # 
  # drive_download(df_zip, 
  #                file.path(si_path("path_vector"),df_zip$name), 
  #                overwrite = TRUE) #set_paths(folderpath_vector = "..")
  # 
  # unzip(file.path(si_path("path_vector"),df_zip$name), 
  #       exdir = si_path("path_vector"))
    
# IDENTIFY INPUTS FOR API -------------------------------------------------
  
  #country and level list
  ctry_list <- get_outable() %>% 
    select(country, country_uid, facility_lvl, psnu_lvl) %>% 
    filter(country == "Zambia")

# RUN API -----------------------------------------------------------------
  
  #pull data
  df_pull <- ctry_list %>%
    pmap_dfr(~pull_results(..1, ..2, ..3, ..4))
  
  #pull site coordinates
  df_coords <- ctry_list %>%
    pmap_dfr(~pull_coords(..1, ..2, ..3, ..4))

# MUNGE -------------------------------------------------------------------

  df_pull <- clean_pull(df_pull)
  
# EXPORT ------------------------------------------------------------------

  curr_pd <- glamr::pepfar_data_calendar %>% 
    dplyr::filter(entry_close < Sys.Date()) %>%
    dplyr::slice_tail() %>% 
    dplyr::mutate(pd = glue("FY{str_sub(fiscal_year, -2)}Q{quarter}{stringr::str_sub(type, end = 1)}")) %>% 
    pull()
  
  write_csv(df_pull, glue("Data/{curr_pd}_DATIM-API.csv"))
  write_csv(df_coords, glue("Data/{curr_pd}_DATIM-coordinates.csv"))
 
  