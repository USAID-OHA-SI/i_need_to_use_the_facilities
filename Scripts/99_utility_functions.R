# PROJECT:  i_need_to_use_the_facilites
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  project utility functions
# REF ID:   38ecc09e 
# LICENSE:  MIT
# DATE:     2022-09-06
# UPDATED:  2022-09-27

# FUNCTION - BUILD URL ----------------------------------------------------

  build_url <- function(ou_uid, org_lvl, is_hts = FALSE, baseurl = "https://final.datim.org/"){
    
    prior_6pds <- glamr::pepfar_data_calendar %>% 
      dplyr::filter(entry_close < Sys.Date(),
                    type == "initial") %>%
      dplyr::slice_tail(n = 6) %>% 
      dplyr::mutate(cy_pd = lubridate::ymd(entry_open) - months(3),
                    cy_pd = cy_pd %>% 
                      lubridate::quarter(with_year = TRUE) %>% 
                      stringr::str_replace("\\.", "Q")) %>%
      dplyr::pull() %>% 
      paste0(collapse = ";")
    
    core_url <- paste0(baseurl,"api/29/analytics?",
                       "dimension=pe:", prior_6pds, "&", #period
                       "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
                       "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency - USAID
                       "dimension=SH885jaRe0o&", #Funding Mechanism
                       "dimension=xRo1uG2KJHk&", #Age: <15/15+ (Coarse)
                       "dimension=lD2x0c8kywj&", #Numerator/Denominator
                       "filter=IeMmjHyBUpi:Jh0jDM5yQ2E&" #Targets / Results - results
    )
    
    if(is_hts == TRUE){
      tech_url <-
        paste0(core_url,
               "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area
               "filter=bEktFhmEKn6&", #HTS Modality (USE ONLY for FY22 Results/FY23 Targets)
               "dimension=bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;mSBg9AZx1lV;viYXyEy7wKi&") #HIV Test Status (Specific)) - Pos/Neg + New Pos/Neg
    } else {
      tech_url <-
        paste0(core_url,
               "dimension=LxhLO68FcXm:RxyNwEV3oQf;MvszPTQrUhy;cSTYDtvP0Nt;udCop657yzi;bZOF8bon1dD&", #indicators
               "filter=HWPJnUTMjEq:h0pvSVe1TYf;k2U2MDOywGj;dV8zIx753Wf;cCTvqWoHxSk;pxz2gGSIQhG;PxGprLSHtqv;wqHjTjIvhlw;QG5SE83IVmL;AtKTQ515PWr&") #Disaggregation Type -> Age/Sex, Age/Sex/HIVStatus, Age Aggregated/Sex/HIVStatus
    }
    
    final_url <-
      paste0(tech_url,
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    return(final_url)
  }



# FUNCTION - CLEAN ORG LEVELS ---------------------------------------------

  org_clean <- function(df, psnu_lvl){
    df %>% 
      dplyr::rename(operatingunit = orglvl_3,
                    psnu = !!paste0("orglvl_", psnu_lvl)) %>% 
      dplyr::relocate(operatingunit, psnu, .before = `Organisation unit`) %>% 
      dplyr::mutate(country = ifelse(stringr::str_detect(operatingunit, "Region"), orglvl_4, operatingunit),
                    .after = operatingunit) %>% 
      dplyr::select(-dplyr::starts_with("orglvl_"))
  }      


# FUNCTION - CLEAN HTS ----------------------------------------------------

  clean_hts <- function(df){
    
    df_combo_hts <- df %>% 
      dplyr::filter(!`HIV Test Status (Specific)` %in%
                      c("Known at Entry Positive (Specific)",
                        "Recent Negatives (Specific)",
                        "HIV Status Unknown (Specific)",
                        NA)) 
    
    hts_ind <- unique(df_combo_hts$`Technical Area`)
    
    df_combo_hts <- df_combo_hts %>%
      dplyr::mutate(`Technical Area` = "HTS_TST")
    
    #create HTS_TST_POS
    df_hts_pos <- df_combo_hts %>%
      dplyr::filter(`HIV Test Status (Specific)` %in% c("HIV Positive (Specific)",
                                                        "Newly Tested Positives (Specific)")) %>%
      dplyr::mutate(`Technical Area` = "HTS_TST_POS") 
    
    df_combo_hts <- df_combo_hts %>% 
      dplyr::bind_rows(df_hts_pos) %>% 
      dplyr::select(-`HIV Test Status (Specific)`) %>% 
      dplyr::group_by(dplyr::across(where(is.character))) %>% 
      dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
    
    
    df %>% 
      dplyr::filter(`Technical Area` %ni% hts_ind) %>% 
      dplyr::bind_rows(df_combo_hts)
  }


# FUNCTION - DATIM API ----------------------------------------------------

  pull_results <- function(cntry, ou_uid, org_lvl, psnu_lvl){
    
    print(paste("Running DATIM API for", cntry,  Sys.time(),
                sep = " ... "))
    
    df_other <- build_url(ou_uid, org_lvl, is_hts = FALSE) %>% 
      grabr::datim_process_query()
    
    df_hts <- build_url(ou_uid, org_lvl, is_hts = TRUE) %>% 
      grabr::datim_process_query()
    
    df_full <- dplyr::bind_rows(df_other, df_hts)
    
    df_full <- org_clean(df_full, psnu_lvl)
    
  }


# FUNCTION - CLEAN DATIM PULL ---------------------------------------------

  clean_pull <- function(df){
    df_clean <- df %>% 
      clean_hts() %>% 
      select(-`HIV Test Status (Specific)`) %>% 
      dplyr::rename(orgunit = `Organisation unit`,
                    funding_agency = `Funding Agency`,
                    mech_code = `Funding Mechanism`,
                    trendscoarse = `Age: <15/15+  (Coarse)`,
                    numeratordenom = `Numerator / Denominator`,
                    indicator = `Technical Area`) %>%
      glamr::convert_datim_pd_to_qtr() %>% 
      dplyr::rename_all(tolower)
    
    df_clean <- df_clean %>% 
      mutate(mech_code = stringr::str_extract(mech_code, "[:digit:]{5,}"),
             trendscoarse = stringr::str_extract(trendscoarse, "(<15|15\\+)"),
             numeratordenom = stringr::str_sub(numeratordenom, end = 1)) %>% 
      gophr::clean_indicator() %>% 
      select(-numeratordenom) 
    
    return(df_clean)
  }
  

# FUNCTION - GET COORDINATES ----------------------------------------------

  pull_coords <- function(cntry, ou_uid, org_lvl, psnu_lvl, baseurl = "https://final.datim.org/"){
    
    print(paste("Running DATIM API for coordinates in", cntry,  Sys.time(),
                sep = " ... "))
    
    paste0(baseurl,
           "api/organisationUnits?filter=path:like:", ou_uid,
           "&filter=level:eq:", org_lvl, "&",
           "&fields=id,path,geometry&paging=false") %>%
      httr::GET(httr::authenticate(glamr::datim_user(),glamr::datim_pwd())) %>%
      httr::content("text") %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("organisationUnits") %>%
      tibble::as_tibble() %>%
      clean_coords(psnu_lvl)
  
  }  


# FUNCTION - CLEAN COORDINATES --------------------------------------------

  clean_coords <- function(df, psnu_lvl){
    
    #limit only to sites with coordinates
    # df <- dplyr::filter(df, geometry$type == "Point") 
    
    #if no sites, return null
    if(nrow(df) < 1)
      return(NULL)
    
    levels <- df$path %>%
      stringr::str_count("/") %>%
      max()
    
    #identify psnu
    df <- df %>% 
      dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
      tidyr::separate(path, paste0("orglvl_", seq(1:levels)), sep = "/", fill = "right") %>% 
      dplyr::select(orgunituid = id, geometry,
                    psnuuid = dplyr::ends_with(as.character(psnu_lvl)))
    
    #return uid + lat + long
    df <- df %>% 
      dplyr::mutate(coordinates = geometry$coordinates) %>% 
      dplyr::select(-geometry) %>% 
      tidyr::unnest_wider(coordinates, names_sep = "_") %>% 
      dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2") %>%
      dplyr::mutate_at(dplyr::vars("longitude", "latitude"), as.double)
    
    return(df)
  }
  
