# PROJECT:  i_need_to_use_the_facilites
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  API pull of site level data
# REF ID:   8c5a97cb 
# LICENSE:  MIT
# DATE:     2022-09-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(grabr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()
  
  baseurl <- getOption("baseurl")
  username <- datim_user()
  password <- datim_pwd()

  ou_uid <- "f5RoebaDLMx"
  org_lvl <- 3

# LEG WORK ----------------------------------------------------------------

  # datim_dimensions() %>%
  #   arrange(dimension) %>%
  #   prinf()
  # 
  # datim_dim_items("Technical Area") %>% 
  #   arrange(item) %>% 
  #   prinf()
  # 
  # ind_uids <- datim_dim_items("Technical Area") %>% 
  #   arrange(item) %>% 
  #   filter(item %in% c("TX_NEW", "TX_CURR", 
  #                      "TX_ML", "TX_PVLS", 
  #                      "OVC_SERV")) %>% 
  #   pull(id) %>% 
  #   paste(collapse = ";")
  # 
  # disagg_uids <- datim_dim_items("Disaggregation Type") %>% 
  #   arrange(item) %>% 
  #   filter(item %in% c("Age/Sex/HIVStatus",
  #                      "Age Aggregated/Sex/HIVStatus",
  #                      "Age/Sex/Indication/HIVStatus",
  #                      "Age Aggregated/Sex/Indication/HIVStatus",
  #                      "Age/Sex/ARTNoContactReason/HIVStatus",
  #                      "Age/Sex/ProgramStatus",
  #                      "Age/Sex/ProgramStatusCaregiver",
  #                      "Age/Sex/DREAMS",
  #                      "Age/Sex/Preventive")) %>% 
  #   pull(id) %>% 
  #   paste(collapse = ";")
  # 
  # mods_uids <- datim_dim_items("HTS Modality (USE ONLY for FY22 Results/FY23 Targets)") %>% 
  #   pull(id) %>% 
  #   paste(collapse = ";")
  


# FUNCTION - BUILD URL ----------------------------------------------------


  build_url <- function(ou_uid, org_lvl, is_hts = FALSE){
    
    # cy_pd <- glamr::pepfar_data_calendar %>% 
    #   dplyr::filter(entry_close < Sys.Date(),
    #                 type == "initial") %>% 
    #   dplyr::slice_tail() %>% 
    #   mutate(fy = fiscal_year - 1) %>% 
    #   pull() %>% 
    #   paste0("Oct", collapse = ";")
    
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
    
# FUNCTION - DATIM API ----------------------------------------------------

  pull_results <- function(cntry, ou_uid, org_lvl){
    
    print(paste("Running DATIM API for", cntry,  Sys.time(),
                sep = " ... "))
    
    df_other <- build_url(ou_uid, org_lvl, is_hts = FALSE) %>% 
      grabr::datim_process_query()
    
    df_hts <- build_url(ou_uid, org_lvl, is_hts = TRUE) %>% 
      grabr::datim_process_query()
    
    df_full <- dplyr::bind_rows(df_other, df_hts)
  }
  
# IDENTIFY INPUTS FOR API -------------------------------------------------
  
  #country and level list
  ctry_list <- get_outable() %>% 
    select(country, country_uid, facility_lvl) %>% 
    filter(country == "Zambia")

# RUN API -----------------------------------------------------------------
  
  df_pull <- ctry_list %>%
    pmap_dfr(~pull_results(..1, ..2, ..3))
  
  
  df_pull <- df_pull %>% 
    dplyr::rename(orgunit = `Organisation unit`,
           funding_agency = `Funding Agency`,
           mech_code = `Funding Mechanism`,
           age_2019 = `Age: <15/15+  (Coarse)`,
           numerator_denom = `Numerator / Denominator`,
           indicator = `Technical Area`,
           # standardizeddisaggregate = `Disaggregation Type`,
           # modality = `HTS Modality (USE ONLY for FY22 Results/FY23 Targets)`,
           status_hiv = `HIV Test Status (Specific)`) %>%
    dplyr::rename_all(tolower)

  df_pull %>% 
    mutate(mech_code = stringr::str_extract(mech_code, "[:digit:]{5,}"),
           age_2019 = stringr::str_extract(age_2019, "(<15|15\\+)"),
           numerator_denom = stringr::str_sub(numerator_denom, end = 1),
           status_hiv = ifelse(stringr::str_detect(status_hiv, "Positive", "Positive", "Negative"))) %>% 
    glimpse()
  