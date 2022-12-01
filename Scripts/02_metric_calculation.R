# PROJECT:  i_need_to_use_the_facilities
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  metric calculations
# REF ID:   dc3e6ba7 
# LICENSE:  MIT
# DATE:     2022-09-06
# UPDATED:  2022-09-27

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(vroom)
  
  source("Scripts/98_metric_functions.R")  

# GLOBAL VARIABLES --------------------------------------------------------
  
  total <- TRUE  

# IMPORT ------------------------------------------------------------------
  
  df_datim <- return_latest("Data", "DATIM-API") %>% 
    vroom(col_types = c(value  = "d", .default = "c"))
  

# AGGREGATE MECH CODES TO SITE --------------------------------------------

  df_datim <- df_datim %>% 
    group_by(across(c(where(is.character), -mech_code))) %>% 
    summarise(mech_code = paste(mech_code, collapse = "|"),
              value = sum(value, na.rm = TRUE),
              .groups = "drop")
  
  #aggregate again so that multiple agencies are combined
  # df_datim <- df_datim %>% 
  #   unite(mech_code, c(funding_agency, mech_code), sep = ": ") %>% 
  #   group_by(across(c(where(is.character), -mech_code))) %>% 
  #   summarise(mech_code = paste(mech_code, collapse = "|"),
  #             value = sum(value, na.rm = TRUE),
  #             .groups = "drop")
  
# HTS ---------------------------------------------------------------------

  #filter to indicators needed for calculating test to pos ratio
  df_hts <- df_datim %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"))

  #append PSNU values
  df_hts <- append_psnu_total(df_hts)
  
  #aggregate ages (<15/15+)
  df_hts <- agg_age(df_hts, total) 

  #spread to calculate test to pos ratio
  df_hts_metric <- df_hts %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    mutate(metric = "Test to Positive Ratio",
           metric_value = round(hts_tst/hts_tst_pos),
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) 
  
  #separate psnu values
  df_hts_metric <- sep_psnu(df_hts_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_hts_metric <- identify_direction(df_hts_metric)
  
  #calculate the relative site size (for comparison)
  df_hts_size <- df_hts_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "HTS_TST",
           rel_size_qtile = ntile(hts_tst, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)

  #bind metric and relative size dfs
  df_hts_metric <- df_hts_metric %>% 
    left_join(df_hts_size, by = c("orgunituid", "mech_code")) %>% 
    select(-c(hts_tst, hts_tst_pos))
  
  rm(df_hts, df_hts_size)
  
# LINKAGE -----------------------------------------------------------------

  #filter to indicators needed for calculating proxy linkage
  df_link <- df_datim %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_NEW"))
  
  #append PSNU values
  df_link <- append_psnu_total(df_link)
  
  #aggregate ages (<15/15+)
  df_link <- agg_age(df_link, total)
  
  #spread to calculate test to proxy linkage
  df_link_metric <- df_link %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    mutate(metric = "Proxy Linkage",
           metric_value = tx_new/hts_tst_pos,
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) 
  
  #separate psnu values
  df_link_metric <- sep_psnu(df_link_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_link_metric <- identify_direction(df_link_metric)
  
  #calculate the relative site size (for comparison)
  df_link_size <- df_link_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "HTS_TST_POS",
           rel_size_qtile = ntile(hts_tst_pos, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)
  
  #bind metric and relative size dfs
  df_link_metric <- df_link_metric %>% 
    left_join(df_link_size, by = c("orgunituid", "mech_code")) %>% 
    select(-c(hts_tst_pos, tx_new))

  rm(df_link, df_link_size)
  
# TX GROWTH ---------------------------------------------------------------

  #filter to indicators needed for NN to curr ratio
  df_txgr <- df_datim %>% 
    filter(indicator == "TX_CURR")
  
  #calculate net new
  df_txgr <- add_nn(df_txgr)
  
  #append PSNU values
  df_txgr <- append_psnu_total(df_txgr)
  
  #aggregate ages (<15/15+)
  df_txgr <- agg_age(df_txgr, total)
  
  #spread to calculate test to  NN to curr ratio
  df_txgr_metric <- df_txgr %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    mutate(metric = "TX_NET_NEW to TX_CURR Ratio",
           metric_value = tx_net_new/tx_curr,
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) 
  
  #separate psnu values
  df_txgr_metric <- sep_psnu(df_txgr_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_txgr_metric <-identify_direction(df_txgr_metric)
  
  #calculate the relative site size (for comparison)
  df_txgr_size <- df_txgr_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "TX_CURR",
           rel_size_qtile = ntile(tx_curr, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)
  
  #bind metric and relative size dfs
  df_txgr_metric <- df_txgr_metric %>% 
    left_join(df_txgr_size, by = c("orgunituid", "mech_code")) %>% 
    select(-c(tx_curr, tx_net_new))

  rm(df_txgr, df_txgr_size)

# IIT ---------------------------------------------------------------------

  #filter to indicators needed for calculating IIT
  df_iit <- df_datim %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR","TX_NEW"))
  
  #append PSNU values
  df_iit <- append_psnu_total(df_iit)
  
  #aggregate ages (<15/15+)
  df_iit <- agg_age(df_iit, total)
  
  #spread to calculate test to proxy linkage
  df_iit_metric <- df_iit %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    group_by(orgunituid, mech_code) %>% 
    mutate(tx_curr_lag1 = lag(tx_curr, by = "period")) %>% 
    ungroup() %>% 
    mutate(metric = "IIT",
           metric_value = tx_ml / (tx_curr_lag1 + tx_new),
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) %>% 
    ungroup()
  
  #separate psnu values
  df_iit_metric <- sep_psnu(df_iit_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_iit_metric <- identify_direction(df_iit_metric)
  
  #calculate the relative site size (for comparison)
  df_iit_size <- df_iit_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "TX_CURR",
           rel_size_qtile = ntile(tx_curr, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)
  
  #bind metric and relative size dfs
  df_iit_metric <- df_iit_metric %>% 
    left_join(df_iit_size, by = c("orgunituid", "mech_code")) %>% 
    select(-c(tx_ml, tx_curr,tx_curr_lag1, tx_new))
  
  rm(df_iit, df_iit_size)

# VLC ---------------------------------------------------------------------

  #filter to indicators needed for calculating IIT
  df_vl <- df_datim %>% 
    filter(indicator %in% c("TX_CURR","TX_PVLS_D", "TX_PVLS"))
  
  #append PSNU values
  df_vl <- append_psnu_total(df_vl)
  
  #aggregate ages (<15/15+)
  df_vl <- agg_age(df_vl, total)
  
  #spread to calculate test to proxy linkage
  df_vlc_metric <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    group_by(orgunituid, mech_code) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = "period")) %>% 
    ungroup() %>% 
    mutate(metric = "VLC",
           metric_value = tx_pvls_d / tx_curr_lag2,
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) %>% 
    ungroup()
  
  #separate psnu values
  df_vlc_metric <- sep_psnu(df_vlc_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_vlc_metric <- identify_direction(df_vlc_metric)
  
  #calculate the relative site size (for comparison)
  df_vlc_size <- df_vlc_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "TX_CURR",
           rel_size_qtile = ntile(tx_curr, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)
  
  #bind metric and relative size dfs
  df_vlc_metric <- df_vlc_metric %>% 
    left_join(df_vlc_size, by = c("orgunituid", "mech_code")) %>%
    select(-c(tx_curr, tx_curr_lag2, tx_pvls_d, tx_pvls))
  
  rm(df_vlc_size)
  

# VLS ---------------------------------------------------------------------
  
  #spread to calculate VLS
  df_vls_metric <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}",
                values_fill = 0) %>%
    group_by(orgunituid, mech_code) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, by = "period")) %>% 
    ungroup() %>% 
    mutate(metric = "VLS",
           metric_value = tx_pvls / tx_pvls_d,
           metric_value = ifelse(is.infinite(metric_value), 0, metric_value)) %>% 
    ungroup()
  
  #separate psnu values
  df_vls_metric <- sep_psnu(df_vls_metric) 
  
  #calculate if metric increased/decreased from last quarter (note: no complete, may have missing quarters)
  df_vls_metric <- identify_direction(df_vls_metric)
  
  #calculate the relative site size (for comparison)
  df_vls_size <- df_vls_metric %>%
    filter(period == max(period)) %>% 
    group_by(operatingunit, country) %>% 
    mutate(rel_size_ind = "TX_CURR",
           rel_size_qtile = ntile(tx_curr, 5),
           rel_size_pd = period) %>% 
    ungroup() %>% 
    select(orgunituid, mech_code, rel_size_ind, rel_size_qtile, rel_size_pd)
  
  #bind metric and relative size dfs
  df_vls_metric <- df_vls_metric %>% 
    left_join(df_vls_size, by = c("orgunituid", "mech_code")) %>%
    select(-c(tx_curr, tx_curr_lag2, tx_pvls_d, tx_pvls))
  
  rm(df_vl, df_vls_size)

  

# OVC ---------------------------------------------------------------------

  last2_pd <- glamr::pepfar_data_calendar %>% 
    dplyr::filter(entry_close < Sys.Date(),
                  type == "initial") %>%
    dplyr::slice_tail(n = 2) %>% 
    dplyr::mutate(pd = glue("FY{str_sub(fiscal_year, -2)}Q{quarter}")) %>% 
    pull()
  
  ovc_org <- df_datim %>% 
    filter(period %in% last2_pd,
           indicator == "OVC_SERV",
           value > 0) %>% 
    distinct(orgunituid) %>% 
    pull()

# COMBINE -----------------------------------------------------------------

  df_metrics <- bind_rows(df_hts_metric,
                          df_link_metric,
                          df_txgr_metric,
                          df_iit_metric,
                          df_vlc_metric,
                          df_vls_metric)
  
  df_metrics <- df_metrics %>% 
    mutate(ovc_reported = orgunituid %in% ovc_org, .after = orgunit)

# EXPORT ------------------------------------------------------------------

  curr_pd <- glamr::pepfar_data_calendar %>% 
    dplyr::filter(entry_close < Sys.Date()) %>%
    dplyr::slice_tail() %>% 
    dplyr::mutate(pd = glue("FY{str_sub(fiscal_year, -2)}Q{quarter}{stringr::str_sub(type, end = 1)}")) %>% 
    pull()
  
  write_csv(df_metrics, glue("Dataout/{curr_pd}_facility-metrics.csv"))  
  