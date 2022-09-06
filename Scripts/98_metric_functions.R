# PROJECT:  i_need_to_use_the_facilites
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  metric functions
# REF ID:   a2738c1b 
# LICENSE:  MIT
# DATE:     2022-09-06
# UPDATED: 



# FUNCTION - AGGREGATE AGE ------------------------------------------------

agg_age <- function(df, total = TRUE){
  if(total == TRUE){
    df <- df %>% 
      dplyr::select(-trendscoarse) %>% 
      dplyr::group_by(dplyr::across(where(is.character))) %>% 
      dplyr::summarise(value = sum(value, na.rm = TRUE),
                       .groups = "drop")
  }
  
  return(df)
}

# FUNCTION - IDENTIFY DIRECTION -------------------------------------------

identify_direction <- function(df){
  df %>% 
    dplyr::group_by(orgunituid, mech_code) %>% 
    dplyr::mutate(metric_direction = dplyr::case_when(metric_value > lag(metric_value, order_by = period) ~ "increase",
                                                      metric_value < lag(metric_value, order_by = period) ~ "decrease")) %>% 
    dplyr::ungroup()
}

# FUNCTION - CALCULATE NET NEW --------------------------------------------

add_nn <- function(df){
  
  df_txcurr <- df %>% 
    filter(indicator == "TX_CURR")
  
  df_txcurr_complete <- df_txcurr %>% 
    tidyr::complete(period, nesting(orgunituid, mech_code), fill = list(value = 0)) %>% 
    dplyr::group_by(mech_code, orgunituid) %>% 
    tidyr::fill(operatingunit, country, psnu, orgunit, 
                funding_agency, mech_code, 
                trendscoarse, indicator, 
                .direction = "downup") %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(operatingunit, orgunituid, mech_code, period)
  
  #calc normal NET_NEW
  df_nn <- df_txcurr_complete %>%
    dplyr::group_by(orgunituid, mech_code) %>% 
    dplyr::mutate(indicator = "TX_NET_NEW",
                  value = value - dplyr::lag(value, order_by = period)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(!is.na(value))
  
  dplyr::bind_rows(df, df_nn)
}
