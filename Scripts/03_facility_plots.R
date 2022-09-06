# PROJECT:  i_need_to_use_the_facilities
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  visualize data
# REF ID:   771a3d77 
# LICENSE:  MIT
# DATE:     2022-09-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(vroom)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "771a3d77"

  file_path <- return_latest("Dataout", "facility-metrics")
  
  file_path_datim <- return_latest("Data", "DATIM-API")
  
  api_source <- glue("{basename(file_path_datim) %>% str_sub(end = 6)} DATIM API [{file.info(file_path_datim)$ctime %>% format('%Y-%m-%d')}]")
  
# IMPORT ------------------------------------------------------------------
  
  df_metrics <- vroom(file_path, 
                      col_types = c(metric_value  = "d", 
                                    metric_value_psnu  = "d", 
                                    rel_size_qtile = "d", ovc_reported= "l",
                                    .default = "c"))

# MUNGE -------------------------------------------------------------------

  #order metrics for plot
  df_metrics <- df_metrics %>% 
    mutate(metric = recode(metric,
                           "Proxy Linkage" = "Proxy Linkage (%)",
                           "IIT" = "IIT (%)",
                           "VLC" = "VLC (%)",
                           "VLS" = "VLS (%)"),
           metric_value = ifelse(str_detect(metric, "\\%"), metric_value * 100, metric_value),
           metric_value_psnu = ifelse(str_detect(metric, "\\%"), metric_value_psnu * 100, metric_value_psnu),
           metric = fct_inorder(metric))
  
  unique(df_metrics$metric)
  set.seed(42)
  rand_site <- unique(df_metrics$orgunituid) %>% sample(1)

  site_sel <- df_metrics %>% 
    filter(orgunituid == rand_site) %>% 
    distinct(orgunit) %>% 
    pull(orgunit)
  
  site_meta <- df_metrics %>% 
    filter(orgunituid == rand_site,
           period == max(period)) %>% 
    distinct(country, psnu, period, funding_agency, mech_code)
  
  psnu_sel <- df_metrics %>% 
    filter(orgunituid == rand_site) %>% 
    distinct(psnu) %>% pull()
  
  
  df_site_direction <- df_metrics %>% 
    filter(period == max(period),
           orgunituid == rand_site) %>% 
    distinct(metric, metric_direction) %>% 
    mutate(metric_site_direction = case_when(metric_direction == "increase" ~ glue("{metric} \u25B2"),
                                             metric_direction == "decrease" ~ glue("{metric} \u25BC"),
                                             TRUE ~ glue("{metric} \u2012)"))) %>% 
    select(-metric_direction)
  

  df_curr_psnu <- df_metrics %>% 
    filter(psnu == psnu_sel,
           period == max(period)) %>% 
    left_join(df_site_direction, by = c("metric")) %>% 
    mutate(metric_site_direction = fct_inorder(metric_site_direction))
  
  v1 <- df_curr_psnu %>% 
    ggplot(aes(metric_value, metric_site_direction)) +
    # geom_boxplot(alpha = .2, color = "gray", outlier.size = 0) +
    geom_point(aes(size = rel_size_qtile), position = position_jitter(height = .5, width =  .01, seed = 42), 
               alpha = .6, color = trolley_grey, na.rm = TRUE) +
    geom_errorbar(aes(xmin = metric_value_psnu, xmax = metric_value_psnu), 
                  color = trolley_grey, size = 1.1, na.rm = TRUE) +
    geom_point(data = df_curr_psnu %>% filter(orgunituid == rand_site),
               aes(size = rel_size_qtile),
               # position = position_jitter(height = .5, seed = 42), 
               shape = 21, fill = scooter, color = "white", #size = 4, 
               alpha = .8, na.rm = TRUE) +
    facet_wrap(metric_site_direction ~ ., scale = "free", ncol = 1) +
    scale_x_continuous(label = comma) +
    labs(x = NULL, y = NULL,
         subtitle = glue("{site_sel} metric values compared against {site_meta$psnu} distribution in {site_meta$period}")
         ) +
    coord_cartesian(clip = "off") +
    si_style_ygrid() +
    theme(strip.text = element_blank(),
          # axis.text.y = element_markdown(),
          legend.position = "none",
          panel.spacing = unit(3, "picas")) 
  
  
  df_trend_site <- df_metrics %>% 
    filter(orgunituid == rand_site)

  v2 <- df_trend_site %>% 
    ggplot(aes(period, metric_value, group = metric)) +
    geom_path(color = trolley_grey) +
    geom_point(color = trolley_grey) +
    facet_wrap(metric ~ ., scale = "free", ncol = 1) +
    si_style_nolines() +
    labs(x = NULL, y = NULL,
         subtitle = glue("{site_sel} 6 period trend across metrics")) +
    coord_cartesian(clip = "off") +
    theme(axis.text = element_blank(),
          strip.text = element_blank(),
          legend.position = "none",
          panel.spacing = unit(3, "picas"))

  v1 + v2  +
    plot_layout(widths = c(2, 1)) +
    plot_annotation(title = glue("{toupper(site_sel)}"),
                    subtitle = glue("{site_meta$country} facility in {site_meta$psnu} supported by {site_meta$funding_agency} {site_meta$mech_code}"),
                    caption = glue("Note: Facilities are sized proportional to their quintile group by relevant indicator 
                        Source: {api_source}"),
                    theme = si_style())
  