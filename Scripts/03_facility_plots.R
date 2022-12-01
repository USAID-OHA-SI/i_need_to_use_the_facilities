# PROJECT:  i_need_to_use_the_facilities
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  visualize data
# REF ID:   771a3d77 
# LICENSE:  MIT
# DATE:     2022-09-06
# UPDATED:  2022-09-27

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(grabr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(vroom)
  library(gisr)
  library(sf)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "771a3d77"

  file_path <- return_latest("Dataout", "facility-metrics")
  file_path_datim <- return_latest("Data", "DATIM-API")
  file_path_coords <- return_latest("Data", "DATIM-coordinates")
  
  
  api_source <- glue("{basename(file_path_datim) %>% str_sub(end = 6)} DATIM API [{file.info(file_path_datim)$ctime %>% format('%Y-%m-%d')}]")
  
# IMPORT ------------------------------------------------------------------
  
  df_metrics <- vroom(file_path, 
                      col_types = c(metric_value  = "d", 
                                    metric_value_psnu  = "d", 
                                    rel_size_qtile = "d",
                                    ovc_reported= "l",
                                    .default = "c"))

  df_coords <- vroom(file_path_coords,
                     col_types = c(latitude = "d",
                                   longitude = "d",
                                   .default = "c"))
  
  spdf_pepfar <- get_vcpolygons() 
  
# MUNGE -------------------------------------------------------------------

  #bind coordinates
  df_metrics <- df_metrics %>%
    left_join(df_coords, by = "orgunituid") %>%
    mutate(has_coords = !is.na(latitude)) %>%
    relocate(psnuuid, .after = psnu) %>% 
    relocate(has_coords, latitude, longitude, .after = orgunit)
  
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
  
  set.seed(42)
  site_sel <- unique(df_metrics$orgunituid) %>% sample(1)

  site_meta <- df_metrics %>% 
    filter(orgunituid == site_sel,
           period == max(period)) %>% 
    distinct(country, psnu, psnuuid, orgunit, orgunituid,
             period, funding_agency, mech_code, ovc_reported,
             has_coords, latitude, longitude)
  
  df_site_direction <- df_metrics %>% 
    filter(period == site_meta$period,
           orgunituid == site_meta$orgunituid) %>% 
    distinct(metric, metric_direction) %>% 
    mutate(metric_site_direction = case_when(metric_direction == "increase" ~ glue("{metric} \u25B2"),
                                             metric_direction == "decrease" ~ glue("{metric} \u25BC"),
                                             TRUE ~ glue("{metric} \u2012)"))) %>% 
    select(-metric_direction)
  

  df_curr_psnu <- df_metrics %>% 
    filter(psnuuid == site_meta$psnuuid,
           period == site_meta$period) %>% 
    left_join(df_site_direction, by = c("metric")) %>% 
    mutate(metric_site_direction = fct_inorder(metric_site_direction))
  
  spdf_psnu <- spdf_pepfar %>% 
    extract_boundaries(country = site_meta$country, 
                       level = get_ouorglevel(
                         operatingunit = site_meta$country,
                         # country = site_meta$country,
                         org_type = "prioritization"
                       ))
  

# VIZ ---------------------------------------------------------------------

  v_metrics <- df_curr_psnu %>% 
    ggplot(aes(metric_value, metric_site_direction)) +
    # geom_boxplot(alpha = .2, color = "gray", outlier.size = 0) +
    geom_point(aes(size = rel_size_qtile), position = position_jitter(height = .5, width =  .01, seed = 42), 
               # alpha = .6, color = trolley_grey, na.rm = TRUE) +
               shape = 21, fill = NA, color = trolley_grey, na.rm = TRUE) +
    geom_errorbar(aes(xmin = metric_value_psnu, xmax = metric_value_psnu), 
                  color = trolley_grey, size = 1.1, na.rm = TRUE) +
    geom_point(data = df_curr_psnu %>% filter(orgunituid == site_meta$orgunituid),
               aes(size = rel_size_qtile),
               shape = 21, fill = scooter, color = "white", #size = 4, 
               alpha = .8, na.rm = TRUE) +
    facet_wrap(metric_site_direction ~ ., scales = "free", ncol = 1) +
    scale_x_continuous(label = comma) +
    labs(x = NULL, y = NULL,
         subtitle = glue("Metric values compared against PSNU distribution in {site_meta$period}")
         # subtitle = glue("{site_meta$orgunit} metric values compared against {site_meta$psnu} distribution in {site_meta$period}")
         ) +
    coord_cartesian(clip = "off") +
    si_style_ygrid() +
    theme(strip.text = element_blank(),
          # axis.text.y = element_markdown(),
          legend.position = "none",
          panel.spacing = unit(3, "picas")) 
  

  v_size <- expand_grid(rel_size_qtile = 1:5,
                    metric = unique(df_metrics$metric)) %>% 
    ggplot(aes(rel_size_qtile, metric, size = rel_size_qtile)) +
    geom_point(shape = 22, color = trolley_grey) +
    geom_point(data = df_curr_psnu %>% 
                 filter(orgunituid == site_meta$orgunituid),
               shape = 22, fill = trolley_grey, color = trolley_grey) +
    scale_size_continuous(limits = c(1, 5)) +
    si_style_nolines() +
    facet_wrap(metric ~ ., scales = "free", ncol = 1) +
    labs(x = NULL, y = NULL,
         subtitle = "Relative size") +
    coord_cartesian(clip = "off") +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "white"),
          strip.text = element_blank(),
          panel.spacing = unit(3, "picas"))
  
  v_trends <- df_metrics %>% 
    filter(orgunituid == site_meta$orgunituid) %>% 
    ggplot(aes(period, metric_value, group = metric)) +
    geom_path(color = trolley_grey) +
    geom_point(color = trolley_grey) +
    facet_wrap(metric ~ ., scale = "free", ncol = 1) +
    si_style_nolines() +
    labs(x = NULL, y = NULL,
         # subtitle = glue("{site_meta$orgunit} 6 period trend across metrics")) +
         subtitle = glue("6 period trend across metrics")) +
    coord_cartesian(clip = "off") +
    theme(axis.text = element_blank(),
          strip.text = element_blank(),
          legend.position = "none",
          panel.spacing = unit(3, "picas"))


  v_ovc <- tibble(x = .5, y = .5) %>% 
    ggplot(aes(x, y)) +
    geom_point(size = 25, color = ifelse(site_meta$ovc_reported == TRUE, trolley_grey, "white")) +
    geom_text(label = "OVC\nREPORTING\nSITE", size = 8/.pt,
              color = "white", family = "Source Sans Pro SemiBold") +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())
  
  
  v_map <- site_meta %>% 
    ggplot() +
    geom_sf(data = spdf_psnu, aes(geometry = geometry), color = matterhorn,
            fill = NA, size = .5, alpha = .2) +
    geom_sf(data = spdf_psnu %>% filter(uid == site_meta$psnuuid), aes(geometry = geometry), color = matterhorn,
            fill = scooter_light, size = .5, alpha = .2) +
    geom_point(aes(longitude, latitude), shape = 24, color = "white", fill = old_rose) +
    labs(x = NULL, y = NULL) +
    si_style_map()
  

  # v_title <- ggplot() +
  #   geom_blank() +
  #   labs(title = glue("{toupper(site_meta$orgunit)}"),
  #        subtitle = glue("Site Level Profile | {site_meta$period}\n{site_meta$country} facility in {site_meta$psnu} supported by {site_meta$mech_code}")) +
  #   si_style_void() +
  #   theme(plot.title = element_text(family = "Source Sans Pro", face = "bold",
  #                                   size = 43, color = scooter),
  #         plot.subtitle = element_text(size = 15))
  
  v_title <- tibble(x = 0, y = .25) %>% 
    ggplot(aes(x, y)) +
    geom_text(label = glue("{toupper(site_meta$orgunit)}"), size = 20,
              color = scooter, family = "Source Sans Pro", fontface = "bold",
              hjust = 0) +
    geom_text(data = tibble(x = 0, y = .1),
              label = glue("Site Level Profile | {site_meta$period}\n{site_meta$country} facility in {site_meta$psnu} supported by {site_meta$mech_code}"),
              color = matterhorn, family = "Source Sans Pro",
              # vjust = 1,
              hjust = 0
    ) +
    expand_limits(x = c(0, 1), y = c(0, .32)) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_y_continuous(expand = c(.005, .005)) +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())

  v_score <- tibble(x = .5, y = .5) %>% 
    ggplot(aes(x, y)) +
    geom_text(label = "42", size = 20,
              color = scooter, family = "Source Sans Pro SemiBold") +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    scale_x_continuous(expand = c(.005, .005)) +
    scale_y_continuous(expand = c(.005, .005)) +
    si_style_nolines() +
    labs(x = NULL, y = NULL) +
    # labs(x = NULL, y = "Site\nScore") +
    theme(axis.text = element_blank(),
          axis.title.y = element_text(hjust = 1, vjust = .5, angle = 0))
  
  
  #STRUCTURE - t, l, b, r
  #with title placeholder
  layout_1 <- c(
    area(1, 1, 1, 9), #v_title
    area(1, 10, 2, 11), #v_map
    area(2, 3, 2, 4), #v_score
    area(2, 8, 2, 9), #v_ovc
    area(3, 1, 8.5, 6), #v_metric
    area(3, 7, 8.5, 8), #v_size
    area(3, 9, 8.5, 11) #v_trends
  )
  plot(layout_1)
  
  #no title placeholder
  layout_2 <- c(
    area(1, 10, 2, 11), #v_map
    area(1, 3, 2, 4), # v_score
    area(1, 8, 2, 9), #v_ovc
    area(3, 1, 8.5, 6), #v_metric
    area(3, 7, 8.5, 8), #v_size
    area(3, 9, 8.5, 11) #v_trends
  )
  plot(layout_2)

  

  #plot
  # v_title + v_map + v_score +  v_ovc + v_metrics + v_size + v_trends +
  #   plot_layout(design = layout_1) +
  #   plot_annotation(
  #     caption = glue("Note: Facilities are sized proportional to their quintile group by relevant indicator
  #                       Source: {api_source}"),
  #     theme = si_style())
  
  v_map + v_score + v_ovc + v_metrics + v_size + v_trends +
    plot_layout(design = layout_2) +
    plot_annotation(title = glue("{toupper(site_meta$orgunit)}"),
                    subtitle = glue("Site Level Profile | {site_meta$period}\n{site_meta$country} facility in {site_meta$psnu} supported by {site_meta$mech_code}"),
                    caption = glue("Note: Facilities are sized proportional to their quintile group by relevant indicator 
                        Source: {api_source}"),
                    theme = si_style() + theme(plot.title = element_text(family = "Source Sans Pro", face = "bold",
                                                                         size = 43, color = scooter),
                                               plot.subtitle = element_text(size = 15)))

  
  #export
  si_save("Images/test2.png", width = 11, height = 8.5)  
  
  
  