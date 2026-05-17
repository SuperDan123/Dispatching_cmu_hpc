
make_fit_figures <- function(simdata, share_data, dirname) {
  
  dir.create(file.path("draft/figuretable/evaluate_fit", dirname), showWarnings = FALSE)
  
  size <- 1
  alpha <- 0.5
  
  simdata <- simdata %>% 
    na.omit() %>% 
    dplyr::rename(
      wage_sim = wage,
      fee_sim = fee,
      s_W_sim = s_W,
      s_F_sim = s_F
    )
  
  # wage
  simdata %>%  
    dplyr::left_join(share_data, by = "id_unique") %>% 
    dplyr::mutate(group = "all") %>% 
    ggplot(
      aes(
        x = wage_sim,
        y = wage,
        color = "group"
      )
    ) +
    geom_point(
      size = size,
      alpha = alpha
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(
      x = "Simulated",
      y = "Actual"
    ) +
    xlim(0, 5) +
    ylim(0, 5) +
    scale_colour_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
    filename = here::here(
      paste("draft/figuretable/evaluate_fit/", dirname, "/wage.png", sep = "")
      ),
    width = 4,
    height = 3
  )
  
  
  # fee
  simdata %>% 
    dplyr::left_join(share_data, by = "id_unique") %>% 
    dplyr::mutate(group = "all") %>% 
    ggplot(
      aes(
        x = fee_sim,
        y = fee,
        color = "group"
      )
    ) +
    geom_point(
      size = size,
      alpha = alpha
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(
      x = "Simulated",
      y = "Actual"
    ) +
    xlim(0, 5) +
    ylim(0, 5) +
    scale_colour_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
      filename = here::here(
        paste("draft/figuretable/evaluate_fit/", dirname, "/fee.png", sep = "")
        ),
    width = 4,
    height = 3
  )
  
  # margin
  simdata %>% 
    dplyr::left_join(share_data, by = "id_unique") %>% 
    dplyr::mutate(group = "all") %>% 
    dplyr::mutate(
      margin_sim = log(fee_sim) - log(wage_sim),
      margin_act = log(fee) - log(wage)
    ) %>%  
    ggplot(
      aes(
        x = margin_sim,
        y = margin_act,
        color = "group"
      )
    )  +
    geom_point(
      size = size,
      alpha = alpha
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(
      x = "Simulated",
      y = "Actual"
    ) +
    scale_colour_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
      filename = here::here(
        paste("draft/figuretable/evaluate_fit/", dirname, "/margin.png", sep = "")
      ),
    width = 4,
    height = 3
  )
  
  
  # worker share
  simdata %>% 
    dplyr::left_join(share_data, by = "id_unique")%>%  
    ggplot(
      aes(
        x = s_W_sim,
        y = s_W,
        color = "group"
      )
    ) +
    geom_point(
      size = size,
      alpha = alpha
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(
      x = "Simulated",
      y = "Actual"
    ) +
    xlim(0, 0.05) +
    ylim(0, 0.05) +
    scale_colour_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
      filename = here::here(
        paste("draft/figuretable/evaluate_fit/", dirname, "/share_worker.png", sep = "")
      ),
    width = 4,
    height = 3
  )
  
  
  # firm share
  simdata %>% 
    dplyr::left_join(share_data, by = "id_unique") %>%  
    ggplot(
      aes(
        x = s_F_sim,
        y = s_F,
        color = "group"
      )
    ) +
    geom_point(
      size = size,
      alpha = alpha
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
    labs(
      x = "Simulated",
      y = "Actual"
    ) +
    xlim(0, 0.01) +
    ylim(0, 0.01) +
    scale_colour_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
  
  ggsave(
      filename = here::here(
        paste("draft/figuretable/evaluate_fit/", dirname, "/share_firm.png", sep = "")
      ),
    width = 4,
    height = 3
  )
  
}
