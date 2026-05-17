
# # linear regression
# reg_linear <- function(data, N_data, N_iv) {
#   
#   data_reg <- data %>% 
#     dplyr::left_join(N_data, by = c("year", "cz")) %>% 
#     dplyr::left_join(N_iv, by = c("year", "cz")) %>% 
#     dplyr::filter(N > 0)
#   
#   data_iv <- dplyr::filter(data_reg, N_adj > 0)
#   N_adj <- data_iv %>%
#     dplyr::pull(N_adj) %>%
#     unique() %>%
#     sort()
#   
#   for (i in 1:length(N_adj)) {
#     j <- N_adj[i]
#     varname <- paste("N_adj", j, sep = "_")
#     data_iv <- data_iv %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N_adj == j, 1, 0))
#   }
#   
#   
#   temp <- paste("N_adj", N_adj, sep = "_", collapse = " + ")
#   N_iv_formula <- paste("log(N) ", temp, sep = " ~ ")
#   
#   result <- list()
#   formula <- list()
#   
#   formula[[1]] <- paste("log(wage) ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[2]] <- paste("log(wage) ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[3]] <- paste("log(wage) ~ ", ctr_vars, " | year + cz + firm_id | (log(N) ~ log(N_adj)) | cz", sep = "")
#   formula[[4]] <- paste("log(wage) ~ ", ctr_vars, " | year + cz + firm_id | (", N_iv_formula, ") | cz", sep = "")
#   
#   formula[[5]] <- paste("log(fee) ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[6]] <- paste("log(fee) ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[7]] <- paste("log(fee) ~ ", ctr_vars, " | year + cz + firm_id | (log(N) ~ log(N_adj)) | cz", sep = "")
#   formula[[8]] <- paste("log(fee) ~ ", ctr_vars, " | year + cz + firm_id | (", N_iv_formula, ") | cz", sep = "")
#   
#   
#   formula[[9]] <- paste("margin ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[10]] <- paste("margin ~ log(N) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   formula[[11]] <- paste("margin ~ ", ctr_vars, " | year + cz + firm_id | (log(N) ~ log(N_adj)) | cz", sep = "")
#   formula[[12]] <- paste("margin ~ ", ctr_vars, " | year + cz + firm_id | (", N_iv_formula, ") | cz", sep = "")
#   
#   
#   formula[[13]] <- paste("log(N) ~ log(N_adj) + ", ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
#   
#   
#   nspec <- length(formula)
#   for (i in 1:nspec) {
#     
#     if (i %in% c(1, 5, 9)) {
#       data_touse <- data_reg
#     } else {
#       data_touse <- data_iv
#     }
#     
#     result[[i]] <- felm(
#       data = data_touse,
#       formula = as.formula(formula[[i]])
#     )
#     
#   }
#   
#   # tidy up the expression
#   i <- 1 + length(ctr_vars_names)
#   coef_order <- c(i, 1:(i-1))
#   
#   for (j in c(3, 4, 7, 8, 11, 12)) {
#     
#     # change label
#     rownames(result[[j]]$coefficients)[i] <- "log(N)"
#     rownames(result[[j]]$beta)[i] <- "log(N)"
#     names(result[[j]]$cse)[i] <- "log(N)"
#     
#     result[[j]]$coefficients <- result[[j]]$coefficients[coef_order, 1, drop = F]
#     result[[j]]$beta <- result[[j]]$beta[coef_order, 1, drop = F]
#     result[[j]]$cse <- result[[j]]$cse[coef_order]
#     result[[j]]$cpval <- result[[j]]$cpval[coef_order]
#     
#   }
#   
#   return(result)
#   
# }
# 
# # show results of the linear regression
# show_result <- function(result) {
#   nspec = length(result)  
#   nyvar = 3
#   stargazer(result,
#             type = type,
#             keep.stat = c("n", "rsq"),
#             dep.var.labels.include = FALSE,
#             column.separate = c(4, 4, 4, 1),
#             column.labels = c("log(wage)", "log(fee)", "margin", "log(N)"),
#             keep = c(1, 2),
#             align = TRUE,
#             add.lines = list(c("IV (Log-linear)",
#                                c(rep(c("No", "No", "Yes", "No"), 3), " ")),
#                              c("IV (Dummies)",
#                                c(rep(c("No", "No", "No", "Yes"), 3), " "))),
#             notes.align = "l")  
# }


# plot coefficients
make_coefficient_plot <- function(data_reg,
                      N_vars,
                      N_vec,
                      ctr_vars,
                      yvar,
                      ylab,
                      ylim_vec = NULL,
                      ref_lev = 1,
                      N_iv_formula = NULL) {
  

  if (is.null(N_iv_formula)) {

    formula <- paste(yvar, " ~ ", N_vars, " + ",  ctr_vars, " | year + cz + firm_id | 0 | cz", sep = "")
    result <- lfe::felm(
      formula = as.formula(formula),
      data = data_reg
    )
    temp <- dim(summary(result)$coefficients)[1]
    coef <- summary(result)$coefficients[1:length(N_vec), 1]
    coef <- c(0, coef)
    se <- summary(result)$coefficients[1:length(N_vec), 2]
    se <- c(0, se)
  } else {

    formula <- paste(yvar, " ~ ",   ctr_vars, " | year + cz + firm_id | (", N_iv_formula, ") | cz", sep = "")
    result <- lfe::felm(
      formula = as.formula(formula),
      data = data_reg
    )
    temp <- dim(summary(result)$coefficients)[1]
    coef <- summary(result)$coefficients[(temp - length(N_vec) + 1):temp, 1]
    coef <- c(0, coef)
    se <- summary(result)$coefficients[(temp - length(N_vec) + 1):temp, 2]
    se <- c(0, se)
  }
  
  
  x_axis <- c(ref_lev, N_vec)

  g <- data.frame(N = x_axis, coef = coef, se = se, group = "all") %>%
    ggplot(
      aes(
        x = N,
        y = coef,
        fill = group
        )
      ) +
    geom_point() + 
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(
        ymax = coef + 1.645 * se,
        ymin = coef - 1.645 * se,
        fill = group
        ),
      alpha = 0.1
      ) +
    labs(
      x = "Number of Platforms",
      y = ylab
    ) +
    scale_fill_viridis_d() +
    theme_classic() +
    theme(
      legend.position = "none"
    )
    
  # Change ylim
  if (!is.null(ylim)) {
    g <- g  + ylim(ylim_vec[1], ylim_vec[2])
  }

  return(g)
  
}

# 
# 
# reg_nonlinear <- function(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name) {
#   
#  # reg_nonlinear_each_N(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name)
#  reg_nonlinear_coarse(data, N_data, N_iv,  yvar_vec, ylab_vec, dir_name)
#  # reg_nonlinear_deciles(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name)
#   
# }
# 
# reg_nonlinear_each_N <- function(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name) {
#   # Regress on 1{N=2}, 1{N=3}, ...
# 
#   # Merge the number of platforms
#   data_reg <- data %>% 
#     dplyr::left_join(N_data, by = c("year", "cz")) %>% 
#     dplyr::left_join(N_iv, by = c("year", "cz")) %>% 
#     dplyr::filter(N > 0)
#   
#   # Distribution of N
#   N_dist <- data_reg %>% 
#     dplyr::select(cz, year, N) %>% 
#     dplyr::distinct() %>% 
#     dplyr::pull(N)
#   
#   N_vec <- unique(N_dist) %>% 
#     sort()  
#   
#   # truncate at F(N) = 0.5
#   N_max <- floor(quantile(N_dist, 0.5))
#   N_vec <- N_vec[N_vec <= N_max]
#   N_vec <- N_vec[2:length(N_vec)] # remove reference level (N=1)
#   
#   for (i in 1:(length(N_vec) - 1)) {
#     j <- N_vec[i]
#     varname <- paste("N", j, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N == j, 1, 0))
#   }
#   
#   varname <- paste("N", N_max, sep = "_")
#   data_reg <- data_reg %>%
#     dplyr::mutate(!!varname := dplyr::if_else(N >= N_max, 1, 0))
#   
#   N_vars <- paste("N", N_vec, sep = "_", collapse = " + ")
#   
#   cat("### Regression on the indicators for each $N$")
#   cat("\n\n")
#   g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec)
#   do.call("grid.arrange", c(g_list, nrow = 2))
#   cat("\n\n")
#   
#   # save
#   for (i in 1:length(ylab_vec)) {
#     filename <- paste("draft/figuretable/competition_regression/", dir_name, "/each_N/competition_OLS_", ylab_vec[i], ".png", sep = "")
#     ggsave(
#       g_list[[i]],
#       filename = here::here(filename),
#       width = 4,
#       height = 3
#     )  
#   }
#   
#   # IV
#   N_adj <- data_reg %>%
#     dplyr::pull(N_adj) %>%
#     unique() %>%
#     sort()
#   
#   for (i in 1:length(N_adj)) {
#     j <- N_adj[i]
#     varname <- paste("N_adj", j, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N_adj == j, 1, 0))
#   }
#   
#   temp1 <- paste("N", N_vec, sep = "_", collapse = " | ")
#   temp2 <- paste("N_adj", N_adj, sep = "_", collapse = " + ")
#   N_iv_formula <- paste(temp1, temp2, sep = " ~ ")
#   
# 
#   cat("### Using IV")
#   cat("\n\n")
#   g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec, ref_lev = 1, N_iv_formula)
#   do.call("grid.arrange", c(g_list, nrow = 2))
#   cat("\n\n")
#   
#   # save
#   
#   for (i in 1:length(ylab_vec)) {
#     filename <- paste("draft/figuretable/competition_regression/", dir_name, "/each_N/competition_IV_", ylab_vec[i], ".png", sep = "")
#     ggsave(
#       g_list[[i]],
#       filename = here::here(filename),
#       width = 4,
#       height = 3
#     )  
#   }
#   
# }
# 
# 
# reg_nonlinear_coarse <- function(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name) {
#   # Regress on 1{N=4,5,6}, 1{N=7,8,9}, ...
#   
#   # Merge the number of platforms
#   data_reg <- data %>% 
#     dplyr::left_join(N_data, by = c("year", "cz")) %>% 
#     dplyr::left_join(N_iv, by = c("year", "cz")) %>% 
#     dplyr::filter(N > 0)
#   
#   # Distribution of N
#   N_dist <- data_reg %>% 
#     dplyr::select(cz, year, N) %>% 
#     dplyr::distinct() %>% 
#     dplyr::pull(N)
#   
#   N_vec <- unique(N_dist) %>% 
#     sort()  
#   
#   N_vec <- c(4, 7, 10, 13, 16, 19)
#   N_size <- length(N_vec) + 1
#   
#   for (i in 2:(N_size - 1)) {
#     varname <- paste("N", i, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N >= N_vec[i-1] & N < N_vec[i], 1, 0))
#   }
#   
#   varname <- paste("N", N_size, sep = "_")
#   data_reg <- data_reg %>%
#     dplyr::mutate(!!varname := dplyr::if_else(N >= N_vec[length(N_vec)], 1, 0))
#   
#   N_vars <- paste("N", 2:N_size, sep = "_", collapse = " + ")
#   
#   title <- "### Regression on the indicators for each $N$"
#   cat("\n\n")
#   g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec)
#   do.call("grid.arrange", c(g_list, nrow = 2))
#   cat("\n\n")
#   
#   # save
#   for (i in 1:length(ylab_vec)) {
#     filename <- paste("draft/figuretable/competition_regression/", dir_name, "/coarse/competition_OLS_", ylab_vec[i], ".png", sep = "")
#     ggsave(
#       g_list[[i]],
#       filename = here::here(filename),
#       width = 4,
#       height = 3
#     )  
#   }
#   
#   # IV
#   N_adj <- data_reg %>%
#     dplyr::pull(N_adj) %>%
#     unique() %>%
#     sort()
#   
#   for (i in 1:length(N_adj)) {
#     j <- N_adj[i]
#     varname <- paste("N_adj", j, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N_adj == j, 1, 0))
#   }  
#   
#   temp1 <- paste("N", 2:N_size, sep = "_", collapse = " | ")
#   temp2 <- paste("N_adj", N_adj, sep = "_", collapse = " + ")
#   N_iv_formula <- paste(temp1, temp2, sep = " ~ ")
#   
#   title <- "### Using IV"
#   cat("\n\n")
#   g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec, ref_lev = 1, N_iv_formula)
#   do.call("grid.arrange", c(g_list, nrow = 2))
#   cat("\n\n")
#   
#   # save
#   
#   for (i in 1:length(ylab_vec)) {
#     filename <- paste("draft/figuretable/competition_regression/", dir_name, "/coarse/competition_IV_", ylab_vec[i], ".png", sep = "")
#     ggsave(
#       g_list[[i]],
#       filename = here::here(filename),
#       width = 4,
#       height = 3
#     )  
#   }
#   
# }
# 
# 
# reg_nonlinear_deciles <- function(data, N_data, N_iv, yvar_vec, ylab_vec, dir_name) {
#   # Regress on indicators for deciles
#   
#   # Merge the number of platforms
#   data_reg <- data %>% 
#     dplyr::left_join(N_data, by = c("year", "cz")) %>% 
#     dplyr::left_join(N_iv, by = c("year", "cz")) %>% 
#     dplyr::filter(N > 0)
#   
#   # Distribution of N
#   N_dist <- data_reg %>% 
#     dplyr::select(cz, year, N) %>% 
#     dplyr::distinct() %>% 
#     dplyr::pull(N)
#   
#   N_vec <- unique(N_dist) %>% 
#     sort() 
#   
# 
#   N_qntl <- quantile(N_dist, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
#   N_size <- length(N_qntl) + 1
#   
#   for (i in 2:(N_size - 1)) {
#     varname <- paste("N", i, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N >= N_qntl[i-1] & N < N_qntl[i], 1, 0))
#   }
#   
#   data_reg <- data_reg %>%
#     dplyr::mutate(N_10 = dplyr::if_else(N >= N_qntl[9], 1, 0))
#   
#   N_vars <- paste("N", 2:N_size, sep = "_") %>%
#     paste(sep = "", collapse = " + ")
#   
#   if (length(N_qntl) == length(unique(N_qntl))) {
#     N_vec <- 2:10 * 0.1
#     title <- "1.1 Regression on the indicators for each $N$"
#     cat(paste("##### ", colorize(title, "blue"), sep = ""))
#     cat("\n\n")
#     g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec)
#     do.call("grid.arrange", c(g_list, nrow = 2))
#     cat("\n\n")
#     
#     # save
#     for (i in 1:length(ylab_vec)) {
#       filename <- paste("draft/figuretable/competition_regression/", dir_name, "/decile/competition_OLS_", ylab_vec[i], ".png", sep = "")
#       ggsave(
#         g_list[[i]],
#         filename = here::here(filename),
#         width = 4,
#         height = 3
#       )  
#     }
#   }
#   
#   # IV
#   N_adj <- data_reg %>%
#     dplyr::pull(N_adj) %>%
#     unique() %>%
#     sort()
#   
#   for (i in 1:length(N_adj)) {
#     j <- N_adj[i]
#     varname <- paste("N_adj", j, sep = "_")
#     data_reg <- data_reg %>%
#       dplyr::mutate(!!varname := dplyr::if_else(N_adj == j, 1, 0))
#   }
#   
#   temp1 <- paste("N", 2:10, sep = "_", collapse = " | ")
#   temp2 <- paste("N_adj", N_adj, sep = "_", collapse = " + ")
#   N_iv_formula <- paste(temp1, temp2, sep = " ~ ")
#   
#   if (length(N_qntl) == length(unique(N_qntl))) {
#     N_vec <- 2:10 * 0.1
#     title <- "1.2 Using IV"
#     cat(paste("##### ", colorize(title, "blue"), sep = ""))
#     cat("\n\n")
#     g_list <- plot_func(data_reg, N_vars, N_vec, yvar_vec, ylab_vec, ref_lev = 1, N_iv_formula)
#     do.call("grid.arrange", c(g_list, nrow = 2))
#     cat("\n\n")
#     
#     # save
#     
#     for (i in 1:length(ylab_vec)) {
#       filename <- paste("draft/figuretable/competition_regression/", dir_name, "/decile/competition_IV_", ylab_vec[i], ".png", sep = "")
#       ggsave(
#         g_list[[i]],
#         filename = here::here(filename),
#         width = 4,
#         height = 3
#       )  
#     }
#   }  
# }
# 
# show_deciles <- function(N_data) {
#   N_data %>%
#     dplyr::filter(N > 0, !is.na(N)) %>% 
#     dplyr::pull(N) %>%
#     quantile(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) %>%
#     t() %>% 
#     kable()
# }
# 
