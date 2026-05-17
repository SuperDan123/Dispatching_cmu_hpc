eq <- readRDS("output/multihome/estimate_data/equilibrium_updated_constrained.rds")

T <- length(eq$exogenous)
J_by_t <- vapply(eq$exogenous, length, integer(1))
years <- if (T == 5) 2010:2014 else seq_len(T)

n_options <- numeric(0)
size_w <- numeric(0)
size_f <- numeric(0)
w_all <- numeric(0)
f_all <- numeric(0)
s_w_all <- numeric(0)
s_f_all <- numeric(0)

for (t in seq_len(T)) {
  for (j in seq_len(J_by_t[t])) {
    ex <- eq$exogenous[[t]][[j]]
    en <- eq$endogenous[[t]][[j]]
    n_options <- c(n_options, length(en$w))
    size_w <- c(size_w, ex$size_w)
    size_f <- c(size_f, ex$size_f)
    w_all <- c(w_all, en$w)
    f_all <- c(f_all, en$f)
    s_w_all <- c(s_w_all, en$s_w)
    s_f_all <- c(s_f_all, en$s_f)
  }
}

margin_all <- (f_all - w_all) / f_all
margin_all[f_all == 0] <- NA_real_

r3 <- function(x) sprintf("%.3f", x)

stats <- list(
  sample_years = paste0(min(years), "--", max(years)),
  T = T,
  J = if (length(unique(J_by_t)) == 1) as.integer(J_by_t[1]) else J_by_t,
  market_years = sum(J_by_t),
  nopt_min = min(n_options),
  nopt_med = as.numeric(stats::median(n_options)),
  nopt_max = max(n_options),
  size_w_mean = as.numeric(mean(size_w)),
  size_f_mean = as.numeric(mean(size_f)),
  w_mean = as.numeric(mean(w_all)),
  f_mean = as.numeric(mean(f_all)),
  margin_mean = as.numeric(mean(margin_all, na.rm = TRUE)),
  s_w_mean = as.numeric(mean(s_w_all)),
  s_f_mean = as.numeric(mean(s_f_all))
)

tex_lines <- c(
  "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lr}",
  "\\toprule\\toprule",
  "Statistic & Value\\\\",
  "\\midrule",
  paste0("Sample years & ", stats$sample_years, "\\\\"),
  paste0("Years (T) & ", stats$T, "\\\\"),
  paste0(
    "Commuting zones per year & ",
    if (length(stats$J) == 1) stats$J else paste(stats$J, collapse = ", "),
    "\\\\"
  ),
  paste0("Market-years & ", stats$market_years, "\\\\"),
  paste0(
    "Options per market-year (min/median/max) & ",
    stats$nopt_min, "/", r3(stats$nopt_med), "/", stats$nopt_max, "\\\\"
  ),
  paste0("Mean worker-side market size (size\\_w) & ", r3(stats$size_w_mean), "\\\\"),
  paste0("Mean firm-side market size (size\\_f) & ", r3(stats$size_f_mean), "\\\\"),
  paste0("Mean wage across options & ", r3(stats$w_mean), "\\\\"),
  paste0("Mean fee across options & ", r3(stats$f_mean), "\\\\"),
  paste0("Mean margin (f>0) & ", r3(stats$margin_mean), "\\\\"),
  paste0("Mean worker share across options & ", r3(stats$s_w_mean), "\\\\"),
  paste0("Mean firm share across options & ", r3(stats$s_f_mean), "\\\\"),
  "\\bottomrule",
  "\\end{tabular*}",
  ""
)

dir.create("draft/figuretable/estimation_result", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_lines, "draft/figuretable/estimation_result/equilibrium_summary.tex")

html_rows <- c(
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Sample years", stats$sample_years),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Years (T)", stats$T),
  sprintf(
    "<tr><td>%s</td><td>%s</td></tr>",
    "Commuting zones per year",
    if (length(stats$J) == 1) stats$J else paste(stats$J, collapse = ", ")
  ),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Market-years", stats$market_years),
  sprintf(
    "<tr><td>%s</td><td>%s</td></tr>",
    "Options per market-year (min/median/max)",
    paste0(stats$nopt_min, "/", r3(stats$nopt_med), "/", stats$nopt_max)
  ),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean worker-side market size (size_w)", r3(stats$size_w_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean firm-side market size (size_f)", r3(stats$size_f_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean wage across options", r3(stats$w_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean fee across options", r3(stats$f_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean margin (f>0)", r3(stats$margin_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean worker share across options", r3(stats$s_w_mean)),
  sprintf("<tr><td>%s</td><td>%s</td></tr>", "Mean firm share across options", r3(stats$s_f_mean))
)

html_block <- c(
  "<div id=\"equilibrium-summary\" class=\"section level1\">",
  "<h1>equilibrium summary statistics</h1>",
  "<table class=\"table table-striped\" style=\"width:100%\">",
  "<thead><tr><th>Statistic</th><th>Value</th></tr></thead>",
  "<tbody>",
  html_rows,
  "</tbody></table>",
  "</div>"
)

writeLines(html_block, "report/multihome/_equilibrium_summary_table.html")

cat("Wrote:\n- draft/figuretable/estimation_result/equilibrium_summary.tex\n- report/multihome/_equilibrium_summary_table.html\n")


