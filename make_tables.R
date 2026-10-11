# -------------------------------------------------------------------------------------------------------------------------
# make_tables.R : write LaTeX tables (and event-study figures) from the staggered DiD script
#
# Run AFTER the main script, in the same R session:
#   source("make_tables.R")
#
# Uses these objects from the main script:
#   long, wfh_history, did_sample, baseline,
#   did_model, did_model_controls,
#   overall_effect, overall_effect_controls,
#   event_effect, event_effect_controls
#
# Base R only (ggplot2 is used for figures if installed; it comes with `did`).
# -------------------------------------------------------------------------------------------------------------------------

dir.create("tables",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)


# ---- Formatting helpers ----

# Number with d decimals; a leading minus becomes a proper LaTeX minus
num <- function(x, d = 2) {
  s <- formatC(x, format = "f", digits = d)
  s <- sub("^-", "$-$", s)
  s[is.na(x)] <- "--"
  s
}

# Integer with thousands separator
int <- function(x) formatC(x, format = "d", big.mark = ",")

# Standard error / SD in parentheses
pp <- function(x, d = 2) paste0("(", num(x, d), ")")

# One table row: cells joined by &, ended by \\
trow <- function(...) paste0(paste(c(...), collapse = " & "), " \\\\")

# Wrap a table body into a full table environment and write it
write_tab <- function(file, caption, label, colspec, header, body, notes = NULL) {
  out <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\small",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (!is.null(notes)) {
    out <- c(
      out,
      "",
      "\\vspace{4pt}",
      "\\begin{minipage}{0.9\\linewidth}",
      "\\footnotesize",
      paste0("\\textit{Notes:} ", notes),
      "\\end{minipage}"
    )
  }
  out <- c(out, "\\end{table}")
  writeLines(out, file.path("tables", file))
  message("Wrote tables/", file)
}


# ---- Common pieces ----

cohort_levels <- c(0, 2020, 2021, 2022)
cohort_names  <- c("Never", "2020", "2021", "2022")

state_codes <- c(1, 2, 3, 4, 5, 6, 8)
state_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS/NT", "ACT")

n_people  <- length(unique(did_sample$xwaveid))
n_pyears  <- nrow(did_sample)


# -------------------------------------------------------------------------------------------------------------------------
# Table 1: Sample construction
# -------------------------------------------------------------------------------------------------------------------------

n_all    <- length(unique(long$xwaveid))
n_cohort <- sum(!is.na(wfh_history$g))

body1 <- c(
  trow("All individuals in HILDA, waves 17--22 (2017--2022)", int(n_all)),
  trow("Valid WFH cohort: not WFH in 2019, WFH status observed as required in 2019--2022", int(n_cohort)),
  trow("Final sample: satisfaction observed and same partner in every year 2019--2022", int(n_people)),
  "\\midrule",
  trow("\\quad of which never treated",      int(sum(baseline$g == 0))),
  trow("\\quad of which first WFH in 2020",  int(sum(baseline$g == 2020))),
  trow("\\quad of which first WFH in 2021",  int(sum(baseline$g == 2021))),
  trow("\\quad of which first WFH in 2022",  int(sum(baseline$g == 2022))),
  trow("Person-years in final sample (2017--2022)", int(n_pyears))
)

write_tab(
  file    = "tab_sample.tex",
  caption = "Construction of the analytical sample",
  label   = "tab:sample",
  colspec = "p{0.72\\linewidth}r",
  header  = trow("", "Individuals"),
  body    = body1,
  notes   = paste(
    "WFH is \\texttt{\\_jbmh} (any usual working hours worked at home), which is asked only of employed respondents,",
    "so the valid-cohort step also requires employment in the years that define the cohort.",
    "2019--2022 is balanced in the final sample; 2017--2018 rows are kept where observed."
  )
)


# -------------------------------------------------------------------------------------------------------------------------
# Table 2: Baseline (2019) characteristics by treatment cohort
# -------------------------------------------------------------------------------------------------------------------------

groups <- list(baseline)
for (c0 in cohort_levels) groups[[length(groups) + 1]] <- baseline[baseline$g == c0, ]
group_names <- c("All", cohort_names)

col_mean <- function(v) sapply(groups, function(d) mean(d[[v]]))
col_sd   <- function(v) sapply(groups, function(d) sd(d[[v]]))

body2 <- c(
  trow("Satisfaction with partner (0--10)", num(col_mean("sat"))),
  trow("",                                  pp(col_sd("sat"))),
  trow("Age in 2019",                       num(col_mean("age19"))),
  trow("",                                  pp(col_sd("age19"))),
  trow("Female",                            num(col_mean("female"))),
  "\\addlinespace",
  paste0("\\multicolumn{", length(groups) + 1, "}{l}{\\textit{State/territory in 2019 (share)}} \\\\")
)
for (k in seq_along(state_codes)) {
  shares <- sapply(groups, function(d) mean(d$state_grp == state_codes[k]))
  body2  <- c(body2, trow(paste0("\\quad ", state_names[k]), num(shares)))
}
body2 <- c(
  body2,
  "\\midrule",
  trow("Individuals", int(sapply(groups, nrow)))
)

write_tab(
  file    = "tab_baseline.tex",
  caption = "Baseline (2019) characteristics by WFH cohort",
  label   = "tab:baseline",
  colspec = paste0("l", strrep("c", length(groups))),
  header  = c(
    paste0(" & & \\multicolumn{", length(cohort_levels), "}{c}{By cohort (first year of WFH)} \\\\"),
    paste0("\\cmidrule(lr){3-", length(groups) + 1, "}"),
    trow("", group_names)
  ),
  body    = body2,
  notes   = paste(
    "One row per person, measured in 2019. Standard deviations in parentheses.",
    "``Never'' = not WFH in any year 2019--2022. NT is pooled with TAS because of small cells.",
    "By construction nobody in the sample works from home in 2019."
  )
)


# -------------------------------------------------------------------------------------------------------------------------
# Table 3: Raw mean satisfaction by year and cohort (estimation sample)
# -------------------------------------------------------------------------------------------------------------------------

means3 <- tapply(did_sample$sat, list(did_sample$year, did_sample$g), mean)
ns3    <- tapply(did_sample$sat, list(did_sample$year, did_sample$g), length)
years  <- sort(unique(did_sample$year))

body3 <- character(0)
for (y in years) {
  yy    <- as.character(y)
  cells <- paste0(num(means3[yy, as.character(cohort_levels)]),
                  " {\\scriptsize [", int(ns3[yy, as.character(cohort_levels)]), "]}")
  body3 <- c(body3, trow(yy, cells))
  if (y == 2019) body3 <- c(body3, "\\midrule")
}

write_tab(
  file    = "tab_rawmeans.tex",
  caption = "Mean satisfaction with partner by year and WFH cohort",
  label   = "tab:rawmeans",
  colspec = paste0("l", strrep("c", length(cohort_levels))),
  header  = c(
    paste0(" & \\multicolumn{", length(cohort_levels), "}{c}{Cohort (first year of WFH)} \\\\"),
    paste0("\\cmidrule(lr){2-", length(cohort_levels) + 1, "}"),
    trow("Year", cohort_names)
  ),
  body    = body3,
  notes   = paste(
    "Estimation sample. Person counts in brackets; 2019--2022 is balanced, 2017--2018 are observed where available.",
    "The rule below 2019 separates years before any cohort is treated."
  )
)


# -------------------------------------------------------------------------------------------------------------------------
# Table 4: Overall ATT
# -------------------------------------------------------------------------------------------------------------------------

att1 <- overall_effect$overall.att;          se1 <- overall_effect$overall.se
att2 <- overall_effect_controls$overall.att; se2 <- overall_effect_controls$overall.se

ci <- function(a, s) paste0("[", num(a - 1.96 * s, 3), ", ", num(a + 1.96 * s, 3), "]")

pre_p <- function(m) if (is.null(m$Wpval)) NA else m$Wpval

body4 <- c(
  trow("WFH (overall ATT)", num(att1, 3), num(att2, 3)),
  trow("",                  pp(se1, 3),  pp(se2, 3)),
  trow("95\\% CI",          ci(att1, se1), ci(att2, se2)),
  "\\addlinespace",
  trow("Pre-trend test $p$-value", num(pre_p(did_model), 3), num(pre_p(did_model_controls), 3)),
  trow("Controls (2019 age, female, state)", "No", "Yes"),
  trow("Comparison group", "Not yet treated", "Not yet treated"),
  "\\midrule",
  trow("Individuals",  int(n_people), int(n_people)),
  trow("Person-years", int(n_pyears), int(n_pyears))
)

write_tab(
  file    = "tab_att.tex",
  caption = "Effect of starting to work from home on satisfaction with partner",
  label   = "tab:att",
  colspec = "lcc",
  header  = trow("", "(1)", "(2)"),
  body    = body4,
  notes   = paste(
    "Callaway and Sant'Anna (2021) estimator, R package \\texttt{did}, doubly robust,",
    "aggregated with \\texttt{aggte(type = \"simple\")}. Bootstrapped standard errors clustered by person in parentheses.",
    paste0("Outcome on a 0--10 scale; baseline SD = ", num(sd(baseline$sat)), "."),
    "The pre-trend test is the Wald test that all pre-treatment ATT($g,t$) equal zero."
  )
)


# -------------------------------------------------------------------------------------------------------------------------
# Table 5: Event study
# -------------------------------------------------------------------------------------------------------------------------

# Mark estimates whose simultaneous 95% band excludes zero
mark <- function(a, s, cv) ifelse(abs(a) > cv * s, "$^{\\dagger}$", "")

es_cells <- function(ev) {
  list(
    e   = ev$egt,
    est = paste0(num(ev$att.egt, 3), mark(ev$att.egt, ev$se.egt, ev$crit.val.egt)),
    se  = pp(ev$se.egt, 3)
  )
}
es1 <- es_cells(event_effect)
es2 <- es_cells(event_effect_controls)

body5 <- character(0)
for (k in seq_along(es1$e)) {
  if (es1$e[k] == 0) body5 <- c(body5, "\\midrule")
  lab   <- ifelse(es1$e[k] < 0, paste0("$", es1$e[k], "$"), as.character(es1$e[k]))
  body5 <- c(body5,
             trow(lab, es1$est[k], es2$est[k]),
             trow("",  es1$se[k],  es2$se[k]))
}

write_tab(
  file    = "tab_event.tex",
  caption = "Event-study estimates by years since starting WFH",
  label   = "tab:event",
  colspec = "lcc",
  header  = c(
    trow("Event time $e = t - g$", "(1) No controls", "(2) Controls"),
    ""
  ),
  body    = body5,
  notes   = paste(
    "\\texttt{aggte(type = \"dynamic\")}. Standard errors in parentheses.",
    "$^{\\dagger}$ the simultaneous 95\\% confidence band excludes zero.",
    "Pre-periods use the default varying base period, so each pre-period estimate is a one-year change rather than a gap relative to $g-1$.",
    "$e=-4$ is identified from the 2022 cohort only."
  )
)


# -------------------------------------------------------------------------------------------------------------------------
# Table A1: Group-time ATTs (appendix)
# -------------------------------------------------------------------------------------------------------------------------

gt1 <- did_model
gt2 <- did_model_controls

body6 <- character(0)
for (k in seq_along(gt1$group)) {
  gg <- gt1$group[k]; tt <- gt1$t[k]
  if (k > 1 && gg != gt1$group[k - 1]) body6 <- c(body6, "\\addlinespace")
  body6 <- c(body6, trow(
    gg, tt,
    ifelse(tt - gg < 0, paste0("$", tt - gg, "$"), tt - gg),
    paste0(num(gt1$att[k], 3), " ", pp(gt1$se[k], 3)),
    paste0(num(gt2$att[k], 3), " ", pp(gt2$se[k], 3))
  ))
}

write_tab(
  file    = "tab_attgt.tex",
  caption = "Group-time average treatment effects ATT($g,t$)",
  label   = "tab:attgt",
  colspec = "cccll",
  header  = trow("Cohort $g$", "Year $t$", "$t-g$", "(1) No controls", "(2) Controls"),
  body    = body6,
  notes   = "Standard errors in parentheses. Rows with $t < g$ are pre-treatment placebo comparisons."
)


# -------------------------------------------------------------------------------------------------------------------------
# Figures: event-study plots
# -------------------------------------------------------------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggsave("figures/event_m1.pdf", ggdid(event_effect),          width = 6, height = 3.5)
  ggplot2::ggsave("figures/event_m2.pdf", ggdid(event_effect_controls), width = 6, height = 3.5)
  message("Wrote figures/event_m1.pdf and figures/event_m2.pdf")
}
