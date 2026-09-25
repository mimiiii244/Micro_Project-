 # -------------------------------------------------------------------------------------------------------------------------
# Staggered DiD: 2017-2022
# -------------------------------------------------------------------------------------------------------------------------

library(haven)
library(did)

 # Amy's set up
 # path <- "~/Micro stata/2. STATA 220c (Zip file 2 of 4 - Combined Data Files l-v) v2"
 # Manya's Set Up
 path <- "/Users/manya/Desktop/Microecom/2. STATA 220c (Zip file 2 of 4 - Combined Data Files l-v) v2"


# -------------------------------------------------------------------------------------------------------------------------
# Load Waves 17-22
# -------------------------------------------------------------------------------------------------------------------------

# Pre-treatment 
w17 <- read_dta(file.path(path, "Combined_q220c.dta"))   # 2017
w18 <- read_dta(file.path(path, "Combined_r220c.dta"))   # 2018
w19 <- read_dta(file.path(path, "Combined_s220c.dta"))   # 2019

w20 <- read_dta(file.path(path, "Combined_t220c.dta"))   # 2020 #Wave 20
w21 <- read_dta(file.path(path, "Combined_u220c.dta"))   # 2021 #Wave 21


w22 <- read_dta(file.path(path, "Combined_v220c.dta"))   # 2022 #Post-Treatment 


# -------------------------------------------------------------------------------------------------------------------------
# Keep variables of interest and give every wave the same variable names
# -------------------------------------------------------------------------------------------------------------------------

# Wave 17
p17 <- w17[c(
  "xwaveid",
  "qhhpxid",
  "qlsrelsp",
  "qhgage",
  "qhgsex",
  "qhhstate",
  "qjbmh"
)]

names(p17) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p17$year <- 2017


# Wave 18
p18 <- w18[c(
  "xwaveid",
  "rhhpxid",
  "rlsrelsp",
  "rhgage",
  "rhgsex",
  "rhhstate",
  "rjbmh"
)]

names(p18) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p18$year <- 2018


# Wave 19
p19 <- w19[c(
  "xwaveid",
  "shhpxid",
  "slsrelsp",
  "shgage",
  "shgsex",
  "shhstate",
  "sjbmh"
)]

names(p19) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p19$year <- 2019


# Wave 20
p20 <- w20[c(
  "xwaveid",
  "thhpxid",
  "tlsrelsp",
  "thgage",
  "thgsex",
  "thhstate",
  "tjbmh"
)]

names(p20) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p20$year <- 2020


# Wave 21
p21 <- w21[c(
  "xwaveid",
  "uhhpxid",
  "ulsrelsp",
  "uhgage",
  "uhgsex",
  "uhhstate",
  "ujbmh"
)]

names(p21) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p21$year <- 2021


# Wave 22
p22 <- w22[c(
  "xwaveid",
  "vhhpxid",
  "vlsrelsp",
  "vhgage",
  "vhgsex",
  "vhhstate",
  "vjbmh"
)]

names(p22) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner",     # Partner's cross-wave person ID
  "sat",         # Satisfaction with partner
  "age",         # Age
  "sex",         # Sex
  "state",       # State/territory
  "wfh"          # Any usual working hours worked at home
)

p22$year <- 2022


# -------------------------------------------------------------------------------------------------------------------------
# 2019 baseline controls
# -------------------------------------------------------------------------------------------------------------------------

# Controls are taken from 2019 so they are measured before
# the COVID-era WFH treatment begins.

controls19 <- w19[c(
  "xwaveid",
  "shgage",      # Age in 2019
  "shgsex",      # Sex
  "shhstate"     # State/territory in 2019
)]

names(controls19) <- c(
  "xwaveid",     # Cross-wave person ID
  "age19",       # Age in 2019
  "sex19",       # Sex
  "state19"      # State/territory in 2019
)


# -------------------------------------------------------------------------------------------------------------------------
# Stack all waves into long format
# -------------------------------------------------------------------------------------------------------------------------

long <- rbind(
  p17,
  p18,
  p19,
  p20,
  p21,
  p22
)

long$xwaveid <- as.numeric(as.character(long$xwaveid))

nrow(long)
table(long$year)


# -------------------------------------------------------------------------------------------------------------------------
# Clean WFH and partner satisfaction variables
# -------------------------------------------------------------------------------------------------------------------------

# Replace HILDA negative missing values with NA
long$wfh[
  long$wfh >= -10 &
    long$wfh <= -1
] <- NA

long$sat[
  long$sat >= -10 &
    long$sat <= -1
] <- NA


# -------------------------------------------------------------------------------------------------------------------------
# Create WFH history for each person
# -------------------------------------------------------------------------------------------------------------------------

wfh_history <- as.data.frame(
  long[c(
    "xwaveid",
    "year",
    "wfh"
  )]
)

wfh_history <- reshape(
  wfh_history,
  idvar = "xwaveid",
  timevar = "year",
  direction = "wide"
)


# -------------------------------------------------------------------------------------------------------------------------
# Define staggered treatment year
# -------------------------------------------------------------------------------------------------------------------------

# g = year first treated
# g = 0 means never treated during 2019-2022

wfh_history$g <- NA


# First treated in 2020:
# not WFH in 2019, then WFH in 2020
wfh_history$g[
  wfh_history$wfh.2019 == 2 &
    wfh_history$wfh.2020 == 1
] <- 2020


 # First treated in 2021:
# not WFH in 2019 or 2020, then WFH in 2021
wfh_history$g[
  wfh_history$wfh.2019 == 2 &
    wfh_history$wfh.2020 == 2 &
    wfh_history$wfh.2021 == 1
] <- 2021


# First treated in 2022:
# not WFH in 2019-2021, then WFH in 2022
wfh_history$g[
  wfh_history$wfh.2019 == 2 &
    wfh_history$wfh.2020 == 2 &
    wfh_history$wfh.2021 == 2 &
    wfh_history$wfh.2022 == 1
] <- 2022


# Never treated during 2019-2022
wfh_history$g[
  wfh_history$wfh.2019 == 2 &
    wfh_history$wfh.2020 == 2 &
    wfh_history$wfh.2021 == 2 &
    wfh_history$wfh.2022 == 2
] <- 0


# Check treatment cohorts before restricting on satisfaction
table(
  wfh_history$g,
  useNA = "always"
)


# -------------------------------------------------------------------------------------------------------------------------
# Merge treatment year and 2019 baseline controls into long data
# -------------------------------------------------------------------------------------------------------------------------

# Add treatment year g to every yearly row for the same person
long <- merge(
  long,
  wfh_history[c(
    "xwaveid",
    "g"
  )],
  by = "xwaveid",
  all.x = TRUE
)

# Make sure ID types match
controls19$xwaveid <- as.numeric(as.character(controls19$xwaveid))

# Add 2019 baseline controls to every yearly row for the same person
long <- merge(
  long,
  controls19,
  by = "xwaveid",
  all.x = TRUE
)


# -------------------------------------------------------------------------------------------------------------------------
# Final staggered DiD analytic sample
# -------------------------------------------------------------------------------------------------------------------------

# Keep observations that:
# i) can be assigned to a valid treatment cohort
# ii) have partner satisfaction recorded in that person-year
# iii) have the same partner in 2019 and 2020
#
# We do NOT require people to:
# - be partnered specifically in 2019
# - have satisfaction observed in every year
# - have a balanced 2017-2022 panel

# Keep valid treatment cohorts with observed satisfaction
did_sample <- long[
  !is.na(long$g) &
    !is.na(long$sat),
]

# Keep only people who have the same partner across all observations
same_partner_ids <- tapply(
  seq_len(nrow(did_sample)),
  did_sample$xwaveid,
  function(idx) {
    d <- did_sample[idx, ]
    d <- d[d$year %in% 2019:2022, ]
    
    length(unique(d$year)) == 4 &&
      all(!is.na(d$partner)) &&
      length(unique(d$partner)) == 1
  }
)

did_sample <- did_sample[
  did_sample$xwaveid %in% names(same_partner_ids)[same_partner_ids],
]

nrow(did_sample)
length(unique(did_sample$xwaveid)) #2812 unique individuals left

# -------------------------------------------------------------------------------------------------------------------------
# Check final treatment groups
# -------------------------------------------------------------------------------------------------------------------------

group_counts <- did_sample[
  !duplicated(did_sample$xwaveid),
]

table(
  group_counts$g,
  useNA = "always"
)

# Number of unique people available to the staggered DiD
length(unique(did_sample$xwaveid))

# ---- Summary Statistics ----

# ---- Table: How the Analytical Sample Was Built ----
# Track how many people are lost at each step

step1 <- length(unique(long$xwaveid))                                  # all people in long data
step2 <- length(unique(wfh_history$xwaveid[!is.na(wfh_history$g)]))    # have a valid treatment cohort
step3 <- length(unique(did_sample$xwaveid))                            # final sample after all restrictions

sample_steps <- data.frame(
  Step = c(
    "1. All individuals in HILDA panel (2017-2022)",
    "2. Have a valid treatment cohort (g) based on 2019-2022 WFH history",
    "3. Final analytical sample (same partner, non-missing satisfaction, 2019-2022)"
  ),
  N_Individuals = c(step1, step2, step3)
)

print(sample_steps)
write.csv(sample_steps, "sample_construction.csv", row.names = FALSE)

# Create a simple treated indicator (1 = ever treated, 0 = never treated)
did_sample$treated <- ifelse(did_sample$g > 0, 1, 0)

# Keep one row per person (baseline year, 2019) so each person is counted once
baseline <- did_sample[did_sample$year == 2019, ]

# Variables to summarize
vars <- c("sat", "wfh", "age19", "sex19")

# Simple function to get mean, sd, min, max, N for one variable
sumstat <- function(x) {
  x <- x[!is.na(x)]
  c(
    Mean = mean(x),
    SD   = sd(x),
    Min  = min(x),
    Max  = max(x),
    N    = length(x)
  )
}

# ---- Table 1: Overall summary statistics ----
overall_table <- sapply(baseline[vars], sumstat)
overall_table <- round(t(overall_table), 2)
print(overall_table)

# ---- Table 2: Summary statistics by treatment status ----
# Split baseline data into treated and control groups
treated_grp <- baseline[baseline$treated == 1, ]
control_grp <- baseline[baseline$treated == 0, ]

treated_stats <- round(t(sapply(treated_grp[vars], sumstat)), 2)
control_stats <- round(t(sapply(control_grp[vars], sumstat)), 2)

# Combine into one table, side by side
balance_table <- data.frame(
  Variable       = vars,
  Treated_Mean   = treated_stats[, "Mean"],
  Treated_SD     = treated_stats[, "SD"],
  Control_Mean   = control_stats[, "Mean"],
  Control_SD     = control_stats[, "SD"]
)
print(balance_table)

# ---- Table: Relationship Satisfaction Change (2019 to 2022) ----
# Compare each person's satisfaction in 2019 vs 2022

sat_wide <- reshape(
  did_sample[c("xwaveid", "year", "sat")],
  idvar = "xwaveid",
  timevar = "year",
  direction = "wide"
)

sat_wide$change_19_22 <- sat_wide$sat.2022 - sat_wide$sat.2019

sat_change_table <- data.frame(
  Statistic = c("Mean satisfaction 2019", "Mean satisfaction 2022",
                "Mean change (2022-2019)", "SD of change", "N"),
  Value = c(
    round(mean(sat_wide$sat.2019, na.rm = TRUE), 2),
    round(mean(sat_wide$sat.2022, na.rm = TRUE), 2),
    round(mean(sat_wide$change_19_22, na.rm = TRUE), 2),
    round(sd(sat_wide$change_19_22, na.rm = TRUE), 2),
    sum(!is.na(sat_wide$change_19_22))
  )
)

print(sat_change_table)
write.csv(sat_change_table, "satisfaction_change.csv", row.names = FALSE)

# ---- Pre-Trends Check ----
# Goal: Confirm treated and control groups had similar trends in satisfaction
# BEFORE treatment starts (2020), which supports the parallel trends assumption

# Use did_sample, but include all pre-treatment years (2017-2019)
pretrend_data <- did_sample[did_sample$year %in% 2017:2019, ]

# Calculate average satisfaction by year and treatment group
avg_by_group <- aggregate(
  sat ~ year + treated,
  data = pretrend_data,
  FUN = mean,
  na.rm = TRUE
)

print(avg_by_group)

# ---- Plot the pre-trends ----
# Split into treated and control for plotting
treated_avg <- avg_by_group[avg_by_group$treated == 1, ]
control_avg <- avg_by_group[avg_by_group$treated == 0, ]

# Simple base R line plot
plot(
  treated_avg$year, treated_avg$sat,
  type = "b", pch = 16, col = "blue",
  ylim = c(min(avg_by_group$sat) - 0.5, max(avg_by_group$sat) + 0.5),
  xlab = "Year", ylab = "Average Satisfaction with Partner",
  main = "Pre-Trends: Treated vs Control (2017-2019)"
)
lines(control_avg$year, control_avg$sat, type = "b", pch = 17, col = "red")

legend(
  "bottomright",
  legend = c("Treated (started WFH)", "Control (never WFH)"),
  col = c("blue", "red"),
  pch = c(16, 17)
)

# ---- Save the plot ----
dev.copy(png, "pretrends_plot.png", width = 800, height = 600)
dev.off()

# -------------------------------------------------------------------------------------------------------------------------
# Model 1: Unadjusted staggered DiD
# -------------------------------------------------------------------------------------------------------------------------

did_model <- att_gt(
  yname = "sat",
  tname = "year",
  idname = "xwaveid",
  gname = "g",
  data = did_sample,
  control_group = "notyettreated",
  panel = TRUE,
  allow_unbalanced_panel = TRUE
)


# -------------------------------------------------------------------------------------------------------------------------
# Model 1: Overall ATT
# -------------------------------------------------------------------------------------------------------------------------

overall_effect <- aggte(
  did_model,
  type = "simple"
)

summary(overall_effect)


# -------------------------------------------------------------------------------------------------------------------------
# Model 1: Event study
# -------------------------------------------------------------------------------------------------------------------------

event_effect <- aggte(
  did_model,
  type = "dynamic"
)

summary(event_effect)

ggdid(event_effect)


# -------------------------------------------------------------------------------------------------------------------------
# Model 2: Adjusted staggered DiD
# -------------------------------------------------------------------------------------------------------------------------

# Baseline controls:
# age19   = age before treatment
# sex19   = sex
# state19 = baseline state/territory
#
# State is included as state indicators using factor(state19).
# Individual and time fixed effects are not manually added because
# Callaway-Sant'Anna estimates group-time treatment effects using
# within-period outcome changes.

did_model_controls <- att_gt(
  yname = "sat",
  tname = "year",
  idname = "xwaveid",
  gname = "g",
  xformla = ~ age19 +
    sex19 +
    factor(state19),
  data = did_sample,
  control_group = "notyettreated",
  panel = TRUE,
  allow_unbalanced_panel = TRUE
)


# -------------------------------------------------------------------------------------------------------------------------
# Model 2: Overall ATT with controls
# -------------------------------------------------------------------------------------------------------------------------

overall_effect_controls <- aggte(
  did_model_controls,
  type = "simple"
)

summary(overall_effect_controls)


# -------------------------------------------------------------------------------------------------------------------------
# Model 2: Event study with controls
# -------------------------------------------------------------------------------------------------------------------------

event_effect_controls <- aggte(
  did_model_controls,
  type = "dynamic"
)

summary(event_effect_controls)

ggdid(event_effect_controls)


# -------------------------------------------------------------------------------------------------------------------------
# Number of people contributing to each post-treatment ATT
# -------------------------------------------------------------------------------------------------------------------------

sat_history <- reshape(
  as.data.frame(
    did_sample[c(
      "xwaveid",
      "year",
      "sat",
      "g"
    )]
  ),
  idvar = c(
    "xwaveid",
    "g"
  ),
  timevar = "year",
  direction = "wide"
)

att_n <- data.frame()

for (g in c(2020, 2021, 2022)) {
  
  for (t in g:2022) {
    
    base <- g - 1
    
    treated_n <- sum(
      sat_history$g == g &
        !is.na(
          sat_history[[paste0("sat.", base)]]
        ) &
        !is.na(
          sat_history[[paste0("sat.", t)]]
        )
    )
    
    control_n <- sum(
      (
        sat_history$g == 0 |
          sat_history$g > t
      ) &
        !is.na(
          sat_history[[paste0("sat.", base)]]
        ) &
        !is.na(
          sat_history[[paste0("sat.", t)]]
        )
    )
    
    att_n <- rbind(
      att_n,
      c(
        g,
        t,
        base,
        treated_n,
        control_n
      )
    )
  }
}

names(att_n) <- c(
  "Cohort",
  "Year",
  "Base_year",
  "Treated_N",
  "Control_N"
)

att_n
