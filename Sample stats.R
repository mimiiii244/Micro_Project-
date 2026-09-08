# HILDA Analysis: WFH during COVID and Partner Satisfaction

library(haven)
library(dplyr)
library(purrr)
library(labelled)

# -----------------------------
# 1. Load data
# -----------------------------
path <- "~/Desktop/Microecom/2. SAS 220c (Zip file 2 of 4 - Combined Data Files l-v) v2"

cat("Loading data...\n")
w19 <- read_sas(file.path(path, "combined_s220c.sas7bdat"))
w20 <- read_sas(file.path(path, "combined_t220c.sas7bdat"))
w21 <- read_sas(file.path(path, "combined_u220c.sas7bdat"))

# -----------------------------
# 2. Variable definitions (source variable names in raw data)
# -----------------------------
# Wave 19 (baseline, pre-COVID)
id_19        <- "xwaveid"
partner_19   <- "shhpxid"
mrcms_19     <- "smrcms"
ordf_19      <- "sordf"
sat_19       <- "slsrelsp"
age_19       <- "shgage"
sex_19       <- "shgsex"

# Wave 20 (COVID, exposure and outcome)
id_20        <- "xwaveid"
wfh_20       <- "tcvwfh"
sat_20       <- "tlsrelsp"

# Wave 21 (follow-up)
id_21        <- "xwaveid"
partner_21   <- "uhhpxid"
mrcms_21     <- "umrcms"
ordf_21      <- "uordf"

# -----------------------------
# 3. HILDA missing-value handling
# -----------------------------
hilda_na <- function(x) {
  if (is.numeric(x)) {
    x[x <= -1 & x >= -10] <- NA_real_
  }
  x
}

clean_wave <- function(dat) {
  dat |> mutate(across(where(is.numeric), hilda_na))
}

w19 <- clean_wave(w19)
w20 <- clean_wave(w20)
w21 <- clean_wave(w21)

# -----------------------------
# 4. Build person-level files
# -----------------------------
cat("\n=== Building person-level files ===\n")

p19 <- w19 |>
  transmute(
    xwaveid   = .data[[id_19]],
    partner19 = as.character(.data[[partner_19]]),
    ordf19    = .data[[ordf_19]],
    marital19 = .data[[mrcms_19]],
    sat19     = .data[[sat_19]],
    age19     = .data[[age_19]],
    sex19     = .data[[sex_19]]
  )

p20 <- w20 |>
  transmute(
    xwaveid = .data[[id_20]],
    wfh20   = .data[[wfh_20]],
    sat20   = .data[[sat_20]]
  )

p21 <- w21 |>
  transmute(
    xwaveid   = .data[[id_21]],
    partner21 = as.character(.data[[partner_21]]),
    ordf21    = .data[[ordf_21]],
    marital21 = .data[[mrcms_21]]
  )

cat("Wave 19 records:", nrow(p19), "\n")
cat("Wave 20 records:", nrow(p20), "\n")
cat("Wave 21 records:", nrow(p21), "\n")

# -----------------------------
# 5. Merge waves
# -----------------------------
panel <- p19 |>
  left_join(p20, by = "xwaveid") |>
  left_join(p21, by = "xwaveid")

cat("Merged panel records:", nrow(panel), "\n")

# -----------------------------
# 6. Define partnered status and same partner
# -----------------------------
# Codes: 1 = Married, 2 = De facto
panel <- panel |>
  mutate(
    # Partnered in wave 19: married OR de facto OR living with partner
    partnered19 = (marital19 %in% c(1, 2)) | (ordf19 == 1),
    
    # Partnered in wave 21: married OR de facto OR living with partner
    partnered21 = (marital21 %in% c(1, 2)) | (ordf21 == 1),
    
    # Same partner: must be partnered in both waves AND have same partner ID
    same_partner = partnered19 & partnered21 &
      !is.na(partner19) &
      !is.na(partner21) &
      partner19 != "" &
      partner21 != "" &
      partner19 == partner21,
    
    sat_change20 = sat20 - sat19
  )

cat("\n=== Partnered status ===\n")
cat("Partnered in wave 19:", sum(panel$partnered19, na.rm = TRUE), "\n")
cat("Partnered in wave 21:", sum(panel$partnered21, na.rm = TRUE), "\n")
cat("Same partner in 19 and 21:", sum(panel$same_partner, na.rm = TRUE), "\n")

# -----------------------------
# 7. Sample flow
# -----------------------------
sample_flow <- tibble(
  stage = c(
    "Linked 19-20-21",
    "Partnered in wave 19",
    "Partnered in wave 21",
    "Same partner in 19 and 21",
    "Non-missing WFH (wave 20)",
    "Non-missing partner sat (19 and 20)",
    "Final analytic sample"
  ),
  n = c(
    nrow(panel),
    sum(panel$partnered19, na.rm = TRUE),
    sum(panel$partnered21, na.rm = TRUE),
    sum(panel$same_partner, na.rm = TRUE),
    sum(
      panel$partnered19 &
        panel$partnered21 &
        panel$same_partner &
        !is.na(wfh_20),
      na.rm = TRUE
    ),
    sum(
      panel$partnered19 &
        panel$partnered21 &
        panel$same_partner &
        !is.na(wfh_20) &
        !is.na(sat_19) &
        !is.na(sat_20),
      na.rm = TRUE
    ),
    NA_integer_
  )
)

# -----------------------------
# 8. Final analytic sample
# -----------------------------
analytic <- panel |>
  filter(
    partnered19,
    partnered21,
    same_partner,
    !is.na(wfh20),      # wfh20, NOT wfh_20
    !is.na(sat19),      # sat19, NOT sat_19
    !is.na(sat20)       # sat20, NOT sat_20
  )

sample_flow$n[7] <- nrow(analytic)

cat("\n=== SAMPLE FLOW ===\n")
print(sample_flow)

# -----------------------------
# 9. WFH exposure groups
# -----------------------------
cat("\n=== WFH variable (tcvwfh) distribution ===\n")
print(table(w20$tcvwfh, useNA = "always"))

analytic <- analytic |>
  mutate(
    wfh_group = case_when(
      wfh20 == 1 ~ "Started/increased WFH",    # wfh20, NOT wfh_20
      wfh20 == 2 ~ "Did not start/increase WFH",
      TRUE ~ NA_character_
    ),
    wfh_group = factor(
      wfh_group,
      levels = c(
        "Did not start/increase WFH",
        "Started/increased WFH"
      )
    )
  )

cat("\nWFH group distribution:\n")
print(table(analytic$wfh_group, useNA = "always"))

# -----------------------------
# 10. Descriptive statistics
# -----------------------------
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

descriptives <- analytic |>
  group_by(wfh_group) |>
  summarise(
    n = n(),
    
    # Baseline demographics (wave 19)
    age_mean = mean(age19, na.rm = TRUE),      # age19, NOT age_19
    age_sd = sd(age19, na.rm = TRUE),
    
    female_percent = mean(sex19 == 2, na.rm = TRUE) * 100,  # sex19, NOT sex_19
    
    # Partner satisfaction wave 19
    sat19_mean = mean(sat19, na.rm = TRUE),    # sat19, NOT sat_19
    sat19_sd = sd(sat19, na.rm = TRUE),
    
    # Partner satisfaction wave 20
    sat20_mean = mean(sat20, na.rm = TRUE),    # sat20, NOT sat_20
    sat20_sd = sd(sat20, na.rm = TRUE),
    
    # Change in satisfaction (20 - 19)
    sat_change_mean = mean(sat_change20, na.rm = TRUE),
    sat_change_sd = sd(sat_change20, na.rm = TRUE),
    
    .groups = "drop"
  )

print(descriptives)

write.csv(descriptives, "descriptives_wfh_partner_satisfaction.csv", row.names = FALSE)

# -----------------------------
# 11. Summary
# -----------------------------
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Output files:\n")
cat("  - descriptives_wfh_partner_satisfaction.csv\n")
cat("\nKey numbers:\n")
cat("1. Final sample size:", nrow(analytic), "\n")
cat("2. WFH exposure:", sum(analytic$wfh_group == "Started/increased WFH"), "people\n")
if(nrow(descriptives) >= 2) {
  cat("3. Mean satisfaction change (WFH group):", 
      round(descriptives$sat_change_mean[descriptives$wfh_group == "Started/increased WFH"], 3), "\n")
  cat("4. Mean satisfaction change (No WFH group):", 
      round(descriptives$sat_change_mean[descriptives$wfh_group == "Did not start/increase WFH"], 3), "\n")
}