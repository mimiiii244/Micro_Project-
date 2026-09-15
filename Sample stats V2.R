library(haven)

path <- "~/Micro stata/2. STATA 220c (Zip file 2 of 4 - Combined Data Files l-v) v2"

w19 <- read_dta(file.path(path, "Combined_s220c.dta")) #2019 data
w20 <- read_dta(file.path(path, "Combined_t220c.dta")) #2020 data
w21 <- read_dta(file.path(path, "Combined_u220c.dta")) #2021 data

# ====================================
# Cbind variables of Interest and rename 
# ====================================

# Wave 19
p19 <- w19[c(
  "xwaveid",
  "shhpxid",
  "smrcms",
  "sordf",
  "slsrelsp",
  "shgage",
  "shgsex",
  "shhstate",
  "sjbmh"
)]

names(p19) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner19",   # Partner's person ID in 2019
  "marital19",   # Current marital status in 2019
  "defacto19",   # Living with someone in a non-marital relationship in 2019
  "sat19",       # Satisfaction with partner in 2019
  "age19",       # Age in 2019
  "sex19",       # Sex in 2019
  "state19",     # State/territory in 2019
  "wfh19"        # Any usual working hours worked at home in 2019
)


# Wave 20
p20 <- w20[c(
  "xwaveid",
  "thhpxid",
  "tmrcms",
  "tordf",
  "tcvwfh",
  "tlsrelsp",
  "thhstate",
  "tjbmh"
)]

names(p20) <- c(
  "xwaveid",       # Cross-wave person ID
  "partner20",     # Partner's person ID in 2020
  "marital20",     # Current marital status in 2020
  "defacto20",     # Living with someone in a non-marital relationship in 2020
  "covid_wfh20",   # Started/increased WFH because of COVID
  "sat20",         # Satisfaction with partner in 2020
  "state20",       # State/territory in 2020
  "wfh20"          # Any usual working hours worked at home in 2020
)

p21 <- w21[c(
  "xwaveid",
  "uhhpxid",
  "umrcms",
  "uordf",
  "ulsrelsp",
  "uhhstate",
  "ujbmh"
)]

names(p21) <- c(
  "xwaveid",     # Cross-wave person ID
  "partner21",   # Partner's person ID in 2021
  "marital21",   # Current marital status in 2021
  "defacto21",   # Living with someone in a non-marital relationship in 2021
  "sat21",       # Satisfaction with partner in 2021
  "state21",     # State/territory in 2021
  "wfh21"        # Any usual working hours worked at home in 2021
)

# =====================================================
# Merge waves 19,20,21 into one dataframe 
# =====================================================
panel <- merge(p19, p20, by = "xwaveid", all.x = TRUE)
panel <- merge(panel, p21, by = "xwaveid", all.x = TRUE)
nrow(panel) # 23263 in total since it keeps all the observations in 2019 with a left join

# --------------------------------------------------------------------------------
# Descriptive statistics: how many people in each wave? 
# -------------------------------------------------------------------------------
# Check how many people in each wave 19 & 20 
# Total people in Wave 19
nrow(p19) # 23263

# Of the Wave 19 people, how many are also observed in Wave 21?
sum(panel$xwaveid %in% p21$xwaveid) 
# 20461 so 2802 dropped out between those 2 years and our final sample size is 20461 people 

# How many have the same co-resident partner in both 2019 and 2021?
sum(panel$same_partner == 1, na.rm = TRUE) # 9766


# -------------------------------------------------------------------------------------------------------------------------
# Descriptive statistics: 
  # i) Check how many are partnered in wave 19 and 21
  # ii) check whether they still have the same partner, a new partner, or no partner
# ------------------------------------------------------------------------------------------------------------------------
#Currently HILDA uses NA for non-partnered, we can convert this to a binary dummy instead 
panel$partnered19 <- as.numeric(
  !is.na(panel$partner19) &
    panel$partner19 != ""
)
panel$partnered21 <- as.numeric(
  !is.na(panel$partner21) &
    panel$partner21 != ""
)

# Wave 19 and 21 partner status
table(panel$partnered19) # Recall: out of 23263 obs, 11530 are partnered (49.6%)
mean(
  panel$partnered19[panel$xwaveid %in% p19$xwaveid]
) * 100

table(panel$partnered21) # Recall: out of 20461 obs,10352 are partnered (50.6%) 
mean(
  panel$partnered21[panel$xwaveid %in% p21$xwaveid]
) * 100

# Among people partnered in 2019 and still observed in 2021,
# Check whether they still have the same partner, a new partner, or no partner

# First, keep only people partnered in 2019 and still observed in 2021
relationship_sample <- panel[
  panel$partnered19 == 1 &
    panel$xwaveid %in% p21$xwaveid,
]
nrow(relationship_sample) #10270. Notice this dropped from 10352 since we excluded those not partnered in 2019. 

# Relationship outcome in 2021
relationship_sample$relationship21 <- ifelse(
  relationship_sample$partner21 == relationship_sample$partner19,
  "Same partner",
  ifelse(
    is.na(relationship_sample$partner21) |
      relationship_sample$partner21 == "",
    "No co-resident partner",
    "Different partner"
  )
)

# Counts
table(relationship_sample$relationship21)

# Percentages
prop.table(table(relationship_sample$relationship21)) * 100

# Hence, out of the 10270 obs, by 2021 
# Different partner : 36 
# No co-resident partner: 468 
# Same partner: 9766 

# As percentages: 
 #95.09% stayed with the same co-resident partner 
 #4.56% had no co-resident partner by 2021 (either genuinely no partner or their partner is not on 2021 Hilda dataset)
 #0.35% had a different co-resident partner (rare case where their partner in 2021 happened to also be an observation on Hilda dataset)


# ----------------------------------------------------------------------------
# Define co-resident partner status
# ----------------------------------------------------------------------------

# Partner ID is recorded for a partner in the same household
panel$coresident19 <- as.numeric(
  !is.na(panel$partner19) &
    panel$partner19 != ""
)

panel$coresident21 <- as.numeric(
  !is.na(panel$partner21) &
    panel$partner21 != ""
)

# Same co-resident partner in 2019 and 2021
panel$same_partner <- as.numeric(
  panel$coresident19 == 1 &
    panel$coresident21 == 1 &
    panel$partner19 == panel$partner21
)

# Check counts
sum(panel$coresident19 == 1, na.rm = TRUE)
sum(panel$coresident21 == 1, na.rm = TRUE)
sum(panel$same_partner == 1, na.rm = TRUE)

# -------------------------------------------------------------------------------------------------------------------------
# Check change in partner satisfaction
# -------------------------------------------------------------------------------------------------------------------------
panel$sat_change <- panel$sat21 - panel$sat19
mean(panel$sat_change,na.rm=TRUE)

# Number of missing sat_change observations
sum(!is.na(panel$sat21))
sum(is.na(panel$sat_change)) # 2802 missing satisfaction ratings out of the 23,263 obs in panel 


# -------------------------------------------------------------------------------------------------------------------------
# Final Dataframe: obs reduce one last time after removing rows without partner satisfaction
# -------------------------------------------------------------------------------------------------------------------------

# Idea: 
# Only keep those who: 
  # have the same co-resident partner in 2019 and 2021
  # have a valid COVID WFH treatment response
  # have partner satisfaction recorded in 2019
  # have partner satisfaction recorded in 2020

analytic <- panel[
  panel$same_partner == 1 &
    !is.na(panel$covid_wfh20) &
    !is.na(panel$sat19) &
    !is.na(panel$sat20),
]

# Final sample size
nrow(analytic) # 9696 
# 10270 - 9696 = 574 dropped out from missing satisfaction ratings 


# -------------------------------------------------------------------------------------------------------------------------
# Descriptive statistics: WFH timing cohorts in the final analytic sample
# -------------------------------------------------------------------------------------------------------------------------
# Creates a new column called cohort and fills every row with missing values first, 
# so that we  can then assign each observation into the correct WFH timing group.
analytic$cohort <- NA

analytic$cohort[analytic$wfh19 == 1] <- "Already WFH in 2019"

analytic$cohort[
  analytic$wfh19 == 2 &
    analytic$wfh20 == 1
] <- "Started in 2020"

analytic$cohort[
  analytic$wfh19 == 2 &
    analytic$wfh20 == 2 &
    analytic$wfh21 == 1
] <- "Started in 2021"

analytic$cohort[
  analytic$wfh19 == 2 &
    analytic$wfh20 == 2 &
    analytic$wfh21 == 2
] <- "Never WFH"

# Count WFH timing groups
table(analytic$cohort, useNA = "always")

# WFH timing groups by state
table(analytic$state19, analytic$cohort)

# Make this table a little more useful ...
# Add readable state names
analytic$state_name <- c(
  "NSW", "VIC", "QLD", "SA",
  "WA", "TAS", "NT", "ACT"
)[analytic$state19]

# Total observations by state
state_total <- as.data.frame(
  table(analytic$state19, analytic$state_name)
)

state_total <- state_total[state_total$Freq > 0, ]

names(state_total) <- c(
  "State code",
  "State",
  "Total observations"
)

# WFH cohort counts by state
state_cohort <- as.data.frame(
  table(analytic$state19, analytic$state_name, analytic$cohort)
)

state_cohort <- state_cohort[state_cohort$Freq > 0, ]

names(state_cohort) <- c(
  "State code",
  "State",
  "Cohort",
  "Count"
)

# Convert cohort counts to wide format
state_cohort_wide <- reshape(
  state_cohort,
  idvar = c("State code", "State"),
  timevar = "Cohort",
  direction = "wide"
)

# Merge totals and cohort counts
state_table <- merge(
  state_total,
  state_cohort_wide,
  by = c("State code", "State")
)

# Clean column names
names(state_table) <- c(
  "State code",
  "State",
  "Total observations",
  "Already WFH in 2019",
  "Never WFH",
  "Started in 2020",
  "Started in 2021"
)

# Sort by HILDA state code
state_table <- state_table[order(state_table$`State code`), ]
state_table


# -------------------------------------------------------------------------------------------------------------------------
# More Descriptive Statistics: by WFH exposure groups
# -------------------------------------------------------------------------------------------------------------------------

# Create treatment group
analytic$wfh_group <- ifelse(
  analytic$covid_wfh20 == 1,
  "Started/increased WFH",
  "Did not start/increase WFH"
)

# Number in each group
table(analytic$wfh_group)

# Percentage in each group
prop.table(table(analytic$wfh_group)) * 100


# -------------------------------------------------------------------------------------------------------------------------
# Mean satisfaction change by WFH group
# -------------------------------------------------------------------------------------------------------------------------

aggregate(
  sat_change ~ wfh_group,
  data = analytic,
  FUN = mean,
  na.rm = TRUE
)


# -------------------------------------------------------------------------------------------------------------------------
# Baseline characteristics by WFH group
# -------------------------------------------------------------------------------------------------------------------------

aggregate(
  age19 ~ wfh_group,
  data = analytic,
  FUN = mean,
  na.rm = TRUE
)

aggregate(
  sat19 ~ wfh_group,
  data = analytic,
  FUN = mean,
  na.rm = TRUE
)

# Female dummy: 1 = female
analytic$female <- as.numeric(analytic$sex19 == 2)

aggregate(
  female ~ wfh_group,
  data = analytic,
  FUN = mean,
  na.rm = TRUE
)

# Convert female proportion to percentage
aggregate(
  female ~ wfh_group,
  data = analytic,
  FUN = function(x) mean(x, na.rm = TRUE) * 100
)
