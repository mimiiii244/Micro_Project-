# -------------------------------------------------------------------------------------------------------------------------
# Staggered DiD: 2015-2022
# -------------------------------------------------------------------------------------------------------------------------

library(haven)
library(did)

path <- "~/Micro stata/2. STATA 220c (Zip file 2 of 4 - Combined Data Files l-v) v2"


# -------------------------------------------------------------------------------------------------------------------------
# Load Waves 15-22
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

p15 <- w15[c(
  "xwaveid",
  "ohhpxid",
  "olsrelsp",
  "ohgage",
  "ohgsex",
  "ohhstate",
  "ojbmh"
)]

names(p15) <- c(
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p15$year <- 2015


p16 <- w16[c(
  "xwaveid",
  "phhpxid",
  "plsrelsp",
  "phgage",
  "phgsex",
  "phhstate",
  "pjbmh"
)]

names(p16) <- c(
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p16$year <- 2016


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p17$year <- 2017


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p18$year <- 2018


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p19$year <- 2019


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p20$year <- 2020


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p21$year <- 2021


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
  "xwaveid",
  "partner",
  "sat",
  "age",
  "sex",
  "state",
  "wfh"
)

p22$year <- 2022


# -------------------------------------------------------------------------------------------------------------------------
# Stack all waves into long format
# -------------------------------------------------------------------------------------------------------------------------

long <- rbind(
  p15,
  p16,
  p17,
  p18,
  p19,
  p20,
  p21,
  p22
)

nrow(long)

table(long$year)
