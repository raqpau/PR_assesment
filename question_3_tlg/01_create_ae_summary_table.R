###############################################################################
# Program: 01_create_ae_summary_table.R
# Purpose: Question 3 - TEAE Summary Table using gtsummary
###############################################################################
#==============================================================================
# Output folder
#==============================================================================

output_dir <- "question_3_tlg"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
#----------------------------------------------------------------------------
# Log
#----------------------------------------------------------------------------
log_file <- file(file.path(output_dir, "01_create_ae_summary_table.log"),
                 open = "wt")
cat("Program started:", format(Sys.time()), "\n")

#----------------------------------------------------------------------------
# Libraries
#---------------------------------------------------------------------------

list.of.packages <- c("dplyr", "gtsummary", "gt", "pharmaverseadam")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(dplyr)
library(gtsummary)
library(gt)
library(pharmaverseadam)
#----------------------------------------------------------------------------
# Read Input Files
#----------------------------------------------------------------------------
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

cat("ADSL records:", nrow(adsl), "\n")
cat("ADAE records:", nrow(adae), "\n")

#----------------------------------------------------------------------------
# Safety Population Denominator
#----------------------------------------------------------------------------
adsl_saf <- adsl %>%
  filter(SAFFL == "Y")

#----------------------------------------------------------------------------
# Treatment-Emergent Adverse Events
#----------------------------------------------------------------------------
adae_teae <- adae %>%
  filter(TRTEMFL == "Y")
cat("TEAE records:", nrow(adae_teae), "\n")

#----------------------------------------------------------------------------
# Calculate SOC/PT Frequency for Sorting
#----------------------------------------------------------------------------

soc_order <- adae_teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, sort = TRUE)

pt_order <- adae_teae %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, sort = TRUE)

adae_teae <- adae_teae %>%
  mutate(
    AESOC = factor(
      AESOC,
      levels = soc_order$AESOC
    ),
    AETERM = factor(
      AETERM,
      levels = pt_order$AETERM
    )
  )

#----------------------------------------------------------------------------
# FDA Table 10 Style Summary
#----------------------------------------------------------------------------
tbl_ae <- adae_teae %>%
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl_saf,
    overall_row = TRUE,
    label = list(
      AESOC ~ "Primary System Organ Class",
      AETERM ~ "Reported Term for the Adverse Event",
      "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
    )
  ) %>%
  add_overall()

#----------------------------------------------------------------------------
# Convert to GT
#----------------------------------------------------------------------------
gt_tbl <- as_gt(tbl_ae)



#----------------------------------------------------------------------------
# Save Output
#----------------------------------------------------------------------------
gtsave(
  gt_tbl,
  file.path(output_dir, "ae_summary_table.html")
)

cat("Program completed:", format(Sys.time()), "\n")
