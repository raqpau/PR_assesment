###############################################################################
# Question 1 - SDTM DS Domain
###############################################################################

###############################################################################
# Log & output folder define
###############################################################################

output_dir <- "question_1_sdtm"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

log_file <- file.path(output_dir, "01_create_ds_domain.log")

###############################################################################
# Install Packages If Missing
###############################################################################

required_packages <- c(
  "sdtm.oak",
  "pharmaverseraw",
  "pharmaversesdtm",
  "dplyr",
  "crayon"
)

installed_packages <- rownames(installed.packages())

missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(
  lapply(required_packages, library, character.only = TRUE)
)

###############################################################################
# Input Data
###############################################################################

cat("Loading input datasets...\n")

ds_raw <- pharmaverseraw::ds_raw
dm <- pharmaversesdtm::dm

study_ct <- read.csv(
  file.path(output_dir, "sdtm_ct.csv"),
  stringsAsFactors = FALSE
)

###############################################################################
# Generate Oak ID Variables
###############################################################################

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

###############################################################################
# Date/Time Variables
###############################################################################

# aCRF mapping specifies IT.DSSDAT -> DSSTDTC.
# IT.DSSDAT does not exist in ds_raw; IT.DSSTDAT is used instead.

ds <-
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "m-d-y",
    id_vars = oak_id_vars()
  ) %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = list(
      c("m-d-y"),
      "H:M"
    ),
    raw_unk = c("UN", "UNK"),
    id_vars = oak_id_vars()
  )

###############################################################################
# Topic Variables
###############################################################################


ds <- ds |>
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

###############################################################################
# Controlled Terminology
###############################################################################

ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

###############################################################################
# aCRF Logic
###############################################################################


ds <- ds %>%
  mutate(
    DSTERM = case_when(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "" ~ ds_raw$OTHERSP,
      TRUE ~ DSTERM
    ),
    DSDECOD = case_when(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "" ~ toupper(ds_raw$OTHERSP),
      TRUE ~ DSDECOD
    ),
    DSCAT = case_when(
      !is.na(ds_raw$OTHERSP) & ds_raw$OTHERSP != "" ~ "OTHER EVENT",
      toupper(ds_raw$IT.DSDECOD) == "RANDOMIZED" ~ "PROTOCOL MILESTONE",
      TRUE ~ "DISPOSITION EVENT"
    ),
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM)
  )

###############################################################################
# DSSEQ
###############################################################################
ds <- ds %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  )

###############################################################################
# Study Day
###############################################################################
ds <- derive_study_day(
  sdtm_in = ds,
  dm_domain = dm,
  tgdt = "DSSTDTC",
  refdt = "RFXSTDTC",
  study_day_var = "DSSTDY"
)

###############################################################################
# Final Variables
###############################################################################

ds <- ds %>%
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    DSSEQ,
    DSTERM,
    DSDECOD,
    DSCAT,
    VISITNUM,
    VISIT,
    DSDTC,
    DSSTDTC,
    DSSTDY
  )

###############################################################################
# Outputs
###############################################################################

cat("\nFinal dataset dimensions:\n")
print(dim(ds))

cat("\nVariable listing:\n")
print(names(ds))

write.csv(
  ds,
  file.path(output_dir, "ds.csv"),
  row.names = FALSE
)

cat("Run completed:", as.character(Sys.time()), "\n")
