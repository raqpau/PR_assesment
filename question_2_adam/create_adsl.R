###############################################################################
# Question 2 - Create ADSL
###############################################################################
###############################################################################
# Log & output folder define
###############################################################################

output_dir <- "question_2_adam"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

log_file <- file.path(output_dir, "create_adsl.log")

###############################################################################
# Install packages if not already installed
###############################################################################

required_pkgs <- c(
  "admiral",
  "dplyr",
  "stringr",
  "pharmaversesdtm",
  "lubridate"
)

missing_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

###############################################################################
# Load packages
###############################################################################

library(admiral)
library(dplyr)
library(stringr)
library(pharmaversesdtm)
library(lubridate)


###############################################################################
# Read Data
###############################################################################

dm <- convert_blanks_to_na(pharmaversesdtm::dm)
ex <- convert_blanks_to_na(pharmaversesdtm::ex)
ae <- convert_blanks_to_na(pharmaversesdtm::ae)
vs <- convert_blanks_to_na(pharmaversesdtm::vs)
ds <- convert_blanks_to_na(pharmaversesdtm::ds)

###############################################################################
# ADSL Base
###############################################################################

adsl <- dm %>%
  select(-DOMAIN)

###############################################################################
# AGEGR9 / AGEGR9N
###############################################################################

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    )
  )

###############################################################################
# TRTSDTM / TRTSTMF
###############################################################################

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    date_imputation = "none",
    time_imputation = "first"
  )
#check/review of imputations
ex_ext %>%
  select(USUBJID, EXSTDTC, EXSTDTM, EXSTTMF) %>%
  filter(!is.na(EXSTTMF)) %>%
  head()


adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    by_vars = exprs(STUDYID, USUBJID),
    
    filter_add =
      (EXDOSE > 0 |
         (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) &
      !is.na(EXSTDTM),
    
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    
    new_vars = exprs(
      TRTSDTM = EXSTDTM,
      TRTSTMF = EXSTTMF
    )
  )

###############################################################################
# ITTFL
###############################################################################

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

###############################################################################
# TRTEDTM (required for LSTAVLDT derivation)
###############################################################################

ex_ext2 <- ex %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    date_imputation = "none",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext2,
    
    by_vars = exprs(STUDYID, USUBJID),
    
    filter_add =
      (EXDOSE > 0 |
         (EXDOSE == 0 & str_detect(toupper(EXTRT), "PLACEBO"))) &
      !is.na(EXENDTM),
    
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    
    new_vars = exprs(
      TRTEDTM = EXENDTM
    )
  )

adsl <- adsl %>%
  derive_vars_dtm_to_dt(
    source_vars = exprs(TRTEDTM)
  )

###############################################################################
# LSTAVLDT
###############################################################################

vs_alive <- vs %>%
  filter(
    !(is.na(VSSTRESN) & is.na(VSSTRESC))
  )

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    
    events = list(
      
      event(
        dataset_name = "vs_alive",
        condition = !is.na(VSDTC),
        order = exprs(VSDTC, VSSEQ),
        set_values_to = exprs(
          LSTAVLDT =
            convert_dtc_to_dt(
              VSDTC,
              highest_imputation = "M"
            )
        )
      ),
      
      event(
        dataset_name = "ae",
        condition = !is.na(AESTDTC),
        order = exprs(AESTDTC, AESEQ),
        set_values_to = exprs(
          LSTAVLDT =
            convert_dtc_to_dt(
              AESTDTC,
              highest_imputation = "M"
            )
        )
      ),
      
      event(
        dataset_name = "ds",
        condition = !is.na(DSSTDTC),
        order = exprs(DSSTDTC, DSSEQ),
        set_values_to = exprs(
          LSTAVLDT =
            convert_dtc_to_dt(
              DSSTDTC,
              highest_imputation = "M"
            )
        )
      ),
      
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDT),
        set_values_to = exprs(
          LSTAVLDT = TRTEDT
        )
      )
    ),
    
    source_datasets = list(
      vs_alive = vs_alive,
      ae = ae,
      ds = ds,
      adsl = adsl
    ),
    
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, event_nr),
    mode = "last",
    
    new_vars = exprs(LSTAVLDT)
  )

###############################################################################
# Select output variables
###############################################################################

adsl_final <- adsl %>%
  select(
    STUDYID,
    USUBJID,
    AGE,
    AGEGR9,
    AGEGR9N,
    TRTSDTM,
    TRTSTMF,
    ITTFL,
    LSTAVLDT,
    everything()
  )

###############################################################################
# Save output dataset in csv file
###############################################################################

write.csv(adsl,
          file.path(output_dir, "adsl.csv") ,
          row.names = FALSE)



