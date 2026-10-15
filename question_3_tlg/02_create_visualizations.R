###############################################################################
# Program: 02_create_visualizations.R
# Purpose: Create AE visualizations for Coding Assessment Question 3
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
log_file <- file(file.path(output_dir, "02_create_visualizations.log"),
                 open = "wt")
cat("Program started:", format(Sys.time()), "\n")
#==============================================================================
# Packages
#==============================================================================

required_packages <- c(
  "dplyr",
  "ggplot2",
  "readr",
  "scales",
  "binom"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(binom)

#==============================================================================
# Read data
#==============================================================================

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

#==============================================================================
# Keep Treatment-Emergent AEs
#==============================================================================

adae_teae <- adae %>%
  filter(TRTEMFL == "Y")

#==============================================================================
# Plot 1
# AE Severity Distribution by Treatment
#==============================================================================

severity_plot_data <- adae_teae %>%
  filter(
    !is.na(ACTARM),
    !is.na(AESEV)
  ) %>%
  count(ACTARM, AESEV)

p1 <- ggplot(
  severity_plot_data,
  aes(
    x = ACTARM,
    y = n,
    fill = AESEV
  )
) +
  geom_col() +
  labs(
    title = "AE Severity Distribution by Treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(
  filename = file.path(
    output_dir,
    "ae_severity_distribution.png"
  ),
  plot = p1,
  width = 8,
  height = 6,
  dpi = 300
)

#==============================================================================
# Plot 2
# Top 10 Most Frequent AEs with 95% CIs
#==============================================================================

# Denominator
n_subjects <- adsl %>%
  filter(SAFFL == "Y") %>%
  distinct(USUBJID) %>%
  nrow()

# Unique-subject incidence by AE term
top10_ae <- adae_teae %>%
  filter(!is.na(AETERM)) %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "subjects") %>%
  arrange(desc(subjects)) %>%
  slice_head(n = 10)

# Clopper-Pearson CI
ci_tbl <- binom.confint(
  x = top10_ae$subjects,
  n = n_subjects,
  methods = "exact"
)

top10_ae <- top10_ae %>%
  mutate(
    pct = subjects / n_subjects * 100,
    lower = ci_tbl$lower * 100,
    upper = ci_tbl$upper * 100
  ) %>%
  arrange(pct)

p2 <- ggplot(
  top10_ae,
  aes(
    x = pct,
    y = reorder(AETERM, pct)
  )
) +
  geom_errorbarh(
    aes(
      xmin = lower,
      xmax = upper
    ),
    height = 0.2
  ) +
  geom_point(size = 3) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0(
      "N = ",
      n_subjects,
      " subjects, 95% Clopper-Pearson CIs"
    ),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(
    output_dir,
    "top10_adverse_events.png"
  ),
  plot = p2,
  width = 8,
  height = 6,
  dpi = 300
)

#==============================================================================
# Validation of grapgs - review counts
#==============================================================================

severity_plot_data <- adae_teae %>%
  filter(
    !is.na(ACTARM),
    !is.na(AESEV)
  ) %>%
  count(
    ACTARM,
    AESEV,
    name = "AE_COUNT"
  ) %>%
  arrange(ACTARM, AESEV)
severity_plot_data

top10_ae_qc <- top10_ae %>%
  select(
    AETERM,
    subjects,
    pct,
    lower,
    upper
  ) %>%
  rename(
    N_SUBJ = subjects,
    PERCENT = pct,
    LOWER_95CI = lower,
    UPPER_95CI = upper
  )
top10_ae_qc

#==============================================================================
# Log Messages
#==============================================================================

cat("AE visualizations created successfully\n")
cat("Output files:\n")
cat("- ae_severity_distribution.png\n")
cat("- top10_adverse_events.png\n")
