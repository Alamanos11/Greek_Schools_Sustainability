# =====================================================================
#  ROBUSTNESS CHECKS — alternative estimators for the Results section
#
#  Sample : N = 391 teachers
#  Data   : national_N391_teaching_dataset.csv
#
#  Purpose: show that the four main effects in Model 1 do not depend on
#  the estimator, and that the ESD x Barriers interaction does. Each
#  section is self-contained and prints its own verdict.
#
#  Run order: this script is independent of the other two. It recomputes
#  the construct scores itself so it can be run on its own.
# =====================================================================

# ---- packages --------------------------------------------------------
# Installed one at a time against an explicit CRAN mirror, then verified.
# A bare install.packages() with no mirror set does nothing useful when a
# script is sourced: on Windows it either opens a mirror chooser or fails,
# the library() calls below then fail too, and execution carries on until
# every later line reports "object 'd' not found". The real cause scrolls
# away. So this block installs deliberately, checks, and STOPS if anything
# is still missing.
CRAN <- unname(getOption("repos")["CRAN"])
if (is.na(CRAN) || !startsWith(CRAN, "http")) CRAN <- "https://cloud.r-project.org"

pkgs <- c("tidyverse", "psych", "sandwich", "lmtest", "boot", "lavaan", "car")

need <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Installing missing package: ", p)
    try(install.packages(p, repos = CRAN), silent = TRUE)
  }
  requireNamespace(p, quietly = TRUE)
}
missing <- pkgs[!vapply(pkgs, need, logical(1))]

if (length(missing))
  stop("\n\n  These packages could not be installed or loaded:\n    ",
       paste(missing, collapse = ", "),
       "\n\n  FIX: paste this into the RStudio console, let it finish, then run\n",
       "  this script again from the top:\n\n",
       "    install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))\n\n",
       "  Nothing below this line can work until every package loads.\n\n",
       call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))

# robustbase is OPTIONAL. It supplies lmrob(), the MM-estimator. If it is
# not available the script falls back to MASS::rlm(), which ships with
# every R installation, and says which one it used. Section 3 is a
# robustness check, so it must never be the thing that stops the script.
HAVE_ROBUSTBASE <- need("robustbase")
if (HAVE_ROBUSTBASE) {
  library(robustbase)
} else {
  message("robustbase not available - section 3 will use MASS::rlm() instead of lmrob().")
}

set.seed(2026)
options(width = 200)

# ---- paths ----------------------------------------------------------
# No absolute path appears anywhere in this repository, so it can be
# unzipped and run from any location. The data file is located by
# searching, in order: the folder named by the ESD_PROJECT_DIR environment
# variable, the working directory, the folder holding this script, and the
# parents of those two, each with and without a "data" sub-folder.
#
# If the file is not found the script STOPS here. It must never fall
# through silently: `d` would then never be created and every later line
# would report "object 'd' not found", hiding the real cause.
DATA_FILE <- "national_N391_teaching_dataset.csv"

.script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) dirname(normalizePath(f[1], mustWork = FALSE)) else NULL
})

.roots <- unique(c(Sys.getenv("ESD_PROJECT_DIR"), getwd(), .script_dir,
                   file.path(c(getwd(), .script_dir), "..")))
.roots <- .roots[nzchar(.roots)]

DATA_DIR <- NA_character_
for (.r in .roots) for (.s in c(".", "data")) {
  if (is.na(DATA_DIR) && file.exists(file.path(.r, .s, DATA_FILE))) {
    DATA_DIR    <- normalizePath(file.path(.r, .s))
    PROJECT_DIR <- normalizePath(.r)
  }
}
if (is.na(DATA_DIR))
  stop("\n\n  Cannot find '", DATA_FILE, "'.\n",
       "  Looked under: ", paste(.roots, collapse = ", "), "\n\n",
       "  FIX: set the working directory to the repository folder, or set the\n",
       "  environment variable ESD_PROJECT_DIR to it, then run this script again\n",
       "  from the top. In RStudio: Session > Set Working Directory >\n",
       "  To Source File Location.\n\n", call. = FALSE)

# Paths are absolute from here on. The script deliberately does NOT call
# setwd(): changing the session's working directory is both an unwanted
# side effect and a way for a relative read to break silently.
DATA_PATH <- file.path(DATA_DIR, DATA_FILE)
OUT <- file.path(PROJECT_DIR, "outputs", "results_national")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)


# ---- data ------------------------------------------------------------
d <- tryCatch(
  read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
  error = function(e)
    stop("\n\n  Could not read the data file:\n    ", DATA_PATH,
         "\n\n  R reported: ", conditionMessage(e),
         "\n\n  Check the file is not open in Excel, and that the path above is right.\n\n",
         call. = FALSE))
if (!is.data.frame(d) || nrow(d) == 0)
  stop("The data file was read but contains no rows: ", DATA_PATH, call. = FALSE)

cat("Data:", DATA_PATH, "| N =", nrow(d), "\n\n")

rev_items <- c("BEL_3","BEL_5","BEL_7","BEL_9","ESD_3","ESD_7","EQD_3","EQD_5","EQD_9")
for (v in rev_items) d[[paste0("r", v)]] <- 6 - d[[v]]
d$rBEL_6 <- 6 - d$BEL_6

bel_ideal <- c("BEL_1","BEL_2","BEL_4","BEL_8")
bel_gap   <- c("rBEL_3","rBEL_5","rBEL_6","rBEL_7","rBEL_9")
esd_set   <- c("ESD_1","ESD_2","rESD_3","ESD_4","ESD_5","ESD_6","rESD_7","ESD_8","ESD_9")
eqd_set   <- c("EQD_1","EQD_2","rEQD_3","EQD_4","rEQD_5","EQD_6","EQD_7","EQD_8","rEQD_9")
bar_set   <- paste0("BAR_", 1:10)

d$BEL_IDEAL <- rowMeans(d[bel_ideal])
d$BEL_GAP   <- rowMeans(d[bel_gap])
d$ESDPRAC   <- rowMeans(d[esd_set])
d$EQDEM     <- rowMeans(d[eqd_set])
d$BARRIER   <- rowMeans(d[bar_set])

for (v in c("BEL_IDEAL","BEL_GAP","ESDPRAC","BARRIER")) {
  key <- c(BEL_IDEAL = "cIDEAL", BEL_GAP = "cGAP", ESDPRAC = "cESD", BARRIER = "cBAR")[[v]]
  d[[key]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
}
d$cINT <- d$cESD * d$cBAR

m1 <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR, data = d)
m2 <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cINT, data = d)

# =====================================================================
#  1. OLS with HC3 robust standard errors — the inference of record
# =====================================================================
cat("################ 1. OLS + HC3 robust SEs (inference of record) ################\n")
rob2 <- lmtest::coeftest(m2, vcov. = sandwich::vcovHC(m2, type = "HC3"))
print(round(rob2[, c(1, 2, 4)], 4))

# =====================================================================
#  2. Bootstrap, 5000 resamples, BCa intervals
# =====================================================================
cat("\n################ 2. Bootstrap (5,000 resamples, BCa) ################\n")
boot_fn <- function(data, i)
  coef(lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cINT, data = data[i, ]))
bo <- boot::boot(d, boot_fn, R = 5000)
for (k in 2:6) {
  ci <- boot::boot.ci(bo, index = k, type = "bca")$bca[4:5]
  cat(sprintf("  %-8s b = %+.3f   95%% BCa CI [%+.3f, %+.3f]   %s\n",
              names(coef(m2))[k], coef(m2)[k], ci[1], ci[2],
              if (ci[1] * ci[2] > 0) "excludes 0" else "INCLUDES 0"))
}

# =====================================================================
#  3. Robust regression — downweights influential cases
# =====================================================================
cat("\n################ 3. Robust regression ################\n")
if (HAVE_ROBUSTBASE) {
  cat("Estimator: robustbase::lmrob(), MM with KS2014 tuning\n")
  mr <- robustbase::lmrob(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cINT,
                          data = d, setting = "KS2014")
  print(round(summary(mr)$coefficients[, c(1, 2, 4)], 4))
} else {
  cat("Estimator: MASS::rlm(), M with Huber weights (robustbase unavailable)\n")
  mr <- MASS::rlm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cINT, data = d)
  print(round(lmtest::coeftest(mr)[, c(1, 2, 4)], 4))
}

# =====================================================================
#  4. Demographic covariates added
# =====================================================================
cat("\n################ 4. + demographic covariates ################\n")
d2 <- d %>% mutate(across(c(gender, age, years_experience, education_level,
                            school_area, training_sustainability,
                            training_democracy, rfcdc_known), factor))
m3 <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cINT + gender + age +
           years_experience + education_level + school_area +
           training_sustainability + training_democracy + rfcdc_known, data = d2)
print(round(lmtest::coeftest(m3, vcov. = sandwich::vcovHC(m3, type = "HC3"))[2:6, c(1, 2, 4)], 4))
cat("  R2 =", round(summary(m3)$r.squared, 3),
    "vs core model", round(summary(m2)$r.squared, 3), "\n")
cat("  Block test for all demographics:\n")
print(anova(m2, m3)[2, ])

# =====================================================================
#  5. Mediation — observed-score path model
# =====================================================================
cat("\n################ 5. Mediation: beliefs/barriers -> ESD -> EQDEM ################\n")
med_spec <- '
  ESDPRAC ~ a1*BEL_IDEAL + a2*BEL_GAP + a3*BARRIER
  EQDEM   ~ b*ESDPRAC + c1*BEL_IDEAL + c2*BEL_GAP + c3*BARRIER
  ind_ideal := a1*b
  ind_gap   := a2*b
  ind_bar   := a3*b
'
fm <- lavaan::sem(med_spec, data = d, se = "bootstrap", bootstrap = 2000)
print(lavaan::parameterEstimates(fm, ci = TRUE) %>%
        filter(op %in% c("~", ":=")) %>%
        select(lhs, op, rhs, label, est, se, pvalue, ci.lower, ci.upper) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

# =====================================================================
#  6. Latent-variable SEM — does measurement error change anything?
# =====================================================================
cat("\n################ 6. Latent-variable SEM ################\n")
mod <- paste0(
  'IDEAL =~ ', paste(bel_ideal, collapse = " + "), '\n',
  'GAP   =~ ', paste(bel_gap,   collapse = " + "), '\n',
  'ESDL  =~ ', paste(esd_set,   collapse = " + "), '\n',
  'EQDL  =~ ', paste(eqd_set,   collapse = " + "), '\n',
  'BARL  =~ ', paste(bar_set,   collapse = " + "), '\n',
  'EQDL ~ IDEAL + GAP + ESDL + BARL\n')
fs <- lavaan::sem(mod, data = d, estimator = "MLR")
print(round(lavaan::fitMeasures(fs, c("chisq.scaled", "df", "pvalue.scaled",
                                      "cfi.robust", "tli.robust",
                                      "rmsea.robust", "srmr")), 3))
cat("\nStructural paths (standardised):\n")
print(lavaan::standardizedSolution(fs) %>%
        filter(lhs == "EQDL", op == "~") %>%
        select(lhs, op, rhs, est.std, se, pvalue, ci.lower, ci.upper) %>%
        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
        as.data.frame())

cat("\n=====================================================================\n")
cat("Done. Redirect this console output to a file if you want it archived:\n")
cat("  in RStudio:  sink(file.path('", OUT, "', 'robustness_output.txt')) ...\n", sep = "")
cat("  or from a shell:  Rscript robustness_checks.R > robustness_output.txt\n")
cat("=====================================================================\n")
