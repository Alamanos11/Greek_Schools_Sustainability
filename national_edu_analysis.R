# =====================================================================
#  NATIONAL ANALYSIS — Education for Sustainability, Democracy & Equality
#  in Greek Primary Education
#
#  Sample : N = 391 teachers
#  Field  : 22 May – 20 September 2026
#  Data   : national_N391_teaching_dataset.csv
#
# =====================================================================

# ---------------------------------------------------------------------
# STEP 0 — PACKAGES, PATHS, REPRODUCIBILITY
# ---------------------------------------------------------------------
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

pkgs <- c("tidyverse", "psych", "broom", "car",
          "sandwich", "lmtest", "effectsize", "sjPlot", "boot", "lavaan")

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
       paste0('\"', missing, '\"', collapse = ", "), "))\n\n",
       "  Nothing below this line can work until every package loads.\n\n",
       call. = FALSE)

invisible(lapply(pkgs, library, character.only = TRUE))


if (getRversion() < "4.1.0")
  stop("This script uses the \\(x) lambda shorthand and needs R >= 4.1.0. ",
       "You have ", getRversion(), ".", call. = FALSE)

set.seed(2026)            # fa.parallel simulates — without this it is not reproducible
options(width = 200)      # wide result tables print on one line

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
op <- function(f) file.path(OUT, f)          # output path helper

# Progress marker, so any failure can be located immediately.
step <- function(n, what) cat(sprintf("\n[STEP %s] %s\n", n, what))

cat("Project dir :", PROJECT_DIR, "\nOutput dir  :", OUT, "\n")
cat("R version   :", R.version.string, "\n")

# ---------------------------------------------------------------------
# STEP 1 — IMPORT
# ---------------------------------------------------------------------
step(1, "Import")
d <- tryCatch(
  read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
  error = function(e)
    stop("\n\n  Could not read the data file:\n    ", DATA_PATH,
         "\n\n  R reported: ", conditionMessage(e),
         "\n\n  Check the file is not open in Excel, and that the path above is right.\n\n",
         call. = FALSE))
if (!is.data.frame(d) || nrow(d) == 0)
  stop("The data file was read but contains no rows: ", DATA_PATH, call. = FALSE)
cat("Rows:", nrow(d), " Columns:", ncol(d), "\n")

likert_cols <- c(paste0("BEL_", 1:9), paste0("ESD_", 1:9),
                 paste0("EQD_", 1:9), paste0("BAR_", 1:10),
                 paste0("APP_", 1:9))

rng <- range(d[likert_cols], na.rm = TRUE)
cat("Likert range:", rng[1], "-", rng[2], "\n")
stopifnot(rng[1] >= 1, rng[2] <= 5)          # now an actual guard, not just a printout

n_missing <- sum(is.na(d[likert_cols]))
cat("Missing values across the 46 Likert items:", n_missing,
    "(", round(100 * n_missing / (nrow(d) * length(likert_cols)), 2), "% )\n")
cat("Consent = Yes:", sum(d$consent == "Yes"), "of", nrow(d), "\n\n")

# ---------------------------------------------------------------------
# STEP 2 — PREPARE VARIABLES
# Demographics -> ordered factors. specialty is free text and is
# collapsed into an analysable grouping. rfcdc_use is genuinely skipped
# by respondents who do not know the framework, so blanks stay NA.
# ---------------------------------------------------------------------
step(2, "Prepare variables")
d <- d %>%
  mutate(
    gender           = factor(gender, levels = c("Female", "Male")),
    age              = factor(age, levels = c("22-30", "31-40", "41-50", "51+")),
    years_experience = factor(years_experience, levels = c("0-5", "6-10", "11-20", "21+")),
    education_level  = factor(education_level, levels = c("Bachelor's", "Master's", "Doctorate")),
    school_area      = factor(school_area, levels = c("Urban", "Semi-urban", "Rural")),
    training_sustainability = factor(training_sustainability, levels = c("No", "Yes")),
    training_democracy      = factor(training_democracy,      levels = c("No", "Yes")),
    rfcdc_known             = factor(rfcdc_known,             levels = c("No", "Yes")),
    rfcdc_use               = factor(rfcdc_use,
                                     levels = c("No", "Rarely", "Sometimes", "Yes, systematically")),
    # free-text specialty -> three interpretable groups
    specialty_grp = case_when(
      str_detect(specialty, "PE70 SEN|PE71|Special Education") ~ "Special education",
      str_detect(specialty, "PE70|Primary Teacher")            ~ "Generalist primary",
      TRUE                                                     ~ "Subject specialist"
    ) %>% factor(levels = c("Generalist primary", "Special education", "Subject specialist"))
  )

cat("--- specialty (free text) collapsed ---\n")
print(count(d, specialty_grp))

# Documented inconsistency, reported rather than silently fixed:
# the RFCDC awareness item was 3-level on the form ("know it well" /
# "have heard of it" / "do not know") and arrives here as binary.
cat("\n--- rfcdc_known x rfcdc_use (skip-logic check) ---\n")
print(table(d$rfcdc_known, addNA(d$rfcdc_use), dnn = c("known", "use")))
cat("Respondents coded 'No' who nevertheless answered the follow-up:",
    sum(d$rfcdc_known == "No" & !is.na(d$rfcdc_use)), "\n\n")

# ---------------------------------------------------------------------
# STEP 3 — REVERSE-CODE NEGATIVELY WORDED ITEMS (6 - x)
# Nine items were negatively worded by design. BEL_6 is reversed in
# addition, on the empirical grounds shown in STEP 5b — it is flagged
# separately so the design reversals and the data-driven one stay
# distinguishable.
# ---------------------------------------------------------------------
step(3, "Reverse-code negatively worded items")
rev_items <- c("BEL_3", "BEL_5", "BEL_7", "BEL_9",
               "ESD_3", "ESD_7",
               "EQD_3", "EQD_5", "EQD_9")
d <- d %>% mutate(across(all_of(rev_items), ~ 6 - .x, .names = "r{.col}"))
d <- d %>% mutate(rBEL_6 = 6 - BEL_6)        # empirical, see STEP 5b
stopifnot(all(d$BEL_3 + d$rBEL_3 == 6, na.rm = TRUE))

bel_set <- c("BEL_1","BEL_2","rBEL_3","BEL_4","rBEL_5","BEL_6","rBEL_7","BEL_8","rBEL_9")
esd_set <- c("ESD_1","ESD_2","rESD_3","ESD_4","ESD_5","ESD_6","rESD_7","ESD_8","ESD_9")
eqd_set <- c("EQD_1","EQD_2","rEQD_3","EQD_4","rEQD_5","EQD_6","EQD_7","EQD_8","rEQD_9")
bar_set <- paste0("BAR_", 1:10)
app_set <- paste0("APP_", 1:9)

# Belief subscales (see the EFA in STEP 5b, which is run on THIS sample).
bel_ideal <- c("BEL_1","BEL_2","BEL_4","BEL_8")                    # what education SHOULD do
bel_gap   <- c("rBEL_3","rBEL_5","rBEL_6","rBEL_7","rBEL_9")       # perceived inadequacy, self + system

# Appraisal subscales (see the EFA in STEP 5c).
app_self <- c("APP_1","APP_2","APP_3","APP_4","APP_5","APP_9")     # self-rated implementation / efficacy
app_cond <- c("APP_7","APP_8")                                     # perceived enabling conditions — EXCLUDED, see below
# APP_6 ("I need more training") stands alone — it is a need, not an appraisal.
#
# EXCLUSION, declared: APP_COND is computed and reported descriptively but is
# NOT used in any inferential analysis. On the administered form this block
# carried the barriers instruction line ("to what extent do you agree that the
# following are an OBSTACLE...") above nine positively worded statements, so it
# is not knowable which frame each respondent answered. The empirical signature
# of that is printed in STEP 5d: APP_COND correlates POSITIVELY with BARRIER,
# where the construct requires a negative correlation. Two items, alpha = .59,
# and a corrupted stem is not a defensible measure. APP_SELF is unaffected and
# is retained.

# ---------------------------------------------------------------------
# STEP 4 — SCALE (CONSTRUCT) SCORES
# Each construct is the mean of its items, one number per respondent.
# require_all(): a score is only computed when every item is answered,
# so a scale can never rest on a single response.
# ---------------------------------------------------------------------
step(4, "Scale scores")
require_all <- function(df, items) {
  m <- as.matrix(df[items])
  ifelse(rowSums(is.na(m)) > 0, NA_real_, rowMeans(m))
}

d$BELIEF    <- require_all(d, bel_set)       # full 9-item version, for reporting only
d$BEL_IDEAL <- require_all(d, bel_ideal)
d$BEL_GAP   <- require_all(d, bel_gap)
d$ESDPRAC   <- require_all(d, esd_set)
d$EQDEM     <- require_all(d, eqd_set)
d$BARRIER   <- require_all(d, bar_set)
d$APP_SELF  <- require_all(d, app_self)
d$APP_COND  <- require_all(d, app_cond)

# ---------------------------------------------------------------------
# STEP 4b — CARELESS-RESPONDING SCREEN
# "longstring" = the longest run of identical consecutive answers across
# the 46 items. Computed and reported here; the primary analysis uses the
# full sample and STEP 8c re-runs the model without flagged cases as a
# declared sensitivity check.
# ---------------------------------------------------------------------
step("4b", "Careless-responding screen")
longstring <- function(x) {
  r <- rle(as.numeric(x))
  max(r$lengths)
}
d$longstring <- apply(d[likert_cols], 1, longstring)
d$within_sd  <- apply(d[likert_cols], 1, sd, na.rm = TRUE)
d$flag_careless <- d$longstring >= 10

cat("--- careless-responding screen ---\n")
cat("longest identical run, median:", median(d$longstring),
    " max:", max(d$longstring), "\n")
cat("flagged (run >= 10):", sum(d$flag_careless),
    " | within-person SD < 0.5:", sum(d$within_sd < 0.5), "\n\n")

# ---------------------------------------------------------------------
# STEP 5 — RELIABILITY (Cronbach's alpha)
# 5a: the four scales as designed. 5b: the beliefs scale, which is not
# unidimensional, diagnosed and split. 5c: the appraisal block.
# ---------------------------------------------------------------------
alpha_bel <- psych::alpha(d[bel_set], check.keys = FALSE, warnings = FALSE)
alpha_esd <- psych::alpha(d[esd_set], check.keys = FALSE, warnings = FALSE)
alpha_eqd <- psych::alpha(d[eqd_set], check.keys = FALSE, warnings = FALSE)
alpha_bar <- psych::alpha(d[bar_set], check.keys = FALSE, warnings = FALSE)
alpha_app <- psych::alpha(d[app_set], check.keys = FALSE, warnings = FALSE)

alphas <- tibble(
  Scale  = c("Sustainability beliefs (full, 9 items)", "ESD practices",
             "Equality & democratic practices", "Barriers",
             "Overall appraisal (full, 9 items)"),
  Items  = c(9, 9, 9, 10, 9),
  Alpha  = round(c(alpha_bel$total$raw_alpha, alpha_esd$total$raw_alpha,
                   alpha_eqd$total$raw_alpha, alpha_bar$total$raw_alpha,
                   alpha_app$total$raw_alpha), 3),
  Mean_r = round(c(alpha_bel$total$average_r, alpha_esd$total$average_r,
                   alpha_eqd$total$average_r, alpha_bar$total$average_r,
                   alpha_app$total$average_r), 3)
)
cat("=== STEP 5a — reliability of the scales as designed ===\n")
print(alphas)
cat("\nTwo scales fall below .70 as designed: beliefs and overall appraisal.\n",
    "Both are diagnosed below and neither is used as a single score.\n\n")

# ---- 5b. Beliefs: diagnosis and split -------------------------------
cat("=== STEP 5b — beliefs scale: EFA and split ===\n")
efa_bel <- psych::fa(d[bel_set], nfactors = 2, rotate = "varimax", fm = "minres")
print(efa_bel$loadings, cutoff = 0.30)
cat("\nEigenvalues:", round(psych::fa.parallel(d[bel_set], fa = "fa",
                                              plot = FALSE, n.iter = 50)$fa.values[1:4], 2), "\n")

alpha_ideal <- psych::alpha(d[bel_ideal], warnings = FALSE)
alpha_gap   <- psych::alpha(d[bel_gap],   warnings = FALSE)

# ---- 5c. Overall appraisal: diagnosis and split ---------------------
cat("\n=== STEP 5c — overall appraisal block: EFA and split ===\n")
efa_app <- psych::fa(d[app_set], nfactors = 3, rotate = "varimax", fm = "minres")
print(efa_app$loadings, cutoff = 0.30)

alpha_self <- psych::alpha(d[app_self], warnings = FALSE)
alpha_cond <- psych::alpha(d[app_cond], warnings = FALSE)

subscale_rel <- tibble(
  Subscale = c("Idealistic beliefs (4 items)",
               "Perceived implementation gap (5 items)",
               "Self-rated implementation (6 items)",
               "Perceived enabling conditions (2 items) [EXCLUDED from inference]"),
  Alpha  = round(c(alpha_ideal$total$raw_alpha, alpha_gap$total$raw_alpha,
                   alpha_self$total$raw_alpha,  alpha_cond$total$raw_alpha), 3),
  Mean_r = round(c(alpha_ideal$total$average_r, alpha_gap$total$average_r,
                   alpha_self$total$average_r,  alpha_cond$total$average_r), 3)
)
cat("\n")
print(subscale_rel)
cat("\nFor short scales alpha runs mechanically low, so the mean inter-item\n",
    "correlation is reported alongside it (.15-.50 is the usual guideline).\n\n")

# ---- 5d. The consistency check the instrument was designed for ------
# The appraisal block was included as a check between each teacher's
# global self-image and their detailed item scores. This is that check.
cat("=== STEP 5d — convergent check: global self-appraisal vs detailed scores ===\n")
consistency <- tibble(
  Pair = c("APP_SELF (self-rated implementation) ~ ESDPRAC",
           "APP_SELF ~ EQDEM",
           "APP_1 'I adequately implement ESD' ~ ESDPRAC",
           "APP_9 'my practice builds active citizens' ~ EQDEM",
           "APP_COND ~ BARRIER  [EXCLUDED: must be negative, is not]"),
  r = round(c(cor(d$APP_SELF, d$ESDPRAC, use = "complete.obs"),
              cor(d$APP_SELF, d$EQDEM,   use = "complete.obs"),
              cor(d$APP_1,    d$ESDPRAC, use = "complete.obs"),
              cor(d$APP_9,    d$EQDEM,   use = "complete.obs"),
              cor(d$APP_COND, d$BARRIER, use = "complete.obs")), 3)
)
print(consistency)
cat("\nThe first four rows are the convergent check the appraisal block was\n",
    "designed for, and it passes: the global self-appraisal tracks the\n",
    "item-level practice scores closely.\n",
    "The last row is the reason APP_COND is excluded: the construct requires\n",
    "a NEGATIVE correlation with BARRIER and the observed one is positive.\n",
    "See the exclusion note in STEP 3.\n\n")

# ---------------------------------------------------------------------
# STEP 6 — DESCRIPTIVES
# ---------------------------------------------------------------------
demo_vars <- c("gender","age","years_experience","education_level",
               "specialty_grp","school_area","training_sustainability",
               "training_democracy","rfcdc_known","rfcdc_use")

demo_tables <- map(demo_vars, \(v)
  d %>% count(.data[[v]]) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    rename(level = 1)
) %>% set_names(demo_vars)

cat("=== STEP 6a — sample profile ===\n")
for (v in demo_vars) { cat("\n--", v, "--\n"); print(as.data.frame(demo_tables[[v]])) }

demo_long <- imap_dfr(demo_tables, \(tb, v)
  tibble(variable = v, level = as.character(tb$level), n = tb$n, pct = tb$pct))

# 6b. Item-level descriptives (raw responses, before reversal).
item_desc <- d %>%
  summarise(across(all_of(likert_cols),
                   list(M = ~mean(.x, na.rm = TRUE), SD = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(), names_to = c("item", ".value"), names_sep = "_(?=[MS])") %>%
  mutate(across(c(M, SD), ~round(.x, 2)))
cat("\n=== STEP 6b — item descriptives (all 46 items) ===\n")
print(item_desc, n = 46)

# 6c. Scale-level descriptives.
constructs_v     <- c("BEL_IDEAL","BEL_GAP","ESDPRAC","EQDEM","BARRIER","APP_SELF")
constructs_desc  <- c(constructs_v, "APP_COND")   # descriptives only — APP_COND is excluded from inference
scale_desc <- d %>%
  select(all_of(constructs_desc)) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "x") %>%
  group_by(scale) %>%
  summarise(n = sum(!is.na(x)), M = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE),
            Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE),
            Skew = psych::skew(x), Kurt = psych::kurtosi(x),
            pct_at_ceiling = 100 * mean(x == 5, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
cat("\n=== STEP 6c — construct descriptives ===\n")
print(as.data.frame(scale_desc))
cat("\npct_at_ceiling = share of respondents scoring the maximum of 5.00.\n",
    "A high value means restricted variance, which attenuates every\n",
    "correlation involving that construct.\n\n")

# ---------------------------------------------------------------------
# STEP 7 — CORRELATIONS
# p printed as 0 means p < .001 — report it as "p < .001".
# ---------------------------------------------------------------------
constructs <- d %>% select(all_of(constructs_v))
corr_res   <- psych::corr.test(constructs, adjust = "BH")
cat("=== STEP 7 — correlations between constructs ===\n")
print(round(corr_res$r, 2))
cat("\np-values (LOWER triangle = raw, UPPER triangle = BH-adjusted):\n")
print(round(corr_res$p, 3))

cat("\nCollinearity check on the two 'perceived obstacle' constructs:\n")
cat("  BEL_GAP ~ BARRIER  r =", round(cor(d$BEL_GAP, d$BARRIER, use = "complete.obs"), 2), "\n\n")

# ---------------------------------------------------------------------
# STEP 8 — REGRESSION
# Does ESD practice (and beliefs, and barriers) predict democratic /
# equitable classroom practice? Predictors are mean-centred.
# ---------------------------------------------------------------------
d <- d %>%
  mutate(cIDEAL = BEL_IDEAL - mean(BEL_IDEAL, na.rm = TRUE),
         cGAP   = BEL_GAP   - mean(BEL_GAP,   na.rm = TRUE),
         cESD   = ESDPRAC   - mean(ESDPRAC,   na.rm = TRUE),
         cBAR   = BARRIER   - mean(BARRIER,   na.rm = TRUE))

m1 <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR, data = d)
m2 <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cESD:cBAR, data = d)

cat("=== STEP 8a — Model 1 (main effects) ===\n")
print(summary(m1))
cat("\n95% confidence intervals:\n"); print(round(confint(m1), 3))
cat("\nStandardised betas:\n")
print(round(coef(lm(scale(EQDEM) ~ scale(cIDEAL) + scale(cGAP) +
                      scale(cESD) + scale(cBAR), data = d))[-1], 3))
cat("\nVariance inflation factors:\n"); print(round(car::vif(m1), 2))

cat("\n--- assumption diagnostics ---\n")
bp <- lmtest::bptest(m1)
cat("Breusch-Pagan (heteroscedasticity): BP =", round(bp$statistic, 2),
    ", p =", format.pval(bp$p.value, digits = 3), "\n")
sw <- shapiro.test(residuals(m1))
cat("Shapiro-Wilk (residual normality) : W =", round(sw$statistic, 3),
    ", p =", format.pval(sw$p.value, digits = 3), "\n")
cook <- cooks.distance(m1)
cat("Cases with Cook's D > 4/n         :", sum(cook > 4 / nrow(d)), "\n")

# Heteroscedasticity is expected with Likert composites, so the
# inferential test of record uses HC3 robust standard errors.
cat("\n--- Model 1 with HC3 robust standard errors (test of record) ---\n")
rob1 <- lmtest::coeftest(m1, vcov. = sandwich::vcovHC(m1, type = "HC3"))
print(rob1)

cat("\n=== STEP 8b — Model 2 (+ ESD x Barriers interaction) ===\n")
print(summary(m2))
cat("\nNested model comparison (classical F):\n"); print(anova(m1, m2))
cat("\nModel 2 with HC3 robust standard errors:\n")
rob2 <- lmtest::coeftest(m2, vcov. = sandwich::vcovHC(m2, type = "HC3"))
print(rob2)
cat("\nRobust Wald test of the interaction:\n")
print(lmtest::waldtest(m1, m2, vcov = sandwich::vcovHC(m2, type = "HC3")))
cat("\nDelta R-squared from adding the interaction:",
    round(summary(m2)$r.squared - summary(m1)$r.squared, 4), "\n")

# The interaction is the one coefficient whose verdict depends on the method,
# so it gets a bootstrap confidence interval as well. BCa, 5000 resamples.
cat("\n--- bootstrap CI for the interaction (5000 resamples, BCa) ---\n")
boot_fn <- function(data, i) coef(lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cESD:cBAR,
                                     data = data[i, ]))
bo  <- boot::boot(d, boot_fn, R = 5000)
bci <- boot::boot.ci(bo, index = 6, type = "bca")$bca[4:5]
cat(sprintf("b = %+.3f, 95%% BCa CI [%+.3f, %+.3f] -> %s\n",
            coef(m2)["cESD:cBAR"], bci[1], bci[2],
            if (bci[1] * bci[2] > 0) "excludes zero" else "INCLUDES ZERO"))
cat("Verdict rule set in advance: the interaction is reported as supported only\n",
    "if BOTH the robust test and the bootstrap interval agree. Here they do not,\n",
    "so the moderation is reported as tested and not supported.\n")

# ---- 8c. Declared sensitivity check ---------------------------------
cat("\n=== STEP 8c — sensitivity: flagged careless responders removed ===\n")
d_screened <- filter(d, !flag_careless)
m1s <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR, data = d_screened)
m2s <- lm(EQDEM ~ cIDEAL + cGAP + cESD + cBAR + cESD:cBAR, data = d_screened)
cat("n =", nrow(d_screened), " (", sum(d$flag_careless), "removed )\n")
print(lmtest::coeftest(m2s, vcov. = sandwich::vcovHC(m2s, type = "HC3")))

sens <- tibble(
  Sample = c("Full sample", "Careless responders removed"),
  n = c(nrow(d), nrow(d_screened)),
  R2_M1 = round(c(summary(m1)$r.squared, summary(m1s)$r.squared), 3),
  b_ESD = round(c(coef(m1)["cESD"], coef(m1s)["cESD"]), 3),
  b_BAR = round(c(coef(m1)["cBAR"], coef(m1s)["cBAR"]), 3),
  b_int = round(c(coef(m2)["cESD:cBAR"], coef(m2s)["cESD:cBAR"]), 3),
  p_int_robust = round(c(rob2["cESD:cBAR", 4],
                         lmtest::coeftest(m2s, vcov. = sandwich::vcovHC(m2s, type = "HC3"))["cESD:cBAR", 4]), 4)
)
cat("\n")
print(as.data.frame(sens))

# ---- 8d. MODEL 3 — mediation: does ESD practice carry the effect? ----
# Model 1 gives the TOTAL effect of each predictor on democratic practice.
# Model 3 splits it: how much travels THROUGH sustainability teaching, and
# how much reaches democratic practice directly. Indirect effects are tested
# with 5000 bootstrap resamples (the product of two coefficients is not
# normally distributed, so a bootstrap interval is the correct test).
#
# CAUSAL LANGUAGE: these are cross-sectional self-reports collected on one
# occasion. The model tests whether the data are CONSISTENT WITH the proposed
# ordering; it cannot establish it. Report accordingly.
cat("\n=== STEP 8d — Model 3: mediation via ESD practice ===\n")

med_spec <- '
  # a-paths: predictors -> mediator
  ESDPRAC ~ a1*BEL_IDEAL + a2*BEL_GAP + a3*BARRIER
  # b-path and direct (c-prime) paths -> outcome
  EQDEM   ~ b*ESDPRAC + c1*BEL_IDEAL + c2*BEL_GAP + c3*BARRIER
  # indirect effects
  ind_ideal := a1*b
  ind_gap   := a2*b
  ind_bar   := a3*b
  # total effects
  tot_ideal := c1 + a1*b
  tot_gap   := c2 + a2*b
  tot_bar   := c3 + a3*b
'
m3 <- lavaan::sem(med_spec, data = d, se = "bootstrap", bootstrap = 5000)

med_tab <- lavaan::parameterEstimates(m3, ci = TRUE) %>%
  filter(op %in% c("~", ":=")) %>%
  transmute(path = ifelse(op == "~", paste(lhs, "<-", rhs), lhs),
            type = case_when(op == "~" & lhs == "ESDPRAC" ~ "a (to mediator)",
                             op == "~" & rhs == "ESDPRAC" ~ "b (mediator to outcome)",
                             op == "~"                    ~ "c' (direct)",
                             str_starts(lhs, "ind")       ~ "indirect",
                             TRUE                         ~ "total"),
            est = round(est, 3), se = round(se, 3),
            ci = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
            p = round(pvalue, 4),
            sig = ifelse(ci.lower * ci.upper > 0, "*", ""))
print(as.data.frame(med_tab), right = FALSE)

prop_med <- med_tab %>% filter(type == "indirect") %>% pull(est) /
            med_tab %>% filter(type == "total")    %>% pull(est)
cat("\nProportion of each total effect carried by ESD practice:\n")
cat(sprintf("  idealistic beliefs : %.0f%%\n  implementation gap : %.0f%%\n  barriers           : %.0f%%\n",
            100 * prop_med[1], 100 * prop_med[2], 100 * prop_med[3]))
cat("\nRead: a large share means the predictor reaches democratic practice mainly\n",
    "BY changing how much sustainability teaching happens. A share near zero means\n",
    "it reaches democratic practice by some other route entirely.\n")

write_csv(med_tab, op("table_mediation_model3.csv"))

# Publication table for Models 1 and 2 (Model 3 exports as its own CSV above).
sjPlot::tab_model(
  m1, m2,
  dv.labels = c("Model 1: main effects", "Model 2: + ESD x Barriers"),
  show.std = TRUE, show.ci = 0.95,
  file = op("regression_table.html")
)

bind_rows(
  broom::tidy(m1, conf.int = TRUE) %>% mutate(model = "M1"),
  broom::tidy(m2, conf.int = TRUE) %>% mutate(model = "M2")
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  write_csv(op("table_regression_coefficients.csv"))

fit_tab <- bind_rows(broom::glance(m1) %>% mutate(model = "M1"),
                     broom::glance(m2) %>% mutate(model = "M2")) %>%
  select(model, r.squared, adj.r.squared, statistic, p.value, df, df.residual, nobs) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))
cat("\n--- model fit ---\n"); print(as.data.frame(fit_tab))

# ---------------------------------------------------------------------
# STEP 9 — GROUP COMPARISONS
# Every demographic variable collected is tested against every construct,
# with an effect size, and the whole family is corrected for multiplicity
# (Benjamini-Hochberg FDR). Both the uncorrected and the corrected p are
# reported so the reader can see what the correction does.
# ---------------------------------------------------------------------
outcomes   <- constructs_v
two_group  <- c("gender", "training_sustainability", "training_democracy", "rfcdc_known")
many_group <- c("age", "years_experience", "education_level", "specialty_grp", "school_area")

res <- list(); k <- 0

for (y in outcomes) {
  for (g in two_group) {
    tt  <- t.test(reformulate(g, y), data = d)                    # Welch
    eff <- effectsize::cohens_d(reformulate(g, y), data = d)
    ns  <- table(d[[g]][!is.na(d[[y]])])
    val <- eff$Cohens_d[1]; lo <- eff$CI_low[1]; hi <- eff$CI_high[1]
    k <- k + 1
    res[[k]] <- tibble(outcome = y, group = g, test = "Welch t",
                       stat = unname(tt$statistic), df = unname(tt$parameter),
                       p = tt$p.value, effect = "Cohen's d",
                       es = val, es_lo = lo, es_hi = hi,
                       n = paste(ns, collapse = "/"))
  }
  for (g in many_group) {
    fit <- aov(reformulate(g, y), data = d)
    s   <- summary(fit)[[1]]
    eff <- effectsize::eta_squared(fit, partial = FALSE)
    ns  <- table(d[[g]][!is.na(d[[y]])])
    val <- eff$Eta2[1]; lo <- eff$CI_low[1]; hi <- eff$CI_high[1]
    k <- k + 1
    res[[k]] <- tibble(outcome = y, group = g, test = "ANOVA F",
                       stat = s[["F value"]][1], df = s[["Df"]][1],
                       p = s[["Pr(>F)"]][1], effect = "eta2",
                       es = val, es_lo = lo, es_hi = hi,
                       n = paste(ns, collapse = "/"))
  }
}

group_tests <- bind_rows(res) %>%
  mutate(p_BH = p.adjust(p, method = "BH"),
         sig_raw = p < .05,
         sig_BH  = p_BH < .05,
         min_n   = map_dbl(n, \(s) min(as.numeric(strsplit(s, "/", fixed = TRUE)[[1]]))),
         small_group = min_n < 30) %>%
  arrange(p)

cat("=== STEP 9 — group comparisons, all demographics, FDR-corrected ===\n")
print(as.data.frame(group_tests %>%
  mutate(across(c(stat, es, es_lo, es_hi), ~round(.x, 3)),
         p = round(p, 4), p_BH = round(p_BH, 4)) %>%
  select(outcome, group, test, stat, df, p, p_BH, sig_BH, effect, es, n, small_group)))

cat("\nTests run:", nrow(group_tests),
    " | significant uncorrected:", sum(group_tests$sig_raw),
    " | surviving BH-FDR:", sum(group_tests$sig_BH), "\n\n")

# Post-hoc only where the corrected test survives.
survivors <- group_tests %>% filter(sig_BH, test == "ANOVA F")
if (nrow(survivors)) {
  for (i in seq_len(nrow(survivors))) {
    y <- survivors$outcome[i]; g <- survivors$group[i]
    cat("--- Tukey HSD:", y, "by", g, "---\n")
    desc <- d %>% group_by(.data[[g]]) %>%
      summarise(n = sum(!is.na(.data[[y]])), M = mean(.data[[y]], na.rm = TRUE),
                SD = sd(.data[[y]], na.rm = TRUE), .groups = "drop")
    print(as.data.frame(desc %>% mutate(across(where(is.numeric), ~round(.x, 2)))))
    print(TukeyHSD(aov(reformulate(g, y), data = d)))
    if (any(desc$n < 30))
      cat("NOTE: at least one group has n < 30. Read the group means before\n",
          "      interpreting: a significant test carried by a small subgroup\n",
          "      is not a demographic finding.\n")
    cat("\n")
  }
} else {
  cat("No ANOVA survives FDR correction — no post-hoc tests are warranted.\n\n")
}

# ---------------------------------------------------------------------
# STEP 10 — DISCRIMINANT VALIDITY: ESD vs EQUALITY/DEMOCRATIC ITEMS
# The empirical answer to "do the two practice scales measure the
# same thing?"
# ---------------------------------------------------------------------
efa_items <- d %>% select(all_of(c(esd_set, eqd_set))) %>% drop_na()
cat("=== STEP 10 — ESD vs equality/democratic practices ===\n")
kmo <- psych::KMO(efa_items)
cat("KMO overall MSA:", round(kmo$MSA, 3), "\n")
bart <- psych::cortest.bartlett(cor(efa_items), n = nrow(efa_items))
cat("Bartlett: chi2 =", round(bart$chisq), ", df =", bart$df,
    ", p =", format.pval(bart$p.value, digits = 3), "\n")

pa <- psych::fa.parallel(efa_items, fa = "fa", plot = FALSE, n.iter = 50)
cat("Parallel analysis suggests", pa$nfact, "factors.\n\n")

efa2 <- psych::fa(efa_items, nfactors = 2, rotate = "varimax", fm = "pa")
print(efa2$loadings, cutoff = 0.30)

L <- unclass(efa2$loadings)
problem <- rownames(L)[rowSums(abs(L) >= 0.30) != 1]
cat("\nItems that cross-load or load on neither factor at .30:",
    if (length(problem)) paste(problem, collapse = ", ") else "none", "\n")
cat("Where these are reversed items, the extra factor is a wording\n",
    "artefact (a method factor), not a substantive dimension.\n\n")

# ---------------------------------------------------------------------
# STEP 11 — FIGURES (300 dpi PNG)
# ---------------------------------------------------------------------
step(11, "Figures")
theme_set(theme_minimal(base_size = 12))
PAL <- c(Disagree = "#C0504D", Neutral = "#BFBFBF", Agree = "#4F9D69")

# FIG 1 — diverging stacked bars, item response distribution.
fig1_dat <- d %>%
  select(all_of(likert_cols)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "resp") %>%
  drop_na() %>%
  mutate(cat = case_when(resp <= 2 ~ "Disagree", resp == 3 ~ "Neutral", TRUE ~ "Agree")) %>%
  count(item, cat) %>%
  group_by(item) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  mutate(pct_plot = ifelse(cat == "Disagree", -pct, pct),
         item = factor(item, levels = rev(likert_cols)))

fig1 <- ggplot() +
  geom_col(data = filter(fig1_dat, cat == "Disagree"), aes(item, pct_plot, fill = cat)) +
  geom_col(data = filter(fig1_dat, cat != "Disagree") %>%
             mutate(cat = factor(cat, levels = c("Agree", "Neutral"))),
           aes(item, pct_plot, fill = cat), position = "stack") +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
  coord_flip() +
  scale_fill_manual(values = PAL, name = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Item response distribution (N = 391)",
       subtitle = "Disagree (left)  |  Neutral / Agree (right)",
       x = NULL, y = "Proportion of respondents")
ggsave(op("fig1_diverging_items.png"), fig1, width = 9, height = 12, dpi = 300)

# FIG 2 — construct means with SD error bars.
lbl <- c(BEL_IDEAL = "Idealistic beliefs", BEL_GAP = "Implementation gap",
         ESDPRAC = "ESD practices", EQDEM = "Equality/democratic",
         BARRIER = "Barriers", APP_SELF = "Self-rated implementation",
         APP_COND = "Enabling conditions")

fig2_dat <- d %>%
  pivot_longer(all_of(constructs_v), names_to = "scale", values_to = "score") %>%
  group_by(scale) %>%
  summarise(M = mean(score, na.rm = TRUE), SD = sd(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(scale = factor(dplyr::recode(scale, !!!lbl), levels = unname(lbl)))

fig2 <- ggplot(fig2_dat, aes(scale, M)) +
  geom_col(fill = "#1F4E79", width = 0.65) +
  geom_errorbar(aes(ymin = M - SD, ymax = M + SD), width = 0.15) +
  geom_text(aes(label = sprintf("%.2f", M)), vjust = -0.8, size = 3.4) +
  scale_y_continuous(limits = c(0, 5.6), breaks = 0:5) +
  labs(title = "Construct means (1-5) with SD", x = NULL, y = "Mean score") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(op("fig2_construct_means.png"), fig2, width = 9, height = 5, dpi = 300)

# FIG 3 — correlation heatmap (built directly in ggplot2, no extra package).
cm <- corr_res$r; pm <- corr_res$p
heat <- as_tibble(as.table(cm), .name_repair = "unique") %>%
  setNames(c("x", "y", "r")) %>%
  left_join(as_tibble(as.table(pm), .name_repair = "unique") %>%
              setNames(c("x", "y", "p")), by = c("x", "y")) %>%
  mutate(x = factor(x, levels = constructs_v), y = factor(y, levels = rev(constructs_v))) %>%
  filter(as.integer(x) + as.integer(y) <= length(constructs_v) + 1) %>%
  mutate(star = case_when(as.character(x) == as.character(y) ~ "",   # no stars on the diagonal
                          p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ ""),
         lab  = paste0(sprintf("%.2f", r), star))

fig3 <- ggplot(heat, aes(x, y, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = lab), size = 3.1) +
  scale_fill_gradient2(low = "#C0504D", mid = "white", high = "#4F9D69",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  scale_x_discrete(labels = lbl) + scale_y_discrete(labels = lbl) +
  labs(title = "Correlations between constructs (N = 391)",
       subtitle = "* p < .05   ** p < .01   *** p < .001", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank())
ggsave(op("fig3_corr_heatmap.png"), fig3, width = 8, height = 6.5, dpi = 300)

# FIG 4 — barriers ranked by mean.
fig4_dat <- d %>%
  summarise(across(all_of(bar_set), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "M") %>%
  arrange(M) %>% mutate(item = factor(item, levels = item))

fig4 <- ggplot(fig4_dat, aes(item, M)) +
  geom_col(fill = "#C0504D", width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", M)), hjust = -0.15, size = 3.3) +
  coord_flip() + scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  labs(title = "Barriers ranked by mean (1-5)", x = NULL, y = "Mean")
ggsave(op("fig4_barriers_ranked.png"), fig4, width = 7, height = 5.5, dpi = 300)

# FIG 5 — the core relationship.
fig5 <- ggplot(d, aes(ESDPRAC, EQDEM)) +
  geom_point(alpha = 0.45, colour = "#1F4E79") +
  geom_smooth(method = "lm", se = TRUE, colour = "#C0504D") +
  labs(title = "ESD practice vs equality/democratic practice",
       subtitle = sprintf("r = %.2f, N = %d", corr_res$r["ESDPRAC", "EQDEM"], nrow(d)),
       x = "ESD practices (scale score)", y = "Equality & democratic practices")
ggsave(op("fig5_scatter_core.png"), fig5, width = 7, height = 5.5, dpi = 300)

# FIG 6 — moderation plot.
fig6 <- sjPlot::plot_model(m2, type = "int", mdrt.values = "meansd",
                           legend.title = "Barriers") +
  scale_colour_manual(values = c("#4F9D69", "#1F4E79", "#C0504D"),
                      labels = c("Low (-1 SD)", "Mean", "High (+1 SD)"),
                      aesthetics = c("colour", "fill")) +
  labs(title = "ESD practice x Barriers on equality/democratic practice",
       subtitle = sprintf("interaction b = %.3f, robust p = %.3f (delta R2 = %.3f)",
                          coef(m2)["cESD:cBAR"], rob2["cESD:cBAR", 4],
                          summary(m2)$r.squared - summary(m1)$r.squared),
       x = "ESD practices (mean-centred)",
       y = "Equality & democratic practices")
ggsave(op("fig6_interaction.png"), fig6, width = 7, height = 5.5, dpi = 300)

# FIG 7 — convergent check: global self-appraisal against detailed practice score.
fig7 <- ggplot(d, aes(ESDPRAC, APP_SELF)) +
  geom_point(alpha = 0.45, colour = "#7030A0") +
  geom_smooth(method = "lm", se = TRUE, colour = "#C0504D") +
  labs(title = "Consistency check: global self-appraisal vs detailed ESD practice",
       subtitle = sprintf("r = %.2f", cor(d$APP_SELF, d$ESDPRAC, use = "complete.obs")),
       x = "ESD practices (item-level scale score)",
       y = "Self-rated implementation (appraisal block)")
ggsave(op("fig7_appraisal_consistency.png"), fig7, width = 7, height = 5.5, dpi = 300)

cat("Figures written: fig1-fig7\n\n")

# ---------------------------------------------------------------------
# STEP 12 — EXPORT TABLES
# ---------------------------------------------------------------------
step(12, "Export tables")
write_csv(alphas,       op("table_alpha_designed_scales.csv"))
write_csv(subscale_rel, op("table_alpha_subscales.csv"))
write_csv(consistency,  op("table_consistency_check.csv"))
write_csv(demo_long,    op("table_demographics.csv"))
write_csv(item_desc,    op("table_item_descriptives.csv"))
write_csv(scale_desc,   op("table_scale_descriptives.csv"))
write_csv(fit_tab,      op("table_model_fit.csv"))
write_csv(sens,         op("table_sensitivity_careless.csv"))
write_csv(group_tests %>% select(-min_n), op("table_group_comparisons.csv"))
as.data.frame(round(corr_res$r, 3)) %>% rownames_to_column("scale") %>%
  write_csv(op("table_correlations_r.csv"))
as.data.frame(round(corr_res$p, 4)) %>% rownames_to_column("scale") %>%
  write_csv(op("table_correlations_p.csv"))

# ---------------------------------------------------------------------
# STEP 13 — RUN LOG
# ---------------------------------------------------------------------
step(13, "Run log")
writeLines(c(
  paste("Run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("Data:", DATA_FILE, "| N =", nrow(d)),
  paste("Fieldwork window: 22 May - 20 September 2026"),
  paste("R:", R.version.string),
  "",
  "Package versions:",
  paste0("  ", pkgs, " ", vapply(pkgs, \(p) as.character(packageVersion(p)), "")),
  "",
  capture.output(sessionInfo())
), op("run_log.txt"))

cat("=====================================================================\n")
cat("Done. All figures and tables are in:\n  ", OUT, "\n")
cat("=====================================================================\n")

# ===================== END OF SCRIPT =====================
