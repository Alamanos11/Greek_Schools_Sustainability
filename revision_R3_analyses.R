# =====================================================================
#  REVISION ANALYSES (Round 1, Reviewer 3) — Education for Sustainability,
#  Democracy & Equality in Greek Primary Education (N = 391)
#
#  Additional analyses requested by Reviewer 3. Self-contained: rebuilds
#  the construct scores exactly as in revision_analyses.R.
#
#   T1  Cost gradient as an explanatory item-response model: random cost
#       slopes, cost x perceived-barriers cross-level interaction,
#       cost x training, item variance explained by cost
#   T2  Response-anchor (periodicity) check: gradient at every threshold
#       of the frequency scale, including never vs at least rarely
#   T3  Belief scale: CFA with a cross-scale wording method factor,
#       correlated-uniqueness model, critical stance without the
#       empirically reflected item (BEL_6)
#   T4  Blocks of predictors in one model (both entry orders) and
#       standardised coefficients for Model 1
#   T5  Equivalence for training against educational benchmarks
#   T6  Sample vs official workforce statistics (Eurostat 2024);
#       raking weights as a sensitivity analysis
#   T7  Timing of responses: before September vs September
#   T8  Staffing proxies: youngest and least experienced teachers
#   T9  Item-rest correlations (form layout check)
# =====================================================================
# ---------------------------------------------------------------------
# STEP 0 — PACKAGES, PATHS, REPRODUCIBILITY
# ---------------------------------------------------------------------
CRAN <- unname(getOption("repos")["CRAN"])
if (is.na(CRAN) || !startsWith(CRAN, "http")) CRAN <- "https://cloud.r-project.org"

pkgs <- c("readr", "dplyr", "tidyr", "psych", "GPArotation", "lavaan",
          "lme4", "lmerTest", "ordinal", "sandwich", "lmtest")
need <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Installing missing package: ", p)
    try(install.packages(p, repos = CRAN), silent = TRUE)
  }
  requireNamespace(p, quietly = TRUE)
}
missing <- pkgs[!vapply(pkgs, need, logical(1))]
if (length(missing))
  stop("\n  These packages could not be installed or loaded: ",
       paste(missing, collapse = ", "),
       "\n  FIX: install.packages(c(", paste0('"', missing, '"', collapse = ", "),
       ")) and run again.\n", call. = FALSE)
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

set.seed(2026)
options(width = 200)

PROJECT_DIR <- "E:/publications/B.Journals/70_vasia/paper"
DATA_FILE   <- "national_N391_teaching_dataset.csv"
if (!file.exists(file.path(PROJECT_DIR, DATA_FILE))) {
  if (file.exists(file.path(getwd(), DATA_FILE))) {
    PROJECT_DIR <- getwd()
    message("NOTE: using the working directory: ", PROJECT_DIR)
  } else stop("Cannot find ", DATA_FILE, " in ", PROJECT_DIR, " or ", getwd(),
              ". Edit PROJECT_DIR at the top of this script.", call. = FALSE)
}
OUT <- file.path(PROJECT_DIR, "results_revision_R3")
dir.create(OUT, showWarnings = FALSE)
op   <- function(f) file.path(OUT, f)
step <- function(n, what) cat(sprintf("\n\n==================== [%s] %s ====================\n", n, what))
r3   <- function(x) round(x, 3)

cat("Project dir :", PROJECT_DIR, "\nOutput dir  :", OUT, "\nR version   :", R.version.string, "\n")

# ---------------------------------------------------------------------
# STEP 1 — IMPORT, RECODE, CONSTRUCT SCORES (identical to the main script)
# ---------------------------------------------------------------------
step("1", "Import and construct scores")
d <- readr::read_csv(file.path(PROJECT_DIR, DATA_FILE),
                     locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)
stopifnot(nrow(d) == 391)

d <- d %>% mutate(
  gender           = factor(gender, levels = c("Female", "Male")),
  age              = factor(age, levels = c("22-30", "31-40", "41-50", "51+")),
  years_experience = factor(years_experience, levels = c("0-5", "6-10", "11-20", "21+")),
  education_level  = factor(education_level, levels = c("Bachelor's", "Master's", "Doctorate")),
  school_area      = factor(school_area, levels = c("Urban", "Semi-urban", "Rural")),
  training_sustainability = factor(training_sustainability, levels = c("No", "Yes")),
  training_democracy      = factor(training_democracy,      levels = c("No", "Yes")),
  rfcdc_known             = factor(rfcdc_known,             levels = c("No", "Yes")),
  specialty_grp = case_when(
    grepl("PE70 SEN|PE71|Special Education", specialty) ~ "Special education",
    grepl("PE70|Primary Teacher", specialty)            ~ "Generalist primary",
    TRUE                                                ~ "Subject specialist") %>%
    factor(levels = c("Generalist primary", "Special education", "Subject specialist"))
)

rev_items <- c("BEL_3", "BEL_5", "BEL_7", "BEL_9", "ESD_3", "ESD_7", "EQD_3", "EQD_5", "EQD_9")
for (v in rev_items) d[[paste0("r", v)]] <- 6 - d[[v]]
d$rBEL_6 <- 6 - d$BEL_6                                   # empirical reflection (main script, STEP 5b)
stopifnot(all(d$BEL_3 + d$rBEL_3 == 6))

bel_ideal <- c("BEL_1", "BEL_2", "BEL_4", "BEL_8")
bel_crit  <- c("rBEL_3", "rBEL_5", "rBEL_6", "rBEL_7", "rBEL_9")   # critical stance (formerly "gap")
esd_set   <- c("ESD_1", "ESD_2", "rESD_3", "ESD_4", "ESD_5", "ESD_6", "rESD_7", "ESD_8", "ESD_9")
eqd_set   <- c("EQD_1", "EQD_2", "rEQD_3", "EQD_4", "rEQD_5", "EQD_6", "EQD_7", "EQD_8", "rEQD_9")
bar_set   <- paste0("BAR_", 1:10)
app_self  <- c("APP_1", "APP_2", "APP_3", "APP_4", "APP_5", "APP_9")
esd_pos   <- c("ESD_1", "ESD_2", "ESD_4", "ESD_5", "ESD_6", "ESD_8", "ESD_9")
eqd_pos   <- c("EQD_1", "EQD_2", "EQD_4", "EQD_6", "EQD_7", "EQD_8")
eqd_neg   <- c("rEQD_3", "rEQD_5", "rEQD_9")

sc <- function(cols) rowMeans(d[, cols])
d <- d %>% mutate(IDEAL = sc(bel_ideal), CRIT = sc(bel_crit), ESD = sc(esd_set),
                  EQD = sc(eqd_set), BAR = sc(bar_set), APP = sc(app_self),
                  ESDpos = sc(esd_pos), EQDpos = sc(eqd_pos), EQDneg = sc(eqd_neg))
cm <- function(x) x - mean(x)
d <- d %>% mutate(IDEALc = cm(IDEAL), CRITc = cm(CRIT), ESDc = cm(ESD), BARc = cm(BAR),
                  ESDposc = cm(ESDpos))
m1 <- lm(EQD ~ IDEALc + CRITc + ESDc + BARc, data = d)
cat("Model 1 replication: R2 =", r3(summary(m1)$r.squared), "\n"); print(r3(coef(m1)))
stopifnot(abs(summary(m1)$r.squared - 0.492) < 0.001)

suppressPackageStartupMessages(library(lme4))
bg <- "gender + age + years_experience + education_level + school_area + specialty_grp + training_sustainability + training_democracy + rfcdc_known"

# ---------------------------------------------------------------------
# Cost coding and long format (shared by T1, T2, T6, T7)
# ---------------------------------------------------------------------
cc <- readr::read_csv(file.path(PROJECT_DIR, "item_cost_coding.csv"), show_col_types = FALSE) %>%
  filter(domain != "Belief")
modal <- function(x) { t <- table(x); as.numeric(names(t)[which.max(t)]) }
cc$cost <- apply(cc[, c("rater1", "rater2", "rater3")], 1, modal)
d$resp  <- factor(seq_len(nrow(d)))
d$BARz  <- as.numeric(scale(d$BAR))
long <- d %>% select(resp, BARz, training_sustainability, all_of(cc$item)) %>%
  pivot_longer(all_of(cc$item), names_to = "item", values_to = "y") %>%
  left_join(cc %>% select(item, cost, domain), by = "item") %>%
  mutate(item = factor(item), yo = factor(y, ordered = TRUE),
         train = as.numeric(training_sustainability == "Yes"),
         costc = cost - 2)
long_esd <- filter(long, domain == "ESD practice")
sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))

# ---------------------------------------------------------------------
# T1 — EXPLANATORY ITEM-RESPONSE MODELS
# ---------------------------------------------------------------------
step("T1", "Cost gradient at the respondent level: random slopes and cross-level interactions")
m0  <- lmer(y ~ 1 + (1 | resp) + (1 | item), data = long, REML = FALSE)
mc  <- lmer(y ~ costc + (1 | resp) + (1 | item), data = long, REML = FALSE)
vi0 <- as.data.frame(VarCorr(m0)); vi1 <- as.data.frame(VarCorr(mc))
iv0 <- vi0$vcov[vi0$grp == "item"]; iv1 <- vi1$vcov[vi1$grp == "item"]
cat("Item variance: null =", r3(iv0), "; with cost =", r3(iv1),
    "; proportion of between-item variance explained by cost =", r3(1 - iv1 / iv0), "\n")
mrs <- lmer(y ~ costc + (1 + costc | resp) + (1 | item), data = long, REML = FALSE)
lr  <- anova(mc, mrs)
cat("Random cost slope across teachers: LRT chi2(2) =", r3(lr$Chisq[2]), ", p =", signif(lr$`Pr(>Chisq)`[2], 3), "\n")
vr <- as.data.frame(VarCorr(mrs)); print(vr[, c("grp", "var1", "var2", "vcov", "sdcor")])
sd_slope <- vr$sdcor[vr$grp == "resp" & vr$var1 == "costc" & is.na(vr$var2)]
fix_slope <- fixef(mrs)["costc"]
cat("Teacher-specific slopes: mean =", r3(fix_slope), ", SD =", r3(sd_slope),
    "; middle 95% of teachers between", r3(fix_slope - 1.96 * sd_slope), "and", r3(fix_slope + 1.96 * sd_slope), "\n")

mx  <- lmerTest::lmer(y ~ costc * BARz + (1 + costc | resp) + (1 | item), data = long, REML = TRUE)
cat("\nCost x perceived barriers (BAR standardised), all 13 items:\n"); print(r3(summary(mx)$coefficients))
mxt <- lmerTest::lmer(y ~ costc * BARz + costc * train + (1 + costc | resp) + (1 | item), data = long, REML = TRUE)
cat("\nCost x barriers and cost x sustainability training, all 13 items:\n"); print(r3(summary(mxt)$coefficients))
mx_esd <- lmerTest::lmer(y ~ costc * BARz + (1 + costc | resp) + (1 | item), data = long_esd, REML = TRUE)
cat("\nCost x barriers, 7 ESD items:\n"); print(r3(summary(mx_esd)$coefficients))
mxd <- lmerTest::lmer(y ~ costc * BARz + domain + (1 + costc | resp) + (1 | item), data = long, REML = TRUE)
cat("\nCost x barriers, adjusting for domain:\n"); print(r3(summary(mxd)$coefficients))
b <- fixef(mx)
cat("Implied cost slope at BAR -1 SD / mean / +1 SD:", r3(b["costc"] - b["costc:BARz"]), r3(b["costc"]),
    r3(b["costc"] + b["costc:BARz"]), "\n")
clx <- ordinal::clmm(yo ~ costc * BARz + (1 | resp) + (1 | item), data = long, nAGQ = 1)
cat("\nCumulative-link mixed model with cost x barriers:\n"); print(r3(summary(clx)$coefficients[c("costc", "BARz", "costc:BARz"), ]))
cat("Odds ratios:", r3(exp(coef(clx)[c("costc", "BARz", "costc:BARz")])), "\n")
t1 <- rbind(
  data.frame(model = "LMM 13 items", term = rownames(summary(mx)$coefficients), summary(mx)$coefficients, row.names = NULL),
  data.frame(model = "LMM 13 items + training", term = rownames(summary(mxt)$coefficients), summary(mxt)$coefficients, row.names = NULL),
  data.frame(model = "LMM 7 ESD items", term = rownames(summary(mx_esd)$coefficients), summary(mx_esd)$coefficients, row.names = NULL),
  data.frame(model = "LMM 13 items + domain", term = rownames(summary(mxd)$coefficients), summary(mxd)$coefficients, row.names = NULL))
write.csv(t1, op("T1_cost_by_barriers.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# T2 — GRADIENT AT EVERY THRESHOLD OF THE FREQUENCY ANCHOR
# ---------------------------------------------------------------------
step("T2", "Gradient at every threshold of the frequency anchor")
thr <- do.call(rbind, lapply(2:5, function(k) {
  pct <- sapply(cc$item, function(i) 100 * mean(d[[i]] >= k))
  lv  <- tapply(pct, cc$cost, mean)
  e   <- cc$domain == "ESD practice"
  data.frame(threshold = paste0(">=", k), L1 = lv[1], L2 = lv[2], L3 = lv[3],
             rho_all = sp(cc$cost, pct), rho_esd = sp(cc$cost[e], pct[e]))
}))
thr[, -1] <- round(thr[, -1], 3); print(thr, row.names = FALSE)
never <- sapply(cc$item, function(i) 100 * mean(d[[i]] == 1))
cat("Share answering 'never', by item:\n"); print(round(setNames(never, cc$item), 1))
cat("Mean 'never' share by level:", round(tapply(never, cc$cost, mean), 1), "\n")
write.csv(thr, op("T2_thresholds.csv"), row.names = FALSE)
items_tab <- cc %>% mutate(statement = statement,
  pct_never = sapply(item, function(i) 100 * mean(d[[i]] == 1)),
  pct_ge3   = sapply(item, function(i) 100 * mean(d[[i]] >= 3)),
  pct_ge4   = sapply(item, function(i) 100 * mean(d[[i]] >= 4)),
  mean      = sapply(item, function(i) mean(d[[i]]))) %>% arrange(cost, desc(pct_ge4))
print(items_tab %>% select(item, domain, rater1, rater2, rater3, cost, mean, pct_never, pct_ge3, pct_ge4))
write.csv(items_tab, op("T2_item_table.csv"), row.names = FALSE)
glm_thr <- do.call(rbind, lapply(c(2, 3, 4), function(k) {
  long$z <- as.numeric(long$y >= k)
  g <- glmer(z ~ costc + (1 | resp) + (1 | item), data = long, family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
  s <- summary(g)$coefficients["costc", ]
  data.frame(outcome = paste0("y >= ", k), log_odds = s[1], se = s[2], z = s[3], p = s[4], OR = exp(s[1]))
}))
print(signif(glm_thr[, -1], 3)); write.csv(glm_thr, op("T2_glmm_thresholds.csv"), row.names = FALSE)
glm_esd <- { long_esd$z <- as.numeric(long_esd$y >= 2)
  g <- glmer(z ~ costc + (1 | resp) + (1 | item), data = long_esd, family = binomial,
             control = glmerControl(optimizer = "bobyqa")); summary(g)$coefficients["costc", ] }
cat("ESD items, never vs at least rarely: log-odds =", r3(glm_esd[1]), ", OR =", r3(exp(glm_esd[1])), ", p =", signif(glm_esd[4], 3), "\n")

# ---------------------------------------------------------------------
# T3 — BELIEF SCALE: WORDING EFFECTS
# ---------------------------------------------------------------------
step("T3", "Belief scale: wording method factor")
cat("Critical-stance items: BEL_3, BEL_5, BEL_7, BEL_9 negatively worded by design; BEL_6 positively worded, reflected empirically\n")
rev_all <- c("rBEL_3", "rBEL_5", "rBEL_7", "rBEL_9", "rESD_3", "rESD_7", "rEQD_3", "rEQD_5", "rEQD_9")
fac <- paste0("IDEALf =~ ", paste(bel_ideal, collapse = " + "), "\n",
              "CRITf  =~ ", paste(bel_crit, collapse = " + "), "\n",
              "ESDf   =~ ", paste(esd_set, collapse = " + "), "\n",
              "EQDf   =~ ", paste(eqd_set, collapse = " + "), "\n",
              "BARf   =~ ", paste(bar_set, collapse = " + "), "\n")
meth <- paste0("METH =~ ", paste(rev_all, collapse = " + "), "\n",
               paste0("METH ~~ 0*", c("IDEALf", "CRITf", "ESDf", "EQDf", "BARf"), collapse = "\n"), "\n")
cu <- { pr <- combn(rev_all, 2); paste(apply(pr, 2, function(x) paste(x[1], "~~", x[2])), collapse = "\n") }
bel2 <- paste0("IDEALf =~ ", paste(bel_ideal, collapse = " + "), "\nCRITf =~ ", paste(bel_crit, collapse = " + "), "\n")
bel1 <- paste0("G =~ ", paste(c(bel_ideal, bel_crit), collapse = " + "), "\n")
fb1 <- lavaan::cfa(bel1, data = d, estimator = "MLR")
fb2 <- lavaan::cfa(bel2, data = d, estimator = "MLR")
fj0 <- lavaan::cfa(fac, data = d, estimator = "MLR")
fj1 <- lavaan::cfa(paste0(fac, meth), data = d, estimator = "MLR")
fj2 <- lavaan::cfa(paste0(fac, cu), data = d, estimator = "MLR")
fmn <- c("chisq.scaled", "df", "cfi.robust", "tli.robust", "rmsea.robust", "srmr", "bic")
t3fit <- rbind("Beliefs, one factor" = lavaan::fitMeasures(fb1, fmn),
               "Beliefs, two factors" = lavaan::fitMeasures(fb2, fmn),
               "Five constructs" = lavaan::fitMeasures(fj0, fmn),
               "Five constructs + wording method factor (9 reverse-worded items)" = lavaan::fitMeasures(fj1, fmn),
               "Five constructs + correlated uniquenesses (9 reverse-worded items)" = lavaan::fitMeasures(fj2, fmn))
print(round(t3fit, 3)); write.csv(round(t3fit, 3), op("T3_fit.csv"))
print(lavaan::lavTestLRT(fj0, fj1, method = "satorra.bentler.2001"))
std <- function(f) lavaan::standardizedSolution(f)
lc <- function(f) { s <- std(f); s[s$op == "=~" & s$lhs %in% c("CRITf", "METH"), c("lhs", "rhs", "est.std", "pvalue")] }
cat("\nCritical-stance and method loadings, with method factor:\n"); print(lc(fj1))
cat("Critical-stance loadings, no method factor:\n"); print(lc(fj0))
lcor <- function(f, lab) { s <- std(f); s <- s[s$op == "~~" & s$lhs != s$rhs & s$lhs %in% c("IDEALf","CRITf","ESDf","EQDf","BARf") & s$rhs %in% c("IDEALf","CRITf","ESDf","EQDf","BARf"), ]
  data.frame(model = lab, pair = paste(s$lhs, s$rhs, sep = "-"), r = r3(s$est.std), lo = r3(s$ci.lower), hi = r3(s$ci.upper)) }
cors <- rbind(lcor(fj0, "no method"), lcor(fj1, "method factor"), lcor(fj2, "correlated uniquenesses"))
print(cors); write.csv(cors, op("T3_latent_correlations.csv"), row.names = FALSE)
# omega of the critical-stance composite net of the method factor
s1 <- lavaan::parameterEstimates(fj1, standardized = TRUE)
lamC <- s1$est[s1$op == "=~" & s1$lhs == "CRITf"]; names(lamC) <- s1$rhs[s1$op == "=~" & s1$lhs == "CRITf"]
lamM <- s1$est[s1$op == "=~" & s1$lhs == "METH"];  names(lamM) <- s1$rhs[s1$op == "=~" & s1$lhs == "METH"]
vC <- s1$est[s1$op == "~~" & s1$lhs == "CRITf" & s1$rhs == "CRITf"]; vM <- s1$est[s1$op == "~~" & s1$lhs == "METH" & s1$rhs == "METH"]
th <- sapply(bel_crit, function(i) s1$est[s1$op == "~~" & s1$lhs == i & s1$rhs == i])
lm_c <- ifelse(bel_crit %in% names(lamM), lamM[bel_crit], 0)
tot <- sum(lamC)^2 * vC + sum(lm_c)^2 * vM + sum(th)
cat("Critical stance composite: omega (content factor) =", r3(sum(lamC)^2 * vC / tot),
    "; share due to method factor =", r3(sum(lm_c)^2 * vM / tot), "\n")
# structural model with and without the wording factor
sem0 <- paste0(fac, "EQDf ~ IDEALf + CRITf + ESDf + BARf\n")
sem1 <- paste0(sem0, meth)
fs0 <- lavaan::sem(sem0, data = d, estimator = "MLR"); fs1 <- lavaan::sem(sem1, data = d, estimator = "MLR")
pth <- function(f, lab) { s <- std(f); s <- s[s$op == "~" & s$lhs == "EQDf", ]
  data.frame(model = lab, predictor = s$rhs, beta = r3(s$est.std), lo = r3(s$ci.lower), hi = r3(s$ci.upper), p = signif(s$pvalue, 3)) }
t3p <- rbind(pth(fs0, "SEM, no wording factor"), pth(fs1, "SEM, wording factor on 9 reverse-worded items"))
print(t3p); write.csv(t3p, op("T3_sem_paths.csv"), row.names = FALSE)
# critical stance without the empirically reflected item
d$CRIT4 <- rowMeans(d[, c("rBEL_3", "rBEL_5", "rBEL_7", "rBEL_9")]); d$CRIT4c <- cm(d$CRIT4)
cat("\nalpha CRIT4 =", r3(alpha_of <- psych::alpha(d[, c("rBEL_3","rBEL_5","rBEL_7","rBEL_9")], warnings = FALSE)$total$raw_alpha),
    "; r(CRIT, CRIT4) =", r3(cor(d$CRIT, d$CRIT4)), "\n")
m4 <- lm(EQD ~ IDEALc + CRIT4c + ESDc + BARc, data = d)
ct4 <- lmtest::coeftest(m4, vcov = sandwich::vcovHC(m4, type = "HC3")); print(r3(ct4)); cat("R2 =", r3(summary(m4)$r.squared), "\n")
cat("BEL_6 (unreflected) correlations: with EQD =", r3(cor(d$BEL_6, d$EQD)), "; with ESD =", r3(cor(d$BEL_6, d$ESD)),
    "; with BEL_5 =", r3(cor(d$BEL_6, d$BEL_5)), "\n")
cat("Ceiling: share at maximum, IDEAL =", r3(mean(d$IDEAL == 5)), "; CRIT =", r3(mean(d$CRIT == 5)), "\n")

# ---------------------------------------------------------------------
# T4 — BLOCKS IN ONE MODEL; STANDARDISED COEFFICIENTS
# ---------------------------------------------------------------------
step("T4", "Blocks of predictors and standardised coefficients")
blk <- list(B = bg, P = "IDEAL + CRIT + BAR", E = "ESD")
fitR2 <- function(sets) if (!length(sets)) 0 else summary(lm(as.formula(paste("EQD ~", paste(unlist(blk[sets]), collapse = " + "))), data = d))$r.squared
combos <- list(c(), "B", "P", "E", c("B","P"), c("B","E"), c("P","E"), c("B","P","E"))
r2tab <- data.frame(blocks = sapply(combos, function(x) if (!length(x)) "none" else paste(x, collapse = "+")),
                    R2 = sapply(combos, fitR2)); r2tab$R2 <- r3(r2tab$R2); print(r2tab)
full <- fitR2(c("B","P","E"))
inc <- data.frame(block = c("Background (9 characteristics, 16 df)", "Perceptions (idealistic beliefs, critical stance, barriers)", "ESD practice"),
                  alone = c(fitR2("B"), fitR2("P"), fitR2("E")),
                  last  = c(full - fitR2(c("P","E")), full - fitR2(c("B","E")), full - fitR2(c("B","P"))))
inc[, -1] <- r3(inc[, -1]); print(inc); cat("Full model R2 =", r3(full), "\n")
ftest <- function(small, big) { a <- anova(small, big); c(F = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p = a$`Pr(>F)`[2]) }
mB <- lm(as.formula(paste("EQD ~", bg)), data = d); mBP <- update(mB, . ~ . + IDEAL + CRIT + BAR); mBPE <- update(mBP, . ~ . + ESD)
mP <- lm(EQD ~ IDEAL + CRIT + BAR, data = d); mPE <- update(mP, . ~ . + ESD); mPEB <- lm(as.formula(paste("EQD ~ IDEAL + CRIT + BAR + ESD +", bg)), data = d)
mE <- lm(EQD ~ ESD, data = d); mBE <- lm(as.formula(paste("EQD ~ ESD +", bg)), data = d)
cat("Order 1: B -> P -> E\n"); print(signif(rbind(B = c(summary(mB)$fstatistic, p = pf(summary(mB)$fstatistic[1], summary(mB)$fstatistic[2], summary(mB)$fstatistic[3], lower.tail = FALSE)),
  P_after_B = ftest(mB, mBP), E_after_BP = ftest(mBP, mBPE)), 4))
cat("Order 2: P -> E -> B\n"); print(signif(rbind(E_after_P = ftest(mP, mPE), B_after_PE = ftest(mPE, mPEB)), 4))
cat("P after B and E:\n"); print(signif(ftest(mBE, mBPE), 4))
write.csv(inc, op("T4_blocks.csv"), row.names = FALSE); write.csv(r2tab, op("T4_R2_by_combination.csv"), row.names = FALSE)
# standardised coefficients, Model 1
dz_ <- d %>% mutate(across(c(EQD, IDEAL, CRIT, ESD, BAR), ~ as.numeric(scale(.x))))
ms <- lm(EQD ~ IDEAL + CRIT + ESD + BAR, data = dz_)
cts <- lmtest::coeftest(ms, vcov = sandwich::vcovHC(ms, type = "HC3")); cis <- lmtest::coefci(ms, vcov. = sandwich::vcovHC(ms, type = "HC3"))
beta <- data.frame(term = rownames(cts), beta = r3(cts[, 1]), se = r3(cts[, 2]), lo = r3(cis[, 1]), hi = r3(cis[, 2]), p = signif(cts[, 4], 3))
print(beta); write.csv(beta, op("T4_standardised_model1.csv"), row.names = FALSE)
cat("Standardised path coefficients (observed scores): a3 =", r3(coef(lm(scale(ESD) ~ scale(IDEAL) + scale(CRIT) + scale(BAR), d))[4]), "\n")
# Model 1 with background characteristics, standardised (continuous predictors only standardised)
mfull <- lm(as.formula(paste("EQD ~ IDEAL + CRIT + ESD + BAR +", bg)), data = dz_)
ctf <- lmtest::coeftest(mfull, vcov = sandwich::vcovHC(mfull, type = "HC3"))
print(r3(ctf[2:5, ]))

# ---------------------------------------------------------------------
# T5 — EQUIVALENCE AGAINST EDUCATIONAL BENCHMARKS
# ---------------------------------------------------------------------
step("T5", "Training equivalence: summary against benchmarks")
g1 <- d$ESD[d$training_sustainability == "Yes"]; g0 <- d$ESD[d$training_sustainability == "No"]
n1 <- length(g1); n0 <- length(g0); dfp <- n1 + n0 - 2
spool <- sqrt(((n1 - 1) * var(g1) + (n0 - 1) * var(g0)) / dfp); diffm <- mean(g1) - mean(g0)
se <- spool * sqrt(1 / n1 + 1 / n0)
for (lvl in c(0.90, 0.95)) { tc <- qt(1 - (1 - lvl) / 2, dfp)
  cat(sprintf("%.0f%% CI for d: [%.3f, %.3f]\n", 100 * lvl, (diffm - tc * se) / spool, (diffm + tc * se) / spool)) }
tost <- function(D) { pL <- pt((diffm + D * spool) / se, dfp, lower.tail = FALSE); pU <- pt((diffm - D * spool) / se, dfp); max(pL, pU) }
cat("p_TOST at 0.20 =", r3(tost(0.20)), "; at 0.35 =", signif(tost(0.35), 3), "\n")
# democratic practice by sustainability training, and ESD by democracy training
for (pair in list(c("EQD", "training_sustainability"), c("ESD", "training_democracy"))) {
  a <- d[[pair[1]]][d[[pair[2]]] == "Yes"]; b0 <- d[[pair[1]]][d[[pair[2]]] == "No"]
  s <- sqrt(((length(a) - 1) * var(a) + (length(b0) - 1) * var(b0)) / (length(a) + length(b0) - 2))
  cat(pair[1], "by", pair[2], ": d =", r3((mean(a) - mean(b0)) / s), "\n") }

# ---------------------------------------------------------------------
# T6 — OFFICIAL WORKFORCE STATISTICS AND RAKING WEIGHTS
# ---------------------------------------------------------------------
step("T6", "Sample vs Eurostat (educ_uoe_perp01, Greece, ISCED 1, 2024) and raking")
# Eurostat 2024 headcounts: total 77,598; females 59,569; <25 3,199; 25-29 8,587; 30-34 9,947;
# 35-39 8,859; 40-44 8,229; 45-49 7,207; 50+ 31,570. Survey bands are 22-30 / 31-40 / 41-50 / 51+,
# official bands <30 / 30-39 / 40-49 / 50+; the one-year offset is ignored.
pop_sex <- c(Female = 59569, Male = 18029) / 77598
pop_age <- c("22-30" = 3199 + 8587, "31-40" = 9947 + 8859, "41-50" = 8229 + 7207, "51+" = 31570) / 77598
cmp <- rbind(data.frame(variable = "gender", level = names(pop_sex), sample = as.numeric(prop.table(table(d$gender))[names(pop_sex)]), official = as.numeric(pop_sex)),
             data.frame(variable = "age", level = names(pop_age), sample = as.numeric(prop.table(table(d$age))[names(pop_age)]), official = as.numeric(pop_age)))
cmp[, 3:4] <- round(100 * cmp[, 3:4], 1); print(cmp); write.csv(cmp, op("T6_sample_vs_official.csv"), row.names = FALSE)
cat("Goodness of fit, gender: chi2 =", r3(chisq.test(table(d$gender)[names(pop_sex)], p = pop_sex)$statistic),
    "p =", signif(chisq.test(table(d$gender)[names(pop_sex)], p = pop_sex)$p.value, 3), "\n")
cat("Goodness of fit, age: chi2 =", r3(chisq.test(table(d$age)[names(pop_age)], p = pop_age)$statistic),
    "p =", signif(chisq.test(table(d$age)[names(pop_age)], p = pop_age)$p.value, 3), "\n")
w <- rep(1, nrow(d))
for (it in 1:100) {
  w <- w * pop_sex[as.character(d$gender)] / (tapply(w, d$gender, sum)[as.character(d$gender)] / sum(w))
  w <- w * pop_age[as.character(d$age)]    / (tapply(w, d$age, sum)[as.character(d$age)] / sum(w))
}
w <- as.numeric(w / mean(w)); d$w <- w
cat("Raking weights: range", r3(range(w)), "; Kish effective n =", r3(sum(w)^2 / sum(w^2)), "\n")
wm <- function(x) sum(w * x) / sum(w)
pct_w <- sapply(cc$item, function(i) 100 * wm(d[[i]] >= 4)); pct_u <- sapply(cc$item, function(i) 100 * mean(d[[i]] >= 4))
cat("Mean % reporting by level, unweighted:", round(tapply(pct_u, cc$cost, mean), 1), "; weighted:", round(tapply(pct_w, cc$cost, mean), 1), "\n")
cat("rho weighted: all =", r3(sp(cc$cost, pct_w)), "; ESD =", r3(sp(cc$cost[cc$domain == "ESD practice"], pct_w[cc$domain == "ESD practice"])), "\n")
cons <- c("IDEAL", "CRIT", "ESD", "EQD", "BAR", "APP")
print(data.frame(construct = cons, unweighted = r3(sapply(cons, function(v) mean(d[[v]]))), weighted = r3(sapply(cons, function(v) wm(d[[v]])))))
cat("Belief and adequacy items, weighted %: BEL_1", round(100 * wm(d$BEL_1 >= 4), 1), "; APP_1", round(100 * wm(d$APP_1 >= 4), 1), "\n")
mw <- lm(EQD ~ IDEALc + CRITc + ESDc + BARc, data = d, weights = w)
ctw <- lmtest::coeftest(mw, vcov = sandwich::vcovHC(mw, type = "HC3")); print(r3(ctw)); cat("weighted R2 =", r3(summary(mw)$r.squared), "\n")
mtw <- lm(ESD ~ training_sustainability, data = d, weights = w)
ctt <- lmtest::coeftest(mtw, vcov = sandwich::vcovHC(mtw, type = "HC3"))
cat("Weighted training difference (raw) =", r3(ctt[2, 1]), "SE =", r3(ctt[2, 2]), "; in pooled-SD units =", r3(ctt[2, 1] / spool),
    "; 90% CI d [", r3((ctt[2, 1] - 1.645 * ctt[2, 2]) / spool), ",", r3((ctt[2, 1] + 1.645 * ctt[2, 2]) / spool), "]\n")
mBw <- lm(as.formula(paste("EQD ~", bg)), data = d, weights = w)
cat("Weighted background-block R2 =", r3(summary(mBw)$r.squared), "\n")
lw <- long %>% left_join(d %>% select(resp, w), by = "resp")
lmw <- lmer(y ~ costc + (1 | resp) + (1 | item), data = lw, weights = w)
cat("Weighted LMM cost slope =", r3(fixef(lmw)["costc"]), "\n")

# ---------------------------------------------------------------------
# T7 — TIMING OF RESPONSES
# ---------------------------------------------------------------------
step("T7", "Timing of responses")
d$date <- as.Date(substr(d$timestamp, 1, 10))
d$period3 <- cut(d$date, breaks = as.Date(c("2026-05-01", "2026-07-01", "2026-09-01", "2026-10-01")),
                 labels = c("May-June", "July-August", "September"), right = FALSE)
d$sept <- factor(ifelse(d$period3 == "September", "September", "Before September"), levels = c("Before September", "September"))
print(table(d$period3)); print(table(d$sept))
cat("First and last response:", format(min(d$date)), format(max(d$date)), "\n")
t7 <- do.call(rbind, lapply(cons, function(v) {
  a <- d[[v]][d$sept == "September"]; b0 <- d[[v]][d$sept != "September"]
  s <- sqrt(((length(a) - 1) * var(a) + (length(b0) - 1) * var(b0)) / (length(a) + length(b0) - 2))
  aov3 <- summary(aov(d[[v]] ~ d$period3))[[1]]
  data.frame(construct = v, M_before = mean(b0), M_sept = mean(a), d = (mean(a) - mean(b0)) / s,
             p_welch = t.test(a, b0)$p.value, p_anova3 = aov3$`Pr(>F)`[1]) }))
t7$p_BH <- p.adjust(t7$p_welch, "BH"); t7[, -1] <- signif(t7[, -1], 3); print(t7)
write.csv(t7, op("T7_timing_constructs.csv"), row.names = FALSE)
lt <- long %>% left_join(d %>% select(resp, sept), by = "resp")
mt <- lmerTest::lmer(y ~ costc * sept + (1 | resp) + (1 | item), data = lt)
print(r3(summary(mt)$coefficients))
for (s in levels(d$sept)) { ds <- d[d$sept == s, ]
  pct <- sapply(cc$item, function(i) 100 * mean(ds[[i]] >= 4))
  cat(s, ": level means", round(tapply(pct, cc$cost, mean), 1), "; rho all =", r3(sp(cc$cost, pct)), "\n") }
m1s <- lm(EQD ~ (IDEALc + CRITc + ESDc + BARc) * sept, data = d)
cat("Model 1 x period: F-test of the five period terms\n"); print(anova(m1, m1s))
print(r3(lmtest::coeftest(m1s, vcov = sandwich::vcovHC(m1s, type = "HC3"))))
cat("School setting by period:\n"); print(round(prop.table(table(d$sept, d$school_area), 1), 3))
for (s in levels(d$sept)) { ds <- d[d$sept == s, ]
  cat(s, ": eta2 of school setting on ESD =", r3(summary(lm(ESD ~ school_area, ds))$r.squared),
      ", on EQD =", r3(summary(lm(EQD ~ school_area, ds))$r.squared), ", on BAR =", r3(summary(lm(BAR ~ school_area, ds))$r.squared), "\n") }
cat("\nThree periods (May-June = school year; July-August = summer; September = new school year):\n")
print(aggregate(cbind(IDEAL, CRIT, ESD, EQD, BAR, APP) ~ period3, d, function(x) round(mean(x), 3)))
p3 <- sapply(cons, function(v) summary(aov(d[[v]] ~ d$period3))[[1]]$`Pr(>F)`[1])
print(data.frame(construct = cons, p_anova = signif(p3, 3), p_BH = signif(p.adjust(p3, "BH"), 3),
                 eta2 = r3(sapply(cons, function(v) summary(lm(d[[v]] ~ d$period3))$r.squared))))
print(TukeyHSD(aov(BAR ~ period3, d)))
bm <- d$BAR[d$period3 == "May-June"]; bl <- d$BAR[d$period3 != "May-June"]
cat("BAR, May-June vs later: d =", r3((mean(bl) - mean(bm)) / sqrt(((length(bm)-1)*var(bm) + (length(bl)-1)*var(bl)) / (length(bm)+length(bl)-2))), "\n")
lt3 <- long %>% left_join(d %>% select(resp, period3), by = "resp")
mt3 <- lmerTest::lmer(y ~ costc * period3 + (1 | resp) + (1 | item), data = lt3); print(r3(summary(mt3)$coefficients))
for (s in levels(d$period3)) { ds <- d[d$period3 == s, ]; pct <- sapply(cc$item, function(i) 100 * mean(ds[[i]] >= 4))
  cat(s, ": level means", round(tapply(pct, cc$cost, mean), 1), "; rho all =", r3(sp(cc$cost, pct)), "\n") }
m1p <- lm(EQD ~ (IDEALc + CRITc + ESDc + BARc) * period3, data = d); print(anova(m1, m1p))
m1pb <- lm(EQD ~ IDEALc + CRITc + ESDc + BARc + period3, data = d); print(r3(coef(m1pb)))
cat("Responses 21-30 September:", sum(d$date >= as.Date("2026-09-21")), "\n")
cat("Age 22-30 share by period:\n"); print(round(prop.table(table(d$sept, d$age), 1), 3))

# ---------------------------------------------------------------------
# T8 — STAFFING PROXIES
# ---------------------------------------------------------------------
step("T8", "Youngest and least experienced teachers")
grp_d <- function(flag, lab) do.call(rbind, lapply(c("ESD", "EQD", "BAR"), function(v) {
  a <- d[[v]][flag]; b0 <- d[[v]][!flag]
  s <- sqrt(((length(a) - 1) * var(a) + (length(b0) - 1) * var(b0)) / (length(a) + length(b0) - 2))
  data.frame(group = lab, construct = v, n = length(a), d = (mean(a) - mean(b0)) / s, p = t.test(a, b0)$p.value) }))
t8 <- rbind(grp_d(d$years_experience == "0-5", "0-5 years of service"), grp_d(d$age == "22-30", "aged 22-30"))
t8[, 4:5] <- signif(t8[, 4:5], 3); print(t8); write.csv(t8, op("T8_staffing_proxies.csv"), row.names = FALSE)
cat("Spearman, years of service (ordinal) with ESD, EQD, BAR:",
    r3(sapply(c("ESD", "EQD", "BAR"), function(v) sp(as.numeric(d$years_experience), d[[v]]))), "\n")

# ---------------------------------------------------------------------
# T9 — ITEM-REST CORRELATIONS (FORM LAYOUT CHECK)
# ---------------------------------------------------------------------
step("T9", "Corrected item-rest correlations")
sets <- list(IDEAL = bel_ideal, CRIT = bel_crit, ESD = esd_set, EQD = eqd_set, BAR = bar_set, APP = app_self)
ir <- do.call(rbind, lapply(names(sets), function(s) { cols <- sets[[s]]
  data.frame(scale = s, item = cols, r_rest = sapply(cols, function(i) cor(d[[i]], rowMeans(d[, setdiff(cols, i), drop = FALSE])))) }))
ir$r_rest <- r3(ir$r_rest); print(ir, row.names = FALSE)
cat("Minimum item-rest r:", min(ir$r_rest), "\n")
cat("APP_7, APP_8 correlations with BAR:", r3(cor(d$APP_7, d$BAR)), r3(cor(d$APP_8, d$BAR)), "; with APP:", r3(cor(d$APP_7, d$APP)), r3(cor(d$APP_8, d$APP)), "\n")
write.csv(ir, op("T9_item_rest.csv"), row.names = FALSE)

cat("\n\nDone. Session info:\n"); print(sessionInfo())
