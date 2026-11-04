# =====================================================================
#  REVISION ANALYSES (Round 1) — Education for Sustainability,
#  Democracy & Equality in Greek Primary Education (N = 391)
#
#  Additional analyses requested by Reviewer 2. Run AFTER (or
#  independently of) national_edu_analysis.R; this script rebuilds the
#  construct scores itself so it can be sourced on its own.
#
#   R1  Practice items: oblique EFA (2 and 3 factors) + belief EFA
#   R2  CFA comparison of competing measurement models (MLR)
#   R3  Reliability beyond alpha: omega; discriminant validity: HTMT
#   R4  Hierarchical regression: background -> perceptions -> ESD practice
#   R5  Critical-stance coefficient with positively worded items only
#   R6  Cost gradient: leave-one-item-out, exact permutation,
#       respondent-level means, crossed random-effects models (lme4, clmm)
#   R7  Equivalence curve for sustainability training (TOST)
#   R8  Common-method diagnostics: Harman test + latent method factor (ULMC)
#   R9  Reverse-ordered path model
#   R10 Early-career teachers (0-5 years) vs the rest
#
#  NOTE ON NAMING: the belief subscale formerly called "perceived
#  implementation gap" (BEL_GAP) is renamed "critical stance toward ESD"
#  (CRIT) in the revision. Items and scoring are unchanged.
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
OUT <- file.path(PROJECT_DIR, "results_revision")
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

# ---------------------------------------------------------------------
# R1 — OBLIQUE EFA
# ---------------------------------------------------------------------
step("R1", "Exploratory factor analysis with oblique (oblimin) rotation")
prac <- d[, c(esd_set, eqd_set)]
efa2 <- psych::fa(prac, nfactors = 2, fm = "minres", rotate = "oblimin")
efa3 <- psych::fa(prac, nfactors = 3, fm = "minres", rotate = "oblimin")
belf <- psych::fa(d[, c(bel_ideal, bel_crit)], nfactors = 2, fm = "minres", rotate = "oblimin")
ld <- function(f) { L <- unclass(f$loadings); L <- round(L, 3); as.data.frame(L) }
L2 <- ld(efa2); L3 <- ld(efa3); LB <- ld(belf)
cat("\nPractice items, 2 factors (oblimin):\n"); print(L2)
cat("Factor correlation:\n"); print(r3(efa2$Phi))
cat("\nPractice items, 3 factors (oblimin):\n"); print(L3)
cat("Factor correlations:\n"); print(r3(efa3$Phi))
cat("\nBelief items, 2 factors (oblimin):\n"); print(LB); print(r3(belf$Phi))
write.csv(cbind(item = rownames(L2), L2), op("R1_efa2_practice_oblimin.csv"), row.names = FALSE)
write.csv(cbind(item = rownames(L3), L3), op("R1_efa3_practice_oblimin.csv"), row.names = FALSE)
write.csv(cbind(item = rownames(LB), LB), op("R1_efa2_belief_oblimin.csv"), row.names = FALSE)
cat("Variance accounted for (2-factor practice):\n"); print(r3(efa2$Vaccounted))
cat("Variance accounted for (3-factor practice):\n"); print(r3(efa3$Vaccounted))

# ---------------------------------------------------------------------
# R2 — CFA COMPARISON OF COMPETING MEASUREMENT MODELS (MLR)
# ---------------------------------------------------------------------
step("R2", "CFA: one factor / two factors / two factors + wording method factor / three factors")
rev_prac <- c("rESD_3", "rESD_7", "rEQD_3", "rEQD_5", "rEQD_9")
mA <- paste("G =~", paste(c(esd_set, eqd_set), collapse = " + "))
mB <- paste0("ESDf =~ ", paste(esd_set, collapse = " + "), "\n",
             "EQDf =~ ", paste(eqd_set, collapse = " + "))
mC <- paste0(mB, "\nMETH =~ ", paste(rev_prac, collapse = " + "),
             "\nMETH ~~ 0*ESDf\nMETH ~~ 0*EQDf")
# three-factor: each item on its primary loading in the 3-factor EFA
prim <- apply(abs(as.matrix(L3)), 1, which.max)
f3 <- split(names(prim), prim)
mD <- paste(sapply(seq_along(f3), function(k)
  paste0("F", k, " =~ ", paste(f3[[k]], collapse = " + "))), collapse = "\n")
cat("Three-factor model, item assignment by primary EFA loading:\n"); print(f3)
fits <- list(
  "1 factor"                       = lavaan::cfa(mA, data = d, estimator = "MLR"),
  "2 factors (ESD, democratic)"    = lavaan::cfa(mB, data = d, estimator = "MLR"),
  "2 factors + wording method factor" = lavaan::cfa(mC, data = d, estimator = "MLR"),
  "3 factors (EFA structure)"      = lavaan::cfa(mD, data = d, estimator = "MLR"))
fm_names <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "tli.robust",
              "rmsea.robust", "srmr", "bic")
cfa_tab <- t(sapply(fits, function(f) lavaan::fitMeasures(f, fm_names)))
cfa_tab <- data.frame(model = rownames(cfa_tab), round(cfa_tab, 3), row.names = NULL)
print(cfa_tab)
write.csv(cfa_tab, op("R2_cfa_comparison.csv"), row.names = FALSE)
cat("\nFactor correlation ESD-democratic, model with method factor:\n")
print(subset(lavaan::standardizedSolution(fits[[3]]), op == "~~" & lhs == "ESDf" & rhs == "EQDf")[, c("lhs","rhs","est.std","ci.lower","ci.upper")])
cat("Method-factor loadings (standardised):\n")
print(subset(lavaan::standardizedSolution(fits[[3]]), op == "=~" & lhs == "METH")[, c("rhs","est.std","pvalue")])
cat("Scaled chi-square difference, 2 factors vs 2 factors + method factor:\n")
print(lavaan::lavTestLRT(fits[[2]], fits[[3]], method = "satorra.bentler.2001"))

# ---------------------------------------------------------------------
# R3 — OMEGA AND HTMT
# ---------------------------------------------------------------------
step("R3", "Reliability (omega) and discriminant validity (HTMT)")
scales <- list(IDEAL = bel_ideal, CRIT = bel_crit, ESD = esd_set, EQD = eqd_set,
               BAR = bar_set, APP = app_self)
omega_tot <- function(cols) {         # one-factor CFA omega (McDonald)
  f <- lavaan::cfa(paste("F =~", paste(cols, collapse = " + ")), data = d,
                   estimator = "MLR", std.lv = TRUE)
  pe <- lavaan::parameterEstimates(f)
  l <- pe$est[pe$op == "=~"]; th <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs != "F"]
  sum(l)^2 / (sum(l)^2 + sum(th))
}
alpha_of <- function(cols) psych::alpha(d[, cols], check.keys = FALSE, warnings = FALSE)$total$raw_alpha
rel <- data.frame(construct = names(scales), k = lengths(scales),
                  alpha = sapply(scales, alpha_of), omega = sapply(scales, omega_tot))
rel[, 3:4] <- round(rel[, 3:4], 3); print(rel)
write.csv(rel, op("R3_reliability_alpha_omega.csv"), row.names = FALSE)
htmt <- function(a, b) {
  C <- abs(cor(d[, c(a, b)]))
  het  <- mean(C[a, b])
  mono <- function(s) { m <- C[s, s]; mean(m[upper.tri(m)]) }
  het / sqrt(mono(a) * mono(b))
}
nm <- names(scales); H <- matrix(NA, length(nm), length(nm), dimnames = list(nm, nm))
for (i in seq_along(nm)) for (j in seq_along(nm)) if (i > j) H[i, j] <- htmt(scales[[i]], scales[[j]])
cat("\nHTMT (lower triangle):\n"); print(round(H, 3))
write.csv(round(H, 3), op("R3_htmt.csv"))

# ---------------------------------------------------------------------
# R4 — HIERARCHICAL REGRESSION
# ---------------------------------------------------------------------
step("R4", "Hierarchical regression for democratic practice")
bg <- "gender + age + years_experience + education_level + school_area + specialty_grp + training_sustainability + training_democracy + rfcdc_known"
h1 <- lm(as.formula(paste("EQD ~", bg)), data = d)
h2 <- update(h1, . ~ . + IDEAL + CRIT + BAR)
h3 <- update(h2, . ~ . + ESD)
p0 <- lm(EQD ~ IDEAL + CRIT + BAR, data = d)
an <- anova(h1, h2, h3)
hier <- data.frame(step = c("1 Background (9 characteristics)",
                            "2 + idealistic beliefs, critical stance, barriers",
                            "3 + ESD practice"),
                   df_model = c(length(coef(h1)) - 1, length(coef(h2)) - 1, length(coef(h3)) - 1),
                   R2 = c(summary(h1)$r.squared, summary(h2)$r.squared, summary(h3)$r.squared))
hier$dR2 <- c(hier$R2[1], diff(hier$R2))
hier$F_change <- c(summary(h1)$fstatistic[1], an$F[2], an$F[3])
hier$df1 <- c(summary(h1)$fstatistic[2], an$Df[2], an$Df[3])
hier$df2 <- c(summary(h1)$fstatistic[3], an$Res.Df[2], an$Res.Df[3])
hier$p <- c(pf(summary(h1)$fstatistic[1], summary(h1)$fstatistic[2], summary(h1)$fstatistic[3], lower.tail = FALSE),
            an$`Pr(>F)`[2], an$`Pr(>F)`[3])
hier[, -1] <- signif(hier[, -1], 4); print(hier)
cat("Perceptions only (no background, no ESD practice): R2 =", r3(summary(p0)$r.squared), "\n")
write.csv(hier, op("R4_hierarchical_regression.csv"), row.names = FALSE)
# ESD practice as outcome, for completeness
e1 <- lm(as.formula(paste("ESD ~", bg)), data = d); e2 <- update(e1, . ~ . + IDEAL + CRIT + BAR)
cat("ESD practice as outcome: background R2 =", r3(summary(e1)$r.squared),
    "; + perceptions R2 =", r3(summary(e2)$r.squared), "\n")

# ---------------------------------------------------------------------
# R5 — CRITICAL STANCE WITH POSITIVELY WORDED ITEMS ONLY
# ---------------------------------------------------------------------
step("R5", "Model 1 re-estimated with positively worded outcome and ESD items")
cat("r(CRIT, democratic positive items) =", r3(cor(d$CRIT, d$EQDpos)),
    "; r(CRIT, democratic reverse items) =", r3(cor(d$CRIT, d$EQDneg)), "\n")
m5 <- lm(EQDpos ~ IDEALc + CRITc + ESDposc + BARc, data = d)
ct5 <- lmtest::coeftest(m5, vcov = sandwich::vcovHC(m5, type = "HC3"))
ci5 <- lmtest::coefci(m5, vcov. = sandwich::vcovHC(m5, type = "HC3"))
r5 <- data.frame(term = rownames(ct5), b = ct5[, 1], se_HC3 = ct5[, 2], p = ct5[, 4],
                 ci_lo = ci5[, 1], ci_hi = ci5[, 2], row.names = NULL)
r5[, -1] <- r3(r5[, -1]); print(r5); cat("R2 =", r3(summary(m5)$r.squared), "\n")
write.csv(r5, op("R5_positive_items_model.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# R6 — COST GRADIENT: ITEM, RESPONDENT AND CROSSED RANDOM-EFFECTS LEVELS
# ---------------------------------------------------------------------
step("R6", "Cost gradient")
cc <- readr::read_csv(file.path(PROJECT_DIR, "item_cost_coding.csv"), show_col_types = FALSE)
cc <- cc %>% filter(domain != "Belief")
modal <- function(x) { t <- table(x); as.numeric(names(t)[which.max(t)]) }
cc$cost <- apply(cc[, c("rater1", "rater2", "rater3")], 1, modal)
cc$pct  <- sapply(cc$item, function(i) 100 * mean(d[[i]] >= 4))
cc$mean <- sapply(cc$item, function(i) mean(d[[i]]))
print(cc[, c("item", "domain", "rater1", "rater2", "rater3", "cost", "pct", "mean")])
write.csv(cc, op("R6_item_level_cost_table.csv"), row.names = FALSE)

sp <- function(x, y) suppressWarnings(cor(x, y, method = "spearman"))
esd_cc <- cc %>% filter(domain == "ESD practice")
eqd_cc <- cc %>% filter(domain == "Democratic practice")
cat("\nSpearman rho: all =", r3(sp(cc$cost, cc$pct)), "; ESD =", r3(sp(esd_cc$cost, esd_cc$pct)),
    "; democratic =", r3(sp(eqd_cc$cost, eqd_cc$pct)), "\n")
loo <- function(tab) sapply(seq_len(nrow(tab)), function(i) sp(tab$cost[-i], tab$pct[-i]))
loo_all <- loo(cc); loo_esd <- loo(esd_cc)
cat("Leave-one-item-out, all items: range [", r3(min(loo_all)), ",", r3(max(loo_all)), "]\n")
cat("Leave-one-item-out, ESD items: range [", r3(min(loo_esd)), ",", r3(max(loo_esd)), "]\n")
write.csv(data.frame(dropped = c(cc$item, esd_cc$item),
                     set = c(rep("all 13", nrow(cc)), rep("ESD 7", nrow(esd_cc))),
                     rho = r3(c(loo_all, loo_esd))), op("R6_leave_one_out.csv"), row.names = FALSE)
# exact permutation test within the 7 ESD items (5040 permutations)
perms <- function(v) { if (length(v) == 1) return(list(v)); out <- list()
  for (i in seq_along(v)) for (p in perms(v[-i])) out[[length(out) + 1]] <- c(v[i], p); out }
obs <- sp(esd_cc$cost, esd_cc$pct)
null <- sapply(perms(esd_cc$pct), function(p) sp(esd_cc$cost, p))
cat("Exact permutation p (ESD items, one-sided) =", r3(mean(null <= obs + 1e-12)),
    " over", length(null), "permutations\n")

# respondent-level means by cost level
lv <- split(cc$item, cc$cost)
for (k in names(lv)) d[[paste0("L", k)]] <- rowMeans(d[, lv[[k]], drop = FALSE])
cat("\nItems per level:", sapply(lv, length), "\n")
cat("Respondent-level means L1, L2, L3:", r3(colMeans(d[, c("L1", "L2", "L3")])), "\n")
cat("Share never higher at a costlier level (L1>=L2>=L3):", r3(mean(d$L1 >= d$L2 & d$L2 >= d$L3)), "\n")
cat("Share L1 > L3:", r3(mean(d$L1 > d$L3)), "\n")
dz <- function(x) mean(x) / sd(x)
for (pr in list(c("L1", "L2"), c("L2", "L3"), c("L1", "L3"))) {
  df_ <- d[[pr[1]]] - d[[pr[2]]]
  cat(sprintf("%s - %s: mean diff = %.3f, dz = %.2f, paired t(390) = %.2f, Wilcoxon p = %.2g\n",
              pr[1], pr[2], mean(df_), dz(df_), t.test(df_)$statistic, wilcox.test(df_)$p.value))
}
esd_lv <- split(esd_cc$item, esd_cc$cost)
for (k in names(esd_lv)) d[[paste0("E", k)]] <- rowMeans(d[, esd_lv[[k]], drop = FALSE])
cat("ESD-only respondent means E1, E2, E3:", r3(colMeans(d[, c("E1", "E2", "E3")])), "\n")

# crossed random-effects models: item responses within teachers x items
long <- d %>% mutate(resp = row_number()) %>%
  select(resp, all_of(cc$item)) %>%
  pivot_longer(-resp, names_to = "item", values_to = "y") %>%
  left_join(cc %>% select(item, cost, domain), by = "item") %>%
  mutate(resp = factor(resp), item = factor(item))
lmm <- lmerTest::lmer(y ~ cost + (1 | resp) + (1 | item), data = long, REML = TRUE)
cat("\nLinear mixed model, all 13 items:\n"); print(summary(lmm)$coefficients)
print(as.data.frame(lme4::VarCorr(lmm))[, c("grp", "vcov", "sdcor")])
ci_lmm <- confint(lmm, parm = "cost", method = "profile")
cat("Profile 95% CI for cost slope:", r3(ci_lmm), "\n")
lmm_esd <- lmerTest::lmer(y ~ cost + (1 | resp) + (1 | item), data = filter(long, domain == "ESD practice"))
cat("\nLinear mixed model, ESD items only:\n"); print(summary(lmm_esd)$coefficients)
lmm_dom <- lmerTest::lmer(y ~ cost + domain + (1 | resp) + (1 | item), data = long)
cat("\nLinear mixed model, all items, adjusting for domain (anchor):\n"); print(summary(lmm_dom)$coefficients)
long$yo <- factor(long$y, ordered = TRUE)
clm1 <- ordinal::clmm(yo ~ cost + (1 | resp) + (1 | item), data = long, nAGQ = 1)
cat("\nCumulative-link mixed model (logit), all 13 items:\n"); print(summary(clm1)$coefficients["cost", ])
cat("Odds ratio per cost level:", r3(exp(coef(clm1)["cost"])), "\n")
mix <- rbind(
  data.frame(model = "LMM, 13 items", t(summary(lmm)$coefficients["cost", ])),
  data.frame(model = "LMM, 7 ESD items", t(summary(lmm_esd)$coefficients["cost", ])),
  data.frame(model = "LMM, 13 items + domain", t(summary(lmm_dom)$coefficients["cost", ])))
print(mix); write.csv(mix, op("R6_mixed_models.csv"), row.names = FALSE)
cl <- summary(clm1)$coefficients["cost", ]
write.csv(data.frame(model = "CLMM logit, 13 items", estimate = cl[1], se = cl[2], z = cl[3], p = cl[4],
                     odds_ratio = exp(cl[1])), op("R6_clmm.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# R7 — EQUIVALENCE CURVE (TOST, pooled SD, Student df)
# ---------------------------------------------------------------------
step("R7", "Equivalence curve: sustainability training vs ESD practice")
g1 <- d$ESD[d$training_sustainability == "Yes"]; g0 <- d$ESD[d$training_sustainability == "No"]
n1 <- length(g1); n0 <- length(g0); dfp <- n1 + n0 - 2
spool <- sqrt(((n1 - 1) * var(g1) + (n0 - 1) * var(g0)) / dfp)
diffm <- mean(g1) - mean(g0); se <- spool * sqrt(1 / n1 + 1 / n0); tcrit <- qt(0.95, dfp)
cat(sprintf("Trained M = %.3f (n = %d), untrained M = %.3f (n = %d), pooled SD = %.3f\n",
            mean(g1), n1, mean(g0), n0, spool))
cat(sprintf("d = %.3f, 90%% CI [%.3f, %.3f]; raw difference = %.3f, 90%% CI [%.3f, %.3f]\n",
            diffm / spool, (diffm - tcrit * se) / spool, (diffm + tcrit * se) / spool,
            diffm, diffm - tcrit * se, diffm + tcrit * se))
eq <- do.call(rbind, lapply(c(0.20, 0.25, 0.30, 0.35, 0.40, 0.50), function(D) {
  tL <- (diffm + D * spool) / se; tU <- (diffm - D * spool) / se
  pL <- pt(tL, dfp, lower.tail = FALSE); pU <- pt(tU, dfp)
  data.frame(bound_d = D, bound_raw = D * spool, t_L = tL, t_U = tU, df = dfp,
             p_L = pL, p_U = pU, p_TOST = max(pL, pU))
}))
eq[, -5] <- signif(eq[, -5], 4); print(eq)
cat("Smallest bound at which equivalence holds (alpha = .05): d =",
    r3(max(abs(c((diffm - tcrit * se) / spool, (diffm + tcrit * se) / spool)))), "\n")
write.csv(eq, op("R7_equivalence_curve.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# R8 — COMMON-METHOD DIAGNOSTICS
# ---------------------------------------------------------------------
step("R8", "Common-method diagnostics")
all_items <- c(bel_ideal, bel_crit, esd_set, eqd_set, bar_set, paste0("APP_", 1:9))
h1f <- psych::fa(d[, all_items], nfactors = 1, fm = "minres", rotate = "none")
cat("Harman single-factor test: first factor explains",
    r3(h1f$Vaccounted["Proportion Var", 1]), "of the variance across", length(all_items), "items\n")
sem_base <- paste0(
  "IDEALL =~ ", paste(bel_ideal, collapse = " + "), "\n",
  "CRITL  =~ ", paste(bel_crit,  collapse = " + "), "\n",
  "ESDL  =~ ", paste(esd_set,   collapse = " + "), "\n",
  "EQDL  =~ ", paste(eqd_set,   collapse = " + "), "\n",
  "BARL  =~ ", paste(bar_set,   collapse = " + "), "\n",
  "EQDL ~ IDEALL + CRITL + ESDL + BARL\n")
sem_items <- c(bel_ideal, bel_crit, esd_set, eqd_set, bar_set)
sem_ulmc <- paste0(sem_base,
  "METHOD =~ ", paste(paste0("m*", sem_items), collapse = " + "), "\n",
  paste0("METHOD ~~ 0*", c("IDEALL", "CRITL", "ESDL", "EQDL", "BARL"), collapse = "\n"), "\n")
fs0 <- lavaan::sem(sem_base, data = d, estimator = "MLR")
fs1 <- lavaan::sem(sem_ulmc, data = d, estimator = "MLR")
paths <- function(f, lab) {
  s <- lavaan::standardizedSolution(f); s <- s[s$lhs == "EQDL" & s$op == "~", ]
  data.frame(model = lab, predictor = s$rhs, beta = r3(s$est.std), se = r3(s$se), p = signif(s$pvalue, 3))
}
ulmc <- rbind(paths(fs0, "SEM without method factor"), paths(fs1, "SEM with latent method factor"))
print(ulmc)
fitc <- rbind(round(lavaan::fitMeasures(fs0, c("chisq.scaled", "df", "cfi.robust", "tli.robust", "rmsea.robust", "srmr")), 3),
              round(lavaan::fitMeasures(fs1, c("chisq.scaled", "df", "cfi.robust", "tli.robust", "rmsea.robust", "srmr")), 3))
rownames(fitc) <- c("without method factor", "with method factor"); print(fitc)
s1 <- lavaan::standardizedSolution(fs1); ml <- s1$est.std[s1$lhs == "METHOD" & s1$op == "=~"]
cat("Method factor: median standardised loading =", r3(median(ml)),
    "; mean share of item variance =", r3(mean(ml^2)), "\n")
R2s <- lavaan::inspect(fs0, "r2")["EQDL"]; R2u <- lavaan::inspect(fs1, "r2")["EQDL"]
cat("Latent R2 for democratic practice: without method factor =", r3(R2s), "; with =", r3(R2u), "\n")
write.csv(ulmc, op("R8_ulmc_paths.csv"), row.names = FALSE)
write.csv(fitc, op("R8_ulmc_fit.csv"))

# ---------------------------------------------------------------------
# R9 — REVERSE-ORDERED PATH MODEL
# ---------------------------------------------------------------------
step("R9", "Path models: original ordering vs reverse ordering (5,000 bootstrap resamples)")
orig <- '
  ESD ~ a1*IDEAL + a2*CRIT + a3*BAR
  EQD ~ b*ESD + c1*IDEAL + c2*CRIT + c3*BAR
  ind_bar := a3*b
  tot_bar := c3 + a3*b
'
reverse <- '
  EQD ~ a1*IDEAL + a2*CRIT + a3*BAR
  ESD ~ b*EQD + c1*IDEAL + c2*CRIT + c3*BAR
  ind_bar := a3*b
  tot_bar := c3 + a3*b
  prop_ind := (a3*b)/(c3 + a3*b)
'
fo <- lavaan::sem(orig, data = d, se = "bootstrap", bootstrap = 5000)
fr <- lavaan::sem(reverse, data = d, se = "bootstrap", bootstrap = 5000)
pe <- function(f, lab) { p <- lavaan::parameterEstimates(f, boot.ci.type = "perc")
  p <- p[p$op %in% c("~", ":="), c("lhs", "op", "rhs", "label", "est", "se", "pvalue", "ci.lower", "ci.upper")]
  p[, 5:9] <- r3(p[, 5:9]); cbind(model = lab, p) }
pm <- rbind(pe(fo, "original: barriers -> ESD -> democratic"),
            pe(fr, "reverse: barriers -> democratic -> ESD"))
print(pm)
cat("Both models are just-identified: chi-square =", r3(lavaan::fitMeasures(fo, "chisq")), "and",
    r3(lavaan::fitMeasures(fr, "chisq")), "on df =", lavaan::fitMeasures(fo, "df"), "\n")
write.csv(pm, op("R9_path_models_original_vs_reverse.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# R10 — EARLY-CAREER TEACHERS (0-5 years) VS THE REST
# ---------------------------------------------------------------------
step("R10", "Early-career (0-5 years) vs the rest")
ec <- d$years_experience == "0-5"
r10 <- do.call(rbind, lapply(c("ESD", "EQD", "BAR", "CRIT", "IDEAL"), function(v) {
  a <- d[[v]][ec]; b <- d[[v]][!ec]
  s <- sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) / (length(a) + length(b) - 2))
  tt <- t.test(a, b)
  data.frame(construct = v, n_early = length(a), n_rest = length(b), M_early = mean(a), M_rest = mean(b),
             d = (mean(a) - mean(b)) / s, welch_t = unname(tt$statistic), p = tt$p.value)
}))
r10[, -1] <- signif(r10[, -1], 3); print(r10)
write.csv(r10, op("R10_early_career.csv"), row.names = FALSE)

cat("\n\nDone. Session info:\n"); print(sessionInfo())
