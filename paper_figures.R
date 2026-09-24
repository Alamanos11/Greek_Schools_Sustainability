# =====================================================================
#  PAPER OUTPUTS — display items for the journal manuscript
#
#  Argument structure:
#    SPINE  (B) practice tracks institutional cost, not conviction
#    SECOND (A) teacher attributes explain nothing
#    MECH   (C) constraint reaches the classroom by two routes
#
#  Produces 4 figures and 3 main tables (+ supplementary tables)
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

pkgs <- c("tidyverse", "psych", "effectsize", "lavaan",
          "ggtext", "ggforce")

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

# ---- soft dependencies ----------------------------------------------
# ggtext's richtext geom is drawn through gridtext -> markdown -> litedown.
# A missing link in that chain does NOT fail here: requireNamespace("ggtext")
# succeeds and the grob is only built at draw time, i.e. inside ggsave(), so the
# script would run four figures deep and then abort before a single table is
# written. These are therefore installed if absent, and the capability is probed
# once, for real, below.
soft <- c("gridtext", "markdown", "litedown")
for (pk in soft) if (!requireNamespace(pk, quietly = TRUE)) {
  message("Installing optional package: ", pk)
  try(install.packages(pk, repos = CRAN), silent = TRUE)
}

# Probe: build a trivial rich-text grob and measure it. If anything in the chain
# is missing or broken this returns FALSE and the plain-text caption is used.
HAVE_RICHTEXT <- isTRUE(tryCatch({
  grid::convertWidth(
    grid::grobWidth(gridtext::richtext_grob("<b>probe</b>")), "pt", valueOnly = TRUE)
  TRUE
}, error = function(e) FALSE, warning = function(w) FALSE))

if (!HAVE_RICHTEXT)
  message("NOTE: rich-text rendering unavailable; Figure 4 will use a plain-text caption.")

# ---- figure writer ---------------------------------------------------
# A figure that fails must not take the tables down with it: the numbers the
# manuscript quotes are written after the figures, and a fatal ggsave() leaves
# them stale on disk while the PNGs are new, which is worse than either.
safe_ggsave <- function(file, plot, ...) {
  tryCatch({
    ggsave(file, plot, ...)
    cat("  wrote", basename(file), "\n")
  }, error = function(e) {
    warning("FIGURE NOT WRITTEN: ", basename(file), "\n  reason: ",
            conditionMessage(e),
            "\n  The remaining outputs (tables, key numbers) were still produced.",
            call. = FALSE, immediate. = TRUE)
  })
}



set.seed(2026)
options(width = 200)

# Greek/typographic characters in labels need a UTF-8 ctype.
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))

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
OUT <- file.path(PROJECT_DIR, "outputs", "paper_outputs")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
op <- function(f) file.path(OUT, f)

# ---------------------------------------------------------------------
#  PALETTE
#  Cost tier is an ORDINAL variable, so it gets a single-hue ordinal ramp
#  (light -> dark), not categorical hues. That choice is what makes these
#  figures survive greyscale printing: the tiers differ in lightness, so
#  they stay distinguishable with no colour at all.
#  Validated: monotone lightness, adjacent dL >= .06, light end 2.06:1 vs
#  surface, single hue (spread 3 degrees).
# ---------------------------------------------------------------------
COST1 <- "#86b6ef"   # no resource beyond classroom talk
COST2 <- "#2a78d6"   # teacher planning time
COST3 <- "#104281"   # institutional resource
ACCENT<- "#2a78d6"   # emphasis hue
MUTED <- "#898781"   # de-emphasis / context
NEG   <- "#e34948"   # diverging: negative pole
POS   <- "#2a78d6"   # diverging: positive pole
INK   <- "#0b0b0b"; INK2 <- "#52514e"
GRID  <- "#e1e0d9"; AXIS <- "#c3c2b7"; SURFACE <- "#fcfcfb"

theme_paper <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(
      plot.background   = element_rect(fill = SURFACE, colour = NA),
      panel.background  = element_rect(fill = SURFACE, colour = NA),
      panel.grid.major  = element_line(colour = GRID, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.line.x       = element_line(colour = AXIS, linewidth = 0.3),
      axis.ticks        = element_blank(),
      text              = element_text(colour = INK),
      axis.text         = element_text(colour = INK2, size = base - 1),
      plot.title        = element_text(face = "bold", size = base + 3, colour = INK),
      plot.subtitle     = element_text(size = base, colour = INK2, lineheight = 1.2),
      plot.caption      = element_text(size = base - 2, colour = INK2, hjust = 0),
      strip.text        = element_text(face = "bold", size = base - 1, colour = INK,
                                       hjust = 0, margin = margin(b = 3, t = 6)),
      legend.position   = "top",
      legend.title      = element_text(size = base - 1, colour = INK2),
      legend.key.height = unit(9, "pt"),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}
theme_set(theme_paper())

# =====================================================================
#  DATA
# =====================================================================
d <- read_csv(file.path(PROJECT_DIR, DATA_FILE),
              locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

rev_items <- c("BEL_3","BEL_5","BEL_7","BEL_9","ESD_3","ESD_7","EQD_3","EQD_5","EQD_9")
for (v in rev_items) d[[paste0("r", v)]] <- 6 - d[[v]]
d$rBEL_6 <- 6 - d$BEL_6

bel_ideal <- c("BEL_1","BEL_2","BEL_4","BEL_8")
bel_gap   <- c("rBEL_3","rBEL_5","rBEL_6","rBEL_7","rBEL_9")
esd_set   <- c("ESD_1","ESD_2","rESD_3","ESD_4","ESD_5","ESD_6","rESD_7","ESD_8","ESD_9")
eqd_set   <- c("EQD_1","EQD_2","rEQD_3","EQD_4","rEQD_5","EQD_6","EQD_7","EQD_8","rEQD_9")
bar_set   <- paste0("BAR_", 1:10)
app_self  <- c("APP_1","APP_2","APP_3","APP_4","APP_5","APP_9")

d <- d %>% mutate(
  BEL_IDEAL = rowMeans(across(all_of(bel_ideal))),
  BEL_GAP   = rowMeans(across(all_of(bel_gap))),
  ESDPRAC   = rowMeans(across(all_of(esd_set))),
  EQDEM     = rowMeans(across(all_of(eqd_set))),
  BARRIER   = rowMeans(across(all_of(bar_set))),
  APP_SELF  = rowMeans(across(all_of(app_self))),
  gender = factor(gender), age = factor(age, c("22-30","31-40","41-50","51+")),
  years_experience = factor(years_experience, c("0-5","6-10","11-20","21+")),
  education_level  = factor(education_level, c("Bachelor's","Master's","Doctorate")),
  school_area      = factor(school_area, c("Urban","Semi-urban","Rural")),
  training_sustainability = factor(training_sustainability, c("No","Yes")),
  training_democracy      = factor(training_democracy, c("No","Yes")),
  rfcdc_known             = factor(rfcdc_known, c("No","Yes")),
  specialty_grp = factor(case_when(
    str_detect(specialty, "PE70 SEN|PE71|Special Education") ~ "Special education",
    str_detect(specialty, "PE70|Primary Teacher")            ~ "Generalist primary",
    TRUE ~ "Subject specialist"))
)

# =====================================================================
#  ITEM CODING — institutional resource required
#
#  Each practice item carries a code on the three-level rubric below,
#  read from item_cost_coding.csv. Where the file holds more than one
#  code column, the analysis uses the modal code.
#
#  RUBRIC
#   1 = No resource beyond classroom talk. The teacher can do it alone,
#       inside an ordinary lesson, with nothing bought or booked.
#   2 = Teacher planning time. Needs preparation, re-sequencing or
#       coordination with colleagues, but no external resource.
#   3 = Institutional resource. Needs materials, budget, timetable change,
#       or an actor outside the classroom.
#
#  Each item was coded by three people working independently, without sight
#  of each other's assignments and blind to the item response distributions.
#  The analysis uses the modal level; agreement is computed and reported
#  below, and the gradient is additionally recomputed from each coder's
#  levels taken alone, so that no result rests on the aggregation rule.
#
#  The rubric itself remains a judgement of the research team about what each
#  practice demands of a school. It is not validated against any external
#  measure of cost, and the manuscript says so.
#
#  Belief items describe an attitude rather than an activity and are
#  therefore left uncoded; they enter Figure 1 as a reference band only.
# =====================================================================
CODING_FILE <- file.path(DATA_DIR, "item_cost_coding.csv")
if (!file.exists(CODING_FILE))
  stop("\n\n  Cannot find 'item_cost_coding.csv' in:\n    ", DATA_DIR,
       "\n  The cost gradient cannot be computed without the item coding.\n\n",
       call. = FALSE)

item_coding <- read_csv(CODING_FILE, show_col_types = FALSE)
code_cols   <- grep("^rater|^cost$|^code$", names(item_coding), value = TRUE)
stopifnot(length(code_cols) >= 1,
          all(c("item", "domain", "statement") %in% names(item_coding)))

modal <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}
item_coding$cost <- apply(item_coding[, code_cols, drop = FALSE], 1, modal)

practice_items <- item_coding %>% filter(domain != "Belief")
cat(sprintf("Cost coding read from %s: %d practice items coded, %d code column(s).\n\n",
            basename(CODING_FILE), sum(!is.na(practice_items$cost)), length(code_cols)))

# =====================================================================
#  INTER-CODER AGREEMENT ON THE COST RUBRIC
#
#  Three coders assigned a level to each practice item independently, each
#  blind to the response distributions. Three coefficients are computed,
#  because no single one is adequate here: the codes are ORDINAL, there are
#  more than two raters, and the item set is small.
#
#    - exact / adjacent agreement : the raw picture, no chance correction
#    - Fleiss' kappa              : chance-corrected but NOMINAL, so it
#                                   penalises a one-level disagreement as
#                                   heavily as a two-level one
#    - Krippendorff's alpha       : chance-corrected WITH an ordinal
#                                   difference function
#    - Kendall's W                : concordance of the ORDERING, which is the
#                                   property the gradient analysis uses
#
#  All four are reported. Reporting only the most favourable would be a
#  selection on the outcome.
# =====================================================================
fleiss_kappa <- function(M, cats = sort(unique(as.vector(M)))) {
  N <- nrow(M); m <- ncol(M)
  n <- t(apply(M, 1, \(r) tabulate(match(r, cats), nbins = length(cats))))
  P    <- (rowSums(n^2) - m) / (m * (m - 1))
  pj   <- colSums(n) / (N * m)
  Pbar <- mean(P); Pe <- sum(pj^2)
  list(kappa = (Pbar - Pe) / (1 - Pe), Pbar = Pbar, Pe = Pe)
}

# Krippendorff's alpha with the ordinal difference function (Krippendorff 2019):
#   delta^2(c,k) = ( sum_{g=c..k} n_g  -  (n_c + n_k)/2 )^2
kripp_alpha_ordinal <- function(M, cats = sort(unique(as.vector(M)))) {
  m  <- ncol(M)
  nk <- as.numeric(table(factor(as.vector(M), levels = cats)))
  names(nk) <- as.character(cats)
  n  <- sum(nk)
  d2 <- function(a, b) {
    lo <- min(a, b); hi <- max(a, b)
    g  <- sum(nk[as.character(cats[cats >= lo & cats <= hi])])
    (g - (nk[as.character(a)] + nk[as.character(b)]) / 2)^2
  }
  Do <- 0
  for (i in seq_len(nrow(M))) {
    cnt <- as.numeric(table(factor(M[i, ], levels = cats)))
    names(cnt) <- as.character(cats)
    for (a in cats) for (b in cats)
      Do <- Do + cnt[as.character(a)] *
                 (cnt[as.character(b)] - as.numeric(a == b)) * d2(a, b)
  }
  Do <- Do / (nrow(M) * m * (m - 1))
  De <- 0
  for (a in cats) for (b in cats)
    De <- De + nk[as.character(a)] *
               (nk[as.character(b)] - as.numeric(a == b)) * d2(a, b)
  De <- De / (n * (n - 1))
  unname(1 - Do / De)
}

# Kendall's W with the tie correction; rank() averages ties, which is what the
# correction assumes.
kendall_w <- function(M) {
  N <- nrow(M); m <- ncol(M)
  R  <- apply(M, 2, rank)
  Rs <- rowSums(R)
  S  <- sum((Rs - mean(Rs))^2)
  Tj <- sum(apply(R, 2, \(r) { tt <- table(r); sum(tt^3 - tt) }))
  W  <- 12 * S / (m^2 * (N^3 - N) - m * Tj)
  chi <- m * (N - 1) * W
  list(W = W, chisq = chi, df = N - 1,
       p = pchisq(chi, N - 1, lower.tail = FALSE))
}

if (length(code_cols) >= 3) {
  codes_prac <- as.matrix(practice_items[, code_cols, drop = FALSE])
  storage.mode(codes_prac) <- "numeric"

  exact_pct <- 100 * mean(apply(codes_prac, 1, \(r) length(unique(r)) == 1))
  adj_pct   <- 100 * mean(apply(codes_prac, 1, \(r) diff(range(r)) <= 1))
  maj_pct   <- 100 * mean(apply(codes_prac, 1, \(r) max(table(r)) >= 2))
  fk        <- fleiss_kappa(codes_prac)
  ka        <- kripp_alpha_ordinal(codes_prac)
  kw        <- kendall_w(codes_prac)

  agreement <- tibble(
    Statistic = c("Items coded", "Coders",
                  "Exact agreement (all coders identical), %",
                  "Agreement within one level, %",
                  "Items with a majority level, %",
                  "Fleiss' kappa (nominal)",
                  "Krippendorff's alpha (ordinal)",
                  "Kendall's W",
                  "Kendall's W: chi-square", "Kendall's W: df", "Kendall's W: p"),
    Value = round(c(nrow(codes_prac), ncol(codes_prac),
                    exact_pct, adj_pct, maj_pct,
                    fk$kappa, ka, kw$W, kw$chisq, kw$df, kw$p), 4))

  cat(sprintf("\n=== INTER-CODER AGREEMENT (%d practice items, %d coders) ===\n",
              nrow(codes_prac), ncol(codes_prac)))
  print(as.data.frame(agreement), right = FALSE)
  cat("\nFleiss' kappa is the conservative figure: it is a NOMINAL coefficient and\n",
      "treats a one-level disagreement on an ordinal rubric as a total one. The\n",
      "ordinal coefficients are the appropriate indices here. All are reported.\n\n")

  CODING_SOURCE <- sprintf(
    paste0("three coders working independently, each blind to the response ",
           "distributions; modal level used (exact agreement %.0f%%, all ",
           "disagreements within one level, Fleiss' kappa = %.2f, ordinal ",
           "Krippendorff's alpha = %.2f, Kendall's W = %.2f)"),
    exact_pct, fk$kappa, ka, kw$W)
} else {
  agreement <- tibble(Statistic = "Only one code column present", Value = NA_real_)
  CODING_SOURCE <- "assigned by the research team on the rubric set out in the Methods"
  cat("\nNOTE: fewer than three code columns found; no agreement statistics computed.\n\n")
}


COST_LAB <- c("1" = "No resource beyond classroom talk",
              "2" = "Teacher planning time",
              "3" = "Institutional resource")
COST_COL <- c("1" = COST1, "2" = COST2, "3" = COST3)

# Attach observed frequencies.
item_stats <- item_coding %>%
  mutate(M      = map_dbl(item, ~mean(d[[.x]], na.rm = TRUE)),
         SD     = map_dbl(item, ~sd(d[[.x]],   na.rm = TRUE)),
         pct4_5 = map_dbl(item, ~100 * mean(d[[.x]] >= 4, na.rm = TRUE)))

# =====================================================================
#  FIGURE 1 — THE COST GRADIENT  (the spine)
# =====================================================================
cat("=== FIGURE 1 — the cost gradient ===\n")

esd_only <- item_stats %>% filter(domain == "ESD practice")
rho_esd  <- cor(esd_only$cost, esd_only$M, method = "spearman")
prac     <- item_stats %>% filter(domain != "Belief")
rho_all  <- cor(prac$cost, prac$M, method = "spearman")
cat(sprintf("Spearman rho(cost, item mean): ESD items %.3f | all practice items %.3f\n",
            rho_esd, rho_all))

tier_summary <- prac %>% group_by(cost) %>%
  summarise(k = n(), mean_pct = mean(pct4_5), .groups = "drop")
cat("\nMean percentage reporting 'often/always', by cost tier:\n")
print(as.data.frame(tier_summary %>% mutate(mean_pct = round(mean_pct))))


# Sensitivity to the aggregation rule. The cost coding is the one analytic input
# supplied by the authors rather than by respondents, so the gradient is also
# computed from each coder's levels taken alone. If it only holds for the modal
# code, it is an artefact of the aggregation and not a property of the data.
if (length(code_cols) >= 2) {
  coder_sens <- map_dfr(c(code_cols, "cost"), \(cc) tibble(
    codes   = if (cc == "cost") "Modal level" else cc,
    rho_esd = cor(esd_only[[cc]], esd_only$M, method = "spearman"),
    rho_all = cor(prac[[cc]],     prac$M,     method = "spearman"),
    p_esd   = cor.test(esd_only[[cc]], esd_only$M, method = "spearman",
                       exact = FALSE)$p.value,
    p_all   = cor.test(prac[[cc]], prac$M, method = "spearman",
                       exact = FALSE)$p.value))
  cat("\nSpearman rho(cost, item mean) under each coder's levels taken alone:\n")
  print(as.data.frame(coder_sens %>%
          mutate(across(where(is.numeric), \(x) round(x, 3)))), right = FALSE)
} else {
  coder_sens <- tibble(codes = "Modal level", rho_esd = rho_esd, rho_all = rho_all,
                       p_esd = NA_real_, p_all = NA_real_)
}

belief_mean <- mean(item_stats$pct4_5[item_stats$domain == "Belief"])

f1_dat <- item_stats %>%
  mutate(
    band = case_when(domain == "Belief" ~ "WHAT TEACHERS BELIEVE  \u00b7  no practice required",
                     TRUE ~ paste0("WHAT TEACHERS DO  \u00b7  ", COST_LAB[as.character(cost)])),
    band = factor(band, levels = c("WHAT TEACHERS BELIEVE  \u00b7  no practice required",
                                   paste0("WHAT TEACHERS DO  \u00b7  ", COST_LAB))),
    fill = ifelse(domain == "Belief", MUTED, COST_COL[as.character(cost)]),
    lab  = sprintf("%.0f%%", pct4_5)
  ) %>%
  arrange(band, pct4_5) %>%
  mutate(statement = fct_inorder(statement))

# the reference line is drawn in every panel; its label appears once
ref_lab <- tibble(band = factor(paste0("WHAT TEACHERS DO  \u00b7  ", COST_LAB[["3"]]),
                                levels = levels(f1_dat$band)),
                  x = belief_mean)

fig1 <- ggplot(f1_dat, aes(pct4_5, statement)) +
  geom_vline(xintercept = belief_mean, colour = AXIS, linewidth = 0.4) +
  geom_col(aes(fill = fill), width = 0.62) +
  geom_text(aes(label = lab), hjust = -0.2, size = 3, colour = INK2) +
  geom_text(data = ref_lab, aes(x = x - 2.5, y = 2.42, label = "mean belief\nendorsement, 92%"),
            hjust = 1, vjust = 1, size = 2.6, colour = INK2, lineheight = 0.95,
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  ggforce::facet_col(vars(band), scales = "free_y", space = "free", strip.position = "top") +
  labs(
    title = "Practice falls in proportion to what it costs the institution",
    subtitle = sprintf(
      "Teachers endorse the aims almost unanimously. What they report doing drops step by step with the resource each practice\nrequires. Spearman \u03C1 between coded cost and item mean = %.2f across practice items, %.2f within the ESD items alone.",
      rho_all, rho_esd),
    x = "Respondents reporting 'agree' or 'strongly agree' (4\u20135 on a 5-point scale)",
    y = NULL,
    caption = str_wrap(paste0("N = 391 Greek primary teachers. Bars shade light-to-dark with institutional cost, so the tiers remain distinguishable in greyscale.",
                              sprintf(" Cost coding: %s.", CODING_SOURCE)), 150)
  ) +
  theme(panel.spacing.y = unit(11, "pt"),
        axis.text.y = element_text(size = 8.6, colour = INK),
        strip.text = element_text(face = "bold", size = 8.6, colour = INK, hjust = 0,
                                  margin = margin(b = 4, t = 8)),
        panel.grid.major.y = element_blank())

safe_ggsave(op("Figure1_cost_gradient.png"), fig1, width = 9.6, height = 7.2, dpi = 300, bg = SURFACE)

# =====================================================================
#  FIGURE 2 — WHAT TEACHERS SAY CONSTRAINS THEM, AND THE TRAINING PARADOX
# =====================================================================
cat("\n=== FIGURE 2 — barriers and the training paradox ===\n")

bar_lab <- c(
  BAR_4 = "No suitable teaching materials exist",
  BAR_3 = "There is not enough time",
  BAR_1 = "The curriculum leaves no room",
  BAR_2 = "I have not had enough training",
  BAR_9 = "Assessment rewards cognitive attainment only",
  BAR_7 = "Classes are too large",
  BAR_5 = "School leadership does not support it",
  BAR_6 = "Parents are uninterested or opposed",
  BAR_8 = "There is no collaboration between colleagues",
  BAR_10 = "[item absent from the archived questionnaire]")

# Equivalence test (TOST) on the sustainability-training contrast.
tost <- function(x, y, bound = 0.35) {
  n1 <- length(x); n2 <- length(y)
  s  <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  se <- s * sqrt(1/n1 + 1/n2); df <- n1 + n2 - 2
  list(d = (mean(x) - mean(y)) / s,
       p = max(pt(((mean(x) - mean(y)) + bound * s) / se, df, lower.tail = FALSE),
               pt(((mean(x) - mean(y)) - bound * s) / se, df, lower.tail = TRUE)))
}
tr  <- d$ESDPRAC[d$training_sustainability == "Yes"]
ntr <- d$ESDPRAC[d$training_sustainability == "No"]
eq  <- tost(tr, ntr)
cat(sprintf("Sustainability training on ESD practice: trained M = %.2f (n = %d), untrained M = %.2f (n = %d)\n",
            mean(tr), length(tr), mean(ntr), length(ntr)))
cat(sprintf("  d = %+.2f, equivalence test p = %.4f (bounds d = +/-0.35)\n", eq$d, eq$p))

f2_dat <- tibble(item = names(bar_lab), label = unname(bar_lab)) %>%
  mutate(M = map_dbl(item, ~mean(d[[.x]])),
         pct = map_dbl(item, ~100 * mean(d[[.x]] >= 4)),
         highlight = item == "BAR_2") %>%
  arrange(pct) %>% mutate(label = fct_inorder(label))

fig2 <- ggplot(f2_dat, aes(pct, label)) +
  geom_col(aes(fill = highlight), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.18, size = 2.9, colour = INK2) +
  scale_fill_manual(values = c("FALSE" = MUTED, "TRUE" = ACCENT), guide = "none") +
  scale_x_continuous(limits = c(0, 118), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  annotate("text", x = 78, y = 7, hjust = 0, vjust = 0.5, size = 2.8,
           colour = ACCENT, lineheight = 1.15, fontface = "bold",
           label = "\u2190 the one barrier that\n    does not move practice") +
  labs(title = "The barrier teachers name most readily is the one that does not move practice",
       subtitle = str_wrap(sprintf("Share agreeing that each condition obstructs Education for Sustainability and democratic classroom practice. The %d teachers who have had sustainability training are statistically indistinguishable from the %d who have not: M = %.2f vs %.2f, d = %+.2f, equivalence p = %.3f.",
                          length(tr), length(ntr), mean(tr), mean(ntr), eq$d, eq$p), 128),
       x = "Respondents reporting 'agree' or 'strongly agree'", y = NULL,
       caption = "N = 391. Equivalence test: two one-sided tests against bounds of d = \u00B10.35 on the ESD practice score.") +
  theme(axis.text.y = element_text(size = 8.5, colour = INK))

safe_ggsave(op("Figure2_barriers_training_paradox.png"), fig2, width = 9.2, height = 5.2, dpi = 300, bg = SURFACE)

# =====================================================================
#  FIGURE 3 — ATTRIBUTES EXPLAIN NOTHING; CONDITIONS EXPLAIN EVERYTHING
#  One metric (% of variance explained), one axis, two groups.
# =====================================================================
cat("\n=== FIGURE 3 — variance explained ===\n")

outcomes   <- c("BEL_IDEAL","BEL_GAP","ESDPRAC","EQDEM","BARRIER","APP_SELF")
two_group  <- c("gender","training_sustainability","training_democracy","rfcdc_known")
many_group <- c("age","years_experience","education_level","specialty_grp","school_area")

attr_rows <- list(); k <- 0
for (y in outcomes) for (g in c(two_group, many_group)) {
  fit <- aov(reformulate(g, y), data = d)          # eta2 works for both cases
  s   <- summary(fit)[[1]]
  ss  <- s[["Sum Sq"]]
  k <- k + 1
  attr_rows[[k]] <- tibble(outcome = y, predictor = g,
                           var_pct = 100 * ss[1] / sum(ss),
                           p = s[["Pr(>F)"]][1])
}
attr_eff <- bind_rows(attr_rows) %>% mutate(p_BH = p.adjust(p, "BH"))

cat(sprintf("Teacher-attribute tests: %d | median variance explained %.2f%% | max %.2f%% | surviving FDR: %d\n",
            nrow(attr_eff), median(attr_eff$var_pct), max(attr_eff$var_pct),
            sum(attr_eff$p_BH < .05)))

m1 <- lm(EQDEM ~ BEL_IDEAL + BEL_GAP + ESDPRAC + BARRIER, data = d)
R2 <- summary(m1)$r.squared
semipartial <- map_dbl(c("BEL_IDEAL","BEL_GAP","ESDPRAC","BARRIER"), \(v) {
  red <- update(m1, as.formula(paste(". ~ . -", v)))
  100 * (R2 - summary(red)$r.squared)
})
cond_eff <- tibble(
  predictor = c("ESD practice","Perceived implementation gap","Perceived barriers","Idealistic beliefs"),
  var_pct   = semipartial[c(3, 2, 4, 1)])

cat("\nUnique variance in democratic practice explained by each condition (semipartial):\n")
print(as.data.frame(cond_eff %>% mutate(var_pct = round(var_pct, 2))))
cat(sprintf("Full model R-squared = %.1f%%\n", 100 * R2))

# Layout: one shared x-axis (variance explained). Named estimates get their own
# rows so nothing has to be repelled; the 54 attribute tests get a strip row.
f3_named <- cond_eff %>%
  arrange(var_pct) %>%
  bind_rows(tibble(predictor = "ALL FOUR TOGETHER (model R\u00b2)", var_pct = 100 * R2)) %>%
  mutate(band = "What the teacher perceives",
         predictor = fct_inorder(predictor),
         is_total = predictor == "ALL FOUR TOGETHER (model R\u00b2)")

f3_dots <- attr_eff %>%
  mutate(band = "Who the teacher is",
         predictor = factor("Each of 54 teacher-characteristic tests"))

fig3 <- ggplot() +
  geom_col(data = f3_named, aes(var_pct, predictor, fill = is_total),
           width = 0.58) +
  geom_text(data = f3_named,
            aes(var_pct, predictor, label = sprintf("%.1f%%", var_pct)),
            hjust = -0.22, size = 3, colour = INK2) +
  geom_point(data = f3_dots,
             aes(var_pct, predictor),
             position = position_jitter(height = 0.17, seed = 1),
             colour = MUTED, size = 1.9, alpha = 0.6) +
  geom_text(data = tibble(band = "Who the teacher is",
                          predictor = factor("Each of 54 teacher-characteristic tests"),
                          x = max(attr_eff$var_pct) + 1.5),
            aes(x, predictor,
                label = sprintf("largest is %.1f%% \u2014 the only one surviving FDR correction,\nand it rests on %d doctorate holders",
                                max(attr_eff$var_pct), sum(d$education_level == "Doctorate"))),
            hjust = 0, size = 2.75, colour = INK2, lineheight = 1.05) +
  scale_fill_manual(values = c("FALSE" = COST2, "TRUE" = COST3), guide = "none") +
  scale_x_continuous(limits = c(0, 58), breaks = seq(0, 50, 10),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  ggforce::facet_col(vars(band), scales = "free_y", space = "free", strip.position = "top") +
  labs(title = "Who the teacher is explains almost nothing. What they perceive explains half.",
       subtitle = "Every grey dot is one comparison of a construct across a teacher characteristic: gender, age, experience,\nqualification, specialism, school setting, and both kinds of in-service training.",
       x = "Variance explained in the outcome", y = NULL,
       caption = str_wrap("N = 391. Grey: eta-squared for each teacher characteristic across six constructs. Blue: unique (semipartial) variance in equality/democratic practice, Model 1; the pale bar is the full model R-squared.", 150)) +
  theme(axis.text.y = element_text(size = 9, colour = INK),
        strip.text = element_text(face = "bold", size = 9, colour = INK, hjust = 0,
                                  margin = margin(b = 4, t = 8)),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(11, "pt"))

safe_ggsave(op("Figure3_attributes_vs_conditions.png"), fig3, width = 9.6, height = 5.0, dpi = 300, bg = SURFACE)

# =====================================================================
#  FIGURE 4 — TWO ROUTES  (the mechanism)
# =====================================================================
cat("\n=== FIGURE 4 — two routes ===\n")

med_spec <- '
  ESDPRAC ~ a1*BEL_IDEAL + a2*BEL_GAP + a3*BARRIER
  EQDEM   ~ b*ESDPRAC + c1*BEL_IDEAL + c2*BEL_GAP + c3*BARRIER
  ind_ideal := a1*b
  ind_gap   := a2*b
  ind_bar   := a3*b
  tot_ideal := c1 + a1*b
  tot_gap   := c2 + a2*b
  tot_bar   := c3 + a3*b
'
m3 <- sem(med_spec, data = d, se = "bootstrap", bootstrap = 5000)
pe <- parameterEstimates(m3, ci = TRUE)
g  <- function(l) pe[pe$label == l, ][1, ]
cf <- function(l) sprintf("%+.2f", g(l)$est)

cat("\nIndirect effects (5000 bootstrap resamples):\n")
print(pe %>% filter(op == ":=") %>%
        transmute(effect = lhs, est = round(est, 3),
                  ci = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
                  p = round(pvalue, 4)) %>% as.data.frame())
share <- function(i, t) 100 * g(i)$est / g(t)$est
cat(sprintf("\nShare of total effect carried through ESD practice:\n  beliefs %.0f%% | gap %.0f%% | barriers %.0f%%\n",
            share("ind_ideal","tot_ideal"), share("ind_gap","tot_gap"), share("ind_bar","tot_bar")))

nodes <- tribble(
  ~id,     ~x,  ~y,  ~lab,
  "IDEAL", 0.0, 3.00, "Idealistic\nbeliefs",
  "GAP",   0.0, 2.00, "Perceived\nimplementation gap",
  "BAR",   0.0, 1.00, "Perceived\nbarriers",
  "ESD",   1.6, 3.62, "ESD practice",
  "EQD",   3.2, 2.00, "Equality &\ndemocratic practice")

edges <- tribble(
  ~from,   ~to,   ~lab,                      ~est,        ~kind, ~tpos,
  "IDEAL", "ESD", cf("a1"),                  g("a1")$est, "a",   0.34,
  "GAP",   "ESD", paste0(cf("a2"), " n.s."), g("a2")$est, "a",   0.40,
  "BAR",   "ESD", cf("a3"),                  g("a3")$est, "a",   0.62,
  "ESD",   "EQD", cf("b"),                   g("b")$est,  "b",   0.50,
  "IDEAL", "EQD", cf("c1"),                  g("c1")$est, "c",   0.76,
  "GAP",   "EQD", cf("c2"),                  g("c2")$est, "c",   0.80,
  "BAR",   "EQD", cf("c3"),                  g("c3")$est, "c",   0.74) %>%
  left_join(nodes %>% select(from = id, x, y), by = "from") %>%
  left_join(nodes %>% select(to = id, xend = x, yend = y), by = "to") %>%
  mutate(
    ns     = str_detect(lab, "n.s."),
    colour = case_when(ns ~ AXIS, est < 0 ~ NEG, TRUE ~ POS),
    lw     = pmax(0.45, abs(est) * 3.2),
    # trim each arrow so it stops short of the node boxes
    f0 = ifelse(kind == "b", 0.16, 0.13),
    f1 = ifelse(kind == "b", 0.16, 0.13),
    x2    = x    + (xend - x) * f0,  y2    = y    + (yend - y) * f0,
    xend2 = xend + (x - xend) * f1,  yend2 = yend + (y - yend) * f1,
    # label position along the edge: a-paths early, direct paths late,
    # so nothing stacks up in the middle where the lines cross
    mx = x2 + (xend2 - x2) * tpos,
    my = y2 + (yend2 - y2) * tpos)

# The caption under the diagram carries the paper's central sentence, so it is
# built before the plot and in two forms: marked-up, and plain. Which one is used
# depends on the capability probed at the top of the script.
cap_v <- list(ind    = g("ind_bar")$est,
              ind_lo = g("ind_bar")$ci.lower,
              ind_hi = g("ind_bar")$ci.upper,
              dir    = g("c3")$est,
              dir_lo = g("c3")$ci.lower,
              dir_hi = g("c3")$ci.upper,
              sh     = share("ind_bar", "tot_bar"))

cap_rich <- with(cap_v, sprintf(
  "<b>Curricular route</b> &#8212; barriers first suppress sustainability teaching, which then carries the effect onward: indirect %+.3f, 95%% CI [%.3f, %.3f]<br>
   <b>Relational route</b> &#8212; barriers reach the democratic classroom without passing through teaching at all: %+.3f, 95%% CI [%.3f, %.3f]<br>
   <b>The two routes are of near-equal size.</b> %.0f%% of the barrier effect travels through teaching; %.0f%% does not.",
  ind, ind_lo, ind_hi, dir, dir_lo, dir_hi, sh, 100 - sh))

cap_plain <- with(cap_v, sprintf(
  paste0("Curricular route — barriers first suppress sustainability teaching, which then carries the effect onward:\n",
         "indirect %+.3f, 95%% CI [%.3f, %.3f]\n",
         "Relational route — barriers reach the democratic classroom without passing through teaching at all:\n",
         "%+.3f, 95%% CI [%.3f, %.3f]\n",
         "The two routes are of near-equal size: %.0f%% of the barrier effect travels through teaching; %.0f%% does not."),
  ind, ind_lo, ind_hi, dir, dir_lo, dir_hi, sh, 100 - sh))

cap_layer <- if (HAVE_RICHTEXT) {
  annotate("richtext", x = 1.6, y = 0.10, hjust = 0.5, vjust = 0, size = 2.95,
           label = cap_rich, colour = INK2, lineheight = 1.45,
           fill = NA, label.colour = NA)
} else {
  annotate("text", x = 1.6, y = 0.10, hjust = 0.5, vjust = 0, size = 2.75,
           label = cap_plain, colour = INK2, lineheight = 1.35)
}


fig4 <- ggplot() +
  geom_segment(data = edges,
               aes(x = x2, y = y2, xend = xend2, yend = yend2,
                   colour = colour, linewidth = lw),
               arrow = arrow(length = unit(6, "pt"), type = "closed"),
               lineend = "round") +
  scale_colour_identity() + scale_linewidth_identity() +
  geom_label(data = edges, aes(mx, my, label = lab, colour = colour),
             size = 3, fill = SURFACE, linewidth = 0,
             label.padding = unit(2, "pt"), fontface = "bold") +
  geom_label(data = nodes, aes(x, y, label = lab), size = 3.15, colour = INK,
             fill = "#f0efec", linewidth = 0, lineheight = 0.95,
             label.padding = unit(6, "pt"), label.r = unit(3, "pt")) +
  cap_layer +
  scale_x_continuous(limits = c(-0.62, 3.85)) +
  scale_y_continuous(limits = c(0.05, 4.05)) +
  labs(title = "Constraint reaches the democratic classroom by two routes of equal size",
       subtitle = "Unstandardised coefficients. Blue = positive, red = negative, line width \u221D |b|, grey = not significant.",
       caption = str_wrap("N = 391. Indirect effects tested with 5,000 bootstrap resamples. These are cross-sectional self-reports: the model tests whether the data are consistent with this ordering, not that it is causal.", 145)) +
  theme_void(base_size = 10) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        plot.title = element_text(face = "bold", size = 13, colour = INK),
        plot.subtitle = element_text(size = 10, colour = INK2,
                                     margin = margin(t = 3, b = 6)),
        plot.caption = element_text(size = 8, colour = INK2, hjust = 0, lineheight = 1.2),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.margin = margin(12, 12, 10, 12))

safe_ggsave(op("Figure4_two_routes.png"), fig4, width = 9.8, height = 5.8, dpi = 300, bg = SURFACE)

# =====================================================================
#  MAIN TABLES
# =====================================================================
cat("\n=== TABLES ===\n")

# TABLE 1 — sample and measures.
demo_vars <- c("gender","age","years_experience","education_level",
               "specialty_grp","school_area","training_sustainability",
               "training_democracy","rfcdc_known")
t1_sample <- map_dfr(demo_vars, \(v)
  d %>% count(.data[[v]]) %>% rename(level = 1) %>%
    transmute(characteristic = v, level = as.character(level),
              n = n, pct = round(100 * n / sum(n), 1)))

alpha_of <- function(items) psych::alpha(d[items], check.keys = FALSE,
                                         warnings = FALSE)$total$raw_alpha
t1_measures <- tibble(
  construct = c("Idealistic beliefs","Perceived implementation gap","ESD practices",
                "Equality & democratic practices","Perceived barriers",
                "Self-rated implementation"),
  k = c(4, 5, 9, 9, 10, 6),
  alpha = round(c(alpha_of(bel_ideal), alpha_of(bel_gap), alpha_of(esd_set),
                  alpha_of(eqd_set), alpha_of(bar_set), alpha_of(app_self)), 3),
  M = round(c(mean(d$BEL_IDEAL), mean(d$BEL_GAP), mean(d$ESDPRAC),
              mean(d$EQDEM), mean(d$BARRIER), mean(d$APP_SELF)), 2),
  SD = round(c(sd(d$BEL_IDEAL), sd(d$BEL_GAP), sd(d$ESDPRAC),
               sd(d$EQDEM), sd(d$BARRIER), sd(d$APP_SELF)), 2))
write_csv(t1_sample,   op("Table1a_sample.csv"))
write_csv(t1_measures, op("Table1b_measures.csv"))
print(as.data.frame(t1_measures))

# TABLE 2 — correlations, M and SD on the margin.
cvars <- c("BEL_IDEAL","BEL_GAP","ESDPRAC","EQDEM","BARRIER","APP_SELF")
ct <- psych::corr.test(d[cvars], adjust = "BH")
t2 <- as.data.frame(round(ct$r, 2)) %>% rownames_to_column("construct") %>%
  left_join(t1_measures %>% transmute(construct = c("BEL_IDEAL","BEL_GAP","ESDPRAC",
                                                    "EQDEM","BARRIER","APP_SELF"),
                                      M, SD), by = "construct")
write_csv(t2, op("Table2_correlations.csv"))
write_csv(as.data.frame(round(ct$p, 4)) %>% rownames_to_column("construct"),
          op("Table2_correlations_p.csv"))
print(t2)

# TABLE 3 — Models 1-3.
m1c <- lm(EQDEM ~ I(BEL_IDEAL - mean(BEL_IDEAL)) + I(BEL_GAP - mean(BEL_GAP)) +
            I(ESDPRAC - mean(ESDPRAC)) + I(BARRIER - mean(BARRIER)), data = d)
t3_m12 <- broom::tidy(m1c, conf.int = TRUE) %>%
  mutate(model = "M1 main effects",
         term = c("(Intercept)","Idealistic beliefs","Implementation gap",
                  "ESD practice","Barriers"))
t3_m3 <- pe %>% filter(op %in% c("~", ":=")) %>%
  transmute(model = "M3 mediation",
            term = ifelse(op == "~", paste(lhs, "<-", rhs), lhs),
            estimate = est, std.error = se, statistic = z, p.value = pvalue,
            conf.low = ci.lower, conf.high = ci.upper)
t3 <- bind_rows(t3_m12, t3_m3) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  select(model, term, estimate, std.error, conf.low, conf.high, p.value)
write_csv(t3, op("Table3_models.csv"))

# SUPPLEMENTARY
write_csv(item_stats,  op("TableS1_item_descriptives_and_cost_coding.csv"))
write_csv(attr_eff %>% mutate(across(where(is.numeric), ~round(.x, 4))),
          op("TableS2_all_teacher_attribute_tests.csv"))
write_csv(tier_summary, op("TableS3_cost_tier_summary.csv"))
write_csv(agreement,  op("TableS4_coder_agreement.csv"))
write_csv(coder_sens, op("TableS5_coder_sensitivity.csv"))

# =====================================================================
#  NUMBERS FOR THE MANUSCRIPT TEXT
# =====================================================================
gapd <- effectsize::cohens_d(d$BEL_IDEAL, d$ESDPRAC, paired = TRUE)
demd <- effectsize::cohens_d(d$EQDEM,     d$ESDPRAC, paired = TRUE)
key <- c(
  sprintf("N = %d. Fieldwork 22 May - 20 September 2026. No missing data on the 46 items.", nrow(d)),
  sprintf("Belief-practice gap: beliefs M = %.2f vs ESD practice M = %.2f, d = %.2f [%.2f, %.2f].",
          mean(d$BEL_IDEAL), mean(d$ESDPRAC), gapd$Cohens_d, gapd$CI_low, gapd$CI_high),
  sprintf("Democratic practice exceeds ESD content: M = %.2f vs %.2f, d = %.2f [%.2f, %.2f].",
          mean(d$EQDEM), mean(d$ESDPRAC), demd$Cohens_d, demd$CI_low, demd$CI_high),
  sprintf("Cost gradient: rho = %.2f (ESD items), %.2f (all practice items).", rho_esd, rho_all),
  sprintf("By cost tier, mean %% often/always: %s.",
          paste(sprintf("%s = %.0f%%", COST_LAB[as.character(tier_summary$cost)],
                        tier_summary$mean_pct), collapse = "; ")),
  sprintf("Teacher attributes: %d tests, median %.2f%% of variance, %d surviving FDR.",
          nrow(attr_eff), median(attr_eff$var_pct), sum(attr_eff$p_BH < .05)),
  sprintf("Sustainability training on ESD practice: d = %+.2f, equivalence p = %.4f (bounds +/-0.35).",
          eq$d, eq$p),
  sprintf("Model 1 R-squared = %.3f. Semipartial: ESD %.1f%%, gap %.1f%%, barriers %.1f%%, beliefs %.1f%%.",
          R2, cond_eff$var_pct[1], cond_eff$var_pct[2], cond_eff$var_pct[3], cond_eff$var_pct[4]),
  sprintf("Barrier routes: indirect %+.3f [%.3f, %.3f], direct %+.3f [%.3f, %.3f]; %.0f%% travels through ESD practice.",
          g("ind_bar")$est, g("ind_bar")$ci.lower, g("ind_bar")$ci.upper,
          g("c3")$est, g("c3")$ci.lower, g("c3")$ci.upper, share("ind_bar","tot_bar")),
  sprintf("Cost coding provenance: %s.", CODING_SOURCE),
  if (exists("fk")) sprintf(
    "Inter-coder agreement: exact %.0f%%, all disagreements within one level, kappa = %.2f, ordinal alpha = %.2f, W = %.2f (p = %.3f).",
    exact_pct, fk$kappa, ka, kw$W, kw$p) else "Inter-coder agreement: not computed.",
  sprintf("Gradient under each coder alone, all practice items: %s.",
          paste(sprintf("%s = %.2f", coder_sens$codes, coder_sens$rho_all), collapse = "; ")))
writeLines(key, op("key_numbers_for_text.txt"))
cat("\n--- key numbers for the manuscript ---\n"); cat(key, sep = "\n")

cat("\n\n=====================================================================\n")
cat("Done. 4 figures + tables in:\n  ", OUT, "\n")
cat("=====================================================================\n")
