# =====================================================================
#  FIGURES 1-4, second revision. Titles, subtitles and notes are not drawn
#  in the images; all explanatory text is in the captions of the article.
#  Self-contained: rebuilds construct scores as in revision_analyses.R.
# =====================================================================
suppressPackageStartupMessages({library(readr); library(dplyr); library(tidyr); library(ggplot2); library(lavaan)})
set.seed(2026)
PROJECT_DIR <- Sys.getenv("ESD_PROJECT_DIR", getwd())
OUT <- file.path(PROJECT_DIR, "figures_R3"); dir.create(OUT, showWarnings = FALSE)
d <- read_csv(file.path(PROJECT_DIR, "national_N391_teaching_dataset.csv"), show_col_types = FALSE)
cc_all <- read_csv(file.path(PROJECT_DIR, "item_cost_coding.csv"), show_col_types = FALSE)
rev_items <- c("BEL_3","BEL_5","BEL_7","BEL_9","ESD_3","ESD_7","EQD_3","EQD_5","EQD_9")
for (v in rev_items) d[[paste0("r", v)]] <- 6 - d[[v]]; d$rBEL_6 <- 6 - d$BEL_6
sc <- function(cols) rowMeans(d[, cols])
d$IDEAL <- sc(c("BEL_1","BEL_2","BEL_4","BEL_8")); d$CRIT <- sc(c("rBEL_3","rBEL_5","rBEL_6","rBEL_7","rBEL_9"))
d$ESD <- sc(c("ESD_1","ESD_2","rESD_3","ESD_4","ESD_5","ESD_6","rESD_7","ESD_8","ESD_9"))
d$EQD <- sc(c("EQD_1","EQD_2","rEQD_3","EQD_4","rEQD_5","EQD_6","EQD_7","EQD_8","rEQD_9"))
d$BAR <- sc(paste0("BAR_", 1:10)); d$APP <- sc(c("APP_1","APP_2","APP_3","APP_4","APP_5","APP_9"))
d$specialty_grp <- case_when(grepl("PE70 SEN|PE71|Special Education", d$specialty) ~ "Special education",
                             grepl("PE70|Primary Teacher", d$specialty) ~ "Generalist primary", TRUE ~ "Subject specialist")

INK <- "#1F1F1F"; INK2 <- "#555555"; GREY <- "#6E6E69"; BLUE <- "#2B78D4"; DARK <- "#0F3F7F"; LIGHT <- "#85B5EE"; RED <- "#D83C3C"
th <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(colour = INK), axis.title = element_text(colour = INK, size = 11),
        strip.text = element_text(face = "bold", hjust = 0, colour = INK, size = 11),
        plot.background = element_rect(fill = "white", colour = NA), legend.position = "none")
pct4 <- function(i) 100 * mean(d[[i]] >= 4)

# ---- Figure 1 --------------------------------------------------------
modal <- function(x) { t <- table(x); as.numeric(names(t)[which.max(t)]) }
cc_all$level <- apply(cc_all[, c("rater1","rater2","rater3")], 1, modal)
f1 <- cc_all %>% mutate(pct = sapply(item, pct4),
  group = ifelse(domain == "Belief", "Beliefs (reference: no practice required)",
          c("Level 1: no resource beyond classroom talk", "Level 2: teacher planning time", "Level 3: institutional resource")[level]),
  shade = ifelse(domain == "Belief", "B", paste0("L", level)))
f1$group <- factor(f1$group, levels = c("Beliefs (reference: no practice required)", "Level 1: no resource beyond classroom talk",
                                        "Level 2: teacher planning time", "Level 3: institutional resource"))
f1 <- f1 %>% arrange(group, pct) %>% mutate(lab = factor(statement, levels = unique(statement)))
mb <- mean(f1$pct[f1$domain == "Belief"])
fig1 <- ggplot(f1, aes(pct, lab, fill = shade)) + geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.6, colour = INK2) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = c(B = GREY, L1 = LIGHT, L2 = BLUE, L3 = DARK)) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Respondents answering 4 or 5 on the five-point scale", y = NULL) + th +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1, size = 10),
        panel.spacing.y = unit(0.8, "lines"), axis.text.y = element_text(size = 10.5))
# put group labels above each panel instead of to the side
fig1 <- ggplot(f1, aes(pct, lab, fill = shade)) + geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.6, colour = INK2) +
  ggforce::facet_col(~ group, scales = "free_y", space = "free") +
  scale_fill_manual(values = c(B = GREY, L1 = LIGHT, L2 = BLUE, L3 = DARK)) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Respondents answering 4 or 5 on the five-point scale", y = NULL) + th +
  theme(panel.spacing.y = unit(0.9, "lines"), axis.text.y = element_text(size = 10.5))
ggsave(file.path(OUT, "Figure1_cost_gradient.png"), fig1, width = 9.6, height = 7.4, dpi = 300, bg = "white")

# ---- Figure 2 --------------------------------------------------------
bl <- c(BAR_1 = "The curriculum leaves no room", BAR_2 = "I have not had enough training", BAR_3 = "There is not enough time",
        BAR_4 = "No suitable teaching materials exist", BAR_5 = "School leadership does not support it",
        BAR_6 = "Parents are uninterested or opposed", BAR_7 = "Classes are too large",
        BAR_8 = "There is no collaboration between colleagues", BAR_9 = "Assessment rewards cognitive attainment only",
        BAR_10 = "[item absent from the archived questionnaire]")
f2 <- tibble(item = names(bl), lab = bl, pct = sapply(names(bl), pct4)) %>% arrange(pct) %>%
  mutate(lab = factor(lab, levels = lab), hl = item == "BAR_2")
fig2 <- ggplot(f2, aes(pct, lab, fill = hl)) + geom_col(width = 0.64) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, size = 3.6, colour = INK2) +
  annotate("text", x = 80, y = which(levels(f2$lab) == bl["BAR_2"]), hjust = 0, size = 3.5, colour = BLUE, fontface = "bold",
           label = "trained vs untrained teachers:\nno difference larger than d = 0.35") +
  scale_fill_manual(values = c(`FALSE` = GREY, `TRUE` = BLUE)) +
  scale_x_continuous(limits = c(0, 138), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Respondents answering 4 or 5 (agree or strongly agree)", y = NULL) + th
ggsave(file.path(OUT, "Figure2_barriers_training.png"), fig2, width = 9.2, height = 5.0, dpi = 300, bg = "white")

# ---- Figure 3 --------------------------------------------------------
chars <- c(gender = "Gender", age = "Age", years_experience = "Years of service", education_level = "Qualification",
           specialty_grp = "Post", school_area = "School setting", training_sustainability = "Training: sustainability",
           training_democracy = "Training: democratic education", rfcdc_known = "Aware of RFCDC")
cons <- c(IDEAL = "Idealistic beliefs", CRIT = "Critical stance", ESD = "ESD practice", EQD = "Democratic practice",
          BAR = "Perceived barriers", APP = "Self-rated implementation")
eta <- expand.grid(con = names(cons), ch = names(chars), stringsAsFactors = FALSE)
eta$eta2 <- mapply(function(cn, ch) summary(lm(d[[cn]] ~ factor(d[[ch]])))$r.squared, eta$con, eta$ch)
eta$con_lab <- factor(cons[eta$con], levels = rev(cons))
cat("Figure 3a: median eta2 =", round(100 * median(eta$eta2), 2), "%, max =", round(100 * max(eta$eta2), 1), "%\n")
p3a <- ggplot(eta, aes(100 * eta2, con_lab)) +
  geom_point(position = position_jitter(height = 0.18, width = 0, seed = 2026), size = 2.6, shape = 21,
             fill = "grey35", colour = "white", stroke = 0.4, alpha = 0.9) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  labs(title = "(a)", x = "Variance explained by one teacher characteristic (bivariate)", y = NULL) + th +
  theme(panel.grid.major.y = element_line(colour = "grey92"), plot.title = element_text(face = "bold", size = 12))
bg <- paste(names(chars), collapse = " + ")
R2 <- function(f) summary(lm(as.formula(paste("EQD ~", f)), data = d))$r.squared
full <- R2(paste(bg, "+ IDEAL + CRIT + BAR + ESD"))
blk <- tibble(block = factor(c("Background characteristics", "Perceptions (beliefs, critical stance, barriers)", "Sustainability practice"),
                              levels = rev(c("Background characteristics", "Perceptions (beliefs, critical stance, barriers)", "Sustainability practice"))),
  first = c(R2(bg), R2("IDEAL + CRIT + BAR"), R2("ESD")),
  last  = c(full - R2("IDEAL + CRIT + BAR + ESD"), full - R2(paste(bg, "+ ESD")), full - R2(paste(bg, "+ IDEAL + CRIT + BAR")))) %>%
  pivot_longer(c(first, last), names_to = "entry", values_to = "r2") %>%
  mutate(entry = factor(ifelse(entry == "first", "entered alone", "entered last (unique)"), levels = c("entered last (unique)", "entered alone")))
print(blk); cat("Full model R2 =", round(full, 3), "\n")
p3b <- ggplot(blk, aes(100 * r2, block, fill = entry)) + geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * r2)), position = position_dodge(width = 0.72), hjust = -0.15, size = 3.4, colour = INK2) +
  scale_fill_manual(values = c(`entered alone` = LIGHT, `entered last (unique)` = DARK), breaks = c("entered alone", "entered last (unique)"), name = NULL) +
  scale_x_continuous(limits = c(0, 42), breaks = seq(0, 40, 10), labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  labs(title = "(b)", x = "Variance in democratic practice explained by each block (one multivariable model)", y = NULL) + th +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 12))
fig3 <- gridExtra::arrangeGrob(p3a, p3b, ncol = 1, heights = c(1, 0.85))
ggsave(file.path(OUT, "Figure3_characteristics_and_blocks.png"), fig3, width = 9.4, height = 7.0, dpi = 300, bg = "white")

# ---- Figure 4 --------------------------------------------------------
pm <- '
 ESD ~ a1*IDEAL + a2*CRIT + a3*BAR
 EQD ~ b*ESD + c1*IDEAL + c2*CRIT + c3*BAR'
fp <- sem(pm, data = d); pe <- parameterEstimates(fp); pe <- pe[pe$op == "~", ]
co <- setNames(pe$est, pe$label); pv <- setNames(pe$pvalue, pe$label); print(round(co, 3))
nodes <- tibble(id = c("IDEAL","CRIT","BAR","ESD","EQD"),
  lab = c("Idealistic\nbeliefs", "Critical stance\ntoward ESD", "Perceived\nbarriers", "ESD practice", "Equality and\ndemocratic practice"),
  x = c(0, 0, 0, 2.1, 4.2), y = c(3, 2, 1, 3.75, 2))
edges <- tibble(from = c("IDEAL","CRIT","BAR","ESD","IDEAL","CRIT","BAR"), to = c(rep("ESD",3), rep("EQD",4)),
                lbl = c("a1","a2","a3","b","c1","c2","c3"), t = c(.30,.22,.20,.5,.62,.62,.62))
gx <- function(id) nodes$x[match(id, nodes$id)]; gy <- function(id) nodes$y[match(id, nodes$id)]
off <- 0.42
edges <- edges %>% mutate(est = co[lbl], p = pv[lbl],
  x0 = gx(from) + ifelse(from == "ESD", 0.45, off), y0 = gy(from) - ifelse(from == "ESD", 0.18, 0),
  x1 = gx(to) - ifelse(to == "EQD", 0.62, 0.55), y1 = gy(to) + ifelse(to == "EQD", ifelse(from == "ESD", 0.22, 0), -0.12),
  col = ifelse(p >= .05, "ns", ifelse(est > 0, "pos", "neg")), w = 0.4 + 4 * abs(est),
  lx = x0 + t * (x1 - x0), ly = y0 + t * (y1 - y0),
  txt = ifelse(p >= .05, sprintf("%+.2f n.s.", est), sprintf("%+.2f", est)))
edges$y1[edges$from != "ESD" & edges$to == "EQD"] <- 2 + c(0.18, 0, -0.18)[match(edges$from[edges$from != "ESD" & edges$to == "EQD"], c("IDEAL","CRIT","BAR"))]
sel <- edges$to == "ESD"
edges$x1[sel] <- c(1.55, 1.85, 2.15)[match(edges$from[sel], c("IDEAL","CRIT","BAR"))]
edges$y1[sel] <- 3.56
bsel <- edges$from == "ESD"; edges$x1[bsel] <- 3.78; edges$y1[bsel] <- 2.36
edges <- edges %>% mutate(lx = x0 + t * (x1 - x0), ly = y0 + t * (y1 - y0))
fig4 <- ggplot() +
  geom_segment(data = edges, aes(x0, y0, xend = x1, yend = y1, colour = col, linewidth = w),
               arrow = arrow(length = unit(0.16, "inches"), type = "closed"), lineend = "round") +
  geom_label(data = edges, aes(lx, ly, label = txt, colour = col), fill = "white", label.size = 0, size = 3.7, fontface = "bold",
             label.padding = unit(0.12, "lines")) +
  geom_label(data = nodes, aes(x, y, label = lab), fill = "#EFEEEA", label.size = 0, size = 4.1, colour = INK, label.padding = unit(0.45, "lines")) +
  scale_colour_manual(values = c(pos = BLUE, neg = RED, ns = "grey65")) + scale_linewidth_identity() +
  coord_cartesian(xlim = c(-0.6, 4.9), ylim = c(0.6, 4.1)) + theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(OUT, "Figure4_decomposition.png"), fig4, width = 9.6, height = 5.2, dpi = 300, bg = "white")
cat("Figures written to", OUT, "\n")
