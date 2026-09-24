# Education for sustainability in Greek primary schools: national teacher survey (N = 391)

Data and analysis code accompanying the article. The survey asked primary teachers
working in Greek public schools about their beliefs concerning education for
sustainability, the classroom practices they report, the barriers they perceive and
their appraisal of their own implementation.

- Sample: 391 teachers
- Fieldwork: 22 May – 20 September 2026
- Instrument: 46 five-point Likert items in five blocks, plus background questions
- Language of administration: Greek

Everything reported in the article and its Supplementary Information is produced by
the three scripts in `code/` from the two files in `data/`.

---

## Contents

```
data/     national_N391_teaching_dataset.csv   responses, one row per teacher
          item_cost_coding.csv                 institutional-cost codes, three coders
code/     national_edu_analysis.R              main analysis
          robustness_checks.R                  alternative estimators
          paper_figures.R                      figures and tables of the article
README.md
```

## Reproducing the analysis

R 4.3 or later. The scripts install any missing package on first run
(R 4.4.3; tidyverse 2.0.0, psych 2.6.5, lavaan 0.7-2, sandwich 3.1-3, lmtest 0.9-40,
effectsize 1.0.3, sjPlot 2.9.0, boot 1.3-31, car 3.1-5, broom 1.0.7, ggforce 0.5.0,
ggtext 0.2.0).

Set the working directory to this folder and run, in this order:

```r
source("code/national_edu_analysis.R")   # -> outputs/results_national/
source("code/robustness_checks.R")       # -> outputs/results_national/robustness_output.txt
source("code/paper_figures.R")           # -> outputs/paper_outputs/
```

The scripts contain no absolute paths. Each locates `data/` by searching the
working directory, its own folder and their parents; alternatively, set the
environment variable `ESD_PROJECT_DIR` to this folder. None of them calls `setwd()`.
The three scripts are independent: each reads the data file directly and can be run
on its own.

`set.seed(2026)` is set in all three, so the bootstrap intervals, the parallel
analysis and the figure jitter reproduce exactly.

---

## Measures

Six construct scores, each the mean of its items; a score is computed only when
every item in the scale is answered. There are no missing values on the 46 items.

| Construct | Items | α |
|---|---|---|
| Idealistic beliefs | 4 | .72 |
| Perceived implementation gap | 5 | .78 |
| ESD practices | 9 | .84 |
| Equality and democratic practices | 9 | .79 |
| Perceived barriers | 10 | .86 |
| Self-rated implementation | 6 | .78 |

Nine items were negatively worded by design and are reverse-scored. `BEL_6` is
reverse-scored in addition, on the factor-analytic grounds set out in Step 5b of
`code/national_edu_analysis.R`.

### Institutional-cost coding

Each practice item carries a code on a three-level rubric, specified before the
item-level response frequencies were examined:

1. **No resource beyond classroom talk** — can be done alone, inside an ordinary
   lesson, with nothing bought or booked.
2. **Teacher planning time** — needs preparation, re-sequencing or coordination with
   colleagues, but no external resource.
3. **Institutional resource** — needs materials, budget, timetable change, or an
   actor outside the classroom.

Three coders assigned a level to each item. Each worked alone, without sight of the
others' assignments, and all were blind to the item response distributions at the
time of coding, working from the item text and the rubric alone. The analysis uses
the modal level of the three. Agreement was exact on 54% of items, every
disagreement fell within one level, and every item had a majority level
(Fleiss' κ = 0.49, ordinal Krippendorff's α = 0.70, Kendall's *W* = 0.80, *p* = .004).
`code/paper_figures.R` also recomputes the gradient under each coder's assignment
taken alone.

Belief items describe an attitude rather than an activity. They are coded in
`item_cost_coding.csv` for completeness but are excluded from the gradient and enter
Figure 1 as a reference band only.

The resulting ordering: ρ = −0.90 between coded cost and reported frequency within
the sustainability items, ρ = −0.92 across all 13 coded practice items; mean
proportion reporting the practice at 4–5 is 86.5% at level 1, 61.8% at level 2 and
42.5% at level 3.

---

## Analytic decisions

**Inference of record.** Heteroscedasticity is present (Breusch–Pagan *p* < .001),
so all regression inference uses HC3 robust standard errors. Classical OLS output is
printed alongside.

**Multiplicity.** The 54 teacher-characteristic comparisons are corrected as one
family using Benjamini–Hochberg FDR. Uncorrected and corrected *p*-values are both
reported. Post-hoc tests are run only where the corrected test survives.

**Equivalence.** Where a null is interpreted as an absence of difference rather than
as a failure to detect one, it is supported by a two one-sided tests procedure with
bounds of ±0.35 *SD*, stated in advance.

**Moderation.** The ESD × barriers interaction was tested and is not supported. The
decision rule was fixed before inspection: the interaction counts as supported only
if the robust test and the bootstrap interval agree. They do not (robust *p* = .076;
95% BCa CI [−0.008, 0.193]; Δ*R*² = .006).

**Mediation.** Indirect effects use 5,000 bootstrap resamples. The data are
cross-sectional self-reports, so the model tests whether they are consistent with the
proposed ordering, not that the ordering is causal.

**Excluded measure.** `APP_COND` (2 items, perceived enabling conditions) is computed
and reported descriptively but excluded from all inferential analysis. On the
administered form this block carried the barriers instruction line above positively
worded statements, so the response frame is not recoverable; the empirical signature
is that it correlates positively with perceived barriers where the construct requires
a negative correlation. Documented in Step 3 of `code/national_edu_analysis.R`.

**Data quality.** Twenty-two respondents gave ten or more identical consecutive
answers across the 46 items. The primary analysis uses the full sample; the screened
sample is reported as a declared sensitivity check.

**Known features of the instrument.** `BAR_10` appears in the dataset but not in the
archived questionnaire and is labelled as undocumented wherever it appears. The RFCDC
awareness item was three-level on the form and binary in the dataset; the skip-logic
cross-tabulation is printed in Step 2. `specialty` was captured as an open field and
is collapsed into three groups for analysis.

---

## Participants and consent

Participation was voluntary, anonymous and preceded by an information page; the
consent item is retained in the dataset (`consent`) and all 391 retained cases
answered it affirmatively. No names, contact details, school identifiers, IP
addresses or open-text responses were collected, and none appear in these files.
Per-response submission timestamps are not retained: they enter no analysis, and a
date-and-time stamp is a quasi-identifier when combined with the background variables.
