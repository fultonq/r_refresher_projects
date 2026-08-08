# R Refresher Projects

A set of self-contained, runnable R scripts covering core R programming
through applied statistical analysis, each paired with an equivalent
`.Rmd` R Notebook. All scripts run top to bottom via `Rscript <file>.R`
or `source()`, and only depend on `ggplot2` plus base R (`MASS` is used,
but ships bundled with any standard R install).

## R Refresher series (Parts 1–5)

Numbered sections continue across files (Part 1–2 ends at Section 27,
Part 3 picks up at 28, etc.) — read them in this order:

| File | Sections | Covers |
|---|---|---|
| `r_refresher.R` / `r_refresher_v2.R` (identical) | 1–22 | Parts 1–2: logical vectors, indexing, object attributes, arrays, matrix algebra (`solve`, `eigen`, `svd`), lists, data frames, file I/O, control flow, and a regression toolkit (`lm`, `glm`, `rlm`, Q-Q plots, `attach`/`with`) |
| `r_chart_selection_guide.R` | 28–35 | Part 3: chart-selection decision framework — which plot fits which data (bar vs. pie, histogram/density/boxplot, scatter + 2D binning, grouped/stacked bar, mosaic plot, line chart, pairs plot + correlation heatmap) |
| `r_refresher_v3.R` | 36–39 | Part 4: applied case study using the school-district data — bar-of-means pitfalls at small n, ANOVA interaction plots, and the same data redesigned for three different audiences |
| `statistics_paradigms.R` | 40–47 | Part 5: the seven statistical paradigms — descriptive, EDA (Anscombe's quartet), inferential (t-test, effect size), predictive (train/test RMSE), multivariate (PCA, k-means), nonparametric (Kruskal-Wallis, Spearman), and Bayesian (conjugate Beta updating) |

## Applied analysis pipelines

| File | What it does |
|---|---|
| `clinical_trials.R` | Pilot clinical trial: 39 patients, 2 arms (Alpha/Placebo), 3 visits. Per-arm summary stats and a Welch t-test on the Day 0 → Day 28 change score, plus a patient-trajectory plot. |
| `clinical_trials_v2.R` | Scaled-up trial: 1,700 patients across 3 sites. Adds Site as a covariate (naive vs. site-adjusted model, plus an Arm × Site interaction check), a real Alpha treatment effect vs. a flat Placebo arm, and a faceted mean ± SE trajectory plot instead of one line per patient. Writes `clinical_trial_data_3site.csv`. |
| `school_district.R` | Splits a small student roster by school with `split()`, builds a weighted-index campus ranking (60% math / 40% attendance), and charts it with `ggplot2`. |
| `school_district_v2.R` | 700-student district: two-way ANOVA (Type I vs. Type II sums of squares for an unbalanced design), model diagnostics (Bartlett's, Shapiro-Wilk), Tukey HSD post-hoc comparisons, and automated per-school report generation archived into a zip file. |
| `supermarket_projection.R` | 6-month grocery cost forecast with inflation and item-specific demand noise, ranks items by coefficient of variation (volatility), and produces faceted category-trend and cumulative-spend charts. |
| `generate_forecast.R` | Placeholder stub — not yet implemented. |

## R Notebooks

Every `.R` file above has a matching `.Rmd` R Notebook of the same base
name (e.g. `clinical_trials_v2.R` → `clinical_trials_v2.Rmd`). Each
notebook mirrors its script's structure as numbered sections/steps, one
runnable chunk per section, with the script's header comments carried
over as notebook intro text. Open any `.Rmd` in RStudio and click
**Preview** to render the interactive HTML notebook (rendering requires
pandoc, bundled with RStudio).

`r_refresher.nb.html` is a pre-rendered example of what that output
looks like.

## Generated output

Running the pipelines writes their own output back into this directory
(git-tracked as of the initial commit):

- `patient_clinical_data.csv` — output of `clinical_trials.R`
- `clinical_trial_data_3site.csv` — output of `clinical_trials_v2.R`

`school_district_v2.R` and `supermarket_projection.R` also write
CSVs/PNGs/a zip archive on each run; those aren't checked in since
they're regenerated from scratch every time the script runs.
