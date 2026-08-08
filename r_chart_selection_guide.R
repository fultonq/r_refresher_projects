# ==============================================================================
# R PROGRAMMING REFRESHER -- PART 3: PLOTS, CHARTS, AND WHICH ONE FITS WHICH DATA
# ==============================================================================
# Continues the numbering from Parts 1-2 (which ended at Section 27). The
# central question in every section below is always the same: how many
# variables, and what TYPE is each one (categorical vs numeric, ordered vs
# not)? The right chart falls out of that almost mechanically -- it's rarely
# a matter of taste. Standalone/runnable via Rscript or source().
#
# Uses ggplot2 (already a dependency in earlier scripts) plus base R for a
# few classics (barplot/pie/mosaicplot/pairs) so you can see both idioms.
# No package beyond ggplot2 is required -- geom_bin2d() (Section 31) is used
# instead of geom_hex() specifically to avoid the extra "hexbin" dependency.
# ==============================================================================

library(ggplot2)

section <- function(num, title) {
  bar <- paste(rep("=", 78), collapse = "")
  cat("\n", bar, "\n", sep = "")
  cat(sprintf("%2s. %s\n", num, title))
  cat(bar, "\n", sep = "")
}

# ------------------------------------------------------------------------------
section(28, "THE DECISION FRAMEWORK: DATA TYPE -> CHART CHOICE")
# ------------------------------------------------------------------------------
cat("The question to ask FIRST is never 'what looks nice' -- it's 'how many\n")
cat("variables am I plotting, and what type is each one'. The chart choice\n")
cat("falls out of that almost automatically:\n\n")

cheat_sheet <- data.frame(
  Situation = c(
    "1 categorical variable",
    "1 numerical variable (distribution shape)",
    "2 numerical variables (relationship)",
    "1 categorical + 1 numerical (compare groups)",
    "2 categorical variables (relationship)",
    "Ordered sequence / time",
    "Part-to-whole composition",
    "3+ variables at once"
  ),
  Best_Chart = c(
    "Bar chart",
    "Histogram / density plot / boxplot",
    "Scatter plot (+ trend line)",
    "Boxplot or violin plot, grouped",
    "Grouped bar, 100% stacked bar, or mosaic plot",
    "Line chart",
    "100% stacked bar -- NOT pie, past about 3 slices",
    "Pairs plot, correlation heatmap, or facets/color as extra encodings"
  ),
  stringsAsFactors = FALSE
)
print(cheat_sheet, row.names = FALSE)

# ------------------------------------------------------------------------------
section(29, "ONE CATEGORICAL VARIABLE -- BAR CHART (AND WHY NOT PIE)")
# ------------------------------------------------------------------------------
survey_df <- data.frame(
  Language = c("Java", "Python", "Rust", "Go", "C++"),
  Votes    = c(42, 58, 21, 33, 27)
)

print(
  ggplot(survey_df, aes(x = reorder(Language, -Votes), y = Votes)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = Votes), vjust = -0.4) +
    labs(title = "Preferred Language -- Bar Chart", x = NULL, y = "Votes") +
    theme_minimal()
)

cat("Bar LENGTH is something the eye compares almost perfectly. Angle and\n")
cat("area -- what a pie chart asks you to compare -- are things the eye is\n")
cat("measurably worse at (Cleveland & McGill, 1984). Not a style opinion.\n\n")

pie(survey_df$Votes, labels = survey_df$Language,
    main = "Same Data as a Pie Chart -- Harder to Rank Accurately")
cat("Try ranking Go vs C++ by eye in the pie above, then check the bar chart\n")
cat("above it. A pie chart is defensible for a SINGLE proportion (e.g. one\n")
cat("highlighted '62% yes' wedge) -- rarely for comparing 3+ slices.\n")

# ------------------------------------------------------------------------------
section(30, "ONE NUMERICAL VARIABLE -- HISTOGRAM / DENSITY / BOXPLOT")
# ------------------------------------------------------------------------------
set.seed(11)
commute_minutes <- rgamma(300, shape = 4, scale = 6)  # right-skewed, like real commute times

print(
  ggplot(data.frame(commute_minutes), aes(x = commute_minutes)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(color = "darkred", linewidth = 1) +
    labs(title = "Commute Time -- Histogram + Density Overlay",
         x = "Minutes", y = "Density") +
    theme_minimal()
)

cat("Histogram bin width is a real analytical choice, not a default to\n")
cat("ignore -- too few bins hides structure (skew, bimodality), too many\n")
cat("turns into noise. A density overlay is a smoothed, bin-free cross-check.\n\n")

cat("Boxplot for the same variable -- less shape detail, but compact and\n")
cat("becomes essential once you want to COMPARE this distribution against\n")
cat("others (see Section 32):\n")
boxplot(commute_minutes, horizontal = TRUE, col = "lightgray",
        main = "Commute Time -- Boxplot", xlab = "Minutes")

# ------------------------------------------------------------------------------
section(31, "TWO NUMERICAL VARIABLES -- SCATTER PLOT")
# ------------------------------------------------------------------------------
set.seed(3)
n_emp <- 200
emp_df <- data.frame(
  years_experience = runif(n_emp, 0, 20),
  salary_k         = NA
)
emp_df$salary_k <- 45 + 3.2 * emp_df$years_experience + rnorm(n_emp, sd = 10)

print(
  ggplot(emp_df, aes(x = years_experience, y = salary_k)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(title = "Salary vs Experience -- Scatter Plot + Linear Trend",
         x = "Years of Experience", y = "Salary ($k)") +
    theme_minimal()
)

cat("A scatter plot is the only chart on this whole list built to answer\n")
cat("'is there a relationship, and what shape is it' for two numeric\n")
cat("variables -- nothing else here shows that.\n\n")

set.seed(4)
n_big  <- 20000
big_df <- data.frame(x = rnorm(n_big), y = rnorm(n_big))

print(
  ggplot(big_df, aes(x = x, y = y)) +
    geom_bin2d(bins = 40) +
    scale_fill_viridis_c() +
    labs(title = paste0("N = ", n_big, " Points -- 2D Binning Instead of a Scatter"),
         subtitle = "A plain scatter of 20,000 points is just an ink blob (overplotting)") +
    theme_minimal()
)

cat("Past a few thousand points, a plain scatter plot overplots into a\n")
cat("solid blob -- geom_bin2d()/geom_hex() (or alpha= transparency) recovers\n")
cat("the actual density structure that a raw scatter plot would hide.\n")

# ------------------------------------------------------------------------------
section(32, "1 CATEGORICAL + 1 NUMERICAL -- COMPARE GROUPS")
# ------------------------------------------------------------------------------
set.seed(5)
dept_df <- data.frame(
  Department = rep(c("Engineering", "Sales", "Support", "Design"), each = 40),
  Salary_k   = c(rnorm(40, 95, 15), rnorm(40, 78, 20), rnorm(40, 58, 10), rnorm(40, 72, 12))
)

print(
  ggplot(dept_df, aes(x = Department, y = Salary_k, fill = Department)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red") +
    labs(title = "Salary by Department -- Boxplot", y = "Salary ($k)") +
    theme_minimal() + theme(legend.position = "none")
)

cat("A boxplot compares the FIVE-NUMBER SUMMARY across groups at a glance --\n")
cat("median, spread, skew, and outliers, all in one shape per group.\n\n")

print(
  ggplot(dept_df, aes(x = Department, y = Salary_k, fill = Department)) +
    geom_violin(alpha = 0.7) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 0.8) +
    labs(title = "Salary by Department -- Violin + Raw Points",
         subtitle = "Violin recovers shape a boxplot flattens away (e.g. bimodality)",
         y = "Salary ($k)") +
    theme_minimal() + theme(legend.position = "none")
)

cat("A violin plot restores distribution SHAPE (bimodal? long-tailed?) that\n")
cat("a boxplot's five numbers can hide. Jittered raw points on top show the\n")
cat("actual sample size per group -- guards against being fooled by a\n")
cat("smooth-looking violin built from very few underlying points.\n")

# ------------------------------------------------------------------------------
section(33, "TWO CATEGORICAL VARIABLES -- GROUPED BAR, STACKED BAR, MOSAIC")
# ------------------------------------------------------------------------------
survey2_df <- data.frame(
  Department = rep(c("Engineering", "Sales", "Support"), each = 3),
  Experience = rep(c("Junior", "Mid", "Senior"), times = 3),
  Headcount  = c(12, 20, 8, 25, 15, 5, 18, 10, 3)
)

print(
  ggplot(survey2_df, aes(x = Department, y = Headcount, fill = Experience)) +
    geom_col(position = "dodge") +
    labs(title = "Headcount by Department and Seniority -- Grouped Bar") +
    theme_minimal()
)

print(
  ggplot(survey2_df, aes(x = Department, y = Headcount, fill = Experience)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Same Data, 100% Stacked -- Shows COMPOSITION, not raw counts",
         y = "Share of Department") +
    theme_minimal()
)

cat("Grouped bar answers 'which department has more Senior people in raw\n")
cat("numbers'. 100% stacked bar answers a DIFFERENT question -- 'which\n")
cat("department is proportionally most Senior' -- same underlying data,\n")
cat("different question, different chart.\n\n")

count_matrix <- xtabs(Headcount ~ Department + Experience, data = survey2_df)
mosaicplot(count_matrix, main = "Mosaic Plot -- Box Area = Cell Count",
           color = c("lightblue", "steelblue", "darkblue"), las = 1)
cat("A mosaic plot encodes count directly as AREA -- useful once there are\n")
cat("more than 2-3 categories per side and a grouped bar chart gets cluttered.\n")

# ------------------------------------------------------------------------------
section(34, "ORDERED SEQUENCE / TIME -- LINE CHART")
# ------------------------------------------------------------------------------
months <- seq(as.Date("2025-01-01"), by = "month", length.out = 12)
set.seed(6)
revenue_df <- data.frame(
  Month   = rep(months, 2),
  Team    = rep(c("North", "South"), each = 12),
  Revenue = c(cumsum(rnorm(12, 5, 3)) + 100, cumsum(rnorm(12, 3, 4)) + 80)
)

print(
  ggplot(revenue_df, aes(x = Month, y = Revenue, color = Team)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    labs(title = "Monthly Revenue by Team -- Line Chart", y = "Revenue ($k)") +
    theme_minimal()
)

cat("Line charts work BECAUSE position along x is meaningfully ordered\n")
cat("(time, in this case). Connecting points with a line on data that ISN'T\n")
cat("ordered -- e.g. connecting bars across unrelated categories -- implies\n")
cat("a trend that doesn't exist. That's a real, common misuse to avoid.\n")

# ------------------------------------------------------------------------------
section(35, "3+ VARIABLES AT ONCE -- PAIRS PLOT, CORRELATION HEATMAP, FACETS")
# ------------------------------------------------------------------------------
set.seed(9)
multi_df <- data.frame(
  hours_studied = runif(80, 0, 10),
  sleep_hours   = runif(80, 4, 9),
  attendance    = runif(80, 0.5, 1.0)
)
multi_df$exam_score <- 40 + 4 * multi_df$hours_studied + 3 * multi_df$sleep_hours +
                        30 * multi_df$attendance + rnorm(80, sd = 5)

pairs(multi_df, main = "pairs() -- Every Variable Against Every Other",
      pch = 19, col = "steelblue")
cat("pairs() is the fast first look at a new dataset: a grid of every\n")
cat("pairwise scatter plot at once, before deciding what deserves a closer,\n")
cat("dedicated chart.\n\n")

corr_matrix <- round(cor(multi_df), 2)
cat("Correlation matrix:\n")
print(corr_matrix)

corr_long <- as.data.frame(as.table(corr_matrix))
names(corr_long) <- c("Var1", "Var2", "Correlation")

print(
  ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = Correlation), size = 3) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred", midpoint = 0) +
    labs(title = "Correlation Heatmap") +
    theme_minimal() +
    theme(axis.title = element_blank())
)

cat("\nFor MORE than about 2-3 numeric variables plus a grouping variable,\n")
cat("facets (facet_wrap()/facet_grid(), used already in the supermarket\n")
cat("script) are usually the honest choice -- one small chart per group,\n")
cat("rather than cramming color + shape + size into a single overloaded plot.\n")

cat("\n", paste(rep("=", 78), collapse = ""), "\n", sep = "")
cat("PART 3 COMPLETE.\n")
