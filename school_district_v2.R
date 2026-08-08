# ==============================================================================
# DISTRICT INTELLIGENCE PIPELINE: INFERENCE, AUTOMATED REPORTING & ARCHIVING
# [REVISED]
# ==============================================================================
# Changes from original:
#   1. FIXED (statistical soundness): aov() computes Type I (sequential) sums
#      of squares. This is only order-independent for BALANCED designs. This
#      dataset is unbalanced by construction (School and Race_Ethnicity are
#      sampled with unequal probabilities), so Type I SS results would shift
#      if you simply reordered the formula terms. Added car::Anova(type = 2)
#      for a design-order-independent test of main effects, alongside the
#      original Type I model for comparison.
#   2. ADDED: diagnostic checks (residual normality, homogeneity of variance)
#      that should accompany any formal ANOVA before trusting the p-values.
#   3. ADDED: Tukey HSD post-hoc test on the School main effect, since the
#      omnibus test alone doesn't say which campuses differ.
#   4. FIXED (robustness): sink() had no on.exit() guard. If anything errored
#      between sink(report_file) and sink(), every later console call in the
#      session would silently keep writing into that file. Wrapped in
#      on.exit(sink(), add = TRUE) so the connection always closes.
#   5. NOTED: NA rows in Math_Grade are silently dropped by aov()'s default
#      na.action -- now explicitly counted and reported.
# ==============================================================================

library(ggplot2)

# NOTE: this version needs NO extra packages. car::Anova(type=2) was replaced
# with a base-R nested-model equivalent below -- if `car` failed to install
# (very common: it pulls in nloptr/pbkrtest/quantreg/mgcv, several of which
# need Fortran + system libs to compile from source), you don't need it.

# ------------------------------------------------------------------------------
# STEP 1: INITIALIZE LARGE-SCALE DEMOGRAPHIC REGISTRY (700 STUDENTS)
# ------------------------------------------------------------------------------
set.seed(2026)
total_students <- 700

campus_names <- c(
  "Lincoln High", "Washington High", "Jefferson Middle", "Kennedy Elementary",
  "Madison Academy", "Roosevelt Tech", "Garfield Charter"
)
races_pool <- c("Asian/Pas. Islander", "Hispanic/Latino", "Black/African Amer.", "White/Caucasian")
ell_pool   <- c("Yes", "No")

students_df <- data.frame(
  Student_ID     = paste0("STU-", 1000 + 1:total_students),
  Name           = paste0("Student_", 1:total_students),
  School         = sample(campus_names, total_students, replace = TRUE),
  Race_Ethnicity = sample(races_pool, total_students, replace = TRUE, prob = c(0.15, 0.35, 0.25, 0.25)),
  ELL_Status     = sample(ell_pool, total_students, replace = TRUE, prob = c(0.20, 0.80)),
  Attendance     = round(pmin(pmax(rnorm(total_students, mean = 0.88, sd = 0.08), 0.60), 1.00), 2),
  Math_Grade     = round(pmin(pmax(rnorm(total_students, mean = 76,  sd = 12),   45), 100), 0),
  stringsAsFactors = FALSE
)
students_df$Math_Grade[sample(1:total_students, size = total_students * 0.05)] <- NA

# Sanity check: are there any empty School x Race_Ethnicity cells?
# (Type II/III models can fail or become uninterpretable with empty cells.)
cell_counts <- table(students_df$School, students_df$Race_Ethnicity)
if (any(cell_counts == 0)) {
  warning("One or more School x Race_Ethnicity cells is empty -- interaction term unreliable.")
}

# ------------------------------------------------------------------------------
# STEP 2: RUN TWO-WAY ANOVA STATISTICAL INFERENCE TEST
# ------------------------------------------------------------------------------
n_dropped <- sum(is.na(students_df$Math_Grade))
cat(sprintf("NOTE: %d of %d rows have missing Math_Grade and are dropped by aov()'s default na.action.\n",
            n_dropped, total_students))

# Original model, Type I (sequential) SS -- order of formula terms matters here
# because the design is unbalanced.
anova_model <- aov(Math_Grade ~ School * Race_Ethnicity, data = students_df)

print("========================================================================")
print("       TYPE I (SEQUENTIAL) ANOVA -- order-dependent on unbalanced data  ")
print("========================================================================")
print(summary(anova_model))

# FIXED: Type II SS -- tests each main effect adjusted for the other main
# effect (but not for the interaction), the standard default for unbalanced
# designs without a strong theoretical reason to expect a real interaction.
#
# Base-R equivalent of car::Anova(type=2), no extra package required:
# Type II SS(A) = RSS(model with B only) - RSS(model with A + B)
# i.e. the improvement from adding A to a model that already has B.
# Comparing two NESTED aov/lm objects with anova() gives exactly this F-test.
print("========================================================================")
print("  TYPE II ANOVA (base R, nested-model F-tests) -- order-independent   ")
print("========================================================================")

m_school_only <- aov(Math_Grade ~ School, data = students_df)
m_race_only   <- aov(Math_Grade ~ Race_Ethnicity, data = students_df)
m_additive    <- aov(Math_Grade ~ School + Race_Ethnicity, data = students_df)
m_full        <- aov(Math_Grade ~ School + Race_Ethnicity + School:Race_Ethnicity, data = students_df)
# m_full is identical to anova_model above (School*Race_Ethnicity is shorthand
# for School + Race_Ethnicity + School:Race_Ethnicity) -- written out here so
# the nesting relationships below are explicit.

cat("\nType II test -- School, adjusted for Race_Ethnicity:\n")
print(anova(m_race_only, m_additive))

cat("\nType II test -- Race_Ethnicity, adjusted for School:\n")
print(anova(m_school_only, m_additive))

cat("\nType II test -- Interaction (School:Race_Ethnicity), adjusted for both main effects:\n")
print(anova(m_additive, m_full))
cat("========================================================================\n")

# ------------------------------------------------------------------------------
# STEP 2b: MODEL DIAGNOSTICS (new -- required before trusting the p-values)
# ------------------------------------------------------------------------------
cat("\n--- MODEL DIAGNOSTICS ---\n")

# Homogeneity of variance across School x Race_Ethnicity cells.
# base-R substitute for car::leveneTest(): Bartlett's test does the same job
# but is more sensitive to non-normality than Levene's -- treat a borderline
# p-value with more caution than you would with Levene's.
# bartlett.test() errors on NA, so use complete cases only (same rows aov() used)
complete_rows  <- !is.na(students_df$Math_Grade)
group_factor   <- interaction(students_df$School[complete_rows],
                               students_df$Race_Ethnicity[complete_rows], drop = TRUE)
bartlett_result <- bartlett.test(students_df$Math_Grade[complete_rows], group_factor)
cat("Bartlett's Test (homogeneity of variance, base R):\n")
print(bartlett_result)

# Residual normality (Shapiro-Wilk caps at n=5000; well within range here)
shapiro_result <- shapiro.test(residuals(anova_model))
cat(sprintf("\nShapiro-Wilk on residuals: W = %.4f, p = %.4f\n",
            shapiro_result$statistic, shapiro_result$p.value))
cat("(p < .05 on either test means an ANOVA assumption is violated --\n")
cat(" interpret main-effect p-values with appropriate caution.)\n")

# ------------------------------------------------------------------------------
# STEP 2c: POST-HOC COMPARISON (new -- omnibus test alone doesn't say WHICH
# schools differ)
# ------------------------------------------------------------------------------
school_only_model <- aov(Math_Grade ~ School, data = students_df)
tukey_result <- TukeyHSD(school_only_model)
cat("\n--- TUKEY HSD: PAIRWISE SCHOOL COMPARISONS ---\n")
print(tukey_result)

# ------------------------------------------------------------------------------
# STEP 3: DEFINE AUTOMATED PRINCIPAL TEXT REPORT ENGINE
# ------------------------------------------------------------------------------
write_executive_summary <- function(school_df, school_name, destination_path) {
  total_enrolled <- nrow(school_df)
  avg_attendance <- mean(school_df$Attendance, na.rm = TRUE) * 100
  avg_math_grade <- mean(school_df$Math_Grade, na.rm = TRUE)
  missing_grades <- sum(is.na(school_df$Math_Grade))
  ell_count      <- sum(school_df$ELL_Status == "Yes")

  report_file <- file.path(destination_path, "executive_performance_brief.txt")

  # FIXED: on.exit guarantees sink() closes even if something below errors,
  # so the console connection can never be left silently redirected.
  sink(report_file)
  on.exit(sink(), add = TRUE)

  cat("========================================================================\n")
  cat(sprintf("        EXECUTIVE CAMPUS AUDIT BRIEF: %s\n", toupper(school_name)))
  cat("========================================================================\n")
  cat(sprintf("Report Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("------------------------------------------------------------------------\n")
  cat(sprintf("Total Enrolled Student Footprint : %d active student records\n", total_enrolled))
  cat(sprintf("Campus-Wide Attendance Continuity: %.1f%%\n", avg_attendance))
  cat(sprintf("Mean Academic Math Performance   : %.1f%%\n", avg_math_grade))
  cat(sprintf("English Language Learner Cohort  : %d students enrolled\n", ell_count))
  cat(sprintf("Flagged Incomplete Data Gaps (NA): %d student entries missing\n", missing_grades))
  cat("------------------------------------------------------------------------\n")
  cat("Confidential Data Notice: This administrative brief contains protected\n")
  cat("subgroup demographics. Distribute in compliance with district policies.\n")
  cat("========================================================================\n")
}

# ------------------------------------------------------------------------------
# STEP 4: DEPLOY FILE ARCHITECTURE & EXECUTE REPORT GENERATOR
# ------------------------------------------------------------------------------
unique_schools <- unique(students_df$School)
folder_vector  <- character(length(unique_schools))

cat("--- DEPLOYING SYSTEM INFRASTRUCTURE DIRECTORY SWEEP ---\n")

for (i in seq_along(unique_schools)) {
  school <- unique_schools[i]

  folder_name <- gsub(" ", "_", tolower(school))
  target_dir  <- file.path(getwd(), folder_name)
  folder_vector[i] <- folder_name

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  school_cohort_df <- students_df[students_df$School == school, ]

  write.csv(school_cohort_df, file = file.path(target_dir, "student_ledger.csv"), row.names = FALSE)
  write_executive_summary(school_cohort_df, school, target_dir)

  cat(sprintf("  [SUCCESS] Packaged folder node: /%-18s | Written text brief & CSV\n", paste0(folder_name, "/")))
}

# ------------------------------------------------------------------------------
# STEP 5: COMPRESS SEGMENTED FOLDERS INTO A UNIFIED ZIP ARCHIVE
# ------------------------------------------------------------------------------
zip_filename <- "district_principals_package.zip"

if (file.exists(zip_filename)) {
  file.remove(zip_filename)
}

cat("\n--- COMPILING COMPRESSED SYSTEM ARCHIVE VIA zip() ---\n")
# NOTE: utils::zip() shells out to an external `zip` binary. On systems
# without one on PATH (some minimal Windows installs) this fails silently
# or errors. The `zip` package (zip::zip()) is a pure-R, cross-platform
# alternative worth switching to if this runs outside your dev machine.
zip(zipfile = zip_filename, files = folder_vector)

cat("------------------------------------------------------------------------\n")
cat("Ecosystem compilation finalized. Master ZIP archive exported to:",
    file.path(getwd(), zip_filename), "\n")
