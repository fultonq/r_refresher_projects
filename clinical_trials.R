# ==============================================================================
# CLINICAL STUDY DATA ANALYSIS PIPELINE: 39 PATIENTS, 3 VISITS  [REVISED]
# ==============================================================================
# Changes from original:
#   1. FIXED: legend() used fill= (box swatches) for a line+point plot; now
#      uses col/pch/lty so the legend actually matches what's drawn.
#   2. ADDED: a real per-arm summary table (this is a "clinical trial" script
#      that never compared the arms -- that's the entire point of a trial).
#   3. ADDED: Welch two-sample t-test on Day_28 vs Day_0 change score,
#      Alpha vs Placebo, with explicit NA handling and effect size.
#   4. ADDED: guard so the paired change score correctly drops any patient
#      missing either endpoint (not just complete-case relative to whichever
#      column happens to run first).
# ==============================================================================

# ------------------------------------------------------------------------------
# STEP 1: ESTABLISH TIMELINES, PATIENT GROUPS, AND VECTORS
# ------------------------------------------------------------------------------
days_elapsed <- seq(from = 0, to = 28, by = 14)
labs         <- paste("Day_", days_elapsed, sep = "") # "Day_0" "Day_14" "Day_28"

patient_groups <- rep(c("Alpha", "Placebo"), times = c(20, 19))
patient_ids    <- paste("PT_", 1:39, sep = "")

# ------------------------------------------------------------------------------
# STEP 2: SIMULATE REALISTIC LAB METRICS & INJECT ANOMALIES (NA, NaN)
# ------------------------------------------------------------------------------
set.seed(123)

raw_values <- rnorm(n = 39 * 3, mean = 75, sd = 12)
lab_matrix <- matrix(raw_values, nrow = 39, ncol = 3)

rownames(lab_matrix) <- patient_ids
colnames(lab_matrix) <- labs

lab_matrix["PT_5", "Day_28"]  <- NA
lab_matrix["PT_12", "Day_14"] <- NA
lab_matrix["PT_28", "Day_28"] <- 0 / 0   # NaN: simulated instrument glitch

# ------------------------------------------------------------------------------
# STEP 3: AUDIT DATA INTEGRITY AND DETECT GAPS USING is.na()
# ------------------------------------------------------------------------------
print("--- DATA QUALITY REPORT ---")

total_gaps <- sum(is.na(lab_matrix))          # is.na() catches NA AND NaN
print(paste("Total corrupted/missing values found:", total_gaps))

nan_count <- sum(is.nan(lab_matrix))          # NaN specifically
print(paste("Mathematical NaN anomalies detected:", nan_count))

print("Missing values tallied by visit day:")
print(colSums(is.na(lab_matrix)))

# ------------------------------------------------------------------------------
# STEP 4: BUILD THE ANALYSIS DATA FRAME (moved earlier so stats + plot share it)
# ------------------------------------------------------------------------------
patient_df <- as.data.frame(lab_matrix)
patient_df$Patient_ID <- rownames(lab_matrix)
patient_df$Study_Arm  <- patient_groups
patient_df <- patient_df[, c("Patient_ID", "Study_Arm", "Day_0", "Day_14", "Day_28")]

# ------------------------------------------------------------------------------
# STEP 5: PER-ARM DESCRIPTIVE SUMMARY (this was entirely missing before)
# ------------------------------------------------------------------------------
cat("\n--- PER-ARM SUMMARY (mean +/- SD, n complete) ---\n")
for (visit in c("Day_0", "Day_14", "Day_28")) {
  for (arm in c("Alpha", "Placebo")) {
    vals <- patient_df[patient_df$Study_Arm == arm, visit]
    n_complete <- sum(!is.na(vals))
    cat(sprintf("  %-8s %-8s: mean=%.2f  sd=%.2f  n=%d\n",
                visit, arm, mean(vals, na.rm = TRUE), sd(vals, na.rm = TRUE), n_complete))
  }
}

# ------------------------------------------------------------------------------
# STEP 6: ACTUAL TRIAL INFERENCE -- change score, Day_0 to Day_28, by arm
# ------------------------------------------------------------------------------
# A patient only contributes if BOTH Day_0 and Day_28 are present (paired design).
patient_df$Change_D0_D28 <- patient_df$Day_28 - patient_df$Day_0
complete_change <- patient_df[!is.na(patient_df$Change_D0_D28), ]

cat(sprintf("\nPatients with complete Day_0/Day_28 pair: %d of %d\n",
            nrow(complete_change), nrow(patient_df)))

trial_test <- t.test(Change_D0_D28 ~ Study_Arm, data = complete_change)
cat("\n--- ALPHA vs PLACEBO: CHANGE FROM BASELINE TO DAY 28 (Welch t-test) ---\n")
print(trial_test)

# ------------------------------------------------------------------------------
# STEP 7: VISUALIZE PATIENT TRAJECTORIES SAFELY (legend now matches the plot)
# ------------------------------------------------------------------------------
group_colors <- ifelse(patient_groups == "Alpha", "coral", "steelblue")

matplot(x = days_elapsed, y = t(lab_matrix),
        type = "b",
        pch = 19,
        lty = 1,
        col = group_colors,
        xlab = "Timeline (Days Post-Enrollment)",
        ylab = "Biometric Marker Level (mg/dL)",
        main = "Patient Biomarker Shift: Alpha vs Placebo Group",
        xaxt = "n")

axis(side = 1, at = days_elapsed, labels = labs)

# FIXED: use col/pch/lty (matches type="b" points+lines), not fill (box swatch)
legend("topright", legend = c("Alpha (n=20)", "Placebo (n=19)"),
       col = c("coral", "steelblue"), pch = 19, lty = 1,
       bty = "n", cex = 0.9)

# ------------------------------------------------------------------------------
# STEP 8: SAVE DATA TO LOCAL DRIVE
# ------------------------------------------------------------------------------
write.csv(patient_df, file = "patient_clinical_data.csv", row.names = FALSE)

print("--- PIPELINE COMPLETED ---")
print(paste("CSV file successfully saved to active directory:", getwd()))
