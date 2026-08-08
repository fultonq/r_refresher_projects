# ==============================================================================
# ADVANCED 6-MONTH SUPERMARKET PROJECTIONS & COHORT DRIFT ANALYTICS  [REVISED]
# ==============================================================================
# Changes from original:
#   1. FIXED (analytic soundness): the original ranked items by raw
#      var(Grand_Total). Every item in the original data follows the exact
#      same inflation curve (1.015^month) with no item-specific randomness,
#      so mean and sd of each item's series both scale linearly with that
#      item's Quantity*Price. Variance scales with the SQUARE of that
#      magnitude -> the "highest variance" item is *guaranteed* to be
#      whichever item has the largest baseline $ magnitude (Frozen_Pizza /
#      Chocolate_Cookies, qty=12), regardless of real volatility. There was
#      no genuine drift differential in the dataset for the analysis to find.
#   2. ADDED: small, reproducible item-specific monthly demand noise, so
#      there is now real differential volatility across items to detect.
#   3. FIXED: ranking now uses Coefficient of Variation (sd/mean), which is
#      scale-invariant, instead of raw variance.
#   4. FIXED: scale_color_manual() used an unnamed vector relying on
#      alphabetical factor-level order lining up by luck. Now named
#      explicitly so a future category rename/reorder can't silently
#      mis-color the plot.
# ==============================================================================

library(ggplot2)

# ------------------------------------------------------------------------------
# STEP 1: INITIALIZE STRUCTURAL VECTOR DATA STACK VIA NAMES()
# ------------------------------------------------------------------------------
base_quantities <- c(3, 1, 5, 4, 1, 2, 1, 12, 1, 2, 3, 2, 12, 2, 4, 1, 2, 1, 2, 3, 2, 1)

names(base_quantities) <- c(
  # --- Category: Essential Food (Tax-Exempt) ---
  "Honeycrisp_Apples", "Baby_Spinach", "Yellow_Bananas", "Roma_Tomatoes",
  "Whole_Milk_1G", "Greek_Yogurt", "Unsalted_Butter", "Large_White_Eggs",
  "Whole_Wheat_Bread", "Jasmine_Rice", "Organic_Olive_Oil",

  # --- Category: Taxable Food (Prepared/Snacks) ---
  "Frozen_Pizza", "Potato_Chips", "Chocolate_Cookies", "Diet_Cola_Pack",

  # --- Category: Taxable Non-Food (Household) ---
  "Toilet_Paper", "Paper_Towels", "Dish_Soap", "Liquid_Detergent",

  # --- Category: Pharmacy & Hygiene ---
  "Advil_Liquid_Gels", "Whitening_Toothpaste", "Moisturizing_Body_Wash"
)

base_categories <- rep(
  c("Food-Essential", "Food-Taxable", "NonFood-Taxable", "Pharmacy-Hygiene"),
  times = c(11, 4, 4, 3)
)

set.seed(88)
base_prices <- round(runif(length(base_quantities), min = 1.99, max = 18.50), 2)
names(base_prices) <- names(base_quantities)

base_prices["Baby_Spinach"]  <- 2.49
base_prices["Roma_Tomatoes"] <- 3.15

# ------------------------------------------------------------------------------
# STEP 2: EXPAND TIMELINE TO 6 MONTHS USING REP() & APPLY INFLATION
# ------------------------------------------------------------------------------
months_forecast  <- 6
items_per_trip   <- length(base_quantities) # 22
repeated_indices <- rep(1:items_per_trip, times = months_forecast)

forecast_df <- data.frame(
  Month        = rep(paste0("Month_", 1:months_forecast), each = items_per_trip),
  Item_Name    = names(base_quantities)[repeated_indices],
  Tax_Category = base_categories[repeated_indices],
  Quantity     = base_quantities[repeated_indices],
  Base_Price   = base_prices[repeated_indices],
  stringsAsFactors = FALSE
)

forecast_df$Month <- factor(forecast_df$Month, levels = paste0("Month_", 1:months_forecast))

monthly_inflation_rates <- 1.015 ^ (0:(months_forecast - 1))
inflation_vector        <- rep(monthly_inflation_rates, each = items_per_trip)

forecast_df$Inflated_Price <- round(forecast_df$Base_Price * inflation_vector, 2)
forecast_df$Subtotal       <- forecast_df$Quantity * forecast_df$Inflated_Price

tax_rates <- ifelse(forecast_df$Tax_Category == "Food-Essential", 0.00,
             ifelse(forecast_df$Tax_Category == "Food-Taxable", 0.02, 0.06))

forecast_df$Tax_Applied <- round(forecast_df$Subtotal * tax_rates, 2)
forecast_df$Grand_Total <- forecast_df$Subtotal + forecast_df$Tax_Applied

# ------------------------------------------------------------------------------
# STEP 2b: NEW -- inject item-specific monthly demand noise
# ------------------------------------------------------------------------------
# Without this, every item's series is a pure deterministic scalar multiple of
# the same inflation curve, so there is nothing for a variance/CV analysis to
# actually differentiate. Each item gets its own noise sd (2%-12%), simulating
# real demand fluctuation (e.g. perishables swing more than paper goods).
set.seed(2026)
item_noise_sd <- setNames(
  round(runif(items_per_trip, min = 0.02, max = 0.12), 3),
  names(base_quantities)
)
noise_factor <- 1 + rnorm(nrow(forecast_df), mean = 0,
                           sd = item_noise_sd[forecast_df$Item_Name])
forecast_df$Grand_Total <- round(forecast_df$Grand_Total * noise_factor, 2)

# ------------------------------------------------------------------------------
# STEP 3: ANALYTICS - ISOLATE VARIANCE DRIFT & HIGHLIGHT PEAK DRIFT (FIXED)
# ------------------------------------------------------------------------------
item_stats <- aggregate(Grand_Total ~ Item_Name, data = forecast_df,
                         FUN = function(x) c(mean = mean(x), sd = sd(x)))
item_stats <- do.call(data.frame, item_stats)
names(item_stats) <- c("Item_Name", "Mean_Total", "SD_Total")

# Coefficient of Variation: scale-invariant, so a $2 item and a $40 item with
# the same *relative* swing rank equally -- unlike raw variance.
item_stats$CV_Pct <- round(100 * item_stats$SD_Total / item_stats$Mean_Total, 2)

highest_cv_item <- item_stats$Item_Name[which.max(item_stats$CV_Pct)]

print("--- VOLATILITY ANALYSIS TARGET REPORT (Coefficient of Variation) ---")
print(item_stats[order(-item_stats$CV_Pct), c("Item_Name", "Mean_Total", "CV_Pct")])
cat(sprintf("\nAsset exhibiting peak RELATIVE volatility over timeline: %s (CV = %.2f%%)\n",
            highest_cv_item, max(item_stats$CV_Pct)))

forecast_df$Is_Volatile_Target <- forecast_df$Item_Name == highest_cv_item

# ------------------------------------------------------------------------------
# STEP 4: ANALYTICS - COMPUTE CHRONOLOGICAL ROLLING CUMULATIVE SUM
# ------------------------------------------------------------------------------
monthly_ledger_sums <- aggregate(Grand_Total ~ Month, data = forecast_df, FUN = sum)
monthly_ledger_sums$Cumulative_Drain <- cumsum(monthly_ledger_sums$Grand_Total)

# ------------------------------------------------------------------------------
# STEP 5: VISUALIZATION - GENERATE FACETED CATEGORY TRACKS
# ------------------------------------------------------------------------------
category_trends <- aggregate(Grand_Total ~ Tax_Category + Month, data = forecast_df, FUN = sum)
category_trends$Month <- factor(category_trends$Month, levels = paste0("Month_", 1:months_forecast))

# FIXED: named vector -- explicit mapping, immune to level-order changes
category_colors <- c(
  "Food-Essential"   = "coral",
  "Food-Taxable"     = "steelblue",
  "NonFood-Taxable"  = "goldenrod",
  "Pharmacy-Hygiene" = "purple4"
)

faceted_line_plot <- ggplot(category_trends, aes(x = Month, y = Grand_Total, group = Tax_Category, color = Tax_Category)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  facet_wrap(~ Tax_Category, nrow = 1) +
  scale_color_manual(values = category_colors) +
  labs(title = "Independent Expenditure Drift Analysis Across Fiscal Segments",
       subtitle = "Faceted tracking layout monitoring categorical trends over a 6-month window",
       x = "Timeline Period Sequence",
       y = "Gross Transaction Cost ($)",
       color = "Tax Partition Tier") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(faceted_line_plot)

# ------------------------------------------------------------------------------
# STEP 6: VISUALIZATION - PLOT LIQUID CAPITAL RUNNING BALANCE DRAIN
# ------------------------------------------------------------------------------
cumulative_drain_plot <- ggplot(monthly_ledger_sums, aes(x = Month, y = Cumulative_Drain, group = 1)) +
  geom_area(fill = "darkred", alpha = 0.15) +
  geom_line(color = "darkred", linewidth = 1.5, linetype = "dashed") +
  geom_point(color = "black", size = 3.5) +
  geom_label(aes(label = sprintf("$%.2f", Cumulative_Drain)), vjust = -0.5, fontface = "bold", size = 3) +
  labs(title = "Cumulative Capital Drain Velocity Summary",
       subtitle = "Aggregated historical budget depletion over time",
       x = "Forecasting Evaluation Intervals",
       y = "Total Invested Capital Outflow ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))

print(cumulative_drain_plot)

# ------------------------------------------------------------------------------
# STEP 7: EXPORT SYSTEM ASSETS TO LOCAL STORAGE FILE DIRECTORY
# ------------------------------------------------------------------------------
write.csv(forecast_df, file = "six_month_supermarket_complete_analytics.csv", row.names = FALSE)

ggsave(filename = "faceted_category_trends.png", plot = faceted_line_plot, width = 11, height = 5, dpi = 300)
ggsave(filename = "cumulative_capital_drain.png", plot = cumulative_drain_plot, width = 8, height = 5, dpi = 300)

print("------------------------------------------------------------------------")
print(paste("Pipeline Execution Finalized. Output assets saved inside directory:", getwd()))
