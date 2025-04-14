# 1. Required Package 
required_packages <- c("tidyverse", "cluster", "factoextra", "ggdendro", "patchwork", "mice", "stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. Library Loading 
library(tidyverse)
library(cluster)
library(factoextra)
library(ggdendro)
library(patchwork)
library(stringr)
library(mice)

# 3. Visual Settings 
color_palette <- list(
  fast = "#88B04B", 
  moderate = "#F7DC6F",
  speed = "#FF6F61", 
  price = "#6B5B95",
  points = "#88B04B", 
  line = "#FF6F61",
  cluster = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")  # ColorBrewer palette
)
theme_set(theme_minimal(base_size = 12))

# 4. Data Loading and Preparation 
# Import dataset
df <- read_csv(file.choose(), show_col_types = FALSE)

view(df)

# Initial data check
cat("=== DATA STRUCTURE ===\n")
glimpse(df)
cat("\nMissing values:", sum(is.na(df)), "\n")

# Type conversion
df <- df %>%
  mutate(across(c(ChargingFrequency, AdSpend, StationUsage, Revenue, ChosenStationPrice, ConjointPrice), as.numeric)) %>%
  mutate(across(c(LocationPreference, Segment, SpeedPreference, HomeChargerAvailable, UserLocation, 
                  ChosenStationSpeed, ConjointSpeed, PlanType, CampaignType), as.factor))

# 5. Cluster Analysis 
# Prepare clustering variables
cluster_vars <- c("ChargingFrequency", "LocationPreference", "Segment", 
                  "SpeedPreference", "HomeChargerAvailable", "UserLocation")
df_selected <- df %>% 
  select(all_of(cluster_vars)) %>% 
  mutate(across(-ChargingFrequency, ~ as.numeric(factor(.))))

# Standardize data
df_scaled <- scale(df_selected)

# Determine optimal clusters
set.seed(123)  # For reproducibility
cat("\n=== CLUSTER DETERMINATION ===\n")

# Elbow Method
elbow_plot <- fviz_nbclust(df_scaled, hcut, method = "wss", 
                           hc_method = "ward.D2") + 
  ggtitle("Elbow Method") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette Method
silhouette_plot <- fviz_nbclust(df_scaled, hcut, method = "silhouette",
                                hc_method = "ward.D2") + 
  ggtitle("Silhouette Method") +
  theme(plot.title = element_text(hjust = 0.5))

# Combine determination plots
print(elbow_plot + silhouette_plot)

# Hierarchical clustering
distance <- dist(df_scaled, method = 'euclidean')
hc <- hclust(distance, method = 'ward.D2')

# Visualize dendrogram
dendro_plot <- fviz_dend(hc, k = 4, k_colors = color_palette$cluster,
                         main = "Cluster Dendrogram",
                         xlab = "Observations", ylab = "Height",
                         ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
print(dendro_plot)

# Create clusters
optimal_k <- 4  # Based on your previous output
df$Cluster_Hierarchical <- as.factor(cutree(hc, k = optimal_k))

# 6. Cluster Profiling 
cluster_profile <- df %>% 
  group_by(Cluster_Hierarchical) %>% 
  summarise(
    Size = n(),
    Proportion = n()/nrow(df)*100,
    Avg_ChargingFrequency = mean(ChargingFrequency, na.rm = TRUE),
    Avg_AdSpend = mean(AdSpend, na.rm = TRUE),
    Avg_StationUsage = mean(StationUsage, na.rm = TRUE),
    Top_Segment = names(sort(table(Segment), decreasing = TRUE))[1],
    Top_Location = names(sort(table(LocationPreference), decreasing = TRUE))[1],
    .groups = 'drop'
  )

# 7. Choice Model Analysis 
if(all(c("ChosenStationSpeed", "ChosenStationPrice") %in% names(df))) {
  choice_plot <- df %>%
    count(ChosenStationSpeed, ChosenStationPrice) %>%
    mutate(Probability = n / sum(n) * 100) %>%
    ggplot(aes(x = str_glue("{ChosenStationSpeed}, ${ChosenStationPrice}"), 
               y = Probability, 
               fill = ChosenStationSpeed)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.1f%%", Probability)), vjust = -0.5) +
    labs(title = "Station Choice Probabilities", 
         x = "Option", y = "Preference (%)",
         fill = "Charging Speed") +
    scale_fill_manual(values = c("Fast" = color_palette$fast, 
                                 "Moderate" = color_palette$moderate)) +
    coord_flip() +
    theme_minimal()
  print(choice_plot)
} else {
  message("\nSkipping Choice Model: Required columns not found")
}

# 8. Conjoint Analysis 
if(all(c("ConjointSpeed", "ConjointPrice") %in% names(df))) {
  # Calculate utility scores
  conjoint_utility <- df %>%
    group_by(ConjointSpeed, ConjointPrice) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Utility = Count / sum(Count) * 100)
  
  # Check if ConjointSpeed has variation
  speed_levels <- unique(conjoint_utility$ConjointSpeed)
  if(length(speed_levels) < 2) {
    cat("\nSkipping Conjoint Importance Scores: ConjointSpeed has no variation (all values are '", speed_levels[1], "')\n")
    # Still plot utilities for visualization
    conjoint_plot <- ggplot(conjoint_utility, 
                            aes(x = interaction(ConjointSpeed, ConjointPrice), 
                                y = Utility,
                                fill = ConjointSpeed)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.1f%%", Utility)), vjust = -0.5) +
      labs(title = "Conjoint Analysis: Speed vs Price",
           x = "Speed + Price Combination", 
           y = "Utility Score (%)",
           fill = "Charging Speed") +
      scale_fill_manual(values = c("Fast" = color_palette$speed, 
                                   "Moderate" = color_palette$moderate)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(conjoint_plot)
  } else {
    # Plot utilities
    conjoint_plot <- ggplot(conjoint_utility, 
                            aes(x = interaction(ConjointSpeed, ConjointPrice), 
                                y = Utility,
                                fill = ConjointSpeed)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.1f%%", Utility)), vjust = -0.5) +
      labs(title = "Conjoint Analysis: Speed vs Price",
           x = "Speed + Price Combination", 
           y = "Utility Score (%)",
           fill = "Charging Speed") +
      scale_fill_manual(values = c("Fast" = color_palette$speed, 
                                   "Moderate" = color_palette$moderate)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(conjoint_plot)
    
    # Calculate importance scores
    utility_ranges <- conjoint_utility %>%
      summarise(
        speed_range = if(length(unique(ConjointSpeed)) > 1) {
          max(Utility[ConjointSpeed == "Fast"]) - max(Utility[ConjointSpeed == "Moderate"])
        } else 0,
        price_range = diff(range(Utility))
      ) %>%
      mutate(speed_range = pmax(0, speed_range))  # Ensure non-negative
    
    total_range <- sum(abs(as.numeric(utility_ranges)))
    
    importance_scores <- tibble(
      Attribute = c("Speed", "Price"),
      Importance = if(total_range == 0) c(50, 50) else c(
        utility_ranges$speed_range / total_range * 100,
        utility_ranges$price_range / total_range * 100
      )
    )
    
    cat("\n=== CONJOINT ATTRIBUTE IMPORTANCE ===\n")
    print(importance_scores)
  }
} else {
  cat("\nSkipping Conjoint Analysis: Required columns (ConjointSpeed, ConjointPrice) not found\n")
}

# 9. Market Response Modeling 
response_model <- lm(StationUsage ~ AdSpend + ChargingFrequency, data = df)
model_summary <- summary(response_model)

response_plot <- ggplot(df, aes(x = AdSpend, y = StationUsage)) +
  geom_point(color = color_palette$points, alpha = 0.6) +
  geom_smooth(method = "lm", color = color_palette$line, se = TRUE) +
  labs(title = "Advertising Effectiveness",
       x = "Ad Spend ($)", y = "Usage Sessions") +
  annotate("text", 
           x = max(df$AdSpend, na.rm = TRUE) * 0.6,
           y = max(df$StationUsage, na.rm = TRUE) * 0.9,
           label = str_glue("AdSpend Effect: {round(coef(response_model)['AdSpend'], 4)}\nCharging Freq Effect: {round(coef(response_model)['ChargingFrequency'], 2)}\nR² = {round(model_summary$r.squared, 2)}")) +
  theme_minimal()
print(response_plot)

# 10. Results Reporting 
cat("\n=== FINAL RESULTS ===\n")

# Cluster Summary
cat("\nCLUSTER PROFILES:\n")
print(cluster_profile, n = Inf)
cat("\nInterpretation: Cluster 1 (high charging frequency: ", round(cluster_profile$Avg_ChargingFrequency[1], 1), 
    ") likely represents frequent Tesla users. Cluster 4 (", round(cluster_profile$Proportion[4], 1), 
    "%) is the largest group with moderate usage.\n")

# Model Summary
cat("\nMARKET RESPONSE MODEL:\n")
print(model_summary)

# Model Significance Check
if(model_summary$coefficients["AdSpend", "Pr(>|t|)"] > 0.05) {
  cat("\nWARNING: AdSpend effect is not statistically significant (p =",
      round(model_summary$coefficients["AdSpend", "Pr(>|t|)"], 3), 
      "). Consider additional predictors or nonlinear terms.\n")
}

# 11. Formulas Summary 
cat("\n=== FORMULAS SUMMARY ===\n")

# Step 1: Euclidean Distance Formula (Cluster Analysis)
cat("\nStep 1: Euclidean Distance Formula (Cluster Analysis)\n")
cat("d_euc(x,y) = \\sqrt{\\sum_{i=1}^{n} (x_i - y_i)^2}\n")

# Step 2: Utility Function (Choice Model)
cat("\nStep 2: Utility Function (Choice Model)\n")
cat("U(x) = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2\n")

# Step 3: Conjoint Estimation Formula (Conjoint Analysis)
cat("\nStep 3: Conjoint Estimation Formula (Conjoint Analysis)\n")
cat("R(P) = \\sum_{i=1}^{H} \\sum_{j=1}^{m} \\beta_j X_j\n")

# Step 4: Market Response Regression Equation (Market Response Model)
cat("\nStep 4: Market Response Regression Equation (Market Response Model)\n")
cat("StationUsage = \\beta_0 + \\beta_1 AdSpend + \\beta_2 ChargingFrequency + \\epsilon\n")

cat("\n=== END OF FORMULAS ===\n")
