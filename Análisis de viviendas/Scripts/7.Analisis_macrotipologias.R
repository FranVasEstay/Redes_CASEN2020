################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
###################### ANÁLISIS DE MACROTIPOLOGÍAS #############################
################################################################################

# LIBRERÍAS
library(igraph)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(vcd)
library(ggmosaic)
library(tidyr)
library(purrr)
library(tidyr)
library(forcats)
library(patchwork)
library(rstatix)
library(kableExtra)
# --- 1. LOAD DATA ---
load("Análisis de viviendas/Data/Data_con_tipología.RData")
macrotipos <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_macrogrupos.csv")

# Merge macrotipologies with main data
data_macro <- data_con_tipologia %>%
  left_join(macrotipos, by = "tipologia") %>%
  filter(!is.na(macrogrupo)) %>%
  mutate(
    macrogrupo = factor(macrogrupo),
    # Ensure categorical variables are factors
    region = factor(region),
    rural_cat = factor(rural_cat, levels = c("Urban", "Mixed", "Rural")),
    sueldo_cat = factor(sueldo_cat, levels = paste0("Q", 1:5), ordered = TRUE),
    # Create sex variable from porcent_hombre
    sex = ifelse(porc_hombre > 50, "Hombre", "Mujer")
  )

# 2. DESCRIPTIVE ANALYSIS -----------------------------------------------------
# 2.1 Frequency table of macrogroups
analyze_macrogroups <- function(data, suffix = "") {
  # 2.1 Frequency table
  macro_freq <- data %>%
    distinct(household, .keep_all = TRUE) %>%
    count(macrogrupo) %>%
    mutate(percent = n/sum(n)*100) %>%
    arrange(desc(n))
  
  # 2.2 Demographic characteristics
  macro_demographics <- data %>%
    group_by(macrogrupo) %>%
    summarise(
      avg_age = mean(edad.prom, na.rm = TRUE),
      prop_female = 100 - mean(porc_hombre, na.rm = TRUE), # Using porc_hombre
      avg_income = median(sueldo, na.rm = TRUE),
      prop_indigenous = mean(porc_ind, na.rm = TRUE),
      prop_employed = mean(porc_empleo, na.rm = TRUE),
      prop_chilean = mean(porc_chi, na.rm = TRUE),
      avg_hh_size = n()/n_distinct(household),
      .groups = "drop"
    )
  
  
  # 2.3 Visualizations
  
  # Plot 1: Macrogroup distribution
  p1 <- ggplot(macro_freq, aes(x = reorder(macrogrupo, -n), y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(round(percent,1), "%")), 
              vjust = -0.5) +
    labs(title = paste("Household Macrogroup Distribution", suffix),
         x = "", y = "Number of Households") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 2: Income distribution by macrogroup
  p2 <- ggplot(data, aes(x = sueldo_cat, fill = macrogrupo)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Income Distribution by Macrogroup", suffix),
         x = "Income Quintile", y = "Percentage") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  
  # Plot 3: Indigenous percentage by macrogroup
  p3 <- data %>%
    group_by(macrogrupo) %>%
    summarise(mean_porc_ind = mean(porc_ind, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(macrogrupo, -mean_porc_ind), y = mean_porc_ind)) +
    geom_col(fill = "darkgreen") +
    labs(title = paste("Average Indigenous Percentage by Macrogroup", suffix),
         x = "", y = "Percentage Indigenous") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    frequency = macro_freq,
    demographics = macro_demographics,
    plots = list(distribution = p1, income = p2, indigenous = p3)
  ))
}
# Analyze both datasets
results_t48 <- analyze_macrogroups(data_macro, "(Tipologies T1-T48)")

# View results
results_t48$frequency

# Combine and view plots
results_t48$plots$distribution
results_t48$plots$income
results_t48$plots$indigenous

# 3. CHI-SQUARE TESTS ---------------------------------------------------------
# Function for typology-wise chi-square tests
chi_test_by_tipology <- function(data, test_var) {
  # Create all combinations of typologies
  typology_pairs <- combn(levels(data$tipologia), 2, simplify = FALSE)
  
  results <- map_dfr(typology_pairs, function(pair) {
    # Filter data for the two typologies being compared
    pair_data <- data %>% filter(tipologia %in% pair)
    
    # Create contingency table
    tbl <- table(pair_data$tipologia, pair_data[[test_var]])
    
    # Skip if any expected counts <5
    if(any(chisq.test(tbl)$expected < 5)) return(NULL)
    
    # Perform chi-square test
    chi_test <- chisq.test(tbl)
    
    # Calculate standardized residuals
    std_res <- as_tibble(chi_test$stdres, rownames = "tipologia") %>%
      pivot_longer(-tipologia, names_to = test_var, values_to = "std_residual")
  }
    # Return results
    tibble(
      typology1 = pair[1],
      typology2 = pair[2],
      variable = test_var,
      chi_sq = chi_test$statistic,
      df = chi_test$parameter,
      p_value = chi_test$p.value,
      cramer_v = sqrt(chi_test$statistic/(sum(tbl)*(min(dim(tbl))-1)),
                      residuals = list(std_res)
      ))
      )
    
    return(results)
    }

# Run tests for key variables
chi_results <- map_dfr(
  c("region", "rural_cat", "sueldo_cat"),
  ~chi_test_by_tipology(data_con_tipologia, .x)
)

# 3. GROUPED MOSAIC PLOTS (6 groups of 8 typologies) -------------------------

# Create grouping variable (6 groups of 8 typologies)
data_grouped <- data_con_tipologia %>%
  mutate(
    typo_group = as.numeric(tipologia) %/% 8 + 1,
    typo_group = factor(paste("Group", typo_group))
    
    # Function to create grouped mosaic plots
    create_grouped_mosaic <- function(data, x_var) {
      ggplot(data) +
        geom_mosaic(aes(x = product(.data[[x_var]], tipologia), fill = tipologia)) +
        labs(title = paste("Typology Distribution by", x_var),
             x = x_var, y = "Typology Group") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        facet_grid(typo_group~., scales = "free_y", space = "free")
    }
    
    # Create and save mosaic plots
    mosaic_plots <- map(
      c("region", "rural_cat", "sueldo_cat"),
      ~create_grouped_mosaic(data_grouped, .x)
    )
    
    # Name and save plots
    plot_names <- c("region", "rural", "income")
    walk2(mosaic_plots, plot_names, ~ggsave(
      paste0("Results/mosaic_", .y, ".png"), .x, width = 12, height = 10
    ))
    
    # 4. RESIDUAL ANALYSIS -------------------------------------------------------
    
    # Extract and plot significant residuals (p<0.05)
    significant_residuals <- chi_results %>%
      filter(p_value < 0.05) %>%
      select(typology1, typology2, variable, residuals) %>%
      unnest(residuals)
    
    # Plot significant residuals
    ggplot(significant_residuals, 
           aes(x = .data[[unique(significant_residuals$variable)]], 
               y = interaction(typology1, typology2, sep = " vs "),
               fill = std_residual)) +
      geom_tile() +
      geom_text(aes(label = round(std_residual, 1)), size = 3) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                           midpoint = 0, limits = c(-4, 4)) +
      labs(title = "Significant Standardized Residuals (p<0.05)",
           x = "Variable Category", 
           y = "Typology Comparison",
           fill = "Std. Residual") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # 5. SAVE RESULTS ------------------------------------------------------------
    
    # Save chi-square results
    write_csv(chi_results, "Results/chi_square_results_by_typology.csv")
    
    # Save residual plot
    ggsave("Results/significant_residuals.png", width = 12, height = 8)

# 4. MULTIPLE CORRESPONDENCE ANALYSIS (MCA) -----------------------------------

# Prepare data for MCA
mca_data <- data_macro %>%
  select(macrogrupo, region, rural_cat, income_q) %>%
  mutate(across(everything(), as.factor)) %>%
  na.omit()

# Run MCA with macrogroup as supplementary variable
mca_result <- MCA(mca_data, quali.sup = 1, graph = FALSE)

# 4.1 Visualize variable categories
fviz_mca_var(mca_result, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

# 4.2 Visualize macrogroups
fviz_mca_ind(mca_result,
             habillage = mca_data$macrogrupo,
             addEllipses = TRUE,
             palette = "Set2",
             ggtheme = theme_minimal())

# 4.3 Dimension description
dim_desc <- dimdesc(mca_result, axes = 1:2)
print(dim_desc)

# 5. SAVE RESULTS ------------------------------------------------------------

# Create output directory
dir.create("Results/Macrotipology_Analysis", showWarnings = FALSE)

# Save statistical results
save(chi_results, file = "Results/Macrotipology_Analysis/chi_results.RData")
save(mca_result, file = "Results/Macrotipology_Analysis/mca_results.RData")

# Save tables
write.csv(macro_freq, "Results/Macrotipology_Analysis/macrogroup_frequencies.csv")
write.csv(macro_demographics, "Results/Macrotipology_Analysis/demographics_by_macrogroup.csv")

# Save plots
ggsave("Results/Macrotipology_Analysis/macrogroup_distribution.png", p1, width = 8)
ggsave("Results/Macrotipology_Analysis/income_distribution.png", p2, width = 8)
ggsave("Results/Macrotipology_Analysis/residual_plots.png", wrap_plots(residual_plots), width = 10, height = 12)