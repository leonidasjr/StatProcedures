# Function to identify significant numeric variables based on ANOVA and ANCOVA ####
find_significant_vars <- function(data, factor_var, covariate_vars = NULL, two_way = FALSE) {
  # Extract numeric variables (excluding factor and covariate variables)
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  numeric_vars <- setdiff(numeric_vars, c(factor_var, covariate_vars))
  
  # Initialize an empty list to store significant variables
  significant_vars <- list()
  
  # Loop through numeric variables
  for (var in numeric_vars) {
    if (is.null(covariate_vars)) {
      formula_str <- paste(var, "~", factor_var)
    } else {
      covariate_formula <- paste(covariate_vars, collapse = " + ")
      formula_str <- paste(var, "~", factor_var, "+", covariate_formula)
    }
    if (two_way) {
      formula_str <- paste(var, "~", factor_var, "*", covariate_formula)
    }
    model <- aov(as.formula(formula_str), data = data)
    p_value <- summary(model)[[1]][["Pr(>F)"]][[1]]
    
    if (p_value < 0.05) {
      significant_vars[[var]] <- p_value
      
      # Print ANOVA/ANCOVA summary
      cat("Variable:", var, "\n")
      print(summary(model))
      cat("\n")
      
      # Print significant variable's p-value
      cat("Significant numeric variable:", var, "\tP-value:", p_value, "\n")
      
      # Perform pairwise comparisons using Tukey's test (if applicable)
      number_of_factor_levels <- length(levels(data[[factor_var]]))
      if (number_of_factor_levels > 2) {
        tukey_result <- TukeyHSD(model, conf.level = 0.95)
        cat("TukeyHSD pairwise comparisons for", var, ":\n")
        print(tukey_result)
        cat("\n")
        cat('=================================')
        cat('\n')
      }
    }
  }
}

# Example usage:
# Replace 'your_data' with your actual dataset, 'YourFactorVar' with the appropriate factor variable,
# and provide a list of covariate variables (if applicable)
# Set 'two_way' to TRUE if you want to perform two-way ANOVA/ANCOVA
find_significant_vars(your_data, factor_var = "YourFactorVar", covariate_vars = c("Covariate1", "Covariate2"), two_way = FALSE)

# Function to identify significant numeric variables based on only ANOVA ####

# Custom function to identify significant numeric variables based on ANOVA
find_significant_vars <- function(data) {
  # Extract numeric variables (excluding response and factor variables)
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  # Initialize an empty list to store significant variables
  significant_vars <- list()
  
  # Loop through numeric variables
  for (var in numeric_vars) {
    formula_str <- paste(var, "~ AUDIOFILE")
    model <- aov(as.formula(formula_str), data = data)
    p_value <- summary(model)[[1]][["Pr(>F)"]][[1]]
    
    if (p_value < 0.05) {
      significant_vars[[var]] <- p_value
    }
  }
  
  # Print significant variables and their p-values
  cat("Significant numeric variables:\n")
  for (var in names(significant_vars)) {
    cat("Variable:", var, "\tP-value:", significant_vars[[var]], "\n")
  }
}

# Example usage:
# Replace 'your_data' with your actual dataset
find_significant_vars(your_data)

# Function to identify significant numeric variables based on Kruskall-Walis test ####

# Custom function to identify significant numeric variables based on Kruskall-Wallis
find_significant_vars <- function(data) {
  # Extract numeric variables (excluding response and factor variables)
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  # Initialize an empty list to store significant variables
  significant_vars <- list()
  
  # Loop through numeric variables
  for (var in numeric_vars) {
    formula_str <- paste(var, "~ T_OP")
    model <- kruskal.test(as.formula(formula_str), data = data)
    p_value <- model$p.value
    
    if (p_value < 0.05) {
      significant_vars[[var]] <- p_value
    }
  }
  
  # Print significant variables and their p-values
  cat("Significant numeric variables:\n")
  for (var in names(significant_vars)) {
    cat("Variable:", var, "\tP-value:", significant_vars[[var]], "\n")
  }
}

# Example used (data loading and preparation code)
# ds_teleop <- read.delim("C:/Users/leoni/OneDrive/Área de Trabalho/Artigo_Barbara/00_prosodic_features.txt", 
#                        stringsAsFactors=TRUE)
# str(ds_teleop)
# attach(ds_teleop)
#
# ds_teleop$T_OP
# ds_teleop$T_OP <- factor(ds_teleop$T_OP, levels = c("T_OPM01", "T_OPM02", "T_OPH01", "T_OPH02"))



# Function to identify significant numeric variables based on Mixed Effects Linear Models ####
library(lme4)
library(emmeans)
library(lmerTest)

# plots
library(tidyverse)

significant_mixed_models <- function(d_vars, fixed_effect, random_effect, data, alpha = 0.05) {
  results <- list()
  
  for (d_var in d_vars) {
    formula <- as.formula(paste(d_var, "~", fixed_effect, "+ (1|", random_effect, ")"))
    
    # Check if random effect exists in the data
    if (!random_effect %in% colnames(data)) {
      stop(paste("Random effect", random_effect, "not found in data."))
    }
    
    model <- try(lmer(formula, data = data), silent = TRUE)
    if (inherits(model, "try-error")) {
      cat("Error fitting model for", d_var, "\n")
      next
    }
    
    # Extract fixed effects and p-values
    fixed_effects <- summary(model)$coefficients
    p_values <- fixed_effects[, "Pr(>|t|)"]
    
    # Check for at least one significant fixed effect (excluding intercept)
    if (any(p_values[-1] < alpha)) {
      cat("\n", "===========", d_var, "===========", "\n")
      print(summary(model))
      
      # Perform post-hoc test
      cat("\n")
      cat("----- POST HOC -----\n")
      emmeans_results <- emmeans(model, pairwise ~ T_OP, adjust = "tukey")
      print(emmeans_results)
    }
  }
}

# Example used (data loading and preparation code)
# ds_teleop <- read.delim("C:/Users/leoni/OneDrive/Área de Trabalho/Artigo_Barbara/00_prosodic_features.txt", 
#                        stringsAsFactors=TRUE)
# str(ds_teleop)
# attach(ds_teleop)
#
# ds_teleop$T_OP
# ds_teleop$T_OP <- factor(ds_teleop$T_OP, levels = c("T_OPM01", "T_OPM02", "T_OPH01", "T_OPH02"))
#
# d_vars <- names(ds_teleop)[sapply(ds_teleop, is.numeric)]
# fixed_effect <- "T_OP"
# random_effect <- "CHUNK"
#
# Call the function
# significant_models <- significant_mixed_models(d_vars, fixed_effect, random_effect, ds_teleop)
#
# save data in a .txt file
sink("output_LMEM.txt")
significant_mixed_models(d_vars, fixed_effect, random_effect, ds_teleop)
sink()
