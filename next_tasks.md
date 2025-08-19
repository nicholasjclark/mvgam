# Comprehensive Joint Species Exploration Dashboard

## Implementation: Create explore_jsdm_data() function that generates an interactive HTML report

Features:
- Species accumulation curves and rarefaction analysis to assess sampling completeness
- Environmental gradient analysis with species turnover along gradients
- Preliminary ordination (PCA, NMDS) with environmental fitting
- Missing data patterns and sampling bias assessment
- Species co-occurrence matrix with significance testing
- Collinearity diagnostics for environmental predictors

## Why this matters: 
No existing JSDM package provides comprehensive pre-analysis data exploration. This would help users understand their data structure, identify potential issues, and make informed modeling decisions before fitting computationally expensive models.

---

# Interactive Ordination Implementation Plan

## Step 1: Core Interactive Visualization Infrastructure
**Goal**: Create basic interactive ordination plots with hover, zoom, and selection capabilities

**Key Packages**:
- `plotly` - Primary interactive plotting engine
- `htmlwidgets` - Web-based widget framework
- `shiny` - Reactive web application framework
- `DT` - Interactive data tables

**Essential Functions**:
- `plotly::plot_ly()` - Create base interactive plots
- `plotly::add_trace()` - Add multiple layers (sites, species, vectors)
- `plotly::layout()` - Configure axes, titles, and interaction modes
- `event_data()` - Capture user interactions (selection, hover, zoom)
- `renderPlotly()` / `plotlyOutput()` - Shiny integration

## Step 2: Dynamic State Capture and Management
**Goal**: Track user customizations (zoom, pan, selections) and maintain plot state

**Key Packages**:
- `shiny` - State management and reactivity
- `jsonlite` - Handle JavaScript-R data exchange
- `htmlwidgets::onRender()` - Custom JavaScript integration

**Essential Functions**:
- `observeEvent()` - React to user interactions
- `reactiveValues()` - Store dynamic plot state
- `input$plotId_relayout` - Capture zoom/pan changes
- `input$plotId_selected` - Capture point selections
- Custom JavaScript callbacks for advanced state tracking

## Step 3: High-Resolution Export with Current State
**Goal**: Generate publication-quality static plots that reflect user customizations

**Key Packages**:
- `ggplot2` - High-quality static plot generation
- `ggrepel` - Intelligent text labeling
- `gridExtra` / `patchwork` - Plot composition
- `grDevices` - Export control (PNG, PDF, SVG)

**Essential Functions**:
- `ggplot2::ggsave()` - High-resolution export
- `ggplot2::coord_cartesian()` - Apply zoom settings to static plots
- `ggplot2::geom_point()` / `geom_text_repel()` - Recreate interactive elements
- `downloadHandler()` - Shiny file downloads
- Custom state-to-ggplot conversion functions

## Success Metrics

- Interactive exploration requires no R coding knowledge
- Export captures exactly what user sees on screen
- High-resolution output suitable for publication
- Fast performance even with large datasets
- Intuitive interface that encourages exploration

---

# mvgam JSDM Enhancement Plan: 5-Step Implementation

## Overview
This plan outlines the implementation of ecological interpretation tools for mvgam's `jsdgam()` function, focusing on gradient analysis and beta diversity decomposition using factor loadings. The goal is to establish mvgam as the gold standard for JSDM analysis through user-friendly ecological interpretation tools.

## Build foundational functions to extract and process factor loadings and environmental gradients.

### Key Functions to Implement:
```r
# Extract factor information from fitted jsdgam models
extract_factor_data <- function(jsdgam_fit) {
  loadings <- get_factor_loadings(jsdgam_fit)    # species × factors
  scores <- get_factor_scores(jsdgam_fit)        # sites × factors  
  uncertainties <- get_loading_uncertainties(jsdgam_fit)
  
  return(list(loadings = loadings, scores = scores, uncertainties = uncertainties))
}

# Identify and standardize environmental gradients
extract_gradients <- function(jsdgam_fit, env_data = NULL, method = c("latent", "measured", "hybrid")) {
  factor_data <- extract_factor_data(jsdgam_fit)
  
  if(method == "latent") {
    gradients <- factor_data$scores
  } else if(method == "measured") {
    gradients <- standardize_environmental_data(env_data)
  } else {
    gradients <- combine_latent_measured_gradients(factor_data, env_data)
  }
  
  return(gradients)
}
```

### User Experience:
```r
# Simple extraction of gradient information
gradients <- extract_gradients(my_jsdm_fit, env_data = my_env_data, method = "hybrid")
```

## Step 2: Gradient Analysis Functions
**Goal**: Implement species turnover and community change analysis along environmental gradients.

### Key Functions to Implement:
```r
# Analyze community change along gradients
analyze_gradient_turnover <- function(jsdgam_fit, gradients, n_points = 50) {
  predictions <- predict_along_gradients(jsdgam_fit, gradients, n_points)
  
  turnover_metrics <- list(
    dissimilarity_matrix = calculate_community_dissimilarity(predictions),
    turnover_rate = calculate_turnover_rate(predictions, gradients),
    breakpoints = detect_community_breakpoints(predictions),
    species_optima = extract_species_optima(predictions, gradients)
  )
  
  return(turnover_metrics)
}

# Identify species assemblages using factor loadings
define_assemblages <- function(jsdgam_fit, threshold = 0.6) {
  loadings <- extract_factor_data(jsdgam_fit)$loadings
  
  assemblages <- cluster_species_by_loadings(loadings)
  indicators <- find_indicator_species(loadings, threshold)
  
  return(list(assemblages = assemblages, indicators = indicators))
}
```

### User Experience:
```r
# One-line gradient analysis
turnover_results <- analyze_gradient_turnover(my_jsdm_fit, gradients)
assemblages <- define_assemblages(my_jsdm_fit)
```

## Step 3: Beta Diversity Decomposition
**Goal**: Implement multi-scale beta diversity analysis with mechanistic component separation.

### Key Functions to Implement:
```r
# Multi-scale beta diversity decomposition
decompose_beta_diversity <- function(jsdgam_fit, scales = NULL, method = c("additive", "multiplicative")) {
  community_predictions <- get_community_predictions(jsdgam_fit)
  
  if(is.null(scales)) {
    # Single-scale analysis
    beta_components <- calculate_beta_components(community_predictions, method)
  } else {
    # Multi-scale hierarchical analysis
    beta_components <- calculate_hierarchical_beta(community_predictions, scales, method)
  }
  
  return(beta_components)
}

# Separate beta diversity into mechanistic components
partition_beta_mechanisms <- function(jsdgam_fit, env_data, spatial_coords = NULL) {
  factor_data <- extract_factor_data(jsdgam_fit)
  
  partitions <- list(
    environmental = calculate_environmental_beta(factor_data, env_data),
    spatial = if(!is.null(spatial_coords)) calculate_spatial_beta(factor_data, spatial_coords) else NULL,
    biotic = calculate_biotic_beta(factor_data),
    residual = calculate_residual_beta(factor_data)
  )
  
  return(partitions)
}
```

### User Experience:
```r
# Simple beta diversity analysis
beta_results <- decompose_beta_diversity(my_jsdm_fit, scales = c("plot", "site", "region"))
mechanisms <- partition_beta_mechanisms(my_jsdm_fit, env_data, spatial_coords)
```

## Step 4: Integrated Visualization Suite
**Goal**: Create publication-ready plots that automatically handle uncertainty and provide ecological insights.

### Key Functions to Implement:
```r
# Comprehensive gradient plotting
plot_gradient_analysis <- function(turnover_results, assemblages = NULL, type = c("turnover", "optima", "assemblages")) {
  
  plots <- list()
  
  if("turnover" %in% type) {
    plots$turnover <- create_turnover_heatmap(turnover_results$dissimilarity_matrix)
    plots$rates <- create_turnover_rate_plot(turnover_results$turnover_rate)
  }
  
  if("optima" %in% type) {
    plots$optima <- create_species_optima_plot(turnover_results$species_optima)
  }
  
  if("assemblages" %in% type && !is.null(assemblages)) {
    plots$assemblages <- create_assemblage_plot(assemblages)
  }
  
  return(combine_plots(plots))
}

# Beta diversity visualization suite  
plot_beta_diversity <- function(beta_results, mechanisms = NULL) {
  plots <- list(
    decomposition = create_beta_decomposition_plot(beta_results),
    mechanisms = if(!is.null(mechanisms)) create_mechanism_partition_plot(mechanisms) else NULL,
    distance_decay = create_distance_decay_plot(beta_results)
  )
  
  return(combine_plots(plots))
}
```

### User Experience:
```r
# Automatic publication-ready plots
plot_gradient_analysis(turnover_results, assemblages, type = c("turnover", "optima"))
plot_beta_diversity(beta_results, mechanisms)
```

## Step 5: Unified Workflow Function
**Goal**: Create a single, intuitive function that performs comprehensive ecological analysis.

### Master Function:
```r
# One-stop ecological interpretation function
interpret_jsdm <- function(jsdgam_fit, env_data = NULL, spatial_coords = NULL, 
                          analysis = c("gradients", "beta_diversity", "both"),
                          scales = NULL, plot = TRUE) {
  
  results <- list()
  
  # Extract gradients
  gradients <- extract_gradients(jsdgam_fit, env_data, method = "hybrid")
  
  if("gradients" %in% analysis || "both" %in% analysis) {
    # Gradient analysis
    results$gradients <- analyze_gradient_turnover(jsdgam_fit, gradients)
    results$assemblages <- define_assemblages(jsdgam_fit)
    
    if(plot) {
      results$gradient_plots <- plot_gradient_analysis(results$gradients, results$assemblages)
    }
  }
  
  if("beta_diversity" %in% analysis || "both" %in% analysis) {
    # Beta diversity analysis
    results$beta_diversity <- decompose_beta_diversity(jsdgam_fit, scales)
    results$mechanisms <- partition_beta_mechanisms(jsdgam_fit, env_data, spatial_coords)
    
    if(plot) {
      results$beta_plots <- plot_beta_diversity(results$beta_diversity, results$mechanisms)
    }
  }
  
  # Add summary interpretation
  results$summary <- generate_ecological_summary(results)
  
  class(results) <- "jsdm_interpretation"
  return(results)
}

# Print method for easy interpretation
print.jsdm_interpretation <- function(x) {
  cat("JSDM Ecological Interpretation Summary\n")
  cat("======================================\n\n")
  
  if(!is.null(x$gradients)) {
    cat("Gradient Analysis:\n")
    cat("- Major environmental axes:", length(x$gradients$species_optima), "\n")
    cat("- Community breakpoints detected:", length(x$gradients$breakpoints), "\n")
    cat("- Species assemblages identified:", length(unique(x$assemblages$assemblages)), "\n\n")
  }
  
  if(!is.null(x$beta_diversity)) {
    cat("Beta Diversity Analysis:\n")
    cat("- Total beta diversity:", round(x$beta_diversity$total_beta, 3), "\n")
    cat("- Turnover component:", round(x$beta_diversity$turnover_component, 3), "\n")
    cat("- Nestedness component:", round(x$beta_diversity$nestedness_component, 3), "\n\n")
  }
  
  if(!is.null(x$mechanisms)) {
    cat("Assembly Mechanisms:\n")
    cat("- Environmental filtering:", round(x$mechanisms$environmental$proportion, 2), "\n")
    cat("- Spatial processes:", round(x$mechanisms$spatial$proportion, 2), "\n")
    cat("- Biotic interactions:", round(x$mechanisms$biotic$proportion, 2), "\n\n")
  }
  
  cat(x$summary$interpretation)
}
```

### Ultimate User Experience:
```r
# Complete ecological analysis in one line
results <- interpret_jsdm(my_jsdm_fit, env_data = my_env_data, 
                         spatial_coords = my_coords, analysis = "both")

# Automatic summary and plots
print(results)  # Ecological summary
results$gradient_plots  # Publication-ready gradient figures
results$beta_plots      # Publication-ready beta diversity figures
```

## Technical Notes

- All functions should handle MCMC uncertainty through posterior sampling
- Design for scalability (large datasets, many species)

This plan positions mvgam as the first JSDM package to seamlessly integrate statistical modeling with ecological interpretation, leveraging the unique strengths of factor loadings for gradient analysis.
