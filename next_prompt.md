/feature-execute stancode-generation-update. 

We need to ensure we have an extremely robust and future-proof way to achieve the following:

  # 1. Generate brms trend model
  trend_setup <- setup_brms_lightweight(trend_formula, data)
  # This creates: trend linear predictors, trend parameters, trend transformations

  # 2. Extract trend components that need to be integrated (apart from the observation likelihood)
  # - Linear predictor calculations
  # - Parameter transformations 
  # - Prior specifications
  # - Model block computations
  # - Generated quantities

  # 3. Rename everything to avoid conflicts with observation model
  # obs model has: b, sigma, mu
  # trend model has: b, sigma, mu  
  # After renaming: b_trend, sigma_trend, mu_trend

  # 4. Inject renamed trend components and trend dynamic stanvars into observation model
  # So observation model can use: mu + mu_trend
  
  dev-tasks-stancode-generation-update
