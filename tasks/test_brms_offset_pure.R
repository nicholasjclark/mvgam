# Pure brms offset test (no mvgam)
library(brms)

cat("=== PURE BRMS OFFSET TEST ===\n\n")

# Create simple data with offset
set.seed(42)
n <- 30
data <- data.frame(
  y = rpois(n, lambda = 5),
  x = rnorm(n),
  log_exposure = log(runif(n, 0.8, 1.2))
)

cat("Fitting brms model with offset...\n")
fit <- brm(
  y ~ x + offset(log_exposure),
  data = data,
  family = poisson(),
  chains = 1,
  iter = 300,
  refresh = 0,
  silent = 2
)

cat("Fit complete\n\n")

# Create newdata
newdata <- data[1:5, ]

cat("=== TEST 1: brms::standata ===\n")
sdata <- brms::standata(fit, newdata = newdata, internal = TRUE)
cat("standata names (first 20):", paste(head(names(sdata), 20), 
                                        collapse = ", "), "\n")

offset_in_sdata <- grep("offset", names(sdata), value = TRUE, 
                        ignore.case = TRUE)
if (length(offset_in_sdata) > 0) {
  cat("\n✓ Offset fields in standata:\n")
  for (f in offset_in_sdata) {
    cat("  ", f, ":", sdata[[f]], "\n")
  }
} else {
  cat("\n✗ No offset in standata\n")
}

cat("\n=== TEST 2: brms::prepare_predictions ===\n")
prep <- brms::prepare_predictions(fit, newdata = newdata)
cat("prep names:", paste(names(prep), collapse = ", "), "\n")

if ("ac" %in% names(prep)) {
  cat("\n✓ prep$ac exists\n")
  cat("  ac names:", paste(names(prep$ac), collapse = ", "), "\n")
  if ("offset" %in% names(prep$ac)) {
    cat("\n✓✓ prep$ac$offset FOUND\n")
    cat("    Values:", prep$ac$offset, "\n")
  }
} else {
  cat("\n✗ prep$ac does NOT exist\n")
}

if ("sdata" %in% names(prep) && "offset" %in% names(prep$sdata)) {
  cat("\n✓ prep$sdata$offset exists:", prep$sdata$offset, "\n")
} else {
  cat("\n✗ prep$sdata$offset does NOT exist\n")
}

cat("\n=== CONCLUSION ===\n")
cat("Offsets stored in: ", 
    if ("ac" %in% names(prep) && "offset" %in% names(prep$ac)) 
      "prep$ac$offset" 
    else if ("sdata" %in% names(prep) && "offset" %in% names(prep$sdata)) 
      "prep$sdata$offset" 
    else 
      "UNKNOWN", "\n")
