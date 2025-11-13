devtools::load_all()
fit9 <- readRDS("tasks/fixtures/fit9.rds")

cat("=== FIT9 DIAGNOSIS ===\n\n")

cat("Formula:\n")
print(fit9$formula)

cat("\n\nFormula class:\n")
print(class(fit9$formula))

cat("\n\nFormula structure (max.level=2):\n")
str(fit9$formula, max.level = 2)

cat("\n\nChecking for nlpars:\n")
cat("  fit9$formula$nlpars exists?:", "nlpars" %in% names(fit9$formula), "\n")
if ("nlpars" %in% names(fit9$formula)) {
  cat("  nlpars value:\n")
  print(fit9$formula$nlpars)
}

cat("\n\nChecking for pforms:\n")
cat("  fit9$formula$pforms exists?:", "pforms" %in% names(fit9$formula), "\n")
if ("pforms" %in% names(fit9$formula)) {
  cat("  pforms value:\n")
  print(fit9$formula$pforms)
}
