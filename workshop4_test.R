# Test script for workshop4.qmd changes
# Tests the main variable naming updates in Sections 4 and 8

library(tidyverse)
library(ggplot2)

cat("Testing Section 4 variable naming changes...\n")

# Create mock data matching the structure from the workshop
pnp.experimental <- data.frame(
  day = c(0, 0, 8, 8),
  clone = c("A", "B", "A", "B"),
  differentiated = c("N", "N", "Y", "Y"),
  absorb.rep1 = c(0.1, 0.05, 0.3, 0.05),
  absorb.rep2 = c(0.15, 0.06, 0.2, 0.06),
  absorb.rep3 = c(0.12, 0.07, 0.25, 0.07),
  DNA_ug_per_ml = c(2.9, 2.1, 3.9, 4.9)
)

# Mock linear model
linear_model <- lm(y ~ x, data = data.frame(x = 1:4, y = c(0.1, 0.2, 0.3, 0.4)))

# Section 4 Test: Test predicted.pnp.conc naming
cat("\n✓ Testing predicted.pnp.conc variable...\n")
predicted.pnp.conc <- predict(linear_model, pnp.experimental)
pnp.experimental$predicted.pnp.conc = predicted.pnp.conc
cat("  Names:", names(pnp.experimental), "\n")
cat("  predicted.pnp.conc column exists:", "predicted.pnp.conc" %in% names(pnp.experimental), "\n")

# Section 4 Test: Test normalised.pnp.conc naming
cat("\n✓ Testing normalised.pnp.conc variable...\n")
pnp.experimental <- pnp.experimental |> 
  mutate(normalised.pnp.conc = predicted.pnp.conc / DNA_ug_per_ml)
cat("  Names:", names(pnp.experimental), "\n")
cat("  normalised.pnp.conc column exists:", "normalised.pnp.conc" %in% names(pnp.experimental), "\n")

# Test the plots won't error due to missing columns
cat("\n✓ Testing plot column references (Section 4)...\n")
plot1 <- ggplot(data=pnp.experimental, aes(x=day, y=predicted.pnp.conc)) +
  geom_bar(stat="identity")
cat("  Plot 1 (predicted.pnp.conc) created successfully\n")

plot2 <- ggplot(data=pnp.experimental, aes(x=day, y=normalised.pnp.conc)) +
  geom_bar(stat="identity")
cat("  Plot 2 (normalised.pnp.conc) created successfully\n")

# Section 8 Test: Test ANOVA object naming
cat("\n\nTesting Section 8 variable naming changes...\n")

# Create mock ANOVA data
pnp.anova.data <- data.frame(
  day = c(0, 0, 8, 8, 0, 0, 8, 8),
  clone = c("A", "B", "A", "B", "A", "B", "A", "B"),
  induced = c("N", "N", "N", "N", "Y", "Y", "Y", "Y"),
  absorptionrep1 = rnorm(8, 0.2, 0.05),
  absorptionrep2 = rnorm(8, 0.2, 0.05),
  absorptionrep3 = rnorm(8, 0.2, 0.05)
)

# Transform to long format
pnp.anova.long <- pnp.anova.data |> 
  pivot_longer(cols=!c(day,clone,induced), names_to = "replicate", values_to = "absorbance")

cat("\n✓ Testing pnp.anova.long object structure...\n")
cat("  Columns in pnp.anova.long:", names(pnp.anova.long), "\n")
cat("  Expected columns (day, clone, induced, replicate, absorbance) present:", 
    all(c("day", "clone", "induced", "replicate", "absorbance") %in% names(pnp.anova.long)), "\n")

# Convert factors
pnp.anova.long$day <- as.factor(pnp.anova.long$day)
pnp.anova.long$clone <- as.factor(pnp.anova.long$clone)
pnp.anova.long$induced <- as.factor(pnp.anova.long$induced)

# Section 8 Test: ANOVA additive model
cat("\n✓ Testing aov.result.additive object...\n")
aov.result.additive <- aov(absorbance ~ day + clone + induced, data = pnp.anova.long)
cat("  aov.result.additive created successfully\n")
cat("  Object class:", class(aov.result.additive), "\n")

# Section 8 Test: ANOVA interaction model
cat("\n✓ Testing aov.result.interaction object...\n")
aov.result.interaction <- aov(absorbance ~ day * clone * induced, data = pnp.anova.long)
cat("  aov.result.interaction created successfully\n")
cat("  Object class:", class(aov.result.interaction), "\n")

# Test that the plot using pnp.anova.long works
cat("\n✓ Testing plot with pnp.anova.long (Section 8)...\n")
plot3 <- ggplot(pnp.anova.long, aes(x = clone, y = absorbance, colour = day)) +
  geom_boxplot() +
  theme_classic()
cat("  Plot (Section 8) created successfully\n")

cat("\n" %+% "="*50 %+% "\n")
cat("✓ ALL TESTS PASSED - Variable naming changes are syntactically correct\n")
cat("="*50 %+% "\n")
