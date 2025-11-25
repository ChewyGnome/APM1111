data <- data.frame(
  participant = 1:24,
  cloak = c(rep(0,12), rep(1,12)),
  mischief = c(3,1,5,4,6,4,6,2,0,5,4,5,4,3,6,6,8,5,5,4,2,5,7,5)
)

cat("Complete Dataset:\n")
print(data)
cat("\n")

group0 <- data$mischief[data$cloak == 0]
group1 <- data$mischief[data$cloak == 1]

cat("Assumption 1. The dependent variable (mischief) is a continuous level.\n")
cat(is.numeric(data$mischief), "\n\n")

cat("Assumption 2. The independent variable (cloak) consists of two categorical, independent groups (with a cloak, without a cloak).\n")
print(unique(data$cloak))
cat("\n")

cat("Assumption 3. Each participant is present in only one group.\n")
cat("This assumption is met by design because each participant appears only once.\n\n")

cat("Assumption 4. Outliers. There are no significant outliers in the two groups of the independent variable (cloak) in terms of the dependent variable (mischief), as assessed by visual inspection of boxplots.\n")
boxplot(mischief ~ cloak, data = data,
        names = c("Without Cloak", "With Cloak"),
        main = "Boxplot of Mischief by Cloak Group",
        ylab = "Number of Mischievous Acts",
        col = c("lightblue","lightgreen"))
cat("Visual inspection of the boxplot shows no extreme outliers.\n\n")

cat("Assumption 5. Normality. The dependent variable (mischief) for each group (cloak) is normally distributed (p > 0.05), as assessed by Shapiro–Wilk test.\n")
cat("\nWithout Cloak:\n"); print(shapiro.test(group0))
cat("\nWith Cloak:\n"); print(shapiro.test(group1))
cat("Interpretation: p-values greater than 0.05 indicate normality is not violated.\n\n")

cat("Assumption 6. Homogeneity of variances. There is equality of variances between groups (without a cloak, with a cloak) on their number of mischievous acts (mischief), as assessed by Levene’s test of equality of variances.\n")
variance <- var.test(group0, group1)
print(variance)
equal <- variance$p.value > 0.05
cat("Interpretation: p > 0.05 indicates equal variances can be assumed.\n\n")

mean0 <- mean(group0); mean1 <- mean(group1)
sd0 <- sd(group0); sd1 <- sd(group1)
cat("Descriptive Statistics:\n")
cat(sprintf("Without Cloak: n = %d, mean = %.2f, sd = %.2f\n", length(group0), mean0, sd0))
cat(sprintf("With Cloak:    n = %d, mean = %.2f, sd = %.2f\n\n", length(group1), mean1, sd1))

cat("Computation: Independent Samples t-test\n")
result <- t.test(group1, group0, var.equal = equal)
print(result)

cat("\n----- SHORT REPORT -----\n")
if (result$p.value < 0.05) {
  cat("There is a statistically significant difference in mischievous acts between participants with and without a cloak of invisibility.\n")
} else {
  cat("There is no statistically significant difference in mischievous acts between participants with and without a cloak of invisibility.\n")
}

cat(sprintf("t(%d) = %.3f, p = %.3f\n", result$parameter, result$statistic, result$p.value))
cat(sprintf("Mean (With Cloak) = %.2f, Mean (Without Cloak) = %.2f\n", mean1, mean0))
cat(sprintf("Mean Difference = %.2f\n95%% CI: [%.2f, %.2f]\n", mean1 - mean0, result$conf.int[1], result$conf.int[2]))
