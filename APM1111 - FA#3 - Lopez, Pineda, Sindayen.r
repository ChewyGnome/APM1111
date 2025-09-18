scores <- c(88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 
            56, 38, 62, 71, 51, 86, 68, 29, 28, 47, 33, 37, 
            25, 36, 33, 94, 73, 46, 42, 34, 79, 72, 88, 99, 
            82, 62, 57, 42, 28, 55, 67, 62, 60, 96, 61, 57, 
            75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)

mode <- function(v) {
uniqv <- sort(unique(v))
uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode <- mode(scores)
mean <- mean(scores)
median <- median(scores)
var <- var(scores)
sd <- sd(scores)
min <- min(scores)
max <- max(scores)

n <- length(scores)
skewness <- (n / ((n-1)*(n-2))) * sum(((scores - mean)/sd)^3)

stdErrorSk <- sqrt(6/(n+3))

kurtosis <- ((n*(n+1))/((n-1)*(n-2)*(n-3))) *
  sum(((scores - mean)/sd)^4) -
  (3*(n-1)^2)/((n-2)*(n-3))

stdErrorKr <- sqrt(24/(n+5))

P25  <- quantile(scores, 0.25)
P50  <- quantile(scores, 0.50)
P75  <- quantile(scores, 0.75)
P90  <- quantile(scores, 0.90)
P95 <- quantile(scores, 0.95)

table <- data.frame(
    
STATISTIC = c("Valid", "Mode", "Median", "Mean", "Standard Deviation",
                "Variance", "Skewness", "Std. Error of Skewness",
                "Kurtosis", "Std. Error of Kurtosis", "Minimum",
                "Maximum", "25th Percentile", "50th Percentile",
                "75th Percentile", "90th Percentile", "95th Percentile"),
VALUE = round(c(n, mode, median, mean, sd, var, skewness, stdErrorSk,
           kurtosis, stdErrorKr, min, max, P25, P50, P75, P90, P95), 3)
)

print("=============================")
print("Descriptive Statistics Table")
print("=============================")

print(table, row.names = FALSE)

print("=============================")