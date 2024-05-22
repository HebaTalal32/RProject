data <- c(20, 23, 25, 23, 25, 36, 70, 70)

num_bins <- 5

hist(data, breaks = num_bins, main = "Histogram", xlab = "Values", ylab = "Frequency")

fd <- table(cut(data, breaks = num_bins, include.lowest = TRUE))

rf <- prop.table(fd)

breaks <- seq(min(data), max(data), length.out = num_bins + 1)
midpoints <- (head(breaks, -1) + tail(breaks, -1))/2

plot(cumsum(fd), type = "o", main = "Ogive Plot", xlab = "Values", ylab = "Cumulative Frequency")

polygon(midpoints, fd, col = "skyblue", border = "blue")
title("Polygon Plot")

plot(midpoints, fd, main = "Dot Plot", xlab = "Midpoints", ylab = "Frequency", pch = 19)



frequency <- c(10, 20, 15, 25)  
bins <-1:4
cumulative_frequency <- cumsum(frequency)


plot(bins, cumulative_frequency, type="o", xlab="Bins", ylab="Cumulative Frequency", main="Cumulative Frequency Plot")

frequency <- c(10, 20, 15, 25)  
bins <- 1:4                     


plot(bins, frequency, type="n", xlab="Bins", ylab="Frequency") 
polygon(bins, frequency, col="skyblue", border="blue")



players <- c(1, 26, 7, 12, 13, 2, 6, 9, 5, 18, 7, 3, 15, 15, 4, 17, 1, 14, 5)


class_limits <- c(1, 5, 9, 13, 17, 21)

freq_table <- table(cut(players, breaks = class_limits, right = FALSE))

midpoint <- (class_limits[-1] + class_limits[-length(class_limits)]) / 2
rel_freq <- prop.table(freq_table)
cum_freq <- cumsum(freq_table)


K <- diff(class_limits)[1]

print("Table")
result_table <- data.frame(
  Class_limit = paste("[", class_limits[-length(class_limits)], "-", class_limits[-1], ")", sep = ""),
  Frequency = as.vector(freq_table),
  Relative_Frequency = as.vector(rel_freq),
  Cumulative_Frequency = cum_freq,
  Midpoint = midpoint
)
print(result_table)






data <- rnorm(100)


hist(data, main="Histogram of Random Data", xlab="Value", ylab="Frequency")

