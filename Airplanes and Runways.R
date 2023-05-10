# Christa Ramos
# STAT 4345 Project
# Simulating Airplanes and Runways

#set.seed(1)
arrival.rate = 250
days.list = c(1, 7, 30, 182, 365)

results = data.frame(days = days.list,
                     mean.runways = numeric(length(days.list)),
                     sd.runways = numeric(length(days.list)),
                     mean.airplanes = numeric(length(days.list)),
                     airplane.runways.rate = numeric(length(days.list)))

for (i in 1:length(days.list)) {
  days = days.list[i]
  airplanes = 0
  # generate random airplanes for 1, 7, 30, 182, and 365 days that follows a poisson distribution   
  for (j in 1:days) {
    lambda = arrival.rate
    u = runif(1)
    k = 0
    p = exp(-lambda)
    
    while (u >= p) {
      k = k + 1
      p = p + (exp(-lambda) * lambda^k) / factorial(k)
    }
    airplanes = airplanes + k
  }
  # Generate the number of runways available using an exponential distribution  
  runways <- 25
  runways.available <- numeric(airplanes)
  
  for (j in 1:airplanes) {
    rate = 1/runways
    u = runif(1)
    runways.amount = -log(1-u)/rate
    runways.available[j] = round(runways.amount)
  }
  
  # Calculate simulation results
  airplane.runways <- runways.amount / airplanes
  
  results[i, "mean.runways"] = mean(runways.available)
  results[i, "sd.runways"] = sd(runways.available)
  results[i, "mean.airplanes"] = mean(airplanes)
  results[i, "airplane.runways.rate"] = airplane.runways
}

print(results)

# Generate combined histogram to show frequency distribution for number of runways available

hist(runways.available[days.list == 1], col = "transparent", border = "red",
     main = "Histogram of Number of Runways Available", xlab = "Runways", ylab = "Frequency")
hist(runways.available[days.list == 7], col = "transparent", border = "orange", add = TRUE)
hist(runways.available[days.list == 30], col = "transparent", border = "yellow", add = TRUE)
hist(runways.available[days.list == 182], col = "transparent", border = "green", add = TRUE)
hist(runways.available[days.list == 365], col = "transparent", border = "blue", add = TRUE)
legend("topright", c("1 day", "7 days", "30 days", "180 days", "365 days"), fill = c("red", "orange", "yellow", "green", "blue"))


# Create a boxplot for days = 1
boxplot1 = boxplot(runways.available[days.list == 1],
                   main = "Boxplot for Number of Days = 1",
                   xlab = "Number of Days  = 1",
                   ylab = "Runways Amount")

mean1 = mean(runways.available[days.list == 1])
points(mean1, col = "purple", pch = 19)
legend("topright", legend = paste("Mean:", round(mean1, 2)), col = "purple", pch = 19)

# Create a boxplot for days = 7
boxplot7 = boxplot(runways.available[days.list == 7],
                   main = "Boxplot for Number of Days = 7",
                   xlab = "Number of Days = 7",
                   ylab = "Runways Amount")

mean7 = mean(runways.available[days.list == 7])
points(mean7, col = "purple", pch = 19)
legend("topright", legend = paste("Mean:", round(mean7, 2)), col = "purple", pch = 19)

# Create a boxplot for days = 30
boxplot30 = boxplot(runways.available[days.list == 30],
                    main = "Boxplot for Number of Days = 30",
                    xlab = "Number of Days = 30",
                    ylab = "Runways Amount")

mean30 = mean(runways.available[days.list == 30])
points(mean30, col = "purple", pch = 19)
legend("topright", legend = paste("Mean:", round(mean30, 2)), col = "purple", pch = 19)

# Create a boxplot for days = 182
boxplot182 = boxplot(runways.available[days.list == 182],
                     main = "Boxplot for Number of Days = 182",
                     xlab = "Number of Days = 182",
                     ylab = "Runways Amount")

mean182 = mean(runways.available[days.list == 182])
points(mean182, col = "purple", pch = 19)
legend("topright", legend = paste("Mean:", round(mean182, 2)), col = "purple", pch = 19)

# Create a boxplot for days = 365
boxplot365 = boxplot(runways.available[num_days_list == 365],
                     main = "Boxplot for Number of Days = 365",
                     xlab = "Number of Days = 365",
                     ylab = "Runways Amount")

mean365 = mean(runways.available[days.list == 365])
points(mean365, col = "purple", pch = 19)
legend("topright", legend = paste("Mean:", round(mean365, 2)), col = "purple", pch = 19)


