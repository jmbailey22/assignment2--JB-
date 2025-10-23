#1. Load the penguins/csv data set
#Load in the data
penguins <- read.csv("penguins.csv")
View(penguins)
head(penguins)

#2. Run at least two descriptive analyses
#summary of data
summary(penguins)

#mean values of data ignoring all missing values
mean_bill_length <- mean(penguins$bill_length_mm, na.rm = TRUE)
#43.92
mean_bill_depth  <- mean(penguins$bill_depth_mm, na.rm = TRUE)
#17.15
mean_flipper     <- mean(penguins$flipper_length_mm, na.rm = TRUE)
#200.92
mean_body_mass   <- mean(penguins$body_mass_g, na.rm = TRUE)
#4201.75

#Summary table of mean statistics
summary_stats <- data.frame(
  mean_bill_length,
  mean_bill_depth,
  mean_flipper,
  mean_body_mass
)
#saving the summary table
write.csv(summary_stats, "summary_stats.csv", row.names = FALSE)

#Regression of body mass vs flipper length in penguins
Regression1 <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
summary(Regression1)
coef(Regression1) #slope = 49.69 intercept = -5780.83
summary(Regression1)$r.squared #R squared = .76

#Scatter plot for Regression1 and saving as PNG
png("bodymass_flipper_regression.png")
plot(
  penguins$flipper_length_mm,
  penguins$body_mass_g,
  col = "blue",
  pch = 19,
  xlab = "Flipper Length (mm)",
  ylab = "Body Mass (g)",
  main = "Regression of Body Mass on Flipper Length"
)
#Adding in regression line
abline(Regression1, col = "red", lwd = 2)
dev.off()

#Multiple regression including species
Regression2 <-lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
summary(Regression2)

#Added plot for test branch
png("bodymass_histogram.png")
hist(penguins$body_mass_g,
     col = "lightblue",
     main = "Distribution of Penguin Body Mass",
     xlab = "Body Mass (g)")
dev.off()