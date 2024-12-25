#Simulation to Determine the Correlation Between Education and Income for Junior High School Equivalent
library(car)
library(readxl)
library(ggplot2)
library(lmtest)

#Import data 
data_income <- read_excel("File/bpsdata.xlsb.xlsx", sheet = "income", na = "NA")

data_education <- read_excel("File/bpsdata.xlsb.xlsx", sheet = "education", na = "NA")

data_all <- data.frame(
  Provinsi = data_income[5:42, 2],
  Income = round(as.numeric(unlist(data_income[5:42, 5])), 2),
  Education = round(as.numeric(unlist(data_education[5:42, 4])), 2)
)

#"Viewing the data to be processed 
colnames(data_all) <- c("Provinsi", "Income", "Education")
print(data_all)

#Data normalization
data_clean <- na.omit(data_all)
z_score_normalize <- function(x) {
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}


data_clean$Income <- z_score_normalize(data_clean$Income)
data_clean$Education <- z_score_normalize(data_clean$Education)

print(data_clean)

# scatter plot to check the linearity of the data
ggplot(data_clean, aes(x = Education, y = Income)) +
  geom_point() +
  labs(title = "Scatter Plot Education vs Income",
       x = "Tahun Education", y = "Income (thousand rupiah)")

# Using a histogram to check for normal distribution
dev.off()
hist(data_clean$Income, main="Histogram Income", xlab="Income (thousand rupiah)")
dev.off()
hist(data_clean$Education, main="Histogram Education", xlab="Education Year")


# Normality Test
shapiro.test(data_clean$Income)
shapiro.test(data_clean$Education)

# checking homogeneity of variance

model <- lm(Income ~ Education, data = data_clean) 
plot(model$fitted.values, model$residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red") 

# Heteroskedastisitas Test
bptest(model)

# Calculating the correlation coefficient
correlation <- cor(data_clean$Education, data_clean$Income)
print(paste("Corelation coefficient :", correlation))

# Interpreting correlation results
if(correlation > 0.7) {
  print("Strong correlation between education and income.")
} else if(correlation > 0.5) {
  print("A Moderate correlation between education and income.")
} else {
  print("Weak correlation between education and income.")
}
