library(rstanarm)
library(ggplot2)

set.seed(123) # Ensuring reproducibility
years <- 2001:2020
hospitals <- paste("Hospital", LETTERS[1:5])
data <- expand.grid(Year = years, Hospital = hospitals)
data$CancerDeaths <- rpois(n = nrow(data), lambda = 100) # Simplified simulation



# Aggregating data for plotting
agg_data <- aggregate(CancerDeaths ~ Year, data, sum)

# Plotting
ggplot(agg_data, aes(x = Year, y = CancerDeaths)) +
  geom_line() + 
  geom_point() +
  theme_minimal() +
  labs(title = "Cancer Deaths in Sydney's Hospitals Over 20 Years",
       x = "Year",
       y = "Number of Cancer Deaths")


# Model assuming deaths increase linearly over time
stan_model <- stan_glm(CancerDeaths ~ Year, data = data, family = poisson(link = "log"))

# Viewing model summary
print(summary(stan_model))
