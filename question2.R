set.seed(123) # For reproducibility

# Parameters
years <- 2001:2020
n_years <- length(years)
hospitals <- paste("Hospital", LETTERS[1:5])
n_hospitals <- length(hospitals)
lambda_base <- 100 # Base rate of deaths

# Simulating data
data <- expand.grid(Year = years, Hospital = hospitals)
data$CancerDeaths <- rpois(n = nrow(data), lambda = lambda_base * runif(n = nrow(data), min = 0.8, max = 1.2))

# Viewing the first few rows of the dataset
head(data)

### Statistical Tests/Analyses to Consider

# 1. **Trend analysis over time:** Apply a linear regression model to test if there's a significant trend in cancer deaths over the years.

lm_trend <- lm(CancerDeaths ~ Year, data = data)
summary(lm_trend)

# 2.
anova_hospitals <- aov(CancerDeaths ~ Hospital, data = data)
summary(anova_hospitals)

# 3.
data$YearShift <- lag(data$CancerDeaths, n = n_hospitals)
data$Change = data$CancerDeaths - data$YearShift
lm_change <- lm(Change ~ Year, data = data)
summary(lm_change)


# 4.
cor.test(~ Year + CancerDeaths, data = data)

#5. 
data$Interaction <- interaction(data$Year, data$Hospital)
lm_interaction <- lm(CancerDeaths ~ Interaction, data = data)
summary(lm_interaction)

#6.
library(lme4)
lmer_model <- lmer(CancerDeaths ~ Year + (1|Hospital), data = data)
summary(lmer_model)


#7.
TukeyHSD(anova_hospitals)


#8.
pois_model <- glm(CancerDeaths ~ Year + Hospital, family = "poisson", data = data)
summary(pois_model)

#9.
library(lme4)
lmer_model <- lmer(CancerDeaths ~ Year + (1|Hospital), data = data)
summary(lmer_model)


#10.
kruskal_test <- kruskal.test(CancerDeaths ~ Hospital, data = data)
summary(kruskal_test)


