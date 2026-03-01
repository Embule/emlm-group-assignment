load("Group_7.RData")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(nlme)
library(lme4)

for (i in 0:2) {
  print(mean(DF$smoking_status == i))
}

for (j in c("male", "female")) {
  print(mean(DF$sex == j))
}

ggplot(data = DF, aes(x = Age, colour = "red")) +
  geom_histogram(stat = "bin", binwidth = 1)

ggplot(data = DF, aes(x = Visit, colour = "blue")) +
  geom_histogram(stat = "bin", binwidth = 1)

# for (k in 0:7){
# print(ggplot(data = DF[DF$Visit == k, ], aes_string(x = "Age", y = "UPDRS")) +
#   geom_bar(stat = "identity") +
#   labs(title = k))
# }

# First set string variables to numeric
# Male = 0, Female = 1

df <- DF
df <- df %>%
  mutate(across(sex, function(x) {
    case_when(x == "male" ~ 0, x == "female" ~ 1)
  }))
# Centre age to get more meaningful results. Need to understand this better.
df$age_centred <- scale(df$Age, center = TRUE, scale = FALSE)

all.patients.summary <- df%>% 
  group_by(smoking_status, Visit) %>% 
  summarise(
    mean_UPDRS = mean(UPDRS, na.rm=TRUE),
    sd_UPDRS = sd(UPDRS, na.rm=TRUE),
    n = sum(!is.na(UPDRS)),
    se_UPDRS = sd_UPDRS / sqrt(n)
  )


ggplot(all.patients.summary, aes(x = Visit, y = mean_UPDRS)) +
  geom_line(color = "black") +
  geom_point(shape = 21, fill = "white", size = 2) +
  geom_errorbar(aes(ymin = mean_UPDRS - se_UPDRS,
                     ymax = mean_UPDRS + se_UPDRS), 
                width = 0.5, color = "blue") +
  facet_wrap(~ smoking_status, nrow = 1, 
             labeller = labeller(smoking_status =
                                   c("0" = "Non-smoker",
                                     "1" = "Former smoker",
                                     "2" = "Current smoker")
                                 )
             ) +
  labs(x = "Visit", y = "UPDRS Score")


# Squared OLS residuals represent the squared vertical distances between 
# observed and predicted values in a linear regression.
# "A common variant is plotting squared residuals against the fitted values, sometimes 
# including a smoothed regression line to better visualize the variance trend."
lin.reg <- lm(UPDRS ~ Visit, data = df)
sq.ols.residuals <- resid(lin.reg)^2

df2 <- df
df2$sq_ols_residuals <- sq.ols.residuals
df2$fitted <- fitted(lin.reg)

ggplot(df2, aes(x = Visit, y = sq_ols_residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Visit", y = "Squared OLS Residuals")

ggplot(df2, aes(x = fitted, y = sq_ols_residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fitted Values", y = "Squared OLS Residuals")
# It does not look like there's any difference between the two

# Model Selection & Refinement
# Try various models and compare them with anova()

model.car1 <- gls(
  UPDRS ~ Visit + smoking_status * age_centred + smoking_status * sex,
  correlation = corCAR1(form = ~ 1 | id),
  data = df,
  method = "REML"
)

model.exp <- gls(
  UPDRS ~ Visit + smoking_status + age_centred + sex,
  correlation = corExp(form = ~ 1 | id),
  data = df,
  method = "REML"
)

model.gaus <- gls(
  UPDRS ~ Visit + smoking_status + age_centred + sex,
  correlation = corGaus(form = ~ 1 | id),
  data = df,
  method = "REML"
)

summary(model.car1)
summary(model.exp)
summary(model.gaus)
# How am I getting that age isn't a significant factor in Parkinson's?
# I need random effects models.

##### Model diagnostics for linear models with correlated errors (slides 154 - 165)
# Plots of normalized residuals
# look OK, no trends visible and spread of residuals in time is fairly constant
plot(residuals(model.car1, type = "normalized"))
plot(residuals(model.exp, type = "normalized"))
plot(residuals(model.gaus, type = "normalized"))

# QQ plots of pearson residuals
plot(qqnorm(residuals(model.car1, type = "normalized")))
plot(qqnorm(residuals(model.exp, type = "normalized")))
plot(qqnorm(residuals(model.gaus, type = "normalized")))

# All QQ plots show a skew which means the violations of normality for residuals 
# is violated



# linear mixed effects model for random intercepts and random slopes
model.lme1 <- lme(
  UPDRS ~ smoking_status + Visit + Age + sex + Visit:smoking_status,
  data = DF,
  random = ~ Visit | id,
  method = "REML"
)
## gives an error???


# linear mixed effects model for random intercepts
model.lme2 <- lme(
  UPDRS ~ smoking_status + Visit + Age + sex + Visit:smoking_status,
  data = DF,
  random = ~ 1 | id,
  method = "REML"
)
summary(model.lme2)

# linear mixed effects model for uncorrelated random intercepts and random slopes

model.lme3 <- lme(
  UPDRS ~ smoking_status + Visit + Age + sex + Visit:smoking_status,
  data = DF,
  random = list(id = pdDiag(form = ~ id)),
  method = "REML"
)
summary(model.lme3)
## also error???





