
library(dplyr)
library(ggplot2)
library(tidyverse)
library(nlme)
library(lme4)

load("~/Documents/EM&LM/Group Assignment/Group_7.RData")
df <- DF

# Exploratory Data Analysis
# Get the shape of the data through plots, means, pairwise correlations (need data in wide format for this), 
# derive the OLS residuals (fit linear regression and then make plots)

df <- df %>% 
  mutate(across(sex,
    ~ case_when(
      . == "male" ~ 0,
      . == "female" ~ 1)
  ))
df$id <- as.numeric(as.character(df$id))


df_wide <- df %>% 
  pivot_wider(
    names_from = Visit,
    values_from = UPDRS,
    names_prefix = "UPDRS_"
  )
df_wide <- df_wide[, c(1:7, 11, 8, 12, 10, 9)] # Rearrange the columns into the correct order

all.patients.summary <- df%>% 
  group_by(smoking_status, Visit) %>% 
  summarise(
    mean_UPDRS = mean(UPDRS, na.rm=TRUE),
    sd_UPDRS = sd(UPDRS, na.rm=TRUE),
    n = sum(!is.na(UPDRS)),
    se_UPDRS = sd_UPDRS / sqrt(n)
  )

# non.smoker.summary <- df[df$smoking_status == 0, ] %>% 
#   group_by(Visit) %>% 
#   summarise(
#     mean_UPDRS = mean(UPDRS, na.rm=TRUE),
#     sd_UPDRS = sd(UPDRS, na.rm=TRUE),
#     n = sum(!is.na(UPDRS)),
#     se_UPDRS = sd_UPDRS / sqrt(n)
#   )

# Graph of UPDRS score for each smoking status
ggplot(all.patients.summary, aes(x = Visit, y = mean_UPDRS)) +
  geom_line(color = "black") +
  geom_point(shape = 21, fill = "white", size = 2) +
  geom_errorbar(aes(ymin = mean_UPDRS - se_UPDRS,
                     ymax = mean_UPDRS + se_UPDRS), 
                width = 0.5, color = "blue") +
  facet_wrap(~ smoking_status, nrow = 1,  # This naming chunk appears in many graphs, could assign it its own name to speed things up
             labeller = labeller(smoking_status =
                                   c("0" = "Non-smoker",
                                     "1" = "Former smoker",
                                     "2" = "Current smoker")
                                 )
             ) +
  labs(x = "Visit", y = "UPDRS Score")

# Spaghetti Plots for random sample
set.seed(101)

random_ids <- df %>% 
  distinct(smoking_status, id) %>% 
  group_by(smoking_status) %>% 
  slice_sample(n = 10)

df_sample <- df %>% 
  semi_join(random_ids, by = c("smoking_status", "id"))

ggplot(df_sample, aes(x = Visit, y = UPDRS, group = id, colour = factor(id))) +
  geom_line(show.legend = FALSE, alpha = 0.75) +
  facet_wrap(~ smoking_status, nrow = 1) +
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

colSums(is.na(df))
colSums(is.na(df_wide)) # Around 180 missing measurements per visit after baseline

df_wide_UPDRS <- subset(df_wide, select = -c(1,2,3,4)) 
cor(df_wide_UPDRS, use = "pairwise.complete.obs")
# Measurements in UPDRS score drop off nonlinearly for each visit.





# Model Selection & Refinement
# Try various models and compare them with anova()

# Not a mixed effects model
# model.linear.car1 <- gls(UPDRS ~ Age*smoking_status*sex, correlation = corCAR1(form = ~ Visit | id),
#                   data = df, method = "REML", na.action = na.exclude,
#                   weights = varIdent(form = ~ 1 | Visit)
# )
# summary(model.linear.car1)


# Make a mixed effects model with AR1 correlation structure, nlme package.
model1 <- lme(UPDRS ~ Visit + smoking_status + sex + Age, 
                  random = ~ 1 | id,
                  correlation = corCAR1(form = ~ Visit | id),
                  data = df,
                  method = "ML"
)
summary(model1)
# nlme seems to crumble at the first hurdle.

# Trying the lme4 package
model1 <- lmer(UPDRS ~ Visit*smoking_status*sex*Age + (Visit | id), data = df)
summary(model1)
vcov(model1)


# Comparing models

# Always set m1 to be the simpler model and m2 the more complex.
# Random intercepts only
m1 <- lmer(UPDRS ~ Visit + smoking_status + Age + sex + (1 | id), data = df)
# Random intercepts and random slopes
m2 <- lmer(UPDRS ~ Visit + smoking_status + Age + sex + (Visit | id), data = df)
anova(m1, m2)
# Lower AIC and BIC for m2 (the models are not nested), so more complex one is needed.

# Random intercepts and random slopes
m1 <- lmer(UPDRS ~ Visit + smoking_status + Age + sex + (Visit | id), data = df)
# RI + RS + all interactions
m2 <- lmer(UPDRS ~ Visit*smoking_status*Age*sex + (Visit | id), data = df)
anova(m1, m2)
# Lower AIC and BIC for m2 so the more complex one is needed

# Remove interaction between visit and everything else
m1 <- lmer(UPDRS ~ Visit + smoking_status*Age*sex + (Visit | id), data = df)
# RI + RS + all interactions
m2 <- lmer(UPDRS ~ Visit*smoking_status*Age*sex + (Visit | id), data = df)
anova(m1, m2)
# More complex wins

# Remove interaction between age and everything else
m1 <- lmer(UPDRS ~ Visit*smoking_status*sex + Age + (Visit | id), data = df)
# RI + RS + all interactions
m2 <- lmer(UPDRS ~ Visit*smoking_status*Age*sex + (Visit | id), data = df)
anova(m1, m2)
# Simpler wins

# Remove interaction between age/sex and everything else
m1 <- lmer(UPDRS ~ Visit*smoking_status + sex + Age + (Visit | id), data = df)
# Remove interaction between age and everything else
m2 <- lmer(UPDRS ~ Visit*smoking_status*sex + Age + (Visit | id), data = df)
anova(m1, m2)
# More complex wins

# Remove age/sex from interaction but have them interact and
m1 <- lmer(UPDRS ~ Visit*smoking_status + sex*Age + (Visit | id), data = df)
# Remove interaction between age and everything else
m2 <- lmer(UPDRS ~ Visit*smoking_status*sex + Age + (Visit | id), data = df)
anova(m1, m2)
# More complex wins

# Looking like the model will be 
true.model <- lmer(UPDRS ~ Visit*smoking_status*sex + Age + (Visit | id), data = df)














