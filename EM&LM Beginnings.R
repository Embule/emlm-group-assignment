load("~/Documents/EM&LM/Group Assignment/Group_7.RData")

library(ggplot2)
library(dplyr)

# Very basic probing of the shape
#----------------------
for (i in 0:2){
  print(mean(DF$smoking_status == i))
}

for (j in c("male", "female")){
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
#----------------------

# First set string variables to numeric
# Male = 0, Female = 1

df <- DF
df <- df %>% 
  mutate(across(sex, function(x){
    case_when(
      x == "male" ~ 0,
      x == "female" ~ 1)}
  ))
# Centre age to get more meaningful results. Need to understand this better.
df$age_centred <- scale(df$Age, center = TRUE, scale = FALSE)

# What was done on slide 121
library(nlme)
# For highly unbalanced data collected at diﬀerent occassions per subject: Continuous AR1, 
# Exponential serial correlation, Gaussian serial correlation.

# Cont. AR1
model.car1 <- gls(UPDRS ~ smoking_status*age_centred+smoking_status*sex, correlation = corCAR1(form = ~ 1 | id),
                  data = df, method = "REML")

model.exp <- gls(UPDRS ~ smoking_status+age_centred+sex, correlation = corExp(form = ~ 1 | id),
                  data = df, method = "REML")

model.gaus <- gls(UPDRS ~ smoking_status+age_centred+sex, correlation = corGaus(form = ~ 1 | id),
                  data = df, method = "REML")

summary(model.car1)
summary(model.exp)
summary(model.gaus)
# How am I getting that age isn't a significant factor in Parkinson's?
# I need random effects models.
