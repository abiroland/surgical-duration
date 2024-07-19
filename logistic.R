#Regression analysis project: modeling_logistic
#Edited on 05-01-2023
#by: Roland Abi and Prasana

#prep-------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(lmtest)
library(fastDummies)
library(car)

# create binary variable from the mean ----------------------------------------
bindata <- lndata6 %>%
  mutate(response = 
    case_when(
      DurationSurgery > 3.581 ~ "Above",
      DurationSurgery < 3.581 ~ "Below"
    )
  )

names(bindata)

newbindata <- dummy_cols(.data = bindata, select_columns = "response") %>%
  rename(
    above = "response_Above"
  )
names(newbindata)

#model1

mod1 <- glm(above ~ BMI, family = "binomial", newbindata)
tidy1 <- tidy(mod1)
tidy1

#mod2
mod2 <- glm(above ~ BMI + Age, family = "binomial", newbindata)
tidy2 <- tidy(mod2)
tidy2

#mod3
mod3 <- glm(above ~ BMI + Age + female, family = "binomial", newbindata)
tidy3 <- tidy(mod3)
tidy3

#mod4 
mod4 <- glm(above ~ BMI + Age + female + Black + White, 
            family = "binomial", newbindata)
tidy4 <- tidy(mod4)
tidy4

#mod5 
mod5 <- glm(above ~ BMI + Age + female + Black + White + Diabetes_Yes,
            family = "binomial", newbindata)
tidy5 <- tidy(mod5)
tidy5

#mod6 
mod6 <- glm(above ~ BMI + Age + female + Black + White + Diabetes_Yes + 
            crf_yes, family = "binomial", newbindata)
tidy6 <- tidy(mod6)
tidy6

#Full model
mod7 <- glm(above ~ BMI + Age + female + Black + White + Diabetes_Yes + 
              crf_yes + healthy, family = "binomial", newbindata)
tidy7 <- tidy(mod7)
tidy7
glance7 <- glance(mod7)
glance7

#interaction model
mod8 <- glm(above ~ BMI + Age + female + Black + White + Diabetes_Yes + 
              crf_yes + I(BMI*female) +
              I(BMI*healthy), family = "binomial", newbindata)
tidy8 <- tidy(mod8)
tidy8

#Reduced model
reduced <- glm(above ~ BMI + healthy, family = "binomial", newbindata)
tidy9 <- tidy(reduced)
tidy9
glance9 <- glance(reduced)
glance9

#Goodness of fit test
lrtest(mod7, reduced)

#Checking assumptions
vif(reduced)

#Residuals for mod9
mod9_aug <- augment(data = newbindata, reduced, type.predict = "response", 
                    type.residuals = "pearson")
head(mod9_aug)

logiplt <- ggplot(data = mod9_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  ggtitle("Pearson's Residuals vs Estimated Probabilities") +
  xlab("Estimated Probabilities") +
  ylab("Pearson's Residuals") +
  theme_bw()

#Influential observations 
infIndexPlot(reduced, vars = c("Cook"), main = "Diagnostic Index Plots")

#Extreme observations
infIndexPlot(reduced, vars = c("hat"), main = "Hat Matrix Diagnostic Index Plots")

