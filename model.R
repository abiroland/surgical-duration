#Regression analysis project: modeling
#Edited on 05-01-2023
#by: Roland Abi and Prasana

# prep -------------------------------------------------------------------------
library(broom)
library(tidyverse)
library(olsrr)
library(lbutils)
library(ggpubr)
library(GGally)
library(fastDummies)
library(car)

# Distribution of response variable

untr <- proj_bmi_df %>%
  ggplot(aes(x=DurationSurgery)) +
  geom_histogram(bins = 10, fill = "#7C96AB", color = "#9BA4B5") +
  theme_bw() +
  labs(
    subtitle = "before transformation"
  ) 

lndata <- proj_bmi_df %>%
  mutate(
    sqrtSD = sqrt(DurationSurgery)
  )

# After transformation ---------------------------------------------------------

trn <- lndata %>%
  ggplot(aes(x=sqrtSD)) +
  geom_histogram(bins = 10, fill = "#957777", color = "#9BA4B5") +
  theme_bw() +
  labs(subtitle = "after sqrt transformation")

trnplt <- ggarrange(untr, trn)

#Examining relationship with predictor variables -------------------------------
ggpairs(lndata, columns = c(1,2,10))

lndata %>%
  ggplot(aes(x = Race, y = sqrtSD)) +
  geom_boxplot()

prop.table(table(lndata$ASAstatus, lndata$Gender))

#model 1
mod1 <- lm(sqrtSD ~ BMI, lndata)
tidy1 <- tidy(mod1)
tidy1
glance1 <- glance(mod1)
glance1

#Residual analysis mod1---------------------------------------------------------
resmod1 <- augment(mod1, data = lndata)

ggplot(resmod1) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  geom_hline(yintercept = 0, linewidth = 1, color = "red") +
  labs(
    title = "Plot of Residuals vs fits for sqrtSD",
    x = "Fitted values of Surgical Duration",
    y = "Residuals"
  ) +
  theme_bw()


ggplot(resmod1) +
  geom_point(aes(
    x = BMI,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on sqrtSD",
    x = "BMI",
    y = "Residuals"
  ) +
  theme_bw()

#qqplot of residuals for reduced
ggplot(resmod1, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals",
    subtitle = "Linear model for sqrtSD") +
  theme_bw()


#Histogram of residuals for reduced
ggplot(resmod1, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    subtitle = "Linear model for SqrtSD",
    x = "Residuals",
    y = "") +
  theme_bw()

resmod1 %>%
  ggplot(aes(x = sqrtSD, y=.fitted)) +
  geom_point()

#mod2 with a quadratic function of BMI
#lndata2 <- dummy_cols(.data = lndata, select_columns = "grpbmi") %>%
 # select(-grpbmi_NA) %>%
  #rename(
   # "normal_weight" = `grpbmi_Normal weight`,
    #"Underweight" = `grpbmi_Underweight`,
    #"Overweight" = `grpbmi_Overweight`
  #)

#lndata2 <- lndata %>%
 # mutate(
  #  bmi2 = BMI*BMI
  #)

mod2 <- lm(sqrtSD ~ BMI + Age, lndata)
tidy2 <- tidy(mod2)
tidy2
glance2 <- glance(mod2)
glance2

#Check for gender difference between BMI and surgical duration------------------

lndata2 <- dummy_cols(.data = lndata, select_columns = "Gender") %>%
  rename(
    "female" = `Gender_Female`
  )

mod3 <- lm(sqrtSD ~ BMI + Age + female, lndata2)
tidy3 <- tidy(mod3)
tidy3
glance3 <- glance(mod3)
glance3

#model4: mod3 + Race
lndata3 <- dummy_cols(.data = lndata2, select_columns = "Race") %>%
  rename(
    "Black" = `Race_Black`,
    "White" = `Race_White`
  )

mod4 <- lm(sqrtSD ~ BMI + Age + female + Black + White, lndata3)
tidy4 <- tidy(mod4)
tidy4
glance4 <- glance(mod4)
glance4

#model5: mod4 + diabetes

lndata4 <- dummy_cols(.data = lndata3, select_columns = "Diabetes")

mod5 <- lm(sqrtSD ~ BMI + Age + female + Black + White + Diabetes_Yes, 
           lndata4)
tidy5 <- tidy(mod5)
tidy5
glance5 <- glance(mod5)
glance5


#mod6: mod5 + Renal failure

lndata5 <- dummy_cols(.data = lndata4, select_columns = "ChronicRenalFailure") %>%
  rename(
    "crf_yes" = `ChronicRenalFailure_Yes`
  )

mod6 <- lm(sqrtSD ~ BMI + Age + female + Black + White + Diabetes_Yes +
             crf_yes,lndata5)
tidy6 <- tidy(mod6)
tidy6
glance6 <- glance(mod6)
glance6

#full model --------------------------------------------------------------------
lndata6 <- lndata5 %>%
  filter(!is.na(ASAstatus)) %>%
  mutate(
    grpASA = case_when(
      ASAstatus == "Normal Healthy Patient" ~ "healthy",
      ASAstatus %in% c("Severe Systemic Disease", "Mild Systemic Disease") ~ "unhealthy" 
      )
  ) %>%
  dummy_cols(select_columns = "grpASA") %>%
  rename(
    "healthy" = `grpASA_healthy`,
  )

mod7 <- lm(sqrtSD ~ BMI + Age + female + I(Age*female) +
             Black + White + Diabetes_Yes + I(Age*Diabetes_Yes) +
             I(Age*crf_yes) + crf_yes + healthy, lndata6)
tidy7 <- tidy(mod7)
tidy7
glance7 <- glance(mod7)
glance7

full_anova <- lb_anovat_lm(mod7, reg_collapse = F)
full_anova

ols_vif_tol(mod7)

mod8_full <- lm(sqrtSD ~ BMI + Age + female +
                  Black + White + Diabetes_Yes + crf_yes + healthy, lndata6)
tidy8_full <- tidy(mod8_full)
tidy8_full

full_anova8 <- lb_anovat_lm(mod8_full, reg_collapse = F)
full_anova8

vif(mod8_full)


#Reduced model------------------------------------------------------------------
modint <- lm(sqrtSD ~ BMI + female + healthy, lndata6)
tidymodint <- tidy(modint)
tidymodint
glanceint <- glance(modint)
glanceint

reduced_anova <- lb_anovat_lm(modint, reg_collapse = F)
reduced_anova

ols_vif_tol(modint)

#Testing hypothesis between reduced model vs full model
Fstat <- ((reduced_anova$SS[3] - full_anova$SS[9])/(reduced_anova$Df[3] -
          full_anova$Df[9]))/((full_anova$SS[9])/(full_anova$Df[9]))
Fstat

Pvalue <- pvalue <- pf(Fstat, 13, 2910, lower.tail = FALSE)
pvalue


#Residual analysis reduced model -----------------------------------------------
reduced_res <- augment(modint, data = lndata6)


ggplot(reduced_res) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  geom_hline(yintercept = 0, linewidth = 1, color = "red") +
  labs(
    title = "Plot of Residuals vs fits for sqrtSD",
    x = "Fitted values of Surgical Duration",
    y = "Residuals"
  ) +
  theme_bw()


ggplot(reduced_res) +
  geom_point(aes(
    x = BMI,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on sqrtSD",
    x = "BMI",
    y = "Residuals"
  ) +
  theme_bw()

#qqplot of residuals for reduced
ggplot(reduced_res, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals",
    subtitle = "Linear model for sqrtSD") +
  theme_bw()

#Histogram of residuals for reduced
ggplot(reduced_res, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    subtitle = "Linear model for SqrtSD",
    x = "Residuals",
    y = "") +
  theme_bw()

#due to assumption of constant variance not being met we used
#weighted least square regression model 

# Weighted least squares regression --------------------------------------------

#Brown-Forsythe test for constancy of error variance

#**Hypothesis:**
  
  # $H_0: \sigma_A^2 = \sigma_B^2$ (variance is constant)

  # $H_a: \sigma_A^2 \neq \sigma_B^2$ (Variance is not constant)

# Perform weighted least square regression--------------------------------------
#Calculate weights vector

modint_res <- reduced_res %>%
  mutate(abs_resid = abs(.resid))

#Test of relationship we did like to model--------------------------------------
#Absolute residual vs BMI
ggplot(modint_res, aes(x=BMI, y=abs_resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  labs(
    title = "Absolute residual vs BMI",
    x = "BMI",
    y = "|Residual|"
  )

#Estimate the weighting function and the weights using the explanatory variable-
#fit the standard the deviation function
lm.abs.resid2 <- lm(abs_resid ~ .fitted, modint_res)

#Extract the new fitted values from the SD function
si <- fitted(lm.abs.resid2)

#calculate the weights vector
wts <- 1/(si*si)

#Fit WLS Regression model
wls <- lm(sqrtSD ~ BMI + healthy, lndata6, weights = wts)
tidy_wls <- tidy(wls)
tidy_wls
glance_wls <- glance(wls)
glance_wls

res_wls <- augment(wls, data = lndata6)
res_wls

#Residual analysis reduced model -----------------------------------------------

wls_1 <- ggplot(res_wls) +
  geom_point(aes(
    x = .fitted,
    y = .resid
  )) +
  labs(
    title = "Plot of Residuals vs fits for sqrtSD",
    x = "Fitted values of Surgical Duration",
    y = "Residuals"
  ) +
  theme_bw()


wls_2 <- ggplot(res_wls) +
  geom_point(aes(
    x = BMI,
    y = .resid
  )) +
  labs(
    title = "Plot of residuals on sqrtSD",
    x = "BMI",
    y = "Residuals"
  ) +
  theme_bw()

#qqplot of residuals for reduced
wls_3 <- ggplot(reduced_res, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals",
    subtitle = "Linear model for sqrtSD") +
  theme_bw()

#Histogram of residuals for reduced
wls_4 <- ggplot(reduced_res, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    subtitle = "Linear model for SqrtSD",
    x = "Residuals",
    y = "") +
  theme_bw()

wls_resid <- ggarrange(wls_1, wls_2, wls_3, wls_4)
