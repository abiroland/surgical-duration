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
sctmat <- ggpairs(lndata, columns = c(1,2,10))

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



replt1a <- ggplot(resmod1) +
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


replt1b <- ggplot(resmod1) +
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
replt1c <- ggplot(resmod1, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals",
    subtitle = "Linear model for sqrtSD") +
  theme_bw()


#Histogram of residuals for reduced
replt1d <- ggplot(resmod1, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    subtitle = "Linear model for SqrtSD",
    x = "Residuals",
    y = "") +
  theme_bw()

replt_grp <- ggarrange(replt1a, replt1b, replt1c, replt1d,
                       nrow = 2, ncol = 2)

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

lndata6b <- lndata6 %>%
  mutate(
    sqage = Age^2
  )

mod2_b <- lm(sqrtSD ~ BMI + Age + sqage, lndata6b)
tidy2_b <- tidy(mod2_b)
tidy2_b

resmod2 <- augment(mod2, lndata)

ggplot(resmod2,aes(x = Age, y = .fitted, 
                  color = Gender)) +
  geom_line()

#Check for gender difference between BMI and surgical duration------------------

lndata2 <- dummy_cols(.data = lndata, select_columns = "Gender") %>%
  rename(
    "female" = `Gender_Female`
  )

mod3 <- lm(sqrtSD ~ BMI + Age + sqage + female, lndata6b)
tidy3 <- tidy(mod3)
tidy3
glance3 <- glance(mod3)
glance3

testmod <- lm(sqrtSD ~ BMI + Age + sqage + I(BMI*Age) + I(BMI*female) + I(Age*female),
              lndata6b)
testmodtidy <- tidy(testmod)
testmodtidy


#model4: mod3 + Race
lndata3 <- dummy_cols(.data = lndata2, select_columns = "Race") %>%
  rename(
    "Black" = `Race_Black`,
    "White" = `Race_White`
  )

mod4 <- lm(sqrtSD ~ BMI + Age + sqage + female + Black + White, lndata6b)
tidy4 <- tidy(mod4)
tidy4
glance4 <- glance(mod4)
glance4

#model5: mod4 + diabetes

lndata4 <- dummy_cols(.data = lndata3, select_columns = "Diabetes")

mod5 <- lm(sqrtSD ~ BMI + Age + sqage + female + Black + White + Diabetes_Yes, 
           lndata6b)
tidy5 <- tidy(mod5)
tidy5
glance5 <- glance(mod5)
glance5


#mod6: mod5 + Renal failure

lndata5 <- dummy_cols(.data = lndata4, select_columns = "ChronicRenalFailure") %>%
  rename(
    "crf_yes" = `ChronicRenalFailure_Yes`
  )

mod6 <- lm(sqrtSD ~ BMI + sqage + Age + female + Black + White + Diabetes_Yes +
             crf_yes,lndata6b)
tidy6 <- tidy(mod6)
tidy6
glance6 <- glance(mod6)
glance6


#Interaction terms-------------------------------------------------------------
mod_int <- lm(sqrtSD ~ BMI + Age + sqage + I(sqage*female) + female  +
             Black + White + Diabetes_Yes + I(sqage*Diabetes_Yes) +
             I(sqage*crf_yes) + crf_yes + healthy, lndata6b)
tidy_int <- tidy(mod_int)
tidy_int
glance_int <- glance(mod_int)
glance_int

vif_interaction <- ols_vif_tol(mod_int)


#full model --------------------------------------------------------------------
lndata6 <- lndata5 %>%
  filter(!is.na(ASAstatus)) %>%
  mutate(
    grpASA = case_when(
      ASAstatus == "Normal Healthy Patient" ~ "healthy",
      ASAstatus %in% c("Severe Systemic Disease", "Mild Systemic Disease") ~ 
        "unhealthy" 
      )
  ) %>%
  dummy_cols(select_columns = "grpASA") %>%
  rename(
    "healthy" = `grpASA_healthy`,
  )

mod7 <- lm(sqrtSD ~ BMI + Age + female +
             Black + White + Diabetes_Yes + crf_yes + healthy, lndata6b)

tidy7 <- tidy(mod7)
tidy7
glance7 <- glance(mod7)
glance7

full_anova <- lb_anovat_lm(mod7, reg_collapse = F)
full_anova

#multicollinearity
vif_full <- ols_vif_tol(mod7)

mod8_full <- lm(sqrtSD ~ BMI + Age + female +
                  Black + White + Diabetes_Yes + crf_yes + healthy, lndata6b)
tidy8_full <- tidy(mod8_full)
tidy8_full

full_anova8 <- lb_anovat_lm(mod8_full, reg_collapse = F)
full_anova8

vif(mod8_full)


#Reduced model------------------------------------------------------------------
modint <- lm(sqrtSD ~ BMI + healthy, lndata6b)
tidymodint <- tidy(modint)
tidymodint
glanceint <- glance(modint)
glanceint

reduced_anova <- lb_anovat_lm(modint, reg_collapse = F)
reduced_anova

#multicollinearity
vif_int <- ols_vif_tol(modint)

#Testing hypothesis between reduced model vs full model
Fstat <- ((reduced_anova$SS[3] - full_anova$SS[9])/(reduced_anova$Df[3] -
          full_anova$Df[9]))/((full_anova$SS[9])/(full_anova$Df[9]))
Fstat

Pvalue <- pf(Fstat, 6, 2910, lower.tail = FALSE)
Pvalue


#Residual analysis reduced model -----------------------------------------------
reduced_res <- augment(modint, data = lndata6b)

fplt1a <- ggplot(reduced_res) +
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


fplt1b <- ggplot(reduced_res) +
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
fplt1c <- ggplot(reduced_res, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = I('blue')) +
  labs(
    title = "Plot3: Normal plot of residuals",
    subtitle = "Linear model for sqrtSD") +
  theme_bw()

#Histogram of residuals for reduced
fplt1d <- ggplot(reduced_res, aes(
  x = .resid)) +
  geom_histogram(bins = 8, fill = I('blue'), color = I("black")) +
  labs(
    title = "Plot4: Distribution of residuals",
    subtitle = "Linear model for SqrtSD",
    x = "Residuals",
    y = "") +
  theme_bw()

redplt <- ggarrange(fplt1a, fplt1b, fplt1c, fplt1d)

dia1 <- ols_plot_resid_stand(modint)
dia2 <- ols_plot_resid_lev(modint)
diad3 <- ols_plot_cooksd_chart(modint)

diag <- ggarrange(dia1, dia2, diad3)

#Without outlier or influential observation
outlier <- c(0.28, 3.06, 3.56, 0.5, 0.98)

modint_2 <- lndata6 %>% 
 filter(sqrtSD != 0.28 & sqrtSD != 3.06 & sqrtSD != 3.56 & sqrtSD != 0.5 &
          sqrtSD != 0.98)

modint1_1 <- lm(sqrtSD ~ BMI + female + healthy, modint_2)
tidymodint_1 <- tidy(modint1_1)
tidymodint_1
glanceint <- glance(modint)
glanceint

