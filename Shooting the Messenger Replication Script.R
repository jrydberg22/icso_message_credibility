#### CPOR Credibility Experiment

#### Setup Workspace ####

## Install Packages

install.packages(c("tidyverse", "haven", "survey", "psych", "lavaan"))

## Load Packages

library(tidyverse)
library(haven)
library(naniar)
library(survey)
library(psych)
library(lavaan)

#### Read in Data ####

dta = read_sav("Shooting the Messenger Replication Data.sav") %>% as_factor()

#### Missingness ####

dta = dta %>%
  rowwise() %>%
  mutate(miss_comp = sum(is.na(intelligent), is.na(expert), is.na(informed), 
                         is.na(competent), is.na(smart)),
         miss_good = sum(is.na(not_slfcentered), is.na(interests_heart), is.na(concerned_wme), 
                         is.na(sensitive), is.na(understanding)),
         miss_trst = sum(is.na(honest), is.na(trustworthy), is.na(honorable), 
                         is.na(ethical), is.na(genuine)),
         miss_breh = sum(is.na(belief_tx_1), is.na(belief_tx_2), is.na(belief_tx_3), 
                         is.na(belief_tx_4)),
         miss_bmh = sum(is.na(belief_mh_1), is.na(belief_mh_2), is.na(belief_mh_3), 
                        is.na(belief_mh_4))) %>%
  ungroup() %>%
  filter(miss_comp < 3 & miss_good < 3 & miss_trst < 3 &
           miss_breh < 3 & miss_bmh < 3)

#### Manuscript Analysis ####

#### Table 1 Descriptive Statistics ####

svyd = svydesign(ids = ~1, weights = ~weight, data = dta)

# Competence

svymean(~intelligent, svyd, na.rm = TRUE); sqrt(svyvar(~intelligent, svyd, na.rm = TRUE))
svymean(~expert, svyd, na.rm = TRUE); sqrt(svyvar(~expert, svyd, na.rm = TRUE))
svymean(~informed, svyd, na.rm = TRUE); sqrt(svyvar(~informed, svyd, na.rm = TRUE))
svymean(~competent, svyd, na.rm = TRUE); sqrt(svyvar(~competent, svyd, na.rm = TRUE))
svymean(~smart, svyd, na.rm = TRUE); sqrt(svyvar(~smart, svyd, na.rm = TRUE))

# Goodwill

svymean(~not_slfcentered, svyd, na.rm = TRUE); sqrt(svyvar(~not_slfcentered, svyd, na.rm = TRUE))
svymean(~interests_heart, svyd, na.rm = TRUE); sqrt(svyvar(~interests_heart, svyd, na.rm = TRUE))
svymean(~concerned_wme, svyd, na.rm = TRUE); sqrt(svyvar(~concerned_wme, svyd, na.rm = TRUE))
svymean(~sensitive, svyd, na.rm = TRUE); sqrt(svyvar(~sensitive, svyd, na.rm = TRUE))
svymean(~understanding, svyd, na.rm = TRUE); sqrt(svyvar(~understanding, svyd, na.rm = TRUE))

# Trustworthiness

svymean(~honest, svyd, na.rm = TRUE); sqrt(svyvar(~honest, svyd, na.rm = TRUE))
svymean(~trustworthy, svyd, na.rm = TRUE); sqrt(svyvar(~trustworthy, svyd, na.rm = TRUE))
svymean(~honorable, svyd, na.rm = TRUE); sqrt(svyvar(~concerned_wme, svyd, na.rm = TRUE))
svymean(~ethical, svyd, na.rm = TRUE); sqrt(svyvar(~ethical, svyd, na.rm = TRUE))
svymean(~genuine, svyd, na.rm = TRUE); sqrt(svyvar(~genuine, svyd, na.rm = TRUE))

# Belief in Rehab Statement

svymean(~belief_tx_1, svyd, na.rm = TRUE); sqrt(svyvar(~belief_tx_1, svyd, na.rm = TRUE))
svymean(~belief_tx_2, svyd, na.rm = TRUE); sqrt(svyvar(~belief_tx_2, svyd, na.rm = TRUE))
svymean(~belief_tx_3, svyd, na.rm = TRUE); sqrt(svyvar(~belief_tx_3, svyd, na.rm = TRUE))
svymean(~belief_tx_4, svyd, na.rm = TRUE); sqrt(svyvar(~belief_tx_4, svyd, na.rm = TRUE))

# Belief in Mental Health Claim

svymean(~belief_mh_1, svyd, na.rm = TRUE); sqrt(svyvar(~belief_mh_1, svyd, na.rm = TRUE))
svymean(~belief_mh_2, svyd, na.rm = TRUE); sqrt(svyvar(~belief_mh_2, svyd, na.rm = TRUE))
svymean(~belief_mh_3, svyd, na.rm = TRUE); sqrt(svyvar(~belief_mh_3, svyd, na.rm = TRUE))
svymean(~belief_mh_4, svyd, na.rm = TRUE); sqrt(svyvar(~belief_mh_4, svyd, na.rm = TRUE))

# Reliabilities

alpha(dplyr::select(dta, intelligent, expert, informed, competent, smart))
alpha(dplyr::select(dta, not_slfcentered, interests_heart, concerned_wme, sensitive, understanding))
alpha(dplyr::select(dta, honest, trustworthy, honorable, ethical, genuine))
alpha(dplyr::select(dta, belief_tx_1, belief_tx_2, belief_tx_3, belief_tx_4))
alpha(dplyr::select(dta, belief_mh_1, belief_mh_2, belief_mh_3, belief_mh_4))

#### Measurement Model Specification ####

# Credibility as a single factor

single_factor = '
# Latent Credibility Construct
credibility =~ intelligent + expert + informed + competent + smart + not_slfcentered + 
interests_heart + concerned_wme + sensitive + understanding + honest + 
trustworthy + honorable + ethical + genuine

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4
'

# Credibility as three sub-dimensions

multi_factor = '
# Latent Credibility Construct
competence =~ intelligent + expert + informed + competent + smart
goodwill =~ not_slfcentered + interests_heart + concerned_wme + sensitive + understanding
trustworthiness =~ honest + trustworthy + honorable + ethical + genuine

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4
'

# Credibility as second order factor

second_order = '
# Latent Constructs

competence =~ intelligent + expert + informed + competent + smart
goodwill =~ not_slfcentered + interests_heart + concerned_wme + sensitive + understanding
trustworthiness =~ honest + trustworthy + honorable + ethical + genuine

credibility =~ competence + goodwill + trustworthiness

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4
'

# Fit CFAs
cfa_single = cfa(single_factor, data = dta, 
                 estimator = "MLR",
                 sampling.weights = "weight", 
                 missing = "ml",
                 meanstructure = TRUE)
summary(cfa_single, fit.measures = TRUE, standardize = TRUE)

cfa_multi = cfa(multi_factor, data = dta, 
                estimator = "MLR",
                sampling.weights = "weight", 
                missing = "ml",
                meanstructure = TRUE)
summary(cfa_multi, fit.measures = TRUE, standardize = TRUE)

cfa_2nd_order = cfa(second_order, data = dta, 
                    estimator = "MLR",
                    sampling.weights = "weight", 
                    missing = "ml",
                    meanstructure = TRUE)
summary(cfa_2nd_order, fit.measures = TRUE, standardize = TRUE)

fitmeasures(cfa_single, fit.measures = c("cfi", "tli", "rmsea", "srmr", "aic", "bic"))
fitmeasures(cfa_multi, fit.measures = c("cfi", "tli", "rmsea", "srmr", "aic", "bic"))
fitmeasures(cfa_2nd_order, fit.measures = c("cfi", "tli", "rmsea", "srmr", "aic", "bic"))
anova(cfa_single, cfa_multi, cfa_2nd_order)
rm(cfa_single, cfa_multi, single_factor, multi_factor)

# Measurement Invariance Testing

# All parameters free
config = cfa(second_order, 
             data = dta, 
             sampling.weights = "weight", 
             group = "tx",
             estimator = "MLR",
             missing = "ml", 
             meanstructure = TRUE)

# Fixed loadings (weak/metric invarance)
weak = cfa(second_order, 
           data = dta, 
           group.equal = "loadings",
           sampling.weights = "weight", 
           group = "tx", 
           estimator = "MLR",
           missing = "ml",  
           meanstructure = TRUE)

# Fixed loadings and means (strong/scalar invariance)
strong = cfa(second_order, 
             data = dta, 
             group.equal = c("loadings", "intercepts"),
             sampling.weights = "weight", 
             group = "tx", 
             estimator = "MLR",
             missing = "ml", 
             meanstructure = TRUE)

# Fixed loadings, means, and variances (strict invariance)
strict = cfa(second_order, 
             data = dta, 
             group.equal = c("loadings", "intercepts", "residuals"), 
             sampling.weights = "weight", 
             group = "tx",
             estimator = "MLR",
             missing = "ml", 
             meanstructure = TRUE)
anova(config, weak, strong, strict)

fitmeasures(config, fit.measures = c("cfi.robust", "rmsea.robust", "srmr")) 
fitmeasures(weak, fit.measures = c("cfi.robust", "rmsea.robust", "srmr"))
fitmeasures(strong, fit.measures = c("cfi.robust", "rmsea.robust", "srmr"))
fitmeasures(strict, fit.measures = c("cfi.robust", "rmsea.robust", "srmr")) # Blows up fit
rm(config, weak, strong, strict)

#### Structural Equation Model ####

# Main Effects (Table 2)

main_sem = '
# Latent Constructs

competence =~ intelligent + expert + informed + competent + smart
goodwill =~ not_slfcentered + interests_heart + concerned_wme + sensitive + understanding
trustworthiness =~ honest + trustworthy + honorable + ethical + genuine

credibility =~ competence + goodwill + trustworthiness

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4

# Regressions

credibility ~ tx_sexoffender + tx_professor
belief_rehab ~ credibility + tx_sexoffender + tx_professor
belief_mh ~ credibility + belief_rehab + tx_sexoffender + tx_professor'

main_effects_fit = sem(main_sem, data = dta, 
                       sampling.weights = "weight", 
                       missing = "ml",
                       estimator = "MLR")

standardizedSolution(main_effects_fit, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == "~")

# Interaction Model (Table 2)

full_interaction = '
# Latent Constructs

competence =~ intelligent + expert + informed + competent + smart
goodwill =~ not_slfcentered + interests_heart + concerned_wme + sensitive + understanding
trustworthiness =~ honest + trustworthy + honorable + ethical + genuine

credibility =~ competence + goodwill + trustworthiness

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4

# Regressions

credibility ~ tx_sexoffender + tx_professor + tx_interaction
belief_rehab ~ credibility + tx_sexoffender + tx_professor + tx_interaction
belief_mh ~ credibility + belief_rehab + tx_sexoffender + tx_professor + tx_interaction'

full_int_fit = sem(full_interaction, data = dta, 
                   sampling.weights = "weight", 
                   missing = "ml",
                   estimator = "MLR")
summary(full_int_fit, fit.measures = TRUE)

standardizedSolution(full_int_fit, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == "~")

# Main effects model supported by both AIC and BIC
anova(main_effects_fit, full_int_fit)

# Include Indirect Effects (Table 3)

sem_model = '
# Latent Constructs

competence =~ intelligent + expert + informed + competent + smart
goodwill =~ not_slfcentered + interests_heart + concerned_wme + sensitive + understanding
trustworthiness =~ honest + trustworthy + honorable + ethical + genuine

credibility =~ competence + goodwill + trustworthiness

belief_rehab =~ belief_tx_1 + belief_tx_2 + belief_tx_3 + belief_tx_4
belief_mh =~ belief_mh_1 + belief_mh_2 + belief_mh_3 + belief_mh_4

# Regressions

credibility ~ a1 * tx_sexoffender + a2 * tx_professor
belief_rehab ~ b1 * credibility + c1 * tx_sexoffender + c2 * tx_professor
belief_mh ~ b2 * credibility + d * belief_rehab + e1 * tx_sexoffender + e2 * tx_professor

# Indirect effects

soCredrehab := a1 * b1
soCredMH := a1 * b2
soCredrehabMH := a1 * b1 * d
profCredrehab := a2 * b1
profCredMH := a2 * b2
profCredrehabMH := a2 * b1 * d

# Total Effects

total_sorehab := c1 + (a1 * b1)
total_profrehab := c2 + (a1 * b1)
total_soMH := e1 + (a1 * b2)
total_profMH := e2 + (a1 * b2)
'

sem_fit = sem(sem_model, data = dta, 
              sampling.weights = "weight", 
              missing = "ml",
              estimator = "MLR")
summary(sem_fit, fit.measures = TRUE, standardized = TRUE)

# Standardized estimates
standardizedSolution(sem_fit, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == "~")

# Indirect and Total Effects
standardizedSolution(sem_fit, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == ":=")

#### Check Analysis for Sensitivity to Weights ####

# Kish approximation for design effect

dta %>%
  summarize(ess = sum(weight)^2 / sum(weight^2),
            design_effect = n() / ess)

## Unweighted fit

# Main Effects
main_effects_fituw = sem(main_sem, data = dta, 
                         missing = "ml",
                         estimator = "MLR")

standardizedSolution(main_effects_fituw, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == "~")

# Indirect and Total Effects
sem_fit = sem(sem_model, data = dta, 
              missing = "ml",
              estimator = "MLR")

standardizedSolution(sem_fit, ci = TRUE, level = 0.95, type = "std.lv") %>%
  filter(op == ":=")

