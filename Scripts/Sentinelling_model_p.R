# ---------------
# Title: Vigilance model
# Date: 21 april2025
# Author: mgranellruiz
# Goal: 
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
source('/Users/mariagranell/Repositories/data/diagnostic_fcns.r')
# plotting
library(patchwork)
library(ggplot2)
library(ggside)
library(ggpubr)
library(gridExtra)
library(ggtext)
# models
library(lme4)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(DHARMa)
library(sjPlot)
library(glmmTMB)
library(effects)
library(car)

# path ------------------------
setwd()

# prepare dataframe, the data is not publically available since there is considerable trimming of the original dataframe.
# but the data wrangling is kept for transparency
{
# data ------------------------
{
# to create this file I first used this file: "/Users/mariagranell/Repositories/data/acess_data/OutputData/vigilance_access.csv"
# ran it through the /Users/mariagranell/Repositories/elo-sociality/elo/Hierarchies_for_all_groups_best.R
# then through the CSI script and then back here.
FOCAL_org <- #read.csv("/Users/mariagranell/Repositories/male_services_index/darting_combined_cleaned/data/CleanData/cleanedcombinedFocal2022-06_2023-5_MS.csv") #%>%
  read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/vigilance_allmyfiles.csv") %>% filter(Total > 0)
rank <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_vigilance_maleservices.csv") %>% mutate(Date = ymd(Date))
csi <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_vigilance_maleservices.csv") %>% mutate(Date = ymd(Date)) %>%
  dplyr::select(-Warning, -ActualDate) # the warning is ok, investigated
sexual <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_vigilance_MS.csv") %>% mutate(Date = ymd(Date))
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
}
# Parameters
MSGroups = c("AK", "BD", "KB", "NH")
unhabituated_cutoff_date = 365 # individuals will be considered habituated only after 1/2 a year, instead of 365, a full year

# preparation dataframe ------
{
focal <- FOCAL_org %>%
  filter(Group %in% MSGroups) %>%
  mutate(Date = ymd(Date)) %>%
  add_season("Date") %>%
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("IDIndividual1" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% "adult") %>%
  add_group_composition("Group", "Date")

  write.csv(focal, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/vigilance_maleservices_basedf.csv", row.names = F)

# unhabituated males ----
# to calculate unhabituation I will need the info if they were birned in the IVP and for how long they have been in the IVP
unhabituation  <- lh %>%
  filter(!is.na(StartDate_mb)) %>%
  group_by(AnimalCode) %>%
  mutate(BornedInIVP = if_else(any(Tenure_type == "BirthGroup"), "yes", "no"),
         FirstDate = min(StartDate_mb, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(AnimalCode, BornedInIVP, FirstDate) %>% distinct()

 # Father calculation
first_father_dates <- focal %>%
  filter(Sex == "M", ) %>%
  distinct(IDIndividual1, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  )

focal1 <- focal %>%
  left_join(rank, by = c("IDIndividual1", "Group", "Date", "Sex", "Age_class")) %>%
  left_join(csi, by = c("IDIndividual1" = "AnimalCode", "Group", "Date")) %>%
  left_join(sexual, by = c("IDIndividual1", "Group", "Date")) %>%
  left_join(first_father_dates, by =c("IDIndividual1", "StartDate_mb", "EndDate_mb")) %>%
  mutate(# season is an ordered factor
         Tenure = as.numeric(ymd(Date)- ymd(StartDate_mb)),
         TenureYears = as.numeric(ymd(Date) - ymd(StartDate_mb)) / 365.25,
         Father = ifelse(Date > FirstBabySeason, "Yes", "No"),
         elo=ifelse(Age_class == "sub-adult", NA, ELO),
         Season = factor(Season,
                      levels = c("Summer", "Mating", "Winter", "Baby"),
                      ordered = TRUE)) %>%
  distinct() %>%
  mutate(prop_babies =n_babies/n_members) %>%
  # calculate unhabituation
  left_join(unhabituation, by= c("IDIndividual1" = "AnimalCode")) %>%
  mutate(Unhabituated = ifelse(BornedInIVP == "no" & difftime(Date, FirstDate) < unhabituated_cutoff_date, "yes", "no"),
         Age = if_else(BornedInIVP == "yes", add_age(DOB_estimate, Date, "Years"), NA),
  )

table(focal1$Sex)

### fatherhood updated WRONG -> LOOK AT VLA
# Calculate the start of tenure (recorded date minus the days present)
#focal2 <-  focal1 %>%
#  mutate(Tenure = as.numeric(ymd(Date)- ymd(StartDate_mb)),
#         TenureYears = as.numeric(ymd(Date) - ymd(StartDate_mb)) / 365.25,
#         MSEndDate = ymd(Date)) %>%
#  # To caluclate potential fatherhood
#  group_by(IDIndividual1, Group) %>%      # Ensure calculations are per individual
#  arrange(MSEndDate) %>%                  # Make sure records are in chronological order
#  mutate(
#  # Determine the mating season year of the monkey
#  # If the record is in the first three months (month < 4), assume mating season was the previous year
#  mating_year = if_else(month(MSEndDate) < 4, year(MSEndDate) - 1, year(MSEndDate)),
#
#  # Define mating season start and end for that year.
#  mating_start = as.Date(paste0(mating_year, "-03-01")),
#  mating_end   = as.Date(paste0(mating_year, "-07-30")),

  # Create a flag for records that meet the conditions:
  # The individual must have been present from before the mating season ended,
  # and the record date must be after the mating season ended,
  # and tenure must be greater than 0.49.
#  flag = if_else(StartDate_mb <= mating_end & Season == "Baby", 1, 0),
  # Use a cumulative maximum on the flag so that once a record qualifies (flag == 1),
  # all subsequent records are marked as "Yes".
#  Father = case_when(
#      mating_year > 1 ~ "Yes",  # override if tenure is > 1 year
#      cummax(flag) == 1 ~ "Yes",
#      TRUE ~ "No"
#    )
#  ) %>%
#  ungroup() %>% mutate(Father = ifelse(Sex == "F", NA, Father)) %>%
#  #dplyr::select( -mating_year, -mating_end, -mating_start, -flag) %>%
#  distinct()

}

# list of unhabitatued individuals
#focal1 %>% filter(Unhabituated == "yes") %>% pull(IDIndividual1) %>% unique()
# from this list apparently Fur was already habituated
focal1 <- focal1 %>% mutate(Unhabituated = ifelse(IDIndividual1 == "Fur", "no", Unhabituated))

range(focal1$Date)
#write.csv(focal1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/vigilance_modeldataframe.csv", row.names = FALSE)

  focal1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/vigilance_modeldataframe.csv")%>%
    select(Sex, asr, n_males, n_members, Season, Group, IDIndividual1, Total, Vigilant, Unhabituated, Date,
           ELO, ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, EndDate_mb, Age_class)
  write.csv(focal1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/sentinelling_modeldataframe_p.csv", row.names = FALSE)

}

# public data is provided from this step forward
# already with vigilance bigets calculated in seconds.
focal1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/sentinelling_modeldataframe_p.csv")

# MODEL 1 - sex differences
{ model_data_model1_vig <- focal1 %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode =IDIndividual1, Total, Vigilant, Unhabituated, Date) %>%
    drop_na() %>% distinct() %>%
    mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[,1]) %>%
  # remove outlier destected with testOutliers(res)
  filter(Vigilant < 700)

  range(model_data_model1_vig$Date)

  # Simlpe visualization to understand varialbes
  ggbetweenstats(model_data_model1_vig, x = Sex, y = Vigilant)
  model_data_model1_vig %>% mutate(porp = log(Vigilant/Total))
  a <- model_data_model1_vig %>% mutate(porp = log(Vigilant/Total)) %>% filter(Sex == "M") %>% ggscatterstats( x = asr, y = porp)
  b <- model_data_model1_vig %>% mutate(porp = log(Vigilant/Total)) %>% filter(Sex == "F") %>% ggscatterstats( x = asr, y = porp)
  a + b

  # Distribution
  descdist(model_data_model1_vig$Vigilant)
  hist(model_data_model1_vig$Vigilant)
  plot(fitdist(model_data_model1_vig$Vigilant, "nbinom")) # nbinom is the best fit

  # model
model <- glmmTMB(
  Vigilant ~ Sex * (asr_z + Season) + (1 | AnimalCode) + Group,
  ziformula = ~Sex + Season + Group,
  dispformula = ~ Season + Sex,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data_model1_vig # model_data_model1_vig[-outliers_list,], no change in results
)

  # model checks, all good
  res <- simulateResiduals(model); plot(res)
  {
testZeroInflation(res)
#testDispersion(model) # no need to check overdispersion for nbinom
testOutliers(res, type = "bootstrap") # outlier test is not significant.
  # Despite the outlier test not being significant I ran the model again removing those 33 outliers
  # model_data_model1_vig[-outliers_list,] and the main effect significance stayed.
outlier_list <- outliers(res)
# Test for temporal autocorrelation
plot(acf(resid(model))) # all good

  # homoscedasticity checks
plotResiduals(res, model_data_model1_vig$Sex) # good
  # There was a different between sexes due to more 0 in females and variance in Sex
  # Thus we included ~Sex in the Zi formula and allow the variance to change with Sex
  # table(model_data_model1_vig$Sex[model_data_model1_vig$Vigilant == 0])
plotResiduals(res, model_data_model1_vig$asr_z) # minimal desviations. Is Ok!
  # just in case I modeled asr with a splinter to allow for non-linear relationships, which did not imporve the fit
  # Vigilant ~ Sex * (ns(asr_z, df = 3) + Season) + Unhabituated + ...
plotResiduals(res, model_data_model1_vig$Season) # good
  # Same explanation as in Sex; table(model_data_model1_vig$Season[model_data_model1_vig$Vigilant == 0])
plotResiduals(res, model_data_model1_vig$Unhabituated) # good
plotResiduals(res, model_data_model1_vig$Group) # good
  # uneven amount of 0 between groups: table(model_data_model1_vig$Group[model_data_model1_vig$Vigilant == 0])

  # normality of random effects
# slight desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])
}

  # null model check. The addition of sex is significant
  { null_model <- glmmTMB(
  Vigilant ~  asr_z + Season + (1 | AnimalCode) + Group,
  ziformula = ~ Season + Group,
  dispformula = ~ Season,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data_model1_vig
)
    anova(null_model, model)}

  # Results
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE); Anova(model)
plot(allEffects(model))

      # effect sizes
  standardized_effects(model)


  # significance
# sex
  model_data_model1_vig %>% group_by(Sex) %>% summarize(perc = mean(Vigilant/Total * 100)) # probability

  #sex:season
  emmeans(model, ~ Sex | Season) %>% contrast(method = "pairwise") %>% summary(infer = TRUE)
  model_data_model1_vig %>% group_by(Season, Sex) %>% summarize(perc = mean(Vigilant/Total * 100))

  # asr
  emtrends(model, ~ Sex, var = "asr_z") %>% summary(infer = TRUE)
  plot(effect("Sex:asr_z", model))

    # for table
  anova_results <- Anova(model) %>% broom::tidy() |> dplyr::select(Term = term, Chisq = statistic, Df = df, p.value)
  beta_results <- standardized_effects(model) |>
  dplyr::mutate(
    Term = dplyr::case_when(
      str_detect(Parameter, "^SexM:Season") ~ "Sex:Season",
      str_detect(Parameter, "^SexM:Threat") ~ "Sex:Threat",
      str_detect(Parameter, "^SexM:asr_z")  ~ "Sex:ASR",
      str_detect(Parameter, "^Season") ~ "Season",
      str_detect(Parameter, "^Group")  ~ "Group",
      str_detect(Parameter, "^Threat") ~ "Threat",
      str_detect(Parameter, "^asr_z")  ~ "ASR",
      str_detect(Parameter, "^SexM$")  ~ "Sex",
      TRUE ~ Parameter
    )) |>
  group_by(Term) |> summarise(beta_std = max(abs(Std_Coefficient), na.rm = TRUE), .groups = "drop")
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_sent.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_sent.csv", row.names = F)
# for plotting
  eff_vig_sex <- as.data.frame(effect("Sex", model))
  write.csv(eff_vig_sex, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_vig_sex.csv", row.names = F)

}

# MODEL 2 - hypothesis testing, differences among males
{
  model_data_base <- focal1 %>%
  filter(Sex == "M", Age_class == "adult") %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode = IDIndividual1, Total, Vigilant,
                elo = ELO, elo_12m = ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated,
                Date, EndDate_mb
  ) %>%
  filter(Vigilant < 700) %>%   # remove outlier destected with testOutliers(res)
  mutate(elo = as.numeric(na_if(elo, "Date out of bounds"))) %>%
  drop_na() %>% distinct() %>%
  mutate(prop = log(Vigilant/Total+1))
model_data <- model_data_base%>% # scale all variables
  mutate(across(c(elo_12m, zCSI, asr, mount_coming12, mount_last12),
                ~ scale(.x, center = TRUE, scale = TRUE)[,1]),
   across(where(is.character), as.factor))

  range(model_data$Date)

nrow(table(model_data$AnimalCode))

  #distribution
descdist(model_data$Vigilant)
hist(model_data$Vigilant)
plot(fitdist(model_data$Vigilant, "nbinom"))

  # random slopes check
{
random_slopes <- fe.re.tab(
  fe.model =
    "Vigilant ~ elo_12m * (Father + mount_coming12 + mount_last12) +
     Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group",
  re = "(1|AnimalCode)",
  data = model_data
)

support_tbl <- tibble(
  name = names(random_slopes$summary),
  support = map_lgl(random_slopes$summary, flag_support)
) %>%
  mutate(
    re = case_when(
      str_detect(name, "within_AnimalCode") ~ "AnimalCode",
      TRUE ~ NA_character_
    )
  )

  model_complex <- glmer(Vigilant ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group +
    (1 + elo_12m + mount_coming12 + mount_last12 | AnimalCode), offset = log(Total),
     data = model_data, family=poisson()
    , control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000)))

  summary(model_complex)
}

model <-
  glmmTMB(
  Vigilant ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1 + mount_coming12 + elo_12m + mount_last12||AnimalCode),
  ziformula = ~ elo_12m + mount_coming12 + Season,
  dispformula = ~ Season + asr,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data
)

  # model checks, all good
  res <- simulateResiduals(model); plot(res)
  {
testZeroInflation(res)
#testDispersion(model) # no need to check overdispersion for nbinom
testOutliers(res, type = "bootstrap") # outlier test is not significant.
  # Despite the outlier test not being significant I ran the model again removing those 31 outliers
  # model1_data[-outliers_list,] and the main effect significance stayed.
outlier_list <- outliers(res)
# Test for temporal autocorrelation
plot(acf(resid(model))) # all good

  # homoscedasticity checks
plotResiduals(res, model_data$elo_12m) # good
  # Because of the amount of 0 we allowed the model to adjust the zi formula to elo_12m
plotResiduals(res, model_data$Father) # good
plotResiduals(res, model_data$mount_coming12) # good.
plotResiduals(res, model_data$mount_last12) # ok.
plotResiduals(res, model_data$zCSI) # ok
plotResiduals(res, model_data$asr) # minimal desviations, good.
plotResiduals(res, model_data$Season) # good
plotResiduals(res, model_data$Unhabituated) # good
plotResiduals(res, model_data$Group) # good

  # normality of random effects
# desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
lm_no_interactions <- lm(
  Vigilant ~ elo_12m + Father +
    mount_coming12 + mount_last12 + Season + zCSI + asr + Unhabituated + Group,
  data = model_data
)
car::vif(lm_no_interactions)
}

  # null model check. testing hypothesis improves fit!
  {null_model <-    glmmTMB(
  Vigilant ~
    Season + zCSI + asr  + Season:mount_coming12 +
    Unhabituated + Group + (1 ||AnimalCode),
  ziformula = ~  Season,
  dispformula = ~ Season + asr,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data
)
    res <- simulateResiduals(null_model); plot(res) #ok
    anova(model, null_model)
  }

summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE); Anova(model)
plot(allEffects(model))

  print(standardized_effects(model), n = Inf)

  plot(effect("Group", model))

  # effect interaction elo father
  plot(effect("elo_12m*Father", model)) # when males are not fathers they increase their alarm calloing with rank
  emtrends(model, ~ Father, var = "elo_12m") %>% summary(infer = TRUE)
  emmeans(model, ~ elo_12m | Father)
  ggpredict(model, terms = c("elo_12m [0, 0.5, 1]", "Father"), type = "fixed") # in non fathers prob alarm: 19% vs. 7% high vs low rankers.
  plot(ggpredict(model, terms = c("elo_12m [0, 0.5, 1]", "Father"), type = "fixed"))

  # seasonal
  plot(effect("Season", model))
  emmeans(model, ~ Season) %>% contrast(method = "pairwise") %>% summary(infer = TRUE)
  emmeans(model, ~ Season, type = "response")
  model_data %>% group_by(Season) %>%summarize(per=mean(Vigilant/Total)*100)

  # season and mount coming
  plot(effect("mount_coming12:Season", model))
  emtrends(model, ~ Season, var = "mount_coming12") %>% summary(infer = TRUE)

  # elo mounts
  plot(effect("elo_12m:mount_coming12", model))
  emtrends(model, ~ elo_12m, var = "mount_coming12", at = list(elo_12m = c(0, 0.5, 1))) %>%
    summary(infer = TRUE) %>% as_tibble() %>%
    dplyr::select(elo_12m, estimate = mount_coming12.trend, SE, asymp.LCL, asymp.UCL, z.ratio, p.value)
  plot(ggpredict(model, terms = c("mount_coming12", "elo_12m [0, 0.5, 1]"), type = "fixed"))
    # Random effects relevance, is Animal identity relevant?
{
  model_re_full <- glmmTMB(
  Vigilant ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1|AnimalCode),
  ziformula = ~ elo_12m + mount_coming12 + Season,
  dispformula = ~ Season + asr,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data
)
  model_re_noAnimalCode  <- glmmTMB(
  Vigilant ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group,
  ziformula = ~ elo_12m + mount_coming12 + Season,
  dispformula = ~ Season + asr,  # Allow dispersion to vary by predictors
  offset = log(Total),
  family = nbinom2(),
  data = model_data
)
  anova(model_re_full, model_re_noAnimalCode)
  summary(model_re_full) #Animal Code 0.065 (0.25)
  # there is an effect of AnimalCode
}

    # for table
  anova_results <- Anova(model) %>% broom::tidy() |> dplyr::select(Term = term, Chisq = statistic, Df = df, p.value)
  beta_results <- standardized_effects(model) |>
    dplyr::mutate(
      Term = dplyr::case_when(

        # ---- 2-way interactions
        str_detect(Parameter, "^elo_12m:Father") ~ "elo_12m:Father",
        str_detect(Parameter, "^elo_12m:mount_coming12") ~ "elo_12m:mount_coming12",
        str_detect(Parameter, "^elo_12m:mount_last12") ~ "elo_12m:mount_last12",
        str_detect(Parameter, "^mount_coming12:Season") ~ "mount_coming12:Season",

        # ---- main effects
        str_detect(Parameter, "^elo_12m$") ~ "elo_12m",
        str_detect(Parameter, "^FatherYes$") ~ "Father",
        str_detect(Parameter, "^mount_coming12$") ~ "mount_coming12",
        str_detect(Parameter, "^mount_last12$") ~ "mount_last12",
        str_detect(Parameter, "^Season") ~ "Season",
        str_detect(Parameter, "^zCSI$") ~ "zCSI",
        str_detect(Parameter, "^asr$") ~ "asr",
        str_detect(Parameter, "^Unhabituated") ~ "Unhabituated",
        str_detect(Parameter, "^Group") ~ "Group",

        TRUE ~ NA_character_
      )
    ) |>
  group_by(Term) |> summarise(beta_std = max(abs(Std_Coefficient), na.rm = TRUE), .groups = "drop")
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_sent.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_sent.csv", row.names = F)

}

# save effects of models plots
{
eff_sentinelling_elofather <- as.data.frame(effect("elo_12m*Father", model, xlevels = list(elo_12m = seq(0, 1, length.out = 100))))

  # pick a reference Total (i.e. the medial of Total) to turn predicted counts into proportions
Total_ref <- model_data %>%
  dplyr::summarise(Total_ref = median(Total, na.rm = TRUE)) %>%
  dplyr::pull(Total_ref)

  # build prediction grid; selecting 100 values of elo and father, non father.
newdat <- tidyr::expand_grid(
  elo_12m = seq(0, 1, length.out = 100),
  Father = sort(unique(model_data$Father))
) %>%
  dplyr::mutate(
    Total = Total_ref,
    mount_coming12 = 0,
    mount_last12   = 0,
    Season         = levels(model_data$Season)[1],
    zCSI           = 0,
    asr            = 0,
    Unhabituated   = levels(model_data$Unhabituated)[1],
    Group          = levels(model_data$Group)[1],
    AnimalCode     = NA
  )

# predict expected COUNT of Vigilant for Total_ref, then divide by Total_ref -> proportio
  # but only considering the fixed effects.
pred <- predict(
  model,
  newdata = newdat,
  type = "response",
  se.fit = TRUE,
  re.form = NA,      # population-level (no random effects)
  allow.new.levels = TRUE
)

eff_sentinelling_elofather <- newdat %>%
  dplyr::mutate(
    fit_cnt = pred$fit,
    se_cnt  = pred$se.fit,
    lower_cnt = pmax(fit_cnt - 1.96 * se_cnt, 0),
    upper_cnt = fit_cnt + 1.96 * se_cnt,
    fit   = fit_cnt   / Total_ref,
    lower = lower_cnt / Total_ref,
    upper = upper_cnt / Total_ref
  ) %>%
  dplyr::select(elo_12m, Father, fit, lower, upper)

write.csv(eff_sentinelling_elofather, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_elofather.csv", row.names = F)

ggplot(eff_sentinelling_elofather, aes(x = elo_12m, y = fit, color = Father, fill = Father)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(x = "Dominance rank (Elo score)",
       y = "Predicted probability of sentinelling",
       color = "Sired offspring",
       fill = "Sired offspring") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


#eff_sentinelling_mountcoming <- ggpredict(model, terms = "mount_coming12 [all]") %>% as.data.frame()
eff_sentinelling_mountcoming <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_coming12")
#write.csv(eff_sentinelling_mountcoming, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_mountcoming.csv", row.names = FALSE)

ggplot(eff_sentinelling_mountcoming, aes(y = var_to_plot_raw, x = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(xmin = conf.low, xmax = conf.high), alpha = 0.2, linetype = 0) +
  #labs(x = "Coming mounts in next year",
  #     y = "Predicted probability of sentinelling calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

eff_sentinelling_mountcoming_past <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_last12")
write.csv(eff_sentinelling_mountcoming_past, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_mountcoming_past.csv", row.names = FALSE)

ggplot(eff_sentinelling_mountcoming_past, aes(y = var_to_plot_raw, x = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(xmin = conf.low, xmax = conf.high), alpha = 0.2, linetype = 0) +
  #labs(x = "Coming mounts in next year",
  #     y = "Predicted probability of sentinelling calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
}
