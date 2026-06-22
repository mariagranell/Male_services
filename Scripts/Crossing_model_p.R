# ---------------
# Title: Crossing model
# Date: 28 aug
# Author: mgranellruiz
# Goal: model the crossing behaviour. answer the question which males cross first?
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
library(glmmTMB)
library(sjPlot)
library(rstatix)
library(effects)
library(emmeans)

# path ------------------------
setwd()


# prepare dataframe, the data is not publically available since there is considerable trimming of the original dataframe.
# but the data wrangling is kept for transparency
{
# data ------------------------
{#crossing1 <- read.csv("/Users/mariagranell/Downloads/Candidates.csv")
crossing <- read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/crossing_cybertracker.csv")
rank <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_crs_maleservices.csv") %>% mutate(Date = ymd(Date))
csi <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_crs_maleservices.csv") %>% mutate(Date = ymd(Date)) %>%
  dplyr::select(-Warning, -ActualDate) # the warning is ok, investigated
sexual <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_crs_MS.csv") %>% mutate(Date = ymd(Date))
unhabituated_cutoff_date = 365 # individuals will be considered habituated only after 1/2 a year, instead of 365, a full year
}
plot_weekly_summary(crossing, "Data", "Date")

range(crossing$Date)


# preparation df
{
# crossing that are promissing
crs <- crossing %>%
  # select only dangerous river crosings
  filter(CrossingType %in% c("Fence", "River - Ground Level", "River - Swimming"),
         Behaviour == "First Crosser",
         Group %in% c("AK", "NH", "BD", "KB")
  ) %>%
  # select only the crossing in where an adult male was seen crossing
  left_join(lh %>% dplyr::select(AnimalCode, Sex, DOB_estimate, Tenure_type, StartDate_mb, EndDate_mb) %>% distinct(),
            by = c("IDIndividual1" = "AnimalCode"), relationship = "many-to-many") %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"),
         AgeClass = get_age_class_w_tenuretype(Sex,Age, Tenure_type)) %>%
  filter(StartDate_mb < Date & EndDate_mb > Date,
         #AgeClass == "AM"
  ) %>% distinct()

  table(crs$AgeClass, crs$CrossingType)

# extra filter. Only select crossing in where at least 10% of the group had crossed
crs_atleast_tenpercent <-
  crossing %>%
  filter(Obs.nr %in% crs$Obs.nr,
         Behaviour %in% c("Crossing","First Crosser","Last Crosser")) %>%
  count(Obs.nr, Date, Group, name = "n_cross") %>%                      # crossers per event
  left_join(
    lh %>% dplyr::select(Group_mb, StartDate_mb, EndDate_mb, AnimalCode),
    by = join_by(Group == Group_mb, Date > StartDate_mb, Date > EndDate_mb)  # members present at that date
  ) %>%
  group_by(Obs.nr, Date, Group, n_cross) %>%
  summarise(n_members = n_distinct(AnimalCode), .groups = "drop") %>%
  mutate(prop_crossed = n_cross / n_members) %>%
  filter(prop_crossed >= 0.10)

crs_keep <- crs %>% filter(Obs.nr %in% crs_atleast_tenpercent$Obs.nr, Obs.nr != 835) %>% # 835 has two frist crossers
  dplyr::select(Date, Obs.nr, Group, CrossingType, IDIndividual1, Season) %>%
  distinct() %>%
  # add the list of AM that were present int he crossings.
  left_join(
    lh %>% dplyr::select(Group_mb, StartDate_mb, EndDate_mb, AnimalCode, Sex, DOB_estimate, Tenure_type),
    by = join_by(Group == Group_mb, Date > StartDate_mb, Date < EndDate_mb)  # members present at that date
  ) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"),
         AgeClass = get_age_class_w_tenuretype(Sex,Age, Tenure_type),
         FirstCrosser = ifelse(IDIndividual1 == AnimalCode, 1, 0)
  )

    table(crs_keep$CrossingType)
crs_keep_males <- crs_keep %>% filter(AgeClass == "AM")
  # base dataframe for the other calculations
write.csv(crs_keep_males, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/crossing_maleservices_basedf.csv", row.names = F)

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
first_father_dates <- crs_keep %>%
  filter(Sex == "M", ) %>%
  distinct(AnimalCode, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  )

crossing1 <- crs_keep %>% mutate(Age_class = "adult", Date = ymd(Date)) %>%
  left_join(rank, by = c("AnimalCode", "Group", "Date", "Sex", "Age_class")) %>%
  left_join(csi, by = c("AnimalCode" = "AnimalCode", "Group", "Date")) %>%
  left_join(sexual, by = c("AnimalCode", "Group", "Date")) %>%
  left_join(first_father_dates, by =c("AnimalCode", "StartDate_mb", "EndDate_mb")) %>%
  mutate(# season is an ordered factor
         Tenure = as.numeric(ymd(Date)- ymd(StartDate_mb)),
         TenureYears = as.numeric(ymd(Date) - ymd(StartDate_mb)) / 365.25,
         Father = ifelse(Date > FirstBabySeason, "Yes", "No"),
         elo=ifelse(Age_class == "sub-adult", NA, ELO),
         Season = factor(Season,
                      levels = c("Summer", "Mating", "Winter", "Baby"),
                      ordered = TRUE)) %>%
  distinct() %>% add_group_composition("Group", "Date") %>% distinct() %>%
  mutate(prop_babies =n_babies/n_members) %>%
  # calculate unhabituation
  left_join(unhabituation, by= c("AnimalCode" = "AnimalCode")) %>%
  mutate(Unhabituated = ifelse(BornedInIVP == "no" & difftime(Date, FirstDate) < unhabituated_cutoff_date, "yes", "no"),
         Age = if_else(BornedInIVP == "yes", add_age(DOB_estimate, Date, "Years"), NA),
  ) %>% # calculate how much longer the males will be present in the group. If you will die it dosen´t count.
  left_join(lh[,c("AnimalCode", "Group_mb", "Fate_probable")],
            by = c("AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  mutate(TenureLeft = case_when(Fate_probable == "dead" ~ NA, TRUE ~ difftime(EndDate_mb, Date, unit = "days")/365))

### fatherhood updated WRONG -> LOOK AT VLA
# Calculate the start of tenure (recorded date minus the days present)
#focal2 <-  focal1 %>%
#  mutate(Tenure = as.numeric(ymd(Date)- ymd(StartDate_mb)),
#         TenureYears = as.numeric(ymd(Date) - ymd(StartDate_mb)) / 365.25,
#         MSEndDate = ymd(Date)) %>%
#  # To caluclate potential fatherhood
#  group_by(AnimalCode, Group) %>%      # Ensure calculations are per individual
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

#write.csv(crossing1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/crs_modeldataframe.csv", row.names = FALSE)
crossing1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/crs_modeldataframe.csv")
  crossing1 <- crossing1 %>%  filter(CrossingType != "Fence") %>%
    mutate(elo_categories = case_when(
    ELO_12m == 1 ~ "dominant",
    TRUE ~ "subordinate"),
  TenureLeft =  as.numeric(difftime(EndDate_mb, Date, unit = "days")/365),
         male_stage_carrear = as.factor(case_when(
           elo_categories == "dominant" & Father == "Yes" ~ "DominantFather",
           elo_categories == "subordinate" & Father == "Yes" ~ "SubFather",
           elo_categories == "dominant" & Father == "No" ~ "DominantNonfather",
           elo_categories == "subordinate" & Father == "No" ~ "SubNonfather"
         )))
write.csv(crossing1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/crs_modeldataframe_p.csv", row.names = FALSE)

}

# public data is provided from this step forward
# already in a binomial format with 1 for leading river crossing and 0 for no.
# Only river crossing events in where all participants were identified
crossing1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/crs_modeldataframe_p.csv")

# MODEL 1
{
model_data_model1_crs <- crossing1 %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, EventID = Obs.nr,
                Unhabituated, FirstCrosser,  CrossingType, Date,
                AM,AF
  ) %>%
   mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1] ) %>%
  drop_na()%>%
  distinct() # the amount of duplicates removes is due to differences in GPS format but still ame info.
               # same for the 1 second difference in Time

table(model_data_model1_crs$CrossingType)
ggbetweenstats(model_data_model1_crs, x = Sex, y = FirstCrosser)


model <-
  glmmTMB(
  FirstCrosser ~
    Sex * (asr_z + Season)
    + Group + (1 | AnimalCode) + (1|EventID),
  family = binomial,
  data = model_data_model1_crs,
  offset = log(n_males))

  # model checks, all good
  res <- simulateResiduals(model); plot(res)
  {
testZeroInflation(res) # good
testDispersion(model) # good
testOutliers(res, type = "bootstrap") # good
# Test for temporal autocorrelation
plot(acf(resid(model))) # good

  # homoscedasticity checks
plotResiduals(res, model_data_model1_crs3$Sex) # good
plotResiduals(res, model_data_model1_crs3$Threat) # good
plotResiduals(res, model_data_model1_crs3$asr) # minimal desviations, good.
plotResiduals(res, model_data_model1_crs3$Season) # good
plotResiduals(res, model_data_model1_crs3$Unhabituated) # good
plotResiduals(res, model_data_model1_crs3$Group) # good

  # normality of random effects
# slight desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
  vif_model <- lm(
  participation_alarm ~ Sex * Threat + asr + Season + Unhabituated +
    + Group,
  data = model_data_model1_crs3); vif(vif_model)

}

    # null model check. The addition of sex is significant
  {null_model <-   glmmTMB(
  FirstCrosser ~ asr_z + Season + Group + (1 | AnimalCode) + (1|EventID),
  family = binomial,
  data = model_data_model1_crs,
  offset = log(n_males))
    anova(null_model, model)}

  # Results
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE); Anova(model)
plot(allEffects(model))

      # effect sizes
  standardized_effects(model)

  # sex
  emmeans(model, ~ Sex, type = "response") # probabilities

  # sex: treat
  plot(effect("Sex*Threat", model)) # males alarm call more for terrestial predators
  emmeans(model, ~ Sex | Threat, type = "response") # probabilites
  emmeans(model, ~ Sex | Threat) %>% contrast(method = "pairwise") %>% summary(infer = TRUE) # comparision

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
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_crs.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_crs.csv", row.names = F)

  # for plotting
  eff_crs_sex <- as.data.frame(effect("Sex", model))
  write.csv(eff_crs_sex, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_crs_sex.csv", row.names = F)

}

# MODEL 2
{
model_data_base <- crossing1 %>%
  filter(Sex == "M") %>%
  dplyr::select(asr, n_males, n_members, Season, Group, AnimalCode, EventID = Obs.nr, Unhabituated, FirstCrosser, CrossingType,
                Sex, asr, n_males, n_members, Season, Group,
                elo = ELO, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated, Date,
                EndDate_mb
  ) %>%
  distinct()
model_data <- model_data_base%>%
  mutate(across(c(elo_12m, zCSI, asr, mount_coming12, mount_last12),
                ~ scale(.x, center = TRUE, scale = TRUE)[,1])) %>%
  drop_na()

nrow(table(model_data$AnimalCode))
aa <- model_data%>% dplyr::select(EventID, CrossingType) %>% distinct()
table(aa$CrossingType)

    # random slopes check
{
random_slopes <- fe.re.tab(
  fe.model =
    "FirstCrosser ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
      Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group ",
  re = "(1| AnimalCode) +(1|EventID)",
  data = model_data
)

support_tbl <- tibble(
  name = names(random_slopes$summary),
  support = map_lgl(random_slopes$summary, flag_support)
) %>%
  mutate(
    re = case_when(
      str_detect(name, "within_AnimalCode") ~ "AnimalCode",
      str_detect(name, "within_EventID")   ~ "EventID",
      TRUE ~ NA_character_
    )
  )

  model_complex <- glmer(FirstCrosser ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
      Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group +
    (1 + elo_12m + mount_coming12 + mount_last12  | AnimalCode) +
    (1 + elo_12m + mount_coming12 + mount_last12  | EventID),
      #(1 + elo_12m + mount_coming12 + mount_last12  | AnimalCode) +
      #(1 | EventID),
    offset = log(n_males),
     data = model_data, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000)))

  summary(model_complex)
}
range(model_data$Date)


model <-
  glmmTMB(
  FirstCrosser ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
      Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group +
      (1 + mount_coming12 + elo_12m + mount_last12|| AnimalCode) +(1|EventID),
  family = binomial,
  data = model_data,
  offset = log(n_males),
  control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS")
))

  # model checks, good enough, test not significant
  res <- simulateResiduals(model); plot(res)
  {
testZeroInflation(res) # good
testDispersion(model) # good
testOutliers(res, type = "bootstrap") # good
# Test for temporal autocorrelation
plot(acf(resid(model))) # good

  # homoscedasticity checks, levene test not important for binomial models
plotResiduals(res, model_data$elo_12m) # good
plotResiduals(res, model_data$Father) # good
plotResiduals(res, model_data$mount_coming12) # good
plotResiduals(res, model_data$mount_last12) # good
plotResiduals(res, model_data$zCSI) # good
plotResiduals(res, model_data$Season) # good
plotResiduals(res, model_data$Unhabituated) # good check
plotResiduals(res, model_data$Group) # good

  # normality of random effects
# slight desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])
qqnorm(ranef(model)$cond$EventID[[1]]); qqline(ranef(model)$cond$EventID[[1]])

      # check mulicolinearity. All good
  vif_model <- lm(
  FirstCrosser ~
    elo_12m + Father + mount_coming12 + mount_last12 + Season + zCSI + Unhabituated +
    + Group,
  data = model_data); vif(vif_model)
}

  # null model check. testing hypothesis marginally improves fit!
{ null_model <-
  glmmTMB(
  FirstCrosser ~
      Season + zCSI + asr + Season +
     Unhabituated + Group +
      (1 | AnimalCode) +(1|EventID),
  family = binomial,
  data = model_data,
  offset = log(n_males))

  res <- simulateResiduals(null_model); plot(res)
  anova(null_model,model)
}

# Results
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(model)
plot(allEffects(model))

  print(standardized_effects(model), n = Inf)

  # significance
  emmeans(model, ~ Father|elo_12m, type = "response") # probabilites
  emtrends(model, ~ Father, var = "elo_12m") %>% summary(infer = TRUE)
  ggpredict(model, terms = c("elo_12m [0, 0.5, 1]", "Father"), type = "fixed")

  #  trend rank:mount_last12
  plot(ggpredict(model, terms = c("mount_last12", "elo_12m [0, 0.5, 1]"), type = "fixed"))
  emtrends(model, ~ 1 | elo_12m, var = "mount_last12", at = list(elo_12m = c(0, 0.5, 1))) %>% summary(infer = TRUE)

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
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_crs.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_crs.csv", row.names = F)
}
    # Random effects relevance, is Animal identity relevant?
{
  model_re_full <- glmmTMB(
  FirstCrosser ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
      Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group +
      (1 | AnimalCode) +(1|EventID),
  family = binomial,
  data = model_data,
  offset = log(n_males)
)
  model_re_noAnimalCode  <-   glmmTMB(
  FirstCrosser ~
    elo_12m * (Father + mount_coming12 + mount_last12) +
      Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group
       +(1|EventID),
  family = binomial,
  data = model_data,
  offset = log(n_males)
)
  anova(model_re_full, model_re_noAnimalCode)
  summary(model_re_full) #Animal Code 0.54 (0.73), EventID	<0.001(<0.0001)
  # there is an effect of AnimalCode, and "none" of EventID, but that kind of makes sense because only one individual cna be the first
}

# save effects of models plots
{
eff_crs_elofather <- as.data.frame(effect("elo_12m*Father", model, xlevels = list(elo_12m = seq(0, 1, length.out = 100))))
write.csv(eff_crs_elofather, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_elofather.csv", row.names = F)

ggplot(eff_crs_elofather, aes(x = elo_12m, y = fit, color = Father, fill = Father)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(x = "Dominance rank (Elo score)",
       y = "Predicted probability of crs calling",
       color = "Sired offspring",
       fill = "Sired offspring") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


#eff_crs_mountcoming <- ggpredict(model, terms = "mount_coming12 [all]") %>% as.data.frame()
eff_crs_mountcoming <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_coming12")
#write.csv(eff_crs_mountcoming, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_mountcoming.csv", row.names = FALSE)

ggplot(eff_crs_mountcoming, aes(x = mount_coming12_raw, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linetype = 0) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of crs calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

eff_crs_mountcoming_past <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_last12")
write.csv(eff_crs_mountcoming_past, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_mountcoming_past.csv", row.names = FALSE)

ggplot(eff_crs_mountcoming_past, aes(x = var_to_plot_raw, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linetype = 0) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of crs calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

}

