# ---------------
# Title: Alarm model
# Date: 15 may 2025
# Author: mgranellruiz
# Goal: clean the aerial data and check 1) sex differences 2) why males do more?
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
source('/Users/mariagranell/Repositories/data/diagnostic_fcns.r')
# models
library(ggplot2)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
library(effects)
library(emmeans)
library(car)

# path ------------------------
setwd()

# prepare dataframe, the data is not publically available since there is considerable trimming of the original dataframe.
# but the data wrangling is kept for transparency
{
# data
{
ALARM_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/alarm_allmyfiles.csv")
rank <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_alarm_maleservices.csv") %>% mutate(Date = ymd(Date))
csi <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_alarm_maleservices.csv") %>% mutate(Date = ymd(Date)) %>%
  dplyr::select(-Warning, -ActualDate) # the warning is ok, investigated
sexual <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_alarm_MS.csv") %>% mutate(Date = ymd(Date))
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
  range(ALARM_org$Date)
}

# parameters ---
MSGroups = c("AK", "BD", "KB", "NH")
unhabituated_cutoff_date = 365 # individuals will be considered habituated after a full year

# filter alarm data ------------------------
range(ALARM_org$Date)
# Select alarm events for MS, that is
ALARM <- ALARM_org %>%
  filter(Group %in% MSGroups) %>%
  mutate(NbUnkInd = ifelse(is.na(NbUnkInd), 0,NbUnkInd)) %>%
  mutate(threat_predator = case_when(
    species %in% c("Bushpig", "Duiker", "Giraffe", "Hare", "5 kudu","Impala","Wildebeest","Warthog","Nyala","Kudu","Not predator", "Vulture", "Non-Raptor Bird", "Ibis","Duck/Goose", "Nightjar", "Not raptor") ~ "Not predator",
    otherspecies %in% c("Blesbok","Cormorant","car","cormorant","Crowned lapwing","Turraco", "terraco", "Porcupine", "pigeon lol","crow","mangose","Hornbill","heroine", "glossy starling","cheeky","cattle","Blesbok", "bees", "antelope") ~ "Not predator",
    species %in% c("1raptor", "2eagles", "2yellow bilked kite followed by a third bigger rapto", "Jackal",
                   "Raptor", "African Harrier Hawk", "African Hawk Eagle", "Caracal", "Crowned Eagle", "Khayalami Dogs", "Martial Eagle",
                   "Owl", "Poacher Dogs", "Spotted Eagle Owl", "Predator", "Spotted Eagle Owl", "Verraux's Eagle Owl", "Genet", "Serval") ~ "Predator",
    otherspecies %in% c("Dogs from the village, not necessarily poacher's dogs as we were next to somebody's yard", "Verraux's eagle", "either aigle or owl",
                        "fish eagle","Some type of eagle", "leopard model", "tawny eagle", "unknown, black dog like body with white tail - could it be a wild dog?",
                        "eagle", "juvenile fish eagle", "crested eagle?", "eagle, exact species unknown", "eagle but not sure which one",
                        "village dog") ~ "Predator",
    OtherContext %in% c("Lukas caracal", "poachers") ~ "Predator",
    otherspecies %in% c("Thickknee", "cow") ~"Not predator",
    otherthreat %in% c("3 dogs", "Eagle model", "Fake Caracal", "eagle expirment", "jongo", "khayalami person running back on main road", "close to the house, see humans and dogs") ~"Predator",
    otherthreat %in% c( "unhabituated group", "bird", "touch screen", "bge", "BD", "helicopter",
                        "Helicopter", "dix (bd male)", "field assistant", "LT") ~"Not predator",
    Threat %in% c("Distant Monkeys Calling", "Carcass", "New Male") ~ "Not predator",
    Remarks %in% c("reaction to a contact call (probably) from another group", "answer to a monkey calling far away") ~ "Not predator",
    TRUE ~ "Unk"
  )) %>%
  group_by(EventID) %>%
  # filter for the events in where there was more action than only chuttering, since is not a service and
  # filter also for events in where there was no unk callers
  mutate(MoreThanChutter = if_else(any(CallType != "Chutter"), "yes", "no"),
         IsThereUnk = if_else(any(str_detect(IDActors, "Unk")), "yes", "no")) %>%
  ungroup() %>%
  filter(MoreThanChutter == "yes",
         IsThereUnk == "no",
         Context != "BGE" # not intrested in bge
  ) %>% distinct()

#View(alarm %>% filter(threat_predator != "Predator"))
#aa <-as.data.frame(table(str_replace(str_to_lower(ALARM$species)," ","")))

table(ALARM$Threat)
table(ALARM$species)
table(ALARM$CallType)
table(ALARM$threat_predator)

# preparation dataframe ------
{
  # select the events you are intrested in
  events <- ALARM %>%
    dplyr::select(EventID, Date, Data, Group, threat_predator, Threat) %>% distinct() %>%
    # check if there are duplicated events
    add_count(EventID) %>%
    # make sure the information of the events is consisent by removing duplicated with less info
    filter(!(n == 2 & threat_predator == "Unk"), !(EventID == 3723 & is.na(Threat)),
           !(n == 3 & threat_predator == "Not predator" & Threat == "Terrestrial" ),
           !(n == 3 & threat_predator == "Unk"),
    ) %>%
    mutate(Threat = ifelse(is.na(Threat), "Unk", Threat)) %>%
    add_group_composition("Group", "Date") %>%
    add_season("Date")%>%

    # add all the adults that were supposed to be present in the event and add a 1 or 0 for participation
    left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("Group" = "Group_mb"), relationship = "many-to-many") %>%
    filter(Date > StartDate_mb & Date < EndDate_mb) %>%
    mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
    filter(Age_class %in% "adult") %>% distinct()

  adults_thatparticipated <- ALARM %>%
    separate_rows(IDActors, sep =";") %>%
    mutate(AnimalCode = str_remove(IDActors , " "), Date = ymd(Date)) %>% distinct() %>%
    left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
    filter(Date > StartDate_mb & Date < EndDate_mb) %>%
    mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
            Age_class = add_age_class(Age,Sex,Tenure_type),
           participation_alarm = 1) %>%
    filter(Age_class %in% "adult") %>%
    dplyr::select(EventID, AnimalCode, CallType, participation_alarm) %>% distinct()

  alarm <- left_join(events, adults_thatparticipated, by = c("EventID", "AnimalCode")) %>%
    mutate(participation_alarm = ifelse(is.na(participation_alarm), 0, 1))

  # save to calculare the next dataframes
  write.csv(alarm, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/alarm_maleservices_basedf.csv", row.names = F)
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
first_father_dates <- alarm %>%
  filter(Sex == "M", ) %>%
  distinct(AnimalCode, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  )

alarm1 <- alarm %>%
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
  distinct() %>%
  mutate(prop_babies =n_babies/n_members) %>%
  # calculate unhabituation
  left_join(unhabituation, by= c("AnimalCode" = "AnimalCode")) %>%
  mutate(Unhabituated = ifelse(BornedInIVP == "no" & difftime(Date, FirstDate) < unhabituated_cutoff_date, "yes", "no"),
         Age = if_else(BornedInIVP == "yes", add_age(DOB_estimate, Date, "Years"), NA),
  ) %>% # calculate how much longer the males will be present in the group. If you will die it dosen´t count.
  left_join(lh[,c("AnimalCode", "Group_mb", "Fate_probable")],
            by = c("AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  mutate(TenureLeft = case_when(Fate_probable == "dead" ~ NA, TRUE ~ difftime(EndDate_mb, Date, unit = "days")/365))

table(alarm1$Sex)
}

#write.csv(alarm1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/alarm_modeldataframe.csv", row.names = FALSE)
alarm1p <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/alarm_modeldataframe.csv")

# keep only predators that have been identified
alarm1p <- alarm1p %>%
    filter(
      Threat %in% c( "Terrestrial", "Aerial"),
      threat_predator == "Predator"
    ) %>%
    dplyr::select(
    EventID,
    Date,
    Group,
    Threat,
    Season,
    AnimalCode,
    Sex,
    participation_alarm,
    asr,
    Unhabituated,

    # male predictors
    ELO_12m,
    zCSI,
    Father,
    TenureYears,
    mount_last12,
    mount_coming12
  )
write.csv(alarm1p, "/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/alarm_modeldataframe_p.csv", row.names = FALSE)
}

# public data is provided from this step forward
# only aerial and predator encounters data. already in a binomial format with 1 for participation and 0 for no call.
# Encounters in where all callers were identified
alarm1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/alarm_modeldataframe_p.csv")

# MODEL 1 - sex differences
{
model_data_firstmodel_alarm <- alarm1 %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Sex, asr, Season, Group, AnimalCode, EventID, Unhabituated, participation_alarm, Threat, Date
  ) %>%
  distinct() %>% # the amount of duplicates removes is due to differences in GPS format but still ame info.
                 # same for the 1 second difference in Time
  mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1])

model<-
  glmmTMB(
    participation_alarm ~ Sex * (Threat + asr_z + Season)
    + Group + (1|AnimalCode) + (1|EventID),
    family = binomial,
    data = model_data_firstmodel_alarm
  )

  # model checks, all good
  res <- simulateResiduals(model); plot(res)
  {
testZeroInflation(res) # good
testDispersion(model) # good
testOutliers(res, type = "bootstrap") # good
# Test for temporal autocorrelation
plot(acf(resid(model))) # good

  # homoscedasticity checks
plotResiduals(res, model_data_firstmodel_alarm$Sex) # good
plotResiduals(res, model_data_firstmodel_alarm$Threat) # good
plotResiduals(res, model_data_firstmodel_alarm$asr) # minimal desviations, good.
plotResiduals(res, model_data_firstmodel_alarm$Season) # good
plotResiduals(res, model_data_firstmodel_alarm$Group) # minimal desviations, good.

  # normality of random effects
# slight desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
  vif_model <- lm(
  participation_alarm ~ Sex * Threat + asr + Season + Unhabituated +
    + Group,
  data = model_data_firstmodel_alarm); vif(vif_model)

}

  # null model check. The addition of sex is marginally significant
  {null_model <-   glmmTMB(
    participation_alarm ~ Threat + asr_z + Season +
    + Group + (1|AnimalCode) + (1|EventID),
    family = binomial,
    data = model_data_firstmodel_alarm)
    anova(null_model, model)}

  # Results
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE); Anova(model)
plot(effect("Sex:Threat", model)) # there is a difference in only in terrestrial encounters
plot(allEffects(model))

  # effect sizes
  standardized_effects(model)

  # threat
  pairs(emmeans(model, ~ Threat), type = "response"); 1/0.38
  # they call 2.6 times more for terrestial threats, odds ratio

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
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_alarm.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_alarm.csv", row.names = F)

  # for plotting
  eff_ala_sex_threat <- as.data.frame(effect("Sex*Threat", model))
  write.csv(eff_ala_sex_threat, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_ala_sex_threat.csv", row.names = F)
  eff_ala_sex <- as.data.frame(effect("Sex", model))
  write.csv(eff_ala_sex, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_ala_sex.csv", row.names = F)


}

# MODEL 2 - hypothesis testing, differences among males
{
model_data_base <- alarm1 %>%
  dplyr::select(Sex, asr, Season, Group, AnimalCode, EventID, Unhabituated, participation_alarm, Threat,
                Sex, asr, Season, Group, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated, Date
  ) %>%
  distinct() %>%
  filter(Sex == "M") %>%
  suppressWarnings(mutate(elo = as.numeric(na_if(elo, "Date out of bounds")))) %>%
  # keep the events in hwere there is at least 1 caller
  group_by(EventID) %>%
  filter(any(participation_alarm == 1)) %>%
  ungroup() %>%
  drop_na()
model_data <- model_data_base%>%
  # scale all variables
  mutate(across(c(elo_12m, zCSI, asr, mount_coming12, mount_last12),
                ~ scale(.x, center = TRUE, scale = TRUE)[,1])) %>%
  mutate(Father = as.factor(Father))

nrow(table(model_data$AnimalCode)) # 39 males
nrow(table(model_data$EventID)) # 62 events
range(model_data$Date)
table(model_data$participation_alarm)

# random slopes check
{
random_slopes <- fe.re.tab(
  fe.model =
    "participation_alarm ~ elo_12m * (Father +Threat + mount_coming12 + mount_last12) +
     Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group",
  re = "(1|AnimalCode)+(1|EventID)",
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

  model_complex <- glmer(participation_alarm ~
    elo_12m * (Father * Threat + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group +
    (1 + elo_12m + mount_coming12 + mount_last12 | AnimalCode) +
    (1 + elo_12m + mount_coming12 + mount_last12 | EventID),
     data = model_data, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000)))

  summary(model_complex) # extremely low variance and extreme complexity.
}

model <-   glmmTMB(
  participation_alarm ~
    elo_12m * (Father * Threat + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1 + mount_coming12 + elo_12m + mount_last12|| AnimalCode) + (1 | EventID),
  family = binomial,
  data = model_data
)

  # model checks, good!
res <- simulateResiduals(model); plot(res)
{
    ranef.diagn.plot(model)

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
plotResiduals(res, model_data$Threat) # good
plotResiduals(res, model_data$zCSI) # good
plotResiduals(res, model_data$asr) # good
plotResiduals(res, model_data$Season) # good
plotResiduals(res, model_data$Unhabituated) # good
plotResiduals(res, model_data$Group) # good

  # normality of random effects
# slight desviations from normality but ok!
qqnorm(ranef(model)$cond$AnimalCode[[1]]); qqline(ranef(model)$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
lm_no_interactions <- lm(
  participation_alarm ~ elo_12m + Father + Threat +
    mount_coming12 + mount_last12 + Season + zCSI + asr + Unhabituated + Group,
  data = model_data
)
car::vif(lm_no_interactions)

}

  # null model check. testing hypothesis improves fit!
{ null_model <-
  glmmTMB(
  participation_alarm ~ Threat +
    Season + zCSI + asr + Unhabituated + Group + (1 | AnimalCode) +(1|EventID),
  family = binomial,
  data = model_data)

  res <- simulateResiduals(null_model); plot(res)
  anova(null_model,model)
}

# Results
summary(model)
plot_model(model, vline.color = "darkred", show.values = TRUE, show.p = F); Anova(model)
plot(allEffects(model))
  plot(effect("Season", model))

    # effect sizes
  print(standardized_effects(model), n = Inf)

  # significant effect interaction
  plot(effect("elo_12m*Father", model)) # when males are not fathers they increase their alarm calloing with rank
  emtrends(model, ~ Father, var = "elo_12m") %>% summary(infer = TRUE)
  emmeans(model, ~ elo_12m | Father)
  ggpredict(model, terms = c("elo_12m [0, 0.5, 1]", "Father"), type = "fixed") # in non fathers prob alarm: 19% vs. 7% high vs low rankers.
  plot(ggpredict(model, terms = c("elo_12m [0, 0.5, 1]", "Father"), type = "fixed"))

plot(effect("mount_coming12", model))
plot(effect("mount_last12", model))

    # Random effects relevance, is Animal identity relevant?
{
  model_re_full <- glmmTMB(
  participation_alarm ~
    elo_12m * (Father * Threat + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1 | AnimalCode) + (1 | EventID),
  family = binomial,
  data = model_data
)
  model_re_noAnimalCode  <- glmmTMB(
  participation_alarm ~
    elo_12m * (Father * Threat + mount_coming12 + mount_last12) +
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group  + (1 | EventID),
  family = binomial,
  data = model_data
)
  anova(model_re_full, model_re_noAnimalCode)
  summary(model_re_full) #Animal Code 0.66 (0.81), EventID	<0.001(<0.01)
  # there is an effect of AnimalCode, and "none" of EventID
}

  # for table
  anova_results <- Anova(model) %>% broom::tidy() |> dplyr::select(Term = term, Chisq = statistic, Df = df, p.value)
  beta_results <- standardized_effects(model) |>
    dplyr::mutate(
      Term = dplyr::case_when(

        # ---- 3-way interaction
        str_detect(Parameter, "^elo_12m:FatherYes:Threat") ~ "elo_12m:Father:Threat",

        # ---- 2-way interactions
        str_detect(Parameter, "^elo_12m:Father") ~ "elo_12m:Father",
        str_detect(Parameter, "^elo_12m:Threat") ~ "elo_12m:Threat",
        str_detect(Parameter, "^elo_12m:mount_coming12") ~ "elo_12m:mount_coming12",
        str_detect(Parameter, "^elo_12m:mount_last12") ~ "elo_12m:mount_last12",
        str_detect(Parameter, "^mount_coming12:Season") ~ "mount_coming12:Season",
        str_detect(Parameter, "^FatherYes:Threat") ~ "Father:Threat",

        # ---- main effects
        str_detect(Parameter, "^elo_12m$") ~ "elo_12m",
        str_detect(Parameter, "^FatherYes$") ~ "Father",
        str_detect(Parameter, "^Threat") ~ "Threat",
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
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_alarm.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_alarm.csv", row.names = F)



}

# save effects of models plots
{
eff_alarm_elofather <- as.data.frame(effect("elo_12m*Father", model, xlevels = list(elo_12m = seq(0, 1, length.out = 100))))
write.csv(eff_alarm_elofather, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_elofather.csv", row.names = F)

ggplot(eff_alarm_elofather, aes(x = elo_12m, y = fit, color = Father, fill = Father)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(x = "Dominance rank (Elo score)",
       y = "Predicted probability of alarm calling",
       color = "Sired offspring",
       fill = "Sired offspring") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


#eff_alarm_mountcoming <- ggpredict(model, terms = "mount_coming12 [all]") %>% as.data.frame()
eff_alarm_mountcoming <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_coming12")
#write.csv(eff_alarm_mountcoming, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_mountcoming.csv", row.names = FALSE)

ggplot(eff_alarm_mountcoming, aes(x = var_to_plot_raw, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linetype = 0) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of alarm calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

eff_alarm_mountcoming_past <- ggpredict_unstadarized_glm(model, model_data_base, var_to_plot = "mount_last12")
write.csv(eff_alarm_mountcoming_past, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_mountcoming_past.csv", row.names = FALSE)

ggplot(eff_alarm_mountcoming_past, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, linetype = 0) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of alarm calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
}