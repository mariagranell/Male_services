# ---------------
# Title: Mating model
# Date: 16 April 2025
# Author: mgranellruiz
# Goal: Have future mounts as the predictor.
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
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
# for model selection
library(tree)
library(party)
library(MuMIn)

# path ------------------------
setwd()

# prepare dataframe, all the data is not publically available since there is considerable trimming of the original dataframe.
# but the data wrangling is kept for transparency
{
# data ------------------------
{
sex <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/Sex_csi_combined_date.csv") %>%
  add_season("date") %>% mutate(Data = "sex", month = month(date)) %>%
  mutate(date = ymd(date), year = year(date)) %>%
  dplyr::filter(group %in% c("NH","AK","BD","KB"), year %in% 2022:2025)
rank <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_maleservices_mating.csv") %>% mutate(year = year(ymd(Date)))
  # calculated in: /Users/mariagranell/Repositories/elo-sociality/sociality/CSI/CSI_calculation_for_MR_MaleServicesMonthlyMating.R
csi <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_maleservices_mating.csv") %>% mutate(year = year(ymd(Date))) %>%
  dplyr::select(-Warning, -ActualDate) # the warning is ok, investigated
ms_mating <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/MSIndex_seasons_males_mating.csv")
}
# parameters ---
MSGroups = c("AK", "BD", "KB", "NH")
unhabituated_cutoff_date = 365 # individuals will be considered habituated only after 1/2 a year, instead of 365, a full year
mating_df <- data.frame( # definition of the mating and MS periods
  year = 2023:2025) %>%
  mutate(StartDate_matingseason = paste0(year, "-04-01"),
         EndDate_matingseason = paste0(year, "-06-30"),
         MSStartDate = paste0(year, "-04-01"),
         MSEndDate = paste0(year, "-06-30"),
  )
{
{
plot_weekly_summary(sex, "Data", "date")


    # Define custom HEX color palettes
  year_colors <- c("2016" = "red", "2017" = "orange", "2018" = "yellow", "2019" = "blue", "2020" = "green",
    "2021"= "#FFB200", "2022" = "#48C9F5", "2023" = "#1f77b4", "2024" = "#ff7f0e", "2025" = "#A31D1D")

# we investigated the histograms by just looking at the, and decided that
# we will look at mating season as number of mounts from 4-7
# we did this because 2022 has a peak in october but the babies are already borned at it also dosen´t make sense for the last baby borned of the year, those mounts out of the range dosen´t seem to count
  sex %>%mutate(month = month(date),
         year = year(date)) %>%
  ggplot(aes(x = as.factor(month), fill = as.factor(year))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = year_colors) +
  theme_classic() +
  facet_grid(cols = vars(Season), scales = "free_x", switch = "x") +
  labs(
    x = "Month",
    y = "Number of matings",
    fill = "Year",
    colour = "Season"
  )

  ##### THIS ONE XDXDXD
  sex %>%
      mutate(month = month(date),
         year = year(date)) %>%
    left_join(lh %>% dplyr::select(AnimalCode, DOB_estimate, Sex, Tenure_type, Group_mb) %>% distinct(), by =c("MaleID"="AnimalCode")) %>%
    filter(Group_mb == group) %>%
    mutate(Age = add_age(DOB_estimate, date, "Years"),
           Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  ggplot(aes(x = as.factor(month), fill = as.factor(Age_class))) +
  geom_bar(position = "dodge") +
  #scale_fill_manual(values = year_colors) +
  theme_classic() +
  facet_grid(cols = vars(Season), scales = "free_x", switch = "x") +
  labs(
    x = "Month",
    y = "Number of matings",
    fill = "Year",
    colour = "Age"
  )

# when are the first/last babies borned?
aa <- lh %>% filter(year(DOB_estimate) == 2023, Tenure_type == "BirthGroup")

# when do males migrate? apparently they come to the groups around april.
# thus we will take as male services from april to june. Including both months
aa <- lh %>% filter(Sex == "M", Tenure_type != "BirthGroup") %>%
  dplyr::filter(Group_mb %in% c("NH","AK","BD","KB"), year(StartDate_mb) %in% 2022:2024) %>%
  add_season("StartDate_mb")

aa %>%mutate(month = month(StartDate_mb),
         year = year(StartDate_mb)) %>%
  ggplot(aes(x = as.factor(month), fill = as.factor(year))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = year_colors) +
  theme_classic() +
  facet_grid(cols = vars(Season), scales = "free_x", switch = "x") +
  labs(
    x = "Month",
    y = "Number of matings",
    fill = "Year",
    colour = "Season"
  )

mating_season <- sex %>%
  mutate(date = ymd(date), year = year(date)) %>%
  dplyr::filter(group %in% c("NH","AK","BD","KB"), year %in% 2021:2024) %>%
  group_by(year, group) %>%
  summarise(
    start_90 = as.Date(quantile(as.numeric(date), 0.05, na.rm = TRUE), origin = "1970-01-01"),
    end_90   = as.Date(quantile(as.numeric(date), 0.95, na.rm = TRUE), origin = "1970-01-01"),
    .groups = "drop"
  )

# the actual dataframe we will use
mating_df <- data.frame(
  year = 2023:2025) %>%
  mutate(StartDate_matingseason = paste0(year, "-04-01"),
         EndDate_matingseason = paste0(year, "-06-30"),
         MSStartDate = paste0(year, "-04-01"),
         MSEndDate = paste0(year, "-06-30"),
  )
} ### calculate sexual periods

### calcualte numbr of mounts per male during those periods
numbr_mounts <- sex %>%
  mutate(date = ymd(date), year = year(date)) %>%
  inner_join(
    mating_df %>%
      dplyr::select(year, StartDate_matingseason, EndDate_matingseason),
    by = c("year")
  ) %>%
  filter(between(ymd(date), ymd(StartDate_matingseason), ymd(EndDate_matingseason))) %>%
  count(year, group, MaleID, StartDate_matingseason, EndDate_matingseason, name = "number_matings")

## that is good, but it should be added to the number of males present for those periods in the group
# that is males that have been in the group at any point from month 4 to 7.

males_present_mating <- lh %>%
  filter(Tenure_type != "BirthGroup", Sex == "M", Group_mb %in% MSGroups) %>%
  crossing(.,
    mating_df %>%
      dplyr::select(year, StartDate_matingseason, EndDate_matingseason)
  ) %>%
  dplyr::filter(StartDate_mb < EndDate_matingseason & EndDate_mb > StartDate_matingseason) %>%
  dplyr::select(year, AnimalCode, Group_mb, StartDate_matingseason, EndDate_matingseason, StartDate_mb, EndDate_mb) %>%
  mutate(
  # calculae the day of the overlap. we always take the most constringent date to calculate
  days_present = as.integer(difftime(
                    pmin(EndDate_mb, EndDate_matingseason),
                    pmax(StartDate_mb, StartDate_matingseason),
                    units = "days")),
  time_present = dplyr::case_when(
    # compare to full season length
    days_present >= as.integer(difftime(EndDate_matingseason, StartDate_matingseason, units = "days")) ~ "All mating season",
    days_present >= 0 ~ as.character(days_present),
    TRUE ~ NA_character_
  )
) %>% distinct()

actual_df <- left_join(males_present_mating, numbr_mounts, by=c("AnimalCode"="MaleID", "year", "Group_mb"="group", "StartDate_matingseason", "EndDate_matingseason")) %>%
  mutate(number_matings = replace_na(number_matings, 0))

write.csv(actual_df, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/mating_basedf.csv", row.names = F)

} # calculate df
# preparation dataframe ------
{
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
first_father_dates <- actual_df %>%
  distinct(AnimalCode, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  )

aa <- #left_join(numbr_mounts, ms_mating, by =c("MaleID" = "AnimalCode", "group"= "Group_mb", "year" = "Season")) %>%
  #filter(Age_class == "adult") %>%
  actual_df %>%
  mutate(TenureLeft = as.numeric(difftime(EndDate_mb, EndDate_matingseason, units = "days")/365),
         TenureYears = as.numeric(difftime(EndDate_matingseason, StartDate_mb, units = "days")/365),
         ) %>%
  left_join(.,ms_mating, by =c("AnimalCode", "Group_mb", "StartDate_mb", "EndDate_mb", "year"="Season")) %>%
  left_join(rank, by = c("AnimalCode" = "IDIndividual1", "Group_mb" = "Group", "year", "Sex", "Age_class")) %>%
  left_join(csi, by = c("AnimalCode", "Group_mb" ="Group", "year")) %>%
  left_join(first_father_dates, by =c("AnimalCode", "StartDate_mb", "EndDate_mb")) %>%
  mutate(
         Father = ifelse(StartDate_matingseason > FirstBabySeason, "Yes", "No"),,
  ) %>%
  distinct() %>%
  # calculate unhabituation
  left_join(unhabituation, by= c("AnimalCode" = "AnimalCode")) %>%
  mutate(Unhabituated = ifelse(BornedInIVP == "no" & difftime(StartDate_matingseason, FirstDate) < unhabituated_cutoff_date, "yes", "no"),
         elo_cat = case_when(
                  ELO_12m == 1 ~ "dominant",
                  #elo == 0 ~"lowest",
                  TRUE ~ "subordinate" #"middle"
                )
  )
    rm(unhabituation, actual_df, first_father_dates, males_present_mating, mating_df, mating_season, ms_mating, csi, MSGroups, numbr_mounts, rank, sex, unhabituated_cutoff_date, year_colors)

#write.csv(aa, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/mating_df_models.csv", row.names = F)
aa <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/mating_df_models.csv") %>%
    filter(Group_mb != "AK") %>%
    dplyr::select(number_matings, N_AlarmService, VigProp, N_BgeService = N_Bge_participates,
                  N_AlarmServiceMP, N_AlarmServiceBARK,
                  days_present, N_CrsService, ELO_12m, elo_cat, zCSI, TenureYears, TenureLeft, Unhabituated, year, Group_mb, AnimalCode) %>%
    distinct() %>%
    drop_na() %>%
    filter(VigProp < 0.49) # outlier also removed from the vigilance models.

  write.csv(aa, "/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/mating_df_models_p.csv")
}
}

# public data is provided from this step forward.
# The group AK was removed since they do not have rivers in their territory, so is not possible to anakyze their crossing behaviour
# We eliminated 1 outlier, the same as in the vigilance model, since most of the time of the focal was spent in vigilance, which was very different to the rest of the dataset and presented signals of stress. Pobably due to the observer.
mating1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/mating_df_models_p.csv") %>%
  # add father
      left_join(.,lh %>% dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb), by = c("AnimalCode", "Group_mb"), relationship = "many-to-many") %>%
    filter(between(year,year(StartDate_mb), year(EndDate_mb))) %>% # remove entries out of season, final number(62)
    mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))),
           FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01")),
           Date = paste0(year, "-03-01"),
           Father = ifelse(Date > FirstBabySeason, "Yes", "No"))

# ANALYSIS FOLLOWING THE RAW SERVICES
  model_df<-mating1  %>%
    mutate(year=as.factor(year),
           Unhabituated=as.factor(Unhabituated),
           Group_mb=as.factor(Group_mb),
           AnimalCode=as.factor(AnimalCode),
           # avoid these to be scaled
           tt=as.character(TenureYears),
           N_BgeService_notstandard = as.character(N_BgeService),
           rank = as.character(ELO_12m),
           number_matings=as.character(number_matings),
           days_present=as.character(days_present),
           across(where(is.numeric), ~as.numeric(scale(.x)))) %>% # scale
    mutate(number_matings=as.numeric(number_matings),
           days_present=as.numeric(days_present),
           rank = as.numeric(rank),
           N_BgeService_notstandard = as.numeric(N_BgeService_notstandard),
           tt=as.numeric(tt)
    )

  # 1 visual checks
  plot(model_df$N_AlarmService, model_df$N_BgeService)
  plot(model_df$N_AlarmServiceMP, model_df$N_BgeService)
  plot(model_df$N_AlarmServiceBARK, model_df$N_BgeService)
  plot(model_df$N_AlarmServiceBARK, model_df$N_AlarmServiceMP)
  plot(model_df$N_AlarmServiceBARK, model_df$ELO_12m)
  plot(model_df$number_matings, model_df$N_BgeService, col = model_df$Group_mb)
  plot(model_df$VigProp, model_df$N_BgeService)
  plot(model_df$ELO_12m, model_df$N_BgeService)

  ggscatterstats(model_df, y = number_matings, x = N_AlarmService)
  ggscatterstats(model_df, y = number_matings, x = VigProp)
  ggscatterstats(model_df, y = number_matings, x = N_BgeService)
  ggscatterstats(model_df, y = number_matings, x = N_CrsService)
  ggscatterstats(model_df, y = number_matings, x = zCSI)
  ggscatterstats(model_df, y = number_matings, x = N_AlarmServiceMP)

  # 2 distribution selection
  hist(log(model_df$number_matings + 0.1))
  hist(log((1 + model_df$number_matings) / model_df$days_present))
  hist(model_df$number_matings)
  plot(fitdist(log(model_df$number_matings + 0.1), distr="norm"))
  plot(fitdist(model_df$number_matings, distr="pois"))
  plot(fitdist(model_df$number_matings, distr="nbinom"))
  plot(fitdist(log((1 + model_df$number_matings) / model_df$days_present), distr="norm")) # ok normal distribution

  # 3 data exploration
  require(party)
  plot(ctree(log(1 + number_matings) / days_present ~
               N_AlarmService +
                 VigProp +
                 N_BgeService +
                 N_CrsService +
                 ELO_12m +
                 TenureLeft +
                 zCSI +
                 TenureYears +
                 Unhabituated +
                 year +
                 Group_mb, data=model_df))

  require(tree)
  plot(tree(log((1 + number_matings) / days_present) ~ TenureLeft +
    N_AlarmServiceMP +
    VigProp +
    N_BgeService +
    N_CrsService +
    ELO_12m +
    zCSI +
    TenureYears +
    Unhabituated +
    Group_mb, data=model_df))
  text(tree(log((1 + number_matings) / days_present) ~ TenureLeft +
    N_AlarmService +
    VigProp +
    N_BgeService +
    N_CrsService +
    ELO_12m +
    zCSI +
    TenureYears +
    Unhabituated +
    Group_mb, data=model_df))

  # 4 guess interactions: no major interactions between services and other social paraments
  coplot(log((1 + number_matings) / days_present) ~ N_BgeService | TenureYears,
         panel=panel.smooth, data=model_df)
  coplot(log((1 + number_matings) / days_present) ~ N_BgeService | zCSI,
         panel=panel.smooth, data=model_df)
  coplot(log((1 + number_matings) / days_present) ~ N_BgeService | ELO_12m,
         panel=panel.smooth, data=model_df)
  coplot(log((1 + number_matings) / days_present) ~ N_BgeService | TenureLeft,
         panel=panel.smooth, data=model_df)

  # 5 mixed model without interactions
  m0<-lmer(log((1 + number_matings) / days_present) ~
               N_AlarmServiceBARK +
               VigProp +
               N_BgeService +
               N_CrsService # services
               +
               ELO_12m +
               zCSI +
               TenureYears # extra variables
               +
               year +
               Unhabituated +
               Group_mb +
               (1 | AnimalCode), # variables to control for
           data=model_df)

m0<-lmer(log((1 + number_matings) / days_present) ~
               N_AlarmServiceBARK +
               VigProp +
               N_BgeService +
               N_CrsService # services
               +
               ELO_12m * Father +
               zCSI + # extra variables
               +
               year +
               Unhabituated +
               Group_mb +
               (1 | AnimalCode), # variables to control for
           data=model_df)

# not the best idea. the model fit is quite terrible compared to the mix model
    m00 <- glmmTMB(log((1+number_matings)/days_present) ~
             N_AlarmServiceMP + VigProp + N_BgeService + N_CrsService # services
             + ELO_12m + zCSI + TenureLeft + TenureYears # extra variables
             + year + Group_mb + Unhabituated + (1 | AnimalCode), # variables to control for
            data = model_df, family = gaussian())

  # Model checks
  res<-simulateResiduals(m0); plot(res)
  check_model(m0)
{
  #Observation-level residual plots
  shapiro.test(resid(m0))  # good
  # test each predictor separately
  testUniformity(res)       # good
  testDispersion(res)       # good

  # continuous predictors one by one
  plotResiduals(res, model_df$TenureYears, main="TenureYears") # good
  plotResiduals(res, model_df$TenureLeft, main="TenureLeft") # good
  plotResiduals(res, model_df$ELO_12m, main="ELO_12m") # good
  plotResiduals(res, model_df$zCSI, main="zCSI") # good
  plotResiduals(res, model_df$VigProp, main="VigProp") # good
  plotResiduals(res, model_df$N_AlarmService, main="N_AlarmService") # good
  plotResiduals(res, model_df$N_CrsService, main="N_CrsService") # good
  plotResiduals(res, model_df$N_BgeService, main="BGE participation") # slight desviations


  # Normality of RE
  qqmath(ranef(m0)); shapiro.test(unlist(ranef(m0))) # good
  # homohedasticity
  plot(fitted(m0), residuals(m0)) # good
  # influencial dots
  plot(influence(m0, obs=TRUE), which="cook")
  # autocorrelation
  var.test(resid(m0) ~ Unhabituated, data=model_df) #good
  vif(m0) # good
}

  # Results
  summary(m0)
  plot_model(m0, vline.color="darkred", show.values=TRUE); Anova(m0)
  plot(allEffects(m0))
  print(standardized_effects(m0), n = Inf)

  # 6 model selection with DREDGE
  require(MuMIn)
  options(na.action="na.fail") # for the case when you have NA in the dataset
  dd<-dredge(m0, rank="AICc")
  dim(dd)
  head(dd) # each line is a model
  best.models<-subset(model.sel(dd), delta < 2)  # near-to-best models
  m.best<-get.models(dd, 1)[[1]]   # extract only the best model
  plot(simulateResiduals(m.best))
  summary(m.best) # see estimates of the best
  Anova(m.best)
  sw(dd)  # importance (probability of being selected in the best model) REPORT THIS
  sw(best.models)  # importance according to the short list

  # results DREDGE
  # 1) Top models table
  top_tbl<-dd %>%
    as.data.frame() %>%
    mutate(delta=AICc - min(AICc)) %>%
    filter(delta < 2) %>%
    dplyr::select(AICc, delta, weight, df, everything())

  ## 2) Top set (ΔAICc < 2) + model averaging
  top_set<-get.models(dd, subset=delta < 2)
  avg_mod<-model.avg(top_set)

  coef_tbl<-summary(avg_mod)$coefmat.full %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    left_join(confint(avg_mod) %>%
                as.data.frame() %>%
                tibble::rownames_to_column("term") %>%
                dplyr::rename(conf.low=`2.5 %`, conf.high=`97.5 %`),
              by="term") %>%
    dplyr::select(term, Estimate, SE=`Std. Error`, `z value`, p=`Pr(>|z|)`, conf.low, conf.high)

  ## 3) Relative importance (sum of weights, sw) for ALL terms
  sw_tbl<-MuMIn::sw(dd) %>%
    tibble::enframe(name="term", value="sw") %>%
    arrange(desc(sw))

top_tbl;coef_tbl;sw_tbl

###################
#write mode_df for plotting
write.csv(model_df, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/mating_modeldataframe.csv", row.names = F)

eff_mating <- as.data.frame(effect("N_BgeService", m.best))
# better prediction back to the orgininal data
pred <- predict_unstandardized_lmer(m.best, mating1, c("N_BgeService", "zCSI"), "N_BgeService")
write.csv(pred, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_mating_bge.csv", row.names = F)

# example of plot with the log response
ggplot(mating1, aes(x = N_BgeService, y = log(number_matings/days_present))) +
  geom_point(alpha = 0.25) +
  geom_ribbon(
    data = pred,
    aes(x = N_BgeService, ymin = log(lwr), ymax = log(upr)),
    inherit.aes = FALSE,
    alpha = 0.25
  ) +
  geom_line(
    data = pred,
    aes(x = N_BgeService, y = log(fit)),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  theme_classic()