# ---------------
# Title: Plots and Descriptive tables
# Date: 6 jan 2025
# Author: mgranellruiz
# Goal: have in one place organized all the plots and tables for the publication.
# some of the plots are trials that did not make it to publication.
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

# palette ------------------------
{col_alarm = "#F5AF4DFF"; col_alarm_light = "#F5AF4D"; col_alarm_dark = "#DA710A"
col_bge = "#DB4743FF"; col_bge_light = "#E26C69"; col_bge_dark = "#761917"
col_vig =  "#7C873EFF"; col_vig_light = "#9CAA4E"; col_vig_dark = "#556D31"
col_crs =  "#5495CFFF"; col_crs_light = "#629DD3"; col_crs_dark = "#2659A6"}

# data ------------------------
sentinelling <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/sentinelling_modeldataframe_p.csv")
alarm <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/alarm_modeldataframe_p.csv")
bge <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/bge_modeldataframe_p.csv")
crossing <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/crs_modeldataframe_p.csv")
mating <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/mating_df_models_p.csv")

# model1 datas
{
  model_data_firstmodel_alarm <- alarm %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, EventID,
                threat_predator, Unhabituated, participation_alarm, Threat, Date,
                AM,AF
  ) %>%
  distinct() %>% # the amount of duplicates removes is due to differences in GPS format but still ame info.
                 # same for the 1 second difference in Time
  mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1])

  model_data_firstmodel_bge <- bge %>%
  mutate(Date = as.Date(Date),
         asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1]) %>%
  dplyr::select(Sex, asr_z, n_males, n_members, Season, Group, AnimalCode, eventID,
                intensity_per_event, Unhabituated, bge_intensity,
                n_behav_per_event, participation_per_event, participation_binomial,
                AM,AF, Date
  ) %>%
  drop_na() %>% distinct()

  model_data_model1_crs <- crossing %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, EventID = Obs.nr,
                Unhabituated, FirstCrosser,  CrossingType, Date,
                AM,AF
  ) %>%
   mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1] ) %>%
  drop_na()%>%
  distinct()

  model_data_model1_vig <- sentinelling %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode =IDIndividual1, Total, Vigilant, Unhabituated, Date) %>%
    drop_na() %>% distinct() %>%
    mutate(asr_z = scale(asr, center = TRUE, scale = TRUE)[,1]) %>%
  # remove outlier destected with testOutliers(res)
  filter(Vigilant < 700)

}

# model2 datas
{
model2_sentinelling_data <- sentinelling %>%
  filter(Sex == "M", Age_class == "adult") %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, IDIndividual1, Total, Vigilant,
                elo = ELO, elo_12m = ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated,
                Date
  ) %>%
  filter(#Unhabituated == "no",
         Vigilant < 700) %>%   # remove outlier destected with testOutliers(res)
  mutate(elo = as.numeric(na_if(elo, "Date out of bounds"))) %>%
  drop_na() %>% distinct() %>%
  mutate(elo_categories = case_when(
    elo_12m == 1 ~ "dominant",
    #elo == 0 ~"lowest",
    TRUE ~ "subordinate" #"middle"
  ),#ifelse(elo == 1, "dominant", "subordinates"),
  prop = Vigilant/Total)


model2_alarm_data <- alarm %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, EventID,
                threat_predator, Unhabituated, participation_alarm, Threat,
                Sex, asr, n_males, n_members, Season, Group, Date,
                elo = ELO, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated,
  ) %>%
  mutate(threat_type = case_when(
      Threat == "Distant Monkeys Calling" ~ "competition",
      Threat == "New Male" ~ "competition",
      Threat == "Aerial" ~ "predator",
      Threat == "Terrestrial" ~"predator",
      Threat == "Carcass" ~"predator",
      Threat == "Reptile" ~"predator",
      Threat == "Humans (non-researchers)" ~ "predator",
      TRUE ~ "Unk"
    )) %>%
  distinct() %>%
  filter(Sex == "M") %>%
  mutate(elo = as.numeric(na_if(elo, "Date out of bounds"))) %>%
  mutate(elo_categories = case_when(
    elo_12m == 1 ~ "dominant",
    TRUE ~ "subordinate"
  ),
          ) %>%
    # keep the events in hwere there is at least 1 caller
  group_by(EventID) %>%
  filter(any(participation_alarm == 1)) %>%
  ungroup() %>%
    # take only terrestrial and aerial events and the predator non predator
  filter(
      Threat %in% c( "Terrestrial", "Aerial"),
    ) %>%
    drop_na()

model2_bge_data <- bge %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, eventID,
                intensity_per_event, Unhabituated, bge_intensity, Date, n_behav_per_event, participation_per_event,
                elo = ELO, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated
  ) %>%
  drop_na()%>%
  distinct() %>% distinct() %>%
  filter(Sex == "M") %>%
  mutate(elo = as.numeric(na_if(elo, "Date out of bounds")), # ok warning
         asr_z = scale(asr, center = TRUE, scale = FALSE)[, 1],
         participation_binomial = ifelse(n_behav_per_event == 0, 0, 1)) %>%
  mutate(elo_categories = case_when(
    elo_12m == 1 ~ "dominant",
    TRUE ~ "subordinate")) %>%
    dplyr::select(
      intensity_per_event,elo_12m,Father, bge_intensity,participation_binomial, n_males, n_members , mount_coming12, mount_last12, Season , zCSI, asr_z , Unhabituated,
     Group, AnimalCode,eventID, n_behav_per_event, participation_per_event, Date) %>%
    drop_na() %>% distinct()

  model2_crs_data <- crossing %>%
    dplyr::select(asr, n_males, n_members, Season, Group, AnimalCode, EventID = Obs.nr, Unhabituated, FirstCrosser, CrossingType,
                Sex, asr, n_males, n_members, Season, Group,
                elo = ELO, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated, Date,
                EndDate_mb
  ) %>%
  distinct() %>%
  mutate(elo_categories = case_when(
    elo_12m == 1 ~ "dominant",
    TRUE ~ "subordinate"
  ),
  TenureLeft =  as.numeric(difftime(EndDate_mb, Date, unit = "days")/365)
  )  %>%
  drop_na() %>%
  filter(CrossingType != "Fence")

  # summary of events
{
summary_model2_alarm <- model2_alarm_data%>%
  dplyr::select(Group, n_males, n_members, EventID, Threat) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Threat,
    values_from = value,
    values_fill = 0,
  ) %>%
  group_by(Group) %>%
  summarise(numb_males = median(n_males), sd_numb_males = sd(n_males),
            group_size = median(n_members), sd_group_size = sd(n_members),
            n_aerial = sum(Aerial), n_terrestrial = sum(Terrestrial)
            )

summary_model2_bge <- model2_bge_data%>%
  dplyr::select(Group, n_males, n_members, eventID, bge_intensity) %>%
  distinct() %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = bge_intensity,
    values_from = value,
    values_fill = 0,
  ) %>%
  group_by(Group) %>%
  summarise(numb_males = median(n_males), sd_numb_males = sd(n_males),
            group_size = median(n_members), sd_group_size = sd(n_members),
            n_low = sum(minor), n_medium = sum(medium), n_high = sum(high)
            )

summary_model2_sentinelling <- model2_sentinelling_data %>%
  dplyr::select(Group, n_males, n_members, Sex) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = Sex,
    values_from = value,
    values_fill = list(value = 0),
    values_fn = list(value = length)
  ) %>%
  group_by(Group) %>%
  summarise(
    numb_males = median(n_males),
    sd_numb_males = sd(n_males),
    group_size = median(n_members),
    sd_group_size = sd(n_members),
    n_males = sum(M),
    n_females = sum(F),
  )

}

}

range(model2_sentinelling_data$Date); nrow(model2_sentinelling_data); length(unique(model2_sentinelling_data$IDIndividual1))
range(model2_alarm_data$Date);length(unique(model2_alarm_data$EventID)); length(unique(model2_alarm_data$AnimalCode))
range(model2_bge_data$Date);length(unique(model2_bge_data$eventID)); length(unique(model2_bge_data$AnimalCode))
range(model2_crs_data$Date);length(unique(model2_crs_data$EventID)); length(unique(model2_crs_data$AnimalCode))
model2_crs_data %>% dplyr::select(Group, EventID) %>% distinct() %>% group_by(Group) %>% summarize(n=n())

## FIGURE S1: first model sex differences
dot_size=5
star_size=10
{  # alarm
{
  # raw data per event
  raw_event_alarm<-model_data_firstmodel_alarm %>%
    dplyr::select(EventID, Sex, Threat, participation_alarm) %>%
    group_by(EventID, Sex, Threat) %>%
    summarise(prop_participation=mean(participation_alarm, na.rm=TRUE), .groups="drop")

  # model predictions
  pred_alarm_threat<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_ala_sex_threat.csv")
  pred_alarm<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_ala_sex.csv")


  ala_sex_threat<-ggplot() +
    geom_jitter(
      data=raw_event_alarm,
      aes(x=Threat, y=prop_participation, color=Sex),
      position=position_jitterdodge(jitter.width=0.15, dodge.width=0.5),
      alpha=0.35,
      size=dot_size
    ) +
    geom_point(
      data=pred_alarm_threat,
      aes(x=Threat, y=fit, group=Sex),
      position=position_dodge(width=0.5),
      size=dot_size,
      color="black"
    ) +
    geom_errorbar(
      data=pred_alarm_threat,
      aes(x=Threat, ymin=lower, ymax=upper, group=Sex),
      position=position_dodge(width=0.5),
      width=0.2,
      linewidth=0.9,
      color="black"
    ) +
    scale_color_manual(values=c("M"=col_alarm_dark, "F"=col_alarm_light),
                       labels=c("M"="Male", "F"="Female")) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    labs(
      x="Threat",
      y="Alarm participation probability",
      color="Sex"
    ) +
    annotate("text", x=2, y=0.8, label="**", size=star_size) +
    coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size=20) +
    theme(legend.position="top")
}
  #bge_intensity
{
  # raw data per event
  raw_event_bge<-model_data_firstmodel_bge %>%
    dplyr::select(eventID, Sex, bge_intensity, participation_binomial) %>%
    group_by(eventID, Sex, bge_intensity) %>%
    summarise(prop_participation=mean(participation_binomial, na.rm=TRUE), .groups="drop")

  # model predictions
  pred_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_bge_sex_int.csv")

  bge_intensity<-ggplot() +
    geom_jitter(
      data=raw_event_bge,
      aes(x=bge_intensity, y=prop_participation, color=Sex),
      position=position_jitterdodge(jitter.width=0.15, dodge.width=0.5),
      alpha=0.35,
      size=dot_size
    ) +
    geom_point(
      data=pred_bge,
      aes(x=bge_intensity, y=fit, group=Sex),
      position=position_dodge(width=0.5),
      size=dot_size,
      color="black"
    ) +
    geom_errorbar(
      data=pred_bge,
      aes(x=bge_intensity, ymin=lower, ymax=upper, group=Sex),
      position=position_dodge(width=0.5),
      width=0.2,
      linewidth=0.9,
      color="black"
    ) +
    scale_color_manual(values=c("M"=col_bge_dark, "F"=col_bge_light),
                       labels=c("M"="Male", "F"="Female")) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    labs(
      x="BGE intensity",
      y="BGE participation probability",
      color="Sex"
    ) +
    coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size=20) +
    theme(legend.position="top")
}
  #bge_sex
{
  # raw data per event
  raw_event_bge<-model_data_firstmodel_bge %>%
    dplyr::select(eventID, Sex, participation_binomial) %>%
    group_by(eventID, Sex) %>%
    summarise(prop_participation=mean(participation_binomial, na.rm=TRUE), .groups="drop")

  # model predictions
  pred_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_bge_sex.csv")

  bge_sex<-ggplot() +
    geom_jitter(
      data=raw_event_bge,
      aes(x=Sex, y=prop_participation, color=Sex),
      position=position_jitterdodge(jitter.width=0.15, dodge.width=0.5),
      alpha=0.35,
      size=dot_size
    ) +
    geom_point(
      data=pred_bge,
      aes(x=Sex, y=fit, group=Sex),
      position=position_dodge(width=0.5),
      size=dot_size,
      color="black"
    ) +
    geom_errorbar(
      data=pred_bge,
      aes(x=Sex, ymin=lower, ymax=upper, group=Sex),
      position=position_dodge(width=0.5),
      width=0.2,
      linewidth=0.9,
      color="black"
    ) +
    scale_color_manual(values=c("M"=col_bge, "F"=col_bge_light),
                       labels=c("M"="Male", "F"="Female")) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    labs(
      x="Sex",
      y="BGE participation probability",
      color="Sex"
    ) +
    annotate("text", x=1.5, y=0.8, label="*", size=star_size) +
    coord_cartesian(ylim=c(0, 1)) +
    theme_classic(base_size=20) +
    theme(legend.position="top")
}
  #crs_sex
{
  # raw data per event
  raw_event_crs<-model_data_model1_crs %>%
    dplyr::select(EventID, Sex, FirstCrosser) %>%
    group_by(EventID, Sex) %>%
    summarise(prop_participation=mean(FirstCrosser, na.rm=TRUE), .groups="drop")

  # model predictions
  pred_crs<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_crs_sex.csv")

  crs_sex<-ggplot() +
    geom_jitter(
      data=raw_event_crs,
      aes(x=Sex, y=prop_participation, color=Sex),
      position=position_jitterdodge(jitter.width=0.15, dodge.width=0.5),
      alpha=0.35,
      size=dot_size
    ) +
    geom_point(
      data=pred_crs,
      aes(x=Sex, y=fit, group=Sex),
      position=position_dodge(width=0.5),
      size=dot_size,
      color="black"
    ) +
    geom_errorbar(
      data=pred_crs,
      aes(x=Sex, ymin=lower, ymax=upper, group=Sex),
      position=position_dodge(width=0.5),
      width=0.2,
      linewidth=0.9,
      color="black"
    ) +
    scale_color_manual(values=c("M"=col_crs_dark, "F"=col_crs_light),
                       labels=c("M"="Male", "F"="Female")) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    labs(
      x="Sex",
      y="BGE participation probability",
      color="Sex"
    ) +
    annotate("text", x=1.5, y=0.15, label="***", size=star_size) +
    #coord_cartesian(ylim = c(0, 1)) +
    theme_classic(base_size=20) +
    theme(legend.position="top")
}
  #vig_sex
{
  # raw data per event
  raw_event_vig<-model_data_model1_vig %>%
    dplyr::select(Sex, Vigilant) #%>%
  #group_by(Sex) %>%
  #summarise(prop_participation = mean(Vigilant, na.rm = TRUE), .groups = "drop")

  # model predictions
  pred_vig<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_vig_sex.csv")

  vig_sex<-ggplot() +
    geom_jitter(
      data=raw_event_vig,
      aes(x=Sex, y=Vigilant, color=Sex),
      position=position_jitterdodge(jitter.width=0.15, dodge.width=0.5),
      alpha=0.35,
      size=dot_size
    ) +
    geom_point(
      data=pred_vig,
      aes(x=Sex, y=fit, group=Sex),
      position=position_dodge(width=0.5),
      size=dot_size,
      color="black"
    ) +
    geom_errorbar(
      data=pred_vig,
      aes(x=Sex, ymin=lower, ymax=upper, group=Sex),
      position=position_dodge(width=0.5),
      width=0.2,
      linewidth=0.9,
      color="black"
    ) +
    scale_color_manual(values=c("M"=col_vig_dark, "F"=col_vig_light),
                       labels=c("M"="Male", "F"="Female")) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    labs(
      x="Sex",
      y="Proportion of time sentinelling",
      color="Sex"
    ) +
    annotate("text", x=1.5, y=500, label="**", size=star_size) +
    #coord_cartesian(ylim = c(0, 1)) +
    theme_classic(base_size=20) +
    theme(legend.position="top")
} } # plot creation
ala_sex_threat +
    bge_sex +
    crs_sex +
    vig_sex +
    plot_annotation(tag_levels="A")

  # barplot option
{
  for_bar_plot_norm<-dplyr::bind_rows(
    raw_event_alarm %>%
      group_by(Sex) %>%
      summarise(prop_participation=mean(prop_participation, na.rm=TRUE), .groups="drop") %>%
      mutate(Service="Alarm"),

    raw_event_crs %>%
      group_by(Sex) %>%
      summarise(prop_participation=mean(prop_participation, na.rm=TRUE), .groups="drop") %>%
      mutate(Service="Crossing"),

    raw_event_bge %>%
      group_by(Sex) %>%
      summarise(prop_participation=mean(prop_participation, na.rm=TRUE), .groups="drop") %>%
      mutate(Service="BGE"),

    raw_event_vig %>%
      group_by(Sex) %>%
      summarise(prop_participation=mean(Vigilant, na.rm=TRUE), .groups="drop") %>%
      mutate(Service="Sentinelling")
  ) %>%
    group_by(Service) %>%
    mutate(
      service_mean=mean(prop_participation, na.rm=TRUE),
      prop_normalized=prop_participation / service_mean
    ) %>%
    ungroup()

  # combine predictions
  pred_all<-dplyr::bind_rows(
    pred_alarm %>% mutate(Service="Alarm"),
    pred_crs %>% mutate(Service="Crossing"),
    pred_bge %>% mutate(Service="BGE"),
    pred_vig %>% mutate(Service="Sentinelling")
  ) %>%
    group_by(Service) %>%
    mutate(
      service_mean=mean(fit, na.rm=TRUE),
      fit_norm=fit / service_mean,
      lower_norm=lower / service_mean,
      upper_norm=upper / service_mean
    ) %>%
    ungroup()

  ####
  library(dplyr)
  library(ggplot2)

  # define colors per Service x Sex
  col_map<-c(
    "Alarm_M"=col_alarm_dark,
    "Alarm_F"=col_alarm_light,
    "Crossing_M"=col_crs_dark,
    "Crossing_F"=col_crs_light,
    "BGE_M"=col_bge_dark,
    "BGE_F"=col_bge_light,
    "Sentinelling_M"=col_vig_dark,
    "Sentinelling_F"=col_vig_light
  )

  for_bar_plot_norm<-for_bar_plot_norm %>%
    mutate(ServiceSex=paste(Service, Sex, sep="_"))

  pred_all<-pred_all %>%
    mutate(ServiceSex=paste(Service, Sex, sep="_"))

  ggplot(for_bar_plot_norm, aes(x=prop_normalized, y=Service, fill=ServiceSex)) +
    geom_col(
      position=position_dodge(width=0.7),
      width=0.6,
      alpha=0.7
    ) +
    geom_point(
      data=pred_all,
      inherit.aes=FALSE,
      aes(x=fit_norm, y=Service, group=Sex),
      position=position_dodge(width=0.7),
      size=3,
      color="black"
    ) +
    geom_errorbarh(
      data=pred_all,
      inherit.aes=FALSE,
      aes(y=Service, xmin=lower_norm, xmax=upper_norm, group=Sex),
      position=position_dodge(width=0.7),
      height=0.2,
      color="black"
    ) +
    scale_fill_manual(values=col_map) +
    geom_vline(xintercept=1, linetype=2) +
    labs(
      x="Participation relative to service mean",
      y=NULL,
      fill="Sex"
    ) +
    annotate("text", y=4, x=1.7, label="**", size=star_size) +
    annotate("text", y=3, x=1.7, label="***", size=star_size) +
    annotate("text", y=2, x=1.7, label="*", size=star_size) +
    theme_classic(base_size=16)

}
  # bar plot for alarm
{library(dplyr)
library(ggplot2)

# observed bars
for_bar_plot_norm <-
  raw_event_alarm %>%
  dplyr::group_by(Sex, Threat) %>%
  dplyr::summarise(
    prop_participation = mean(prop_participation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(Threat) %>%
  dplyr::mutate(
    threat_mean = mean(prop_participation, na.rm = TRUE),
    prop_normalized = prop_participation / threat_mean
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ThreatSex = paste(Threat, Sex, sep = "_"))

# model predictions
pred_all <-
  pred_alarm_threat %>%
  dplyr::group_by(Threat) %>%
  dplyr::mutate(
    threat_mean = mean(fit, na.rm = TRUE),
    fit_norm = fit / threat_mean,
    lower_norm = lower / threat_mean,
    upper_norm = upper / threat_mean
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ThreatSex = paste(Threat, Sex, sep = "_"))

col_map <- c(
  "Aerial_M" = col_alarm_dark,
  "Aerial_F" = col_alarm_light,
  "Terrestrial_M" = col_alarm_dark,
  "Terrestrial_F" = col_alarm_light
)

ggplot(for_bar_plot_norm, aes(x = prop_normalized, y = Threat, fill = ThreatSex)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    data = pred_all,
    inherit.aes = FALSE,
    aes(x = fit_norm, y = Threat, group = Sex),
    position = position_dodge(width = 0.7),
    size = 3,
    color = "black"
  ) +
  geom_errorbarh(
    data = pred_all,
    inherit.aes = FALSE,
    aes(y = Threat, xmin = lower_norm, xmax = upper_norm, group = Sex),
    position = position_dodge(width = 0.7),
    height = 0.2,
    color = "black"
  ) +
  scale_fill_manual(values = col_map) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(
    x = "Participation relative to threat mean",
    y = NULL,
    fill = "Sex"
  ) +
  annotate("text", y=2, x=2, label="**", size=star_size) +
  theme_classic(base_size = 16)
}

## FIGURE 1: dominance rank and paternity status
{eff_sentinelling_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_elofather.csv")
eff_alarm_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_elofather.csv")
eff_bge_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_elofather.csv")
eff_crs_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_elofather.csv")
} # data
# rank * father plots without bubles! and rug
size_star = 10; range1 = 3; range2 = 10; alpa_bubble = 0.5; font_size = 15; bin_width = 4
{
 s <- ggplot(model2_sentinelling_data, aes(x = elo_12m, y = Vigilant / Total, color = Father, fill = Father , linetype = Father)) +
  #geom_smooth(method = "lm") +
    ## model effects: confidence ribbon
  geom_ribbon(data = eff_sentinelling_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_sentinelling_elofather, aes(x = elo_12m, y = fit, color = Father, linetype = Father),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
   scale_linetype_manual(values = c("No" = "solid", "Yes" = "dashed")) +
   guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("No" = col_vig_dark, "Yes" = col_vig_light)) +
  scale_fill_manual(values = c("No" = col_vig_dark, "Yes" = col_vig_light)) +
  labs(
    x = "Dominance rank (Elo score)",
    y = "Proportion of time sentinelling",
    color = "",
    fill = ""
  ) +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
  annotate("text", x = 0.5, y = 0.005, label = "*", size = size_star, vjust = -0.2, color = col_vig_dark)+
    # bottom rug (e.g. time == "No")
  geom_rug(
    data = model2_sentinelling_data %>% dplyr::filter(Father == "No"),
    aes(x = elo_12m, color = Father),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  # top rug (e.g. time == "Yes")
  geom_rug(
    data = model2_sentinelling_data %>% dplyr::filter(Father == "Yes"),
    aes(x = elo_12m, color = Father),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )+
  scale_y_continuous(limits = c(0, 0.025))

a <-
  ggplot(model2_alarm_data, aes(x = elo_12m, y = participation_alarm, color = Father, fill = Father, linetype = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_alarm_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_alarm_elofather, aes(x = elo_12m, y = fit, color = Father, linetype = Father),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
    scale_linetype_manual(values = c("No" = "solid", "Yes" = "dashed")) +
   guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("No" = col_alarm_dark, "Yes" = col_alarm_light)) +
  scale_fill_manual(values = c("No" = col_alarm_dark, "Yes" = col_alarm_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of alarm calling",
       color = "Potential father",
       fill = "Potential father") +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
     annotate("text", x = 0.5, y = 0.18, label = "**", size = size_star, vjust = -0.2, color = col_alarm_dark) +
      geom_rug(
    data = model2_alarm_data %>% dplyr::filter(Father == "No"),
    aes(x = elo_12m, color = Father),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = model2_alarm_data %>% dplyr::filter(Father == "Yes"),
    aes(x = elo_12m, color = Father),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )+
  scale_y_continuous(limits = c(0, 0.6))

c <-
  ggplot(model2_crs_data, aes(x = elo_12m, y = FirstCrosser, color = Father, fill = Father, linetype = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_crs_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_crs_elofather, aes(x = elo_12m, y = fit, color = Father, linetype = Father),
    linewidth = 1, inherit.aes = FALSE)  +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +scale_linetype_manual(values = c("No" = "solid", "Yes" = "dashed")) +
   guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("No" = col_crs_dark, "Yes" = col_crs_light)) +
  scale_fill_manual(values = c("No" = col_crs_dark, "Yes" = col_crs_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of crossing first",
       color = "",
       fill = "") +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
     annotate("text", x = 0.5, y = 0.18, label = "*", size = size_star, vjust = -0.2, color = col_crs_dark) +
          geom_rug(
    data = model2_crs_data %>% dplyr::filter(Father == "No"),
    aes(x = elo_12m, color = Father),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = model2_crs_data %>% dplyr::filter(Father == "Yes"),
    aes(x = elo_12m, color = Father),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )+
  scale_y_continuous(limits = c(0, 0.6))

b <- model2_bge_data %>% filter(#bge_intensity == "minor",
                                #Group != "AK"
) %>%
  ggplot(., aes(x = elo_12m, y = participation_binomial, color = Father, fill = Father, linetype = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_bge_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_bge_elofather, aes(x = elo_12m, y = fit, color = Father, linetype = Father),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_linetype_manual(values = c("No" = "solid", "Yes" = "dashed")) +
  guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("No" = col_bge_dark, "Yes" = col_bge_light)) +
  scale_fill_manual(values = c("No" = col_bge_dark, "Yes" = col_bge_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of participating in BGC",
       color = "",
       fill = "")  +
  theme_classic(base_size = font_size) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "top") +
        geom_rug(
    data = model2_bge_data %>% dplyr::filter(Father == "No"),
    aes(x = elo_12m, color = Father),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = model2_bge_data %>% dplyr::filter(Father == "Yes"),
    aes(x = elo_12m, color = Father),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )+
  scale_y_continuous(limits = c(0, 0.6))

a | b | c |s +
  plot_annotation(tag_levels = "A")
  # width 1049 ox, leght 445 px, resolution 75px
}
# trial: with bubbles, not chosen, a bit too messy
{
  eff_sentinelling_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_elofather.csv")
  eff_alarm_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_elofather.csv")
  eff_crs_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_elofather.csv")
  eff_bge_elofather <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_elofather.csv")

# bubble data
{
bubble_data_sentinelling <- model2_sentinelling_data %>%
  dplyr::mutate(
    prop_vig = Vigilant / Total,
    elo_2dp = round(elo_12m, 1)
  ) %>%
  dplyr::group_by(elo_2dp, Father) %>%
  dplyr::summarise(
    prop_mean = mean(prop_vig, na.rm = TRUE),
    freq = dplyr::n(),
    .groups = "drop"
  )
  bubble_data_alarm <- model2_alarm_data %>%
  dplyr::mutate(elo_2dp = round(elo_12m, 1)) %>%
  dplyr::group_by(elo_2dp, Father) %>%
  dplyr::summarise(
    prop_mean = mean(participation_alarm, na.rm = TRUE),
    freq = dplyr::n(),
    .groups = "drop"
  )
  bubble_data_crs <- model2_crs_data %>%
  dplyr::mutate(elo_2dp = round(elo_12m, 1)) %>%
  dplyr::group_by(elo_2dp, Father) %>%
  dplyr::summarise(
    prop_mean = mean(FirstCrosser, na.rm = TRUE),
    freq = dplyr::n(),
    .groups = "drop"
  )
  bubble_data_bge <- model2_bge_data %>%
  dplyr::mutate(elo_2dp = round(elo_12m, 1)) %>%
  dplyr::group_by(elo_2dp, Father) %>%
  dplyr::summarise(
    prop_mean = mean(participation_per_event, na.rm = TRUE),
    freq = dplyr::n(),
    .groups = "drop"
  )
}

# rank * father plots
{
s <- ggplot(model2_sentinelling_data, aes(x = elo_12m, y = Vigilant / Total, color = Father, fill = Father)) +
  #geom_smooth(method = "lm") +
    ## model effects: confidence ribbon
  geom_ribbon(data = eff_sentinelling_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_sentinelling_elofather, aes(x = elo_12m, y = fit, color = Father),
    linewidth = 1, inherit.aes = FALSE) +
  # bubbles
  geom_point(
    data = bubble_data_sentinelling,
    aes(x = elo_2dp, y = prop_mean, size = freq),
    shape = 21, alpha = alpa_bubble, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_color_manual(values = c("No" = col_vig_dark, "Yes" = col_vig_light)) +
  scale_fill_manual(values = c("No" = col_vig_dark, "Yes" = col_vig_light)) +
  labs(
    x = "Dominance rank (Elo score)",
    y = "Proportion of time sentinelling",
    color = "",
    fill = ""
  ) +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
  annotate("text", x = 0.5, y = 0.005, label = "*", size = 10, vjust = -0.2, color = col_vig_dark)+
  geom_rug(
    aes(x = elo_12m, color = Father),
    sides = "tb",
    inherit.aes = FALSE,
    alpha= 0.5
  )

a <-
  ggplot(model2_alarm_data, aes(x = elo_12m, y = participation_alarm, color = Father, fill = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_alarm_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_alarm_elofather, aes(x = elo_12m, y = fit, color = Father),
    linewidth = 1, inherit.aes = FALSE) +
   # bubbles
  geom_point(
    data = bubble_data_alarm,
    aes(x = elo_2dp, y = prop_mean, size = freq),
    shape = 21, alpha = alpa_bubble, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_color_manual(values = c("No" = col_alarm_dark, "Yes" = col_alarm_light)) +
  scale_fill_manual(values = c("No" = col_alarm_dark, "Yes" = col_alarm_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of alarm calling",
       color = "Potential father",
       fill = "Potential father") +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
    geom_rug(sides = "tb",
    alpha= 0.5) +
     annotate("text", x = 0.5, y = 0.18, label = "**", size = 10, vjust = -0.2, color = col_alarm_dark)

c <-
  ggplot(model2_crs_data, aes(x = elo_12m, y = FirstCrosser, color = Father, fill = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_crs_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_crs_elofather, aes(x = elo_12m, y = fit, color = Father),
    linewidth = 1, inherit.aes = FALSE) +
       # bubbles
  geom_point(
    data = bubble_data_crs,
    aes(x = elo_2dp, y = prop_mean, size = freq),
    shape = 21, alpha = alpa_bubble, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  geom_rug(sides = "tb",
    alpha= 0.5) +
  scale_color_manual(values = c("No" = col_crs_dark, "Yes" = col_crs_light)) +
  scale_fill_manual(values = c("No" = col_crs_dark, "Yes" = col_crs_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of crossing first",
       color = "",
       fill = "") +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
     annotate("text", x = 0.5, y = 0.18, label = "*", size = 10, vjust = -0.2, color = col_crs_dark)

b <- model2_bge_data %>% filter(#bge_intensity == "minor",
                                #Group != "AK"
) %>%
  ggplot(., aes(x = elo_12m, y = participation_binomial, color = Father, fill = Father)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_bge_elofather, aes(x = elo_12m, ymin = lower, ymax = upper, fill = Father),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_bge_elofather, aes(x = elo_12m, y = fit, color = Father),
    linewidth = 1, inherit.aes = FALSE) +
     # bubbles
  geom_point(
    data = bubble_data_bge,
    aes(x = elo_2dp, y = prop_mean, size = freq),
    shape = 21, alpha = 0.6, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_color_manual(values = c("No" = col_bge_dark, "Yes" = col_bge_light)) +
  scale_fill_manual(values = c("No" = col_bge_dark, "Yes" = col_bge_light)) +
  labs(x = "Dominance rank (Elo score)",
       y = "Probability of participating in BGE",
       color = "",
       fill = "") +
  geom_rug(sides = "tb",
    alpha= 0.5) +
  theme_classic(base_size = font_size) +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "top")

a + b + c +s
}

# mounting with actual data
mounting_actual_Data <- bind_rows(
  model2_alarm_data %>% mutate(variable = participation_alarm, data = "Alarm") %>% dplyr::select(data, variable, mount_coming12) ,
  model2_sentinelling_data %>% mutate(variable = Vigilant/Total, data = "Sentinelling") %>% dplyr::select(data, variable, mount_coming12),
  model2_bge_data %>% mutate(variable = participation_binomial, data = "BGE") %>% dplyr::select(data, variable, mount_coming12),
  model2_crs_data %>% mutate(variable = FirstCrosser, data = "Crossing") %>% dplyr::select(data, variable, mount_coming12)
)

# trial with bubbles + predictions
{
eff_sentinelling_mountcoming <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_mountcoming.csv") %>%
  # scaled
  mutate(conf.low = conf.low/10, conf.high = conf.high/10, predicted = predicted/10)
eff_bge_mountcoming <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_mountcoming.csv")
eff_crs_mountcoming <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_mountcoming.csv") %>%
  # scaled
  mutate(conf.low = conf.low*10, conf.high = conf.high*10, predicted = predicted*10)
eff_alarm_mountcoming <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_mountcoming.csv") %>%
  # scaled
  mutate(conf.low = conf.low*1.2, conf.high = conf.high*1.2, predicted = predicted*1.2)

#--- 2) put ALL your saved effects into the same format
# Replace the object names below with your actual effect objects you already saved.
eff_all <- dplyr::bind_rows(
  eff_alarm_mountcoming %>% dplyr::mutate(service = "Alarm"),
  eff_sentinelling_mountcoming %>% dplyr::mutate(service = "Sentinelling"),
  eff_bge_mountcoming %>% dplyr::mutate(service = "BGE"),
  eff_crs_mountcoming %>% dplyr::mutate(service = "Crossing")
)

bubble_all <- dplyr::bind_rows(
  model2_alarm_data %>%
    dplyr::mutate(service = "Alarm", y = participation_alarm, elo = as.numeric(elo_12m)),
  model2_sentinelling_data %>%
    dplyr::mutate(service = "Sentinelling", y = Vigilant / Total, elo = as.numeric(elo_12m)),
  model2_bge_data %>%
    dplyr::mutate(service = "BGE", y = participation_binomial, elo = as.numeric(elo_12m)),
  model2_crs_data %>%
    dplyr::mutate(service = "Crossing", y = FirstCrosser, elo = as.numeric(elo_12m))
) %>%
  dplyr::mutate(mount_bin = floor(mount_coming12 / bin_width) * bin_width + bin_width/2) %>%
  dplyr::group_by(service, mount_bin) %>%
  dplyr::summarise(
    y_mean = mean(y, na.rm = TRUE),
    freq   = dplyr::n(),
    .groups = "drop"
  )

e <- ggplot() +
  geom_ribbon(
    data = eff_all,
    aes(x = mount_coming12_raw, ymin = conf.low, ymax = conf.high, fill = service),
    alpha = 0.20, colour = NA
  ) +
  geom_line(
    data = eff_all,
    aes(x = mount_coming12_raw, y = predicted, color = service),
    linewidth = 1
  ) +
  #geom_smooth(data = mounting_actual_Data, aes(x = mount_coming12, y = variable, color = data, fill = data), method = "loess") +
  geom_point(
    data = bubble_all,
    aes(x = mount_bin, y = y_mean, size = freq, fill = service),
    shape = 21, alpha = 0.6, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  #coord_cartesian(xlim = c(0, 50), ylim = c(-0.03, 0.75)) +
  scale_color_manual(values = c("Alarm" = col_alarm, "BGE" = col_bge, "Crossing" = col_crs, "Sentinelling" = col_vig)) +
  scale_fill_manual(values  = c("Alarm" = col_alarm, "BGE" = col_bge, "Crossing" = col_crs, "Sentinelling" = col_vig)) +
  labs(
    x = "Future mounts in next year",
    y = "Predicted service provision",
    color = "Service",
    fill  = "Service"
  ) +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top", legend.title = element_text(size = font_size+8),
    legend.text  = element_text(size = font_size+5)) +
    annotate("text", x = 49, y = 0.4, label = "***", size = 10, vjust = -0.2, color = col_bge) +
   annotate("text", x = 49, y = 0.18, label = "**", size = 10, vjust = -0.2, color = col_alarm) +
 annotate("text", x = 49, y = 0.01, label = "*", size = 10, vjust = -0.2, color = col_crs)+
  annotate("text", x = 49, y = 0.05, label = "*", size = 10, vjust = -0.2, color = col_vig)

}

(a|b|c|s)/(e)+
  plot_annotation(tag_levels = "A")
}

### FIGURE 2: male service provision to past and future mating success
# trial with bubbles + predictions
{
  # mounting with actual data
mounting_past_actual_Data <- bind_rows(
  model2_alarm_data %>% mutate(variable = participation_alarm, data = "Alarm") %>% dplyr::select(data, variable, mount_last12) ,
  model2_sentinelling_data %>% mutate(variable = Vigilant/Total, data = "Sentinelling") %>% dplyr::select(data, variable, mount_last12),
  model2_bge_data %>% mutate(variable = participation_binomial, data = "BGE") %>% dplyr::select(data, variable, mount_last12),
  model2_crs_data %>% mutate(variable = FirstCrosser, data = "Crossing") %>% dplyr::select(data, variable, mount_last12)
)

eff_sentinelling_mountcoming_past <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_sentinelling_mountcoming_past.csv") %>%
  # scaled
  mutate(conf.low = conf.low/10, conf.high = conf.high/10, predicted = predicted/10)
eff_bge_mountcoming_past <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_mountcoming_past.csv")
eff_crs_mountcoming_past <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_crs_mountcoming_past.csv") %>%
  # scaled
  mutate(conf.low = conf.low*10, conf.high = conf.high*10, predicted = predicted*10)
eff_alarm_mountcoming_past <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_alarm_mountcoming_past.csv") %>%
  # scaled
  mutate(conf.low = conf.low*1.2, conf.high = conf.high*1.2, predicted = predicted*1.2)

#--- 2) put ALL your saved effects into the same format
# Replace the object names below with your actual effect objects you already saved.
eff_all_past <- dplyr::bind_rows(
  eff_alarm_mountcoming_past %>% dplyr::mutate(service = "Alarm"),
  eff_sentinelling_mountcoming_past %>% dplyr::mutate(service = "Sentinelling"),
  eff_bge_mountcoming_past %>% dplyr::mutate(service = "BGE"),
  eff_crs_mountcoming_past %>% dplyr::mutate(service = "Crossing")
)

bubble_all_past <- dplyr::bind_rows(
  model2_alarm_data %>%
    dplyr::mutate(service = "Alarm", y = participation_alarm, elo = as.numeric(elo_12m)),
  model2_sentinelling_data %>%
    dplyr::mutate(service = "Sentinelling", y = Vigilant / Total, elo = as.numeric(elo_12m)),
  model2_bge_data %>%
    dplyr::mutate(service = "BGE", y = participation_binomial, elo = as.numeric(elo_12m)),
  model2_crs_data %>%
    dplyr::mutate(service = "Crossing", y = FirstCrosser, elo = as.numeric(elo_12m))
) %>%
  dplyr::mutate(mount_bin = floor(mount_last12 / bin_width) * bin_width + bin_width/2) %>%
  dplyr::group_by(service, mount_bin) %>%
  dplyr::summarise(
    y_mean = mean(y, na.rm = TRUE),
    freq   = dplyr::n(),
    .groups = "drop"
  )

f <-  ggplot() +
  geom_ribbon(
    data = eff_all_past,
    aes(x = var_to_plot_raw, ymin = conf.low, ymax = conf.high, fill = service),
    alpha = 0.20, colour = NA
  ) +
  geom_line(
    data = eff_all_past,
    aes(x = var_to_plot_raw, y = predicted, color = service),
    linewidth = 1
  ) +
  #geom_smooth(data = mounting_actual_Data, aes(x = mount_coming12, y = variable, color = data, fill = data), method = "loess") +
  geom_point(
    data = bubble_all_past,
    aes(x = mount_bin, y = y_mean, size = freq, fill = service),
    shape = 21, alpha = 0.6, stroke = 0
  ) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  #coord_cartesian(xlim = c(0, 50), ylim = c(-0.03, 0.75)) +
  scale_color_manual(values = c("Alarm" = col_alarm, "BGE" = col_bge, "Crossing" = col_crs, "Sentinelling" = col_vig)) +
  scale_fill_manual(values  = c("Alarm" = col_alarm, "BGE" = col_bge, "Crossing" = col_crs, "Sentinelling" = col_vig)) +
  labs(
    x = "Past mounts in last year",
    y = "Predicted service provision",
    color = "Service",
    fill  = "Service"
  ) +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top", legend.title = element_text(size = font_size+8),
    legend.text  = element_text(size = font_size+5)) +
    annotate("text", x = 49, y = 0.4, label = "***", size = 10, vjust = -0.2, color = col_bge) +
   annotate("text", x = 49, y = 0.18, label = "**", size = 10, vjust = -0.2, color = col_alarm) +
 annotate("text", x = 49, y = 0.01, label = "*", size = 10, vjust = -0.2, color = col_crs)+
  annotate("text", x = 49, y = 0.05, label = "*", size = 10, vjust = -0.2, color = col_vig)

}
 # mounting plots without bubbles but divided in two instead of four
{

service_cols <- c(
  "Sentinelling x10" = col_vig,
  "Alarm calling"   = col_alarm,
  "BGC"             = col_bge,
  "Crossing first"  = col_crs
)
# scale vigilance
model2_sentinelling_data_prop <- model2_sentinelling_data %>%
  mutate(vig_prop = Vigilant/Total *10)
# order the services
service_order <- c(
  "Alarm calling",
  "BGC",
  "Crossing first",
  "Sentinelling x10"
)

service_cols_ordered <- service_cols[service_order]
make_obs <- function(data, response, service, scale_y = 1) {
  data %>%
    dplyr::select(mount_last12, mount_coming12, {{ response }}) %>%
    pivot_longer(
      cols = c(mount_last12, mount_coming12),
      names_to = "time",
      values_to = "n_mounts"
    ) %>%
    mutate(
      service = service,
      y = {{ response }} * scale_y
    )
}
make_eff <- function(eff, time_name, service, scale_y = 1) {
  eff %>%
    mutate(
      time = time_name,
      service = service,
      predicted = predicted * scale_y,
      conf.low = conf.low * scale_y,
      conf.high = conf.high * scale_y
    )
}

obs_all <- bind_rows(
  make_obs(model2_sentinelling_data_prop, vig_prop, "Sentinelling x10"),
  make_obs(model2_alarm_data, participation_alarm, "Alarm calling"),
  make_obs(model2_bge_data, participation_binomial, "BGC"),
  make_obs(model2_crs_data, FirstCrosser, "Crossing first")
)

eff_all <- bind_rows(
  make_eff(eff_sentinelling_mountcoming_past, "mount_last12", "Sentinelling x10", scale_y = 10),
  make_eff(eff_sentinelling_mountcoming %>%
             rename(var_to_plot_raw = mount_coming12_raw),
           "mount_coming12", "Sentinelling x10", scale_y = 10),

  make_eff(eff_alarm_mountcoming_past, "mount_last12", "Alarm calling"),
  make_eff(eff_alarm_mountcoming %>%
             rename(var_to_plot_raw = mount_coming12_raw),
           "mount_coming12", "Alarm calling"),

  make_eff(eff_bge_mountcoming_past, "mount_last12", "BGC"),
  make_eff(eff_bge_mountcoming %>%
             rename(var_to_plot_raw = mount_coming12_raw),
           "mount_coming12", "BGC"),

  make_eff(eff_crs_mountcoming_past, "mount_last12", "Crossing first"),
  make_eff(eff_crs_mountcoming %>%
             rename(var_to_plot_raw = mount_coming12_raw),
           "mount_coming12", "Crossing first")
)

plot_mount_services <- function(time_keep, title_x) {

  rug_data <- obs_all %>%
  dplyr::filter(time == time_keep) %>%
  mutate(
    service = factor(service, levels = service_order),
    rug_lane = -0.015 * as.numeric(service)
  )

  ggplot(
    eff_all %>% dplyr::filter(time == time_keep),
    aes(x = var_to_plot_raw, y = predicted, color = service, fill = service)
  ) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.15,
      colour = NA,
      show.legend = T
    ) +
    geom_line(
      linewidth = 1,
      key_glyph = "smooth"
    ) +
    geom_linerange(
      data = rug_data,
      aes(x = n_mounts, ymin = rug_lane, ymax = rug_lane + 0.01, color = service),
      inherit.aes = FALSE,
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_color_manual(
  values = service_cols_ordered,
  breaks = service_order,
  name = ""
) +
scale_fill_manual(
  values = service_cols_ordered,
  breaks = service_order,
  name = ""
) +
guides(
  color = guide_legend(
    override.aes = list(
      fill = unname(service_cols_ordered),
      alpha = 0.15,
      linewidth = 1
    )
  ),
  fill = "none"
)+
    coord_cartesian(ylim = c(-0.08, 0.7), clip = "off") +
    labs(
      x = title_x,
      y = "Predicted service provision"
    ) +
    theme_classic(base_size = font_size) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
}

future_plot <- plot_mount_services(
  time_keep = "mount_coming12",
  title_x = "Future mounts (number mounts next year)"
) +
  annotate("text", x = 60, y = 0.45, label = "***", size = size_star, vjust = -0.2, color = col_bge) +
  annotate("text", x = 60, y = 0.3, label = "*", size = size_star, vjust = -0.2, color = col_vig) +
  annotate("text", x = 60, y = 0.15, label = "**", size = size_star, vjust = -0.2, color = col_alarm) +
  annotate("text", x = 60, y = 0.05, label = "*", size = size_star, vjust = -0.2, color = col_crs)

past_plot <- plot_mount_services(
  time_keep = "mount_last12",
  title_x = "Past mounts (Number of mounts last year)"
) +
    annotate("text", x = 50, y = 0.4, label = "**", size = size_star, vjust = -0.2, color = col_bge) +
  annotate("text", x = 51, y = 0.25, label = ".", size = (size_star +3), vjust = -0.2, color = col_alarm)


future_plot | past_plot
}
  # mounting plots without bubles! and rug, another visualization.
{
    sent <- model2_sentinelling_data %>%
  pivot_longer(cols=c("mount_last12", "mount_coming12"), names_to = "time", values_to = "n_mounts")

  eff_sent <- rbind(
    eff_sentinelling_mountcoming_past %>% mutate(time = "mount_last12"),
    eff_sentinelling_mountcoming %>% mutate(time = "mount_coming12") %>% rename(var_to_plot_raw =mount_coming12_raw)
  )


s <- ggplot(sent, aes(x = n_mounts, y = Vigilant / Total, color = time, fill = time, linetype = time)) +
  #geom_smooth(method = "lm") +
    ## model effects: confidence ribbon
  geom_ribbon(data = eff_sent, aes(x = var_to_plot_raw, ymin = conf.low, ymax = conf.high, fill = time),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_sent, aes(x = var_to_plot_raw, y = predicted, color = time, linetype = time),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_linetype_manual(values = c("mount_coming12" = "solid", "mount_last12" = "dashed"),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("mount_coming12" = col_vig_dark, "mount_last12" = col_vig_light),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  scale_fill_manual(values = c("mount_coming12" = col_vig_dark, "mount_last12" = col_vig_light),
                    labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  labs(
    x = "Number of mounts",
    y = "Proportion of time sentinelling",
    color = "",
    fill = ""
  ) +
  coord_cartesian(ylim=c(0, 0.07)) +
  theme_classic(base_size = font_size) +
  theme(legend.position = "top") +
  annotate("text", x = 25, y = 0.05, label = "*", size = size_star, vjust = -0.2, color = col_vig_dark)+
    # bottom rug (e.g. time == "No")
  geom_rug(
    data = sent %>% dplyr::filter(time == "mount_coming12"),
    aes(x = n_mounts, color = time),
    sides = "b",
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = sent %>% dplyr::filter(time == "mount_last12"),
    aes(x = n_mounts, color = time),
    sides = "t",
    inherit.aes = FALSE
  )

    ala <- model2_alarm_data %>%
  pivot_longer(cols=c("mount_last12", "mount_coming12"), names_to = "time", values_to = "n_mounts")

  eff_ala <- rbind(
    eff_alarm_mountcoming_past %>% mutate(time = "mount_last12"),
    eff_alarm_mountcoming %>% mutate(time = "mount_coming12") %>% rename(var_to_plot_raw =mount_coming12_raw)
  )

a <-
  ggplot(ala, aes(x = n_mounts, y = participation_alarm, color = time, fill = time, linetype = time)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_ala, aes(x = var_to_plot_raw, ymin = conf.low, ymax = conf.high, fill = time),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_ala, aes(x = var_to_plot_raw, y = predicted, color = time, linetype=time),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_linetype_manual(values = c("mount_coming12" = "solid", "mount_last12" = "dashed"),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("mount_coming12" = col_alarm_dark, "mount_last12" = col_alarm_light),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  scale_fill_manual(values = c("mount_coming12" = col_alarm_dark, "mount_last12" = col_alarm_light),
                    labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  labs(x = "Number of mounts",
       y = "Probability of alarm calling",
       color = "",
       fill = "") +
  theme_classic(base_size = font_size) +
    coord_cartesian(ylim=c(0, 0.7)) +
  theme(legend.position = "top") +
     annotate("text", x = 25, y = 0.5, label = "**", size = size_star, vjust = -0.2, color = col_alarm_dark) +
    annotate("text",  x = 25, y = 0.5, label = ".", size = (size_star + 3), vjust = -0.2, color = col_alarm_light) +
    geom_rug(
    data = ala %>% dplyr::filter(time == "mount_coming12"),
    aes(x = n_mounts, color = time),
    sides = "b",
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = ala %>% dplyr::filter(time == "mount_last12"),
    aes(x = n_mounts, color = time),
    sides = "t",
    inherit.aes = FALSE
  )

    crs <- model2_crs_data %>%
  pivot_longer(cols=c("mount_last12", "mount_coming12"), names_to = "time", values_to = "n_mounts")

  eff_crs <- rbind(
    eff_crs_mountcoming_past %>% mutate(time = "mount_last12"),
    eff_crs_mountcoming %>% mutate(time = "mount_coming12") %>% rename(var_to_plot_raw =mount_coming12_raw)
  )


c <-
  ggplot(crs, aes(x = n_mounts, y = FirstCrosser, color = time, fill = time, linetype = time)) +
  #geom_smooth(method = "lm") +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_crs, aes(x = var_to_plot_raw, ymin = conf.low, ymax = conf.high, fill = time),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_crs, aes(x = var_to_plot_raw, y = predicted, color = time, linetype = time),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  scale_linetype_manual(values = c("mount_coming12" = "solid", "mount_last12" = "dashed"),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("mount_coming12" = col_crs_dark, "mount_last12" = col_crs_light),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  scale_fill_manual(values = c("mount_coming12" = col_crs_dark, "mount_last12" = col_crs_light),
                    labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  labs(x = "Number of mounts",
       y = "Probability of crossing first",
       color = "",
       fill = "") +
  theme_classic(base_size = font_size) +
    coord_cartesian(ylim=c(0, 0.7)) +
  theme(legend.position = "top") +
     annotate("text",  x = 25, y = 0.5, label = "*", size = size_star, vjust = -0.2, color = col_crs_dark)   +
  geom_rug(
    data = crs %>% dplyr::filter(time == "mount_coming12"),
    aes(x = n_mounts, color = time),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = crs %>% dplyr::filter(time == "mount_last12"),
    aes(x = n_mounts, color = time),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )

  bge <- model2_bge_data %>%
  pivot_longer(cols=c("mount_last12", "mount_coming12"), names_to = "time", values_to = "n_mounts")

  eff_bge <- rbind(
    eff_bge_mountcoming_past %>% mutate(time = "mount_last12"),
    eff_bge_mountcoming %>% mutate(time = "mount_coming12") %>% rename(var_to_plot_raw =mount_coming12_raw)
  )

b <- bge %>%
  ggplot(., aes(x = n_mounts, y = participation_binomial, color = time, fill = time, linetype = time)) +
  ## model effects: confidence ribbon
  geom_ribbon(data = eff_bge, aes(x = var_to_plot_raw, ymin = conf.low, ymax = conf.high, fill = time),
    alpha = 0.25, colour = NA, inherit.aes = FALSE) +
  ## model effects: fitted line
  geom_line(data = eff_bge, aes(x = var_to_plot_raw, y = predicted, color = time, linetype = time),
    linewidth = 1, inherit.aes = FALSE) +
    scale_size_continuous(
    name = "Frequency",
    range = c(range1, range2),
    guide = "none"
  ) +
  annotate("text",  x = 25, y = 0.5, label = "***", size = size_star, vjust = -0.2, color = col_bge_dark)   +
  annotate("text",  x = 25, y = 0.45, label = "*", size = size_star, vjust = -0.2, color = col_bge_light)   +
  scale_linetype_manual(values = c("mount_coming12" = "solid", "mount_last12" = "dashed"),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  guides(linetype = "none") + # remove line type leyend add in biorender
  scale_color_manual(values = c("mount_coming12" = col_bge_dark, "mount_last12" = col_bge_light),
                     labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  scale_fill_manual(values = c("mount_coming12" = col_bge_dark, "mount_last12" = col_bge_light),
                    labels = c("mount_coming12" = "Future mounts", "mount_last12"   = "Past mounts")) +
  labs(x = "Number of mounts",
       y = "Probability of participating in BGE",
       color = "",
       fill = "")  +
  theme_classic(base_size = font_size) +
  coord_cartesian(ylim=c(0, 0.7)) +
  theme(legend.position = "top") +
  geom_rug(
    data = bge %>% dplyr::filter(time == "mount_coming12"),
    aes(x = n_mounts, color = time),
    sides = "b",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +

  # top rug (e.g. time == "Yes")
  geom_rug(
    data = bge %>% dplyr::filter(time == "mount_last12"),
    aes(x = n_mounts, color = time),
    sides = "t",
    alpha = 0.5,
    inherit.aes = FALSE
  )

a | b | c |s +
  plot_annotation(tag_levels = "A")
}


### FIGURE 3: last model, male services and mating rate
{predictions_mating <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_mating_bge.csv")
 lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>% filter(!is.na(AnimalCode))
  mating_plot <- mating %>% mutate(mounts_presence_corrected = log1p(number_matings) - log(days_present))  %>%
     # add father non-father. just to check, not actually used.
  left_join(.,lh %>% dplyr::select(AnimalCode, StartDate_mb, EndDate_mb, Group_mb), by = c("AnimalCode", "Group_mb"), relationship = "many-to-many") %>%
    filter(between(year,year(StartDate_mb), year(EndDate_mb))) %>% # remove entries out of season, final number(62)
    mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))),
           FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01")),
           Date = paste0(year, "-03-01"),
           Father = ifelse(Date > FirstBabySeason, "Yes", "No"))


bge <- mating_plot %>%
  ggplot(aes(x = N_BgeService,
             y = mounts_presence_corrected,
             color = ELO_12m)) +
  geom_ribbon(
    data = predictions_mating,
    aes(x = N_BgeService, ymin = log(lwr), ymax = log(upr)),
    inherit.aes = FALSE,
    alpha = 0.25, fill = col_bge
  ) +
  geom_line(
    data = predictions_mating,
    aes(x = N_BgeService, y = log(fit)),
    inherit.aes = FALSE,
    linewidth = 1, color = col_bge
  ) +
  geom_point(alpha = 0.7, size =5) +
  scale_color_viridis_c(option = "inferno", direction = -1,
                        name = "Male rank",
                        breaks = c(0, 0.5, 1),
                        labels = c("Low", "Mid", "High")) +
  labs(x = "BGC participation",
       y = "log of mating success corrected for presence") +
  annotate("text", x = 18, y = -2, label = "***", size = 13, vjust = -0.2, color = col_bge) +
  # draw annotation and arrow
  annotate("text", x = 4, y = 0.2, label = "~ 1 mating per day", size = 6, vjust = -0.2, color = "black") +
  theme_classic(base_size = 20) +
  theme(
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white"),
    legend.title = element_text(size = 25),
    legend.text  = element_text(size = 22),
    legend.key.size = unit(1, "cm")
)

alarm <- mating_plot %>%
  ggplot(aes(x = N_AlarmService,
             y = mounts_presence_corrected,
             color = ELO_12m)) +
  geom_smooth(method = "lm", color = col_alarm, fill = col_alarm) +
  geom_point(alpha = 0.7, size =5) +
  scale_color_viridis_c(option = "inferno", direction = -1,
                        name = "Male rank",
                        breaks = c(0, 0.5, 1),
                        labels = c("Low", "Mid", "High")) +
  labs(x = "Alarm participation",
       y = "") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-5, 0), breaks = c(-4, -3, -2, -1, 0)) +
  scale_x_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) +
  theme(legend.position = "none")

crs <- mating_plot %>%
  ggplot(aes(x = N_CrsService,
             y = mounts_presence_corrected,
             color = ELO_12m)) +
  geom_smooth(method = "lm", color = col_crs, fill = col_crs) +
  geom_point(alpha = 0.7, size =5) +
  scale_color_viridis_c(option = "inferno", direction = -1,
                        name = "Male rank",
                        breaks = c(0, 0.5, 1),
                        labels = c("Low", "Mid", "High")) +
  labs(x = "Crossing first participation",
       y = "") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-5, 0), breaks = c(-4, -3, -2, -1, 0)) +
  theme(legend.position = "none")

vig <- mating_plot %>%
  ggplot(aes(x = VigProp,
             y = mounts_presence_corrected,
             color = ELO_12m)) +
  geom_smooth(method = "lm", color = col_vig, fill = col_vig) +
  geom_point(alpha = 0.7, size =5) +
  scale_color_viridis_c(option = "inferno", direction = -1,
                        name = "Male rank",
                        breaks = c(0, 0.5, 1),
                        labels = c("Low", "Mid", "High")) +
  labs(x = "Proportion of time sentinelling",
       y = "") +
  theme_classic(base_size = 20) +
  scale_y_continuous(limits = c(-5, 0), breaks = c(-4, -3, -2, -1, 0)) +
  theme(legend.position = "none")

bge + (alarm/crs/vig) +
  plot_annotation(tag_levels = "A")
}