# ---------------
# Title: BGE model
# Date: 19 may 2025
# Author: mgranellruiz
# Goal: clean the BGE data and check 1) sex differences 2) why males do more?
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
library(car)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(DHARMa)
library(glmmTMB)
library(emmeans)
library(effects)
library(sjPlot)
library(ggeffects)

# path ------------------------
setwd()

# prepare dataframe, the data is not publically available since there is considerable trimming of the original dataframe.
# but the data wrangling is kept for transparency
{
  # data
{
BGE_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/bge_interactions_allmyfiles.csv")
rank <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/ELO_bge_maleservices.csv") %>% mutate(Date = ymd(Date))
csi <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/CSI_bge_maleservices.csv") %>% mutate(Date = ymd(Date)) %>%
  dplyr::select(-Warning, -ActualDate) # the warning is ok, investigated
sexual <- read.csv("/Users/mariagranell/Repositories/elo-sociality/sexual/OutputFiles/SexualInteractions_bge_MS.csv") %>% mutate(Date = ymd(Date))
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>%
  filter(!is.na(AnimalCode))
}

# First remove all the BGE events in where there was an adult individual unidentified.
#bge_with_unk <- c(6,8,16,20,21,23,25,31,33,189,191,192,195,201,204,257,270,271,272,276,306,312,315,358,359,393,394,395,396,407,422,423,424,425,428,429,431,433,439,505,623,626,630,634,637,706,709,710,711,736,738,739,740,741,742,744)
bge_with_unk <- BGE_org %>%
  filter(str_detect(Initiators, regex("\\bUnk(?:A(?:M|F)?)?\\b"))) %>%
  distinct(BGE_id_interactions) %>% pull()

filtered_df <- BGE_org %>% filter(!(BGE_id_interactions %in% bge_with_unk)) %>%
  group_by(BGE_id_interactions) %>%
  mutate(any_unk = as.integer(any(grepl("UnkA", Initiators)))) %>%
  ungroup() %>%
  filter(any_unk == 0)

length(unique(filtered_df$BGE_id_interactions))

# parameters ---
MSGroups = c("AK", "BD", "KB", "NH")
unhabituated_cutoff_date = 365 # individuals will be considered habituated only after 1/2 a year, instead of 365, a full year

# prepare bge data in long fromat, summary of participation per individual per bge, only focal group ------------------------
{
range(BGE_org$Date)
# Select bge events for MS, that is

# TODO no, I will only select the focalling group, since I cannot trust the datacollection to be reliable for the other group
# I am going to make two dataframes one for the focal and one for the encounter --------------
# but actually we should only conseder the focal because we can´t consider that the iders have both
# with virtually the same information.Since we don´t mind the behaviour of the reciever. They simply did a behaviour
# from the perspective of the focal
# ignore the warnings
{

# Focal
dFocal <- BGE_org %>%
  dplyr::select(Date, Time, Group, Initiators, ActorsBehaviour, OtherBehaviour, Remarks, BGE_id_interactions) %>%

  # integrate Other Behaviours, the rest I will remove
  mutate(ActorsBehaviour = ifelse(is.na(ActorsBehaviour), NA_character_, as.character(ActorsBehaviour)),
         ActorsBehaviour = stringr::str_trim(ActorsBehaviour)) %>%
  mutate(ActorsBehaviour = case_when(
    ActorsBehaviour == "Other" & grepl("ch", OtherBehaviour, ignore.case = T) ~ "Advance fast",
    ActorsBehaviour == "Other" & grepl("Chirp|bark", OtherBehaviour, ignore.case = T) ~ "Alarm calls",
    ActorsBehaviour == "Other" & grepl("ap", OtherBehaviour, ignore.case = T) ~ "Advance slow",
    ActorsBehaviour == "Other" & grepl("Head flick", OtherBehaviour, ignore.case = T) ~ "Head bob",
    ActorsBehaviour == "Other" & grepl("fi", OtherBehaviour, ignore.case = T) ~ "Fight",
    ActorsBehaviour == "Other" & grepl("at", OtherBehaviour, ignore.case = T) ~ "Attack",
    is.na(ActorsBehaviour) & OtherBehaviour == "ap2" ~ "Advance slow",
    is.na(ActorsBehaviour) & OtherBehaviour %in% c("ap5", "approach the center of Ankhase") ~ "Advance slow",
    ActorsBehaviour == "Attack; Other" & grepl("fi", OtherBehaviour, ignore.case = T) ~ "Attack; Fight",
    ActorsBehaviour == "Head bob; Stare; Other" & grepl("at", OtherBehaviour, ignore.case = T) ~ "Head bob; Stare; Advance slow",
    TRUE ~ ActorsBehaviour
  )) %>%
  # Split behaviours
  tidyr::separate_rows(Initiators, sep = "[;, ]+") %>%
  tidyr::separate_rows(ActorsBehaviour, sep = ";") %>%

  # clean initiators a bit
  mutate(
    Initiators = str_remove(Initiators, "#\\d+"),
    Initiators = str_remove_all(Initiators, "[()]"),
    Initiators = str_to_title(Initiators)
  )%>%
  integrate_otherid(., Initiators) %>%
  correct_pru_que_mess("Initiators", "Date", "Group") %>% distinct() %>%
  # include remarks
  mutate(Initiators = case_when(
    Initiators == "Unkam" & Remarks == "DAL" ~ "Dal",
    Remarks == "Noha UnkAM possible Havana" ~ "Hav",
    Remarks == "male is Aal" ~ "Aal",
    Remarks == "nak" ~ "Nak",
    Initiators == "Unkam" & Remarks == "male is Aal" ~"Aal",
    Initiators == "Unkam" & Remarks %in% c("nakhu is the unknown male","Nakhu the unknown male") ~ "Nak",
    grepl("Kno", Remarks, ignore.case = T) ~ "Kno",
    grepl("UnkADM is Yazoo", Remarks, ignore.case = T) & Initiators == "Unkam" ~ "Yaz",
    grepl("UnkAM is Kno", Remarks, ignore.case = T) ~ "Eis; Nge; Nuk; Pie; Pix; Rid; Sig; Kno",
    TRUE ~ Initiators)
  ) %>%
  # clean the names
  mutate(Initiators = case_when(
    Initiators == "Vlad" ~"Vla",
    Initiators == "Newnewguy" ~ "Lus",
    Initiators == "Plainjane" ~ "PlainJane",
    Initiators == "Newmale090622" ~ "Man",
    Initiators == "Newmale06.22" ~ "Vry",
    Initiators == "Overbite" ~ "Bob",
    Initiators == "Cheseled" ~ "Bra",
    Initiators == "Round" ~ "Bob",
    Initiators == "Knee" ~ "Dal",
    Initiators == "Short" ~ "Tho",
    TRUE ~Initiators
  )) %>%

  # only keep adults that were recorded propperly
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("Initiators" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% "adult") %>% distinct()

# Encountered
dEncountered <- BGE_org %>%
  dplyr::select(Date, Time, EncounterGp, IDReceivers, BehaviourReceivers, OtherRecBehaviour, Remarks, BGE_id_interactions) %>%
  rename(Group = EncounterGp, Initiators = IDReceivers, ActorsBehaviour = BehaviourReceivers, OtherBehaviour = OtherRecBehaviour) %>%
    # integrate Other Behaviours, the rest I will remove
  mutate(ActorsBehaviour = ifelse(is.na(ActorsBehaviour), NA_character_, as.character(ActorsBehaviour)),
         ActorsBehaviour = stringr::str_trim(ActorsBehaviour)) %>%
  mutate(ActorsBehaviour = case_when(
    ActorsBehaviour == "Other" & grepl("ch", OtherBehaviour, ignore.case = T) ~ "Advance fast",
    ActorsBehaviour == "Other" & grepl("chirp|bark", OtherBehaviour, ignore.case = T) ~ "Alarm calls",
    ActorsBehaviour == "Other" & grepl("ap", OtherBehaviour, ignore.case = T) ~ "Advance slow",
    ActorsBehaviour == "Other" & grepl("Head flick", OtherBehaviour, ignore.case = T) ~ "Head bob",
    ActorsBehaviour == "Other" & grepl("hb", OtherBehaviour, ignore.case = T) ~ "Head bob",
    ActorsBehaviour == "Other" & grepl("fi", OtherBehaviour, ignore.case = T) ~ "Fight",
    ActorsBehaviour == "Other" & grepl("at", OtherBehaviour, ignore.case = T) ~ "Attack",
    TRUE ~ ActorsBehaviour
  )) %>%
  # include remarks
  mutate(Initiators = case_when(
    grepl("Pom", Remarks, ignore.case = T) ~ "Pom",
    grepl("Wavyears is the UnkAM", Remarks, ignore.case = T) ~ "WavyEars",
    grepl("Rey", Remarks, ignore.case = T) ~ "Rey",
    grepl("UnkAM is Bab", Remarks, ignore.case = T) ~ "Bab",
    grepl("UnkAM is Gil", Remarks, ignore.case = T) ~ "Gil",
    grepl("UnkA and UnkAM are Gil, Guz", Remarks, ignore.case = T) ~ "Gil;Guz",
    grepl("unk is chiselled nose and he vo back after they vo", Remarks, ignore.case = T) ~ "Bra",
    grepl("unk is chiselled nose", Remarks, ignore.case = T) ~ "Bra",
    grepl("Unknown Male is Skh Skhumbuzo)", Remarks, ignore.case = T) ~ "Skh",
    grepl("skh", Remarks, ignore.case = T) ~ "Skh",
    grepl("War bgeing", Remarks, ignore.case = T) ~ "War",
    TRUE ~ Initiators)) %>%
  # Split behaviours
  tidyr::separate_rows(Initiators, sep = "[;, ]+") %>%
  tidyr::separate_rows(ActorsBehaviour, sep = ";") %>%
  # clean initiators a bit
  mutate(
    Initiators = str_remove(Initiators, "#\\d+"),
    Initiators = str_remove_all(Initiators, "[()]"),
    Initiators = str_to_title(Initiators)
  )%>%
  integrate_otherid(., Initiators) %>%
  correct_pru_que_mess("Initiators", "Date", "Group") %>% distinct() %>%
  # clean the names
  mutate(Initiators = case_when(
    Initiators == "Vlad" ~"Vla",
    Initiators == "Newnewguy" ~ "Lus",
    Initiators == "Plainjane" ~ "PlainJane",
    Initiators == "Plainjohn" ~ "PlainJane",
    Initiators == "Plain" ~ "PlainJane",
    Initiators == "Newmale090622" ~ "Man",
    Initiators == "Newmale06.22" ~ "Vry",
    Initiators == "Overbite" ~ "Bob",
    Initiators == "Cheseled" ~ "Bra",
    Initiators == "Round" ~ "Bob",
    Initiators == "Knee" ~ "Dal",
    Initiators == "Short" ~ "Tho",
    TRUE ~Initiators
  )) %>%

  # only keep adults that were recorded propperly
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("Initiators" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% "adult") %>% distinct()


dat0 <- #rbind(dFocal, dEncountered) %>%
  dFocal %>%
  rename(Individual =Initiators, Behaviour = ActorsBehaviour) %>%
  # remove the space that creates split behaviours in the names
  mutate(
    Individual = str_trim(Individual, side = "left"),
    Behaviour = str_trim(Behaviour, side = "left")
  ) %>%
  filter(Group %in% MSGroups,
         # remove if behaviour is other or if there is no Behaviour
         Behaviour != "Other", !is.na(Behaviour),
         # remove when the actor is unk
         !grepl("Unk|ZZ_All", Individual), !is.na(Individual),
         # remove Other behaviours that are not intresting
         !grepl("cross the", OtherBehaviour)
         ) %>%
    # create a better bge tracker per event per group:
    mutate(eventID = paste0(BGE_id_interactions, "_", Group))

rm(dFocal,dEncountered)}

unique(dat0$Behaviour)
# cateogrize individual behaviours in levels of services
{minor <- c( "Advance slow", "Contact calls", "Alarm calls", "Vocalise", "Vigilant", "Chorus calls", "Stand-up",
             "advance (slow)", "aggression call", "bge call", "contact call (cc)", "chorus aggression call", "chorus cc"
)
medium <-  c("Advance fast", "Front line (ind at the interface)", "Stare", "Head bob",
             "advance (fast)", "hb" )
high <-  c("Attack", "Fight", "Bite", "Chase", "Face offs", "face-off")
all <- c(minor, medium, high)
}

# modify the dataframe so it fits in those categories
dat1 <- dat0 %>%
  mutate(
    Behaviour = case_when(
    Behaviour %in%  minor ~ 'minor',
    Behaviour %in%  medium ~ 'medium',
    Behaviour %in%  high ~ 'high',
    TRUE ~ Behaviour
  )) %>%
  tidyr::separate_rows(Behaviour, sep = "[ .]+") %>%
  mutate(Behaviour = case_when(
    Behaviour %in% c("su", "ap5", "ap10", "ac", "vo", "ap","cc","ba","gu") ~ "minor",
    Behaviour %in% c("st", "hb", "ap2") ~ "medium",
    Behaviour %in% c("Hit", "ch", "fi", "at") ~ "high",
    TRUE ~ Behaviour
  )) %>%
  filter(Behaviour %in% c("minor", "medium", "high")) %>% distinct()

unique(dat1$Behaviour) # before the filter, not services
table(dat1$Group, dat1$Behaviour)
table(dat1$BGE_id_interactions, dat1$Behaviour)
range(dat1$Date)


# select the events you are intrested in
events <- dat1 %>%
    group_by(BGE_id_interactions) %>%
    mutate(bge_intensity = case_when(
      any(Behaviour == "high") ~ "high",
      any(Behaviour == "medium") ~ "medium",
      TRUE ~ "minor"
    )) %>%
    ungroup() %>%
    dplyr::select(eventID, Date, Group, bge_intensity) %>%
    # check if there are duplicated events
    add_count(eventID) %>% # if so (not the case here), make sure the information of the events is consisent by removing duplicated with less info %>%
    add_group_composition("Group", "Date") %>%
    add_season("Date") %>%

    # add all the adults that were supposed to be present in the event and add a 1 or 0 for participation
    left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("Group" = "Group_mb"), relationship = "many-to-many") %>%
    filter(Date > StartDate_mb & Date < EndDate_mb) %>%
    mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
    filter(Age_class %in% "adult") %>% distinct()

# summary of individual participation per event and weight those behaviours
adults_thatparticipated <- dat1 %>%
   mutate(intensity_score = case_when(
    Behaviour == "high" ~ 3,
    Behaviour == "medium" ~ 2,
    Behaviour == "minor" ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(eventID, Individual) %>%
  summarise(intensity_per_event = sum(intensity_score), n_behav_per_event = n(), .groups = "drop")

# join the individual participation (dat1) to events.
bge <- left_join(events, adults_thatparticipated, by = c("eventID", "AnimalCode" ="Individual")) %>%
    mutate(intensity_per_event = ifelse(is.na(intensity_per_event), 0, intensity_per_event),
           n_behav_per_event = ifelse(is.na(n_behav_per_event), 0, n_behav_per_event),
           participation_per_event = ifelse(n_behav_per_event == 0, 0, 1)
           )


# save to calculate the next dataframes
write.csv(bge, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/bge_maleservices_basedf.csv", row.names = F)
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
first_father_dates <- bge %>%
  filter(Sex == "M", ) %>%
  distinct(AnimalCode, StartDate_mb, EndDate_mb) %>%
  mutate(FirstMatingSeason = case_when(
           month(StartDate_mb) <= 7 ~ ymd(paste0(year(StartDate_mb), "-03-01")),
           month(StartDate_mb) > 7 ~ ymd(paste0(year(StartDate_mb) + 1, "-03-01"))
         ),
         FirstBabySeason = ymd(paste0(year(FirstMatingSeason), "-10-01"))
  )

bge1 <- bge %>%
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
  )

table(bge1$Sex)

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
rm(adults_thatparticipated, all, csi, rank, dat0, dat1, events, first_father_dates, high, medium, minor, rank, sexual, unhabituated_cutoff_date, unhabituation)
range(bge1$Date)
length(unique(bge1$eventID))


#write.csv(bge1, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/bge_modeldataframe.csv", row.names = FALSE)
bge1p <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/bge_modeldataframe.csv") %>%
  mutate(participation_binomial = ifelse(n_behav_per_event == 0, 0, 1))

write.csv(bge1p, "/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/bge_modeldataframe_p.csv", row.names = FALSE)
}

# public data is provided from this step forward
# already in a binomial format with 1 for participation and 0 for no. Look into data wranging for definitions of intensity.
# Encounters in where all participants were identified
bge1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/bge_modeldataframe_p.csv")

aa <- bge1 %>% mutate(data = "bge", date = Date) %>%
  dplyr::select(data, date, eventID, Season) %>% distinct()

  plot_weekly_summary(aa, "data", "date")

# MODEL 1 - sex differences
{
model_data_firstmodel_bge <- bge1 %>%
  mutate(Date = as.Date(Date),
         asr_z = scale(asr, center = TRUE, scale = TRUE)[, 1]) %>%
  dplyr::select(Sex, asr_z, n_males, n_members, Season, Group, AnimalCode, eventID,
                intensity_per_event, Unhabituated, bge_intensity,
                n_behav_per_event, participation_per_event, participation_binomial,
                AM,AF, Date
  ) %>%
  drop_na() %>% distinct()

  table(model_data_firstmodel_bge$bge_intensity)
  table(model_data_firstmodel_bge$intensity_per_event)
  length(unique(model_data_firstmodel_bge$eventID)) #768 events
  length(unique(model_data_firstmodel_bge$AnimalCode)) #123 unique individuals, all adults
  table(model_data_firstmodel_bge$Sex, model_data_firstmodel_bge$Unhabituated)
  range(model_data_firstmodel_bge$Date)

ggbetweenstats(model_data_firstmodel_bge, x = Sex, y = intensity_per_event)
ggplot(model_data_firstmodel_bge, aes(x= bge_intensity, y = intensity_per_event, colour = Sex)) +
  geom_boxplot()

model_binomial <- glmmTMB(
  participation_binomial ~ Sex * (bge_intensity + asr_z + Season) + Group +
    (1 | AnimalCode) + (1 | eventID),
  family = binomial(),
  data = model_data_firstmodel_bge
)

  # model checks, all good
  res <- simulateResiduals(model_binomial ); plot(res)
  {
testZeroInflation(res) # good
testDispersion(model_binomial) # good
testOutliers(res, type = "bootstrap") # good
plot(acf(resid(model_binomial ))) # good

  # homoscedasticity checks
plotResiduals(res, model_data_firstmodel_bge$Sex) # good
plotResiduals(res, model_data_firstmodel_bge$bge_intensity) # good
plotResiduals(res, model_data_firstmodel_bge$asr_z) # ok
plotResiduals(res, model_data_firstmodel_bge$Season) # good
plotResiduals(res, model_data_firstmodel_bge$Group) # ok

  # normality of random effects
# good!
qqnorm(ranef(model_binomial )$cond$AnimalCode[[1]]); qqline(ranef(model_binomial )$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
  vif_model <- lm(
  participation_binomial ~ Sex * (bge_intensity + asr_z + Season) + Unhabituated + Group,
  data = model_data_firstmodel_bge); vif(vif_model)

}

  # null model check. The addition of sex is significant
  {null_model <-   glmmTMB(
    participation_binomial ~  bge_intensity + asr_z + Season + Group +
    (1 | AnimalCode) + (1 | eventID),
    family = binomial,
    data = model_data_firstmodel_bge)
    anova(null_model, model_binomial)}

 # model_binomial results
summary(model_binomial)
plot_model(model_binomial, vline.color = "darkred", show.values = TRUE, show.p = T); Anova(model_binomial)
plot(allEffects(model_binomial))
plot(effect("Sex:bge_intensity", model_binomial))

      # effect sizes
  standardized_effects(model_binomial)

  # sex
  emmeans(model_binomial, ~ Sex, type = "response") # probabilites

  # sex:season
  emmeans(model_binomial, ~ Sex|Season, type = "response") # probabilites
  emmeans(model_binomial, ~ Sex |Season) %>% contrast(method = "pairwise") %>% summary(infer = TRUE)

  # sex:asr
  emtrends(model_binomial, ~ Sex, var = "asr_z") %>% summary(infer = TRUE) # trends

  # sex:bge_intensity
emm <- emmeans(model_binomial, ~ Sex | bge_intensity)
pairs(emm, adjust = "tukey"); plot(emm)
pairs(emm, type = "response")
emmeans(model_binomial, ~ Sex, type = "response")

    # for table
  anova_results <- Anova(model_binomial) %>% broom::tidy() |> dplyr::select(Term = term, Chisq = statistic, Df = df, p.value)
  beta_results <- standardized_effects(model_binomial) |>
  dplyr::mutate(
    Term = dplyr::case_when(
      str_detect(Parameter, "^SexM:Season") ~ "Sex:Season",
      str_detect(Parameter, "^SexM:bge_intensity") ~ "Sex:BGE_intensity",
      str_detect(Parameter, "^SexM:asr_z")  ~ "Sex:ASR",
      str_detect(Parameter, "^Season") ~ "Season",
      str_detect(Parameter, "^Group")  ~ "Group",
      str_detect(Parameter, "^bge_intensity") ~ "BGE_intensity",
      str_detect(Parameter, "^asr_z")  ~ "ASR",
      str_detect(Parameter, "^SexM$")  ~ "Sex",
      TRUE ~ Parameter
    )) |>
  group_by(Term) |> summarise(beta_std = max(abs(Std_Coefficient), na.rm = TRUE), .groups = "drop")
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_bge.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_bge.csv", row.names = F)

  # for plotting
  eff_bge_sex_int <- as.data.frame(effect("Sex:bge_intensity", model_binomial))
  write.csv(eff_bge_sex_int, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_bge_sex_int.csv", row.names = F)
  eff_bge_sex <- as.data.frame(effect("Sex", model_binomial))
  write.csv(eff_bge_sex, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/eff_bge_sex.csv", row.names = F)


}

# MODEL 2 - hypothesis testing, differences among males
{
  model_data_base <- bge1 %>%
  dplyr::select(Sex, asr, n_males, n_members, Season, Group, AnimalCode, eventID,
                intensity_per_event, Unhabituated, bge_intensity, Date,
                Sex, asr, n_males, n_members, Season, Group, n_behav_per_event, participation_per_event,
                elo = ELO, elo_12m =ELO_12m, zCSI, Father, TenureYears, mount_coming12, mount_last12, Unhabituated,
                EndDate_mb, Date, participation_binomial
  ) %>%
  drop_na() %>% distinct() %>%
  filter(Sex == "M")
  model_data <- model_data_base %>%
    # scale all variables
  mutate(across(c(elo_12m, zCSI, asr, mount_coming12, mount_last12),
                ~ scale(.x, center = TRUE, scale = TRUE)[,1]))

nrow(table(model_data$AnimalCode)) #51 males
nrow(table(model_data$eventID)) # 610 bge
table(model_data$bge_intensity)


  # random slopes check
{
random_slopes <- fe.re.tab(
  fe.model =
    "participation_binomial ~ elo_12m * (Father +bge_intensity + mount_coming12 + mount_last12) +
     Season + zCSI + asr + Season:mount_coming12 +
     Unhabituated + Group",
  re = "(1|AnimalCode)+(1|eventID)",
  data = model_data
)

support_tbl <- tibble(
  name = names(random_slopes$summary),
  support = map_lgl(random_slopes$summary, flag_support)
) %>%
  mutate(
    re = case_when(
      str_detect(name, "within_AnimalCode") ~ "AnimalCode",
      str_detect(name, "within_eventID")   ~ "eventID",
      TRUE ~ NA_character_
    )
  )

  model_complex <- glmer(participation_binomial ~
    elo_12m * (Father * bge_intensity + mount_coming12 + mount_last12) +
    Season + zCSI + asr  + Season:mount_coming12 +
    Unhabituated + Group +
      (1 + elo_12m + mount_coming12 + mount_last12  | AnimalCode) +
      (1 | eventID),
     data = model_data, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000)))

  summary(model_complex) # extremely low variance and extreme complexity. Compromise and keep only elo as random slope
}

  #library(glmmLasso)
model_binomial <-
  glmmTMB(
  participation_binomial ~
    elo_12m * (Father * bge_intensity + mount_coming12 + mount_last12)+
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1 + mount_coming12 + elo_12m + mount_last12|| AnimalCode) +(1|eventID),
  family = binomial(),
  data = model_data
)

  # model checks, good. The addition of random slopes provides slight inestability on homoscedasticity checks,
  # but not at all concerning. adding random effects or not do not change the model output.
  res <- simulateResiduals(model_binomial); plot(res)
  {
testZeroInflation(res) # good
testDispersion(model_binomial) # good
testOutliers(res, type = "bootstrap") # good
plot(acf(resid(model_binomial))) # good

  # homoscedasticity checks
plotResiduals(res, model_data$elo_12m) # ok
plotResiduals(res, model_data$Father) # ok
plotResiduals(res, model_data$mount_coming12) # ok
plotResiduals(res, model_data$mount_last12) # ok
plotResiduals(res, model_data$bge_intensity) # ok
plotResiduals(res, model_data$zCSI) # ok
plotResiduals(res, model_data$asr_z) # ok
plotResiduals(res, model_data$Season) # good
plotResiduals(res, model_data$Unhabituated) # ok
plotResiduals(res, model_data$Group) # ok

  # normality of random effects
# desviations from normality but ok!
qqnorm(ranef(model_binomial)$cond$AnimalCode[[1]]); qqline(ranef(model_binomial)$cond$AnimalCode[[1]])

      # check mulicolinearity. All good
lm_no_interactions <- lm(
  participation_binomial ~ elo_12m + Father + bge_intensity +
    mount_coming12 + mount_last12 + Season + zCSI + asr + Unhabituated + Group,
  data = model_data
)
car::vif(lm_no_interactions)

}

# null model check. testing hypothesis improves fit!
{ null_model <-
  glmmTMB(
  participation_binomial ~ bge_intensity +
    Season + zCSI + asr + Unhabituated + Group + (1 | AnimalCode) +(1|eventID),
  family = binomial,
  data = model_data)

  res <- simulateResiduals(null_model); plot(res)
  anova(null_model,model_binomial)
}

  # Results
summary(model_binomial)
plot_model(model_binomial, vline.color = "darkred", show.values = TRUE, show.p=F); Anova(model_binomial)
plot(allEffects(model_binomial))

  # effect sizes
  print(standardized_effects(model_binomial), n = Inf)

  # dominant non fathers did more than potential fathers
  plot(effect("elo_12m*Father", model_binomial))
  emtrends(model_binomial, ~ Father, var = "elo_12m") %>% summary(infer = TRUE)
  emmeans(model_binomial, ~ Father|elo_12m, type = "response")

  # results bge_intensity:elo,
  emtrends(model_binomial, ~ bge_intensity, var = "elo_12m") %>% summary(infer = TRUE)
  plot(emtrends(model_binomial, ~ bge_intensity, var = "elo_12m") %>% summary(infer = TRUE))

  # dominnat non-fathers doing more in low intensity bge
  plot(effect("elo_12m*bge_intensity", model_binomial))
  emtrends(model_binomial, ~ Father | bge_intensity, var = "elo_12m") %>% summary(infer = TRUE)

  # Random effects relevance, is Animal identity relevant?
{
  model_binomial_re_full <-
  glmmTMB(
  participation_binomial ~
    elo_12m * (Father * bge_intensity + mount_coming12 + mount_last12)+
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group + (1 | AnimalCode) +(1|eventID),
  family = binomial(),
  data = model_data
)
  model_binomial_re_noAnimalCode <-
  glmmTMB(
  participation_binomial ~
    elo_12m * (Father * bge_intensity + mount_coming12 + mount_last12)+
    Season + zCSI + asr + Season:mount_coming12 +
    Unhabituated + Group +(1|eventID),
  family = binomial(),
  data = model_data
)
  anova(model_binomial_re_full, model_binomial_re_noAnimalCode)
  summary(model_binomial_re_full) # AnimalCode 0.23 (0.48), eventID	0.29 (0.53)
  # there is a large effect of AnimalCode, but also event, event being even larger.
}

    # for table
  anova_results <- Anova(model_binomial) %>% broom::tidy() |> dplyr::select(Term = term, Chisq = statistic, Df = df, p.value)
  beta_results <- standardized_effects(model_binomial) |>
    dplyr::mutate(
      Term = dplyr::case_when(

        # ---- 3-way interaction
        str_detect(Parameter, "^elo_12m:FatherYes:bge_intensity") ~ "elo_12m:Father:bge_intensity",

        # ---- 2-way interactions
        str_detect(Parameter, "^elo_12m:Father") ~ "elo_12m:Father",
        str_detect(Parameter, "^elo_12m:bge_intensity") ~ "elo_12m:BGE_intensity",
        str_detect(Parameter, "^elo_12m:mount_coming12") ~ "elo_12m:mount_coming12",
        str_detect(Parameter, "^elo_12m:mount_last12") ~ "elo_12m:mount_last12",
        str_detect(Parameter, "^mount_coming12:Season") ~ "mount_coming12:Season",
        str_detect(Parameter, "^FatherYes:bge_intensity") ~ "Father:BGE_intensity",

        # ---- main effects
        str_detect(Parameter, "^elo_12m$") ~ "elo_12m",
        str_detect(Parameter, "^FatherYes$") ~ "Father",
        str_detect(Parameter, "^bge_intensity") ~ "BGE_intensity",
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
  write.csv(anova_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_bge.csv", row.names = F)
  write.csv(beta_results, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_bge.csv", row.names = F)


}

# save effects of models plots
{
eff_bge_elofather <- as.data.frame(effect("elo_12m*Father", model_binomial, xlevels = list(elo_12m = seq(0, 1, length.out = 100))))
write.csv(eff_bge_elofather, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_elofather.csv", row.names = F)

ggplot(eff_bge_elofather, aes(x = elo_12m, y = fit, color = Father, fill = Father)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  scale_color_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(x = "Dominance rank (Elo score)",
       y = "Predicted probability of bge calling",
       color = "Sired offspring",
       fill = "Sired offspring") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


#eff_bge_mountcoming <- ggpredict(model_binomial, terms = "mount_coming12 [all]") %>% as.data.frame()
eff_bge_mountcoming <- ggpredict_unstadarized_glm(model_binomial, model_data_base, var_to_plot = "mount_coming12")
#write.csv(eff_bge_mountcoming, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_mountcoming.csv", row.names = FALSE)

ggplot(eff_bge_mountcoming, aes(mount_coming12_raw, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(linewidth = 1) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of bge calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

eff_bge_mountcoming_past <- ggpredict_unstadarized_glm(model_binomial, model_data_base, var_to_plot = "mount_last12")
write.csv(eff_bge_mountcoming_past, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/effect_df_bge_mountcoming_past.csv", row.names = FALSE)

ggplot(eff_bge_mountcoming_past, aes(var_to_plot_raw, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_line(linewidth = 1) +
  labs(x = "Coming mounts in next year",
       y = "Predicted probability of bge calling") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

}