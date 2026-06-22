# ---------------
# Title: MSIndex automated
# Date: 25 feb 2025
# Author: mgranellruiz
# Goal: The idea is to have this script and by just feeding it data. Get an output at the end.
# Make sure you have these directories:
# setwd/OutputFiles/PlotBias/
# ---------------


# library ---------------------
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(patchwork)
library(purrr)
source('/Users/mariagranell/Repositories/data/functions.R')

# path ------------------------
setwd("/Users/mariagranell/Repositories/male_services_index/MSpublication/Scripts")

# parameters ------------------
MSGroups <- c("NH", "AK", "BD", "KB", "LT", "IF", "CR")
years <- 2023:2025 # for these years
{mating_df <- data.frame(
  year = 2023:2025) %>%
  mutate(MSStartDate = paste0(year, "-04-01"),
         MSEndDate = paste0(year, "-06-30"),
  )
mating_df} # define mating


# data ------------------------
# required files are encoded in each section. A factcheked life history data.
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv")
# data
{
  ALARM_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/alarm_allmyfiles.csv")
  FOCAL_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/vigilance_allmyfiles.csv") %>%
    filter(Total > 0)
  CROSSING_org <-read.csv("/Users/mariagranell/Repositories/data/Jakobcybertrackerdatafiles/CleanFiles/crossing_cybertracker.csv")
  BGE_org <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/CleanFiles/bge_interactions_allmyfiles.csv")
}

# SELECT MANUALLY check that you have enough range for the season calulcation
season_df_possible <- mating_df %>% # should be all possible to calculate
  filter(MSStartDate > "2022-05-05" & MSEndDate < "2025-08-01") %>% rename(Season = year)
indv_df_possible <- mating_df %>% dplyr::select(MSStartDate, MSEndDate, Indv =AnimalCode) %>% distinct() # there are duplicated values for some individuals becuae of migration and calculations of rank

# to check
MSStartDate = ymd("2023-04-01") #- months(6)
MSEndDate = ymd("2023-06-30")
Indv = "Vul"

# FUNCTIONS ---------
calc_MSIndex_males_mating <- function(MSStartDate, MSEndDate) {

# CALCULATE THE LIST OF INDIVIDUALS PROVIDING SERVICES -----------------
list_individuals <- lh %>%
  mutate(Age = add_age(DOB_estimate, MSEndDate, "Years"), # calculate their age based on the start of the cutoff
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(#Sex == "M", # I want to select both males and females
         !is.na(AnimalCode),
         Group_mb %in% MSGroups, # remove CR is not reliable data
         EndDate_mb >= MSStartDate & StartDate_mb <= MSEndDate,
         Age_class %in% c("adult","sub-adult"))
table(list_individuals$Sex)

# data ------------------------
# there are 4 different types of behavioural data I´ve resumed by:
# 1) I created a collumn in where i counted the number of times a male provided a service, e.g. N_XxxService.
# 2) I counted the number of opportunitites a male had to perform a behaviour. e.g: N_Xxx.
# --- at the end of the script ----
# 3) I calculated the probability an individual has of providing a service wheen an opportunity happens.
#    XxxProbService = N_XxxService/N_Xxx
# 4) I normalized the Prob collumn by Group
#    XxxProbServiceNorm = (XxxProbService - min(XxxProbService)) /
#                                  (max(XxxProbService) - min(XxxProbService))
# 5) Defined MSI as the average normalized proportion of services, ignoring the cases in where they had no change to provide them
#    MSI =  mean(Norm_AlarmService, Norm_BgeServiceWeight, Norm_CrsService, Norm_VigPer)

# ALARM INDEX MORE THAN PREDATORS ----
# the alarm data considers all alarm events excluding events with unk individuals and with only chutters (i.e. snakes)
{ALARM_MP <- ALARM_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)
# Select alarm events for MS, that is
ALARM_MP_base <- ALARM_MP %>%
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
         #Context != "BGE" # not intrested in bge
  ) %>% distinct() %>%
  # only selected the predators
  mutate(threat_type = case_when(
      Threat == "Distant Monkeys Calling" ~ "competition",
      Threat == "New Male" ~ "competition",
      Threat == "Aerial" ~ "predator",
      Threat == "Terrestrial" ~"predator",
      Threat == "Carcass" ~"predator",
      Threat == "Reptile" ~"predator",
      Threat == "Humans (non-researchers)" ~ "predator",
      TRUE ~ "Unk"
    )) #%>%
    #  filter(
    #  Threat %in% c( "Terrestrial", "Aerial"),
    #  #threat_predator %in% c("Predator", "Not predator")
    #  threat_predator == "Predator"
    #)


table(ALARM_org$Threat)
table(ALARM_org$species)
table(ALARM_org$CallType)

# select the events you are intrested in
  eventsMP <- ALARM_MP_base %>%
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

  adults_thatparticipated_MP <- ALARM_MP_base %>%
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

# merge both dataframes --------------------------
MSalarmMP <- left_join(eventsMP, adults_thatparticipated_MP, by = c("EventID", "AnimalCode")) %>%
    mutate(participation_alarm = ifelse(is.na(participation_alarm), 0, 1)) %>%
    group_by(AnimalCode, Group) %>%
    reframe(
      N_AlarmsMP = n_distinct(EventID),   # number of unique events
      N_AlarmServiceMP = sum(participation_alarm)
    ) %>% rename(IDIndividual1 = AnimalCode)
# errors alarms
MSalarm_errorsMP <- MSalarmMP %>%
  filter(N_AlarmsMP < N_AlarmServiceMP)

rm(ALARM_MP_base, adults_thatparticipated_MP, eventsMP, MSalarm_errorsMP)
}

# ALARM INDEX ONLY BARKS ----
{ALARM_BARK <- ALARM_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)
# Select alarm events for MS, that is
ALARM_BARK_base <- ALARM_BARK %>%
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
         IsThereUnk = if_else(any(str_detect(IDActors, "Unk")), "yes", "no"),
         IsThereBark = if_else(any(str_detect(CallType, "Bark")), "yes", "no")
  ) %>%
  ungroup() %>%
  filter(#MoreThanChutter == "yes",
         IsThereUnk == "no",
         IsThereBark == "yes"
         #Context != "BGE" # not intrested in bge
  ) %>% distinct() %>%
  # only selected the predators
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
      filter(
        CallType == "Bark"
    #  Threat %in% c( "Terrestrial", "Aerial"),
    #  #threat_predator %in% c("Predator", "Not predator")
    #  threat_predator == "Predator"
    )


table(ALARM_org$Threat)
table(ALARM_org$species)
table(ALARM_org$CallType)

# select the events you are intrested in
  eventsBARK <- ALARM_BARK_base %>%
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

  adults_thatparticipated_BARK <- ALARM_BARK_base %>%
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

# merge both dataframes --------------------------
MSalarmBARK <- left_join(eventsBARK, adults_thatparticipated_BARK, by = c("EventID", "AnimalCode")) %>%
    mutate(participation_alarm = ifelse(is.na(participation_alarm), 0, 1)) %>%
    group_by(AnimalCode, Group) %>%
    reframe(
      N_AlarmsBARK = n_distinct(EventID),   # number of unique events
      N_AlarmServiceBARK = sum(participation_alarm)
    ) %>% rename(IDIndividual1 = AnimalCode)
# errors alarms
MSalarm_errorsBARK <- MSalarmBARK %>%
  filter(N_AlarmsBARK < N_AlarmServiceBARK)

rm(ALARM_BARK_base, adults_thatparticipated_BARK, eventsBARK, MSalarm_errorsBARK)
}

# VIGILANCE INDEX --------
# no need to have a MSvigilance_errors
{# You need to run the Clean_focal_data.r that Jos made. You can find it here: /Users/mariagranell/Repositories/data/data2022-06_2023-12/Cleaning_focal_data.r
FOCAL <- FOCAL_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)

MSvigilance <- FOCAL %>% # calculate the age of each Ind when the focal was done
  left_join(lh[,c("AnimalCode", "Sex", "DOB_estimate", "Group_mb", "StartDate_mb", "EndDate_mb", "Tenure_type")],
            by = c("IDIndividual1" = "AnimalCode", "Group" = "Group_mb"), relationship = "many-to-many") %>%
  filter(Date > StartDate_mb & Date < EndDate_mb) %>%
  mutate(Age = add_age(DOB_estimate, Date, "Years"), # calculate their age based on the date of the focal
         Age_class = add_age_class(Age,Sex,Tenure_type)) %>%
  filter(Age_class %in% c("adult","sub-adult")) %>%
  mutate(VigProportion = Vigilant/Total) %>%
  group_by(IDIndividual1, Group) %>%
  dplyr::summarize(VigProp = mean(VigProportion), .groups = "drop")

  rm(FOCAL)}

# CROSSING INDEX adjusted--------
{
CROSSING <- CROSSING_org %>%
  filter(!(Date <= MSStartDate | Date >= MSEndDate), Group %in% MSGroups)

CROSSING_base <- CROSSING %>%
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
  ) %>% distinct() %>%
  filter(CrossingType != "Fence")

# extra filter. Only select crossing in where at least 10% of the group had crossed
crs_atleast_tenpercent <-
  CROSSING %>%
  filter(Obs.nr %in% CROSSING_base$Obs.nr,
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

crs_keep <- CROSSING_base %>% filter(Obs.nr %in% crs_atleast_tenpercent$Obs.nr, Obs.nr != 835) %>% # 835 has two frist crossers
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
  ) %>%
  filter(AgeClass == "AM") %>%
  add_group_composition("Group", "Date")

  # to calculate the proportion, I did the expected and observed proportion for each event
complex_calculation <- crs_keep %>%
  group_by(AnimalCode, Group, Obs.nr, n_males) %>%
  summarise(
    Obs = max(FirstCrosser),           # was this male first in this event?
    Exp = 1/unique(n_males),           # expected probability in this event
    .groups = "drop"
  )
  # here I did the main summary
simple_calculation <- crs_keep %>%
  group_by(AnimalCode, Group) %>%
  reframe(
    N_Crossings = n_distinct(Obs.nr),   # number of unique events
    N_CrsService = sum(FirstCrosser),
    mean_n_males = mean(n_males)
  )


# calculate MScrossing by dividing the observed proportion (N_Crossings/N_CrsService) by the expected proportion (1/n_males)
MScrossing <- complex_calculation %>%
  group_by(AnimalCode, Group) %>%
  summarise(
    ObservedProportion = mean(Obs),
    Expected_proportion = mean(Exp),
    MSCrossing = ObservedProportion / Expected_proportion,
    .groups = "drop"
  ) %>%
  left_join(simple_calculation, by = c("AnimalCode", "Group"))

rm(complex_calculation, CROSSING_base, crs_atleast_tenpercent, crs_keep, simple_calculation)
}

# BGE INDEX --------
# not possible to have a MSbge_errors. warnings are ok
{
BGE_base <- BGE_org %>% filter(!(Date <= MSStartDate | Date >= MSEndDate)) %>% mutate(Remarks = NA)
#BGE_summary <- BGE_summary_org %>% filter(!(Date <= MSStartDate | Date >= MSEndDate)) # not needed using interactions

# First remove all the BGE events in where there was an adult individual unidentified.
#bge_with_unk <- c(6,8,16,20,21,23,25,31,33,189,191,192,195,201,204,257,270,271,272,276,306,312,315,358,359,393,394,395,396,407,422,423,424,425,428,429,431,433,439,505,623,626,630,634,637,706,709,710,711,736,738,739,740,741,742,744)
bge_with_unk <- BGE_base %>%
  filter(str_detect(Initiators, regex("\\bUnk(?:A(?:M|F)?)?\\b"))) %>%
  distinct(BGE_id_interactions) %>% pull()

filtered_df <- BGE_base %>% filter(!(BGE_id_interactions %in% bge_with_unk)) %>%
  group_by(BGE_id_interactions) %>%
  mutate(any_unk = as.integer(any(grepl("UnkA", Initiators)))) %>%
  ungroup() %>%
  filter(any_unk == 0)

  # prepare bge data in long fromat, summary of participation per individual per bge, only focal group ------------------------
  # Only select the focalling group, since I cannot trust the datacollection to be reliable for the other group
{

# Focal
dFocal <- BGE_base %>%
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

dat0 <- dFocal %>%
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

rm(dFocal)}

  # cateogrize individual behaviours in levels of services
{minor <- c( "Advance slow", "Contact calls", "Alarm calls", "Vocalise", "Vigilant", "Chorus calls", "Stand-up",
             "advance (slow)", "aggression call", "alarm call", "contact call (cc)", "chorus aggression call", "chorus cc"
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
MSbge <- left_join(events, adults_thatparticipated, by = c("eventID", "AnimalCode" ="Individual")) %>%
    mutate(intensity_per_event = ifelse(is.na(intensity_per_event), 0, intensity_per_event),
           n_behav_per_event = ifelse(is.na(n_behav_per_event), 0, n_behav_per_event),
           participation_per_event = ifelse(n_behav_per_event == 0, 0, 1),
           bge_intesity = case_when(bge_intensity == "medium" ~ 2,
                                    bge_intensity == "high" ~ 3,
                                    bge_intensity == "minor" ~ 1,
                                    TRUE ~ NA,
           )
           ) %>%
    group_by(AnimalCode, Group) %>%
    reframe(
      N_Bge = n_distinct(eventID),   # number of unique events
      N_Bge_participates = sum(participation_per_event), # how many bges a male participated in
      N_BgeService_indv = sum(n_behav_per_event), # number of individualistic behaviours
      N_BgeServiceWeight_indv = sum(intensity_per_event), # number of weighted behaviours
      N_Bge_weight = sum(bge_intesity) # ho intense the bge were
    )

rm(adults_thatparticipated, all, BGE_base, bge_with_unk, dat1, events, filtered_df, high, medium, minor)
}

# MERGE ALL INDEX
{MSIndex <- list_individuals %>%
  dplyr::select(AnimalCode,Sex,Age_class,Group_mb,Tenure_type) %>%
  left_join(.,MSalarm, by = c("AnimalCode" = "IDIndividual1","Group_mb" = "Group")) %>%
  left_join(.,MSalarmMP, by = c("AnimalCode" = "IDIndividual1","Group_mb" = "Group")) %>%
  left_join(.,MSalarmBARK, by = c("AnimalCode" = "IDIndividual1","Group_mb" = "Group")) %>%
  left_join(.,MSbge, by = c("AnimalCode","Group_mb" = "Group")) %>%
  left_join(.,MScrossing, by = c("AnimalCode","Group_mb" = "Group")) %>%
  left_join(.,MSvigilance, by = c("AnimalCode" = "IDIndividual1","Group_mb" = "Group")) %>%
  dplyr::mutate_all(~replace_na(., 0))

# DEFINE MALE SERVICE INDEX ---------------------
MSIndex_males <- MSIndex %>%
  filter(Sex == "M") %>%
  # Create new columns to compute the proportion of each behavior by dividing the count of observed behavior
  # by the number of opportunities for that behavior
  mutate(
    MSAlarm = N_AlarmService / N_Alarms,                          # Proportion of participation in alarm services
    MSBge_binomial = N_Bge_participates / N_Bge,                  # Proportion of participation in bge
    MSBge_indv_sum = N_BgeService_indv / N_Bge,                   # How much they participated
    MSBge_indv_sum_weight = N_BgeServiceWeight_indv / N_Bge,      # Weighted proportion of participation BGE services by number of BGE
    MSBge_doubleweight = N_BgeServiceWeight_indv / N_Bge_weight,  # To compare between groups we also weghed the bge by intensity
    MSVigilance = VigProp,                                        # N.secods spent of vigilance/by observed seconds
    MSCrossing = ObservedProportion / Expected_proportion         # (MSCrossing is already computed as observed/expected)
  ) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), 0, .))) %>%
  # If there was no opportunity to provide a service put NA
  mutate(MSBge_binomial = ifelse(N_Bge == 0, NA, MSBge_binomial),
         MSBge_indv_sum = ifelse(N_Bge == 0, NA, MSBge_indv_sum),
         MSBge_indv_sum_weight = ifelse(N_Bge == 0, NA, MSBge_indv_sum_weight),
         MSAlarm = ifelse(N_Alarms == 0, NA, MSAlarm),
         MSCrossing = ifelse(N_Crossings == 0, NA, MSCrossing)
  )
rm(MSIndex)
  return(MSIndex_males)
}
}

# RESULTS --------
# mating results
{
  results_list_males <- pmap(season_df_possible, function(Season, MSStartDate, MSEndDate) {
  tryCatch({
    calc_MSIndex_males_mating(MSStartDate, MSEndDate) %>%
      mutate(Season = Season,
             MSStartDate = MSStartDate,
             MSEndDate = MSEndDate)
  }, error = function(e) {
    message("Error for Season ", Season, " (", MSStartDate, " to ", MSEndDate, "): ", e$message)
    NULL  # Return NULL for this iteration if an error occurs.
  })
})
  combined_results_males_mating <- bind_rows(results_list_males)%>%
 left_join(lh %>% dplyr::select(AnimalCode, Group_mb, Tenure_type, StartDate_mb, EndDate_mb),
                                                                  by = c("AnimalCode", "Group_mb", "Tenure_type"))

table(combined_results_males$Season)}

# EXPORT ------
#write.csv(combined_results_males_mating, "/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/MSIndex_seasons_males_mating.csv", row.names = F)
