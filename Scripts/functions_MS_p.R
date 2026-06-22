# ---------------
# Title: functions
# Date: 31 Jan 2023
# Due date: 20 Feb 2023
# Author: mgranellruiz
# Goal: host all the functions used in this project
# ---------------
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

## tips how to create functions:
## https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function

# add lh
lh <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH.csv") %>%
  filter(!is.na(AnimalCode))


# Age ---------------------------------------------------------------------

#add age if I have already BirthDate available
# modifications by maria, added Date as Sys.Date and added a pull(unit) so we don´t get the
# whole dataframe but just the desired column

add_age <- function(birthdate, date = Sys.Date(), unit) {
  if (unit == "Months") {
    round(as.numeric(difftime(date, birthdate, units = "days")) / 30.4375, digits = 2)
  } else if (unit == "Days") {
    round(as.numeric(difftime(date, birthdate, units = "days")), digits = 2)
  } else if (unit == "Years") {
    round(as.numeric(difftime(date, birthdate, units = "days")) / 365.25, digits = 2)
  } else {
    stop("Invalid unit specified.")
  }
}

# Group names to codes -------------------
# example: data <- change_group_names(data,c("FocalGp", "EncounterGp"))
# for dplyr: %>% change_group_names(.,c("Group", "EncounterGp"))
change_group_names <- function(df, group_col_names) {
  library(dplyr)

  # Define the name changes as a named vector
  name_changes <- c("Ankhase" = "AK",
                    "Baie Dankie" = "BD",
                    "Baie_Dankie" = "BD",
                    "Crossing" = "CR",
                    "Crossing," = "CR",
                    "IFamily" = "IF",
                    "Ifamily" = "IF",
                    "Iceland" = "SC",
                    "Kubu" = "KB",
                    "Lemon Tree" = "LT",
                    "Lemon_Tree" = "LT",
                    "Noha" = "NH",
                    "Sonic" = "SC",
                    "RnB" = "RB")

  # Apply the name changes to each specified group column
  for (col_name in group_col_names) {
    df <- df %>%
      mutate(!!sym(col_name) := case_when(!!sym(col_name) %in% names(name_changes) ~ name_changes[as.character(!!sym(col_name))],
                                          TRUE ~ as.character(!!sym(col_name))))
  }

  return(df)
}

### FUNCTIONS CREATED BY JOSEFIEN ---------------------------

# WinnerAge = calculate_age(DOBAgg, FRAgg, DIAgg, Date))
## To calculate the age of an individual
calculate_age <- function(date_of_birth, first_recorded, departure_date, target_date) {
  if (is.na(date_of_birth)) {
    age_in_years <- as.numeric(difftime(target_date, first_recorded, units = "weeks")) / 52.143
    if (!is.na(departure_date) && departure_date <= target_date) {
      age_in_years <- age_in_years + 5
    }
  } else {
    age_in_years <- as.numeric(difftime(target_date, date_of_birth, units = "weeks")) / 52.143
    if (!is.na(departure_date) && departure_date <= target_date) {
      age_in_years <- age_in_years + 2
    }
  }
  return(age_in_years)
}


## To determine the age class
get_age_class <- function(Sex, Age) {
  Adjuv <- vector("character", length = length(Sex))
  for (i in seq_along(Sex)) {
    if (is.na(Sex[i]) || is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if (Sex[i] == "F" && Age[i] >= 4) {
        Adjuv[i] <- "AF"
      } else if (Sex[i] == "F" && Age[i] >= 1 && Age[i] < 4) {
        Adjuv[i] <- "JF"
      } else if (Sex[i] == "M" && Age[i] >= 1 && Age[i] < 4) {
        Adjuv[i] <- "JM"
      } else if (Sex[i] == "M" && Age[i] >= 4 && Age[i] < 5) {
        Adjuv[i] <- "SM"
      } else if (Sex[i] == "M" && Age[i] > 5) {
        Adjuv[i] <- "AM"
      } else if (Age[i] < 1) {
        Adjuv[i] <- "BB"
      } else {
        Adjuv[i] <- NA
      }
    }
  }
  return(Adjuv)
}

# modeified function so it takes tenure type into consideration
#  mutate(LoserAge = add_age(DOB_estimate, Date, "Years"),
#         AgeClassLoser = get_age_class_w_tenuretype(Sex,LoserAge, Tenure_type))

get_age_class_w_tenuretype <- function(Sex, Age_yr_estimate, Tenure_type) {
  dplyr::case_when(
    is.na(Sex) | is.na(Age_yr_estimate) ~ NA_character_,
    Age_yr_estimate < 1 ~ "BB",
    Sex == "F" & Age_yr_estimate >= 4 ~ "AF",
    Sex == "F" & Age_yr_estimate >= 1 & Age_yr_estimate < 4 ~ "JF",
    Sex == "M" & Age_yr_estimate >= 1 & Age_yr_estimate < 4 ~ "JM",
    Sex == "M" & Tenure_type == "BirthGroup" & Age_yr_estimate >= 4 #& Age_yr_estimate < 5 # if you want to add this part of the code, individuals still staying in their bitrh group but older than 5 will be considered as adult males
      ~ "SM",
    Sex == "M" & (Tenure_type != "BirthGroup" | Age_yr_estimate > 5) ~ "AM",
    TRUE ~ NA_character_
  )
}


# Define the function
add_age_class <- function(Age_yr_estimate, Sex, Tenure_type) {
  case_when(
    Age_yr_estimate < 1 & Tenure_type == "BirthGroup" ~ "baby",
    Age_yr_estimate < 4 & Tenure_type == "BirthGroup"~ "juvenile",
    # Sex == "M" & Tenure_type == "BirthGroup" & Age_yr_estimate <= 5 ~ "sub-adult", # this is what we agreed on, considering natal males as adults if they are
    # older than 5 years old, but for my studies natal males regardless their age are adults.
    Sex == "M" & Tenure_type == "BirthGroup" & Age_yr_estimate >= 4 ~ "sub-adult",
    Age_yr_estimate >= 4 & Sex == "F" ~ "adult",
    Sex == "M" & Tenure_type != "BirthGroup" ~ "adult",
    TRUE ~ NA_character_
  )
}

# Example usage:
# d <- d %>% mutate(Age = add_age(DOB_estimate, Date, "Years"),
#                   Age_class = add_age_class(Age,Sex,Tenure_type)))

## add season
# we consider 4 seasons for the vervets dividided in months: # summer (1-3), mating (4-6), winter (7-9), baby (10-12)
# and this fucntion orders the dataframe following the semason
# the date collumn has to be in format "YYYY-MM-DD"
# you can use it as in here, putside of the mutate:
# dd <- data %>%
#  add_season("Date")

add_season <- function(data, date_column) {
  # Ensure date_column is converted to Date format
  data[[date_column]] <- as.Date(data[[date_column]])

  # Extract the month
  data$Month <- as.numeric(month(data[[date_column]]))

  # Assign seasons based on the month
  data$Season <- ifelse(
    data$Month < 4, "Summer",
    ifelse(
      data$Month < 7, "Mating",
      ifelse(
        data$Month < 10, "Winter", "Baby"
      )
    )
  )

  # Convert seasons to an ordered factor
  data$Season <- factor(data$Season,
                        levels = c("Baby", "Summer", "Mating", "Winter"),
                        ordered = TRUE)

  # Drop temporary Month column
  data <- data %>% dplyr::select(-Month)
}

# Plot weekly summary
# this plot is to visualize how much data you have and where does it fall within the seasons.
# use this way. I is assumed that you have a Date collumn in ymd format and a Data cllumns that days "affiliate" or something like that for the title
# plot_weekly_summary(affiliative, "Data", "Date")
library(ggplot2)
plot_weekly_summary <- function(dd, data_column, date_column) {
  # Extract DataType from the first row of the specified column
  DataType <- dd[[data_column]][1]

  # Ensure the date column is in Date format
  dd <- dd %>%
    mutate(!!date_column := as.Date(!!sym(date_column)))  # Convert to Date type

  # Process the data
  data2 <- dd %>%
    mutate(
      week = week(!!sym(date_column)),
      year = as.character(year(!!sym(date_column)))
    ) %>%
    group_by(week, Season, year) %>%
    summarise(value = n(), .groups = "drop") %>%
    mutate(Season = factor(Season, levels = c("Summer", "Mating", "Winter", "Baby")))

  # Define custom HEX color palettes
  year_colors <- c("2016" = "red", "2017" = "orange", "2018" = "yellow", "2019" = "blue", "2020" = "green",
    "2021"= "#FFB200", "2022" = "#48C9F5", "2023" = "#1f77b4", "2024" = "#ff7f0e", "2025" = "#A31D1D")

  # Create the plot
  plot <- ggplot(data2, aes(x = as.factor(week), y = value, fill = year)) +
    geom_bar(stat = "identity",
             #position = "dodge" if you don´t want it stack but side by side
    ) +
    scale_fill_manual(values = year_colors) +  # Set custom colors for year
    theme_classic() +
    facet_grid(cols = vars(Season), scales = "free_x", switch = "x") +
    labs(
      title = DataType,
      x = "Week",
      y = "Number of entries",
      fill = "Year",
      colour = "Season"
    )

  return(plot)
}



library(hms)
library(stringr)
library(dplyr)

# Make sure the changes on Pru, Que and Zeu are implemented ------------------------
# Anything from 2022-06-01 to Jan 2024, recorded as Pru in Kubu is Que.
# Anything from 2022-06-01 to Jan 2024, recorded as Zeu in IF (i family) is Pru.
# Function to correct monkey IDs based on conditions
# I also changed that Ren in NH sometimes was wrongly named Reno. (this change dosen´t accoun for time)

#dd <- dd %>% correct_pru_que_mess("idindividual1", "date", "group")

correct_pru_que_mess <- function(data, monkey_id_col, date_col, group_col) {

  # Convert column names to symbols for dplyr
  monkey_id_col <- sym(monkey_id_col)
  date_col <- sym(date_col)
  group_col <- sym(group_col)

  # Define the date range
  start_date <- as.Date("2022-06-01")
  end_date <- as.Date("2024-01-30") # This represents Jan 2024

  # Apply transformations
  data <- data %>%
    mutate(
      !!monkey_id_col := case_when(
        # Pru in Kubu should be changed to Que within the date range
        !!monkey_id_col == "Pru" & !!group_col == "KB" & (!!date_col >= start_date & !!date_col < end_date) ~ "Que",
        # Zeu in IF (i family) should be changed to Pru within the date range
        !!monkey_id_col == "Zeu" & !!group_col == "IF" & (!!date_col >= start_date & !!date_col < end_date) ~ "Pru",
        # Reno in NH should be changed for Ren
        !!monkey_id_col == "Reno" & !!group_col == "NH" ~ "Ren",
        # Keep all other cases unchanged
        TRUE ~ !!monkey_id_col
      )
    )

    # Define the date range. To correct Ves-Hei. when people started following Crossing again they confused
  # a monkey with Hei, later realized it was a new male called from then on Ves.
  start_date <- as.Date("2021-05-25")
  end_date <- as.Date("2022-12-08")

  # Apply transformations
  data <- data %>%
    mutate(
      !!monkey_id_col := case_when(
        # Zeu in IF (i family) should be changed to Pru within the date range
        !!monkey_id_col == "Hei" & !!group_col == "CR" & (!!date_col >= start_date & !!date_col < end_date) ~ "Ves",
        TRUE ~ !!monkey_id_col
      )
    )

  return(data)
}


# add group compositon:
# Define the function
library(dplyr)
library(tidyr)
library(purrr)
#aa <- add_group_composition(df, "Group", "Date") # remember to add lh

add_group_composition <- function(df, gp_column, date_column) {

  df_out <- df %>%
    mutate(comp = pmap(
      list(Group = .[[gp_column]], Date = .[[date_column]]),
      function(Group, Date) {
        comp <- lh %>%
          filter(
            Group_mb == Group,
            StartDate_mb <= Date,
            EndDate_mb >= Date
          ) %>%
          dplyr::select(AnimalCode, Group_mb, Sex, DOB_estimate, Tenure_type, StartDate_mb, EndDate_mb) %>%
          mutate(
            Age = add_age(DOB_estimate, Date, "Years"),
            Age_class = add_age_class(Age, Sex, Tenure_type)
          ) %>%
          mutate(Demographic = case_when(
            Age_class == "adult"   & Sex == "F" ~ "AF",
            Age_class == "adult"   & Sex == "M" ~ "AM",
            Age_class == "juvenile"             ~ "J",
            Age_class == "sub-adult"            ~ "SA",
            Age_class == "baby"                 ~ "B"
          )) %>%
          group_by(Demographic) %>%
          summarise(total = n(), .groups = "drop") %>%
          pivot_wider(names_from = Demographic, values_from = total, values_fill = list(total = 0)) %>%
          {
            # Ensure that all expected columns exist. If not, create them with value 0.
            expected <- c("AF", "AM", "B", "J", "SA")
            missing_cols <- setdiff(expected, names(.))
            if(length(missing_cols) > 0){
              for(col in missing_cols){
                .[[col]] <- 0
              }
            }
            .
          } %>%
          mutate(
            asr         = AM / (AM + AF),
            n_adults    = AM + AF,
            n_males     = AM,
            n_members   = AF + AM + B + J + SA,
            n_juveniles = J,
            n_subadults = SA,
            n_babies    = B
          )
        return(comp)
      }
    )) %>%
    unnest_wider(comp)

  return(df_out)
}

add_group_3monthbabies <- function(df, gp_column, date_column) {

  df_out <- df %>%
    mutate(comp = pmap(
      list(Group = .[[gp_column]], Date = .[[date_column]]),
      function(Group, Date) {
        comp <- lh %>%
          filter(
            Group_mb == Group,
            StartDate_mb <= Date,
            EndDate_mb >= Date
          ) %>%
          dplyr::select(AnimalCode, Group_mb, Sex, DOB_estimate, Tenure_type, StartDate_mb, EndDate_mb) %>%
          mutate(
            Age = add_age(DOB_estimate, Date, "Months")
          ) %>%
          mutate(Demographic = case_when(
            Age <= 3 ~ "baby",
            TRUE ~ "notbaby")
          ) %>%
          group_by(Demographic) %>%
          summarise(total = n(), .groups = "drop") %>%
          pivot_wider(names_from = Demographic, values_from = total, values_fill = list(total = 0)) %>%
          {
            # Ensure that all expected columns exist. If not, create them with value 0.
            expected <- c("baby", "notbaby")
            missing_cols <- setdiff(expected, names(.))
            if(length(missing_cols) > 0){
              for(col in missing_cols){
                .[[col]] <- 0
              }
            }
            .
          } %>%
          mutate(
            notbaby = notbaby,
            n_babies3month    = baby
          )
        return(comp)
      }
    )) %>%
    unnest_wider(comp)

  return(df_out)
}

# This is to help you report models.

# this you can add to your methods:
#To facilitate interpretation of effect magnitude, we additionally report standardized regression coefficients (β) obtained by refitting the model with predictors standardized
# (Gelman 2008; Ben-Shachar et al. 2020). These provide a common scale across predictors, allowing comparison of their relative importance.

# What this function does (simple summary):
# - Standardizes model coefficients (default via "basic""), returning standardized betas and CIs. But you can also use the method = "refit" that will standarize all your variables in case you haven´t done it.
# is better yo use basic if you have offset or zi formula, dispersion. Othewise refiut works too.
# - Adds abs_beta = |Std_Coefficient| to compare effect magnitudes. I report the abosolute beta standard.
# - Sorts terms by absolute effect size (largest first) and adds a rank.

# Returns only FIXED effects from the main (conditional) part of the model
standardized_effects <- function(model,
                                      method = "basic",
                                      keep_components = c("conditional","mean","location","count")) {

  out <- effectsize::standardize_parameters(model, method = method) %>%
    dplyr::as_tibble()

  # keep only fixed effects if column exists
  if ("Effects" %in% names(out)) {
    out <- dplyr::filter(out, Effects == "fixed")
  }

  # keep only the main/conditional component if column exists
  if ("Component" %in% names(out)) {
    out <- dplyr::filter(out, Component %in% keep_components)
  }

  out %>%
    dplyr::filter(Parameter != "(Intercept)") %>%
    dplyr::mutate(
      abs_beta = abs(Std_Coefficient),
    ) %>%
    dplyr::arrange(dplyr::desc(abs_beta)) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::select(Parameter, Std_Coefficient, CI_low, CI_high, abs_beta, rank)
}

# Function to plot predictor effect from linear model. Be aware that if the y was fitted as a log you will need to log all the predictor calculations, i.e. the
# lwr and upr condifent intervals and the fit
library(ggeffects)
ggpredict_unstadarized_glm <- function (model, model_data_base, var_to_plot){
  var_to_plot_all <- paste(var_to_plot, "[all]")
suppressWarnings(eff <- ggpredict(model, terms = var_to_plot_all) %>% as.data.frame())

# 2) unscale the x-axis (var_to_plot) to raw units
fit_idx <- as.integer(rownames(stats::model.frame(model)))

center <- model_data_base %>%
  dplyr::slice(fit_idx) %>%
  dplyr::summarise(mu = mean(.data[[var_to_plot]], na.rm = TRUE)) %>%
  dplyr::pull(mu)

scale <- model_data_base %>%
  dplyr::slice(fit_idx) %>%
  dplyr::summarise(s = sd(.data[[var_to_plot]], na.rm = TRUE)) %>%
  dplyr::pull(s)

eff_unscaled <- eff %>%
  dplyr::mutate(
    var_to_plot_raw = x * scale + center
  )
  return(eff_unscaled)
  }
predict_unstandardized_lmer <- function(model, df_raw, scaled_variables, var_to_plot, n_grid = 300){
# collect the rows actually used in the fitted model
fit_idx <- as.integer(rownames(model@frame))
# compute mean/sd on those rows for the scaled variables of the model
df_train_used <- df_raw %>% dplyr::slice(fit_idx)
  centers <- df_train_used %>% dplyr::summarise(across(all_of(scaled_variables), ~ mean(.x, na.rm = TRUE))) %>% as.list()
  scales <- df_train_used %>% dplyr::summarise(across(all_of(scaled_variables), ~ sd(.x, na.rm = TRUE))) %>% as.list()
# predictors actually used in the model (fixed effects only)
pred_vars <- all.vars(stats::delete.response(stats::terms(model)))
# now what we are doing is determine what is the mean value for the levels that are # not the variable we want to plot, to get a reliable prediction.
# reference row: mean for numeric, first level for factors
ref_row <- df_raw %>% dplyr::slice(fit_idx) %>%
  dplyr::select(dplyr::all_of(pred_vars)) %>%
  dplyr::summarise( dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                    dplyr::across(where(is.factor), ~ levels(.x)[1]) )
# raw sequence for focal predictor
var_to_plot_seq_from_raw <- df_raw %>% dplyr::slice(fit_idx) %>% # only keep predictor variables
 dplyr::summarise( # we calculate the range of values of our variable to plot
 xmin = min(.data[[var_to_plot]], na.rm = TRUE),
 xmax = max(.data[[var_to_plot]], na.rm = TRUE) ) %>%
# now we create a range of N_BgeService so is a smooth line
(\(x) seq(x$xmin, x$xmax, length.out = n_grid))()

# build raw prediction grid
newdata_raw <- ref_row[rep(1, n_grid), , drop = FALSE] %>% dplyr::mutate(!!var_to_plot := var_to_plot_seq_from_raw)
# standardize predictors to match model scale
newdata <- newdata_raw %>%
  dplyr::mutate( dplyr::across( dplyr::all_of(scaled_variables), ~ (.x - centers[[cur_column()]]) / scales[[cur_column()]] ) )
# feed the new data into predict
pred <- predict( model,
                 newdata = newdata,
                 re.form = NA, # ignore random effects
                 se.fit = TRUE ) # attach predictions to the grid
pred_df <- newdata_raw %>% dplyr::mutate( fit = pred$fit, se = pred$se.fit )
# STEP 6: back-transform to original response scale
pred_df <- pred_df %>% dplyr::mutate(
  lwr = fit - 1.96 * se, # 95% CI
  upr = fit + 1.96 * se, # 95% CI
  fit = exp(fit), lwr = exp(lwr), upr = exp(upr) )

  return(pred_df) }