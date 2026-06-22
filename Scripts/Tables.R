# ---------------
# Title: Create tables for publication
# Date: 4 feb 2026
# Author: mgranellruiz
# Goal: create the tables to is reproducible and transparent where the numebrs come from :)
# ---------------

# library ---------------------
# data manipulation
library(dplyr)
library(stringr)
library(tidyr)
library(gt)
library(broom)
source('/Users/mariagranell/Repositories/data/functions.R')

# path ------------------------
setwd("/Users/mariagranell/Repositories/male_services_index/MSpublication/Figures")

### MODEL 1 ###
{ # data ------------------------
  model1_anova_alarm<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_alarm.csv")
  model1_beta_alarm<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_alarm.csv")
  model1_anova_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_bge.csv")
  model1_beta_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_bge.csv")
  model1_anova_crs<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_crs.csv")
  model1_beta_crs<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_crs.csv")
  model1_anova_sent<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_anova_sent.csv")
  model1_beta_sent<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model1_beta_sent.csv")


  # functions
  create_table<-function(anova_table, beta_table) {
    combined<-anova_table |>
      mutate(Term=str_replace(Term, "asr_z", "ASR"),
             Term=str_replace(Term, "bge", "BGE")) |>
      dplyr::left_join(beta_table, by="Term") |>
      dplyr::mutate(
        sig=p.value < 0.05
      )
    return(combined)
  }

  table_maker<-function(wide_table) {
    tbl_gt<-
      wide_table |>
        gt(rowname_col="Term") |>
        tab_header(title="Model results") |>
        tab_spanner("Alarm", columns=starts_with("Alarm_")) |>
        tab_spanner("BGE", columns=starts_with("BGE_")) |>
        tab_spanner("Sentinelling", columns=starts_with("Sentinelling_")) |>
        tab_spanner("Crossing", columns=starts_with("Crossing_")) |>
        cols_label(
          Alarm_Chisq="Chisq", Alarm_Df="Df", Alarm_p.value="p-value", Alarm_beta_std=gt::md("|β<sub>std</sub>|"), Alarm_sig="sig",
          BGE_Chisq="Chisq", BGE_Df="Df", BGE_p.value="p-value", BGE_beta_std=gt::md("|β<sub>std</sub>|"), BGE_sig="sig",
          Sentinelling_Chisq="Chisq", Sentinelling_Df="Df", Sentinelling_p.value="p-value", Sentinelling_beta_std=gt::md("|β<sub>std</sub>|"), Sentinelling_sig="sig",
          Crossing_Chisq="Chisq", Crossing_Df="Df", Crossing_p.value="p-value", Crossing_beta_std=gt::md("|β<sub>std</sub>|"), Crossing_sig="sig"
        ) |>
        # hide sig columns (but keep them for bolding rules below)
        cols_hide(columns=ends_with("_sig")) |>
        # round numeric columns (except p-value; we format that next)
        fmt_number(columns=ends_with("_Chisq"), decimals=3) |>
        fmt_number(columns=ends_with("_beta_std"), decimals=3) |>
        fmt_number(columns=ends_with("_Df"), decimals=0) |>
        # p-values: 3 decimals, but show < 0.001 instead of 0.000
        fmt(columns=ends_with("_p.value"),
            fns=function(x) ifelse(is.na(x), "-", ifelse(x < 0.001, "< 0.001", format(round(x, 3), nsmall=3)))) |>
        # bold each model block when sig == TRUE
        tab_style(style=cell_text(weight="bold"),
                  locations=cells_body(
                    columns=c(Alarm_Chisq, Alarm_Df, Alarm_p.value, Alarm_beta_std),
                    rows=Alarm_sig == TRUE
                  )) |>
        tab_style(style=cell_text(weight="bold"),
                  locations=cells_body(
                    columns=c(BGE_Chisq, BGE_Df, BGE_p.value, BGE_beta_std),
                    rows=BGE_sig == TRUE
                  )) |>
        tab_style(style=cell_text(weight="bold"),
                  locations=cells_body(
                    columns=c(Sentinelling_Chisq, Sentinelling_Df, Sentinelling_p.value, Sentinelling_beta_std),
                    rows=Sentinelling_sig == TRUE
                  )) |>
        tab_style(style=cell_text(weight="bold"),
                  locations=cells_body(
                    columns=c(Crossing_Chisq, Crossing_Df, Crossing_p.value, Crossing_beta_std),
                    rows=Crossing_sig == TRUE
                  )) |>
        # italics for trends (0.05 <= p < 0.1)
        tab_style(style=cell_text(style="italic"),
                  locations=cells_body(
                    columns=c(Alarm_Chisq, Alarm_Df, Alarm_p.value, Alarm_beta_std),
                    rows=Alarm_sig == FALSE & Alarm_p.value < 0.1
                  )) |>
        tab_style(style=cell_text(style="italic"),
                  locations=cells_body(
                    columns=c(BGE_Chisq, BGE_Df, BGE_p.value, BGE_beta_std),
                    rows=BGE_sig == FALSE & BGE_p.value < 0.1
                  )) |>
        tab_style(style=cell_text(style="italic"),
                  locations=cells_body(
                    columns=c(Sentinelling_Chisq, Sentinelling_Df, Sentinelling_p.value, Sentinelling_beta_std),
                    rows=Sentinelling_sig == FALSE & Sentinelling_p.value < 0.1
                  )) |>
        tab_style(style=cell_text(style="italic"),
                  locations=cells_body(
                    columns=c(Crossing_Chisq, Crossing_Df, Crossing_p.value, Crossing_beta_std),
                    rows=Crossing_sig == FALSE & Crossing_p.value < 0.1
                  )) |>
        opt_table_outline() |>
        opt_align_table_header(align="left") |>
        sub_missing(everything(), missing_text="-")

    return(tbl_gt)
  }

  ### MODEL 1 ###
  model1_alarm_table<-create_table(model1_anova_alarm, model1_beta_alarm)
  model1_bge_table<-create_table(model1_anova_bge, model1_beta_bge)
  model1_crs_table<-create_table(model1_anova_crs, model1_beta_crs)
  model1_sent_table<-create_table(model1_anova_sent, model1_beta_sent)

  # Combine tables
  wide_tbl<-
    dplyr::bind_rows(
      model1_alarm_table |> dplyr::mutate(Model="Alarm"),
      model1_bge_table   |> dplyr::mutate(Model="BGE"),
      model1_sent_table  |> dplyr::mutate(Model="Sentinelling"),
      model1_crs_table   |> dplyr::mutate(Model="Crossing")
    ) |>
      dplyr::select(Model, Term, Chisq, Df, p.value, beta_std, sig) |>
      tidyr::pivot_wider(
        names_from=Model,
        values_from=c(Chisq, Df, p.value, beta_std, sig),
        names_glue="{Model}_{.value}"
      ) |>
      dplyr::arrange(Term)

  # Make a table with spanners + bold significant blocks
  model1_table<-table_maker(wide_tbl)
  # Export to word.
  gt::gtsave(model1_table, filename="model1_results_table.docx")
}

### MODEL 2 ###
{ # data ------------------------
  model2_anova_alarm<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_alarm.csv")
  model2_beta_alarm<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_alarm.csv")
  model2_anova_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_bge.csv")
  model2_beta_bge<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_bge.csv")
  model2_anova_crs<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_crs.csv")
  model2_beta_crs<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_crs.csv")
  model2_anova_sent<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_anova_sent.csv")
  model2_beta_sent<-read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/OutputFiles/model2_beta_sent.csv")

  ### MODEL 2 ###
  model2_alarm_table<-create_table(model2_anova_alarm, model2_beta_alarm)
  model2_bge_table<-create_table(model2_anova_bge, model2_beta_bge)
  model2_crs_table<-create_table(model2_anova_crs, model2_beta_crs)
  model2_sent_table<-create_table(model2_anova_sent, model2_beta_sent)


  term_order<-c(
    # model testing
    "Father", "Rank", "FutureMounts", "PastMounts",
    "Rank:Father", "Rank:FutureMounts", "Rank:PastMounts",
    # testing for context
    "PredatorThreat", "Father:PredatorThreat", "Rank:PredatorThreat", "Rank:Father:PredatorThreat",
    "BGE_intensity", "Father:BGE_intensity", "Rank:BGE_intensity", "Rank:Father:BGE_intensity",
    # to control for
    "ASR",
    "zCSI",
    "Season",
    "Group",
    "Unhabituated",
    "FutureMounts:Season"
  )


  # Combine tables
  wide_tbl<-
    dplyr::bind_rows(
      model2_alarm_table |> dplyr::mutate(Model="Alarm"),
      model2_bge_table   |> dplyr::mutate(Model="BGE"),
      model2_sent_table  |> dplyr::mutate(Model="Sentinelling"),
      model2_crs_table   |> dplyr::mutate(Model="Crossing")) |>
      dplyr::select(Model, Term, Chisq, Df, p.value, beta_std, sig) |>
      tidyr::pivot_wider(
        names_from=Model,
        values_from=c(Chisq, Df, p.value, beta_std, sig),
        names_glue="{Model}_{.value}") |>
      dplyr::mutate(Term=str_replace(Term, "asr", "ASR"),
                    Term=str_replace(Term, "elo_12m", "Rank"),
                    Term=str_replace(Term, "mount_coming12", "FutureMounts"),
                    Term=str_replace(Term, "mount_last12", "PastMounts"),
                    Term=str_replace(Term, "Threat", "PredatorThreat"),
                    Term=str_replace(Term, "bge_intensity", "BGE_intensity"),
      ) |>
      dplyr::mutate(Term=factor(Term, levels=term_order)) |>
      dplyr::arrange(Term)

  # Make a table with spanners + bold significant blocks
  model2_table<-table_maker(wide_tbl)
  # Export to word.
  gt::gtsave(model2_table, filename="model2_results_table.docx") }

### MODEL 3 ###
mating1 <- read.csv("/Users/mariagranell/Repositories/male_services_index/MSpublication/Public_data/mating_df_models_p.csv")

mating1 %>% group_by(Group_mb, year) %>%
  summarise(n = sum(number_matings)) %>%
  pivot_wider(names_from = Group_mb, values_from = n)


