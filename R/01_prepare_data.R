library(tidyverse)
library(lubridate)


## LOAD DATA ======================================================================

###  BTW RESULTS ==================================
# Source: https://www.bundeswahlleiter.de/bundestagswahlen/2017/ergebnisse/weitere-ergebnisse.html

url_btw <- "https://www.bundeswahlleiter.de/dam/jcr/2e018ffc-0368-4c87-b85f-23dae3a5c8f5/btw2017kreis.csv"
path_btw <- file.path("data", "btw2017kreis.csv")

if (!file.exists(path_btw)) {
  download.file(url_btw, destfile = path_btw)  
}

btw_lines <- read_lines(path_btw, 
                        skip_empty_rows = TRUE, n_max = 10, 
                        locale = locale(encoding = "iso-8859-15"))
header_part1 <- str_split(btw_lines[5], pattern = ";") %>% unlist()
header_part2 <- str_split(btw_lines[6], pattern = ";") %>% unlist()
header <- str_c(header_part2, header_part1, sep = "_") %>% 
  str_remove_all("\\\n") %>% 
  str_remove_all("^_")
header[c(2, 3)] <- c("Kennziffer", "Landkreis")


btw <- read_csv2(path_btw, skip = 11, col_names = header)

btw <- btw %>% 
  filter(!is.na(Land))


### POPULATION ==============================
# Source: https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/04-kreise.html

url_population <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/04-kreise.xlsx?__blob=publicationFile"
path_population <- file.path("data", "kreise.xlsx")

if (!file.exists(path_population)) {
  download.file(url_population, destfile = path_population, mode = "wb")  
}

population <- readxl::read_xlsx(path_population, sheet = 2, skip = 3) %>% 
  na.omit() %>% 
  filter(`Schlüssel-nummer` != "1")

colnames(population) <- c("kennziffer", "regionale_bezeichnung", "name", "NUTS3",
                          "flaeche_km2", "einwohner", "bev_maennlich", "bev_weiblich", "bev_dichte")

population <- population %>% 
  mutate(across(einwohner:bev_dichte, as.numeric),
         bev_anteil_m = bev_maennlich / einwohner)



### CORONA CASES =================================

url_corona_lk_api <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
corona_lk_daily_df <- read_csv(url_corona_lk_api)
write_rds(corona_lk_daily_df, file.path("data", "corona_lk_daily_df.rds"), compress = "gz")
corona_lk_daily_df <- read_rds(file.path("data", "corona_lk_daily_df.rds"))

corona_lk_daily_grp_df <- corona_lk_daily_df %>%
  mutate(Meldedatum = as_date(Meldedatum)) %>% 
  group_by(IdLandkreis, Landkreis, Bundesland, Meldedatum) %>% 
  summarize(cases = sum(AnzahlFall), .groups = "drop") %>% 
  complete(Meldedatum, nesting(IdLandkreis, Landkreis, Bundesland), fill = list(cases = 0)) %>%  
  inner_join(population, by = c("IdLandkreis" = "kennziffer"))

corona_lk_weekly_grp_df <- corona_lk_daily_grp_df %>% 
  mutate(Meldeinterval = floor_date(Meldedatum, "1 week", week_start = 1)) %>% 
  group_by(IdLandkreis, Landkreis, Bundesland, Meldeinterval) %>% 
  summarize(cases = sum(cases), 
            einwohner = max(einwohner), 
            .groups = "drop") %>% 
  mutate(cases_per_100k = cases / einwohner * 10 ^ 5) %>% 
  select(IdLandkreis, Landkreis, Bundesland, Meldeinterval, cases, cases_per_100k) 

corona_lk_monthly_grp_df <- corona_lk_daily_grp_df %>%
  mutate(Meldeinterval = floor_date(Meldedatum, "1 month")) %>%
  group_by(IdLandkreis, Landkreis, Bundesland, Meldeinterval) %>%
  summarize(
    cases = sum(cases),
    einwohner = max(einwohner),
    .groups = "drop"
  ) %>%
  mutate(
    cases_per_100k = cases / einwohner * 10 ^ 5,
    cases7_per_100k = cases_per_100k * 7 / days_in_month(Meldeinterval),
    cases7_per_100k = ifelse(
      floor_date(today(), "1 month") == Meldeinterval,
      cases_per_100k * 7 / day(today()),
      cases7_per_100k
    )
  ) %>%
  select(
    IdLandkreis,
    Landkreis,
    Bundesland,
    Meldeinterval,
    cases,
    cases_per_100k,
    cases7_per_100k
  )


corona_lk_weekly_grp_df %>% 
  summarize(max(Meldeinterval))
    

plot_cases_by_state <- function(x, color) {
  corona_lk_weekly_grp_df %>% 
    filter(Meldeinterval < as_date("2020-12-07")) %>% 
    ggplot(aes(Meldeinterval, cases_per_100k)) +
    geom_line(aes(group = Landkreis), size = 0.05, col = "grey50") +
    geom_line(data = . %>% filter(Bundesland == x),
              aes(group = Landkreis), size = 0.1, col = color) +
    theme_minimal()
}

p1 <- plot_cases_by_state("Sachsen", "red")
p2 <- plot_cases_by_state("Bayern", "blue")
p3 <- plot_cases_by_state("Nordrhein-Westfalen", "green") 

library(patchwork)

p1 / p2 / p3



## STAATSGRENZEN ==========================================

lk_grenzen <- read_csv(file.path("data", "landkreise_staatsgrenze.csv"), 
                       guess_max = 400, locale = locale(encoding = "iso-8859-15"))
glimpse(lk_grenzen)

lk_grenzen <- lk_grenzen %>% 
  mutate(has_borders = !is.na(Grenze),
         has_borders_dk = str_detect(Grenze, "DK"),
         has_borders_nl = str_detect(Grenze, "NL"),
         has_borders_be = str_detect(Grenze, "BE"),
         has_borders_lu = str_detect(Grenze, "LU"),
         has_borders_fr = str_detect(Grenze, "FR"),
         has_borders_ch = str_detect(Grenze, "CH"),
         has_borders_at = str_detect(Grenze, "AT"),
         has_borders_cz = str_detect(Grenze, "CZ"),
         has_borders_pl = str_detect(Grenze, "PL"))


## THÜNEN ===========================================
# https://karten.landatlas.de/app/landatlas/

# function to read XLXS files from Thünen Institut
read_thuenen_xlsx <- function(path, sheet = 1) {
  message(path)
  readxl::read_xlsx(path, sheet, 
                    col_types = c("skip", "text", "text", "text", "numeric")) %>% 
    rename(value = 4)  %>% 
    mutate(t_typ = factor(str_sub(t_typ, 1, 1), 
                          levels = c(3, 1, 2, 4, 5))) %>% 
    # create a consistent ID
    mutate(Kennziffer = ifelse(str_length(Kennziffer) > 5,
                               str_sub(Kennziffer, 1, str_length(Kennziffer) - 5),
                               Kennziffer
                               )%>% 
             str_pad(width = 5, side = "left", pad = "0")) 
}


paths_thuenen <- list(
  laendlichkeit = "Laendlichkeit_Kreise_2016.xlsx",
  erreichbarkeit_krankenhaus = "Krankenhaus Regelversorgung_Kreise_2019.xlsx",
  erreichbarkeit_polizei = "Polizei_Kreise_2016.xlsx",
  erreichbarkeit_schule_sek2 = "Schule mit SEK II_Kreise_2016.xlsx",
  erreichbarkeit_bushaltestelle = "Bushaltestelle_Kreise_2018.xlsx",
  erreichbarkeit_jobcenter = "Jobcenter_Kreise_2017.xlsx",
  erreichbarkeit_discounter = "Discounter_Kreise_2017.xlsx",
  erreichbarkeit_oberzentrum = "Oberzentrum_Kreise_2014.xlsx",
  erreichbarkeit_hausarzt = "Hausarzt_Kreise_2016.xlsx",
  breitband = "Breitband_Kreise_2017.xlsx",
  lte = "LTE_Kreise_2019.xlsx",
  wohnflaeche = "Wohnfläche_Kreise_2017.xlsx",
  beschaeftigte_akademisch = "Beschäftigte mit akademischem Abschluss_Kreise_2017.xlsx",
  einkuenfte = "Einkünfte_Kreise_2017.xlsx",
  schulabg_abi = "Schulabgänger mit Hochschulreife_Kreise_2017.xlsx",
  schulabg_ohne = "Schulabgänger ohne Abschluss_Kreise_2017.xlsx",
  bip = "Bruttoinlandsprodukt_Kreise_2016.xlsx",
  altersgrp_65 = "Altersgruppen_65_Kreise_2017.xlsx",
  altersgrp_75 = "Altersgruppen_75_Kreise_2017.xlsx",
  stationaere_pflege = "Stationäre Pflege_Kreise_2017.xlsx",
  wanderungen = "Wanderungen_Kreise_2017.xlsx",
  bev_entwicklung = "Nat. Bevölkerungsentwicklung_Kreise_2017.xlsx",
  siedlungsdichte = "Siedlungsdichte_Kreise_2017.xlsx",
  lifexp_m = "Männliche Lebenserwartung_Kreise_2017.xlsx",
  steuerkraft = "Kommunale Steuerkraft_Kreise_2017.xlsx",
  schulden = "Kommunale Schulden_Kreise_2017.xlsx"
)


thuenen_data <- map(paths_thuenen, ~read_thuenen_xlsx(file.path("data", .x)))

thuenen_df <- bind_rows(thuenen_data, .id = "key") %>% 
  pivot_wider(id_cols = c("Kennziffer", "t_typ"), names_from = "key", 
              values_from = "value", values_fn = max) %>% 
  mutate(wanderungen_neg = wanderungen < 0)
dim(thuenen_df)

levels(thuenen_df$t_typ)
#' Thünen-Typen:
#' 
#' 1 = "sehr ländlich/weniger gute sozioökonomische Lage"
#' "sehr ländlich/gute sozioökonomische Lage"
#' "eher ländlich/gute sozioökonomische Lage"
#' "eher ländlich/weniger gute sozioökonomische Lage"
#' 5 = "nicht ländlich"
         


## OST-WEST
ost <- c("BB", "MV", "ST", "SN", "TH")
  
  
#################################################


interval <- "month"

if (interval == "month") {
  df <- btw %>% 
    inner_join(corona_lk_monthly_grp_df, by = c("Kennziffer" = "IdLandkreis"), suffix = c("", ".y"))
} else if (interval == "week") {
  df <- btw %>% 
    inner_join(corona_lk_weekly_grp_df, by = c("Kennziffer" = "IdLandkreis"), suffix = c("", ".y"))
}

df <- df %>% 
  inner_join(lk_grenzen, by = "Kennziffer", suffix = c("", ".y")) %>% 
  inner_join(population, by = c("Kennziffer" = "kennziffer"), suffix = c("", ".y")) %>% 
  inner_join(thuenen_df, by = "Kennziffer", suffix = c("", ".y")) %>% 
  mutate(ost = (Land %in% ost)) %>% 
  mutate(z_afd = 100 * Zweitstimmen_AfD / `Zweitstimmen_Gültige`,
         z_gruene = 100 * `Zweitstimmen_GRÜNE` / `Zweitstimmen_Gültige`,
         z_spd = 100 * `Zweitstimmen_SPD` / `Zweitstimmen_Gültige`,
         z_fdp = 100 * `Zweitstimmen_FDP` / `Zweitstimmen_Gültige`,
         z_linke = 100 * `Zweitstimmen_DIE LINKE` / `Zweitstimmen_Gültige`,
         z_union = 100 * (`Zweitstimmen_CDU` + `Zweitstimmen_CSU`) / `Zweitstimmen_Gültige`,
         wbt = `Wähler` / `Wahlberechtigte`) %>% 
  mutate(across(starts_with("has_borders_"), replace_na, FALSE)) %>% 
  # remove the absolute vote counts
  select(-starts_with("Erststimmen_"), -starts_with("Zweitstimmen_")) %>% 
  # remove the per-country border variables
  select(-starts_with("Grenze_")) %>% 
  # rename(cases7_per_100k = cases_per_100k) %>% 
  select(-ends_with(".y"))



## Create train/test split ==============================

library(tidymodels)

# set.seed(123)
# df_split <- initial_split(df, p = 0.8)
# df_train <- training(df_split)
# df_test <- testing(df_split)

set.seed(123)
df_split <- df %>% 
  group_split(Meldeinterval) %>% 
  map(initial_split, p = 0.8) 
df_train <- map(df_split, training) %>% bind_rows()
df_test <- map(df_split, testing) %>% bind_rows()

## Preprocessing =========================================
rec <- 
  recipe(cases7_per_100k ~ ., data = df_train) %>% 
  update_role(Land, Kennziffer, Landkreis, Bundesland, name, regionale_bezeichnung, new_role = "ID") %>% 
  step_other(all_nominal(), -has_role("ID"), threshold = 0.05) %>% 
  step_nzv(all_predictors()) %>% 
  step_dummy(has_role("predictor"), -all_numeric()) %>%  
  step_mutate(z_afd_log = log(z_afd + 1)) %>%
  step_normalize(all_numeric(), -has_role("outcome"), -cases)

rec

trained_rec <- prep(rec, training = df_train)
df_train_prep <- bake(trained_rec, new_data = df_train)
df_test_prep <- bake(trained_rec, new_data = df_test)

# split into weekly dataframes
start <- as_date("2020-03-01")
end <- as_date("2020-12-13")

df_names <- arrange(df, Meldeinterval) %>%
  filter(Meldeinterval >= start & Meldeinterval <= end) %>% 
  distinct(Meldeinterval) %>% 
  pull(Meldeinterval)

# create a list of dataframes split by week
dfs <- df_train_prep %>% 
  arrange(Meldeinterval) %>% 
  group_split(Meldeinterval) %>% 
  keep(~max(.x$Meldeinterval) >= start & max(.x$Meldeinterval) <= end) %>% 
  set_names(df_names)

# save list of dataframes
write_rds(dfs, file.path("output", "dfs.rds"))

