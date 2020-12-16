library(tidyverse)
library(lubridate)
library(ggtext)
library(broom)
library(colorspace)
library(extrafont)

# load fonts from system
loadfonts()

# custom ggplot theme
custom_theme <- theme_minimal(base_family = "Source Sans Pro") +
  theme(legend.position = "top",
        legend.justification = "left",
        text = element_text(color = "grey20"),
        plot.title = element_markdown(family = "Source Sans Pro SemiBold", color = "black",
                                      lineheight = 1.2),
        plot.subtitle = element_markdown(lineheight = 1.2),
        plot.caption = element_markdown(hjust = 0, padding = margin(t = 10),
                                        lineheight = 1),
        strip.text = element_text(family = "Source Sans Pro SemiBold", color = "grey20"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey92"))

theme_set(custom_theme)

colors_sequential <- sequential_hcl(4, palette = "Blues 3")
colors_qualitative <- qualitative_hcl(2, palette = "Harmonic")


# fit models and return estimators and metrics
fit_models <- function(formula, df_list = dfs) {
  mods <- map(df_list, ~lm(formula, data = .x))
  mods_metrics <- map_df(mods, glance)
  mods_tidy <- map(mods, tidy)
  list("terms" = mods_tidy, "metrics" = mods_metrics)
}


# plot the estimators with errorbars for each t for the variable selected
plot_estimates <- function(m, x = "z_afd", ylim = c(-0.5, 2)) {
  terms_str <- str_c(unique(m[[1]]$term), collapse = "+") %>% 
    str_remove("\\(Intercept\\)\\+")
  
  p <- bind_rows(m, .id = "month") %>% 
    mutate(month = as_date(month)) %>% 
    filter(term == x) %>% 
    ggplot(aes(month, estimate)) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = estimate - 1.96 * `std.error`, 
                      ymax = estimate + 1.96 * `std.error`),
                  width = 2, size = 0.25,  col = "grey70") +
    geom_point(aes(shape = ifelse(p.value < 0.05, "signifikant", "nicht signifikant")),
               size = 2.5, col = colors_sequential[1], fill = "white") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_color_manual(values = c("signifikant" = "#225ADD", "nicht signifikant" = "grey60"), 
                       name = "") +
    scale_shape_manual(values = c("signifikant" = 16, "nicht signifikant" = 21)) +
    coord_cartesian(ylim = ylim) +
    labs(title = "Schätzer AfD-Stimmenanteil je Woche",
         subtitle = terms_str,
         x = NULL, y = "Regressionskoeffizient",
         shape = NULL) +
    theme(plot.subtitle = element_markdown(size = 8),
          axis.text.x = element_text())
  print(p)
}


# function to calculate the share of times a predictor had a significant effect
calculate_significant_share <- function(model) {
  bind_rows(model[["terms"]], .id = "week") %>% 
    filter(term != "(Intercept)") %>% 
    group_by(term) %>% 
    summarize(sig_share = sum(p.value < 0.05) / n()) %>% 
    arrange(-sig_share)
}


## FIND A GOOD BASELINE MODEL =========================================================

## full period combined
df_train_prep_combined <- df_train_prep %>% 
  nest(data = -Kennziffer) %>% 
  mutate(total_cases = map_dbl(data, ~sum(.x$cases7_per_100k))) %>% 
  unnest(data, names_repair = "unique") %>% 
  # mutate(has_borders = as.numeric(has_borders)) %>% 
  group_by(Kennziffer) %>% 
  summarize_if(function(x) is.numeric(x) | is.logical(x), max)

dim(df_train_prep_combined)
glimpse(df_train_prep_combined)

if (!exists("mods")) {
  mods <- list()
}


formulas <- c(total_cases ~ siedlungsdichte  + has_borders + t_typ_X1 + t_typ_X4 + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche,
              total_cases ~ siedlungsdichte  + has_borders + t_typ_X1 + t_typ_X4 + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + wohnflaeche + bip,
              total_cases ~ siedlungsdichte  + has_borders + t_typ_X1 + t_typ_X4 + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + breitband,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_schule_sek2,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_schule_sek2 + 
                erreichbarkeit_bushaltestelle,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_schule_sek2 + 
                erreichbarkeit_jobcenter,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_schule_sek2 + 
                erreichbarkeit_discounter,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_discounter,
              total_cases ~ bev_dichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_discounter + 
                erreichbarkeit_oberzentrum,
              total_cases ~ siedlungsdichte  + has_borders + bev_entwicklung + schulabg_abi + beschaeftigte_akademisch + 
                wohnflaeche + laendlichkeit + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_discounter + 
                erreichbarkeit_oberzentrum + lifexp_m,
              total_cases ~ siedlungsdichte + has_borders + bev_entwicklung + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + 
                erreichbarkeit_oberzentrum + erreichbarkeit_krankenhaus + erreichbarkeit_polizei,
              total_cases ~ siedlungsdichte + has_borders + bev_entwicklung + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + 
                erreichbarkeit_oberzentrum + erreichbarkeit_krankenhaus + erreichbarkeit_polizei + erreichbarkeit_hausarzt,
              total_cases ~ siedlungsdichte + has_borders + bev_entwicklung + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + 
                erreichbarkeit_polizei + erreichbarkeit_hausarzt,
              total_cases ~ siedlungsdichte + has_borders + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + schulabg_ohne +
                erreichbarkeit_oberzentrum  + erreichbarkeit_polizei + erreichbarkeit_hausarzt + 
                altersgrp_75,
              total_cases ~ siedlungsdichte + has_borders + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + schulabg_ohne +
                erreichbarkeit_oberzentrum  + erreichbarkeit_polizei + erreichbarkeit_hausarzt + 
                altersgrp_75 + steuerkraft,
              total_cases ~ siedlungsdichte + has_borders + 
                beschaeftigte_akademisch + wohnflaeche + schulabg_abi + schulabg_ohne +
                erreichbarkeit_oberzentrum  + erreichbarkeit_polizei + erreichbarkeit_hausarzt + 
                altersgrp_75 + steuerkraft
              )

# fit many models
mods <- map(formulas, fit_models, df_list = list(df_train_prep_combined))

map(mods, pluck, "terms")

mods[[length(mods)]][["terms"]] %>% 
  bind_rows() %>% 
  arrange(p.value)

map_dfr(mods, pluck, "metrics", .id = "model") %>% 
  mutate(model = as.numeric(model)) %>% 
  arrange(BIC) %>% View()

# check BIC/AIC
bind_rows(mods[[length(mods)]][["metrics"]], .id = "week") %>% 
  summarize_at(vars(AIC), .funs = list(median = median, min = min, max = max))


# plot BIC per model
map_dfr(mods, pluck, "metrics", .id = "model") %>%
  mutate(model = as.numeric(model)) %>% 
  group_by(model) %>% 
  summarize_at(vars(BIC), .funs = list(mean = mean, sd = sd, 
                                       median = median, min = min, max = max#,
#                                       perc25 = ~quantile(.x, 0.25),
#                                       perc75 = ~quantile(.x, 0.75)
  )) %>% 
  ggplot(aes(model)) +
  geom_ribbon(aes(ymin = min, ymax = max),
              alpha = 0.1, col = "grey30", fill = "grey30") +
  geom_line(aes(y = mean), col = "red", size = 1) 


# fit final baseline model per week
formulas <- list(cases7_per_100k ~ siedlungsdichte + has_borders + 
                   beschaeftigte_akademisch + wohnflaeche + schulabg_abi + schulabg_ohne +
                   erreichbarkeit_oberzentrum  + erreichbarkeit_polizei + erreichbarkeit_hausarzt + 
                   altersgrp_75 + steuerkraft)
mods <- map(formulas, fit_models)



## WITH AfD =========================================================

formulas_with_afd <- list(
  cases7_per_100k ~ z_afd + siedlungsdichte + has_borders + 
    beschaeftigte_akademisch + wohnflaeche + schulabg_abi + schulabg_ohne +
    erreichbarkeit_oberzentrum  + erreichbarkeit_polizei + erreichbarkeit_hausarzt + 
    altersgrp_75 + steuerkraft
)

# fit many models
mods_with_afd <- map(formulas_with_afd, fit_models)

# write models object to file
write_rds(mods_with_afd, file.path("output", "models.rds"))

walk(mods_with_afd, ~plot_estimates(.x[["terms"]], ylim = c(-10, 90)))

# Plot latest model only
plot_estimates(mods_with_afd[[length(mods_with_afd)]][["terms"]], ylim = c(-10, 100))
ggsave(file.path("plots", "Estimator_AfD_BestModel.png"), type = "cairo", dpi = 200, width = 6, height = 4, scale = 1.1)


map(mods_with_afd[[1]][["terms"]], ~mutate(.x, sig = ifelse(p.value < 0.05, "*", "")))
  

mean(df$z_afd)
sd(df$z_afd)
min(df$z_afd)
max(df$z_afd)

bind_rows(mods_with_afd[[length(mods_with_afd)]][["terms"]], .id = "week") %>% 
  filter(week == max(week)) %>% 
  knitr::kable()


extract_rsquared <- function(model, adjusted = TRUE) {
  metrics <- model[["metrics"]]
  if (adjusted) {
    r2_name <- sym("r.squared")
  } else {
    r2_name <- sym("adj.r.squared")
  }
  r2 <- metrics %>% 
    mutate(row = row_number()) %>% 
    select(row, r2 = {{r2_name}})
  r2
}

extract_bic <- function(model) {
  metrics <- model[["metrics"]]
  bic <- metrics %>% 
    mutate(row = row_number()) %>% 
    select(row, BIC)
  bic
}


plot_rsquared <- function(model, adjusted = TRUE) {
  metrics <- extract_rsquared(model, adjusted)
  p <- metrics %>% 
    mutate(row = row_number()) %>% 
    ggplot(aes(row, r2)) +
    geom_line() +
    coord_cartesian(ylim = c(0, 0.6))
  print(p)
}


p_r2_baseline <- plot_rsquared(mods[[length(mods)]])
p_best <- plot_rsquared(mods_with_afd[[length(mods_with_afd)]])

r2_baseline <- extract_rsquared(mods[[length(mods)]])
r2_best <- extract_rsquared(mods_with_afd[[length(mods_with_afd)]])


start <- as_date("2020-03-01")
end <- as_date("2020-12-13")
weeks <- unique(df$Meldeinterval[df$Meldeinterval >= start & df$Meldeinterval < end])
weeks <- unique(df$Meldeinterval[df$Meldeinterval >= start & df$Meldeinterval < end])

bind_rows(r2_baseline, r2_best, .id = "model") %>%
  bind_cols(week = rep(weeks, 2)) %>% 
  mutate(model = ifelse(model == 1, "Baseline-Modell", "Modell mit AfD") %>% 
           fct_rev()) %>% 
  ggplot(aes(week, r2)) +
  geom_line(aes(group = week), 
            col = "grey50", lty = "dashed", size = 0.2) +
  geom_point(aes(fill = model, size = model), 
             alpha = 0.8, shape = 21, col = "white") +
  # annotate("richtext", label = "<b style='color:colors_qualitative[1]'>Baseline<b>") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete_qualitative(palette = "Harmonic") +
  scale_color_discrete_qualitative(palette = "Harmonic") +
  scale_size_manual(values = c(2, 4)) +
  guides(size = FALSE) +
  labs(title = "AfD-Modell ab November mit höherem R<sup>2</sup> als das Baseline-Modell",
       subtitle = "Je höher das adj. R<sup>2</sup>, desto besser erklärt das Modell
       die unterschiedlichen<br>7-Tage-Inzidenzen in den Landkreisen",
       x = NULL, y = "Korrigiertes R<sup>2</sup>", fill = NULL, col = NULL) +
  theme(axis.title.y = element_markdown())

ggsave("plots/AdjR2_baseline-vs-best.png", type = "cairo", dpi = 200, width = 6, height = 4)




## Model without Saxony
mods_with_afd <- map(formulas_with_afd, 
                     fit_models, 
                     df_list = map(dfs, filter, Bundesland != "Sachsen"))
walk(mods_with_afd, ~plot_estimates(.x[["terms"]], ylim = c(-10, 80)))



## Fit final model and predict

mod_final <- lm(formulas[[length(formulas)]], data = dfs[[length(dfs)]])
summary(mod_final)

test_max_week <- df_test_prep %>% 
  filter(Meldeinterval == max(Meldeinterval))

pred <- predict(mod_final, newdata = test_max_week)


test_max_week %>% 
  select(Meldeinterval, z_afd, cases7_per_100k) %>% 
  bind_cols(pred = pred) %>% 
  ggplot(aes(cases7_per_100k, pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)


count(df_test_prep, Meldeinterval) %>% 
  arrange(desc(Meldeinterval))



bind_rows(mods[[10]][["terms"]], .id = "week") %>% 
  filter(term != "(Intercept)") %>% 
  # filter(p.value < 0.05) %>% 
  group_by(term) %>% 
  summarize(sig_share = sum(p.value < 0.05) / n()) %>% 
  arrange(-sig_share)


