library(tidyverse)
library(ggtext)
library(colorspace)
library(glue)
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



latest_week <- as_date("2020-12-07")

df %>% 
  filter(Meldewoche == latest_week) %>% 
  ggplot(aes(z_afd, cases7_per_100k)) +
  geom_point(aes(col = ost), alpha = 0.7) +
  geom_smooth(method = "lm") +
  colorspace::scale_color_discrete_qualitative(palette = "Harmonic") 


foo <- subset(df, Meldewoche == as_date("2020-03-30") & ost)
r_ost <- cor(foo$z_afd, foo$cases7_per_100k)
foo <- subset(df, Meldewoche == as_date("2020-03-30") & !ost)
r_west <- cor(foo$z_afd, foo$cases7_per_100k)

p1 <- df %>% 
  filter(Meldewoche == as_date("2020-03-30")) %>% 
  ggplot(aes(z_afd, cases7_per_100k)) +
  geom_smooth(method = "lm", color = "grey50", fill = "grey90", size = 0.5) +
  geom_point(aes(col = ost), alpha = 0.5, show.legend = FALSE) +
  colorspace::scale_color_discrete_qualitative(palette = "Harmonic") +
  facet_wrap(vars(ost), labeller = labeller(ost = c("FALSE" = "West", "TRUE" = "Ost"))) +
  labs(title = "KW 16: Kein Zusammenhang",
       x = "Stimmenanteil AfD (%)", y = "7-Tage-Inzidenz pro 100.000 Einw.")
p1
ggsave("plots/corona_afd_ostwest_kw16.png", type = "cairo", dpi = 200, width = 6, height = 4)


# KW 49
foo <- subset(df, Meldewoche == as_date("2020-11-30") & ost)
r_ost <- cor(foo$z_afd, foo$cases7_per_100k)
foo <- subset(df, Meldewoche == as_date("2020-11-30") & !ost)
r_west <- cor(foo$z_afd, foo$cases7_per_100k)
r_ost 
r_west

# KW latest
foo <- subset(df, Meldewoche == latest_week & ost)
r_ost <- cor(foo$z_afd, foo$cases7_per_100k)
foo <- subset(df, Meldewoche == latest_week & !ost)
r_west <- cor(foo$z_afd, foo$cases7_per_100k)
r_ost 
r_west


p2 <- df %>% 
  filter(Meldewoche == latest_week) %>% 
  ggplot(aes(z_afd, cases7_per_100k)) +
  geom_smooth(method = "lm", color = "grey50", fill = "grey90", size = 0.5) +
  geom_point(aes(col = ost), alpha = 0.5, show.legend = FALSE) +
  colorspace::scale_color_discrete_qualitative(palette = "Harmonic") +
  facet_wrap(vars(ost), labeller = labeller(ost = c("FALSE" = "West", "TRUE" = "Ost"))) +
  labs(title = "KW 50: Vor allem im Osten: <br>Je höher der Stimmenanteil der AfD im Landkreis,<br>
       desto höher die Corona-Fallzahlen",
       x = "Stimmenanteil AfD (%)", y = "7-Tage-Inzidenz pro 100.000 Einw.")
p2
ggsave("plots/corona_afd_ostwest_kw-latest.png", type = "cairo", dpi = 200, width = 6, height = 4)

library(patchwork)

p1 / p2

ggsave("plots/corona_afd_ostwest_kombi.png", type = "cairo", dpi = 200, width = 6, height = 8, scale = 1.25)


p3 <- df %>% 
  filter(Meldewoche %in% c(as_date("2020-04-13"), latest_week)) %>% 
  ggplot(aes(z_afd, cases7_per_100k)) +
  geom_smooth(method = "lm", color = "grey50", fill = "grey90", size = 0.5) +
  geom_point(aes(col = ost), alpha = 0.5, show.legend = FALSE) +
  colorspace::scale_color_discrete_qualitative(palette = "Harmonic") +
  facet_grid(rows = vars(Meldewoche), cols = vars(ost), 
             labeller = labeller(ost = c("FALSE" = "West", "TRUE" = "Ost"), Meldewoche = c("2020-04-13" = "KW 16", "2020-11-30" = "KW 49"))) +
  labs(title = "KW 16 vs. KW 49: Je höher der Stimmenanteil der AfD im Landkreis,<br>
       desto höher die Corona-Fallzahlen",
       x = "Stimmenanteil AfD (%)", y = "7-Tage-Inzidenz pro 100.000 Einw.") 
p3

ggsave("plots/corona_afd_ostwest_kombi.png", type = "cairo", dpi = 200, width = 6, height = 8)


df %>% 
  filter(Meldewoche == latest_week) %>% 
  ggplot(aes(z_afd, cases7_per_100k)) +
  geom_smooth(method = "lm", color = "grey50", fill = "grey90", size = 0.5) +
  geom_point(aes(col = t_typ), alpha = 0.5) +
  colorspace::scale_color_discrete_qualitative(palette = "Harmonic") +
  facet_wrap(vars(ost), labeller = labeller(ost = c("FALSE" = "West", "TRUE" = "Ost"))) +
  labs(title = "KW 49: Je höher der Stimmenanteil der AfD im Landkreis,<br>
       desto höher die Corona-Fallzahlen",
       x = "Stimmenanteil AfD (%)", y = "7-Tage-Inzidenz pro 100.000 Einw.")
  


# Entwicklung der Pandemie anhand von 7-Tage-Inzidenzen
df %>% 
  mutate(Meldemonat = floor_date(Meldewoche, "1 month")) %>% 
  filter(Meldemonat >= as_date("2020-02-01")) %>% 
  select(Bundesland, Landkreis, Meldemonat, cases7_per_100k) %>% 
  group_by(Bundesland, Landkreis, Meldemonat) %>% 
  summarize(max_cases7_per_100k = max(cases7_per_100k), .groups = "drop") %>% 
  mutate(max_50plus = max_cases7_per_100k >= 50) %>% 
  group_by(Meldemonat) %>% 
  summarize(landkreise_50plus = sum(max_50plus),
            landkreise_50plus_rel = landkreise_50plus / n()) %>% 
  ggplot(aes(Meldemonat, landkreise_50plus_rel)) +
  geom_segment(aes(xend = Meldemonat, y = 0, yend = landkreise_50plus_rel), col = "grey20", size = 0.5) +
  geom_point(size = 2, shape = 21, fill = "grey20", col = "white") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "",
       subtitle = "Anteil Landkreise mit 7-Tage-Inzidenz 50 oder höher",
       caption = "Daten: RKI",
       x = NULL, y = NULL)


# Entwicklung der Pandemie anhand von 7-Tage-Inzidenzen differenziert in 3 Gruppen

colors_sequential <- sequential_hcl(4, palette = "Blues 3")
colors_qualitative <- qualitative_hcl(2, palette = "Harmonic")
plot_subtitle <- glue("Anteil Landkreise mit 7-Tage-Inzidenz von 
                       <b style='color:{colors_sequential[1]}'>30-50</b> 
                       und <b style='color:color:{colors_sequential[2]}'>50 oder höher</b>")

df %>% 
  mutate(Meldemonat = floor_date(Meldewoche, "1 month")) %>% 
  filter(Meldemonat >= as_date("2020-02-01")) %>% 
  select(Bundesland, Landkreis, Meldemonat, cases7_per_100k) %>% 
  group_by(Bundesland, Landkreis, Meldemonat) %>% 
  summarize(max_cases7_per_100k = max(cases7_per_100k), .groups = "drop") %>% 
  # filter(!is.na(max_inci)) %>% 
  group_by(Meldemonat) %>% 
  summarize(max_inci_50 = sum(max_cases7_per_100k >= 50),
            max_inci_30_50 = sum(max_cases7_per_100k >= 30 & max_cases7_per_100k < 50),
            max_inci_50 = max_inci_50 / n(),
            max_inci_30_50 = max_inci_30_50 / n()) %>%
  pivot_longer(cols = -Meldemonat, names_to = "incidence_grp", values_to = "share") %>% 
  mutate(incidence_grp = case_when(
    incidence_grp == "max_inci_50" ~ "50 oder höher",
    incidence_grp == "max_inci_30_50" ~ "30-50"
  )) %>% 
  ggplot(aes(fct_inorder(format(Meldemonat, "%b")), y = share, fill = fct_rev(incidence_grp))) +
  geom_col(position = "stack", width = 0.75, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = colors_sequential[c(2, 1)]) +
  labs(title = "Im Herbst sind fast alle Landkreise betroffen",
       subtitle = plot_subtitle,
       caption = "Daten: RKI",
       x = NULL, y = NULL)

ggsave("plots/Landkreise_mit_Inzidenz50.png", type = "cairo", dpi = 200, width = 6, height = 4)
# 
# 
# 
# Osten:
#   18 mit AfD > 25 % => 16 mit 7dI > 200, 2 < 200
# mit AfD < 25 % => 16 > 200, 42 < 200

foo <- tribble(
  ~afd, ~corona, ~n,
  ">25%", ">200", 16,
  ">25%", "<200", 2,
  "<25%", ">200", 16,
  "<25%", "<200", 42,
) %>% 
  pivot_wider(id_cols = afd, names_from = "corona", values_from = "n")

as.matrix(foo[, 2:3])
chisq.test(as.matrix(foo[, 2:3]))
