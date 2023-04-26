################################################################################
#
# 1. Laden der fuer die Datenanalyse verwendeten Packete:
#
################################################################################

library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(cowplot)

################################################################################
#
# 2. Programmierung der fuer die Datenanalyse verwendeten Funktionen:
#
################################################################################

# Programmierung einer Funktion, die es ermöglicht die Summe der Werte für die letzten zehn Zeilen zu berechnen:
lag_sum <- function(x, before = 1) {
  sum <- 0
  while(before >= 1) {
    result <- case_when(
      is.na(lag(x,before)) ~ 0,
      TRUE  ~ lag(x,before)
    )
    sum <- sum + result
    before <- before - 1
  }
  return(sum)
}

################################################################################
#
# 3. Import, Bereinigung und Vereinigung der fuer die Datenanalyse verwendeten Datensaetze:
#
################################################################################

# Hinweis: Es kann sein, dass sie die hier verwendeten Datensaetze bezueglich Dateiname und Dateiformat vom Original abweichen, dies betrifft aber nicht den Inhalt der Datensaetze und somit auch nicht die Ergebnisse der Datenanalyse! 

# Festlegung des working directories:
setwd("C:/Users/thoma/Documents/R/Data")


################################################################################
#
# 3.1 Import, Bereinigung und Vereinigung der SIPRI-Datensaetze:
#
################################################################################
  
# Imports des Datensatz fuer Waffenexporte:
sipri_weapons_imports <- read_csv("SIPRI_WeaponsImports_Data_v1950-2022.csv", skip = 10) %>%
  select(-Total) %>%
  pivot_longer(cols = 2:74,
               names_to = "year",
               values_to = "imports") %>%
  mutate(year = as.numeric(year),
         imports = replace_na(imports,0)
         ) %>%
  rename(state_name = "...1") %>%
  filter(state_name != "Total")

# Imports des Datensatz fuer Waffenimporte:
sipri_weapons_exports <- read_csv("SIPRI_WeaponsExports_Data_v1950-2022.csv", skip = 10) %>%
  select(-Total) %>%
  pivot_longer(cols = 2:74,
               names_to = "year",
               values_to = "exports") %>%
  mutate(year = as.numeric(year),
         exports = replace_na(exports,0)
         ) %>%
  rename(state_name = "...1") %>%
  filter(state_name != "Total")

# Vereinigung des Datensatz fuer Waffenimport mit dem Datensatz fuer Waffenexporte:
sipri_weapons_flow <- sipri_weapons_exports %>%
  left_join(sipri_weapons_imports,
            by = c("state_name",
                   "year")
            ) %>%
  mutate(state_name = case_when(state_name == "Cote d'Ivoire" ~ "Ivory Coast",
                                state_name == "Turkye" ~ "Turkey",
                                state_name == "Viet Nam" ~ "Vietnam",
                                TRUE ~ state_name),
         imports = replace_na(imports,0),
         ) %>%
  arrange(state_name,
          year)

rm(sipri_weapons_exports,
   sipri_weapons_imports)

################################################################################
#
# 3.2 Import, Bereinigung und Vereinigung der UCDP-Datensaetze:
#
################################################################################

# Import des Datensatz fuer bewaffnete Konflikte:
ucdp_conflict_dyadic <- read_xlsx("UCDP_PRIO_ArmedConflict_DyadYear_Data_v22.1.xlsx") %>%
  select(year,
         conflict_id,
         dyad_id,
         side_a_id,
         side_a_name = side_a,
         side_a_2nd,
         side_b_id,
         side_b_name = side_b,
         side_b_2nd,
         intensity_level,
         type_of_conflict,
         start_date = start_date2) %>%
  filter(type_of_conflict == 2) %>%
  select(-type_of_conflict) %>%
  mutate(side_a_id = str_split(side_a_id,", "),
         side_a_name = str_split(side_a_name,", "),
         side_a_2nd = str_split(side_a_2nd,", "),
         side_b_id = str_split(side_b_id,", "),
         side_b_name = str_split(side_b_name,", "),
         side_b_2nd = str_split(side_b_2nd,", ")
         ) %>%
  unnest_longer(c(side_a_id,side_a_name)) %>%
  unnest_longer(c(side_b_id,side_b_name)) %>%
  mutate(side_a_id = as.numeric(side_a_id),
         side_b_id = as.numeric(side_b_id),
         start_date = ymd(start_date),
         start_year = year(start_date)
         ) %>%
  relocate(year,
           .before = dyad_id) %>%
  arrange(year,
          conflict_id,
          dyad_id)

# Import des Datensatz fuer Ausgaenge bewaffneter Konflikte:
ucdp_termination_dyadic <- read_csv2("UCDP_ConflictTermination_Dyadic_Data_v3.csv") %>%
  select(year,
         conflict_id,
         dyad_id,
         dyadep_id,
         dyadep = dyadepisode,
         start_date = start_date2,
         end_date = ependdate,
         outcome) %>%
  mutate(start_date = dmy(start_date),
         start_year = year(start_date),
         end_date = dmy(end_date),
         end_year = year(end_date)
         ) %>%
  arrange(year,
          conflict_id,
          dyad_id,
          dyadep_id)

# Vereinigung der Datensatze fuer Ausgaenge bewaffneter Konflikte mit dem Datensatz fuer bewaffnete Konflikte:
ucdp_conflict_dyadic <- ucdp_conflict_dyadic %>%
  left_join(ucdp_termination_dyadic,
            by = c("year",
                   "conflict_id",
                   "dyad_id",
                   "start_year",
                   "start_date")
            ) %>%
  mutate(conflict_duration = difftime(end_date,
                                      start_date,
                                      units = "weeks")
         ) %>%
  relocate(c(dyadep_id,dyadep),
           .after = dyad_id) %>%
  relocate(c(conflict_duration,
             outcome),
           .after = end_year)

rm(ucdp_termination_dyadic)

# Import des Datensatz fuer die Anzahl von Todesopfern bewaffneter Konflikte:
ucdp_battledeaths <- read_csv("C:/Users/thoma/Documents/R/Data/UCDP_BattleDeaths_Dyadic_Data_v22.1.csv") %>%
  select(year,
         conflict_id,
         dyad_id,
         bd_best) %>%
  mutate(bd_best_tsd = bd_best / 1000)

# Vereinigung des Datensatz fuer die Anzahl von Todesopfern bewaffneter Konflikte mit dem Datensatz fuer bewaffnete Konflikte:
ucdp_conflict_dyadic <- ucdp_conflict_dyadic %>%
  left_join(ucdp_battledeaths,
            by = c("year",
                   "dyad_id",
                   "conflict_id")
            )

rm(ucdp_battledeaths)

# Import des Datensatz fuer externe Unterstuetzung:
ucdp_support_dyadic <- read_xlsx("UCDP_ExternalSupport_SupporterActorDyadYear_Data_v18.1.xlsx") %>%
  select(year,
         dyad_id,
         conflict_id,
         type_of_conflict = civil,
         actor_id,
         actor_name,
         actor_type_of_org = actor_nonstate,
         ext_id,
         ext_name,
         ext_type_of_org = ext_nonstate,
         ext_coalition,
         ext_coalition_name,
         ext_bothsides,
         ext_sup,
         ext_x,
         ext_p,
         ext_y,
         ext_w,
         ext_m,
         ext_t,
         ext_f,
         ext_i,
         ext_l) %>%
  filter(type_of_conflict == 0 & actor_type_of_org == 0 & ext_type_of_org == 0) %>%
  select(-type_of_conflict,
         -actor_type_of_org,
         -ext_type_of_org) %>%
  mutate(actor_name = str_split(actor_name, ", ")
  ) %>%
  unnest_longer(actor_name) %>%
  arrange(year,
          conflict_id,
          dyad_id,
          ext_id)

# Vereinigung des Datensatz fuer externe Unterstuetzung mit dem Datensatz fuer bewaffnete Konflikte:
ucdp_conflict_support_dyadic <- ucdp_conflict_dyadic %>%
  left_join(ucdp_support_dyadic,
            by = c("year",
                   "conflict_id",
                   "dyad_id")
            ) %>%
  filter(year >= 1975) %>%
  mutate(ext_sup = case_when(is.na(ext_id) ~ 0,
                                 TRUE ~ ext_sup),
         ext_side = case_when(side_a_name == actor_name ~ "a",
                              side_b_name == actor_name ~ "b"),
         ext_side_2nd = case_when(c(ext_side == "a" & ext_name %in% side_a_2nd) | c(ext_side == "b" & ext_name %in% side_b_2nd) ~ 1,
                                    TRUE ~ 0),
         across(ext_x:ext_l, ~ replace_na(.,0)),
         ext_sum = ext_p + ext_y + ext_w + ext_m + ext_t + ext_f + ext_i + ext_l) %>%
  select(-actor_id,
         -actor_name) %>%
  relocate(c(ext_sup,ext_side,ext_side_2nd), .after = bd_best) %>%
  arrange(year,
          conflict_id,
          dyad_id,
          dyadep_id,
          ext_id)

rm(ucdp_support_dyadic)


# Erstellung eines Datensatz der angibt, wie oft ein Unterstützer in einem bewaffneten Konflikt mit Truppen unterstützt hat:
ext_x_before <- ucdp_conflict_support_dyadic %>%
  group_by(dyadep_id,
           ext_id) %>%
  summarise(ext_x_before = lag_sum(ext_x, n()) ) %>%
  mutate(ext_x_before = case_when(ext_x_before >= 1 ~ 1,
                                  TRUE ~ 0)
         )

# Vereinigung des Datensatz der angibt, ob ein Unterstützer in einem bewaffneten Konflikt mit Truppen unterstützt hat:
ucdp_conflict_support_dyadic <- ucdp_conflict_support_dyadic %>%
  left_join(ext_x_before,
            by = c("dyadep_id",
                   "ext_id")
            ) %>%
  relocate(ext_x_before, .after = ext_x)

rm(ext_x_before)

################################################################################
#
# 3.3 Vereinigung der SIPRI- und UCDP-Datensaetze:
#
################################################################################

# Import des Datensatz fuer saemtliche Namen und Ids fuer samentliche Staaten der UCDP-Datensaetze:
ucdp_actors <- read_excel("UCDP_Actor_Data_v22.1.xlsx") %>%
  select(id = ActorId,
         name = NameData,
         type_of_org = Org) %>%
  filter(type_of_org == 4) %>%
  select(-type_of_org) %>%
  mutate(state_name = str_extract(name,
                                  "(?<=(of\\s))[a-zA-Z\\s]+")
  )

# Vereinigung des Datensatz fuer Waffenhandel mit dem Datensatz fuer saemtliche Namen und Ids fuer Sametliche Staaten der UCDP-Datensaetze:
sipri_weapons_flow_ucdp_actors <- sipri_weapons_flow %>%
  left_join(ucdp_actors,
            by = "state_name") %>%
  relocate(c(id,name,state_name,exports,imports),
           .after = year)

rm(ucdp_actors)

# Umformatierung des Datensatz fuer bewaffneten Konflikten von einem wide- in ein long-Format:
ucdp_conflict_dyadic_longer <- ucdp_conflict_dyadic %>%
  select(year,
         dyadep_id,
         side_a_id,
         side_a_name,
         side_b_id,
         side_b_name,
         intensity_level,
         conflict_duration,
         outcome,
         bd_best,
         bd_best_tsd) %>%
  pivot_longer(cols = c(side_a_id,side_b_id),
               names_to = "side_id",
               values_to = "id") %>%
  pivot_longer(cols = c(side_a_name,side_b_name),
               names_to = "side_name",
               values_to = "name") %>%
  mutate(side = case_when(side_id == "side_a_id" & side_name == "side_a_name" ~ "a",
                          side_id == "side_b_id" & side_name == "side_b_name" ~ "b")
         ) %>%
  filter(side != is.na(side)) %>%
  select(-side_id,
         -side_name)

# Vereinigung des Datensatz fuer bewaffnete Konflikte mit dem Datensatz fuer Waffenlieferungen:
sipri_weapons_flow_ucdp_conflicts <- sipri_weapons_flow_ucdp_actors %>%
  left_join(ucdp_conflict_dyadic_longer,
            by = c("year",
                   "id",
                   "name")
            ) %>%
  mutate(conflict = case_when(is.na(dyadep_id) ~ 0,
                              TRUE ~ 1),
         conflict_level = case_when(is.na(intensity_level) ~ 0,
                                    TRUE ~ intensity_level)
         ) %>%
  relocate(c(conflict,conflict_level,dyadep_id,side,intensity_level,outcome,conflict_duration,bd_best,bd_best_tsd),
           .after = state_name)

rm(sipri_weapons_flow_ucdp_actors,
   ucdp_conflict_dyadic_longer)

################################################################################
#
# 4. Erstellung von Themes fuer die Datenvisualisierung:
#
################################################################################

# Gestaltung des themes fuer die Datenvisualisierungen von "War and Peace and Data Science"

# ... fuer Saeulendiagramme:
theme_wpd1 <-  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(t = 0, r = 0.5, b = 0, l = 0.5)),
                     plot.subtitle = element_text(size = 20, hjust = 0.5),
                     plot.caption = element_text(size = 10),
                     axis.title = element_blank(),
                     axis.ticks = element_line(color = "grey"),
                     axis.text.x = element_text(size = 15),
                     axis.text.y = element_text(size = 15, face = "bold"),
                     legend.title = element_text(size = 15, face = "bold"),
                     legend.text = element_text(size = 15),
                     legend.position = "bottom",
                     legend.background = element_rect(color = "black"),
                     panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_line(color = "grey"),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()
)

# ... fuer Balkendiagramme:
theme_wpd2 <-  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                     plot.subtitle = element_text(size = 20, hjust = 0.5),
                     plot.caption = element_text(size = 10),
                     axis.title = element_blank(),
                     axis.ticks.x = element_line(color = "grey"),
                     axis.ticks.y = element_blank(),
                     axis.text.x = element_text(size = 15),
                     axis.text.y = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(r = -30)),
                     legend.title = element_text(size = 15, face = "bold"),
                     legend.text = element_text(size = 15),
                     legend.position = "bottom",
                     legend.background = element_rect(color = "black"),
                     panel.grid.major.x = element_line(color = "grey"),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()
)

#... fuer Punktdiagramme:
theme_wpd3 <-  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                     plot.subtitle = element_text(size = 20, hjust = 0.5),
                     plot.caption = element_text(size = 10),
                     axis.title = element_text(size = 15, face = "bold"),
                     axis.ticks = element_line(color = "grey"),
                     axis.text = element_text(size = 15, face = "bold"),
                     legend.title = element_text(size = 15, face = "bold"),
                     legend.text = element_text(size = 15),
                     legend.position = "bottom",
                     legend.background = element_rect(color = "black"),
                     panel.grid.major.x = element_line(color = "grey"),
                     panel.grid.major.y = element_line(color = "grey"),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()
)

################################################################################
#
# 5. Datenvisualisierung:
#
################################################################################

# Visualisierung der fuenf groessten Waffenxporteure 2022:
plot1 <- sipri_weapons_flow %>%
  filter(year == 2022) %>%
  slice_max(exports, n = 10) %>%
  ggplot( aes(x = exports, y = reorder(state_name,exports) ) ) +
  geom_bar(stat = "identity",
           fill = brewer.pal(9,"Reds")[6]) +
  scale_y_discrete(labels = c("Polen","Israel","Spanien","Vereinigtes Königreich","Deutschland",
                              "Italien","China","Russland","Frankreich","Vereinigte Staaten")
                   ) +
  labs(title = "Die zehn größten Waffenexporteure 2022",
       subtitle = "Produktionskosten exportierter Waffen in Mio. US-Dollar",
       caption = "Quelle: SIPRI") +
  theme_wpd2

ggdraw() +
  draw_plot(plot1) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = -0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der fuenf groessten Waffenimporteure 2022:
plot2 <- sipri_weapons_flow %>%
  filter(year == 2022) %>%
  slice_max(imports, n = 10) %>%
  ggplot( aes(x = imports, y = reorder(state_name,imports) ) ) +
  geom_bar(stat = "identity",
           fill = brewer.pal(9,"Reds")[6]) +
  scale_y_discrete(labels = c("Israel","Vereinigte Staaten","Norwegen","Japan","Pakistan",
                              "Kuwait","Saudi Arabien","Ukraine","Indien","Katar")
                   ) +
  labs(title = "Die zehn größten Waffenimporteure 2022",
       subtitle = "Produktionskosten importierter Waffen in Mio. US-Dollar 2022",
       caption = "Quelle: SIPRI") +
  theme_wpd2

ggdraw() +
  draw_plot(plot2) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = -0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der Waffenexporte der fuenf groessten Waffenexporteure 2022 im Zeitverlauf:
plot3 <- sipri_weapons_flow %>%
  filter(year >= 2012,
         state_name %in% c("United States",
                           "France",
                           "Russia",
                           "China",
                           "Italy")
         ) %>%
  ggplot( aes(x = year, y = exports, fill = as.factor(state_name) ) ) +
  geom_area() +
  scale_x_continuous(breaks = seq(2012,2022,2)) +
  scale_fill_manual(name = "Exporteur:",
                    labels = c("China","Frankreich","Italien","Russland","Vereinigte Staaten"),
                    values = c(brewer.pal(5,"YlOrRd"))
                     ) +
  labs(title = "Die fünf groeßten Waffenexporteure 2022",
       subtitle = "Produktionskosten exportierter Waffen in Mio. US-Dollar 2018-2023",
       y = "Importe in Mio US-Dollar",
       captio = "Quelle: SIPRI") +
  theme_wpd1 +
  theme(legend.position = "right")

ggdraw() +
  draw_plot(plot3) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der Waffenimporte der fuenf groessten Waffenimporteure 2022 im Zeitverlauf:
plot4 <- sipri_weapons_flow %>%
  filter(year >= 2012,
         state_name %in% c("Qatar",
                           "India",
                           "Ukraine",
                           "Saudi Arabia",
                           "Kuwait")
         ) %>%
  ggplot( aes(x = year, y = imports, fill = as.factor(state_name) ) ) +
  geom_area() +
  scale_x_continuous(breaks = seq(2012,2022,2)) +
  scale_fill_manual(name = "Importeur:",
                    labels = c("Indien","Kuwait","Katar","Saudi-Arabien","Ukraine"),
                    values = brewer.pal(5,"YlOrRd")
  ) +
  labs(title = "Die fünf groeßten Waffenimporteure 2022",
       subtitle = "Produktionskosten importierter Waffen in Mio. US-Dollar 2018-2023",
       y = "Importe in Mio US-Dollar",
       captio = "Quelle: SIPRI") +
  theme_wpd1 +
  theme(legend.position = "right")

ggdraw() +
  draw_plot(plot4) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Kalkulierung der durchschnittlichen Waffenexporte pro Jahr:
weapons_exports_mean <- sipri_weapons_flow_ucdp_conflicts %>%
  group_by(state_name,year) %>%
  summarise(exports_mean = mean(exports)) %>%
  pull() %>% mean()

# Visualisierung der durchschnittlichen Waffenexporte kriegführender und nichtkriegführenden Staaten:
plot5 <- sipri_weapons_flow_ucdp_conflicts %>%
  group_by(conflict_level) %>%
  summarise(exports_mean = mean(exports)) %>%
  ggplot( aes( x = as.factor(conflict_level), y = exports_mean) ) +
  geom_bar(stat = "identity",
           width = 0.25,
           fill = brewer.pal(9,"Reds")[6]) +
  geom_hline(yintercept = weapons_exports_mean,
             linetype = "dashed",
             color = "grey20") +
  geom_text(x = 3,
            y = 220,
            label = "Durchschnitt für sämtliche Staaten",
            color = "grey20",
            size = 6) +
  scale_x_discrete(labels = c("kein Konflikt",
                              "interstaatlicher bewaffneter Konflikt",
                              "interstaatlicher Krieg")
  ) +
  lims(y = c(0,300)) +
  labs(title = "Waffenexporte im Krieg und Frieden",
       subtitle = "Durchschnittlich pro Jahr exportierte Waffen (1950-2022)",
       y = "Waffenexporte in Mio. US Dollar",
       caption = "Quelle: SIPRI; PRIO/UCDP Armed Conflict Dataset") +
  theme_wpd1 +
  theme(axis.title.y = element_text(size = 15, face = "bold"))

ggdraw() +
  draw_plot(plot5) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Kalkulierung der durchschnittlichen Waffenimporte pro Jahr:
weapons_imports_mean <- sipri_weapons_flow_ucdp_conflicts %>%
  group_by(state_name,year) %>%
  summarise(imports_mean = mean(imports)) %>%
  pull() %>% mean()

# Visualisierung der durchschnittlichen Waffenimporte kriegführender und nichtkriegführenden Staaten:
plot6 <- sipri_weapons_flow_ucdp_conflicts %>%
  group_by(conflict_level) %>%
  summarise(imports_mean = mean(imports)) %>%
  ggplot( aes( x = as.factor(conflict_level), y = imports_mean) ) +
  geom_bar(stat = "identity",
           width = 0.25,
           fill = brewer.pal(9,"Reds")[6]) +
  geom_hline(yintercept = weapons_imports_mean,
             linetype = "dashed",
             color = "grey20") +
  geom_text(x = 1,
            y = 280,
            label = "Durchschnitt für sämtliche Staaten",
            color = "grey20",
            size = 6) +
  scale_x_discrete(labels = c("bei keinem Konflikt",
                              "bei interstaatlichen bewaffneten Konflikt",
                              "bei interstaatlichen Krieg")
                   ) +
  labs(title = "Waffenimporte im Krieg und Frieden",
       subtitle = "Durchschnittlich pro Jahr importierte Waffen (1950-2021)",
       y = "Waffenimporte in Mio. US. Dollar",
       caption = "Quelle: SIPRI; PRIO/UCDP Armed Conflict Dataset") +
  theme_wpd1 +
  theme(axis.title.y = element_text(size = 15, color = "grey20", face = "bold"))

ggdraw() +
  draw_plot(plot6) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der jährlichen Waffenimporte krieg- und nichtkriegführender Staaten im Zeitverlauf:
plot7 <- sipri_weapons_flow_ucdp_conflicts %>%
  group_by(year,conflict_level) %>%
  summarise(imports_sum = sum(imports)) %>%
  ggplot( aes( x = year, y = imports_sum, fill = as.factor(conflict_level)) ) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Konfliktstatus:",
                    labels = c("kein Konflikt",
                               "interstaatlicher bewaffneter Konflikt",
                               "interstaatlicher Krieg"),
                    values = c(brewer.pal(9,"Reds")[2],
                               brewer.pal(9,"Reds")[4],
                               brewer.pal(9,"Reds")[6])
  ) +
  labs(title = "Waffenimporte in Krieg und Frieden",
       subtitle = "Anteil Waffenimporte krieg- und nichtkriegführender Staaten",
       y = "Waffenimporte in Mio. US-Dollar",
       caption = "Quelle: SIPRI; PRIO/UCDP Armed Conflict Dataset") +
  theme_wpd1

ggdraw() +
  draw_plot(plot7) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von Waffenlieferungen auf die Dauer von bewaffneten Konflikten:
plot8 <- sipri_weapons_flow_ucdp_conflicts %>%
  filter(conflict >= 1 & !is.na(imports) & !is.na(conflict_duration)) %>%
  group_by(dyadep_id) %>%
  summarise(imports_sum = sum(imports),
            conflict_duration = unique(conflict_duration)
            ) %>%
  ggplot( aes(x = imports_sum, y = conflict_duration) ) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = brewer.pal(9,"Reds")[6]) +
  scale_x_continuous(name = "Waffenimporte in Mio. US-Dollar") +
  scale_y_continuous(name = "Konfliktdauer in Wochen") +
  labs(title = "Waffeinimporte und Konfliktdauer*",
       subtitle = "linearer Zusammenhang (1975-2021)",
       caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
       Quelle: SIPRI; UCDP Conflict Termination Dataset") +
  theme_wpd3

ggdraw() +
  draw_plot(plot8) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von Waffenlieferungen auf die Anzahl von Todesopfern bewaffneter Konflikte:
plot9 <- sipri_weapons_flow_ucdp_conflicts %>%
  filter(conflict >= 1 & !is.na(imports) & !is.na(bd_best_tsd)) %>%
  group_by(dyadep_id) %>%
  summarise(imports_sum = sum(imports),
            bd_best_tsd_sum = sum(bd_best_tsd)
  ) %>%
  ggplot( aes(x = imports_sum, y = bd_best_tsd_sum) ) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = brewer.pal(9,"Reds")[6]) +
  scale_x_continuous(name = "Waffenimporte während des Konflikts in Mio. US-Dollar") +
  scale_y_continuous(name = "Konfliktote in Tsd.") +
  labs(title = "Waffeinimporte und Konflikttote*",
       subtitle = "linearer Zusammenhang (1989-2021)",
       caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
       SIPRI; UCDP Battle-realated Deaths Dataset") +
  theme_wpd3

ggdraw() +
  draw_plot(plot9) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von Waffenlieferungen auf Ausgaenge von bewaffneten Konflikten:
plot10 <- sipri_weapons_flow_ucdp_conflicts %>%
  filter(!is.na(outcome)) %>%
  group_by(side,outcome,dyadep_id) %>%
  summarise(imports_mean = mean(imports)) %>%
  ungroup() %>%
  ggplot( aes(x = as.factor(outcome), y = imports_mean, fill = as.factor(side) ) ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  scale_x_discrete(labels = c("Friedensabkommen",
                              "Waffenstillstand",
                              "Sieg von Staat A",
                              "Sieg von Staat B",
                              "Geringe Aktivität")
                   ) +
  scale_fill_manual(name = "Waffenimporte von:",
                      labels = c("Staat A","Staat B"),
                      values = c(brewer.pal(9,"BuGn")[6],
                                 brewer.pal(9,"BuPu")[6])
                      ) +
  labs(title = "Waffenimporte und Konfliktausgänge*",
       subtitle = "Durchschnitt jährlich importierter Waffen** (1950-2021)",
       y = "Waffenimporte in Mio. US-Dollar",
       caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
       **für die Dauer des Konflikts
       Quelle: SIPRI; PRIO/UCDP Armed Conflict Dataset; UCDP Conflict Termination Dataset") +
  theme_wpd1 +
  theme(axis.title.y = element_text(size = 15, face = "bold"))

ggdraw() +
  draw_plot(plot10) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von externer Unterstützung auf Dauer bewaffneter Konflikte:
plot11 <- ucdp_conflict_support_dyadic %>%
  filter(!is.na(ext_sup) & !is.na(conflict_duration)) %>%
  group_by(dyadep_id) %>%
  summarise(ext_sum_conflict = sum(ext_sum),
            conflict_duration = unique(conflict_duration)
            ) %>%
  ggplot( aes(x = ext_sum_conflict, y = conflict_duration)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = brewer.pal(9,"Reds")[6]) +
  scale_x_continuous(name = "Anzahl externer Unterstützungmaßnahmen") +
  scale_y_continuous(name = "Dauer in Wochen") +
  labs(title = "Externe Unterstützung und Konfliktdauer*",
       subtitle = "linearer Zusammenhang (1975-2021)",
       caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
       Quelle: UCDP Conflict Termination Dataset; UCDP External Support Dataset") +
  theme_wpd3
  
ggdraw() +
  draw_plot(plot11) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von externert Unterstützung auf die Anzahl der Todesopfer bewaffneter Konflikte:
plot12 <- ucdp_conflict_support_dyadic %>%
  filter(!is.na(ext_sup) & !is.na(bd_best)) %>%
  group_by(dyadep_id) %>%
  summarise(ext_sum_conflict = sum(ext_sum),
            bd_best_tsd_sum = sum(bd_best_tsd)
  ) %>%
  ggplot( aes(x = ext_sum_conflict, y = bd_best_tsd_sum)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = brewer.pal(9,"Reds")[6]) +
  scale_x_continuous(name = "Anzahl externer Unterstützungmaßnahmen") +
  scale_y_continuous(name = "Todesopfer in Tsd.") +
labs(title = "Externe Unterstützung und Konfliktote*",
     subtitle = "linearer Zusammenhang (1989-2021)",
     caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
     Quelle: UCDP Conflict Termination Dataset; UCDP External Support Dataset") +
  theme_wpd3

ggdraw() +
  draw_plot(plot12) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung des Einfluss von externer Unterstützung auf Ausgaenge bewaffneter Konflikte:
plot13 <- ucdp_conflict_support_dyadic %>%
  filter(!is.na(outcome) & !is.na(ext_sup) & !is.na(ext_side)) %>%
  group_by(ext_side,outcome,dyadep_id) %>%
  summarise(ext_sum_conflict = sum(ext_sum)) %>%
  summarise(ext_sum_conflict_mean = mean(ext_sum_conflict)) %>%
  ggplot( aes(x = as.factor(outcome), y = ext_sum_conflict_mean, fill = ext_side) ) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.5) +
  scale_x_discrete(labels = c("Friedensabkommen",
                              "Waffenstillstand",
                              "Sieg von Staat A",
                              "Sieg von Staat B",
                              "Geringe Aktivität")
  ) +
  scale_fill_manual(name = "Unterstützung für:",
                    labels = c("Staat A","Staat B"),
                    values = c(brewer.pal(9,"BuGn")[6],
                               brewer.pal(9,"BuPu")[6])
  ) +
  labs(title = "Externe Unterstützung und Konfliktausgänge*",
       subtitle = "Durchschnittliche Anzahl Unterstützungsmaßnahmen (1975-2021)",
       caption = "*berücksichtig wurden interstaatliche bewaffnete Konflikte
       Quelle: UCDP Conflict Termination Dataset; UCDP External Support Dataset") +
  theme_wpd1

ggdraw() +
  draw_plot(plot13) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der Anzahl von Staaten die eine Konfliktpartei uterstützen und selber Kriegspartei sind/wurden:
plot14 <- ucdp_conflict_support_dyadic %>%
  filter(!is.na(ext_sup) & !is.na(ext_side_2nd) & ext_sup == 1 & ext_x_before == 0) %>%
  group_by(ext_side_2nd,
           dyadep_id) %>%
  summarise(ext_n = length(unique(ext_id)) ) %>%
  summarise(ext_n = sum(ext_n)) %>%
  ggplot( aes(x = as.factor(ext_side_2nd), y = ext_n, fill = as.factor(ext_side_2nd) ) ) +
  geom_bar(stat = "identity",
           width = 0.25) +
  geom_text(x = as.factor(0),
            y = 142,
            label = "137",
            color = "grey20",
            size = 5) +
  geom_text(x = as.factor(1),
            y = 11,
            label = "6",
            color = "grey20",
            size = 5) +
  scale_x_discrete(labels = c("Unterstützer war und wurde keine Konfliktpartei",
                             "Unterstützer war oder wurde Konfliktpartei")
                  ) +
  scale_fill_manual(name = "Der Unterstützer war und/oder wurde",
                    labels = c("... keine Konfliktpartei",
                               "... Konfliktpartei"),
                    values = c(brewer.pal(9,"BuGn")[6],
                               brewer.pal(9,"BuPu")[6])
  ) +
  lims(y = c(0,150)) +
  labs(title = "Externe Unterstützung und Konfliktbeteiligung*",
       subtitle = "Anzahl externer Unterstützer in bewaffneten Konflikten (1975-2021)",
       caption = "*berücksichtigt werden interstaatliche bewaffnete Konflikte
       Quelle: UCDP External Support Dataset") +

  theme_wpd1 +
  theme(legend.position = "none")
  
ggdraw() +
  draw_plot(plot14) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)
  