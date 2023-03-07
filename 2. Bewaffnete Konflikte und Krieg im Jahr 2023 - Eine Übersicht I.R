# Laden sämtlicher für die Datenanalyse relevanten R-Packete:
library(readr)
library(readxl)
library(maps)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(cowplot)

# Festlegung des Workingdirectorys (Damit der Code reproduziert werden kann, muessem in dem hier definierten Ordern saemtliche der hier verwendeten Datensaetze abgespeichert sein):
setwd("C:/Users/thoma/Documents/R/Data")

# Erstellung eines Datentabelle der Grenzen saemtlicher Länder der Welt mit map_data():
map <- map_data("world") %>%
  select(-subregion)

# Import des Datensatzes fuer bewaffnete Konflikte (online unter: https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-221-csv.zip) und Selektion der relevanten Variablen mit select():
ucdp_conflict <- read_csv("UCDP_PRIO_ArmedConflict_Annual_Data_v22.1.csv") %>%
  select(conflict_id,
         location,
         territory_name,
         side_a,
         side_b,
         incompatibility,
         year,
         intensity_level,
         type_of_conflict,
         start_date,
         start_date2,
         ep_end_date
         ) %>%
  rename("start_date1" = "start_date",
         "end_date" = "ep_end_date") %>%
  # Mit mutate werden bestehende Variablen ueberschrieben und neue erstellt:
  # Erstellung der Variable conflict_interval, welche den Zeitraum eines Konfliktes angibt:
  mutate(conflict_interval = start_date2 %--% end_date,
         #Erstellung der Variabel conflict_duration, welche die Dauer eines Konfliktes in Wochen angibt:
         conflict_duration = difftime(end_date,start_date2, units = "weeks"),
         # Bestehende diskrete Variabeln werden mit as.factor faktorisiert:
         incompatibility = as.factor(incompatibility),
         intensity_level = as.factor(intensity_level),
         type_of_conflict = as.factor(type_of_conflict),
         type_of_conflict = fct_recode(type_of_conflict,
                                       "extrastaatlich" = '1',
                                       "interstaatlich" = '2',
                                       "intrastaatlich" = '3',
                                       "internationalisiert-intrastaatlich" = '4'),
         incompatibility = fct_recode(incompatibility,
                                      "Kontrolle von Territorien" = '1',
                                      "Konrolle der Regierung" = '2',
                                      "Kontrolle von Territorien und der Regierung" = '3'
                                      )
         )

# Import des Datensatz fuer Todesopfer infolge von Kampfhandlungen (online unter: https://ucdp.uu.se/downloads/brd/ucdp-brd-conf-221-csv.zip):
ucdp_battledeaths <- read_csv("UCDP_BattleDeaths_Data_v22.1.csv") %>%
  select(conflict_id,
         year,
         bd_best)

# Vereinigung der beiden Datensaetze ucdp_conflit und ucdp_deaths mit right_join über die gemeinsame Konflikt-ID:
ucdp_conflict_battledeaths <- right_join(ucdp_conflict,ucdp_battledeaths, by = c("conflict_id","year"))

# Beschraenkung des Datensaetz auf die Faelle mit der aktuellsten Jahreszahl mit filter():
ucdp_conflict_2022 <- ucdp_conflict %>%
  filter(year == max(year)) %>%
  # Da es sich bei den hier ausgewahlten Konflikten um fortlaufende Konflikte handelt, haben sie kein Enddatum und daher auch keine Werte fuer diese und daraus brechneten Variabeln. Dementsprechend werden diese Variabeln mit select() deselektiert:
  select(-end_date,
         -conflict_interval,
         -conflict_duration) %>%
  # Die Jahresezahl wird mit mutate() auf 2022 uebrschrieben und aktualisiert:
  mutate(year = 2022)

# Da der Datensatz noch auf dem Stand von 2021 muss er teilweise munuell ueberschrieben und aktualisierter werden.
# Da sich die Angaben für den Konflikt in der Ukraine mittlerweile ueberholt sind (aus einem bewaffneten Konflikt bis 2021 ist in 2022 ein Krieg geworden), muessn die entsprechenden Werte in der entsprechenden Zeile 3 fuer die betroffenen Variabeln manuell ueberschrieben und aktualisiert werden:
ucdp_conflict_2022[3,6] <- as.factor("Kontrolle von Territorien und der Regierung")
ucdp_conflict_2022[3,8] <- as.factor(2)
ucdp_conflict_2022[3,9] <- as.factor("interstaatlich")

# Da es Faelle gibt, wo unter der Variable "location" anstelle von einer, zwei Regionen pro Fall gelistet werden, muessen die entsprechenden Zeilen 4 und 19 in der Datentabelle fuer die spaetere Verieinigung mit dem Datensatz map dupliziert und ihre Werte im naechsten Schritt manuell ueberschrieben werden:
# Die Zeilen 4, und 19 werden mit rbind() kopiert und der Datentabelle ein zweites Mal hinzugefuegt:
ucdp_conflict_2022 <- ucdp_conflict_2022 %>%
  rbind(ucdp_conflict_2022[4,]) %>%
  rbind(ucdp_conflict_2022[19,]) %>%
  # Alpahbetische Anordnung saemtlicher Faelle in der Datentabelle mit arrange() nach den Werten fuer die Variable "location":
  arrange(location) %>%
  # Manuelle Uebrschreibung der Werte fuer "location" mit mutate(), diese Werte sind nun kompitibel mit den Werten fuer die Variable "region" des Datensatz map und ermoeglichen so eine Vereinigung beider Datensaetze in einem naechsten Schritt:
  mutate(location = rep(c("Afghanistan","Azerbaijan","Burkina Faso","Burundi","Cameroon",
                          "Central African Republic","Chad","Colombia","Democratic of the Congo","Egypt",
                          "Ethiopia","India","Indonesia","Iran","Israel",
                          "Iraq","Israel","Kenya","Kyrgyzstan","Tajikistan",
                          "Mali","Mozambique","Myanmar","Niger","Nigeria",
                          "Pakistan","Philippines","Somalia","South Sudan","Sudan",
                          "Syria","Thailand","Turkey","Ukraine","Yemen"),
                        c(2,1,2,1,2,
                          1,2,1,2,1,
                          2,2,1,2,1,
                          1,1,1,1,1,
                          2,1,5,2,3,
                          3,2,2,1,1,
                          2,1,1,1,1)
                        )
         )

# Vereinigung der beiden Datensaetze  upcd_conflict und map über die Laendernamen der jeweiligen Konfliktregionen und Laendergrenzen mit left_join():
map_ucdp_conflict <- left_join(map,ucdp_conflict_2022, by = c("region" = "location")) %>%
  na.omit()

# Gestaltung des themes fuer die Karten von "War and Peace and Data Science":
theme_wpd_map <-  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
                        plot.subtitle = element_text(size = 20),
                        plot.caption = element_text(size = 10),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        axis.line = element_blank(),
                        legend.title = element_text(size = 15, face = "bold"),
                        legend.text = element_text(size = 15),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "black"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank()
                        )

# Visualisierung aller aktuellen bewaffneten Konflikte:  
map1 <- map %>%
  ggplot( aes(x = long, y = lat, group = group) ) +
  geom_polygon(color = "black", fill = "gray90") +
  geom_polygon(data = map_ucdp_conflict, aes(x = long, y = lat, group = group, fill = intensity_level), color = "black" ) +
  scale_fill_manual(name = "Anzahl der Todesopfer:",
                    labels = c("25 - 999 (Bewaffneter Konflikt)","> 1000 (Krieg)"),
                    values = c(brewer.pal(9,"Reds")[6],
                               brewer.pal(9,"Reds")[8]
                    )
  ) +
  labs(title = "Bewaffnete Konflikte Anfang 2023",
       caption = "Quelle: UCDP/PRIO Armed Conflict Dataset; persönliche Schätzung") +
  theme_wpd_map +
  theme(legend.position = "bottom")

ggdraw() +
  draw_plot(map1) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

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

# ... fuer Flächendiagramme:
theme_wpd3 <-  theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(t = 0, r = 0.5, b = 0, l = 0.5)),
                     plot.subtitle = element_text(size = 20, hjust = 0.5),
                     axis.title = element_blank(),
                     axis.ticks = element_line(color = "grey"),
                     axis.text = element_text(size = 15),
                     legend.title = element_text(size = 15, face = "bold"),
                     legend.text = element_text(size = 15),
                     legend.position = "bottom",
                     legend.background = element_rect(color = "black"),
                     panel.grid.major.x = element_blank(),
                     panel.grid.major.y = element_line(color = "grey"),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank()
)


# Visualisierung der Anzahl aktueller bewaffneter Konflikte je Konfliktregion nach Konfliktintensitaet:
plot1 <- ucdp_conflict_2022 %>%
  group_by(location) %>%
  mutate(conflicts_per_location = n()) %>%
  ungroup() %>%
  ggplot( aes(y = reorder(location,conflicts_per_location), fill = intensity_level) ) +
  geom_bar() +
  scale_y_discrete(labels = c("Aserbaidschan","Burundi","Zentralafrika","Kolumbien","Ägypten",
                              "Indonesien","Irak","Kenia","Kirgistan","Mosambik",
                              "Südsudan","Sudan","Tadschikistan","Thailand","Türkei",
                              "Ukraine","Jemen","Afghanistan","Burkina Faso","Kamerun",
                              "Tschad","DR Kongo","Äthiopien","Indien","Iran",
                              "Israel","Mali","Niger","Philippinen","Somalia",
                              "Syrien","Nigeria","Pakistan","Myanmar")
                   ) +
  labs(title = "Bewaffnete Konflikte Anfang 2023",
       subtitle = "Anzahl nach Konfliktregion und -intensität",
       caption = "Quelle: PRIO/UCDP Armed Conflict Dataset; persönliche Schätzung") +
  scale_fill_manual(name = "Anzahl der Todesopfer:",
                    labels = c("25 - 999 (Bewaffneter Konflikt)","> 1000 (Krieg)"),
                    values = c(brewer.pal(9,"Reds")[6],
                               brewer.pal(9,"Reds")[8])
  ) +
  theme_wpd2 +
  theme(axis.text.y = element_text(size = 10))

ggdraw() +
  draw_plot(plot1) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Visualisierung der Anzahl aktueller Bewaffneter Konflikte je Konflikttyp nach Konfliktrursache: 
plot2 <- ucdp_conflict_2022 %>%
  group_by(type_of_conflict) %>%
  mutate(conflicts_per_type = n()) %>%
ggplot( aes(x = reorder(type_of_conflict,desc(conflicts_per_type)), fill = incompatibility) ) +
  geom_bar(width = 0.15) +
  labs(title = "Bewaffnete Konflikte Anfang 2023",
       subtitle = "Anzahl nach Konfliktyp- und gegenstand",
       caption = "Quelle: PRIO/UCPD Armed Conflict Dataset; persönliche Schätzung") +
  scale_fill_manual(name = "Konfliktgegenstand:",
                    values = c(brewer.pal(9,"Reds")[4],
                               brewer.pal(9,"Reds")[6],
                               brewer.pal(9,"Reds")[8])
                    ) +
  theme_wpd1

ggdraw() +
  draw_plot(plot2) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Kalkulierung der durchschnittlichen Anzahl von Todesopfern eines bewaffneten Konfliktes
conflict_battledeaths_mean <- ucdp_conflict_battledeaths %>%
  group_by(conflict_id) %>%
  summarise(battledeaths_per_type_of_conflict = sum(bd_best)) %>%
  summarise(batlledeaths_per_conflict_mean = mean(battledeaths_per_type_of_conflict)) %>%
  pull()

# visualisierung der durchschnittlichen Anzahl von Todesopfern durch Kampfhandlungen nach Konflikttyp:
plot3 <- ucdp_conflict_battledeaths %>%
  group_by(type_of_conflict,conflict_id) %>%
  summarise(battledeaths_per_conflict = sum(bd_best)) %>%
  summarise(battledeaths_mean_per_type_of_conflict = mean(battledeaths_per_conflict)) %>%
  ungroup() %>%
  ggplot( aes(x = reorder(type_of_conflict,desc(battledeaths_mean_per_type_of_conflict)), y = battledeaths_mean_per_type_of_conflict)) +
  geom_bar(stat = "identity",
           width = 0.15,
           fill = brewer.pal(9,"Reds")[6]) +
  geom_hline(yintercept = conflict_battledeaths_mean,
             linetype = "dashed",
             color = "grey20") +
  geom_text(x = "intrastaatlich",
            y = 9250,
            label = "Durchschnitt für sämtliche Konflikte",
            color = "grey20",
            size = 6) +
  labs(title = "Todesopfer nach Konfliktart",
       subtitle = "durchschnittliche Anzahl (1989 - 2021)",
       caption = "Quelle: PRIO/UCPD Armed Conflict Dataset; UCDP Battle-related Deaths Dataset") +
  theme_wpd1 +
  theme(axis.text.x = element_text(face = "bold"))

ggdraw() +
  draw_plot(plot3) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Kalkulierung der durchschnittlichen Dauer eines bewaffneten Konfliktes
conflict_duration_mean <- ucdp_conflict %>%
  na.omit() %>%
  summarise(conflict_duration_mean = mean(conflict_duration)) %>%
  pull()

#Visualisierung der Dauer bewaffneter Konflikte nach Konflikttyp:
plot4 <- ucdp_conflict %>%
  na.omit() %>%
  group_by(type_of_conflict) %>%
  summarise(conflict_duration_mean = mean(conflict_duration)) %>%
  ggplot( aes(x = reorder(type_of_conflict,desc(conflict_duration_mean)), y = conflict_duration_mean) ) +
  geom_bar(stat = "identity",
           width = 0.2,
           fill = brewer.pal(9,"Reds")[6]) +
  geom_hline(yintercept = conflict_duration_mean,
             linetype = "dashed",
             color = "grey20"
  ) +
  geom_text(x = "internationalisiert intrastaatlich",
            y = 175,
            label = "Durchschnitt für sämtliche Konflikte",
            size = 6,
            color = "grey20") +
  labs(title = "Dauer bewaffneter Konflikte",
       subtitle = "durchschnittliche Dauer in Wochen (1946 - 2021)",
       caption = "Quelle: PRIO/UCPD Armed Conflict Dataset") +
  theme_wpd1 +
  theme(axis.text.x = element_text(face = "bold"))

ggdraw() +
  draw_plot(plot4) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)

# Brechnung eines T-Test bezüglich des statistischen Einflussses von Internationalisierung auf:

# ... die Anzahl von Todesopfern durch direkte Kampfhandlungen:
ucdp_conflict_t.test_battledeaths <- ucdp_conflict_battledeaths %>%
  select(conflict_id,type_of_conflict,bd_best) %>%
  filter(type_of_conflict %in% c("intrastaatlich","internationalisiert-intrastaatlich")) %>%
  group_by(type_of_conflict,conflict_id) %>%
  summarise(bd_best_total = sum(bd_best)) %>%
  na.omit()

t.test(ucdp_conflict_t.test_battledeaths$bd_best_total ~ ucdp_conflict_t.test_battledeaths$type_of_conflict, var.equal = TRUE, alternative = "less")

# ... die Dauer von Bürgerkriege:
ucdp_conflict_t.test_duration <- ucdp_conflict %>%
  select(conflict_id,type_of_conflict,conflict_duration) %>%
  filter(type_of_conflict %in% c("intrastaatlich","internationalisiert-intrastaatlich")) %>%
  group_by(type_of_conflict,conflict_id) %>%
  summarise(conflict_duration_total = sum(conflict_duration)) %>%
  na.omit()

t.test(ucdp_conflict_t.test_duration$conflict_duration_total ~ ucdp_conflict_t.test_duration$type_of_conflict, var.equal = TRUE, alternative = "less")


# Visualisierung der Anzahl bewaffneter Konflikte im Zeitverlauf nach Konfliktyp:
plot5 <- ucdp_conflict %>%
  group_by(year, type_of_conflict) %>%
  summarise(n = n()) %>%
  ggplot( aes(x = year, y = n, fill = type_of_conflict) ) +
  geom_area() +
  geom_vline(xintercept = c(1947,1991,2011),
             linetype = "dashed",
             color = "grey20") +
  geom_text(x = 1948.5,
            y = 30,
            size = 6,
            label = "Beginn Kalter Krieg",
            angle = 270,
            color = "grey20") +
  geom_text(x = 1992.5,
            y = 30,
            size = 6,
            label = "Ende Kalter Krieg",
            angle = 270,
            color = "grey20") +
  geom_text(x = 2012.5,
            y = 30,
            size = 6,
            label = "Beginn der Arabischen Aufstände",
            angle = 270,
            color = "grey20") +
  labs(title = "Bewaffnete Konflikte im Zeitverlauf",
       subtitle = "Anzahl nach Konfliktyp (1946 - 2021)",
       caption = "Quelle: PRIO/UCPD Armed Conflict Dataset") +
  lims(y = c(0,60)) +
  scale_fill_manual(name = "Art des Konflikts:",
                    values = c(brewer.pal(9,"Reds")[2],
                               brewer.pal(9,"Reds")[4],
                               brewer.pal(9,"Reds")[6],
                               brewer.pal(9,"Reds")[8])
  ) +
  theme_wpd1 +
  theme(legend.title = element_blank())

ggdraw() +
  draw_plot(plot5) +
  draw_image("C:/Users/thoma/Documents/R/WarPeaceDataScience/Logo/wpd_logo_was_klein.png",
             x = 0.455,
             y = 0.425,
             scale = 0.125)