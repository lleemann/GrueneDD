#######################################################
# Die Grünen und die direkte Demokratie
# Lucas Leemann und Angela Odermatt, UZH
# Februar 2022
#######################################################

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(cowplot)


setwd("") #anpassen

load("Grüne_Abstimmungen.RData")


###################
#Grafik 1: Aktive Verwendung Volksrechte nach Typ und Thema
###################

green_votes <- green_votes %>%
  mutate(jaStimmenInProzent = ifelse(rechtsform == 2, 100-`volkja-proz`, `volkja-proz`)) %>%
  filter(is.na(initiiert) ==  F)


green_votes$umwelt <-  recode(green_votes$umwelt, `1` = "Umweltthema", `0` = "anderes Thema")
green_votes$rechtsform <-  recode(green_votes$rechtsform, `2` = "fakultatives Referendum", `3` = "Volksinitiative")
green_votes$initiiert <- recode(green_votes$initiiert, `1` = "initiiert", `2` = "mitgetragen")


ggplot() + 
  geom_point(data = green_votes, 
             aes(y = jaStimmenInProzent, x = year, fill = rechtsform, colour = initiiert, shape = umwelt), 
             size = 4) +  
  geom_step(data = green_votes, 
            aes(y = `w-gps`, x = year), 
            color="grey") +
  theme_minimal() +
  ylab("Zustimmung zu Position der GPS") + xlab("Abstimmungsdatum") + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size=15)) +
  geom_hline(yintercept = 50, 
             linetype = "dashed", 
             color = "lightgrey") +
  scale_shape_manual(values = c(21, 22)) +
  scale_colour_manual(values = c("black", "white")) +
  guides(fill=guide_legend(title="Rechtsform", 
         override.aes = list(shape = 21, colour = NA)),
         shape=guide_legend(title="Umweltthema"),
         colour="none")


###################
#Grafik 2: (Mit-)Lancierung von Urnenvorlagen nach Inhalt
###################

green_votes_perioden <- green_votes %>%
  add_count(perioden, sozialpolitik == 1, name = "Sozialpolitik") %>%
  mutate(Sozialpolitik = ifelse(`sozialpolitik == 1` == FALSE, 0, Sozialpolitik)) %>%
  add_count(perioden, wirtschaft == 1, name = "Wirtschaft") %>%
  mutate(Wirtschaft = ifelse(`wirtschaft == 1` == FALSE, 0, Wirtschaft)) %>%
  add_count(perioden, `1st.dim` == 1, name = "1st.dim_year") %>%
  mutate(`1st.dim_year` = ifelse(.[,35] == FALSE, 0, `1st.dim_year`)) %>%
  add_count(perioden, `2nd.dim` == 1, name = "2nd.dim_year") %>%
  mutate(`2nd.dim_year` = ifelse(.[,37] == FALSE, 0, `2nd.dim_year`)) %>%
  add_count(perioden, staatsordnung == 1, name = "Staatsordnung") %>%
  mutate(Staatsordnung = ifelse(`staatsordnung == 1` == FALSE, 0, Staatsordnung)) %>%
  add_count(perioden, sicherheitspolitik == 1, name = "Sicherheitspolitik") %>%
  mutate(Sicherheitspolitik = ifelse(`sicherheitspolitik == 1` == FALSE, 0, Sicherheitspolitik)) %>%
  add_count(perioden, landwirtschaft == 1, name = "Landwirtschaft") %>%
  mutate(Landwirtschaft = ifelse(`landwirtschaft == 1` == FALSE, 0, Landwirtschaft)) %>%
  add_count(perioden, oefffinanzen == 1, name = "Oefffinanzen") %>%
  mutate(Oefffinanzen = ifelse(`oefffinanzen == 1` == FALSE, 0, Oefffinanzen)) %>%
  add_count(perioden, energie == 1, name = "Energie") %>%
  mutate(Energie = ifelse(`energie == 1` == FALSE, 0, Energie)) %>%
  add_count(perioden, verkehr_infrastruktur == 1, name = "verkehr_infrastruktur_year") %>%
  mutate(verkehr_infrastruktur_year = ifelse(`verkehr_infrastruktur == 1` == FALSE, 0, verkehr_infrastruktur_year)) %>%
  add_count(perioden, umwelt_lebensraum == 1, name = "umwelt_lebensraum_year") %>%
  mutate(umwelt_lebensraum_year = ifelse(`umwelt_lebensraum == 1` == FALSE, 0, umwelt_lebensraum_year)) %>%
  add_count(perioden, bildung_forschung == 1, name = "bildung_forschung_year") %>%
  mutate(bildung_forschung_year = ifelse(`bildung_forschung == 1` == FALSE, 0, bildung_forschung_year)) %>%
  # add_count(perioden, kultur_religion_medien == 1, name = "kultur_religion_medien_year") %>%
  # mutate(kultur_religion_medien_year = ifelse(`kultur_religion_medien == 1` == FALSE, 0, kultur_religion_medien_year)) %>%
  # add_count(perioden, aussenpolitik == 1, name = "aussenpolitik_year") %>%
  # mutate(aussenpolitik_year = ifelse(`aussenpolitik == 1` == FALSE, 0, aussenpolitik_year)) %>%
  dplyr::select(perioden, Sozialpolitik, Wirtschaft, `1st.dim_year`, `2nd.dim_year`, Staatsordnung, Sicherheitspolitik, Landwirtschaft, Oefffinanzen,
         Energie, verkehr_infrastruktur_year, umwelt_lebensraum_year, bildung_forschung_year) %>%
  group_by(perioden,) %>%
  summarise_all(~ max(., na.rm=T))

w <- green_votes_perioden[,-1]
w <- as.data.frame(rbind(w[1,]/8, w[2,]/11, w[3,]/9, w[4,]/5, w[5,]/6))
green_votes_perioden <- cbind(green_votes_perioden[,1], w)


green_votes_perioden <- green_votes_perioden %>%
  gather(key, value, -perioden)


green_votes_first_second <- filter(green_votes_perioden, key %in% c("1st.dim_year", "2nd.dim_year"))
green_votes_first_second$perioden <- as.factor(green_votes_first_second$perioden)
green_votes_first_second$key <- recode_factor(green_votes_first_second$key, '1st.dim_year'="Ökonomische Dimension",
                                              '2nd.dim_year'="Kulturelle Dimension")


ggplot(data = green_votes_first_second, 
       aes(y = value, x = perioden)) +
  geom_col() + 
  facet_wrap(~key) +
  theme_minimal() +
  xlab("Zeitperioden") + 
  ylab("Jahresdurchschnitt in Zeitperioden initiierte Abstimmungen") +
  scale_x_discrete(labels=c("1" = "1980-1991", "2" = "1992-2002",
                            "3" = "2003-2011", "4" = "2012-2016", "5" = "2017-heute")) +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        strip.text.x = element_text(size = 12))


###################
#Grafik 3: Initiierung von Abstimmungen nach Themen und Zeitperioden
###################

green_votes_themes <- filter(green_votes_perioden, !key %in% c("1st.dim_year", "2nd.dim_year"))
green_votes_themes$perioden <- as.factor(green_votes_themes$perioden)
green_votes_themes$key <- recode_factor(green_votes_themes$key,
                                        'bildung_forschung_year'="Bildung & Forschung",
                                        'oefffinanzen'="Öffentliche Finanzen",
                                        'umwelt_lebensraum_year'="Umwelt & Lebensraum",
                                        'verkehr_infrastruktur_year'="Verkehr & Infrastruktur", 
                                        'Energie'="Energie",
                                        'Landwirtschaft'="Landwirtschaft",
                                        'Sicherheitspolitik'="Sicherheitspolitik",
                                        'Sozialpolitik'="Sozialpolitik",
                                        'Staatsordnung'="Staatsordnung",
                                        'Wirtschaft'="Wirtschaft")



ggplot(data = green_votes_themes, 
       aes(y = value, x = perioden)) +
  geom_col() + 
  facet_wrap(~key) +
  theme_minimal() +
  xlab("Zeitperioden") + 
  ylab("Jahresdurchschnitt in Zeitperioden initiierte Abstimmungen") +
  scale_x_discrete(labels=c("1" = "1980-1991", "2" = "1992-2002",
                            "3" = "2003-2011", "4" = "2012-2016", "5" = "2017-heute")) +
  theme(axis.text.x = element_text(angle = 45, size = 6))



###################
#Grafik 4: Anteil Abstimmungen, zu denen Die GPS und die SPS Stimmempfehlungen in einer Zeitperiode nach Thema abgaben
###################

load("Grüne_Abstimmungen.RData")

green_votes <- green_votes %>%
  rename_all(~str_replace_all(.,"\\-","_"))

green_votes$umwelt <-  recode(green_votes$umwelt, `1` = "Umweltthema", `0` = "anderes Thema")
green_votes$rechtsform <-  recode(green_votes$rechtsform, `2` = "fakultatives Referendum", `3` = "Volksinitiative")
green_votes$initiiert <- recode(green_votes$initiiert, `1` = "initiiert", `2` = "mitgetragen")


parolen_year <- green_votes %>%
  add_count(year, umwelt == "Umweltthema", name = "v_year") %>%
  add_count(year, (umwelt == "Umweltthema") & (p_gps %in% c(1,2)), name = "parolen_year") %>%
  mutate(parolen_year = ifelse(`(umwelt == "Umweltthema") & (p_gps %in% c(1, 2))` == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_umweltvorlagen = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -`(umwelt == "Umweltthema") & (p_gps %in% c(1, 2))`)


parolen_year <- parolen_year %>%
  add_count(year, `2nd.dim` == 1, name = "v_year") %>%
  add_count(year, `2nd.dim` == 1 & (p_gps %in% c(1,2)), name = "parolen_year") %>%
  rename_at(34,~"parole") %>%
  mutate(parolen_year = ifelse(parole == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_2nddim = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -parole)


parolen_year <- parolen_year %>%
  add_count(year, `1st.dim` == 1, name = "v_year") %>%
  add_count(year, `1st.dim` == 1 & (p_gps %in% c(1,2)), name = "parolen_year") %>%
  rename_at(36,~"parole") %>%
  mutate(parolen_year = ifelse(parole == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_1stdim = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -parole)


parolen_year <- parolen_year %>%
  dplyr::select(year, perioden, parolen_umweltvorlagen, parolen_2nddim, parolen_1stdim) %>%
  group_by(year) %>%
  summarise_all(~ max(., na.rm=T))


parolen_perioden <- parolen_year %>%
  group_by(perioden) %>%
  dplyr::summarize(p_umweltvorlagen = mean(parolen_umweltvorlagen[parolen_umweltvorlagen != 0], na.rm=TRUE), 
                   p_1stdim = mean(parolen_1stdim[parolen_1stdim !=0], na.rm=TRUE),
                   p_2nddim = mean(parolen_2nddim[parolen_2nddim != 0], na.rm=TRUE))


parolen_perioden <- parolen_perioden %>%
  gather(key, value, -perioden)
parolen_perioden$perioden <- as.factor(parolen_perioden$perioden)
parolen_perioden$key <- recode_factor(parolen_perioden$key,
                                             'p_1stdim'="Ökonomische Dimension",
                                             'p_2nddim'="Kulturelle Dimension",
                                             'p_umweltvorlagen'="Umweltthemen")


plot_gps <- ggplot(data = parolen_perioden) +
  geom_col(aes(x = perioden, y = value)) + 
  facet_wrap(~key) +
  theme_minimal() +
  xlab("Zeitperioden") + 
  ylab("Anteil Parolen zu Vorlagen") + 
  ggtitle("GPS") +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        strip.text.x = element_text(size = 12)) +
  scale_x_discrete(labels=c("1" = "1980-1991", "2" = "1992-2002",
                            "3" = "2003-2011", "4" = "2012-2016", "5" = "2017-heute"))

###
#Dasselbe für die SPS
###

parolen_year_sps <- green_votes %>%
  add_count(year, umwelt == "Umweltthema", name = "v_year") %>%
  add_count(year, (umwelt == "Umweltthema") & (p_sps %in% c(1,2)), name = "parolen_year") %>%
  mutate(parolen_year = ifelse(`(umwelt == "Umweltthema") & (p_sps %in% c(1, 2))` == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_umweltvorlagen = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -`(umwelt == "Umweltthema") & (p_sps %in% c(1, 2))`)


parolen_year_sps <- parolen_year_sps %>%
  add_count(year, `2nd.dim` == 1, name = "v_year") %>%
  add_count(year, `2nd.dim` == 1 & (`p_sps` %in% c(1,2)), name = "parolen_year") %>%
  rename_at(34,~"parole") %>%
  mutate(parolen_year = ifelse(parole == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_2nddim = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -parole)


parolen_year_sps <- parolen_year_sps %>%
  add_count(year, `1st.dim` == 1, name = "v_year") %>%
  add_count(year, `1st.dim` == 1 & (`p_sps` %in% c(1,2)), name = "parolen_year") %>%
  rename_at(36,~"parole") %>%
  mutate(parolen_year = ifelse(parole == FALSE, 0, parolen_year)) %>%
  group_by(year) %>%
  mutate(parolen_1stdim = parolen_year / v_year) %>%
  dplyr::select(-v_year, -parolen_year, -parole)


parolen_year_sps <- parolen_year_sps %>%
  dplyr::select(year, perioden, parolen_umweltvorlagen, parolen_2nddim, parolen_1stdim) %>%
  group_by(year) %>%
  summarise_all(~ max(., na.rm=T))


parolen_perioden_sps <- parolen_year_sps %>%
  group_by(perioden) %>%
  dplyr::summarize(p_umweltvorlagen = mean(parolen_umweltvorlagen[parolen_umweltvorlagen != 0], na.rm=TRUE), 
                   p_1stdim = mean(parolen_1stdim[parolen_1stdim !=0], na.rm=TRUE),
                   p_2nddim = mean(parolen_2nddim[parolen_2nddim != 0], na.rm=TRUE),)


parolen_perioden_sps <- parolen_perioden_sps %>%
  gather(key, value, -perioden) %>%
  filter(key %in% c("p_umweltvorlagen", "p_1stdim", "p_2nddim"))

parolen_perioden_sps$perioden <- as.factor(parolen_perioden_sps$perioden)
parolen_perioden_sps$key <- recode_factor(parolen_perioden_sps$key,
                                          'p_1stdim'="Ökonomische Dimension",
                                          'p_2nddim'="Kulturelle Dimension",
                                          'p_umweltvorlagen'="Umweltthemen")

plot_sp <- ggplot(data = parolen_perioden_sps) +
  geom_col(aes(x = perioden, y = value)) + 
  facet_wrap(~key) +
  theme_minimal() +
  xlab("Zeitperioden") + 
  ylab("Anteil Parolen zu Vorlagen") + 
  ggtitle("SPS") +
  theme(axis.text.x = element_text(angle = 45, size = 10),
        strip.text.x = element_text(size = 12)) +
  scale_x_discrete(labels=c("1" = "1980-1991", "2" = "1992-2002",
                            "3" = "2003-2011", "4" = "2012-2016", "5" = "2017-heute"))


plot_grid(plot_gps, plot_sp)

