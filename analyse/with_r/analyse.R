library(psych)
library(MESS)
library(ggplot2)
library(DescTools)
library(corrplot)
library(dplyr)
library(reshape2)
library(car)
#library(robustbase)
#library(data.table) # für melt

# Daten einlesen & formatieren
df_wma_csv <- read.csv2(file = '../../data/wmm_data/daten_wmm_all_prepared.csv',header = TRUE,dec = ".",sep = ";")
df_wetter_csv <- read.csv2(file = '../../data/wetter/daten_wetter_tmp_dew_sr.csv',header = TRUE,dec = ".",sep = ";")
# as.Date(df_wma_csv$Datum, format = "%Y-%m-%d")
# as.Date(df_wma_csv$Datum, format = "%Y-%m-%d")
df_wma_1 <- df_wma_csv
df_wetter_1 <- df_wetter_csv
df_wma_1$Datum <- strptime(x = as.character(df_wma_1$Datum),"%Y-%m-%d")
df_wetter_1$Datum <- strptime(x = as.character(df_wetter_1$Datum),"%Y-%m-%d")

# Nicht benötigte spalten entfernen (Marathondaten)
df_wma_2 <- subset(df_wma_1, select = -c(T_KM_5,T_KM_10,T_KM_15,T_KM_20,T_KM_HM,T_KM_25,T_KM_30,T_KM_35,T_KM_40,T_KM_FN,Startzeit,Datum_Startzeit_UTC))

# Wetterdaten: Runden (auf 0.5)
df_wetter_2 <- df_wetter_1
df_wetter_2$TMP_MEAN_RND <- round(df_wetter_2$TMP_MEAN/5,1)*5 # Auf 0.5 
df_wetter_2$DEW_MEAN_RND <- round(df_wetter_2$DEW_MEAN/5,1)*5 # Auf 0.5
df_wetter_2$WND_SR_MEAN_RND <- round(df_wetter_2$WND_SR_MEAN,1) # Normal

# Nicht benötigte spalten entfernen (Wetterdaten)
df_wetter_3 <- subset(df_wetter_2, select = -c(WND_SR_MIN, WND_SR_MAX, WND_SR_MEDIAN, DEW_MIN, DEW_MAX, DEW_MEDIAN, TMP_MIN, TMP_MAX, TMP_MEDIAN))

# Wetterdaten und Marathondaten mergen (1)
df_wma_wetter_2 <- merge(x=df_wma_2, y=df_wetter_3, by.x = c("Jahr", "Ort", "Datum"), by.y = c("Jahr", "Ort", "Datum"))


# Wetterdaten: Runden (normal) 
df_wetter_4 <- df_wetter_3
df_wetter_4$TMP_MEAN_RND1 <- round(df_wetter_4$TMP_MEAN, digits = 1)
df_wetter_4$DEW_MEAN_RND1 <- round(df_wetter_4$DEW_MEAN, digits = 1)
df_wetter_4$WND_SR_MEAN_RND1 <- round(df_wetter_4$WND_SR_MEAN, digits = 1)
# Wetter um Jahr bereinigt
df_wetter_4y <- subset(df_wetter_4, (Jahr >= 2010))
df_wetter_4y <- subset(df_wetter_4y, (Jahr != 2012))

# Wetterdaten und Marathondaten mergen (2)
df_wma_wetter_3 <- merge(df_wma_2, df_wetter_4, by = c("Jahr","Ort","Datum"))

# Neues Dataframe (normal und um die Jahre bereinigt)
df_ww3 <- df_wma_wetter_3
df_ww3y <- subset(df_ww3, (Jahr==2010 | Jahr==2011 | Jahr > 2012))


# Neue Dataframes nach Geschlecht und Platzierung

# Alle: 2010,2011,2013-2019
df_ww3y_top5 <- subset(df_ww3y, (Platz <= 5))
df_ww3y_top3 <- subset(df_ww3y, (Platz <= 3))

df_ww3y_m_all <- subset(df_ww3y, (Geschlecht=='M'))
df_ww3y_m_top3 <- subset(df_ww3y, (Geschlecht=='M' & Platz <= 3))
df_ww3y_w_all <- subset(df_ww3y, (Geschlecht=='W'))
df_ww3y_w_top3 <- subset(df_ww3y, (Geschlecht=='W' & Platz <= 3))


# London: 2010,2011,2013-2019
df_ww3y_london_m_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M'))
df_ww3y_london_m_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M' & Platz <= 3))
df_ww3y_london_w_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W'))
df_ww3y_london_w_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W' & Platz <= 3))

# Berlin: 2010,2011,2013-2019
df_ww3y_berlin_m_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M'))
df_ww3y_berlin_m_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 3))
df_ww3y_berlin_w_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W'))
df_ww3y_berlin_w_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 3))

# Chicago: 2010,2011,2013-2019
df_ww3y_chicago_m_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M'))
df_ww3y_chicago_m_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 3))
df_ww3y_chicago_w_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W'))
df_ww3y_chicago_w_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 3))

# NewYork: 2010,2011,2013-2019
df_ww3y_newyork_m_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M'))
df_ww3y_newyork_m_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 3))
df_ww3y_newyork_w_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W'))
df_ww3y_newyork_w_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 3))

# Tokyo: 2010,2011,2013-2019
df_ww3y_tokyo_m_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M'))
df_ww3y_tokyo_m_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 3))
df_ww3y_tokyo_w_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W'))
df_ww3y_tokyo_w_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 3))
## --------------------------------------------------------------------------------------------

# Boxplot: Ergebnisse / Wettbewerbsort (M)
ggplot(df_ww3y_m_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-10 (N=450)") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "bplt_ergb_m_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-3 (N=135)") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "bplt_ergb_m_n135_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Boxplot: Ergebnisse / Wettbewerbsort (W)
ggplot(df_ww3y_w_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-10 (N=450)") +
  scale_y_continuous(breaks = seq(8000,11000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "bplt_ergb_w_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-3 (N=135)") +
  scale_y_continuous(breaks = seq(8000,10000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3") +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red") 
ggsave(filename = "bplt_ergb_w_n135_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## ----------------------------------------------------------------------
# Histogramm: Verteilung der Ergebnisse
# Männer
ggplot(data = df_ww3y_m_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Zeit (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): TOP-10 (N=450)") + 
  scale_x_continuous(breaks = seq(7000,11000,200)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_m_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_m_top3, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): TOP-3") + 
  scale_x_continuous(breaks = seq(7000,9000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_m_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen
ggplot(data = df_ww3y_w_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 100, color="white", fill="skyblue") + 
  labs(x="Zeit (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): TOP-10 (N=450)") + 
  scale_x_continuous(breaks = seq(7000,11000,200)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_w_n450_top10.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_w_top3, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 140, color="white", fill="skyblue") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): TOP-3") + 
  scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_w_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------------------------

# Scatterplots:
# Männer
ggplot(df_ww3y_m_all, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=3) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-10", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort:") +
  theme(legend.position = "bottom")
ggsave(filename = "sctr_ergb_tmp_m_top10.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=3) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort:") +
  theme(legend.position = "bottom")
ggsave(filename = "sctr_ergb_tmp_m_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen
ggplot(df_ww3y_w_all, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=1, size=3) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-10", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,11000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort:") +
  theme(legend.position = "bottom")
ggsave(filename = "sctr_ergb_tmp_w_top10.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=1, size=3) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,11000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort:") +
  theme(legend.position = "bottom")
ggsave(filename = "sctr_ergb_tmp_w_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Allgemein:
ggplot(df_ww3y_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Geschlecht)) + geom_point(alpha=1, size=1.5) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M/W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,10000,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") +
  facet_wrap(~Ort, ncol=5)
  #theme(axis.text.x = element_text(angle = 45))
ggsave(filename = "sctr_ergb_tmp_mw_top3_grp.pdf", plot = last_plot(),units = "px",scale = 2, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=2) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") +
  facet_wrap(~Ort, ncol=5) +
  theme(legend.position = "none") 
ggsave(filename = "sctr_ergb_tmp_m_top3_grp.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort)) + geom_point(alpha=1, size=2) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,10000,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") +
  facet_wrap(~Ort, ncol=5) +
  theme(legend.position = "none") 
ggsave(filename = "sctr_ergb_tmp_w_top3_grp.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)


ggplot(df_ww3y_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Geschlecht)) + geom_point(alpha=0.8, size=3) + 
  labs(y="Zeit (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M/W): TOP-3", subtitle = "Zeit ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7100,10000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Geschlecht:") +
  theme(legend.position = "bottom") 
ggsave(filename = "sctr_ergb_tmp_mw_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)


## -----------------------------------------------------------------
## Temperatur - Liniendiagramme

ggplot(df_wetter_4y, aes(y=TMP_MEAN_RND1, x=Jahr, color=Ort)) + 
  geom_line(alpha=0.5, size=1) + geom_point() +
  labs(y="Temperatur (°C)", x="Jahr", title = "Temperaturverlauf (ausgewählte Jahre)") + 
  scale_y_continuous(breaks = seq(0,25,1.0)) + 
  scale_x_continuous(breaks = c(2010,2011,2013,2014,2015,2016,2017,2018,2019)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort") #+ facet_wrap(~Ort, ncol=5)
ggsave(filename = "line_tmp_y_ort.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)


ggplot(df_wetter_4y, aes(y=TMP_MEAN_RND1, x=Jahr, color=Ort)) + 
  geom_line(alpha=0.5, size=1) + geom_point() +
  labs(y="Temperatur (°C)", x="Jahr", title = "Temperaturverlauf (ausgewählte Jahre)") + 
  scale_y_continuous(breaks = seq(0,25,1.0)) + 
  scale_x_continuous(breaks = c(2010,2011,2013,2014,2015,2016,2017,2018,2019)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort") + 
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") + 
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "line_tmp_y_ort_wrap.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_wetter_4y, aes(y=TMP_MEAN_RND1, x=Jahr, color=Ort)) + 
  geom_bar(stat = "identity") +
  geom_hline(data=aggregate(x=df_wetter_4y$TMP_MEAN_RND1, by=list(Ort=df_wetter_4y$Ort), FUN="mean"), aes(yintercept = x), color="red") + 
  labs(y="Temperatur (°C)", x="Jahr", title = "Temperaturverlauf (ausgewählte Jahre)") + 
  scale_y_continuous(breaks = seq(0,25,1.0)) + 
  scale_x_continuous(breaks = c(2010,2011,2013,2014,2015,2016,2017,2018,2019)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort") + 
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") + 
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "bar_tmp_y_ort_wrap.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## ----------------------------------------------------------------
# Verteilung der Temperatur pro Geschlecht / Ort
print_temps(df_ww3)
## ----------------------------------------------------------------
## ----------------------------------------------------------------
## Korrelation
round(cor(df_ww3y_top3$TMP_MEAN_RND1, df_ww3y_top3$S_KM_FN), 2)

round(cor(df_ww3y_m_top3$TMP_MEAN_RND1, df_ww3y_m_top3$S_KM_FN), 2)

round(cor(df_ww3y_berlin_m_top3$TMP_MEAN_RND1, df_ww3y_berlin_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_london_m_top3$TMP_MEAN_RND1, df_ww3y_london_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_chicago_m_top3$TMP_MEAN_RND1, df_ww3y_chicago_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_newyork_m_top3$TMP_MEAN_RND1, df_ww3y_newyork_m_top3$S_KM_FN), 2)
round(cor(df_ww3y_tokyo_m_top3$TMP_MEAN_RND1, df_ww3y_tokyo_m_top3$S_KM_FN), 2)

round(cor(df_ww3y_w_top3$TMP_MEAN_RND1, df_ww3y_w_top3$S_KM_FN), 2)

round(cor(df_ww3y_berlin_w_top3$TMP_MEAN_RND1, df_ww3y_berlin_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_london_w_top3$TMP_MEAN_RND1, df_ww3y_london_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_chicago_w_top3$TMP_MEAN_RND1, df_ww3y_chicago_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_newyork_w_top3$TMP_MEAN_RND1, df_ww3y_newyork_w_top3$S_KM_FN), 2)
round(cor(df_ww3y_tokyo_w_top3$TMP_MEAN_RND1, df_ww3y_tokyo_w_top3$S_KM_FN), 2)

## Spezifische Regressionen
## Manuell mit Funktion
my_reg_skm_tmp(data_frame = df_ww3,reg_poly=2,tmp_min=0, tmp_max=25, platz_min=1, platz_max=10, ort=NULL, geschlecht="M")
my_reg_skm_tmp_2(data_frame = df_ww3,reg_poly=2,tmp_min=0, tmp_max=25, platz_min=1, platz_max=1, ort=NULL, geschlecht="W", skm="S_KM_35")
my_reg_skm_tmp_2(data_frame = df_ww3,reg_poly=2,tmp_min=0, tmp_max=25, platz_min=1, platz_max=1, ort=NULL, geschlecht="W", skm="S_KM_40") # Best Parameter (p2): W
my_reg_skm_tmp_2(data_frame = df_ww3,reg_poly=1,tmp_min=0, tmp_max=12.5, platz_min=1, platz_max=1, ort=NULL, geschlecht="W", skm="S_KM_40") # Best Parameter (p1): W
my_reg_skm_tmp_2(data_frame = df_ww3,reg_poly=1,tmp_min=12.5, tmp_max=25, platz_min=1, platz_max=1, ort=NULL, geschlecht="W", skm="S_KM_40") # Best Parameter (p1): Wmy_reg_skm_tmp_2(data_frame = df_ww3y,reg_poly=2,tmp_min=0, tmp_max=25, platz_min=1, platz_max=3, ort="Berlin", geschlecht="M", skm="S_KM_FN")

## Manuel: ohne Funktio
## Alle Orte je Geschlecht
ggplot(subset(df_ww3y, (Geschlecht=="M" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), aes(y=S_KM_FN, x=TMP_MEAN_RND1, fill=Ort)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2)) +
  labs(title = "Ergebnisse (M): TOP-3", x="Temperatur (°C)", y="Zeit (in Sek.)", subtitle = "Zeit ~ Temperatur(x^2)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(7100,8300,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "reg_p2_tmp_m_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(subset(df_ww3y, (Geschlecht=="W" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), aes(y=S_KM_FN, x=TMP_MEAN_RND1, fill=Ort)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2)) +
  labs(title = "Ergebnisse (W): TOP-3", x="Temperatur (°C)", y="Zeit (in Sek.)", subtitle = "Zeit ~ Temperatur(x^2)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(8000,9500,100)) + 
  scale_x_continuous(breaks = seq(0,22,2)) +
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~Ort, ncol=5)
ggsave(filename = "reg_p2_tmp_w_top3.pdf", plot = last_plot(),units = "px",scale = 1.5, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Regressionen Summary:
lm_berlin_m_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="M" & Ort=="Berlin" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_london_m_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="M" & Ort=="London" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_chicago_m_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="M" & Ort=="Chicago" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_newyork_m_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="M" & Ort=="NewYork" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_tokyo_m_top3_poly2 <-lm(data = subset(df_ww3y, (Geschlecht=="M" & Ort=="Tokyo" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))

summary(lm_berlin_m_top3_poly2)
summary(lm_london_m_top3_poly2)
summary(lm_chicago_m_top3_poly2)
summary(lm_newyork_m_top3_poly2)
summary(lm_tokyo_m_top3_poly2)

lm_berlin_w_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="W" & Ort=="Berlin" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_london_w_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="W" & Ort=="London" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_chicago_w_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="W" & Ort=="Chicago" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_newyork_w_top3_poly2 <- lm(data = subset(df_ww3y, (Geschlecht=="W" & Ort=="NewYork" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))
lm_tokyo_w_top3_poly2 <-lm(data = subset(df_ww3y, (Geschlecht=="W" & Ort=="Tokyo" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), formula = S_KM_FN ~ poly(TMP_MEAN_RND1,2))

summary(lm_berlin_w_top3_poly2)
summary(lm_london_w_top3_poly2)
summary(lm_chicago_w_top3_poly2)
summary(lm_newyork_w_top3_poly2)
summary(lm_tokyo_w_top3_poly2)

## Regressionen: (bereinigte Jahre)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 3)

create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 3)

## Regressionen: (alle Jahre)
create_reg_plots(data_frame = df_ww3, reg_poly = 2, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3, reg_poly = 2, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 3)

create_reg_plots(data_frame = df_ww3, reg_poly = 1, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3, reg_poly = 1, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 3)
## ----------------------------------------------------------------

## Automatisierte Erstellung der Regressionen: alle Jahre
create_reg_plots_2(data_frame = df_ww3, reg_poly = 2, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 10)
create_reg_plots_2(data_frame = df_ww3, reg_poly = 2, tmp_min = 0, tmp_max = 25, platz_min = 1, platz_max = 3)

# Automatische Erstellung alle Plots: reg_poly = 2; tmp = 0-25; alle Orte; Plätze = 1,3,5,10, Geschlecht: alle
lm_regs_from_reg_plots_3 <- create_reg_plots_3(data_frame = df_ww3)

# Erzeugen eines Dataframes mit RSQ und ARSQ aus den LM-Summarys
lm_regs_from_reg_plots_3_df <- create_reg_plots_3_to_df(lm_regs_from_reg_plots_3)

# Aggregation für R-Quadrat Analyse
lm_regs_from_reg_plots_3_df_agg <- aggregate(cbind(Ort, Platz, Geschlecht, SKM) ~ RSQ, lm_regs_from_reg_plots_3_df,max)

lm_regs_from_reg_plots_3_df_agg_srt <- lm_regs_from_reg_plots_3_df_agg[order(lm_regs_from_reg_plots_3_df_agg$Ort,lm_regs_from_reg_plots_3_df_agg$Geschlecht,
                                            lm_regs_from_reg_plots_3_df_agg$RSQ, decreasing = TRUE),]

write.csv2(x=lm_regs_from_reg_plots_3_df_agg_srt, file = "lm_rquadrat.csv", sep=";", 
           col.names = c("RSQ","Ort","Platz","Geschlecht","SKM"), dec = ".", quote = TRUE,row.names = FALSE)
## ----------------------------------------------------------------
## Zeiten / Pace-Analysen
df_ww4 <- df_ww3
df_ww4$FN_M_S <- round(42195 / df_ww4$S_KM_FN, digits = 2) # Pace in m/s - FN
df_ww4$HM_M_S <- round(21097.5 / df_ww4$S_KM_HM, digits = 2) # Pace in m/s - HM

## Pace analyse: Nur ein Test
ggplot(subset(df_ww4, (Geschlecht=="M" & Ort=="Berlin" & Platz <= 3 & (TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 25))), aes(y=HM_M_S, x=TMP_MEAN_RND1)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2))
#labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)


df_ww5 <- subset(df_ww4, select = c(Jahr, Ort, Geschlecht, Platz, S_KM_5, S_KM_10, S_KM_15, S_KM_20, S_KM_HM, S_KM_25, S_KM_30, S_KM_35, S_KM_40, S_KM_FN, TMP_MEAN_RND1, ZZ_INVALID))
df_ww5rs <- reshape(df_ww5, direction = "long", varying = c("S_KM_5","S_KM_10","S_KM_15","S_KM_20","S_KM_HM","S_KM_25","S_KM_30","S_KM_35","S_KM_40","S_KM_FN"), idvar = c("SKM_ID"), v.names = "SKM_ZEIT", timevar = "SKM_TYP")
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 1 & df_ww5rs$SKM_ZEIT!=0] <- round(5000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 1 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 2 & df_ww5rs$SKM_ZEIT!=0] <- round(10000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 2 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 3 & df_ww5rs$SKM_ZEIT!=0] <- round(15000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 3 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 4 & df_ww5rs$SKM_ZEIT!=0] <- round(20000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 4 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 5 & df_ww5rs$SKM_ZEIT!=0] <- round(21097.5 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 5 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 6 & df_ww5rs$SKM_ZEIT!=0] <- round(25000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 6 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 7 & df_ww5rs$SKM_ZEIT!=0] <- round(30000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 7 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 8 & df_ww5rs$SKM_ZEIT!=0] <- round(35000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 8 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 9 & df_ww5rs$SKM_ZEIT!=0] <- round(40000 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 9 & df_ww5rs$SKM_ZEIT!=0], digits = 2)
df_ww5rs$SKM_PACE[df_ww5rs$SKM_TYP == 10 & df_ww5rs$SKM_ZEIT!=0] <-round(42195 / df_ww5rs$SKM_ZEIT[df_ww5rs$SKM_TYP == 10 & df_ww5rs$SKM_ZEIT!=0], digits = 2)

# Verlauf - alle Jahre pro Wettbewerbsort: Zeit / Streckenabschnitt
ggplot(subset(df_ww5rs, (Geschlecht=="W" & Ort=="Tokyo" & Platz <=3 & SKM_TYP >= 5 & ZZ_INVALID == FALSE)), aes(x=SKM_TYP, y=SKM_ZEIT, group=Platz)) +
  geom_line(stat = "identity", position = "dodge", aes(color=Platz))+ 
  scale_x_continuous(breaks = seq(0,10,1)) +
  #scale_y_continuous(breaks = seq(4000,9000,500)) +
  scale_y_log10() +
  facet_wrap(~Jahr)

# Verlauf - einzeln: Zeit / Streckenabschnitt
ggplot(subset(df_ww5rs, (Geschlecht=="M" & Ort=="Berlin" & Jahr==2013 & Platz <=3 & SKM_TYP >= 5 & ZZ_INVALID == FALSE)), aes(x=SKM_TYP, y=SKM_ZEIT, group=Platz)) +
  geom_line(aes(color=Platz))+
  geom_point() +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(0,9000,500)) +
  scale_fill_brewer(palette="Set3")

ggplot(subset(df_ww5rs, (Geschlecht=="M" & Ort=="Berlin" & Platz <=3 & SKM_TYP >= 1 & ZZ_INVALID == FALSE)), aes(x=SKM_TYP, y=SKM_PACE, group=Platz)) +
  geom_line(stat = "identity", position = "dodge", aes(color=Platz)) + 
  #geom_point() + 
  scale_color_continuous(breaks=seq(1,3,1)) +
  scale_x_continuous(breaks = seq(1,10,1), labels = c("5","10","15","20","21","25","30","35","40","42")) +
  scale_y_log10() +
  labs(y="Geschwindikeit (in m/s)", x="Kilometerabschnitt", title = "Pace in ","ORT"," (GESCHLECT): TOP-3") + 
  #theme(legend.position = "none") +
  facet_wrap(~Jahr)

# Verlauf - einzeln: Meter pro Sekunden / Streckenabschnitt
ggplot(subset(df_ww5rs, (Geschlecht=="M" & Ort=="Berlin" & Platz <=3 & SKM_TYP >= 1 & ZZ_INVALID == FALSE & Jahr == 2019)), aes(x=SKM_TYP, y=SKM_PACE, group=Platz)) +
  geom_line(stat = "identity", position = "dodge", aes(color=Platz))+ 
  scale_color_continuous(breaks=seq(1,3,1)) +
  scale_x_continuous(breaks = seq(1,10,1), labels = c("5","10","15","20","21","25","30","35","40","42")) +
  #labs(y="Geschwindikeit (in m/s)", x="Kilometerabschnitt", title = "Pace in ","ORT"," (GESCHLECT): TOP-3") + 
  scale_y_log10()

plot_paces()
## ----------------------------------------------------------------
# Korrelation
# Subset-Test
subset(df_ww5, (Ort=="Berlin" & Geschlecht=="M" & ZZ_INVALID==FALSE), 
       c("TMP_MEAN_RND1","S_KM_5","S_KM_10","S_KM_15","S_KM_20","S_KM_HM","S_KM_25","S_KM_30","S_KM_35","S_KM_40","S_KM_FN"))

# Korrelation
cor_ww5rs <- round(cor(subset(df_ww5, (Ort=="Chicago" & Geschlecht=="W" & Platz<=1 & ZZ_INVALID==FALSE), 
                 c("TMP_MEAN_RND1","S_KM_5","S_KM_10","S_KM_15","S_KM_20","S_KM_HM","S_KM_25","S_KM_30","S_KM_35","S_KM_40","S_KM_FN"))),2)
# Triangle
cor_ww5rs[upper.tri(cor_ww5rs)] <- NA
# Cor-Plot
ggplot(data = melt(cor_ww5rs, na.rm = TRUE)) + 
  geom_tile(aes(x=Var1, y=Var2, fill=value)) +
  geom_text(aes(x=Var1, y=Var2, label=value), color="white")

## ----------------------------------------------------------------
# Finale Zeiten
describeBy(df_ww3y_m_all$S_KM_FN, df_ww3y_m_all$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
describeBy(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)

describe(df_ww3y_m_top3$S_KM_FN,quant = c(.25,.75), skew=TRUE)

describeBy(df_ww3y_w_all$S_KM_FN, df_ww3y_w_all$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
describeBy(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)

describe(df_ww3y_w_top3$S_KM_FN,quant = c(.25,.75), skew=TRUE)

# Wetter
describeBy(df_wetter_4$TMP_MEAN_RND1, df_wetter_4$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
describeBy(df_wetter_4y$TMP_MEAN_RND1, df_wetter_4y$Ort, quant = c(.25,.75), skew=TRUE, mat=TRUE, digits = 2)
## ----------------------------------------------------------------
## pairwise-test: M TOP10
# pairwise-test: two.sided
pairwise.t.test(df_ww3y_m_all$S_KM_FN, df_ww3y_m_all$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = FALSE)
# pairwise-test: less
pairwise.t.test(df_ww3y_m_all$S_KM_FN, df_ww3y_m_all$Ort, p.adjust.method = "bonferroni", alternative = "less", paired = FALSE, pool.sd = FALSE)
# pairwise-test: greater
pairwise.t.test(df_ww3y_m_all$S_KM_FN, df_ww3y_m_all$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE)
## --------------------------------------------------------------------
## pairwise-test: W TOP10
# pairwise-test: two.sided
pairwise.t.test(df_ww3y_w_all$S_KM_FN, df_ww3y_w_all$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = FALSE)
# pairwise-test: less
pairwise.t.test(df_ww3y_w_all$S_KM_FN, df_ww3y_w_all$Ort, p.adjust.method = "bonferroni", alternative = "less", paired = FALSE, pool.sd = FALSE)
# pairwise-test: greater
pairwise.t.test(df_ww3y_w_all$S_KM_FN, df_ww3y_w_all$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE)
## --------------------------------------------------------------------
## pairwise-test: M TOP3
# pairwise-test: two.sided
pairwise.t.test(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
# pairwise-test: less
pairwise.t.test(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, p.adjust.method = "bonferroni", alternative = "less", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
# pairwise-test: greater
pairwise.t.test(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
## --------------------------------------------------------------------
## pairwise-test: W TOP3
## pairwise-test: two-sided
pairwise.t.test(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, p.adjust.method = "bonferroni", alternative = "two.sided", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
# pairwise-test: less
pairwise.t.test(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, p.adjust.method = "bonferroni", alternative = "less", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
# pairwise-test: greater
pairwise.t.test(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort, p.adjust.method = "bonferroni", alternative = "greater", paired = FALSE, pool.sd = FALSE, conf.level = 0.95)
## ----------------------------------------------------------------
# Einseitiger Test
t.test(x=subset(df_ww3y_m_all, (Ort=='Berlin'), select = c(S_KM_FN)), 
       y=subset(df_ww3y_m_all, (Ort=='Chicago'), select = c(S_KM_FN)),
       paired = FALSE, conf.level = 0.975, var.equal = FALSE, alternative = "less"
)
## ----------------------------------------------------------------