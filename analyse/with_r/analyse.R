library(psych)
library(MESS)
library(ggplot2)
library(DescTools)
library(corrplot)

# df_wma_csv <- read.csv2(file = '../../data/wmm_data/daten_wmm_all_prepared.csv',header = TRUE,dec = ".",sep = ";")
# df_wetter_csv <- read.csv2(file = '../../data/wetter/daten_wetter_tmp_dew_sr.csv',header = TRUE,dec = ".",sep = ";")
# as.Date(df_wma_csv$Datum, format = "%Y-%m-%d")
# as.Date(df_wma_csv$Datum, format = "%Y-%m-%d")
# df_wma_1 <- df_wma_csv
# df_wetter_1 <- df_wetter_csv
# df_wma_1$Datum <- strptime(x = as.character(df_wma_1$Datum),"%Y-%m-%d")
# df_wetter_1$Datum <- strptime(x = as.character(df_wetter_1$Datum),"%Y-%m-%d")
# df_wma_2 <- subset(df_wma_1, select = -c(T_KM_5,T_KM_10,T_KM_15,T_KM_20,T_KM_HM,T_KM_25,T_KM_30,T_KM_35,T_KM_40,T_KM_FN,Startzeit,Datum_Startzeit_UTC))
# df_wetter_2_mean <- subset(df_wetter_1, select = c(Jahr,Ort,Datum,WND_SR_MEAN,TMP_MEAN,DEW_MEAN))
# df_wma_wetter <- merge(x = df_wma_2, y = df_wetter_2_mean, by = c("Jahr","Ort","Datum"))
# df_wetter_2 <- df_wetter_1
# df_wetter_2$TMP_MEAN_RND <- round(df_wetter_2$TMP_MEAN/5,1)*5
# df_wetter_2$DEW_MEAN_RND <- round(df_wetter_2$DEW_MEAN/5,1)*5
# df_wetter_2$WND_SR_MEAN_RND <- round(df_wetter_2$WND_SR_MEAN,1)
# df_wetter_3 <- subset(df_wetter_2, select = -c(WND_SR_MIN, WND_SR_MAX, WND_SR_MEDIAN, DEW_MIN, DEW_MAX, DEW_MEDIAN, TMP_MIN, TMP_MAX, TMP_MEDIAN))
# df_wma_wetter_2 <- merge(x=df_wma_2, y=df_wetter_3, by.x = c("Jahr", "Ort", "Datum"), by.y = c("Jahr", "Ort", "Datum"))


#
df_wetter_4$TMP_MEAN_RND1 <- round(df_wetter_4$TMP_MEAN, digits = 1)
df_wetter_4$DEW_MEAN_RND1 <- round(df_wetter_4$DEW_MEAN, digits = 1)
df_wetter_4$WND_SR_MEAN_RND1 <- round(df_wetter_4$WND_SR_MEAN, digits = 1)


df_wma_wetter_3 <- merge(df_wma_2, df_wetter_4, by = c("Jahr","Ort","Datum"))

df_ww3 <- df_wma_wetter_3
df_ww3y <- subset(df_ww3, (Jahr==2010 | Jahr==2011 | Jahr > 2012))

# Alle
df_ww3_m_all <- subset(df_ww3, (Geschlecht=='M'))
df_ww3_m_top5 <- subset(df_ww3, (Geschlecht=='M' & Platz <= 5))
df_ww3_m_top3 <- subset(df_ww3, (Geschlecht=='M' & Platz <= 3))
df_ww3_w_all <- subset(df_ww3, (Geschlecht=='W'))
df_ww3_w_top5 <- subset(df_ww3, (Geschlecht=='W' & Platz <= 5))
df_ww3_w_top3 <- subset(df_ww3, (Geschlecht=='W' & Platz <= 3))

# Alle: 2010,2011,2013-2019
df_ww3y_top5 <- subset(df_ww3y, (Platz <= 5))
df_ww3y_top3 <- subset(df_ww3y, (Platz <= 3))

df_ww3y_m_all <- subset(df_ww3y, (Geschlecht=='M'))
df_ww3y_m_top5 <- subset(df_ww3y, (Geschlecht=='M' & Platz <= 5))
df_ww3y_m_top3 <- subset(df_ww3y, (Geschlecht=='M' & Platz <= 3))
df_ww3y_w_all <- subset(df_ww3y, (Geschlecht=='W'))
df_ww3y_w_top5 <- subset(df_ww3y, (Geschlecht=='W' & Platz <= 5))
df_ww3y_w_top3 <- subset(df_ww3y, (Geschlecht=='W' & Platz <= 3))

# London: 2010 - 2019
df_ww3_london_m_all <- subset(df_ww3, (Ort=='London' & Geschlecht=='M'))
df_ww3_london_m_top5 <- subset(df_ww3, (Ort=='London' & Geschlecht=='M' & Platz <= 5))
df_ww3_london_m_top3 <- subset(df_ww3, (Ort=='London' & Geschlecht=='M' & Platz <= 3))
df_ww3_london_w_all <- subset(df_ww3, (Ort=='London' & Geschlecht=='W'))
df_ww3_london_w_top5 <- subset(df_ww3, (Ort=='London' & Geschlecht=='W' & Platz <= 5))
df_ww3_london_w_top3 <- subset(df_ww3, (Ort=='London' & Geschlecht=='W' & Platz <= 3))

# London: 2010,2011,2013-2019
df_ww3y_london_m_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M'))
df_ww3y_london_m_top5 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M' & Platz <= 5))
df_ww3y_london_m_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='M' & Platz <= 3))
df_ww3y_london_w_all <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W'))
df_ww3y_london_w_top5 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W' & Platz <= 5))
df_ww3y_london_w_top3 <- subset(df_ww3y, (Ort=='London' & Geschlecht=='W' & Platz <= 3))

# Berlin: 2007 - 2019
df_ww3_berlin_m_all <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='M'))
df_ww3_berlin_m_top5 <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 5))
df_ww3_berlin_m_top3 <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 3))
df_ww3_berlin_w_all <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='W'))
df_ww3_berlin_w_top5 <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 5))
df_ww3_berlin_w_top3 <- subset(df_ww3, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 3))

# Berlin: 2010,2011,2013-2019
df_ww3y_berlin_m_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M'))
df_ww3y_berlin_m_top5 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 5))
df_ww3y_berlin_m_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='M' & Platz <= 3))
df_ww3y_berlin_w_all <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W'))
df_ww3y_berlin_w_top5 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 5))
df_ww3y_berlin_w_top3 <- subset(df_ww3y, (Ort=='Berlin' & Geschlecht=='W' & Platz <= 3))

# Chicago: 2007 - 2019
df_ww3_chicago_m_all <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='M'))
df_ww3_chicago_m_top5 <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 5))
df_ww3_chicago_m_top3 <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 3))
df_ww3_chicago_w_all <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='W'))
df_ww3_chicago_w_top5 <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 5))
df_ww3_chicago_w_top3 <- subset(df_ww3, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 3))

# Chicago: 2010,2011,2013-2019
df_ww3y_chicago_m_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M'))
df_ww3y_chicago_m_top5 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 5))
df_ww3y_chicago_m_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='M' & Platz <= 3))
df_ww3y_chicago_w_all <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W'))
df_ww3y_chicago_w_top5 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 5))
df_ww3y_chicago_w_top3 <- subset(df_ww3y, (Ort=='Chicago' & Geschlecht=='W' & Platz <= 3))

# NewYork: 2007 - 2011; 2013 - 2019
df_ww3_newyork_m_all <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='M'))
df_ww3_newyork_m_top5 <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 5))
df_ww3_newyork_m_top3 <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 3))
df_ww3_newyork_w_all <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='W'))
df_ww3_newyork_w_top5 <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 5))
df_ww3_newyork_w_top3 <- subset(df_ww3, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 3))

# NewYork: 2010,2011,2013-2019
df_ww3y_newyork_m_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M'))
df_ww3y_newyork_m_top5 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 5))
df_ww3y_newyork_m_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='M' & Platz <= 3))
df_ww3y_newyork_w_all <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W'))
df_ww3y_newyork_w_top5 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 5))
df_ww3y_newyork_w_top3 <- subset(df_ww3y, (Ort=='NewYork' & Geschlecht=='W' & Platz <= 3))

# Tokyo: 2007 - 2019
df_ww3_tokyo_m_all <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='M'))
df_ww3_tokyo_m_top5 <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 5))
df_ww3_tokyo_m_top3 <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 3))
df_ww3_tokyo_w_all <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='W'))
df_ww3_tokyo_w_top5 <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 5))
df_ww3_tokyo_w_top3 <- subset(df_ww3, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 3))

# Tokyo: 2010,2011,2013-2019
df_ww3y_tokyo_m_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M'))
df_ww3y_tokyo_m_top5 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 5))
df_ww3y_tokyo_m_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='M' & Platz <= 3))
df_ww3y_tokyo_w_all <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W'))
df_ww3y_tokyo_w_top5 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 5))
df_ww3y_tokyo_w_top3 <- subset(df_ww3y, (Ort=='Tokyo' & Geschlecht=='W' & Platz <= 3))
## --------------------------------------------------------------------------------------------

describeBy(df_ww3y_m_top5$S_KM_FN, df_ww3y_m_top5$Ort)
describeBy(df_ww3y_m_top3$S_KM_FN, df_ww3y_m_top3$Ort)

describeBy(df_ww3y_w_top5$S_KM_FN, df_ww3y_w_top5$Ort)
describeBy(df_ww3y_w_top3$S_KM_FN, df_ww3y_w_top3$Ort)
## --------------------------------------------------------------------------------------------

# Boxplot: Ergebnisse / Wettbewerbsort (M)
ggplot(df_ww3y_m_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): Gesamt") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_m_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top5, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-5") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_m_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (M): TOP-3") +
  scale_y_continuous(breaks = seq(7000,8350,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_m_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Boxplot: Ergebnisse / Wettbewerbsort (W)
ggplot(df_ww3y_w_all, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): Gesamt") +
  scale_y_continuous(breaks = seq(8000,11000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_w_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top5, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-5") +
  scale_y_continuous(breaks = seq(8000,10000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_w_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot(alpha=0.7) +
  labs(y="Zeit (in Sek.)", x="Wettbewerbsort", title = "Ergebnisse (W): TOP-3") +
  scale_y_continuous(breaks = seq(8000,10000,100)) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Set3")
ggsave(filename = "bplt_ergb_w_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## ----------------------------------------------------------------------
# Histogramm: Verteilung der Ergebnisse
# Männer
ggplot(data = df_ww3y_m_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): Gesamt") + 
  scale_x_continuous(breaks = seq(7000,9000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_m_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_m_top5, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): TOP-5") + 
  scale_x_continuous(breaks = seq(7000,9000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_m_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_m_top3, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 50, color="white", fill="orange") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (M): TOP-3") + 
  scale_x_continuous(breaks = seq(7000,9000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_m_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen
ggplot(data = df_ww3y_w_all, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 100, color="white", fill="skyblue") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): Gesamt") + 
  scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_w_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_w_top5, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 120, color="white", fill="skyblue") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): TOP-5") + 
  scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_w_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(data = df_ww3y_w_top3, aes(x=S_KM_FN)) + 
  geom_histogram(binwidth = 140, color="white", fill="skyblue") + 
  labs(x="Ergebnisse (in Sek.)", y="Häufigkeit (abs)", title = "Verteilung d. Ergebnisse (W): TOP-3") + 
  scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,70,5))
ggsave(filename = "hplt_ergb_vert_w_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## --------------------------------------------------------------------------------------------------------------

# Scatterplots:
# Männer
ggplot(df_ww3y_m_all, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): Gesamt", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_m_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top5, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-5", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_m_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_m_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (M): TOP-3", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(7000,8500,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_m_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

# Frauen
ggplot(df_ww3y_w_all, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): Gesamt", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,11000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_w_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top5, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-5", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,11000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_w_top5.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

ggplot(df_ww3y_w_top3, aes(y=S_KM_FN, x=TMP_MEAN_RND1, color=Ort), ) + geom_point(alpha=0.5, size=1) + 
  labs(y="Ergebnisse (in Sek.)", x="Temperatur (°C)", title = "Ergebnisse (W): TOP-3", subtitle = "Ergebnisse ~ Temperatur") + 
  scale_y_continuous(breaks = seq(8000,11000,100)) + 
  scale_x_continuous(breaks = seq(0,22,1)) +
  scale_fill_brewer(palette="Set3") +
  scale_color_discrete("Wettbewerbsort")
ggsave(filename = "sctr_ergb_tmp_w_top3.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

## ----------------------------------------------------------------

create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 5)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 3)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 2)
create_reg_plots(data_frame = df_ww3y, reg_poly = 2, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 1)

create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 10)
create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 5)
create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 3)
create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 2)
create_reg_plots(data_frame = df_ww3y, reg_poly = 1, tmp_min = 0, tmp_max = 22, platz_min = 1, platz_max = 1)

## ----------------------------------------------------------------
# Verteilung der Temperatur pro Geschlecht / Ort
ggplot(data = df_ww3y, aes(x=TMP_MEAN_RND1)) + 
  geom_histogram(color="white", fill="orange") + 
  labs(x="Temperatur (in C°)", y="Häufigkeit (abs)", title = "Verteilung d. Temperatur") + 
  scale_x_continuous(breaks = seq(0,25,1)) + scale_y_continuous(breaks = seq(0,100,5))
#ggsave(filename = "hplt_ergb_m_all.pdf", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "pdf", dpi=300, width = 1920, height = 1080)

print_temps(df_ww3)
#table(df_ww3y$TMP_MEAN_RND1, df_ww3y$Ort, df_ww3y$Geschlecht)
#table(df_ww3y$TMP_MEAN, df_ww3y$Ort, df_ww3y$Geschlecht)

