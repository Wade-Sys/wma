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






