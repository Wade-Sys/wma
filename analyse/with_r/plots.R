ggsave(filename = "test.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(data = df_wetter_3, aes(x=TMP_MEAN_RND)) + geom_histogram(binwidth = 1, color="white", fill="skyblue") + labs(x="Temperatur (°C)", y="Häufigkeit (abs)", title = "Durchschn. Temperatur (ger.)") + scale_y_continuous(breaks = seq(0,11,1)) + scale_x_continuous(breaks = seq(0,25,1))
ggplot(data = df_wetter_3, aes(x=TMP_MEAN_RND)) + geom_histogram(binwidth = 2, color="white", fill="skyblue") + labs(x="Temperatur (°C)", y="Häufigkeit (abs)", title = "Durchschn. Temperatur (ger.)") + scale_y_continuous(breaks = seq(0,13,1)) + scale_x_continuous(breaks = seq(0,25,1))

ggplot(data = df_wetter_3, aes(x=DEW_MEAN_RND)) + geom_histogram(binwidth = 1, color="white", fill="skyblue") + labs(x="Temperatur (°C)", y="Häufigkeit (abs)", title = "Durchschn. Taupunkt (ger.)") + scale_y_continuous(breaks = seq(0,13,1)) + scale_x_continuous(breaks = seq(-15,25,1))
ggplot(data = df_wetter_3, aes(x=DEW_MEAN_RND)) + geom_histogram(binwidth = 3, color="white", fill="skyblue") + labs(x="Temperatur (°C)", y="Häufigkeit (abs)", title = "Durchschn. Taupunkt (ger.)") + scale_y_continuous(breaks = seq(0,13,1)) + scale_x_continuous(breaks = seq(-15,25,1))

ggplot(data = df_wetter_3, aes(x=WND_SR_MEAN_RND)) + geom_histogram(binwidth = 1, color="white", fill="skyblue") + labs(x="Windstärke (m/s)", y="Häufigkeit (abs)", title = "Durchschn. Windstärke (ger.)") + scale_y_continuous(breaks = seq(0,30,2)) + scale_x_continuous(breaks = seq(0,15,1))

ggplot(data = subset(df_wma_2, (Geschlecht=='M')), aes(x=S_KM_FN)) + geom_histogram(binwidth = 50, color="white", fill="skyblue") + labs(x="Laufzeiten (in sek.)", y="Häufigkeit (abs)", title = "Laufzeiten (Männer)") + scale_x_continuous(breaks = seq(7000,9000,100)) + scale_y_continuous(breaks = seq(0,70,5))
ggplot(data = subset(df_wma_2, (Geschlecht=='W')), aes(x=S_KM_FN)) + geom_histogram(binwidth = 50, color="white", fill="skyblue") + labs(x="Laufzeiten (in sek.)", y="Häufigkeit (abs)", title = "Laufzeiten (Frauen)") + scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,70,5))
ggplot(df_wma_2, aes(x=S_KM_FN,color=Geschlecht, fill=Geschlecht)) + geom_histogram(binwidth = 70,alpha=0.5, position = "dodge") + labs(x="Laufzeiten (in sek.)", y="Häufigkeit (abs)", title = "Laufzeiten (Frauen & Männer)") + scale_x_continuous(breaks = seq(7000,11000,500)) + scale_y_continuous(breaks = seq(0,100,10))

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=TMP_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Temperatur (°C)", title = "Laufzeiten / Temperatur (Männer)") + scale_y_continuous(breaks = seq(7000,9000,100)) + scale_x_continuous(breaks = seq(0,26,1))
ggplot(subset(df_wma_wetter_2,(Geschlecht=='W')), aes(y=S_KM_FN, x=TMP_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Temperatur (°C)", title = "Laufzeiten / Temperatur (Frauen)") + scale_y_continuous(breaks = seq(7000,11000,100)) + scale_x_continuous(breaks = seq(0,26,1))
ggplot(df_wma_wetter_2, aes(y=S_KM_FN, x=TMP_MEAN_RND,color=Geschlecht, fill=Geschlecht)) + geom_point() + labs(y="Laufzeiten", x="Temperatur (°C)", title = "Laufzeiten / Temperatur (Männer & Frauen)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(0,26,1))

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=DEW_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Taupunkt (°C)", title = "Laufzeiten / Taupunkt (Männer)") + scale_y_continuous(breaks = seq(7000,9000,100)) + scale_x_continuous(breaks = seq(-12,21,1))
ggplot(subset(df_wma_wetter_2,(Geschlecht=='W')), aes(y=S_KM_FN, x=DEW_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Taupunkt (°C)", title = "Laufzeiten / Taupunkt (Frauen)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(-12,21,1))
ggplot(df_wma_wetter_2, aes(y=S_KM_FN, x=DEW_MEAN_RND,color=Geschlecht, fill=Geschlecht)) + geom_point() + labs(y="Laufzeiten", x="Taupunkt (°C)", title = "Laufzeiten / Taupunkt (Männer & Frauen)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(-11,26,1))

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=WND_SR_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Windstärke (m/s)", title = "Laufzeiten / Windstärke (Männer)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(0,12,1))
ggplot(subset(df_wma_wetter_2,(Geschlecht=='W')), aes(y=S_KM_FN, x=WND_SR_MEAN_RND)) + geom_point() + labs(y="Laufzeiten", x="Windstärke (m/s)", title = "Laufzeiten / Windstärke (Frauen)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(0,12,1))
ggplot(df_wma_wetter_2, aes(y=S_KM_FN, x=WND_SR_MEAN_RND,color=Geschlecht, fill=Geschlecht)) + geom_point() + labs(y="Laufzeiten", x="Windstärke (m/s)", title = "Laufzeiten / Windstärke (Männer & Frauen)") + scale_y_continuous(breaks = seq(7000,11000,200)) + scale_x_continuous(breaks = seq(0,12,1))

