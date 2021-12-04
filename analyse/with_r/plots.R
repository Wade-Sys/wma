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

library(corrplot)
corrplot(df_wetter_3_cor_matrix_rnd,type = "upper", order = "hclust", method = "color", addCoef.col = "white", cl.ratio = 0.5, tl.srt = 45)

ggplot(df_wetter_3, aes(sample=scale(TMP_MEAN_RND))) + stat_qq() + stat_qq_line() + 
  labs(title = "QQ-Plot (z-scaled) - Temperatur", x="theoretische Quantile", y="tatsächliche Quantile")
ggsave(filename = "plt_qq_tmp.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(df_wetter_3, aes(sample=scale(DEW_MEAN_RND))) + stat_qq() + stat_qq_line() + 
  labs(title = "QQ-Plot (z-scaled) - Taupunkt", x="theoretische Quantile", y="tatsächliche Quantile")
ggsave(filename = "plt_qq_dew.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(df_wetter_3, aes(sample=scale(WND_SR_MEAN_RND))) + stat_qq() + stat_qq_line() + 
  labs(title = "QQ-Plot (z-scaled) - Windstärke", x="theoretische Quantile", y="tatsächliche Quantile")
ggsave(filename = "plt_qq_wnd_sr.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(subset(df_wma_2, (Geschlecht=='M') ), aes(sample=scale(S_KM_FN))) + stat_qq() + stat_qq_line() + 
  labs(title = "QQ-Plot (z-scaled) - Finale Zeiten (Männer)", x="theoretische Quantile", y="tatsächliche Quantile")
ggsave(filename = "plt_qq_skmfn_m.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(subset(df_wma_2, (Geschlecht=='W') ), aes(sample=scale(S_KM_FN))) + stat_qq() + stat_qq_line() + 
  labs(title = "QQ-Plot (z-scaled) - Finale Zeiten (Frauen)", x="theoretische Quantile", y="tatsächliche Quantile")
ggsave(filename = "plt_qq_skmfn_w.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

model_skmfn_tmr_m <- lm(data = subset(df_wma_wetter_2,(Geschlecht=='M')), formula = S_KM_FN ~ TMP_MEAN_RND)
summary(model_skmfn_tmr_m)

model_skmfn_wsmr_m <- lm(data = subset(df_wma_wetter_2,(Geschlecht=='M')), formula = S_KM_FN ~ WND_SR_MEAN_RND)
summary(model_skmfn_wsmr_m)

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, TMP_MEAN_RND)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~x) +
  labs(title = "LR: Laufzeiten(m) ~ Temperatur (gerundet)", x="Temperatur (°C)", y="Zeiten (sek)")
ggsave(filename = "plt_lr_tmp_zeiten_m.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, WND_SR_MEAN_RND)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~x) +
  labs(title = "LR: Laufzeiten(m) ~ Windstärke (gerundet)", x="Windstärke (m/s)", y="Zeiten (sek)")
ggsave(filename = "plt_lr_wndsr_zeiten_m.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

model_skmfn_tmp_wsmr_m <- lm(data = subset(df_wma_wetter_2,(Geschlecht=='M')), formula = S_KM_FN ~ TMP_MEAN_RND + WND_SR_MEAN_RND)
summary(model_skmfn_tmp_wsmr_m)


ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, TMP_MEAN_RND)) + 
  geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2)) +
  labs(title = "LR: Laufzeiten(m) ~ Temperatur^2 (gerundet)", x="Temperatur (°C)", y="Zeiten (sek)")
ggsave(filename = "plt_lr_poly2_tmp_zeiten_m.png", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

model_poly2_skmfn_tmr_m <- lm(data = subset(df_wma_wetter_2,(Geschlecht=='M')), formula = S_KM_FN ~ poly(TMP_MEAN_RND,2))
summary(model_poly2_skmfn_tmr_m)

model_poly2_skmfn_tmr_wsmr_m <- lm(data = subset(df_wma_wetter_2,(Geschlecht=='M')), 
                              formula = S_KM_FN ~ poly(TMP_MEAN_RND,2) + WND_SR_MEAN_RND)
summary(model_poly2_skmfn_tmr_wsmr_m)


qqnorm(rstandard(model_skmfn_tmr_m))
qqline(rstandard(model_skmfn_tmr_m))

qqnorm(rstandard(model_skmfn_wsmr_m))
qqline(rstandard(model_skmfn_wsmr_m))

qqnorm(rstandard(model_skmfn_tmp_wsmr_m))
qqline(rstandard(model_skmfn_tmp_wsmr_m))

qqnorm(rstandard(model_poly2_skmfn_tmr_wsmr_m))
qqline(rstandard(model_poly2_skmfn_tmr_wsmr_m))

qqnorm(rstandard(model_poly2_skmfn_tmr_m))
qqline(rstandard(model_poly2_skmfn_tmr_m))

plot(fitted.values(model_poly2_skmfn_tmr_wsmr_m), residuals(model_poly2_skmfn_tmr_wsmr_m))
plot(fitted.values(model_poly2_skmfn_tmr_wsmr_m), rstandard(model_poly2_skmfn_tmr_wsmr_m))
plot(model_poly2_skmfn_tmr_wsmr_m,1)

durbinWatsonTest(model_poly2_skmfn_tmr_wsmr_m)

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=TMP_MEAN_RND, color=Ort)) + geom_point(alpha=0.5, size=2) + 
  labs(y="Laufzeiten", x="Temperatur (°C)", title = "Laufzeiten / Temperatur (Männer)") + 
  scale_y_continuous(breaks = seq(7000,9000,100)) + 
  scale_x_continuous(breaks = seq(0,26,1))
ggsave(filename = "plt_skmfn_temp_m_ort", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=WND_SR_MEAN_RND, color=Ort)) + geom_point(alpha=0.5, size=2) + 
  labs(y="Laufzeiten", x="Windstärke (m/s)", title = "Laufzeiten / Windstärke (Männer)") + 
  scale_y_continuous(breaks = seq(7000,11000,200)) + 
  scale_x_continuous(breaks = seq(0,12,1))
ggsave(filename = "plt_skmfn_wndsr_m_ort", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")


ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=TMP_MEAN_RND, x=WND_SR_MEAN_RND, color=Ort)) + 
  geom_point(aes(size=S_KM_FN)) + 
  labs(y="Temperatur (°C)", x="Windstärke (m/s)", title = "Temperatur (°C) / Windstärke / Laufzeit (Männer)") + 
  scale_y_continuous(breaks = seq(0,26,1)) + 
  scale_x_continuous(breaks = seq(0,12,1)) +
  scale_size_continuous(range = c(1,8))
ggsave(filename = "plt_tmp_wndsr_m_ort", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")


ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=TMP_MEAN_RND, color=Ort, alpha=0.2)) + 
  geom_point(aes(size=WND_SR_MEAN_RND)) + 
  labs(y="Laufzeit", x="Temperatur (°C)", title = "Temperatur (°C) / Windstärke / Laufzeit (Männer)") + 
  scale_y_continuous(breaks = seq(7000,9000,100)) + 
  scale_x_continuous(breaks = seq(0,26,1)) +
  scale_size_continuous(range = c(1,3))
ggsave(filename = "plt_lfz_tmp_wndsr_m_ort", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")

ggplot(subset(df_wma_wetter_2,(Geschlecht=='M')), aes(y=S_KM_FN, x=Ort, fill=Ort)) + 
  geom_boxplot() +
  labs(y="Laufzeit", x="Ort", title = "Laufzeit (Männer) / Ort")
ggsave(filename = "plt_box_lfz_m_ort", plot = last_plot(),units = "px",scale = 1, limitsize = FALSE, device = "png")


#install.packages("psych")
#library(psych)

df_wma_ort_skm_m <- subset(df_wma_2, (Geschlecht=='M'), select = c(S_KM_FN, Ort))

describeBy(df_wma_ort_skm_m$S_KM_FN, df_wma_ort_skm_m$Ort)

# Anova
anova_skmfn_ort_m_training <- aov(df_wma_ort_skm_m$S_KM_FN ~ df_wma_ort_skm_m$Ort)
summary(anova_skmfn_ort_m_training)

qqnorm(rstandard(anova_skmfn_ort_m_training))
qqline(rstandard(anova_skmfn_ort_m_training))

# Varinaz analyse
leveneTest(S_KM_FN ~ Ort, df_wma_ort_skm_m)

# one-way-test
oneway.test(S_KM_FN ~ Ort,  data = df_wma_ort_skm_m, var.equal = FALSE)

# pairwise-test: greater
pairwise.t.test(df_wma_ort_skm_m$S_KM_FN, df_wma_ort_skm_m$Ort, 
                p.adjust.method = "bonferroni", alternative = "greater",
                paired = FALSE, pool.sd = FALSE)

# pairwise-test: less
pairwise.t.test(df_wma_ort_skm_m$S_KM_FN, df_wma_ort_skm_m$Ort, 
                p.adjust.method = "bonferroni", alternative = "less",
                paired = FALSE, pool.sd = FALSE)

# pairwise-test: two.sided
pairwise.t.test(df_wma_ort_skm_m$S_KM_FN, df_wma_ort_skm_m$Ort, 
                p.adjust.method = "bonferroni", alternative = "two.sided",
                paired = FALSE, pool.sd = FALSE)

# var-test
var.test(x=unlist(subset(df_wma_ort_skm_m, (Ort=='Berlin'), select = c(S_KM_FN))), 
       y=unlist(subset(df_wma_ort_skm_m, (Ort=='London'), select = c(S_KM_FN))),
       conf.level = 0.95
)

# t-Test - Berlin ~ NewYork
t.test(x=subset(df_wma_ort_skm_m, (Ort=='Berlin'), select = c(S_KM_FN)), 
       y=subset(df_wma_ort_skm_m, (Ort=='NewYork'), select = c(S_KM_FN)),
       paired = FALSE, conf.level = 0.95, var.equal = FALSE, alternative = "greater"
)

# t-Test - Berlin ~ London
t.test(x=subset(df_wma_ort_skm_m, (Ort=='Berlin'), select = c(S_KM_FN)), 
       y=subset(df_wma_ort_skm_m, (Ort=='London'), select = c(S_KM_FN)),
       paired = FALSE, conf.level = 0.95, var.equal = FALSE, alternative = "greater"
)

t.test(x=subset(df_wma_ort_skm_m, (Ort=='NewYork'), select = c(S_KM_FN)), 
       y=subset(df_wma_ort_skm_m, (Ort=='Chicago'), select = c(S_KM_FN)),
       paired = FALSE, conf.level = 0.95, var.equal = FALSE, alternative = "greater"
)
