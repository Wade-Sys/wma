#summary(lm(data = subset(df_ww3_m_all, ((TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 20) & (Platz >= 1 & Platz <= 10) & Ort == "Tokyo")), formula = S_KM_FN ~ TMP_MEAN_RND1))

#ggplot(subset(df_ww3_m_all, ((TMP_MEAN_RND1 >= 0 & TMP_MEAN_RND1 <= 8) & (Platz >= 1 & Platz <= 10) & Ort == "Tokyo")), aes(y=S_KM_FN, TMP_MEAN_RND1)) + 
#  geom_point() + geom_smooth(method = "lm", formula = y~x) +
#  labs(title = "LR: Laufzeiten(m) ~ Temperatur (gerundet)", x="Temperatur (°C)", y="Zeiten (sek)")

# Eigene Funktion für die Regression zwischen den Ergebnisse und Temperatur
## ----------------------------------------------------------------------------------------------------------------------------------
my_reg_skm_tmp <- function(data_frame,reg_poly=1,tmp_min, tmp_max, platz_min, platz_max, ort=NULL, geschlecht=NULL) {
  if(!is.null(ort) && !is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Ort == ort & Geschlecht == geschlecht))  
  }
  else if(is.null(ort) && !is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Geschlecht == geschlecht))    
  }
  else if(!is.null(ort) && is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Ort == ort))    
  }
  
  if(platz_min == platz_max) {
    st_platz <- platz_min
  }
  else {
    st_platz <- paste(platz_min," - ",platz_max)
  }
  
  if(tmp_min == tmp_max) {
    st_tmp <- tmp_min
  }
  else {
    st_tmp <- paste(tmp_min," - ",tmp_max)
  }
  
  sub_title <- paste("Wettbewerb: ",ort,"; Platz: ",st_platz, "; Temp.: ",st_tmp, sep = "")
  
  plot_reg <- ggplot(selection, aes(y=S_KM_FN, TMP_MEAN_RND1)) + 
    geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
    labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
  plot(plot_reg)
  
  lm_reg_sum <- summary(lm(data = selection, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,reg_poly)))
  cat(paste(sub_title, "; Geschlecht: ", geschlecht, sep=""))
  print(lm_reg_sum)
  cat(paste("-----------------------------------------------------------------------------\n"))
}
## ----------------------------------------------------------------------------------------------------------------------------------
## ----------------------------------------------------------------------------------------------------------------------------------

create_reg_plots <- function(data_frame,reg_poly=1,tmp_min, tmp_max, platz_min, platz_max) {
  file_name = paste("reg_p",reg_poly,"_tmp",tmp_min,"_",tmp_max,"_platz",platz_min,"_",platz_max,sep = "")
  pdf(file = paste(file_name,".pdf",sep = ""), width = 6, height = 6)
  sink(file = paste(file_name,".txt",sep = ""), append = TRUE)
  for(g in unique(data_frame$Geschlecht)) {
    for(o in unique(data_frame$Ort)) {
      my_reg_skm_tmp(data_frame,reg_poly,tmp_min,tmp_max,platz_min,platz_max, o,g)
    }
  }
  sink()
  dev.off()
}
## ----------------------------------------------------------------------------------------------------------------------------------
my_reg_skm_tmp_2 <- function(data_frame,reg_poly=1,tmp_min=0, tmp_max=25, platz_min=1, platz_max=10, ort=NULL, geschlecht=NULL, skm="S_KM_FN") {
  if(!is.null(ort) && !is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Ort == ort & Geschlecht == geschlecht))  
  }
  else if(is.null(ort) && !is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Geschlecht == geschlecht))    
  }
  else if(!is.null(ort) && is.null(geschlecht)) {
    selection <- subset(data_frame, ((TMP_MEAN_RND1 >= tmp_min & TMP_MEAN_RND1 <= tmp_max) & 
                                       (Platz >= platz_min & Platz <= platz_max) & Ort == ort))    
  }
  
  if(platz_min == platz_max) {
    st_platz <- platz_min
  }
  else {
    st_platz <- paste(platz_min," - ",platz_max)
  }
  
  if(tmp_min == tmp_max) {
    st_tmp <- tmp_min
  }
  else {
    st_tmp <- paste(tmp_min," - ",tmp_max)
  }
  
  if(skm != "S_KM_FN") {
    if(is.null(ort)) {
      selection_chicago <- subset(selection, (Ort == "Chicago" & Jahr != 2012 & Jahr != 2013))
      selection_all <- subset(selection, (Ort!="Chicago"))
      selection <- rbind(selection_all, selection_chicago)
    } else if(ort == "Chicago") {
      selection <- subset(selection, (Jahr != 2012 & Jahr != 2013)) 
    }
  } 
  
  
  sub_title <- paste("Wettbewerb: ",ort,"; Platz: ",st_platz, "; Temp.: ",st_tmp,"; KM-Abschnitt: ", skm, sep = "")
  if(skm == 'S_KM_HM') {
    final_selection <- subset(selection, S_KM_HM != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_HM, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_HM ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_FN') {
    final_selection <- subset(selection, S_KM_FN != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_FN, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_FN ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_5') {
    final_selection <- subset(selection, S_KM_5 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_5, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_5 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_10') {
    final_selection <- subset(selection, S_KM_10 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_10, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_10 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_15') {
    final_selection <- subset(selection, S_KM_15 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_15, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_15 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_20') {
    final_selection <- subset(selection, S_KM_20 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_20, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_20 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_25') {
    final_selection <- subset(selection, S_KM_25 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_25, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_25 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_30') {
    final_selection <- subset(selection, S_KM_30 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_30, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_30 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_35') {
    final_selection <- subset(selection, S_KM_35 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_35, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_35 ~ poly(TMP_MEAN_RND1,reg_poly)))
  } else if(skm == 'S_KM_40') {
    final_selection <- subset(selection, S_KM_40 != 0)
    plot_reg <- ggplot(final_selection, aes(y=S_KM_40, TMP_MEAN_RND1)) + 
      geom_point() + geom_smooth(method = "lm", formula = y~poly(x,reg_poly)) +
      labs(title = paste("LM: Ergebnisse (",geschlecht,") ~ Temperatur (x^", reg_poly,")", sep = ""), x="Temperatur (°C)", y="Ergebnisse (in Sek.)", subtitle = sub_title)
    lm_reg_sum <- summary(lm(data = final_selection, formula = S_KM_40 ~ poly(TMP_MEAN_RND1,reg_poly)))
  }
  
 
  plot(plot_reg)
  cat(paste(sub_title, "; Geschlecht: ", geschlecht, sep=""))
  print(lm_reg_sum)
  cat(paste("-----------------------------------------------------------------------------\n"))
}

## ----------------------------------------------------------------------------------------------------------------------------------
## Plots erstellen (2)
create_reg_plots_2 <- function(data_frame,reg_poly=1,tmp_min, tmp_max, platz_min, platz_max, ort=c("Berlin","London","NewYork","Chicago","Tokyo")) {
  skms <- c("S_KM_5","S_KM_10","S_KM_15","S_KM_20","S_KM_HM","S_KM_25","S_KM_30","S_KM_35","S_KM_40","S_KM_FN")
  file_name = paste("reg_p",reg_poly,"_tmp",tmp_min,"_",tmp_max,"_platz",platz_min,"_",platz_max,sep = "")
  pdf(file = paste(file_name,".pdf",sep = ""), width = 8, height = 6)
  sink(file = paste(file_name,".txt",sep = ""), append = TRUE)
  for(g in unique(data_frame$Geschlecht)) {
    for(o in ort) {
      for(s in skms) {
        my_reg_skm_tmp_2(data_frame,reg_poly,tmp_min, tmp_max, platz_min, platz_max, ort=o, geschlecht=g, skm=s) 
      }
    }
  }
  sink()
  dev.off()
}
# Andere Reihenfolge der Plots: Plaetze bereits gesetzt
create_reg_plots_3 <- function(data_frame) {
  skms <- c("S_KM_5","S_KM_10","S_KM_15","S_KM_20","S_KM_HM","S_KM_25","S_KM_30","S_KM_35","S_KM_40","S_KM_FN")
  ort <- c("Berlin","London","NewYork","Chicago","Tokyo")
  plaetze <- c(10,5,3,1)
  file_name = paste("reg_p2_alle_jahre",sep = "")
  pdf(file = paste(file_name,".pdf",sep = ""), width = 8, height = 6)
  sink(file = paste(file_name,".txt",sep = ""), append = TRUE)
  for(g in unique(data_frame$Geschlecht)) {
    for(o in ort) {
      for(s in skms) {
        for(p in plaetze) {
          my_reg_skm_tmp_2(data_frame,reg_poly = 2,tmp_min = 1, tmp_max = 25, platz_min = 1, platz_max = p, ort=o, geschlecht=g, skm=s)  
        }
      }
    }
  }
  sink()
  dev.off()
}
## ----------------------------------------------------------------------------------------------------------------------------------
print_temps <- function(data_frame) {
  plaetze = c(1,3,5,10)
  sink(file = "temperatur_verteilung.txt", append = TRUE)
  for(g in unique(data_frame$Geschlecht)) {
    for(o in unique(data_frame$Ort)) {
      cat(paste("Ort: ", o, "; Geschlecht: ", g, "\n",sep = ""))
      cat(paste("-----------------------------------------\n", sep = ""))
      for(p in plaetze) {
        cat(paste("Platz <= ",p,sep = ""))
        print(table(subset(data_frame, (Ort==o & Geschlecht==g & Platz <= p), select = "TMP_MEAN_RND1", drop = TRUE)))
      }
      cat(paste("-----------------------------------------\n", sep = ""))
    }
  }
  cat(paste("Gesamt (table): -----------------------------------------\n", sep = ""))
  print(table(data_frame$TMP_MEAN_RND1, data_frame$Ort, data_frame$Geschlecht))
  sink()
}
## ----------------------------------------------------------------------------------------------------------------------------------
# Verlauf - alle Jahre pro Wettbewerbsort: Meter pro Sekunden / Streckenabschnitt
plot_paces <- function() {
  pdf(file = paste("plt_paces",".pdf",sep = ""), width = 9, height = 7)
  for(g in unique(df_ww5rs$Geschlecht)) {
    for(o in unique(df_ww5rs$Ort)) {
      if(o != "Chicago") {
        plot_pace <- ggplot(subset(df_ww5rs, (Geschlecht==g & Ort==o & Platz <=3 & SKM_TYP >= 1 & ZZ_INVALID == FALSE)), aes(x=SKM_TYP, y=SKM_PACE, group=Platz)) +
          geom_line(stat = "identity", position = "dodge", aes(color=Platz)) + 
          #geom_point() + 
          scale_color_continuous(breaks=seq(1,3,1)) +
          scale_x_continuous(breaks = seq(1,10,1), labels = c("5","10","15","20","21","25","30","35","40","42")) +
          scale_y_log10() +
          labs(y="Geschwindikeit (in m/s)", x="Kilometerabschnitt", title = paste("Pace in ",o," (",g,"): TOP-3", sep="")) + 
          #theme(legend.position = "none") +
          facet_wrap(~Jahr)
      }
      else if(o == "Chicago") { # Das Jahr 2012 und 2013 enthält inkorrekte Daten
        plot_pace <- ggplot(subset(df_ww5rs, (Geschlecht==g & Ort==o & Platz <=3 & SKM_TYP >= 1 & ZZ_INVALID == FALSE & Jahr!=2012 & Jahr!=2013)), aes(x=SKM_TYP, y=SKM_PACE, group=Platz)) +
          geom_line(stat = "identity", position = "dodge", aes(color=Platz)) + 
          #geom_point() + 
          scale_color_continuous(breaks=seq(1,3,1)) +
          scale_x_continuous(breaks = seq(1,10,1), labels = c("5","10","15","20","21","25","30","35","40","42")) +
          scale_y_log10() +
          labs(y="Geschwindikeit (in m/s)", x="Kilometerabschnitt", title = paste("Pace in ",o," (",g,"): TOP-3", sep="")) + 
          #theme(legend.position = "none") +
          facet_wrap(~Jahr)
      }
      print(plot_pace)
    }
  }
  dev.off()
}
## ----------------------------------------------------------------------------------------------------------------------------------