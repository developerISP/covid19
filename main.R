# Script di analisi open data sul covid19
# Author: Federico Dallo
# Data: 15 marzo 2020

# ------------------------------
# ------------------------------ HEADER DA MODIFICARE IN FUNZIONE DEI FILEPATH E OS 
# ------------------------------ HEADER DA MODIFICARE IN FUNZIONE DEI FILEPATH E OS
# ------------------------------ HEADER DA MODIFICARE IN FUNZIONE DEI FILEPATH E OS
# ------------------------------
# Pulizia dello spazio di lavoro:
rm(list = ls())
# ------------------------------
# Update data
pull_file <- "./get_update.sh"
setwd("/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/")
system(command = pull_file, intern = TRUE)
rm(pull_file)
# ------------------------------
# Move to working directory
setwd("/home/rosy/Documents/NicoMaffe/covid_wd/")
#
# Dati andamento nazionale:
today <- format(Sys.time(), "%Y%m%d")
target_dir <- "/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/dati-andamento-nazionale/"
target_file <- paste0(target_dir,"dpc-covid19-ita-andamento-nazionale.csv")
# target_file <- paste0(target_dir,"/COVID-19-geographic-disbtribution-worldwide-",today,".csv")
#
# ------------------------------
# ------------------------------ NON INSERIRE PATH O SYSTEM DEPENDENT VARIABLES OLTRE QUESTO PUNTO 
# ------------------------------ NON INSERIRE PATH O SYSTEM DEPENDENT VARIABLES OLTRE QUESTO PUNTO
# ------------------------------ NON INSERIRE PATH O SYSTEM DEPENDENT VARIABLES OLTRE QUESTO PUNTO
# ------------------------------
# Lettura dei dati
raw_data <- read.csv(file = target_file, header = TRUE, sep = ",")
raw_data$data <- as.POSIXct(raw_data$data, format="%Y-%m-%dT%H:%M:%S")
#--- Questi comandi non sono più necessari -------------------------------
#--- df_raw_data <- data.frame(DATE=raw_data$DateRep,
#---                           COUNTRY=raw_data$CountryExp, 
#---                           DAYCASES=raw_data$NewConfCases,
#---                           DATDEATHS=raw_data$NewDeaths)
#--- df_italy <- df_raw_data[is.element(df_raw_data$COUNTRY, "Italy"),]
#--- C'e' una discrepanza di un giorno rispetto ai dati del sole24ore
#--- https://lab24.ilsole24ore.com/coronavirus/
#--- df_italy$DATE <- df_italy$DATE - as.difftime(1, unit = "days")
#--- Questi comandi non sono più necessari -------------------------------
# df_italy <- data.frame(t(sapply(raw_data,c)))
# df_italy <- data.frame((sapply(raw_data,c)))
df_italy <- raw_data
#---------
# GUARITI GIORNALIERI
tmp_vec <- vector()
for (i in seq(1,length(df_italy$data),1)) {
  tmp_vec <- c(tmp_vec, (df_italy$dimessi_guariti[i] - df_italy$dimessi_guariti[i-1]) )
}
tmp_vec <- c(NA, tmp_vec) #aggiungo il differenziale nullo del primo giorno
df_italy$nuovi_guariti <- tmp_vec 
rm(tmp_vec)
#---------
# DECEDUTI GIORNALIERI
tmp_vec <- vector()
for (i in seq(1,length(df_italy$data),1)) {
  tmp_vec <- c(tmp_vec, (df_italy$deceduti[i] - df_italy$deceduti[i-1]) )
}
tmp_vec <- c(NA, tmp_vec) #aggiungo il differenziale nullo del primo giorno
df_italy$nuovi_deceduti <- tmp_vec 
rm(tmp_vec)
#---------
# TOTALI GIORNALIERI
tmp_vec <- df_italy$nuovi_attualmente_positivi+df_italy$nuovi_guariti+df_italy$nuovi_deceduti
df_italy$nuovi_totali <- tmp_vec 
rm(tmp_vec)
#---------
# GIORNI 
df_italy$days <- seq(1,length(df_italy$data),1)
#
# ------------------------------
# ------------------------------
# DA QUESTO MOMENTO IL DATASET DF_ITALY È COMPLETO LA
# PRIMA RIGA CONTIENE NA E QUINDI IN ALCUNE MANIPOLAZIONI
# DEI DATI IN SEGUITO POTRÀ VENIR RIMOSSA/RIAGGIUNTA LA PRIMA
# RIGA.
# ------------------------------
# ------------------------------
# PLOTS
# ------------------------------
# ------------------------------
#daterange=c(as.POSIXct("2020-02-20"), df_italy$DATE[2])
daterange=c(as.POSIXct(df_italy$data[1]), as.POSIXct(df_italy$data[length(df_italy$data)] +3600*24*5))
# ------------------------------
# RIMOZIONE
tmp_row <- df_italy[1,]
df_italy <- df_italy[-1,]
#######################################################################
# NUOVI CASI GIORNALIERI ##############################################
#######################################################################
plot(df_italy$data, df_italy$nuovi_totali, 
     xaxt = "n",
     xlim = daterange,
     ylim = c(min(df_italy$nuovi_totali), max(df_italy$nuovi_totali)+0.5*max(df_italy$nuovi_totali) ),
     main = "Nuovi casi giornalieri (totali = nuovi_attualmente_positivi + nuovi_guariti + nuovi_deceduti)",
     xlab = "",
     ylab = "Counts",
     type = "o",
     col = "firebrick",
     pch = 19,
     cex = 1.2,
     lwd = 3)
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%b %d", las=2)
abline(v = as.POSIXct("2020-03-08"))
text("Decreto zona rossa", x = as.POSIXct("2020-03-08") + 3600*6, y = 2000, srt = 90, adj = 0.5)
# ------------------------------
# modello esponenziale
exp.mod.nuoviTotali <- lm(log(df_italy$nuovi_totali)~df_italy$days)
timevalues <- data.frame(data=seq(1, length(df_italy$days), 0.1))
nuovi_totali_predict <- exp(predict(exp.mod.nuoviTotali, list(days=timevalues) ))
lines(df_italy$data, nuovi_totali_predict,
      col = 1,
      lwd = 3)
#----------
# qualità del modello
# chi-squared test
exp.mod.nuoviTotali.ChiSq.res <- chisq.test(x = df_italy$nuovi_totali,
                                            y = nuovi_totali_predict)
# exp.mod.nuoviTotali.ChiSqres <- chisq.test(x = df_italy$nuovi_totali, 
#                                            p = nuovi_totali_predict, 
#                                            rescale.p = TRUE)
# points(df_italy$data, exp.mod.nuoviTotali.ChiSq.res$expected,
#        col="navy", 
#        pch= 19,
#        cex= 1.2)
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.nuoviTotali$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.nuoviTotali$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.nuoviTotali)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared (0.05) p-value:",
                  format(round(exp.mod.nuoviTotali.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = df_italy$data[1],
     y = (max(df_italy$nuovi_totali, na.rm = TRUE)-min(df_italy$nuovi_totali, na.rm = TRUE))*9/10,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
# RIAGGIUNTA --------------------------
df_italy <- rbind(tmp_row, df_italy)  #
rm(tmp_row)                           #
#--------------------------------------
# - Previsioni -
pred <- data.frame(DAYS = seq(1,length(df_italy$days)+5,1))
pred$pred_new_total_predict <- exp(pred$DAYS*exp.mod.nuoviTotali$coefficients[2] + exp.mod.nuoviTotali$coefficients[1] )
plot(pred$DAYS, pred$pred_new_total_predict, type = "l",
     col= 1,
     lwd= 3,
     main = "Previsione a 5 giorni dei casi totali gionalieri",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Conteggi")
points(df_italy$days, df_italy$nuovi_totali, 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]  # as.POSIXct("2020-03-08") + 3600*6
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, y = 2000, srt = 90, adj = 0.5)
text(paste0(as.character(lastDay), " ",format(round(pred$pred_new_total_predict[length(pred$pred_new_total_predict)]), nsmall = 2)), 
     x = pred$DAYS[length(pred$DAYS)], 
     y = pred$pred_new_total_predict[length(pred$pred_new_total_predict)],adj = 1)
totalNew <- sum(pred$pred_new_total_predict)
text(paste0("Contagiati totali= ",format(round(totalNew), nsmall = 2)), 
     x = pred$DAYS[1], 
     y = pred$pred_new_total_predict[length(pred$pred_new_total_predict)], 
     adj = 0)
# --
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.nuoviTotali$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.nuoviTotali$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.nuoviTotali)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared p-value:",
                  format(round(exp.mod.nuoviTotali.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = pred$DAYS[1],
     y = (max(pred$pred_new_total_predict, na.rm = TRUE)-min(pred$pred_new_total_predict, na.rm = TRUE))*6/10,
     pos = 4 
)
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
# --------------------------##########################################################
# MORTI GIORNALIERE         ##########################################################
# RIMOZIONE-----------------##########################################################
tmp_row <- df_italy[1,]    #
df_italy <- df_italy[-1,]  #
#---------------------------
plot(df_italy$data, df_italy$nuovi_deceduti,
     xaxt = "n",
     xlim = daterange,
     ylim = c(min(df_italy$nuovi_deceduti), max(df_italy$nuovi_deceduti)+0.5*max(df_italy$nuovi_deceduti) ),
     main = "Deceduti giornalieri",
     xlab = "",
     ylab = "Counts",
     type = "o",
     col = "firebrick",
     pch = 19,
     cex = 1.2,
     lwd = 3)
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%b %d", las=2)
abline(v = as.POSIXct("2020-03-08"))
text("Decreto zona rossa", x = as.POSIXct("2020-03-08") + 3600*6, y = (max(df_italy$nuovi_deceduti)-min(df_italy$nuovi_deceduti))/2, srt = 90, adj = 0.5)
# RIAGGIUNTA --------------------------
df_italy <- rbind(tmp_row, df_italy)  #
rm(tmp_row)                           #
#--------------------------------------
# modello esponenziale
# exp.mod.nuoviMorti <- lm(log(df_italy$nuovi_deceduti)~df_italy$days)
# df_italy$nuovi_deceduti[1] <- df_italy$nuovi_deceduti[2]
#last_tmp_value <- df_italy$nuovi_deceduti[1] 
#df_italy$nuovi_deceduti[1] <- NA
exp.mod.nuoviMorti <- lm( log(df_italy$nuovi_deceduti) ~ df_italy$days)
#df_italy$nuovi_deceduti[1] <- last_tmp_value
#rm(last_tmp_value)
timevalues <- data.frame(data=seq(1, length(df_italy$days), 0.1))
nuovi_morti_predict <- exp(predict(exp.mod.nuoviMorti, list(days=timevalues) ))
lines(df_italy$data, nuovi_morti_predict,
      col = 1,
      lwd = 3)
# qualità del modello
# chi-squared test
exp.mod.nuoviMorti.ChiSq.res <- chisq.test(x = df_italy$nuovi_deceduti,
                                            y = nuovi_morti_predict)
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.nuoviMorti$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.nuoviMorti$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.nuoviMorti)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared p-value:",
                  format(round(exp.mod.nuoviMorti.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = df_italy$data[1],
     y = (max(df_italy$nuovi_deceduti, na.rm = TRUE)-min(df_italy$nuovi_deceduti, na.rm = TRUE))*9/10,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
# - Previsioni -
#pred <- data.frame(DAYS = seq(1,length(df_italy$days)+5,1))
pred$pred_new_death_predict <- exp(pred$DAYS*exp.mod.nuoviMorti$coefficients[2] + exp.mod.nuoviMorti$coefficients[1] )
plot(pred$DAYS, pred$pred_new_death_predict, type = "l",
     col= 1,
     lwd= 3,
     main = "Previsione a 5 giorni delle morti gionaliere",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Conteggi")
points(df_italy$days, df_italy$nuovi_deceduti, 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]   # as.POSIXct("2020-03-08") + 3600*6
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred$pred_new_death_predict)-min(pred$pred_new_death_predict))/2, srt = 90, adj = 0.5)
text(paste0(as.character(lastDay), " ",format(round(pred$pred_new_death_predict[length(pred$pred_new_death_predict)]), nsmall = 2)), 
     x = pred$DAYS[length(pred$DAYS)], 
     y = pred$pred_new_death_predict[length(pred$pred_new_death_predict)], 
     adj = 1)
totalDeath <- sum(pred$pred_new_death_predict)
text(paste0("Morti totali= ",format(round(totalDeath), nsmall = 2)), 
     x = pred$DAYS[1], 
     y = pred$pred_new_death_predict[length(pred$pred_new_death_predict)], 
     adj = 0)
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.nuoviMorti$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.nuoviMorti$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.nuoviMorti)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared p-value:",
                  format(round(exp.mod.nuoviMorti.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = pred$DAYS[1],
     y = (max(pred$pred_new_death_predict, na.rm = TRUE)-min(pred$pred_new_death_predict, na.rm = TRUE))*6/10,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
# --------------------------------------------------------------############################
# ANDAMENTO VARIAZIONE PERCENTUALE GIORNALIERA DEI CONTAGIATI   ############################
# --------------------------------------------------------------############################
tmp_vec <- vector() # creo un vettore per calcolare la variazione percentuale giornaliera di contagiati
for (i in seq(1, length(df_italy$data) - 1, 1) ){
  tmp_vec <- c(tmp_vec, (df_italy$totale_casi[i+1]/df_italy$totale_casi[i] - 1)*100 )
}
tmp_vec <- c(NA, tmp_vec) # <- inserisco un NA in testa che si riferisce al day1 == 24 febbraio
df_italy$daily_var_perc <- tmp_vec # <- inserisco il vettore nel dataset dei dati italiani
rm(tmp_vec)
#---
# RIMOZIONE-----------------
tmp_row <- df_italy[1,]    #
df_italy <- df_italy[-1,]  #
#---------------------------
#daterange=c(as.POSIXct(df_italy$data[2]), as.POSIXct(df_italy$data[length(df_italy$data)]))
daterange=c(as.POSIXct(df_italy$data[1]), as.POSIXct(df_italy$data[length(df_italy$data)] +3600*24*10))
plot(df_italy$data, df_italy$daily_var_perc,
     ylim = c(0,max(df_italy$daily_var_perc)+0.2*max(df_italy$daily_var_perc)),
     type = "h",
     lwd = 10,
     xaxt = "n",
     xlim = daterange,
     main = "Variazione percentuale giornaliera",
     xlab = "",
     ylab = "%",
     col="firebrick",
)
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%b %d", las=2)
abline(v = as.POSIXct("2020-03-08"))
text("Decreto zona rossa", x = as.POSIXct("2020-03-08") + 3600*6, 
     y = (max(df_italy$daily_var_perc)-min(df_italy$daily_var_perc))/1.2, srt = 90, adj = 0.5)
# RIAGGIUNTA --------------------------
df_italy <- rbind(tmp_row, df_italy)  #
rm(tmp_row)                           #
#--------------------------------------
# MODELLO ESPONENZIALE DAILY VAR PERC
#df_italy$daily_var_perc[1] <- df_italy$daily_var_perc[2]
# togliamo la prima riga e la rimettiamo dopo aver fatto girare il modello
# last_tmp_value <- df_italy$daily_var_perc[1]
# df_italy$daily_var_perc[1] <- NA
exp.mod.dailyVarPerc <- lm( log(df_italy$daily_var_perc) ~ df_italy$days)
# df_italy$daily_var_perc[1] <- last_tmp_value
# rm(last_tmp_value)
timevalues <- data.frame(data=seq(1, length(df_italy$days), 0.1))
var_perc_giorn_predict <- exp(predict(exp.mod.dailyVarPerc, list(days=timevalues) ))
lines(df_italy$data, var_perc_giorn_predict,
      col = 1,
      lwd = 3)
# qualità del modello
# chi-squared test
exp.mod.dailyVarPerc.ChiSq.res <- chisq.test(x = df_italy$daily_var_perc,
                                             y = var_perc_giorn_predict)
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.dailyVarPerc$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.dailyVarPerc$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.dailyVarPerc)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared (0.05) p-value:",
                  format(round(exp.mod.dailyVarPerc.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = df_italy$data[length(df_italy$data)],
     y = (max(df_italy$daily_var_perc, na.rm = TRUE)-min(df_italy$daily_var_perc, na.rm = TRUE))*9/10,
     pos = 4 
     )
grid(NA, NULL, lwd = 2)
rm(tmp_to_print)
# previsioni a 5 giorni
pred$pred_dailyVarPerc <- exp(pred$DAYS*exp.mod.dailyVarPerc$coefficients[2] + exp.mod.dailyVarPerc$coefficients[1] )
plot(pred$DAYS, pred$pred_dailyVarPerc, type = "l",
     ylim = c(0,max(df_italy$daily_var_perc, na.rm = TRUE)+0.2*max(df_italy$daily_var_perc, na.rm = TRUE)),
     xlim = c(2,max(df_italy$days)+10), # <- da 2 per essere consistenti con "datarange" del graf. precedente
     col= 1,
     lwd= 3,
     main = "Previsione a 5 giorni della variazione percentuale gionaliera",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "%")
points(df_italy$days[2:length(df_italy$data)], df_italy$daily_var_perc[2:length(df_italy$daily_var_perc)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
# tabella qualità
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.dailyVarPerc$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.dailyVarPerc$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.dailyVarPerc)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared (0.05) p-value:",
                  format(round(exp.mod.dailyVarPerc.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = df_italy$days[length(df_italy$days)],
     y = (max(df_italy$daily_var_perc, na.rm = TRUE)-min(df_italy$daily_var_perc, na.rm = TRUE))*6/10,
     pos = 4 
     )
rm(tmp_to_print)
# valori stimati
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]   # as.POSIXct("2020-03-08") + 3600*6
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred$pred_dailyVarPerc)-min(pred$pred_dailyVarPerc))*1.2, srt = 90, adj = 0.5)
points(pred$DAYS[length(pred$DAYS)], pred$pred_dailyVarPerc[length(pred$pred_dailyVarPerc)],
       col="navy",
       pch=17,
       cex=1.2)
text(paste0(as.character(format(lastDay, format = "%Y-%m-%d")), " ",
            format(round(pred$pred_dailyVarPerc[length(pred$pred_dailyVarPerc)]), nsmall = 2), "%"), 
     x = pred$DAYS[length(pred$DAYS)], 
     y = 0.8*pred$pred_dailyVarPerc[length(pred$pred_dailyVarPerc)], 
     pos = 2)
#----------
# calcolo dei contagiati totali a partire dal modello esponenziale decrescente
# per la variazione percentuale giornaliera.
tmp_vec <- vector()
tmp_start_cases <- df_italy$totale_casi[1]
tmp_vec[1] <- tmp_start_cases # <- il primo valore della serie e' il valore "vero" dei contagiati del 24 aprile
for (i in seq(2,length(pred$DAYS),1)){
  tmp_vec[i] <- tmp_vec[i-1] + tmp_vec[i-1]*pred$pred_dailyVarPerc[i]/100
}
pred$pred_new_total_from_dailyVarPerc <- tmp_vec
rm(tmp_vec)
rm(tmp_start_cases)
# text(paste0("Prev.tot.cont.= ",format(round(pred$pred_new_total_from_dailyVarPerc[length(pred$pred_new_total_from_dailyVarPerc)]), nsmall = 2)),
#      x = max(pred$DAYS), 
#      y = max(df_italy$daily_var_perc), 
#      adj = 1)
tmp_val <- pred$DAYS[length(pred$DAYS)]
tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
abline(v = tmp_val)
text(paste0("Stima del totale contagiati al ",
            format(tmp_day, format = "%Y-%m-%d"),
            ", Nr: ",
            format(round(pred$pred_new_total_from_dailyVarPerc[length(pred$pred_new_total_from_dailyVarPerc)]), nsmall = 0)
),
x = max(pred$DAYS), 
y = max(df_italy$daily_var_perc, na.rm = TRUE), 
adj = 1
)
grid(NULL, NULL, lwd = 2)
rm(tmp_val)
rm(tmp_day)
# text(bquote(atop("first line", 
#                  "second line" ~ x ^ 2)), 
#      x = 24, y = 40)
# tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri), "DAYS"]
# tmp_day <- df_italy$data[1] + 3600*24*tmp_val

# ----------------------------------------------######################################
# PREVISIONE LUNGO PERIODO ("days_tot" giorni)--######################################
# ----------------------------------------------######################################
days_tot <- 200 # prendiamo il dataset italiano e stimiamo cosa accadrà a 200 giorni
pred_lt <- data.frame(DAYS = seq(1,length(df_italy$days)+days_tot,1))
pred_lt$pred_dailyVarPerc <- exp(pred_lt$DAYS*exp.mod.dailyVarPerc$coefficients[2] + exp.mod.dailyVarPerc$coefficients[1] )
# calcolo dei contagiati totali con il modello di decadimento esponenziale 
# per la variazione percentuale giornaliera
tmp_vec <- vector()
tmp_start_cases <- df_italy$totale_casi[1]
tmp_vec[1] <- tmp_start_cases
for (i in seq(2,length(pred_lt$DAYS),1)){
  tmp_vec[i] <- tmp_vec[i-1] + tmp_vec[i-1]*pred_lt$pred_dailyVarPerc[i]/100
}
pred_lt$pred_new_total_from_dailyVarPerc <- tmp_vec
rm(tmp_vec)
rm(tmp_start_cases)
# ---------
#-- SPOSTIAMO IL GRAFICO DOPO LA DERIVATA PER AVERE IL MASSIMO
#-- plot(pred_lt$DAYS, pred_lt$pred_new_total_from_dailyVarPerc, type = "l",
#--      col= 1,
#--      lwd= 1,
#--      main = "Previsione totale contagiati",
#--      xlab = "Giorni a partire dal 24 febbraio 2020",
#--      ylab = "Counts")
#-- points(df_italy$days[2:length(df_italy$data)], df_italy$totale_casi[2:length(df_italy$totale_casi)], 
#--        col="firebrick",
#--        type = "o",
#--        pch = 19,
#--        cex = 1.2,
#--        lwd = 3)
#-- abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
#-- text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
#--      y = (max(pred_lt$pred_new_total_from_dailyVarPerc)-min(pred_lt$pred_new_total_from_dailyVarPerc))/2, srt = 90, adj = 0.5)
#-- points(pred_lt$DAYS[200], pred_lt$pred_new_total_from_dailyVarPerc[200],
#--        col="navy", 
#--        pch = 19,
#--        cex = 1.5)
#-- text(paste0(format(df_italy$data[1]+3600*24*199, format = "%Y-%m-%d")," ", 
#--             format(round(pred_lt$pred_new_total_from_dailyVarPerc[200]), nsmall = 0) ), 
#--      x = pred_lt$DAYS[200],
#--      y = pred_lt$pred_new_total_from_dailyVarPerc[200],
#--      adj = 1,pos = 1)
# -----------------------------------------
# derivata, ovvero i contagiati giornalieri
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_new_total_from_dailyVarPerc[i+1] - pred_lt$pred_new_total_from_dailyVarPerc[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_cont_giornalieri <- tmp_vec
rm(tmp_vec)
#pred_lt$pred_cont_giornalieri[1] <- pred_lt$pred_cont_giornalieri[2]
# RIMOZIONE-----------------
tmp_row <- pred_lt[1,]    #
pred_lt <- pred_lt[-1,]   #
#
# PLOT
plot(pred_lt$DAYS, pred_lt$pred_cont_giornalieri, type = "l",
     main = "Stima a lungo periodo dei nuovi contagiati giornalieri",
     lwd=1.5,
     pch=19,
     cex=1.2,
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
# RIAGGIUNTA --------------------------
pred_lt <- rbind(tmp_row, pred_lt)    #
rm(tmp_row)                           #
# -------------------------------------
points(df_italy$days, df_italy$nuovi_totali, 
       col="firebrick",
       pch=19,
       cex=1.2)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)-min(pred_lt$pred_cont_giornalieri, na.rm = TRUE))/2, srt = 90, adj = 0.5)
abline(v = pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"],
       col="navy",
       lwd=3)
# PUNTI SIGNIFICATIVI
tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
points(pred_lt$DAYS[200], pred_lt$pred_cont_giornalieri[200],
       col="navy", 
       pch = 19,
       cex = 1.5)
text(paste0(format(df_italy$data[1]+3600*24*199, format = "%Y-%m-%d")," New: ",format(round(pred_lt$pred_cont_giornalieri[200]), nsmall = 0)), 
     x = pred_lt$DAYS[200],
     y = pred_lt$pred_cont_giornalieri[200],
     pos = 3)
#-
points(tmp_val, pred_lt$pred_cont_giornalieri[tmp_val],
       col = "red",
       pch = 17,
       cex = 1.5)
text(paste0(" ", format(tmp_day, format = "%Y-%m-%d")), x = tmp_val, 
     y = (max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)-min(pred_lt$pred_cont_giornalieri, na.rm = TRUE))/2,
     adj = 0)
text(paste0(" Max= ", format(round(max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)), nsmall = 0)), 
     x = tmp_val,
     y = max(pred_lt$pred_cont_giornalieri, na.rm = TRUE),
     adj = 0)
# tabella qualità
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.dailyVarPerc$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.dailyVarPerc$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.dailyVarPerc)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared (0.05) p-value:",
                  format(round(exp.mod.dailyVarPerc.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 150,
     y = max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)/2,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
#--
#-- GRAFICO DELLA CRESCITA DEL TOTALE DEI CONTAGIATI
plot(pred_lt$DAYS, pred_lt$pred_new_total_from_dailyVarPerc, type = "l",
     col= 1,
     lwd= 1,
     main = "Previsione totale contagiati",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[2:length(df_italy$data)], df_italy$totale_casi[2:length(df_italy$totale_casi)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_new_total_from_dailyVarPerc)-min(pred_lt$pred_new_total_from_dailyVarPerc))/2, srt = 90, adj = 0.5)
# PUNTI SIGNIFICATIVI
points(pred_lt$DAYS[200], pred_lt$pred_new_total_from_dailyVarPerc[200],
       col="navy", 
       pch = 19,
       cex = 1.5)
text(paste0(format(df_italy$data[1]+3600*24*199, format = "%Y-%m-%d")," Tot: ", 
            format(round(pred_lt$pred_new_total_from_dailyVarPerc[200]), nsmall = 0) ), 
     x = pred_lt$DAYS[200],
     y = pred_lt$pred_new_total_from_dailyVarPerc[200],
     adj = 1,pos = 1)
points(tmp_val, pred_lt$pred_new_total_from_dailyVarPerc[tmp_val],
       col = "red",
       pch = 17,
       cex = 1.5)
text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
     y = pred_lt$pred_new_total_from_dailyVarPerc[tmp_val],
     pos = 4)
text(paste0("   Max= ", format(round(max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)), nsmall = 0)), 
     x = tmp_val,
     y = pred_lt$pred_new_total_from_dailyVarPerc[tmp_val],
     pos = 3) 
# tabella qualità
#----------
tmp_to_print <- c("Modello esponenziale",
                  paste0("Interc.: ", format(round(exp.mod.dailyVarPerc$coefficients[1],digits = 4),nsmall = 2),
                         ", Slope: ", format(round(exp.mod.dailyVarPerc$coefficients[2],digits = 4),nsmall = 2) ),
                  "Model adj R2:", 
                  format(round(summary(exp.mod.dailyVarPerc)$adj.r.squared, digits = 4), nsmall = 2),
                  "Chi-squared (0.05) p-value:",
                  format(round(exp.mod.dailyVarPerc.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 150,
     y = max(pred_lt$pred_new_total_from_dailyVarPerc, na.rm = TRUE)/2,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
#grid(NA, 5, lwd = 2)
rm(tmp_to_print)
rm(tmp_val)
rm(tmp_day)

### MORE MODELS LOGISTIC
### MORE MODELS LOGISTIC
### MORE MODELS LOGISTIC

# Logistic sigmoid per totale deceduti 
logistic.mod.tot.death.getInitial <- getInitial(deceduti ~ SSlogis(days, Asym, xmid, scal), data = df_italy)
logistic.mod.fit.tot.death <- nls(deceduti ~ SSlogis(days, Asym, 
                                           xmid, 
                                           scal), data = df_italy)
# aggiungiamo previsioni a pred_lt
deceduti_logistic_predict <- predict(logistic.mod.fit.tot.death, df_italy$days)

# qualità del modello
# chi-squared test
logistic.mod.fit.tot.death.ChiSq.res <- chisq.test(x = df_italy$deceduti,
                                                   y = deceduti_logistic_predict)
# PLOT DECEDUTI TOTALI
plot(df_italy$days, deceduti_logistic_predict, type = "l",
     col= 1,
     lwd= 1,
     main = "Previsione totale morti",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$days)], df_italy$deceduti[1:length(df_italy$days)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(df_italy$deceduti)-min(df_italy$deceduti))/2, srt = 90, adj = 0.5)
# tabella qualità
tmp_to_print <- c("Modello Logistico",
                  paste0("Estimate      Std. Error      t value      Pr(>|t|)"),
                  paste0("asym: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[1,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("xmid: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[2,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("scal: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[3,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("Ach.conv.tol.: ", (summary(logistic.mod.fit.tot.death)$convInfo)$finTol),
                  "Chi-squared (0.05) p-value:",
                  format(round(logistic.mod.fit.tot.death.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 1,
     y = max(df_italy$deceduti, na.rm = TRUE)/2,
     pos = 4 
     )
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
#----------
# ora proviamo una stima di lungo perido in pred_lt
pred_lt$pred_death_logistic <- SSlogis(input = pred_lt$DAYS, 
                                       Asym = (summary(logistic.mod.fit.tot.death)$param[1]),
                                       xmid = (summary(logistic.mod.fit.tot.death)$param[2]), 
                                       scal = (summary(logistic.mod.fit.tot.death)$param[3]) 
                                       )
plot(pred_lt$DAYS, pred_lt$pred_death_logistic, type = "l",
     xlim = c(0,100),
     col= 1,
     lwd= 1,
     main = "Previsione totale deceduti",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$data)], df_italy$deceduti[1:length(df_italy$data)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_death_logistic)-min(pred_lt$pred_death_logistic))/2, srt = 90, adj = 0.5)
# tabella qualità
tmp_to_print <- c("Modello Logistico",
                  paste0("Estimate      Std. Error      t value      Pr(>|t|)"),
                  paste0("asym: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[1,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("xmid: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[2,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("scal: ", paste0(format(round(summary(logistic.mod.fit.tot.death)$param[3,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("Ach.conv.tol.: ", (summary(logistic.mod.fit.tot.death)$convInfo)$finTol),
                  "Chi-squared (0.05) p-value:",
                  format(round(logistic.mod.fit.tot.death.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 40,
     y = max(pred_lt$pred_death_logistic, na.rm = TRUE)/2,
     pos = 4 
)
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)

# derivata, ovvero i morti giornalieri
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_death_logistic[i+1] - pred_lt$pred_death_logistic[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_death_logistic_giornalieri <- tmp_vec
rm(tmp_vec)

# PLOT DECEDUTI GIORNALIERI
plot(pred_lt$DAYS, pred_lt$pred_death_logistic_giornalieri, type = "l",
     xlim = c(0,100),
     ylim = c(0, max(df_italy$nuovi_deceduti, na.rm = TRUE)),
     col= 1,
     lwd= 1,
     main = "Previsione deceduti giornalieri",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$data)], df_italy$nuovi_deceduti[1:length(df_italy$data)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_death_logistic_giornalieri, na.rm = TRUE)
          -min(pred_lt$pred_death_logistic_giornalieri, na.rm = TRUE))/2, srt = 90, adj = 0.5)
# PUNTI SIGNIFICATIVI
tmp_val <- pred_lt[pred_lt$pred_death_logistic_giornalieri == max(pred_lt$pred_death_logistic_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
points(pred_lt$DAYS[80], pred_lt$pred_death_logistic_giornalieri[80],
       col="navy", 
       pch = 19,
       cex = 1.5)
text(paste0(format(df_italy$data[1]+3600*24*79, format = "%Y-%m-%d")," New: ", 
            format(round(pred_lt$pred_death_logistic_giornalieri[80]), nsmall = 0) ), 
     x = pred_lt$DAYS[80],
     y = pred_lt$pred_death_logistic_giornalieri[80],
     adj = 1,pos = 1)
points(tmp_val, pred_lt$pred_death_logistic_giornalieri[tmp_val],
       col = "red",
       pch = 17,
       cex = 1.5)
text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
     y = pred_lt$pred_death_logistic_giornalieri[tmp_val],
     pos = 4)
text(paste0("   Max= ", format(round(max(pred_lt$pred_death_logistic_giornalieri, na.rm = TRUE)), nsmall = 0)), 
     x = tmp_val,
     y = pred_lt$pred_death_logistic_giornalieri[tmp_val],
     pos = 3)
rm(tmp_val)
rm(tmp_day)

# --- CONTAGIATI TOTALI MODELLO LOGISTICO
# Logistic sigmoid per totale deceduti 
logistic.mod.tot.casi.getInitial <- getInitial(totale_casi ~ SSlogis(days, Asym, xmid, scal), data = df_italy)
logistic.mod.fit.tot.casi <- nls(totale_casi ~ SSlogis(days, Asym, xmid, scal), data = df_italy)
# previsione 
totale_casi_logistic_predict <- predict(logistic.mod.fit.tot.casi, df_italy$days)
# qualità del modello
# chi-squared test
logistic.mod.fit.tot.casi.ChiSq.res <- chisq.test(x = df_italy$totale_casi,
                                                  y = deceduti_logistic_predict)
# ora proviamo una stima di lungo perido in pred_lt
pred_lt$pred_totale_casi_logistic <- SSlogis(input = pred_lt$DAYS,
                                             Asym = (summary(logistic.mod.fit.tot.casi)$param[1]),
                                             xmid = (summary(logistic.mod.fit.tot.casi)$param[2]),
                                             scal = (summary(logistic.mod.fit.tot.casi)$param[3]) 
)
# DERIVATA, NUOVI TOTALI GIORNALIERI
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_totale_casi_logistic[i+1] - pred_lt$pred_totale_casi_logistic[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_logistic_nuovi_totali_giornalieri <- tmp_vec
rm(tmp_vec)

#PLOT TOTALI 
plot(pred_lt$DAYS, pred_lt$pred_totale_casi_logistic, type = "l",
     xlim = c(0,100),
     col= 1,
     lwd= 1,
     main = "Previsione totale contagiati",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$data)], df_italy$totale_casi[1:length(df_italy$data)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_totale_casi_logistic)-min(pred_lt$pred_totale_casi_logistic))/2, srt = 90, adj = 0.5)
# tabella qualità
tmp_to_print <- c("Modello Logistico",
                  paste0("Estimate      Std. Error      t value      Pr(>|t|)"),
                  paste0("asym: ", paste0(format(round(summary(logistic.mod.fit.tot.casi)$param[1,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("xmid: ", paste0(format(round(summary(logistic.mod.fit.tot.casi)$param[2,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("scal: ", paste0(format(round(summary(logistic.mod.fit.tot.casi)$param[3,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("Ach.conv.tol.: ", (summary(logistic.mod.fit.tot.casi)$convInfo)$finTol),
                  "Chi-squared (0.05) p-value:",
                  format(round(logistic.mod.fit.tot.casi.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 40,
     y = max(pred_lt$pred_totale_casi_logistic, na.rm = TRUE)/2,
     pos = 4 
)
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)

# PLOT NUOVI CASI GIORNALIERI
plot(pred_lt$DAYS, pred_lt$pred_logistic_nuovi_totali_giornalieri, type = "l",
     xlim = c(0,100),
     ylim = c(0, max(df_italy$nuovi_totali, na.rm = TRUE)),
     col= 1,
     lwd= 1,
     main = "Previsione nuovi casi giornalieri",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$data)], df_italy$nuovi_totali[1:length(df_italy$data)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE)
          -min(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE))/2, srt = 90, adj = 0.5)
# PUNTI SIGNIFICATIVI
tmp_val <- pred_lt[pred_lt$pred_logistic_nuovi_totali_giornalieri == max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
points(pred_lt$DAYS[80], pred_lt$pred_logistic_nuovi_totali_giornalieri[80],
       col="navy", 
       pch = 19,
       cex = 1.5)
text(paste0(format(df_italy$data[1]+3600*24*79, format = "%Y-%m-%d")," New: ", 
            format(round(pred_lt$pred_logistic_nuovi_totali_giornalieri[80]), nsmall = 0) ), 
     x = pred_lt$DAYS[80],
     y = pred_lt$pred_logistic_nuovi_totali_giornalieri[80],
     adj = 1,pos = 1)
points(tmp_val, pred_lt$pred_logistic_nuovi_totali_giornalieri[tmp_val],
       col = "red",
       pch = 17,
       cex = 1.5)
text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
     y = pred_lt$pred_logistic_nuovi_totali_giornalieri[tmp_val],
     pos = 4)
text(paste0("   Max= ", format(round(max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE)), nsmall = 0)), 
     x = tmp_val,
     y = pred_lt$pred_logistic_nuovi_totali_giornalieri[tmp_val],
     pos = 3)
rm(tmp_val)
rm(tmp_day)

### MORE MODELS GOMPERTZ
### MORE MODELS GOMPERTZ
### MORE MODELS GOMPERTZ
# Gompertz fitting per totale deceduti 
gompertz.mod.tot.death.getInitial <- getInitial(deceduti ~ SSgompertz(days, Asym, b2, b3), data = df_italy)
gompertz.mod.fit.tot.death <- nls(deceduti ~ SSgompertz(days, Asym, b2, b3), data = df_italy)
# aggiungiamo previsioni a pred_lt
deceduti_gompertz_predict <- predict(gompertz.mod.fit.tot.death, df_italy$days)
# qualità del modello
# chi-squared test
gompertz.mod.fit.tot.death.ChiSq.res <- chisq.test(x = df_italy$deceduti, y = deceduti_gompertz_predict)

# PLOT DECEDUTI TOTALI
plot(df_italy$days, deceduti_gompertz_predict, type = "l",
     col= 1,
     lwd= 1,
     main = "Previsione totale morti, Gompertz",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$days)], df_italy$deceduti[1:length(df_italy$days)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(df_italy$deceduti)-min(df_italy$deceduti))/2, srt = 90, adj = 0.5)
# tabella qualità
tmp_to_print <- c("Modello Gompertz",
                  paste0("Estimate      Std. Error      t value      Pr(>|t|)"),
                  paste0("asym: ", paste0(format(round(summary(gompertz.mod.fit.tot.death)$param[1,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("b2: ", paste0(format(round(summary(gompertz.mod.fit.tot.death)$param[2,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("b3: ", paste0(format(round(summary(gompertz.mod.fit.tot.death)$param[3,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                  paste0("Ach.conv.tol.: ", (summary(gompertz.mod.fit.tot.death)$convInfo)$finTol),
                  "Chi-squared (0.05) p-value:",
                  format(round(gompertz.mod.fit.tot.death.ChiSq.res$p.value, digits = 4), nsmall = 2))
text(paste(tmp_to_print, collapse="\n"),
     x = 1,
     y = max(df_italy$deceduti, na.rm = TRUE)/2,
     pos = 4 
)
grid(NULL, NULL, lwd = 2)
rm(tmp_to_print)
#----------
# ora proviamo una stima di lungo perido in pred_lt
pred_lt$pred_death_gompertz <- SSgompertz(x = pred_lt$DAYS,
                                          Asym = (summary(gompertz.mod.fit.tot.death)$param[1]),
                                          b2 =   (summary(gompertz.mod.fit.tot.death)$param[2]),
                                          b3 =   (summary(gompertz.mod.fit.tot.death)$param[3]) 
)
# DERIVATA, NUOVI DECEDUTI GIORNALIERI, GOMPERTZ
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_death_gompertz[i+1] - pred_lt$pred_death_gompertz[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_gompertz_nuovi_morti_giornalieri <- tmp_vec
rm(tmp_vec)
#
plot(pred_lt$DAYS, pred_lt$pred_death_gompertz, type = "l",
     xlim = c(0,100),
     col= 1,
     lwd= 1,
     main = "Previsione totale deceduti, Gompertz",
     xlab = "Giorni a partire dal 24 febbraio 2020",
     ylab = "Counts")
points(df_italy$days[1:length(df_italy$data)], df_italy$deceduti[1:length(df_italy$data)], 
       col="firebrick",
       type = "o",
       pch = 19,
       cex = 1.2,
       lwd = 3)
abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
     y = (max(pred_lt$pred_death_gompertz)-min(pred_lt$pred_death_gompertz))/2, srt = 90, adj = 0.5)

# --- CONTAGIATI TOTALI MODELLO GOMPERTZ
# Logistic sigmoid per totale deceduti 
gompertz.mod.tot.casi.getInitial <- getInitial(totale_casi ~ SSgompertz(days, Asym, b2, b3), data = df_italy)
gompertz.mod.fit.tot.casi <- nls(totale_casi ~ SSgompertz(days, Asym, b2, b3), data = df_italy)
# previsione 
totale_casi_gompertz_predict <- predict(gompertz.mod.fit.tot.casi, df_italy$days)
# qualità del modello
# chi-squared test
gompertz.mod.fit.tot.casi.ChiSq.res <- chisq.test(x = df_italy$totale_casi,
                                                  y = totale_casi_gompertz_predict)
# ora proviamo una stima di lungo perido in pred_lt
pred_lt$pred_totale_casi_gompertz <- SSgompertz(x = pred_lt$DAYS,
                                             Asym = (summary(gompertz.mod.fit.tot.casi)$param[1]),
                                             b2   = (summary(gompertz.mod.fit.tot.casi)$param[2]),
                                             b3   = (summary(gompertz.mod.fit.tot.casi)$param[3]))

# DERIVATA, NUOVI TOTALI GIORNALIERI, GOMPERTZ
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_totale_casi_gompertz[i+1] - pred_lt$pred_totale_casi_gompertz[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_gompertz_nuovi_totali_giornalieri <- tmp_vec
rm(tmp_vec)

# 3 modelli a confronto per i contagiati totali
plot(pred_lt$DAYS, pred_lt$pred_totale_casi_gompertz, type = "l", col=1)
lines(pred_lt$DAYS, pred_lt$pred_new_total_from_dailyVarPerc, type = "l", col=2)
lines(pred_lt$DAYS, pred_lt$pred_totale_casi_logistic, type = "l", col=3)
text("Gompertz", x = 150, y = max(pred_lt$pred_totale_casi_gompertz), pos = 4)
text("daily delta", x = 150, y = max(pred_lt$pred_new_total_from_dailyVarPerc), pos = 4)
text("Logistic", x = 150, y = max(pred_lt$pred_totale_casi_logistic), pos = 4)
# 3 modelli a confronto per i contagiati giornalieri
plot(pred_lt$DAYS, pred_lt$pred_gompertz_nuovi_totali_giornalieri, type = "l", col=1)
lines(pred_lt$DAYS, pred_lt$pred_cont_giornalieri, type = "l", col=2)
lines(pred_lt$DAYS, pred_lt$pred_logistic_nuovi_totali_giornalieri, type = "l", col=3)
tmp_val <- pred_lt[pred_lt$pred_gompertz_nuovi_totali_giornalieri == max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
text("Gompertz", x = tmp_val, y = max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE), pos = 4)
tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
text("daily delta", x = tmp_val, y = max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), pos = 4)
tmp_val <- pred_lt[pred_lt$pred_logistic_nuovi_totali_giornalieri == max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
text("Logistic", x = tmp_val, y = max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE), pos = 4)
rm(tmp_val)