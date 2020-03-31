# Sorgente da importare per shiny server
# contiene analisi open data sul covid19
# Author: Federico Dallo
# Data: 21 marzo 2020

# ------------------------------
# Pulizia dello spazio di lavoro:
#rm(list = ls())
#
# ------------------------------
# Update data
# pull_file <- "/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/get_update.sh"
# setwd("/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/")
# system(command = pull_file, intern = TRUE)
# rm(pull_file)
#
# ------------------------------
# Move to working directory
#setwd("/home/rosy/Documents/NicoMaffe/covid_wd/")
#setwd("/home/rosy/Documents/NicoMaffe/covid_wd/covid19/")

# Dati andamento nazionale:
today <- format(Sys.time(), "%Y%m%d")
#target_dir <- "/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/dati-andamento-nazionale/"
target_dir <- "data/"
target_file <- paste0(target_dir,"dpc-covid19-ita-andamento-nazionale.csv")
# target_file <- paste0(target_dir,"/COVID-19-geographic-disbtribution-worldwide-",today,".csv")
#
# ------------------------------
# Lettura dei dati
### ATTENZIONE: goto: "INTERFACCIA INTERATTIVA PER UTENTE"
### ATTENZIONE: si trovano altri dati caricati
raw_data <- read.csv(file = target_file, header = TRUE, sep = ",")
raw_data$data <- as.POSIXct(raw_data$data, format="%Y-%m-%dT%H:%M:%S")
df_italy <- raw_data

#### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
#### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
#### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
target_dir_dati_italia <- "data/"
target_file_nazionale <- paste0(target_dir_dati_italia,"dpc-covid19-ita-andamento-nazionale.csv")
target_file_regioni <-   paste0(target_dir_dati_italia,"dpc-covid19-ita-regioni.csv")
target_file_province <-  paste0(target_dir_dati_italia,"dpc-covid19-ita-province.csv")
#
raw_data_nazionale <- read.csv(file = target_file_nazionale, header = TRUE, sep = ",")
raw_data_nazionale$data <- as.POSIXct(raw_data_nazionale$data, format="%Y-%m-%dT%H:%M:%S")
#
raw_data_regioni <- read.csv(file = target_file_regioni, header = TRUE, sep = ",")
raw_data_regioni$data <- as.POSIXct(raw_data_regioni$data, format="%Y-%m-%dT%H:%M:%S")
#
raw_data_province <- read.csv(file = target_file_province, header = TRUE, sep = ",")
raw_data_province$data <- as.POSIXct(raw_data_province$data, format="%Y-%m-%dT%H:%M:%S")
# CREAZIONE DATASET NAZIONALE E NUMERO GIORNI
df_italy_nazione   <- raw_data_nazionale
df_italy_nazione$days <- seq(1,length(df_italy_nazione$data),1) #colonna numero dei giorni
# CREAZIONE DATASET REGIONALE E NUMERO GIORNI
df_italy_regioni   <- raw_data_regioni
tmp_days <- as.data.frame(df_italy_regioni$data[length(df_italy_regioni$data)] - 
                            df_italy_regioni$data[1])
tmp_days <- round(as.numeric(tmp_days[1])) + 1
tmp_nreg <- max(df_italy_regioni$codice_regione)+1 #Trento e Bolzano hanno lo stesso codice regione
for (i in seq(1,tmp_days,1)){
  for(j in seq(1,tmp_nreg,1)){
    df_italy_regioni$days[j+(tmp_nreg*(i-1))] <- i
  }
}
rm(tmp_days,tmp_nreg,i,j)
# CREAZIONE DATASET PROVINCIALE E NUMERO GIORNI
df_italy_province <- raw_data_province
df_italy_province$days <- NA
# *scorrere l'indice e contare la differenza in giorni tra data(i) e data(i-1)
for (i in seq(1,length(df_italy_province$data),1)){
  df_italy_province$days[i] <- round(as.numeric(as.data.frame(df_italy_province$data[i]-df_italy_province$data[1]))) + 1
}
#---------
#---------
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
# ------------------------------
# modello esponenziale
exp.mod.nuoviTotali <- lm(log(df_italy$nuovi_totali)~df_italy$days)
timevalues <- data.frame(data=seq(1, length(df_italy$days), 0.1))
nuovi_totali_predict <- exp(predict(exp.mod.nuoviTotali, list(days=timevalues) ))
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
# RIAGGIUNTA --------------------------
df_italy <- rbind(tmp_row, df_italy)  #
rm(tmp_row)                           #
#--------------------------------------
# - Previsioni -
pred <- data.frame(DAYS = seq(1,length(df_italy$days)+5,1))
pred$pred_new_total_predict <- exp(pred$DAYS*exp.mod.nuoviTotali$coefficients[2] + exp.mod.nuoviTotali$coefficients[1] )
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]  # as.POSIXct("2020-03-08") + 3600*6
totalNew <- sum(pred$pred_new_total_predict)
# --------------------------##########################################################
# MORTI GIORNALIERE         ##########################################################
# RIMOZIONE-----------------##########################################################
tmp_row <- df_italy[1,]    #
df_italy <- df_italy[-1,]  #
#---------------------------
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
# qualità del modello
# chi-squared test
exp.mod.nuoviMorti.ChiSq.res <- chisq.test(x = df_italy$nuovi_deceduti,
                                            y = nuovi_morti_predict)
#----------
# - Previsioni -
#pred <- data.frame(DAYS = seq(1,length(df_italy$days)+5,1))
pred$pred_new_death_predict <- exp(pred$DAYS*exp.mod.nuoviMorti$coefficients[2] + exp.mod.nuoviMorti$coefficients[1] )
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]   # as.POSIXct("2020-03-08") + 3600*6
totalDeath <- sum(pred$pred_new_death_predict)
#----------
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
# qualità del modello
# chi-squared test
exp.mod.dailyVarPerc.ChiSq.res <- chisq.test(x = df_italy$daily_var_perc,
                                             y = var_perc_giorn_predict)
#----------
# previsioni a 5 giorni
pred$pred_dailyVarPerc <- exp(pred$DAYS*exp.mod.dailyVarPerc$coefficients[2] + exp.mod.dailyVarPerc$coefficients[1] )
# tabella qualità
#----------
# valori stimati
lastDay <- df_italy$data[1] + 3600*24*pred$DAYS[length(pred$DAYS) - 1]   # as.POSIXct("2020-03-08") + 3600*6
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
# RIAGGIUNTA --------------------------
pred_lt <- rbind(tmp_row, pred_lt)    #
rm(tmp_row)                           #
# -------------------------------------
tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"]
tmp_val <- na.omit(tmp_val)
tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
# 
# tabella qualità
#----------
#--
# 
# tabella qualità
#----------
#grid(NA, 5, lwd = 2)
rm(tmp_val)
rm(tmp_day)

### MODELLO LOGISTICA
### MODELLO LOGISTICA
### MODELLO LOGISTICA
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
# ora proviamo una stima di lungo perido in pred_lt
pred_lt$pred_death_logistic <- SSlogis(input = pred_lt$DAYS, 
                                       Asym = (summary(logistic.mod.fit.tot.death)$param[1]),
                                       xmid = (summary(logistic.mod.fit.tot.death)$param[2]), 
                                       scal = (summary(logistic.mod.fit.tot.death)$param[3]) 
)
# derivata, ovvero i morti giornalieri
tmp_vec <- vector()
for (i in seq(1,length(pred_lt$DAYS)-1,1)) {
  tmp_vec[i] <- pred_lt$pred_death_logistic[i+1] - pred_lt$pred_death_logistic[i] 
}
tmp_vec <- c(NA,tmp_vec)
pred_lt$pred_death_logistic_giornalieri <- tmp_vec
rm(tmp_vec)

# --- CONTAGIATI TOTALI MODELLO LOGISTICO
# Logistic sigmoid per totale deceduti 
logistic.mod.tot.casi.getInitial <- getInitial(totale_casi ~ SSlogis(days, Asym, xmid, scal), data = df_italy)
logistic.mod.fit.tot.casi <- nls(totale_casi ~ SSlogis(days, Asym, xmid, scal), data = df_italy)
# previsione 
totale_casi_logistic_predict <- predict(logistic.mod.fit.tot.casi, df_italy$days)
# qualità del modello
# chi-squared test
logistic.mod.fit.tot.casi.ChiSq.res <- chisq.test(x = df_italy$totale_casi,
                                                  y = totale_casi_logistic_predict)
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
gompertz.mod.fit.tot.death.ChiSq.res <- chisq.test(x = df_italy$deceduti, 
                                                   y = deceduti_gompertz_predict)
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








