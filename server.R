######VERSION 5.1.0
library(shiny)
#source(file = "/home/rosy/Documents/NicoMaffe/covid_wd/covid19/main2source.R", verbose = TRUE)
######
shinyServer(function(input, output, session) {
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  # output$akuCombin <- renderUI({
  #   tags$iframe(src = "https://www.youtube.com/embed/203ZyNWblrI", width="100%", height=600, autoplay = TRUE)
  # })
  ####################################################################################################
  # today<-format(Sys.time(), "%Y_%m_%d")
  # user<- Sys.getenv("LOGNAME")
  # setwd(file.path("/home/rosy/Documents/NicoMaffe/covid_wd/covid19/"))
  # app_dir <- file.path("/home/rosy/Documents/NicoMaffe/covid_wd")
  # data_dir <- file.path("/home/rosy/Documents/NicoMaffe/covid_wd/ministero_data/COVID-19/")
  # data_file_nazionale <- paste0(data_dir,"dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
  # fl <- list.files(data_dir, full.names = FALSE)
  ####################################################################################################
  
  # Carico lo script main.R
  source(file = "main2source.R", verbose = TRUE)
  
  # Output delle variabili
  output$lastTimeRead <- renderText(paste({as.character(df_italy$data[length(df_italy$data)])}))
  output$actual.positive <- renderText(paste0("Positivi: ",
                                              df_italy$totale_attualmente_positivi[length(df_italy$data)]))
  output$actual.death <- renderText(paste0("Deceduti: ",
                                           df_italy$deceduti[length(df_italy$data)]))
  output$actual.healed <- renderText(paste0("Guariti: ",
                                           df_italy$dimessi_guariti[length(df_italy$data)]))
  output$actual.total <- renderText(paste0("Casi totali: ",
                                           df_italy$totale_casi[length(df_italy$data)]))
  
  # GRAFICI NELLA PAGINA PRINCIPALE
  
  # MODELLO ESPONENZIALE Previsione totale contagiati
  # MODELLO ESPONENZIALE Previsione totale contagiati
  # MODELLO ESPONENZIALE Previsione totale contagiati
  output$pred_lt.tot.contagiati <- renderPlot({
    invalidateLater(60000,session)
    tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"]
    tmp_val <- na.omit(tmp_val)
    tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
    plot(pred_lt$DAYS, pred_lt$pred_new_total_from_dailyVarPerc, type = "l",
         col= 1,
         lwd= 1,
         main = "Previsione totale contagiati",
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
    points(df_italy$days[2:length(df_italy$data)], df_italy$totale_casi[2:length(df_italy$totale_casi)], 
           col="firebrick",
           type = "o",
           pch = 19,
           cex = 1.2,
           lwd = 3)
    abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
    text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
         y = (max(pred_lt$pred_new_total_from_dailyVarPerc)-min(pred_lt$pred_new_total_from_dailyVarPerc))/2, srt = 90, adj = 0.5)
    # Punti significativi
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
    rm(tmp_to_print)
  })
  
  ### MODELLO ESPONENZIALE Previsione giornalieri contagiati
  ### MODELLO ESPONENZIALE Previsione giornalieri contagiati
  ### MODELLO ESPONENZIALE Previsione giornalieri contagiati
  output$pred_lt.daily.contagiati <- renderPlot({
    invalidateLater(60000,session)
    # RIMOZIONE-----------------
    tmp_row <- pred_lt[1,]    #
    pred_lt <- pred_lt[-1,]   #
    #
    # PLOT
    plot(pred_lt$DAYS, pred_lt$pred_cont_giornalieri, type = "l",
         main = "Stima a lungo periodo dei nuovi contagiati giornalieri",
         ylim = c(0, max(df_italy$nuovi_totali, na.rm = TRUE)),
         lwd=1.5,
         pch=19,
         cex=1.2,
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
    # RIAGGIUNTA --------------------------
    pred_lt <- rbind(tmp_row, pred_lt)    #
    rm(tmp_row)                           #
    # -------------------------------------
    points(df_italy$days, df_italy$nuovi_totali, 
           type = "o",
           col="firebrick",
           pch=19,
           cex=1.2)
    abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
    text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
         y = (max(pred_lt$pred_cont_giornalieri, na.rm = TRUE)-min(pred_lt$pred_cont_giornalieri, na.rm = TRUE))/2, srt = 90, adj = 0.5)
    abline(v = pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"],
           col="navy",
           lwd=3)
    tmp_val <- pred_lt[pred_lt$pred_cont_giornalieri == max(pred_lt$pred_cont_giornalieri, na.rm = TRUE), "DAYS"]
    tmp_val <- na.omit(tmp_val)
    tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
    # Punti significativi
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
  })
  
  ### MODELLO ESPONENZIALE Previsione 5 giorni variazione percentuale giornaliera contagiati 
  ### MODELLO ESPONENZIALE Previsione 5 giorni variazione percentuale giornaliera contagiati 
  ### MODELLO ESPONENZIALE Previsione 5 giorni variazione percentuale giornaliera contagiati 
  output$pred_5.daily.avg.perc <- renderPlot({
    invalidateLater(60000,session)
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
  })
  
  ### MODELLO ESPONENZIALE su variazione percentuale giornaliera dei contagiati 
  ### MODELLO ESPONENZIALE su variazione percentuale giornaliera dei contagiati 
  ### MODELLO ESPONENZIALE su variazione percentuale giornaliera dei contagiati 
  output$daily.avg.perc <- renderPlot({
    invalidateLater(60000,session)
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
    lines(df_italy$data, var_perc_giorn_predict,
          col = 1,
          lwd = 3)
    # qualità del modello
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
  })
  
  # MODELLO LOGISTICA Previsione totale contagiati
  # MODELLO LOGISTICA Previsione totale contagiati
  # MODELLO LOGISTICA Previsione totale contagiati
  
  output$pred_lt.tot.contagiati.logistic <- renderPlot({
    invalidateLater(60000,session)
    plot(pred_lt$DAYS, pred_lt$pred_totale_casi_logistic, type = "l",
         xlim = c(0,100),
         col= 1,
         lwd= 1,
         main = "Previsione totale contagiati",
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
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
         x = 60,
         y = max(pred_lt$pred_totale_casi_logistic, na.rm = TRUE)/2,
         pos = 4 
    )
    grid(NULL, NULL, lwd = 2)
    rm(tmp_to_print)
    # PUNTI SIGNIFICATIVI
    tmp_val <- pred_lt[pred_lt$pred_logistic_nuovi_totali_giornalieri == max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
    tmp_val <- na.omit(tmp_val)
    tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
    points(pred_lt$DAYS[80], pred_lt$pred_totale_casi_logistic[80],
           col="navy", 
           pch = 19,
           cex = 1.5)
    text(paste0(format(df_italy$data[1]+3600*24*79, format = "%Y-%m-%d")," Tot: ", 
                format(round(pred_lt$pred_totale_casi_logistic[80]), nsmall = 0) ), 
         x = pred_lt$DAYS[80],
         y = pred_lt$pred_totale_casi_logistic[80],
         adj = 1,pos = 1)
    points(tmp_val, pred_lt$pred_totale_casi_logistic[tmp_val],
           col = "red",
           pch = 17,
           cex = 1.5)
    text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
         y = pred_lt$pred_totale_casi_logistic[tmp_val],
         pos = 4)
    text(paste0("   Max= ", format(round(max(pred_lt$pred_logistic_nuovi_totali_giornalieri, na.rm = TRUE)), nsmall = 0)), 
         x = tmp_val,
         y = pred_lt$pred_totale_casi_logistic[tmp_val],
         pos = 3)
    rm(tmp_val)
    rm(tmp_day)
  })
  
  output$pred_lt.daily.contagiati.logistic <- renderPlot({
    invalidateLater(60000,session)
    plot(pred_lt$DAYS, pred_lt$pred_logistic_nuovi_totali_giornalieri, type = "l",
         xlim = c(0,100),
         ylim = c(0, max(df_italy$nuovi_totali, na.rm = TRUE)),
         col= 1,
         lwd= 1,
         main = "Previsione nuovi casi giornalieri",
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
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
         adj = 1,pos = 3)
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
         pos = 2)
    grid(NULL, NULL, lwd = 2)
    rm(tmp_val)
    rm(tmp_day)
    
  })
  

  # MODELLO GOMPERTZ Previsione totale contagiati
  # MODELLO GOMPERTZ Previsione totale contagiati
  # MODELLO GOMPERTZ Previsione totale contagiati
  
  output$pred_lt.tot.contagiati.gompertz <- renderPlot({
    invalidateLater(60000,session)
    plot(pred_lt$DAYS, pred_lt$pred_totale_casi_gompertz, type = "l",
         col= 1,
         lwd= 1,
         main = "Previsione totale contagiati",
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
    points(df_italy$days[1:length(df_italy$data)], df_italy$totale_casi[1:length(df_italy$data)], 
           col="firebrick",
           type = "o",
           pch = 19,
           cex = 1.2,
           lwd = 3)
    abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
    text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
         y = (max(pred_lt$pred_totale_casi_gompertz)-min(pred_lt$pred_totale_casi_gompertz))/2, srt = 90, adj = 0.5)
    # tabella qualità
    tmp_to_print <- c("Modello Gompertz",
                      paste0("Estimate      Std. Error      t value      Pr(>|t|)"),
                      paste0("asym: ", paste0(format(round(summary(gompertz.mod.fit.tot.casi)$param[1,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                      paste0("b2: ",   paste0(format(round(summary(gompertz.mod.fit.tot.casi)$param[2,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                      paste0("b3: ",   paste0(format(round(summary(gompertz.mod.fit.tot.casi)$param[3,], digits = 3), justify = "right", nsmall = 2), collapse = "\t"), collapse = "\t"),
                      paste0("Ach.conv.tol.: ", (summary(gompertz.mod.fit.tot.casi)$convInfo)$finTol),
                      "Chi-squared (0.05) p-value:",
                      format(round(gompertz.mod.fit.tot.casi.ChiSq.res$p.value, digits = 4), nsmall = 2))
    text(paste(tmp_to_print, collapse="\n"),
         x = 100,
         y = max(pred_lt$pred_totale_casi_gompertz, na.rm = TRUE)/3,
         pos = 4 
    )
    grid(NULL, NULL, lwd = 2)
    rm(tmp_to_print)
    # PUNTI SIGNIFICATIVI
    tmp_val <- pred_lt[pred_lt$pred_gompertz_nuovi_totali_giornalieri == max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
    tmp_val <- na.omit(tmp_val)
    tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
    points(pred_lt$DAYS[200], pred_lt$pred_totale_casi_gompertz[200],
           col="navy", 
           pch = 19,
           cex = 1.5)
    text(paste0(format(df_italy$data[1]+3600*24*199, format = "%Y-%m-%d")," Tot: ", 
                format(round(pred_lt$pred_totale_casi_gompertz[200]), nsmall = 0) ), 
         x = pred_lt$DAYS[200],
         y = pred_lt$pred_totale_casi_gompertz[200],
         adj = 1,pos = 1)
    points(tmp_val, pred_lt$pred_totale_casi_gompertz[tmp_val],
           col = "red",
           pch = 17,
           cex = 1.5)
    text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
         y = pred_lt$pred_totale_casi_gompertz[tmp_val],
         pos = 4)
    text(paste0("   Max= ", format(round(max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE)), nsmall = 0)), 
         x = tmp_val,
         y = pred_lt$pred_totale_casi_gompertz[tmp_val],
         pos = 3)
    rm(tmp_val)
    rm(tmp_day)
  })
  
  output$pred_lt.daily.contagiati.gompertz <- renderPlot({
    invalidateLater(60000,session)
    plot(pred_lt$DAYS, pred_lt$pred_gompertz_nuovi_totali_giornalieri, type = "l",
         ylim = c(0, max(df_italy$nuovi_totali, na.rm = TRUE)),
         #xlim = c(0,100),
         col= 1,
         lwd= 1,
         main = "Previsione nuovi casi giornalieri",
         xlab = "Giorni a partire dal 24 febbraio 2020",
         ylab = "")
    points(df_italy$days[2:length(df_italy$data)], df_italy$nuovi_totali[2:length(df_italy$data)], 
           col="firebrick",
           type = "o",
           pch = 19,
           cex = 1.2,
           lwd = 3)
    abline(v = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"])
    text("Decreto zona rossa", x = df_italy[df_italy$data == "2020-03-08 18:00:00", "days"] + 0.25, 
         y = (max(pred_lt$pred_gompertz_nuovi_totali_giornalieri)-min(pred_lt$pred_gompertz_nuovi_totali_giornalieri))/2, srt = 90, adj = 0.5)
    # PUNTI SIGNIFICATIVI
    tmp_val <- pred_lt[pred_lt$pred_gompertz_nuovi_totali_giornalieri == max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE), "DAYS"]
    tmp_val <- na.omit(tmp_val)
    tmp_day <- df_italy$data[1] + 3600*24*tmp_val - 3600*24
    points(pred_lt$DAYS[200], pred_lt$pred_gompertz_nuovi_totali_giornalieri[200],
           col="navy", 
           pch = 19,
           cex = 1.5)
    text(paste0(format(df_italy$data[1]+3600*24*199, format = "%Y-%m-%d")," New: ", 
                format(round(pred_lt$pred_gompertz_nuovi_totali_giornalieri[200]), nsmall = 0) ), 
         x = pred_lt$DAYS[200],
         y = pred_lt$pred_gompertz_nuovi_totali_giornalieri[200],
         adj = 1,pos = 3)
    points(tmp_val, pred_lt$pred_gompertz_nuovi_totali_giornalieri[tmp_val],
           col = "red",
           pch = 17,
           cex = 1.5)
    text(paste0("   ", format(tmp_day, format="%Y-%m-%d")), x = tmp_val, 
         y = pred_lt$pred_gompertz_nuovi_totali_giornalieri[tmp_val],
         pos = 4)
    text(paste0("   Max= ", format(round(max(pred_lt$pred_gompertz_nuovi_totali_giornalieri, na.rm = TRUE)), nsmall = 0)), 
         x = tmp_val,
         y = pred_lt$pred_gompertz_nuovi_totali_giornalieri[tmp_val],
         pos = 2)
    grid(NULL, NULL, lwd = 2)
    rm(tmp_val)
    rm(tmp_day)
  })
  
  #### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
  #### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
  #### ---- DA QUI' LAVORO ALL'INTERFACCIA INTERATTIVA PER UTENTE ---- ####
  
  toListen <- reactive({
    list(input$inputId_dataset, input$national_variables, 
         input$regional_region, input$regional_variables, 
         input$province_province, input$province_variables)
  })
  
  #datasetInput <- reactive({
  datasetInput <- eventReactive(toListen(), {
    switch(input$inputId_dataset,
           "Nazionali" = df_italy_nazione,
           "Regionali" = df_italy_regioni,
           "Provinciali" = df_italy_province
           )
  })
  
  observeEvent(toListen(), {
    output$defaultPlot <- renderPlot({
      dataset <- datasetInput()
      if (identical(dataset,df_italy_nazione) == TRUE) {
        tmp_variable <- as.character(input$national_variables)
        tmp_var <- as.name(input$national_variables)
        print(tmp_variable)
        dataset <- subset(dataset, select = c("data", tmp_variable))
        dataset$days <- seq(1,length(dataset$data),1)
        print(head(dataset))
        #--- model logistic
        # logis.mod.getInitial <- getInitial(tmp_var ~ SSlogis(days, Asym, xmid, scal), data = df_italy_nazione)
        # logis.mod.getInitial <- getInitial(ricoverati_con_sintomi ~ SSlogis(days, Asym, xmid, scal), data = df_italy_nazione)
        # logis.mod.fit <- nls(tmp_var~ SSlogis(days,Asym,xmid,scal), data = df_italy_nazione)
        # logis.mod.fit <- nls(ricoverati_con_sintomi ~ SSlogis(days,Asym,xmid,scal), data = df_italy_nazione)
        # logis.predict <- predict(logis.mod.fit, df_italy_nazione$data)
      } else if (identical(dataset, df_italy_regioni) == TRUE) { 
        tmp_region <- as.character(input$regional_region)
        dataset <- dataset[dataset[,"denominazione_regione"] == tmp_region, ]
        tmp_variable <- as.character(input$regional_variables)
        dataset <- subset(dataset, select = c("data", tmp_variable))
        dataset$days <- seq(1,length(dataset$data),1)
      } else if (identical(dataset, df_italy_province) == TRUE) {
        tmp_province <- as.character(input$province_province)
        dataset <- dataset[dataset[,"denominazione_provincia"] == tmp_province, ]
        tmp_variable <- as.character(input$province_variables)
        dataset <- subset(dataset, select = c("data", tmp_variable))
        dataset$days <- seq(1,length(dataset$data),1)
      }
      variabile<- names(dataset)[2]
      assex <- names(dataset)[1]
      plot(dataset[,1],dataset[,2], type = "o",
           col= "firebrick",
           pch=19,
           cex=1.2,
           lwd=1.2,
           main=as.character(variabile),
           xlab =as.character(assex),
           ylab = ""
           )
      #
      # if(exists("logis.predict")){
      #   lines(logis.predict, col="green", lwd=3)
      # }
      #
      grid(NULL, NULL, lwd = 2)
      output$info <- renderText({
        paste0("x=", as.Date(input$plot_click$x), "\ny=", input$plot_click$y)
        #paste0("y=", input$plot_click$y)
      })
      output$brush_info <- renderPrint({
        brushedPoints(df = dataset, brush = input$default_plot_brush,
                      xvar = names(dataset)[1], yvar = names(dataset)[2])
      })
    })
  })
})
