######VERSION 0.0.1
library(shiny)
#source(file = "/home/rosy/Documents/NicoMaffe/covid_wd/covid19/main2source.R", verbose = TRUE)
source(file = "main2source.R", verbose = TRUE) #per shinyapps.io
url <- "https://twitter.com/intent/tweet?text=Coronavirus%20SARS-CoV-2%20in%20Italia%20at&url=http://colmargherita.dsa.unive.it/covid19/"
#url <- "https://twitter.com/intent/tweet?text=Coronavirus%20SARS-CoV-2%20in%20Italia%20at&url=https://rosyproject.shinyapps.io/covid19/"
######
shinyUI(fluidPage(
# ESEMPIO PAGINA WEB CA' FOSCARI
# <meta name="generator" content="TYPO3 CMS">
# <meta name="viewport" content="width=device-width, initial-scale=1.0">
# <meta http-equiv="X-UA-Compatible" content="IE=edge">
# <meta property="og:type" content="website">
# <meta property="og:site_name" content="Università Ca' Foscari Venezia">
# <meta name="twitter:card" content="summary">
# <meta name="twitter:site" content="@CaFoscari">
# <meta property="og:url" content="http://www.unive.it/pag/12469/">
# <meta property="og:title" content="Home">
# <meta name="twitter:url" content="http://www.unive.it/pag/12469/">
# <meta name="twitter:title" content="Home">
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="SARS-CoV-2", content="..."),
    tags$meta(name="Covid19", content="..."),
    tags$meta(name="Italy", content="width=device-width, initial-scale=1.0")
  ),
  theme = ("bootswatch-cerulean.css"),
  #theme = ("bootswatch-simplex.css"),
  
  tags$hr(),
  
  fluidRow(
    
    column(2, offset = 2,
           tags$img(height=140,
                    width=110,
                    src="italy.png")),
    
    column(5, offset=0,
           tags$h1("Coronavirus SARS-CoV-2 in Italia"),
           tags$h2("Dati storici e proiezioni"),
           # tags$h4("v0.0.04"),
           h5(a(href="https://github.com/pcm-dpc/COVID-19",
                "Link ai dati ufficiali del Ministero delle Salute"), align="left")
    ),
    
    column(2, offset = 0,
           tags$img(height=140,
                    width=250,
                    src="covid19.jpg")
    )
    
  ),
  
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(h1("Ultimi dati aggiornati:", align = "center"),
                 h4("Data e Ora correnti", align = "center"),
                 h4(textOutput("currentTime"), align = "center"),
                 tags$hr(),
                 h2("Dati globali aggiornati al:"),
                 h2(textOutput("lastTimeRead"), align = "center"),
                 fluidRow(
                   #column(1, offset = 0, tags$img(height=70, src="actual.positive.png")),
                   column(10, offset = 0, h3(textOutput("actual.positive"), style = "color:firebrick", align = "left"))#temperatura
                 ),
                 fluidRow(
                   column(10, offset = 0, h3(textOutput("actual.death"), style = "color:black", align = "left"))
                 ),
                 fluidRow(
                   #column(1, offset = 0, tags$img(height=70, src="actual.healed.png")),
                   column(10, offset = 0, h3(textOutput("actual.healed"), style = "color:forestgreen", align = "left"))#umidità
                 ),
                 fluidRow(
                   column(10, offset = 0, h3(textOutput("actual.total"), style = "color:navy", align = "left"))
                 ),
                 tags$hr(),
                 fluidRow(
                   column(10, offset = 0,
                          tags$a(href=url, "Tweet", class="twitter-share-button"),
                          includeScript("http://platform.twitter.com/widgets.js")
                   )
                 )
                 # tags$hr(),
                 # fluidRow(
                 #   column(4, offset=0,
                 #          tags$h4("A cura di: ", a(href="mailto: federico.dallo@unive.it","Federico_Dallo"), 
                 #                  a(href="mailto: niccolo.maffezzoli@unive.it","Niccolò_Maffezzoli"), 
                 #                  a(href="mailto: daniele.zannonu@unive.it","Daniele_Zannoni"))
                 #   )
                 # )
    ),
    
    ####### QUESTA LA PAGINA PRINCIPALE
    mainPanel(
      #tabsetPanel(type="pills", id="TABS",
      tabsetPanel(type="tabs", id="TABS",
                  tabPanel("Mod. Esponenziale", id="exp.mod",
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.tot.contagiati")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.daily.contagiati")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    plotOutput("pred_5.daily.avg.perc")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    plotOutput("daily.avg.perc")
                             )
                           )
                  ),
                  tabPanel("Mod. Logistica", id="logistic.mod",
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.tot.contagiati.logistic")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.daily.contagiati.logistic")
                             )
                           )
                  ),
                  tabPanel("Mod. Gompertz", id="gompertz.mod",
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.tot.contagiati.gompertz")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    plotOutput("pred_lt.daily.contagiati.gompertz")
                             )
                           )
                  ),
                  tabPanel("Storico e Analisi", id="personal.mod",
                           fluidRow(
                             column(4,
                                    selectInput("inputId_dataset", "Seleziona dati:", 
                                                choices = c("Nazionali", "Regionali", "Provinciali"))
                             ),
                             column(4, offset = 0,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset == 'Nazionali'",
                                      selectInput("national_variables", "Variabile",
                                                  c(names(df_italy_nazione[3:ncol(df_italy_nazione)])))
                                    )
                             )
                           ),
                           fluidRow(
                             column(4, offset = 1,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset == 'Regionali'",
                                      selectInput("regional_region", "",
                                                  c(levels(df_italy_regioni$denominazione_regione)))
                                    )
                             ),
                             column(4, offset = 0,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset == 'Regionali'",
                                      selectInput("regional_variables", "",
                                                  c(names(df_italy_regioni[7:ncol(df_italy_regioni)]))))
                             ),
                             column(4, offset = 1,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset == 'Provinciali'",
                                      selectInput("province_province", "",
                                                  c(levels(df_italy_province$denominazione_provincia)))
                                    )
                             ),
                             column(4, offset = 0,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset == 'Provinciali'",
                                      selectInput("province_variables", "",
                                                  c(names(df_italy_province[ncol(df_italy_province)-3]))))
                             )
                           ),
                           fluidRow(
                             column(12, offset = 0,
                                    conditionalPanel(
                                      condition = "input.inputId_dataset",
                                      plotOutput("defaultPlot", click = "plot_click", brush = brushOpts(id= "default_plot_brush")),
                                      h4("Punti contenuti nell'area: (*usare il mouse sul grafico)"),
                                      verbatimTextOutput("brush_info")
                                    )
                             )
                           )
                  ),
                  #tabPanel("Avviso", id="avvisi",
                  tabPanel(tags$p("Avviso", class="text-danger"), id="avvisi",
                           fluidRow(
                             column(10, offset = 1,
                                    tags$br(),
                                    h2("DISCLAIMER", align="center"),
                                    h4("Le statistiche e proiezioni che trovate in questo sito 
                                    sono effettuate sui dati rilasciati quotidianamente 
                                    dalla Protezione Civile e
                                    dal Ministero delle Salute.
                                    Gli autori ci tengono a precisare che non hanno ricevuto nessun
                                    incarico o sovvenzione e il presente lavoro ha il solo scopo
                                    di soddisfare la curiosità degli autori e contribuire 
                                    costruttivamente al loro personale dibattito con amici, 
                                    parenti e persone interessate.", align="center"),
                                    h4("IMPORTANTE: 
                                       i risultati delle presenti analisi NON possono essere utilizzati da 
                                       inesperti o qualsivoglia persona incapace di comprendere i
                                       fondamenti delle analisi effettuate.
                                       Le analisi in questo sito NON potranno in particolare avvalorare teorie prive di fondamento 
                                       scientifico. Gli autori prendono preventivamente le distanze
                                       da queste persone.", align="center"),
                                    tags$br(),
                                    h3("INVITO", align="center"),
                                    h4("Chiunque avesse suggerimenti e commenti tecnico-scientifici è benvenuto
                                    e invitato a scriverci una mail privata! Ringraziamo sia gli 
                                    eventuali supporter che gli eventuali denigratori, con i quali
                                    ci scusiamo nel caso avessimo inavvertitamente urtato la loro
                                    sensibilità. In entrambi questi ultimi due casi vi preghiamo di scriverci solo
                                    per ragioni urgenti. Grazie!", align="center"),
                                    tags$br(),
                                    h3("Dati", align="center"),
                                    h4(a(href="https://github.com/pcm-dpc/COVID-19",
                                         "Link ai dati della Protezione Civile e del 
                                      Ministero della Salute"), align="left"),
                                    h4(a(href="https://github.com/CSSEGISandData/COVID-19",
                                         "Link ai dati della Johns Hopkins University Center 
                                      for Systems Science and Engineering (JHU CSSE)"), align="left"),
                                    h3("Pagine esterne", align="center"),
                                    #h4(a(href="","")), align="left"),
                                    h4(a(href="http://www.salute.gov.it/portale/nuovocoronavirus/dettaglioContenutiNuovoCoronavirus.jsp?lingua=italiano&id=5351&area=nuovoCoronavirus&menu=vuoto",
                                         "Link alla pagina \"Nuovo coronavirus\" del Ministero della Salute"), align="left"),
                                    h4(a(href="http://opendatadpc.maps.arcgis.com/apps/opsdashboard/index.html#/b0c68bce2cce478eaac82fe38d4138b1",
                                         "Link alla pagina arcgis del Dipartimento della Protezione Civile"), align="left"),
                                    h4(a(href="https://lab24.ilsole24ore.com/coronavirus/",
                                         "Link alla pagina \"lab24\" del sole24 ore con l'analisi
                                         delle serie storiche"), align="left")
                             )
                           )
                  ),
                  tabPanel("Contatti e info", id="contacts",
                           fluidRow(
                             h2("Contributi:", align = "left")
                           ),
                           fluidRow(
                             h4(a(href="mailto: federico.dallo@unive.it","Federico Dallo") , align = "left")
                           ),
                           fluidRow(
                             h5("Postdoc Research Fellow presso ISP-CNR di Venezia. Chimico.",
                                a(href="https://www.linkedin.com/in/federico-dallo-b351a582/","LinkedIn"))
                           ),
                           fluidRow(
                             h4(a(href="mailto: niccolo.maffezzoli@unive.it","Niccolò Maffezzoli"), align = "left")
                           ),
                           fluidRow(
                             h5("Marie-Curie Fellow presso Università Ca' Foscari di Venezia. Fisico.",
                                a(href="https://www.linkedin.com/in/niccol%C3%B2-maffezzoli-8a92247a/","LinkedIn"))
                           ),
                           fluidRow(
                             h4(a(href="mailto: daniele.zannoni@unive.it","Daniele Zannoni"), align = "left")
                           ),
                           fluidRow(
                             h5("Postdoc Research Fellow presso ISP-CNR di Venezia. Scienziato Ambientale.",
                                a(href="https://www.researchgate.net/profile/Daniele_Zannoni","ResearchGate"))
                           ),
                           fluidRow(
                             h4(a(href="mailto: fabrizio.deblasi@unive.it","Fabrizio de Blasi"), align = "left")
                           ),
                           fluidRow(
                             h5("Postdoc Research Fellow presso ISP-CNR di Venezia. Scienziato Forestale e Ambientale.",
                                a(href="https://www.linkedin.com/in/fabrizio-de-blasi-76381883/","LinkedIn"))
                           ),
                           fluidRow(
                             h4(a(href="mailto: magnani.msm@gmail.com","Massimo Magnani"), align = "left")
                           ),
                           fluidRow(
                             h5("Process Safety Researcher presso Olon SpA. Chimico.",
                                a(href="https://www.linkedin.com/in/magnanimsm/","LinkedIn"))
                           ),
                           tags$hr(),
                           fluidRow(
                             h5("Questa pagina è stata creata il 21 marzo 2020", align = "left"),
                             h5("Versione corrente: v0.0.09 del 30 marzo 2020", align = "left")
                           ),
                           fluidRow(
                             h4("Credits"),
                             h5("Questa applicazione è stata costruita usando i pacchetti", a(href="https://www.r-project.org/","R"), "e", a(href="https://shiny.rstudio.com/","Shiny.")),
                             h5("Source code", a(href="https://github.com/developerISP/covid19","github.com/developerISP"))
                           )
                  )
      )
    )
  )
  
)
)