

## @knitr SetUp2
RegulatoryEffort <- c("foreach","ggthemes","kableExtra","qwraps2","tidyverse","data.table","tidyr",
                      "utils","scales","matrixStats","readr","directlabels","dplyr","janitor","lubridate")  # included in tidyverse: "dplyr","tidyr","ggplot2","pandoc","table1", "summarytools"
library(rmsfuns)
load_pkg(RegulatoryEffort)

# Valores de eficiencia en distribución ####

# Río Mendoza
{EfMendozaBalance <- as.data.frame(rbind(
                  c("Compuertas Vistalba", 55.1, 95.9, 52.9),
                  c("Chacras de Coria",45.0, 78.0, 35.1),
                  c("Margen Derecha", 58.1 ,71.5 ,41.6),
                  c("Área Metropolitana" ,43.0, 92.0, 39.6),
                  c("Lujan" ,52.0 ,80.9, 42.0),
                  c("Gil" ,52.0 ,93.2, 48.4),
                  c("Sobremonte", 50.3, 90.6, 45.6),
                  c("Mathus Hoyos" ,50.3 ,79.4, 39.9),
                  c("Algarrobal ",50.3, 74.9 ,37.7),
                  c("Cruz de Piedra" ,52.0, 85.7, 44.5),
                  c("Cespedes" ,45.0, 73.8 ,33.2),
                  c("Rodeo Beltrán" ,45.0 ,82.0, 36.9),
                  c("Sánchez" ,42.0, 90.6, 38.1),
                  c("Tulumaya" ,49.7 ,71.0, 35.3),
                  c("Jocoli" ,49.7, 73.2, 36.4),
                  c("California", 53.0, 99.0, 52.5),
                  c("Costa de Araujo" ,53.0, 89.9, 47.6),
                  c("Gustavo André" ,53.0, 80.5, 42.7),
                  c("Galigniana" ,56.0, 75.8, 42.4),
                  c("Reyna Marienhoff" ,56.0 ,75.8 ,42.4),
                  c("Barrancas" ,52.0, 85.0, 44.2),
                  c("Arroyo Carrizal" ,45.0 ,87.0, 39.2),
                  c("El Salto Las Vegas" ,59.0, 97.0, 57.2),
                  c("Uspallata" ,59.0 ,82.0, 48.4),
                  c("ACRE Campo Espejo", 47.0 ,85.0 ,40.0),
                  c("ACRE Paramillo" ,47.0 ,83.0, 39.0)
))
colnames(EfMendozaBalance) <- c("UAM","EfApl","EfCond","EfGlob")
EfMendozaBalance[,"EfApl"]   <- as.numeric(as.character(EfMendozaBalance[,"EfApl"]))
EfMendozaBalance[,"EfCond"]   <- as.numeric(as.character(EfMendozaBalance[,"EfCond"]))
EfMendozaBalance[,"EfGlob"]   <- as.numeric(as.character(EfMendozaBalance[,"EfGlob"]))
sapply(EfMendozaBalance,class)
#view(EfMendozaBalance)

# Caudal m3/segundo 
# Fuente Cuneo et al. (2015)
CaudalMen <- c(16.4, 18.5, 17.5, 26.3, 26.5, 39.9, 66.4, 61.2, 43.2, 26.1, 21.1, 16.9, 31.7)
names(CaudalMen) <- c("Jul","Ago","Sep","Oct","Nov","Dic","Ene","Feb","Mar","Abr","May","Jun","Módulo")
sapply(CaudalMen, class)

EfMendoza <- as.data.frame(read.csv("DgiData/EfCond.csv", sep = ",")) #, header = TRUE, sep=",")
indxx <- c("TierraQ0","TierraQ0rep","TierraQ0medio","TierraQl","TierraQlrep","TierraQlmedio",
           "Distancia","KmInvert","EfcTierra","LongMedia","EfTierraLong","EfCanales","EfHijuelas","KmCanales",
           "EfUnidadManejo","ha","litroSeg","horas","Q1_","caudalAnual","VolHa")
EfMendoza[indxx] <- lapply(EfMendoza[indxx], function(x) as.numeric(as.character(x)))
EfMendoza$Q0   <- EfMendoza[,ifelse(!is.na("TierraQ0rep"),"TierraQ0medio",
            ifelse(!is.na("TierraQ0rep") <= !is.na("TierraQ0medio"), "TierraQ0rep",
            ifelse(!is.na("TierraQ0medio") <= !is.na("TierraQ0"),"TierraQ0medio","TierraQ0rep")))]

#Q1 será el caudal ajustado recibido de Cipoletti (ajustado al coeficiente 0.8 y sistema de turnado)

#EfMendoza$Q1   <- EfMendoza[, ifelse((!is.na("TierraQlmedio") >= !is.na("TierraQl") & !is.na("TierraQlmedio") >= !is.na("TierraQlrep")),"TierraQlmedio",  ifelse((!is.na("TierraQl") >= !is.na("TierraQlmedio") & !is.na("TierraQl") >= !is.na("TierraQlrep")),"TierraQl",ifelse(!is.na("TierraQlrep"),"TierraQl","TierraQlrep")))]
glimpse(EfMendoza)
}

# Caudal19 <- c(590,490,140,850,460,460)
#names(Caudal19) <- c("Atuel","Diamante","Malargüe","Mendoza","Tun.Inferior","Tun.Superior")
  
# Working with databases ----

# Caudales ----

{ ## @knitr CaudalWeb
  
#CaudalWeb <- 
 # bind_rows(
  #  read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/a5ab3304-b340-4164-b484-1a807bc6fd90/download/caudales-semanal-irrigacion-noviembre-2018.csv", sep = ";"),
   # read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/f7d4a8bf-421f-4a32-869f-5d45aa2c7bfa/download/caudales-semanal-irrigacion-octubre-2018.csv", sep=";"), 
#    read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/fd515be8-1eb0-47d7-864e-47747f53a446/download/caudales-semanal-irrigacion-septiembre-2018.csv", sep=";"),
 #   read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/4ad42b42-403b-4766-94e8-ef2791da1c77/download/caudales-semanal-irrigacion-agosto-2018.csv", sep=";"),
  #  read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/59bfa818-80d2-4023-9f7e-5802173f4190/download/caudales-semanal-irrigacion-julio-2018.csv",sep = ";"),
   # read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/a6cd9597-298c-417a-affd-66d09637f005/download/caudales-semanal-irrigacion-junio-2018.csv",sep = ";"),
    #read.csv("http://datosabiertos.mendoza.gov.ar/dataset/ce02de3f-7ac8-48c8-9315-13a31175d0fc/resource/7034112a-f095-4c53-b7ef-70047715a057/download/caudales-irrigacion-25-02-al-01-04.csv",sep = ";"),
#    read.csv("http://datosabiertos.mendoza.gov.ar/dataset/6caa8224-d0bf-4ca4-9c31-52fa08472c7d/resource/c8000caa-01ad-4e01-afc9-861acf47b5b2/download/caudales-semanal-irrigacion-febrero-2019.csv",sep = ";"),
 #   read.csv("http://datosabiertos.mendoza.gov.ar/dataset/6caa8224-d0bf-4ca4-9c31-52fa08472c7d/resource/d430e333-a54b-4528-b4db-982f61f04f79/download/caudales-semanal-irrigacion-enero-2019.csv",sep = ";")
    #read.csv("",sep = ";"),
    #read.csv("",sep = ";")
    #pre_iaaf %>% select(time, athlete, nationality:date) %>% mutate(era = "Pre-IAAF"),
  #)
}

CaudalWeb <- read.csv("/Users/SebastianRiera/Google Drive/Biblio/Base de datos/DGI/CaudalesWeb.csv", sep = ",")
#CaudalWeb <- read.csv("DgiData/CaudalWeb.csv", sep = ",")
#glimpse(CaudalWeb)

CaudalWeb <-  CaudalWeb %>% #clean_names() %>%   #as_tibble() %>%
  mutate(fecha_desde = ymd(fecha_desde)) %>%  mutate(fecha_fin = ymd(fecha_fin))
colnames(CaudalWeb)[2] <- "CodigoCauce"
glimpse(CaudalWeb)


CaudalWebQ <- do.call(data.frame, round(aggregate(CaudalWeb$valor_promedio_relevado/1000, 
                  by=list(CaudalWeb$CodigoCauce), 
                  FUN = function(x) c(Qwebav = mean(x), Qwebyear = sum(x) ))
                   ,3)) 
colnames(CaudalWebQ) <- c("CodigoCauce","Qwebav","Qwebyear") # m3/s
write_csv(CaudalWebQ, 'DgiData/CaudalWebQ.csv', na = "NA", append = FALSE, quote_escape = "double")

#var <- "Qanual"
#summarise(CaudalWeb, avg = mean(.data[[var]], na.rm = TRUE))
#CaudalWeb %>% 
# group_by(CaudalWeb$codigo_punto_medicion) %>% 
#mutate(Qanual = sum(CaudalWeb$valor_promedio_relevado, na.rm = T)) 

#CaudalWeb %>%  select(nombre_de_zona, codigo_punto_medicion, fecha_fin, valor_promedio_relevado,latitud, longitud) %>%
#  filter(nombre_de_zona=="SUBDELEGACION DEL RIO TUNUYAN SUPERIOR") %>% 
#  group_by(codigo_punto_medicion) 
#glimpse(CaudalWeb)



#CaudalWeb %>% select(nombre_de_zona,codigo_punto_medicion,valor_promedio_relevado, fecha_fin) %>%
#  filter(codigo_punto_medicion=="4019" & valor_promedio_relevado>=1) %>% 
#  ggplot(aes(fecha_fin,valor_promedio_relevado)) +  geom_point(alpha = 0.7) +
#  labs(title = "Caudal promedio", x = "fecha", y = "lt/seg",  caption = "Fuente: Datos abiertos Mendoza (2019)" )
  


# Obras 2017 ----

## @knitr DataObras17

Dgi2017 <- read.csv("DgiData/DgiObras2017inventario.csv", sep = ",") #, header = TRUE, sep=",")
Dgi2017a <- read.csv("DgiData/DgiObras2017.csv", sep = ",") 
head(Dgi2017,3)
glimpse(Dgi2017)
indxx <- c("NumObra","Expediente","CodigoCauce","inversion","Metros","MetrosAvance","Ano",
           "CaudalDiseno","Superficie","bm","Bm","Hm") # "CodigoCauceAlt",
Dgi2017[indxx] <- lapply(Dgi2017[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2017$inversion)

Dgi2017$metros <- ifelse(!is.na(Dgi2017$MetrosAvance) & (Dgi2017$MetrosAvance > Dgi2017$Metros), Dgi2017$MetrosAvance, Dgi2017$Metros)

Dgi2017$InvMt    <- as.numeric(round(Dgi2017$inversion/Dgi2017$metros, digits = 0))
Dgi2017$InvUsd   <- as.numeric(round(Dgi2017$inversion/17, digits = 1)) # $Ars/Usd 17
Dgi2017$InvMtUsd <- as.numeric(round(Dgi2017$InvUsd/Dgi2017$metros, digits = 1))

options(qwraps2_markup = "latex")
#Obras17 <- as.data.frame(Dgi2017)

Dgi2017 %>%
  filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & Status!="No se ejecuta") %>%
  select(CodigoCauce, Subdelegacion, inversion, metros, InvMt, InvMtUsd,Ano) %>% #, hectareas, Padrones
  mutate(coment= paste0(InvMtUsd," USD/metro revestido 2017")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(CodigoCauce) 

Mza17 <- as.data.table(Dgi2017 %>%
           filter(Subdelegacion=="Mendoza" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
           select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad,Obra, CaudalDiseno) %>% #
           arrange(CodigoCauce)) 

tSup17 <- as.data.table(Dgi2017 %>%
           filter(Subdelegacion=="Tunuyán Superior" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
           select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad,Obra, CaudalDiseno) %>% # 
           arrange(CodigoCauce)) 


# Obras 2018 ####

## @knitr DataObras18

Dgi2018 <- read.csv("DgiData/DgiObras2018.csv", sep = ",") #, header = TRUE, sep=",")

indxx <- c("hectareas","Padrones","LONGITUD","MONTO","MONTO.CONTRATO","Inversion","Ano") # Convert many columns in numeric #https://stackoverflow.com/questions/27528907/how-to-convert-data-frame-column-from-factor-to-numeric
Dgi2018[indxx] <- lapply(Dgi2018[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2018$MONTO)

Dgi2018$inversion <- as.numeric(ifelse(is.na(Dgi2018$MONTO),
                            ifelse(is.na(Dgi2018$MONTO.CONTRATO),
                              ifelse(is.na(Dgi2018$Presupuesto), " ", Dgi2018$Presupuesto),
                              Dgi2018$MONTO.CONTRATO),Dgi2018$Inversion))
summary(Dgi2018$Inversion, na.rm=T)
# Rename
#Dgi2018       <- Dgi2018 %>% rename(metros = LONGITUD) #, Inversion = MONTO)
names(Dgi2018)[12]<-"metros"
Dgi2018$metros

Dgi2018$InvMt    <- as.numeric(round(Dgi2018$inversion/Dgi2018$metros, digits = 0))
Dgi2018$InvUsd   <- as.numeric(round(Dgi2018$inversion/27.425, digits = 1)) # $Ars/Usd 27.425
Dgi2018$InvMtUsd <- as.numeric(round(Dgi2018$InvUsd/Dgi2018$metros, digits = 1))

#options(qwraps2_markup = "markdown")
options(qwraps2_markup = "latex")

#getOptions("qwraps2_frmt_digits", 1)

Obras18 <- as.data.frame(Dgi2018)
options(digits = 1)
SummaryObras <- list(
  "Inversión" = list(
    "prom.(d.s.)" = ~qwraps2::gmean_sd(InvUsd, 
       digits = getOption("qwraps2_frmt_digits", 0),na_rm = TRUE),#"mediana (Q1, Q3)" = ~qwraps2::median_iqr(inversion, na_rm = TRUE),
    "min" = ~round(min(InvUsd, na.rm = TRUE),0),
    "max" = ~round(max(InvUsd, na.rm = TRUE),0) ),
  "Metros rev." =    list(
    "promedio" = ~qwraps2::gmean_sd(metros, digits = getOption("qwraps2_frmt_digits", 1),na_rm = TRUE), #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
    "min" = ~round(min(metros, na.rm = TRUE),0),
    "max" = ~round(max(metros, na.rm = TRUE),0 ) ),
  "USD/mt" = list(
    "promedio" = ~qwraps2::gmean_sd(InvMtUsd, digits = getOption("qwraps2_frmt_digits", 1),na_rm = TRUE), #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
    "min" = ~round(min(InvMtUsd, na.rm = TRUE),1),"max" = ~round(max(InvMtUsd, na.rm = TRUE),1) ) )

Dgi2018 %>%
  filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & ESTADO!="NO EJECUTADA") %>%
  select(CodigoCauce, Subdelegacion, hectareas, Padrones, inversion, metros, InvMtUsd,Ano) %>%
  mutate(coment= paste0(InvMtUsd," USD/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(Subdelegacion,InvMtUsd) 

Mza18 <- as.data.table(Dgi2018 %>%
              filter(Subdelegacion=="Mendoza" & ESTADO!="NO EJECUTADA" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
              select(CodigoCauce,Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad, Obra,Tipologia,CaudalDiseno) %>% # , CodigoCauceAlt
              arrange(CodigoCauce)) 
tSup18 <- as.data.table(Dgi2018 %>%
             filter(Subdelegacion=="Tunuyán Sup." & ESTADO!="NO EJECUTADA" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
             select(CodigoCauce,Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad, Obra,Tipologia,CaudalDiseno) %>% # , CodigoCauceAlt
             arrange(CodigoCauce)) 

#Mza18 <- merge(x= Mza18, y= EfMendoza[ , c(2,6,9,12,15,18:19,24,27) ], by= c("CodigoCauce"), all.x= TRUE)

## @knitr Sum2018
options(digits = 1)
print(qwraps2::summary_table(  dplyr::group_by(Obras18, Subdelegacion),
  SummaryObras),
rtitle = "Resumen de obras 2018",
cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tunuyán Inf.","Tunuyán Sup.")
)


## @knitr OtherSum2018
library(Amisc)
pander::pandoc.table(Amisc::describeBy( data = Obras18,
  var.names = c("inversion", "Padrones", "hectareas","InvMt"),
  by1 = "Subdelegacion",   #dispersion = "sd", Missing = TRUE,
  stats = "non-parametric" ),
split.tables = Inf )

## @knitr SummaryBase
# List of lists to replicate the analysis

Obras18 %>%
  dplyr::select(.data$inversion, .data$Padrones, .data$hectareas, .data$InvMt) %>%
  qsummary(.)

ejemplo  <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryObras)
# ejemplo1 <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryBase)
ejemplo2 <- summary_table(Obras18,SummaryObras)

#Obras18 %>%

AltSum18 <- Dgi2018 %>%
  dplyr::select(.data$inversion, .data$InvMt, .data$hectareas) %>%
  qsummary(.)#,
           # numeric_summaries = list("Minimum" = "~ min(%s)",
           #                        "Maximum" = "~ max(%s)"),
           # n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))

## @knitr PrintSumBase18
options(digits = 1, scipen=999)
print(ejemplo,
      rtitle = "Resumen de obras 2018",
      cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior"),
      align = c("l",rep("r",6)))

## @knitr PrintAltSum18
AltSum18


# Obras 2019 ####
## @knitr DataObras19

Dgi2019 <- read.csv("DgiData/DgiObras2019.csv", sep = ",", stringsAsFactors=FALSE) # 

Dgi2019$metros <- as.numeric(as.character(Dgi2019$Metros))

Dgi2019$inversion <- as.numeric(as.character(Dgi2019$Inversion)) 
Dgi2019$InvMt     <- as.numeric(round(Dgi2019$inversion/Dgi2019$metros, digits = 1))
Dgi2019$InvUsd    <- as.numeric(round(Dgi2019$inversion/43.8, digits = 1)) # $Ars/Usd 43.8
Dgi2019$InvMtUsd  <- as.numeric(round(Dgi2019$InvUsd/Dgi2019$metros, digits = 1))

Dgi2019 %>%
  filter((metros!="NA") & (inversion!="NA") & Status!="No se ejecuta") %>% #(Inversion!="" & Inversion!="-")) %>%
  select(CodigoCauce, Subdelegacion, Cauce, Inspeccion,Obra,inversion, metros, Tipologia,InvMtUsd,Ano) %>% #, hectareas, Padrones
  mutate(coment= paste0(InvMtUsd," USD/metro revestido 2019")) %>%
  arrange(CodigoCauce)

Mza19 <- as.data.table(Dgi2019 %>%
      filter(Subdelegacion=="Mendoza" & Status!="No se ejecuta" &( metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
      select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad, Obra,Tipologia,CaudalDiseno) %>% # , CodigoCauceAlt
      arrange(CodigoCauce)) 

tSup19 <- as.data.table(Dgi2019 %>%
           filter(Subdelegacion=="T. Superior" & Status!="No se ejecuta" &( metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
           select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad, Obra,Tipologia,CaudalDiseno) %>% # , CodigoCauceAlt
           arrange(CodigoCauce)) 


## @knitr Sum2019
print(qwraps2::summary_table(
  dplyr::group_by(Dgi2019, Subdelegacion),
  SummaryObras
),
rtitle = "Resumen de obras 2019",
cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior"),
align = c("l",rep("r",6)) )

# Obras 2020 ####
## @knitr DataObras20

Dgi2020 <- read.csv("DgiData/DgiObras2020.csv", sep = ",") 
head(Dgi2020,3)
glimpse(Dgi2020)
Dgi2020 <- Dgi2020[, c(1:13)]
indxx <- c("NumObra","CodigoCauce","Inversion","Metros","Beneficiarios","Ano","Superficie") # "CodigoCauceAlt",
Dgi2020[indxx] <- lapply(Dgi2020[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2020$Inversion)

names(Dgi2020)[8] <-"inversion"
names(Dgi2020)[12] <-"metros"

Dgi2020$InvMt    <- as.numeric(round(Dgi2020$inversion/Dgi2020$metros, digits = 0))
Dgi2020$InvUsd   <- as.numeric(round(Dgi2020$inversion/70, digits = 1)) # $Ars/Usd 64
Dgi2020$InvMtUsd <- as.numeric(round(Dgi2020$InvUsd/Dgi2020$metros, digits = 1))

Dgi2020 %>%
  filter(Subdelegacion=="Mendoza" & is.numeric(metros) & !is.na(metros)) %>% #metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & Status!="No se ejecuta") %>%
  select(CodigoCauce, Subdelegacion, inversion, metros, InvMt, InvMtUsd,Ano) %>% #, hectareas, Padrones
  #mutate(comment= paste0(InvMtUsd," USD/metro revestido 2017")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(Subdelegacion,InvMtUsd) 

Mza20 <- as.data.table(Dgi2020 %>%
          filter(Subdelegacion=="Mendoza" & (is.numeric(metros) & !is.na(metros))) %>%
          select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad,Obra) %>% #
          arrange(InvMtUsd)) 

tSup20 <- as.data.table(Dgi2020 %>%
            filter(Subdelegacion=="T. Superior" & (is.numeric(metros) & !is.na(metros))) %>%
            select(CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad,Obra) %>% # 
            arrange(CodigoCauce)) 


# Obras Full ----
## @knitr DataObrasFull 


DgiFull <- read.csv('DgiData/DgiObrasCompleta.csv', sep = ",") 
DgiFull <- DgiFull[,c(1:17)]
names(DgiFull)[9] <-"metros"
names(DgiFull)[10]<-"inversion"
indxx <- c("Ano","Codigo","CodigoCauce","metros","inversion","hectareas","Padrones","TdC") # Convert many columns in numeric #https://stackoverflow.com/questions/27528907/how-to-convert-data-frame-column-from-factor-to-numeric
DgiFull[indxx] <- lapply(DgiFull[indxx], function(x) as.numeric(as.character(x)))

# Clasificación: Canal (1), Rama (2), Hijuela (3), Ramo (4), otro (5)

DgiFull$clasif <- ifelse(grepl(" Hijuelas | Hijuela | Hij. | H. | H.", DgiFull$Obra, ignore.case = TRUE),3,
  ifelse(grepl(" C.| Canal | Can. | C. | Cnl ", DgiFull$Obra, ignore.case = TRUE),1,
    ifelse(grepl(" Rama | Ram | R.| R. ", DgiFull$Obra, ignore.case = TRUE),2,
    ifelse(grepl("Ramo | Ram. | R.", DgiFull$Obra, ignore.case = TRUE),4,5))))
DgiFull$clasif <- ifelse(grepl("Hiejuelas|Hijuela|Hij.|H.", DgiFull$Obra, ignore.case = TRUE),3,
   ifelse(grepl("Canal|Can.|C.|Cnl", DgiFull$Obra, ignore.case = TRUE),1,
    ifelse(grepl("Rama|Ram|R.", DgiFull$Obra, ignore.case = TRUE),2,
    ifelse(grepl("Ramo|Ram.|R.", DgiFull$Obra, ignore.case = TRUE),4,5))))
DgiFull$clasif <- factor(DgiFull$clasif, levels = c(1:5),
       labels = c('Canal','Rama','Hijuela','Ramo','otro'))
DgiFull %>% select(CodigoCauce,clasif,Obra,Subdelegacion) %>% filter(Subdelegacion=='Tun. Superior') %>% arrange(CodigoCauce,clasif)


DgiFull$InvMt    <- as.numeric(round(DgiFull$inversion/DgiFull$metros, digits = 0))
DgiFull$InvUsd   <- as.numeric(DgiFull$inversion/DgiFull$TdC)
#DgiFull$InvUsd   <- as.numeric(ifelse(DgiFull$Ano==2017,round(DgiFull$inversion/17, digits = 1),ifelse(DgiFull$Ano==2018,round(DgiFull$inversion/27.425, digits = 1),ifelse(DgiFull$Ano==2019,round(DgiFull$inversion/43.8, digits = 1),ifelse(DgiFull$Ano==2020,round(DgiFull$inversion/70, digits = 1),"NA"))))) 
DgiFull$InvMtUsd <- as.numeric(round(DgiFull$InvUsd/DgiFull$metros, digits = 1))

options(qwraps2_markup = "latex")
# Cuadro summary
ObrasFull <- DgiFull %>% select(Ano,CodigoCauce,Subdelegacion,inversion,InvUsd,InvMtUsd,metros,Padrones,hectareas) %>%
             filter(InvUsd>0 & metros>0 & InvMtUsd>0)

DgiFull %>%
  filter(metros!= "Global" | metros>0 ) %>% #!="-" | metros!="a determinar" | metros!="NA" ) %>%
  select(CodigoCauce, Subdelegacion, hectareas, inversion, metros, InvMtUsd,Ano) %>%
  mutate(coment= paste0(InvMtUsd," USD/metro revestido")) %>% 
  arrange(Subdelegacion,InvMtUsd) 

## @knitr SumFull
options(digits = 1)
print(qwraps2::summary_table(dplyr::group_by(ObrasFull, Subdelegacion),
                               SummaryObras), 
  align = c("l",rep("r",6)),
  cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior"),
  caption = "\\label{SumFull}Resumen de obras 2017-2020 (USD)", #footnote("T. Superior e Inferior obras 1999-2020")
)


## @knitr SummaryBaseFull
# List of lists to replicate the analysis
DgiFull %>%
  dplyr::select(.data$inversion, .data$Padrones, .data$hectareas, .data$InvMt) %>%
  qsummary(.)

ejFull  <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryObras)


# Mendoza ----

## @knitr MdzEf
Dgi2018 %>%
  filter(metros!= "Global" | metros!="-" | metros!="a determinar" | metros!="NA" & Subdelegacion=="Mendoza") %>%
  select(Codigo, hectareas, inversion, metros, InvMt, InvMtUsd, Obra) %>% #, Padrones
  #mutate(coment= paste0(InvMtUsd," USD/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(InvMtUsd) 

## @knitr MendozaTable


Mendoza <- rbind(Mza17,Mza18,Mza19,Mza20, fill=TRUE)

Mendoza <- merge(x= Mendoza,
    y= EfMendoza[ , c(2:4,6,13,15,17:21,24:28,33:34) ], #EfMendoza[ , c(2:4,6,13,15,17:21,24:29) ], 
    by= c("CodigoCauce"), all.x=TRUE)
Mendoza <- merge(x= Mendoza,
    y= CaudalWebQ, 
    by= c("CodigoCauce"), all.x=TRUE)

umendoza <- read.csv('DgiData/SanchezSuperficies.csv', sep = ",")
glimpse(umendoza)

Mendoza <- merge(x= Mendoza,
    y= umendoza, 
    by= c("CodigoCauce"), all.x=TRUE)

library(sf)
cuenca  <- st_read("/Users/SebastianRiera/Downloads/cuencas_unidades_de_manejo/cuencas_unidades_de_manejo.shp")
#ctunsup <- cuenca %>% select(CUENCA, TIPO,NOMBRE,SUP_HA,ALT_MEDIA,PP_MED_A,P___MEDIA) %>% filter(CUENCA=="RÃ­o TunuyÃ¡n Superior")# & TIPO=='Cuenca')
{umendoza <- cuenca %>% filter(CUENCA=="RÃ­o Mendoza" & TIPO=='UM') %>% 
  select(NOMBRE,SUP_HA) %>% arrange(NOMBRE)

umendoza$Inspeccion <- as.numeric(as.character(NA))

umendoza$Inspeccion[1] <- 7 # 1ro Vistalba => Compuertas
umendoza$Inspeccion[2] <- 11 # Lujan oeste
umendoza$Inspeccion[6] <- 3 # Bajada de Araujo
umendoza$Inspeccion[10] <- 5 # "Céspedes"
umendoza$Inspeccion[13] <- 6 # chacras de coria 
umendoza$Inspeccion[16] <- 7 # Compuertas
umendoza$Inspeccion[18] <- 9  # Esteban => jocoli
umendoza$Inspeccion[19] <- 8  # Galignana
umendoza$Inspeccion[23] <- 9  # Jocoli
umendoza$Inspeccion[25] <- 2  # Algarrobal => LH01
umendoza$Inspeccion[33] <- 13  # Lunlunta
umendoza$Inspeccion[38] <- 14  # Marienhoff
umendoza$Inspeccion[39] <- 15  # Matus Hoyos
umendoza$Inspeccion[40] <- 5  # Mercery => cespedes
umendoza$Inspeccion[41] <- 17 # Morales Villanueva
umendoza$Inspeccion[42] <- 18  # Naciente Chachingo
umendoza$Inspeccion[43] <- 19 # Natalio Estrella
umendoza$Inspeccion[44] <- 20  # Ortega
umendoza$Inspeccion[51] <- 21 # san pedro y sp 
umendoza$Inspeccion[53] <- 9  # Santa Rita => Jocolí
umendoza$Inspeccion[55] <- 23  # Sobremonte
umendoza$Inspeccion[57] <- 24 # "Tulumaya"
umendoza$Inspeccion[58] <- 24 # "Tulumaya"
umendoza$Inspeccion <- ifelse(is.na(umendoza$Inspeccion),1,umendoza$Inspeccion)

umendoza$Inspeccion <- factor(umendoza$Inspeccion, 
  levels = c(1,2,3,5,6,7,9,11,13,14,15,17,18,20,21,23,24),
  labels = c('otro','Algarrobal',"Bajada de Araujo",'Céspedes','Compuertas','Chacras de Coria','Jocolí','Luján Oeste',
             'Lunlunta','Marienhoff','Mathus Hoyos','Morales Villanueva','Naciente Chachingo','Ortega','San Pedro y San Pablo','Sobremonte','Tulumaya'))
}

Mendoza <- merge(x= Mendoza, y= umendoza[,c(2,4)], 
          by= c("Inspeccion"), all.x=TRUE)
Mendoza <- Mendoza %>% distinct(inversion, Obra, .keep_all = TRUE) %>%  select(everything(),-geometry)
arrange(Mendoza,CodigoCauce)


# Eficiencia post entubamiento ==1 & revestimiento 0.99
Mendoza$EfPost     <- Mendoza[, ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Mendoza$Obra), 1,
  ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Mendoza$Obra), 0.995,
  0.99))]
Mendoza$EfAnte     <- Mendoza[, ifelse(!is.na(EfCanales) & grepl("C.|Canal|Canales | Can. | Canal. | Cl | canal", Mendoza$Obra), EfCanales,
  ifelse(!is.na(EfHijuelas) & grepl("H.| Hijuela | Hij. | Hij | Hj | hijuela | HIJUELA | HIJ.", Mendoza$Obra), EfHijuelas,
  EfUnidadManejo))]

# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento

# EfTierraLong/KmTierra es eficiencia x kilómetro en la UM
Mendoza$PerdidaxKm        <- round(Mendoza[, (Q0) * EfTierraLong/KmTierra ], digits = 3) 
Mendoza$DeltaPerdida      <- round(Mendoza[, (PerdidaxKm * EfPost * metros/1000 * 4147200)], digits=0) 

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200

# Volumen anual de agua que pasa por los canales en Hm3
# enfoque de distancia media
options(digits = 4)
Mendoza$ahorroAgua <- round(Mendoza[ , (VolHa) * 1.65 * SUP_HA * EfTierraLong/KmTierra * EfPost * metros/1000], digits = 4) 
Mendoza$ahorroAgua1 <- round(Mendoza[ , (VolHa) * 1.65 * SUP_HA * (EfPost - EfTierraLong)/KmTierra * metros/1000], digits = 4) 
Mendoza$ahorroAgua2 <- round(Mendoza[ , (VolHa) * 1.65 * Hectareas * (EfTierraLong)/KmTierra * EfPost * metros/1000], digits = 4) 


# Cálculo en base a EfC
# 1ro: Caudal de entrada x (ganancia de eficiencia) / distancia del aforo en km
# 2do: kilómetros revestidos (metros/1000)
# 3ro: x segundos anuales de riego (1 turno semanal durante 8 meses)
#Mendoza$ACaudal       <- round(Mendoza[, (Q0) * (EfPost - EfAnte) / (Distancia)], digits = 2) # KmTierra # total inspección 
#Mendoza$ACaudalAnual  <- round(Mendoza[, (Q0) * (EfPost - EfAnte) / (Distancia)  *  metros/1000 * 2764800], digits=1) 

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo
Mendoza$ACaudalUm       <- round(Mendoza[, ifelse(!is.na(EfCanales) & !is.na(KmCanales) & grepl("Canal | Canales | Can. | Canal. | Cl | canal | C.", Mendoza$Obra),
    (Q0) * (EfPost - EfAnte) / (KmCanales),
    ifelse(!is.na(KmHijuela) & grepl("Hijuela | Hij. | Hij | Hj | hijuela | H.", Mendoza$Obra),
    (Q0) * (EfPost - EfAnte) / (KmHijuela),
    (Q0) * EfTierraLong / KmTierra ))], digits = 3) # KmTierra # total inspección 
# Obra Compuertas
Mendoza$ACaudalUm <- round(ifelse( Mendoza$inversion == 845000,       
    (Mendoza$Q0) * (Mendoza$EfPost - Mendoza$EfAnte) / (Mendoza$KmCanales), Mendoza$ACaudalUm), digits = 3) # KmTierra # total inspección 

Mendoza$ACaudalAnualUm  <- round(Mendoza[, ACaudalUm * metros/1000 * 4147200], digits=0) 

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200

Mendoza$valuePerd  <- round(Mendoza[, InvUsd / DeltaPerdida], digits = 3)
Mendoza$valueEf    <- round(Mendoza[, InvUsd / ACaudalAnualUm], digits = 3)
Mendoza$valueAho   <- round(Mendoza[, InvUsd / ahorroAgua], digits = 3)
Mendoza$valueAho1   <- round(Mendoza[, InvUsd / ahorroAgua1], digits = 3)
# adicional plus obras x administración
Mendoza$valuePerdBis <- round(Mendoza[, ifelse( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA",
    valuePerd, valuePerd * 1.32)], digits = 2)

Mendoza$valueEfBis <- round(Mendoza[, ifelse(Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", #grepl("Licitacion | Licitación | LICITADA", Mendoza$Modalidad),
    valueEf, valueEf * 1.32)], digits = 2)
Mendoza$valueAhorro <- round(Mendoza[, ifelse(Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", 
    valueAho, valueAho * 1.32)], digits = 2)
Mendoza$valueAhorro1 <- round(Mendoza[, ifelse(Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", 
    valueAho1, valueAho1 * 1.32)], digits = 2)

Mendoza$valueAho2   <- round(Mendoza[, InvUsd / ahorroAgua2], digits = 3)
Mendoza$valueAhorro2 <- round(Mendoza[, ifelse(Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", 
  valueAho2, valueAho2 * 1.32)], digits = 2)

Mendoza <- Mendoza[order(Ano,valueAhorro)]

#write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")
write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorroNov.csv', na = "NA", append = FALSE, quote_escape = "double")


MzaTableComparacion <- as.data.frame(Mendoza[ ,c(10,13,9,5,7,33,32,34:36, 42:44)]) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTableComparacion}Río Mendoza - Comparación metodologías", align = c("l", "c",rep("r", 12)),
    row.names = FALSE, booktabs = TRUE,  
    col.names = c("Obra","Zona","Modalidad","Metros","USD/mt","ex-ante","ex-post","pérdida","EfC",'nuevo',"perd","efc","nuevo")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  add_header_above(c(" "=5,"Ef.Conducción" =2, "Ahorro m3/km" = 3,  "USD/m3 (ajuste)" = 3)) %>%
  pack_rows("2017", 1,8) %>%
  pack_rows("2018", 9,11) %>%
  pack_rows("2019", 12,17) %>%
  pack_rows("2020", 18,18) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

MzaTablePpt <- as.data.frame(Mendoza[ c(1:7,9:27), c(9,12,8,26,4:5,31,36)]) %>% 
  mutate_all(linebreak) %>% #caption = "\\label{tab:MzaTablePpt}Río Mendoza - Estimaciones preliminares", align = c("l", "c","c",rep("r", 4)),
  kable(format = "latex", #longtable=T, "repeat header",
    row.names = FALSE, booktabs = TRUE,  
    col.names = c("Obra","Zona","Modalidad","Caudal (m3/s)","Metros","USD/mt","Ahorro (m3)","USD/m3")) %>%
        #add_header_above(c(" "=5, "USD/m3 (ajuste)" = 2)) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center",full_width = FALSE, font_size=10) %>% # , full_width = FALSE
  pack_rows("2017", 1,7) %>% pack_rows("2018", 8,10) %>% pack_rows("2019", 11,16) %>% pack_rows("2020", 17,26) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", 
    footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


MzaTable <- Mendoza[ c(1:7,9:27), c(3,12,5:6,29,28,32:33,7,35)] %>% 
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTable}Río Mendoza - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 10)),
    row.names = FALSE, booktabs = TRUE,  col.names = c("Cauce","Zona","Metros","Inv.(USD)","EfC(0)","EfC(1)","A(m3/seg)", "A(m3/Obra)","Inv.(USD/mt)","USD/m3")
        #col.names = linebreak(c("Cauce","Zona","Metros","Inversión\\\\(USD)","Inversión \\\\(USD/mt)","Ef.Cond\n (previa)","Ef.Cond.\n(post)","Ahorro\n($$m^3/seg$$)", "Ahorro\n(Hm3/Obra)","USD/Hm3"), 
         #           align = "c", linebreaker = "\n", double_escape = F) #,"Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
  ) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=12) %>% # latex_options = c("striped", "scale_down")
  pack_rows("2017", 1,7) %>%
  pack_rows("2018", 8,10) %>%
  pack_rows("2019", 11,16) %>%
  pack_rows("2020", 17,26) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
    footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) #, longtable=T

## @knitr MzaCaudales
Mendoza %>% select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qwebav,Qwebyear,KmTierra,metros,PerdidaxKm, ACaudalUm,VolHa,EfPost,EfAnte) %>%
  #select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  mutate(GananciaEfC= paste0(GananciaEfC= (EfPost-EfAnte)*100,"%")) %>%
  arrange(Ano) %>% 
  select(CodigoCauce, Obra,Q0,CaudalDiseno, Qwebav,VolHa,GananciaEfC) %>%
  mutate_all(linebreak) %>%
  mutate_all(funs(replace_na(., "-"))) %>%
  kable(format = "latex",caption = "\\label{tab:MzaCaudales}Mendoza - Caudal por obra información disponible (m3/s)", align = c("c", "l",rep("c", 10)),
    row.names = FALSE, booktabs = TRUE,  col.names = c("Código","Obra", "Utilizado","Diseño","Web",'m3/año',"GananciaEfC")) %>% 
footnote( general = "Elaboración propia en base a DGI (2015), Datos abiertos (2019) y entrevistas (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
    footnote_as_chunk=TRUE, escape=FALSE, threeparttable = T)
#landscape()


## @knitr MzaTableComparacion
MzaTableComparacion %>% landscape()

## @knitr MzaTablePpt
MzaTablePpt #%>% landscape()

## @knitr MzaTable
MzaTable

## @knitr MzaTableLandscape
MzaTable %>% 
  landscape()

## @knitr MendozaPlots
# teoría acorde
# Heckscher diagram (1918) order-of-merit. Dynamic tructural analysis of Salter (1961)
OfertaMza <- Mendoza[order(valueEfBis)]
OfertaMza$AAcum   <- cumsum((OfertaMza$ACaudalAnual)) 
OfertaMza1 <- Mendoza[order(valuePerdBis)]
OfertaMza1$AAcum   <- cumsum((OfertaMza1$DeltaPerdida)) 
OfertaMza2 <- Mendoza[order(valueAhorro)]
OfertaMza2$AAcum   <- cumsum((OfertaMza2$ahorroAgua)) 
OfertaMza2[order(valueAhorro1)]
OfertaMza2$AAcum1   <- cumsum((OfertaMza2$ahorroAgua1)) 
OfertaMza2[order(valueAhorro2)]
OfertaMza2$AAcum2   <- cumsum((OfertaMza2$ahorroAgua2)) 


OfertaMza2 %>% select(ahorroAgua,ahorroAgua2,valueAhorro,valueAhorro2,AAcum,AAcum2,SUP_HA,Hectareas,Inspeccion,NodoDistribucion)

round(mean(OfertaMza$valueEfBis, na.rm=T), digits = 3)
CProm <- round(mean(OfertaMza2$valueAhorro, na.rm=T), digits = 3)
AhorroCtoPromedio <- OfertaMza1[[17,38]]
AhorroStock <- AhorroCtoPromedio/.12

#library(Hmisc)
#demand <- Hmisc::bezier('xdem' = c(1, 70, 150),
                        #'ydem' = c(CProm,CProm,CProm)) %>% as_data_frame()

#require(greppel)
AhorroMza <- ggplot(OfertaMza) + # , color=Zona
  geom_step(aes(y= valueEfBis, x=AAcum/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueEfBis, x=AAcum/1000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum

AhorroMza1 <- ggplot(OfertaMza1) + # , color=Zona
  geom_step(aes(y= valuePerdBis, x=AAcum/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/1000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=-.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaMza1$AAcum/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum

AhorroMza2 <- ggplot(OfertaMza2) + # , color=Zona
  geom_step(aes(y= valueAhorro, x=AAcum/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueAhorro, x=AAcum/1000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=-.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaMza2$AAcum/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum


## @knitr AhorroMza
AhorroMza + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
            axis.text.y = element_text(size = 10),
            panel.background = element_rect(fill = "white"), 
            axis.title = element_text(size = 9)) + 
            scale_y_continuous(breaks = c(seq(0,80,5))) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado") # Hm^{3}
#+  geom_ribbon(data=subset(x, 2 <= x & x <= 3), aes(ymin=twox,ymax=x2), fill="blue", alpha="0.5") 
ggsave('DgiData/Graphs/OfertaMza.png', height = 4, width = 12)

## @knitr AhorroMzaPerd
AhorroMza1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                  axis.text.y = element_text(size = 10),
                  panel.background = element_rect(fill = "white"), 
                  axis.title = element_text(size = 9)) + 
  scale_y_continuous(breaks = c(seq(0,12,3),20,30,40,50,60)) + 
  theme(axis.line = element_line(colour = "grey50")) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMzaPerd.png', height = 4, width = 12)

## @knitr AhorroMzaPerd2
AhorroMza2 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10),
                   panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  theme(axis.line = element_line(colour = "grey50")) + 
#  geom_path(data = demand, color = "#FF4036", size = 1,aes(x, y)) + #
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMzaAhorro.png', height = 10, width = 12)

## @knitr CtoPromedio
cat(CProm)

## @knitr AhorroCtoPromedio
cat(AhorroCtoPromedio)

## @knitr AhorroPromBis
cat(round(AhorroCtoPromedio/1000000, digits = 2))

## @knitr AhorroStock
cat(AhorroStock)

## @knitr AhorroStockBis
cat(round(AhorroStock/1000000, digits = 2))

## @knitr MendozaInvTables
OfertaMzaInv          <- as.data.frame(OfertaMza1[order(AAcum)])
OfertaMzaInv$InvAcum  <- cumsum((OfertaMzaInv$InvUsd)) 

SumInv <- OfertaMzaInv %>%
  dplyr::select(.data$InvAcum, .data$metros, .data$AAcum) %>%
  qsummary(.)


MzaSum <-  rbind(round(OfertaMzaInv$InvAcum[[18]], digits = 0),
                  round(OfertaMzaInv$AAcum[[18]]/1000,digits = 1),
                  round(OfertaMzaInv$AAcum[[18]]/2073600,digits = 3))
rownames(MzaSum) = c("Inv. USD","Ahorro agua ('000 m3)","Caudal ahorrado (m3/s)")


## @knitr MendozaInv
MzaSum %>% 
  kable("latex",caption = "\\label{MendozaInv}Resumen subdelegación Mendoza", booktabs = TRUE, #align = c("l"),
        #row.names = c("Inv. USD","Ahorro agua ('000 m3)","caudal(m3/s)"), 
        col.names = c("Primeras inversiones") 
  ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elaboración propia", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE, threeparttable = T)



# T.Superior ----

## @knitr SupEf
options(digits = 3)
EfSuperior <- as.data.frame(read.csv("DgiData/EfConduccion/TunuyanSuperior/EfCondTunSup.csv", sep = ",")) #, header = TRUE, sep=",")
indxx <- c("CodigoCauce","Superficie","LongTotal","LongRevest","EfRevest","LongHijuelas","PorcRevest","EfTierra",
           "EfGlobal","Caudal","CoefMoritz","PerdidaPorcentaje","TiempoMojado","PerdidaTiempo","EfUM")
EfSuperior[indxx] <- lapply(EfSuperior[indxx], function(x) round(as.numeric(as.character(x)), 3))

EfSuperior <- spread(EfSuperior,Clasificacion, Caudal)
#EfSuperior <- EfSuperior[ , c(1:18,20:22) ]
#EfSuperior %>% select(CodigoCauce,CaudalObrador,CaudalBalance,caudalCanal:caudalRama) %>%
#  filter(CodigoCauce==5701)
colnames(EfSuperior)[20:23] <- c('caudalCanal','caudalHijuela','caudalTierra','caudalRama')

Superior <- DgiFull %>% filter(Subdelegacion=="Tun. Superior", metros>0, !is.na(CodigoCauce))

Superior <- merge(x= Superior,
             y= EfSuperior[ , c(1:7,9:10,15:23)], by= c("CodigoCauce"), all.x=TRUE)

longCanalesTipo <- as.data.frame(read.csv('DgiData/longCanalesTipo.csv', sep = ","))
colnames(longCanalesTipo)[3:8] <- c('mtTierra','mtHormigon','mtEntubado','mtPvc','mtChapa','mtSD')
glimpse(longCanalesTipo)
class(longCanalesTipo$CodigoCauce)
longCanalesTipo$CodigoCauce <- as.factor(longCanalesTipo$CodigoCauce)

Superior <- Superior %>% select(everything(),-Subdelegacion,-Tipologia,-Financiamiento,-Estado,-Expediente,-Padrones,-Observaciones,-FECHA.REC..PROVISORIA) %>% 
  distinct(inversion, Obra, .keep_all = TRUE)
glimpse(Superior)
Superior <- merge(x= Superior, y= longCanalesTipo[, c(1,3:8)], by = c("CodigoCauce"), all.x = TRUE)

Superior <- Superior %>% mutate(kmTotal=((mtTierra + mtHormigon + mtEntubado + mtPvc + mtChapa + mtSD)/1000))

Superior %>% select(CodigoCauce,Ano,metros,LongTotal,kmTierraBalance,kmTotal,LongRevest,mtTierra:mtSD,Obra) %>% arrange(CodigoCauce,Ano) 
Superior$LongTotal <- ifelse(!is.na(Superior$kmTotal) & Superior$kmTotal > Superior$LongTotal, Superior$kmTotal,
        ifelse(Superior$kmTierraBalance > Superior$LongTotal,Superior$kmTierraBalance,Superior$LongTotal))

summary(Superior$LongTotal)
# Eficiencia post entubamiento ==1 & revestimiento 0.99
Superior$EfPost  <- ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Superior$Obra), 0.995,
    ifelse(grepl("Mej. | Mejora", Superior$Obra), 0.99, 
    ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Superior$Obra), 1,  0.98)))

# check if any is NA
# no hay missing en EfTierra => ver efC ==1!!!
Superior %>% select(CodigoCauce,Obra,EfTierra,EfGlobal,EfUM,caudalCanal:caudalRama,CaudalObrador,CaudalBalance)


#Superior$EfAnte     <- round(ifelse(!is.na(Superior$EfTierra),Superior$EfTierra,ifelse(!is.na(Superior$EfGlobal),Superior$EfGlobal,ifelse(is.na(Superior$EfUM),mean(Superior$EfAnte,na.rm = T),Superior$EfUM))), digits = 3)
Superior$EfAnte <- round(ifelse( (Superior$EfGlobal < Superior$EfTierra), Superior$EfTierra,
          ifelse((Superior$EfGlobal < Superior$EfUM), Superior$EfUM,  Superior$EfGlobal)),3)
                 #quantile(Superior$EfTierra, 0.25,na.rm = T))), digits = 3)
#Superior$EfAnte <- ifelse(Superior$EfAnte==1.0, quantile(Superior$EfAnte, 0.25,na.rm = T), Superior$EfAnte)
#[, ifelse(!is.na(EfCanales) & grepl("C. | Canal | Canales | Can. | Canal. | Cl | canal", Superior$Obra), EfCanales,ifelse(!is.na(EfHijuelas) & grepl("H. | Hijuela | Hij. | Hij | Hj | hijuela | HIJUELA | HIJ.", Superior$Obra), EfHijuelas, EfUM))]


# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento
arrange(Superior, CodigoCauce)
Superior <- merge(x= Superior, y= CaudalWebQ, by= c("CodigoCauce"), all.x=TRUE)
Superior <- Superior %>% select(everything()) %>% distinct(inversion, Obra, .keep_all = TRUE)
glimpse(Superior)

Superior$Q0 <- ifelse(Superior$clasif=='Canal' & !is.na(Superior$caudalCanal),Superior$caudalCanal,
    ifelse(Superior$clasif=='Rama' & !is.na(Superior$caudalRama), Superior$caudalRama,
    ifelse(Superior$clasif=='Hijuela' & !is.na(Superior$caudalHijuela), Superior$caudalHijuela,
    Superior$caudalTierra)))
  
supqmax <- as.data.frame(
  Superior %>% select(CodigoCauce,Obra, clasif,Q0,Qwebav,CaudalBalance,CaudalObrador,caudalCanal,caudalHijuela,caudalRama,caudalTierra) %>% 
  replace(is.na(.), 0) %>% rowwise() %>% 
  mutate(qmax = max(Qwebav,CaudalObrador,caudalCanal,caudalHijuela,caudalRama,caudalTierra)) %>%
  select(CodigoCauce,clasif,qmax)
  )

Superior <- merge(x= Superior, y= supqmax, by= c("CodigoCauce",'clasif'), all.x=TRUE)
glimpse(Superior)
Superior %>% select(CodigoCauce,Obra,metros,LongTotal,Q0,qmax,Qwebyear, caudalCanal:caudalTierra,CaudalBalance,CaudalObrador,EfAnte)
Superior$qmax <- ifelse(Superior$CodigoCauce==9774 | Superior$CodigoCauce==9776 | Superior$CodigoCauce==5700 | Superior$CodigoCauce==5774,
        Superior$Qwebyear,Superior$qmax)

Superior$qmax <- ifelse(Superior$CodigoCauce==5701, 5.7, Superior$qmax)

# EfTierraLong/KmTierra es eficienci x kilómetro en la UM
Superior$PerdidaxKm   <-  round( (Superior$qmax) * Superior$EfAnte/Superior$LongTotal, digits = 3) 
Superior$DeltaPerdida <- round( (Superior$PerdidaxKm * Superior$EfPost * Superior$metros/1000 * 4147200), digits=0) 
summary(Superior$DeltaPerdida)

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200


# Cálculo en base a EfC
# 1ro: Caudal de entrada x (ganancia de eficiencia) / distancia del aforo en km
# 2do: kilómetros revestidos (metros/1000)
# 3ro: x segundos anuales de riego (1 turno semanal durante 8 meses)

#Superior$ACaudal       <- round(Superior[, (qmax) * (EfPost - EfAnte) / (Distancia)], digits = 2) # KmTierra # total inspección 
#Superior$ACaudalAnual  <- round(Superior[, (qmax) * (EfPost - EfAnte) / (Distancia)  *  metros/1000 * 6912000], digits=1) 

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo
#Superior$ACaudalUm    <- round( ((Superior$EfRevest * Superior$LongRevest) + (Superior$EfTierra * Superior$LongHijuelas)) / 
#                            (Superior$LongRevest + Superior$LongHijuelas), digits = 3)
#Superior$ACaudalAnualUm  <- round(Superior$Q0 * Superior$ACaudalUm * Superior$metros/1000 * 1036800, digits=0) 

#Superior %>% select(CodigoCauce, Q0,Caudal,Qwebav,ACaudalUm,Ano, Obra) %>% filter(is.na(ACaudalUm))
#EfSuperior %>% select(CodigoCauce, EfUM,LongTotal) %>% filter(CodigoCauce==c(5004,9707,9708,9713,9776,9777))
#EfSuperior$CodigoCauce
#EfSuperior %>% select(UnidadesManejo,CodigoCauce)

Superior$valuePerd  <- 0
Superior$valuePerd  <- round(Superior$InvUsd / Superior$DeltaPerdida, digits = 3)

Superior$valuePerdBis <- round( ifelse( #(Superior$Modalidad=="Lic." | Superior$Modalidad=="Licitación" | Superior$Modalidad=="Licitacion" | Superior$Modalidad=="LICITADA"),
  grepl("Lic. | Licitacion | Licitación | LICITADA", Superior$Modalidad), 
  Superior$valuePerd, Superior$valuePerd * 1.32), digits = 2)
Superior <- Superior %>% select(everything()) %>% distinct(Obra, .keep_all = TRUE)

Superior %>% select(CodigoCauce, Q0,qmax,Qwebyear,Ano,InvMtUsd,EfAnte,LongTotal,PerdidaxKm,DeltaPerdida,valuePerdBis, Obra) %>% arrange(valuePerdBis)
summary(Superior$valuePerdBis)

Superior <- arrange(Superior, Ano,valuePerdBis)
Superior$metros <- round(Superior$metros, 0)
  
write_csv(Superior, 'DgiData/Estimaciones/SupAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")

# estancado
SupTableComparacion <- #as.data.frame(Superior[ ,c(23,11,31,43,30,4, 22,39,  17,29,32,16,3,  9,8,4,6,26,25,29,31:35) ]) %>% #Superior[ ,c(9,8,4,6,26,25,29,31:35) ]) %>%
  Superior %>% select(Ano,Obra, Modalidad, metros,InvMtUsd,EfAnte,EfPost,DeltaPerdida,valuePerdBis) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTableComparacion}Tunuyán Superior - Comparación metodologías", align = c("l", "l",rep("r", 12)),
        row.names = FALSE, booktabs = TRUE,  
        col.names = c("Año","Obra","Modalidad","Metros","USD/mt","EfC_o","EfC_1","Ahorro m3/km","USD/m3")) %>% #col.names = c("Año","Obra","Modalidad","Metros","USD/mt","ex-ante","ex-post","Delta pérdida","Delta EfC","Pérdida","EfC","Pérdida")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  #add_header_above(c(" "=5,"Ef.Conducción" =2, "Ahorro m3/km" = 2, "USD/m3" = 2, "USD/m3 (ajuste)" = 1), align = "c") %>%
  #pack_rows("2017", 1,7) %>% pack_rows("2018", 8,11) %>%  pack_rows("2019", 12,14) %>%  pack_rows("2020", 15,17) %>%
  column_spec(1,bold = F) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

SupTable <- Superior %>%   #[ , c(9,4,5,26,25,30,31,6,35) ] %>% 
  select(Obra, metros,InvUsd,EfAnte,EfPost,PerdidaxKm,InvMtUsd,valuePerdBis) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTable}Tunuyán Superior - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Obra","Metros","Inv.(USD)","EfC_o","EfC_1","A(m3/km)", "Inv.(USD/mt)","USD/m3")) %>%
  kable_styling(latex_options = c("scale_down","HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>% # latex_options = c("striped", "scale_down")
  column_spec(1,bold = F) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  #pack_rows("2017", 1,7) %>% pack_rows("2018", 8,11) %>%  pack_rows("2019", 12,14) %>%  pack_rows("2020", 15,17) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Superior extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


## @knitr SupCaudales
  Superior %>% select(Ano,CodigoCauce, Obra, qmax,Qwebav,Qwebyear,CaudalBalance,Q0,metros,PerdidaxKm,EfPost,EfAnte) %>%
  #select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  mutate(GananciaEfC= paste0(GananciaEfC= round((EfPost-EfAnte)*100,4),"%")) %>%
  mutate(CaudalBalance=round(CaudalBalance,2),Qwebav=round(Qwebav,2),Q0=round(Q0,2),qmax=round(qmax,2)) %>%
  arrange(Ano,CodigoCauce) %>%
  select(CodigoCauce, Obra, Q0,Qwebav,CaudalBalance,qmax,GananciaEfC) %>%
  mutate_all(linebreak) %>% 
    mutate_all(funs(replace_na(., "-"))) %>% #mutate_if(is.numeric, funs(replace_na(., "-"))) %>%
  kable(format = "latex",caption = "\\label{tab:SupCaudales}Tunuyán Superior - Caudal promedio por obra información disponible (m3/s)", align = c("c", "l",rep("c", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Código","Obra","Informe.EfC","Web","Bal.2015", "Utilizado","GananciaEfC")) %>%
  kable_styling(latex_options = "scale_down") %>%
    footnote( general = "Elaboración propia en base a DGI (2015), Datos abiertos (2019) y entrevistas (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            number = "La información de caudales proviene del informe de eficiencia de conducción del T. Superior, datos disponibles en la web y balance hídrico 2015. Se utilizó el menor caudal reportado atendiendo a potenciales errores de medición", footnote_as_chunk=TRUE, escape=FALSE, threeparttable = T) #landscape()

## @knitr SupTableComparacion
SupTableComparacion %>% landscape()

## @knitr SupTable
SupTable

## @knitr SupTableLandscape
SupTable %>% 
  landscape()

## @knitr SuperiorPlots
OfertaSup <- arrange(Superior[,c("CodigoCauce","Ano","Obra","PerdidaxKm","DeltaPerdida","valuePerdBis","qmax","CaudalBalance","UnidadesManejo","InvUsd","InvMtUsd","TdC","metros","clasif")],valuePerdBis)
OfertaSup$AAcum   <- cumsum((OfertaSup$DeltaPerdida)) 
OfertaSup %>% select(Ano, Obra,CodigoCauce, PerdidaxKm,valuePerdBis,AAcum) #%>% filter(valueEfBis<=3)

#OfertaSup1 <- Superior[order(valuePerdBis)]
#OfertaSup1$AAcum   <- cumsum((OfertaSup$DeltaPerdida)) 
#require(greppel)
AhorroSup <- ggplot(OfertaSup) + # , color=Zona
  geom_step(aes(y= valuePerdBis, x=AAcum/1000000,colour=subSist),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/1000000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T) +#,nudge_x = -10, nudge_y = -10) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum/1000000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaSup$AhAcum


## @knitr AhorroSup
AhorroSup + theme(axis.text.x = element_text(size = 8, angle=0, vjust = .4), 
  axis.text.y = element_text(size = 8),
  panel.background = element_rect(fill = "white"), 
  axis.title = element_text(size = 9)) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #scale_x_continuous(breaks= round(OfertaSup$AAcum/1000000, digits = 0)) +
  xlab("hectómetros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado") # Hm^{3}
  #scale_x_continuous(breaks = c(seq(0,15000,9))) 
ggsave('DgiData/Graphs/OfertaSup.png', height = 4, width = 12)

  

# T. Inferior ----

## @knitr InfEf
DgiFull %>%
  filter(Subdelegacion=="Tun. Inferior",metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & Estado!="NO EJECUTADA") %>%
  select(CodigoCauce, Subdelegacion, hectareas, inversion, metros, InvMtUsd,Ano) %>%
  #mutate(coment= paste0(InvMtUsd," USD/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(Subdelegacion,InvMtUsd) 


tInf <- as.data.table(DgiFull %>%
        filter(Subdelegacion=="Tun. Inferior" & Estado!="NO EJECUTADA" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
        select(CodigoCauce,inversion, metros, InvUsd,InvMtUsd,Ano, Modalidad, Obra) %>% # , CodigoCauceAlt
        arrange(CodigoCauce)) 

EfInferior <- as.data.frame(read.csv("DgiData/EfConduccion/TunuyanInferior/EfCondInferior.csv", sep = ",")) #, header = TRUE, sep=",")
EfInferior <- EfInferior[,c(1:9)]
indxx <- c("UM","Cauce","CodigoCauce","EfC","Superficie","LongitudRed","Revestida","SinRevestir","RevestPorc")
EfInferior[indxx] <- lapply(EfInferior[indxx], function(x) as.numeric(as.character(x)))

## @knitr InferiorTable

Inferior <- tInf[ !is.na(CodigoCauce),]
Inferior <- merge(x= Inferior,
                  y= EfInferior,#[ , c(1:8,10:14,17:19) ], 
                  by= c("CodigoCauce"), all.x=TRUE)
arrange(Inferior,CodigoCauce)
Inferior <- Inferior %>% select(everything()) %>% distinct(inversion, Obra, .keep_all = TRUE)

# Eficiencia post entubamiento ==1 & revestimiento 0.99
Inferior$EfPost     <- Inferior[, ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Inferior$Obra), 1,
                            ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Inferior$Obra), 0.995,
                            ifelse(grepl("Mej. | Mejora", Inferior$Obra), 0.987,  0.98)))]
Inferior$EfAnte     <- Inferior$EfC/100 # ifelse(!is.na(Inferior$EfTierra),Inferior$EfTierra,Inferior$EfGlobal)



# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento
Inferior <- merge(x= Inferior,                   
                  y= CaudalWebQ,                   
                  by= c("CodigoCauce"), all.x=TRUE)
glimpse(Inferior)

Inferior$Q0   <- Inferior$Qwebyear
#Inferior$Q0   <- ifelse(is.na(Inferior$Qwebyear),mean(Inferior$Qwebyear,na.rm = T),Inferior$Qwebyear) 
summary(Inferior$Q0)

Inferior$PerdidaxKm   <- round(Inferior[, (Q0) * EfAnte/(LongitudRed/1000) ], digits = 3) # EfAnterior/ longitud de red (en metros) x kilómetro en la UM
Inferior$DeltaPerdida <- round(Inferior[, (PerdidaxKm * EfPost * metros/1000 * 4147200)], digits=0) 

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200

# Cálculo en base a EfC
# 1ro: Caudal de entrada x (ganancia de eficiencia) / distancia del aforo en km
# 2do: kilómetros revestidos (metros/1000)
# 3ro: x segundos anuales de riego (1 turno semanal durante 8 meses)
#Inferior$ACaudal       <- round(Inferior[, (Q0) * (EfPost - EfAnte) / (Distancia)], digits = 2) # KmTierra # total inspección 
#Inferior$ACaudalAnual  <- round(Inferior[, (Q0) * (EfPost - EfAnte) / (Distancia)  *  metros/1000 * 2764800], digits=1) 

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo
#Inferior$ACaudalUm       <- round(Inferior[, Q0 * ((EfRevest * Revestida) + (EfTierra * LongHijuelas))/ (LongRevest + LongHijuelas) ], digits = 3)
#summary(Inferior$ACaudalUm)
#Inferior$ACaudalAnualUm  <- round(Inferior[, ACaudalUm * metros/1000 * 1036800], digits=0) 

Inferior$valuePerd  <- round(Inferior[, InvUsd / DeltaPerdida], digits = 2)
#Inferior$valueEf    <- round(Inferior[, InvUsd / ACaudalAnualUm], digits = 2)

Inferior$valuePerdBis <- round(Inferior[, ifelse(Modalidad=="Lic." | Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA",
  valuePerd, valuePerd * 1.32)], digits = 2)
#Inferior$valueEfBis <- round(Inferior[, ifelse(Modalidad=="Lic." | Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", #grepl("Licitacion | Licitación | LICITADA", Inferior$Modalidad),                                              valueEf, valueEf * 1.32)], digits = 2)


Inferior <- Inferior[order(Ano,valuePerdBis)]

write_csv(Inferior, 'DgiData/Estimaciones/InfAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")


InfTable <- as.data.frame(Inferior[ , c(8,7,3,4,18,17,22:24)]) %>%
  mutate_all(linebreak) %>%
  mutate(EfPost=round(EfPost,1),EfAnte=round(EfAnte,2),PerdidaxKm=round(PerdidaxKm,2)) %>%
  kable(format = "latex",caption = "\\label{tab:InfTable}Tunuyán Inferior", align = c("l", "c",rep("r", 12)),
        row.names = FALSE, booktabs = TRUE,  
        col.names = c("Obra","Modalidad","Metros","USD/mt","ex-ante","ex-post","Pérdida","Delta pérdida","USD/m3")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  #add_header_above(c(" "=4,"Ef.Conducción" =2, "Ahorro m3/km" = 2, "USD/m3" = 2, "USD/m3 (ajuste)" = 2), align = "c") %>%
  pack_rows("2017", 1,7) %>%
  pack_rows("2018", 8,15) %>%
  pack_rows("2019", 15,22) %>%
  pack_rows("2020", 23,29) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


## @knitr InfCaudales
Inferior %>% select(Ano,CodigoCauce, Obra,Qwebav,Qwebyear,metros,PerdidaxKm,EfPost,EfAnte) %>%
  #select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  mutate(GananciaEfC= paste0(GananciaEfC= round((EfPost-EfAnte)*100,4),"%")) %>%
  mutate(EfPost=round(EfPost,2),EfAnte=round(EfAnte,2),Qwebav=round(Qwebav,2),Qwebyear=round(Qwebyear,2)) %>%
  arrange(Ano) %>%
  select(CodigoCauce, Obra,Qwebav,Qwebyear,EfPost,EfAnte,GananciaEfC) %>%
  mutate_all(linebreak) %>% 
  mutate_all(funs(replace_na(., "-"))) %>% #mutate_if(is.numeric, funs(replace_na(., "-"))) %>%
  kable(format = "latex",caption = "\\label{tab:InfCaudales}Tunuyán Inferior - Caudal promedio por obra información disponible (m3/s)", align = c("c", "l",rep("c", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Código","Obra","Q_prom","Q_anual","Ef1","Ef0","GananciaEfC")) %>%
  footnote( general = "Elaboración propia en base a DGI (2015), Datos abiertos (2019) y entrevistas (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE, threeparttable = T) #landscape()

## @knitr InfTable
InfTable 


## @knitr InferiorPlots

#OfertaSup <- Inferior[order(valueEfBis)]
#OfertaSup$AAcum   <- cumsum((OfertaSup$ACaudalAnual)) 
OfertaInf1 <- Inferior[order(valuePerdBis)]
OfertaInf1$AAcum   <- cumsum((OfertaInf1$DeltaPerdida)) 
#require(greppel)
#AhorroInf <- ggplot(OfertaInf) + # , color=Zona
#  geom_step(aes(y= valueEfBis, x=AAcum/10000),color = "#0073D9", size = 1) + 
#  geom_text(aes(y= valueEfBis, x=AAcum/10000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
#            vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
#  scale_x_continuous(breaks= round(OfertaSup$AAcum/10000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaSup$AhAcum

AhorroInf1 <- ggplot(OfertaInf1) + # , color=Zona
  geom_step(aes(y= valuePerdBis, x=AAcum/10000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/10000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=-.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaInf1$AAcum/10000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaSup$AhAcum

## @knitr AhorroInf
AhorroInf + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                  axis.text.y = element_text(size = 10),
                  panel.background = element_rect(fill = "white"), 
                  axis.title = element_text(size = 9)) + 
  #  scale_y_continuous(breaks = c(seq(0,80,5))) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
#ggsave('DgiData/Graphs/OfertaInf.png', height = 4, width = 12)

## @knitr AhorroInfPerd
AhorroInf1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10),
                   panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
#  scale_y_continuous(breaks = c(-5,seq(0,12,3),20,30,40,50,60)) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaInfPerd.png', height = 4, width = 12)




# General Summary table ----

## @knitr MainTables

#Rev2015 <- aggregate(Dgi2015$metros, by= list(Dgi2015$Subdelegacion), FUN=sum, na.rm= TRUE)
#Rev2016 <- aggregate(Dgi2016$metros, by= list(Dgi2016$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2017 <- aggregate(Dgi2017$metros, by= list(Dgi2017$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2018 <- aggregate(Dgi2018$metros, by= list(Dgi2018$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2019 <- aggregate(Dgi2019$metros, by= list(Dgi2019$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2020 <- aggregate(Dgi2020$metros, by= list(Dgi2020$Subdelegacion), FUN=sum, na.rm= TRUE)
#Rev2020 <- Rev2020[-1,]
  
#library(scales)
FormatoNum <- number_format(big.mark = ".", decimal.mark = ",")

Revestimiento <- data.frame(
  #`2015`=            FormatoNum(Rev2015$x),
  #`2016`=            FormatoNum(Rev2016$x),
  `2017`=            FormatoNum(Rev2017$x),
  `2018`=            FormatoNum(Rev2018$x),
  `2019`=            FormatoNum(Rev2019$x),`2020`= FormatoNum(Rev2020$x),
  `Total`=           FormatoNum(Rev2017$x + Rev2018$x + Rev2019$x + Rev2020$x)#,#colSums(Revestimiento[,1:2])),
  #`Ef.Cond.`=        FormatoNum(round(c(80,80,80,round(mean(EfMendoza[["EfUnidadManejo"]],na.rm = T),digits=2),80,80),digits = 3)),
  #`Q_m3.año`=        FormatoNum(c(0,0,0,sum(CaudalMen),0,0)),
  #`Ahorro_Hm3.año`= FormatoNum(round((Rev2017$x + Rev2018$x + Rev2019$x)* mean(EfMendoza[["EfUnidadManejo"]], na.rm = T)* sum(CaudalMen)/1000000,digits=3))
)
#colnames(Revestimiento) <- c(#"Subdelegacion", "2015","2016","2017",   "2018","2019","Total","Ef.Cond.","Q_(m3/año)","Ahorro (Hm3/año)")
rownames(Revestimiento) <- c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior")


## @knitr Revestimiento
Revestimiento[1:6,] %>% 
  kable("latex",caption = "\\label{Revestimiento}Metros revestidos por cuenca", align = c("l", rep("r", 6)),
        row.names = TRUE, booktabs = TRUE, col.names = c("2017","2018","2019","2020","Total"),linesep = "" #,"Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
        ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elab. propia en base a DGI (2020)", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)



# river maps ----

## @knitr spdataLoad

# probar estimar extension de 

redRiego <- st_read("/Users/SebastianRiera/Google Drive/Biblio/Base de mapas/DGI/v_red_riego_provincia/v_red_riego_provincia.shp")
glimpse(redRiego)
redRiego <- st_transform(redRiego, crs = st_crs(cuenca))
colnames(redRiego)[2] <- "CodigoCauce"

redRiego$id_cat <- factor(redRiego$id_cat, 
    levels = c("ALCANTARILLA","ARROYO", "CANAL","CANAL VIEJO","COLECTOR DE DESAGUE","DESAGUE","HIJUELA","HORMIGON","LINEA AUXILIAR","LINEA AUXILIAR A ELI","PROYECTADO","PUENTE CANAL","RAMA","RAMO","RIO","SIFON", "SIN DATO"),
    labels = c("Alcantarilla","Arroyo", "Canal","Canal Viejo","Colector De Desagüe","Desagüe","Hijuela","Hormigon","Linea Auxiliar","Linea Auxiliar A Eli","Proyectado","Puente Canal","Rama","Ramo","Rio","Sifon", "Sin Dato")
                          )
redRiego$id_mat <- factor(redRiego$id_mat, 
    levels = c("CHAPA", "ENTUBADO", "HORMIGON", "PVC", "SIN DATO", "TIERRA"),
    labels = c('Chapa','Entubado','Hormigon','PVC','Sin dato','Tierra')
                              )
redRiego$subdel <- factor(redRiego$subdel, 
    levels = c("RIO ATUEL","RIO DIAMANTE","RIO MALARGÜE","RIO MENDOZA","RIO TUNUYAN INFERIOR","RIO TUNUYAN SUPERIOR"),
    labels = c("Rio Atuel","Rio Diamante","Rio Malargüe","Rio Mendoza","Rio Tunuyan Inferior","Rio Tunuyan Superior")
                          )
redRiego$codigo = redRiego$CodigoCauce
redRiego$codigo = as.numeric(as.character(redRiego$codigo))
# 96744 observations

redRiego$denominacion <- as.character(redRiego$denominaci)
redRiego$subdelegacion <- as.character(redRiego$subdel)
redRiego$subSist <- ifelse(
  # Las Tunas
  grepl("ANCHAYUYO | LAS TUNAS | GUALTALLARY | ARROYO VILLEGAS | CANAL ESQUINA | CANAL ANCON | ANCON | ANCÓN | ESQUINA | TUPUNGATO | RIO DE LA PAMPA | QUEBRADA DE GUEVARA | EL PERAL | HIJ. PALMA | PALMA | EL INGENIO | ARROYO ALTO VERDE | ARROYO GUIÐAZU | ARROYO GUIÑAZU | ACEQUIA DEL DIABLO | Sauce | CALLE QUINTANA | ARROYO CIENEGAS | ARROYO TORRECITAS | MATRIZ SUR", redRiego$denominacion,ignore.case = TRUE), 1,
  # Arroyo Grande
  ifelse(grepl("ARROYO GRANDE | SALAS CAROCA | LA PIRCA | ARROYO LA RIOJA | ARROYO LA BARRANCA | ARROYO SILVA O MANANTIALES | La Quebrada | CENTRO PRINCIPAL O RIO VIEJO | DESAGÜES PREDIOS PARTICULARES DE TUNUYAN | ARROYO CLARO -", redRiego$denominacion,ignore.case = TRUE), 2,
  # Diq. Valle de Uco
  ifelse(grepl("CANAL UCO | CANAL DE UCO | MELOCOTON | RAMA QUIROGA | ELVIRA BUSTOS | MARGEN DERECHA | MATRIZ VALLE DE UCO | CONSULTA | CAPACHO | CANAL CAÐADA DE LAS ROSAS | DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS | PEÐALOZA | CANAL MANZANO | MANZANO | CANAL VISTA FLORES | CANAL RINCON | RINCÓN | ARROYO CLARO", redRiego$denominacion,ignore.case = TRUE), 3,
  # Yaucha - Aguanda
  ifelse(grepl("MATRIZ YAUCHA | MATRIZ AGUANDA | YAUCHA | AGUANDA | CANAL MATRIZ YAUCHA | ARROYO LA SALAMANCA", redRiego$denominacion,ignore.case = TRUE), 4,
  ifelse(grepl("RIO MENDOZA",redRiego$subdelegacion,ignore.case = TRUE),5,
  ifelse(grepl("RIO TUNUYAN INFERIOR",redRiego$subdelegacion,ignore.case = TRUE),6,
  ifelse(grepl("RIO MALARGUE",redRiego$subdelegacion,ignore.case = TRUE),7,
  ifelse(grepl("RIO DIAMANTE",redRiego$subdelegacion,ignore.case = TRUE),8,
  ifelse(grepl("RIO ATUEL",redRiego$subdelegacion,ignore.case = TRUE),9,
         999)))))))))

# controlando por los espacios
redRiego$subSist <- ifelse(
  # Las Tunas
  grepl("ANCHAYUYO|LAS TUNAS|GUALTALLARY|ARROYO VILLEGAS|CANAL ESQUINA|CANAL ANCON|ANCON|ANCÓN|ESQUINA|TUPUNGATO|RIO DE LA PAMPA|QUEBRADA DE GUEVARA|EL PERAL|HIJ. PALMA|PALMA|EL INGENIO|ARROYO ALTO VERDE|ARROYO GUIÐAZU|ARROYO GUIÑAZU|ACEQUIA DEL DIABLO|Sauce|CALLE QUINTANA|ARROYO CIENEGAS|ARROYO TORRECITAS|MATRIZ SUR", redRiego$denominacion,ignore.case = TRUE), 1,
  # Arroyo Grande
  ifelse(grepl("ARROYO GRANDE|SALAS CAROCA|LA PIRCA|ARROYO LA RIOJA|ARROYO LA BARRANCA|ARROYO SILVA O MANANTIALES|La Quebrada|CENTRO PRINCIPAL O RIO VIEJO|DESAGÜES PREDIOS PARTICULARES DE TUNUYAN|ARROYO CLARO -", redRiego$denominacion,ignore.case = TRUE), 2,
  # Diq. Valle de Uco
  ifelse(grepl("CANAL UCO|CANAL DE UCO|MELOCOTON|RAMA QUIROGA|ELVIRA BUSTOS|MARGEN DERECHA|MATRIZ VALLE DE UCO|CONSULTA|CAPACHO|CANAL CAÐADA DE LAS ROSAS|DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS|PEÐALOZA|CANAL MANZANO|MANZANO|CANAL VISTA FLORES|CANAL RINCON|RINCÓN|ARROYO CLARO", redRiego$denominacion,ignore.case = TRUE), 3,
  # Yaucha - Aguanda
  ifelse(grepl("MATRIZ YAUCHA|MATRIZ AGUANDA|YAUCHA|AGUANDA|CANAL MATRIZ YAUCHA|ARROYO LA SALAMANCA", redRiego$denominacion,ignore.case = TRUE), 4,
  ifelse(grepl("RIO MENDOZA",redRiego$subdelegacion,ignore.case = TRUE),5,
  ifelse(grepl("RIO TUNUYAN INFERIOR",redRiego$subdelegacion,ignore.case = TRUE),6,
  ifelse(grepl("RIO MALARGUE",redRiego$subdelegacion,ignore.case = TRUE),7,
  ifelse(grepl("RIO DIAMANTE",redRiego$subdelegacion,ignore.case = TRUE),8,
  ifelse(grepl("RIO ATUEL",redRiego$subdelegacion,ignore.case = TRUE),9,
          999)))))))))

redRiego$subSist <- factor(redRiego$subSist, levels = c(1:9,999),
  labels = c('Las Tunas','Arroyo Grande','Diq. Valle de Uco','Yaucha-Aguanda','Mendoza','T.Inferior','Malargue','Diamante','Atuel','otro'))

# Suma los metros por categoria y material de acuerdo al código de cauce. 3033 observations
redRiegoLong <- redRiego %>% #filter(long_m!=0) %>%  # filtra los valores mapeados con long_metros ==0
 group_by(CodigoCauce,id_cat,id_mat,subdel,subSist,codigo,denominacion) %>% summarise_at(c("long_m"), sum, na.rm = TRUE) #%>%
#write_csv(redRiegoLong, 'DgiData/redRiegoLong.csv', na = "NA", append = FALSE, quote_escape = "double")
glimpse(redRiegoLong)

# Tabla x categoría (2471 observaciones no negativas)
redRiegoCat <- spread(redRiegoLong, key= 'id_cat', long_m)
redRiegoCat <- redRiegoCat %>% 
  select(CodigoCauce,codigo,subdel,subSist,id_mat,Canal,Hijuela,`Puente Canal`,Rama,Sifon,`Linea Auxiliar`, Proyectado,everything()) %>%
  relocate(geometry, .after = last_col())

redRiegoCat %>% filter(subdel=='Rio Tunuyan Superior')

summary(redRiegoCat$id_mat)  
# Tabla x material
redRiegoMat <- spread(redRiegoLong, key= 'id_mat', long_m)                        
redRiegoMat <- redRiegoMat %>% 
  select(CodigoCauce,codigo,subdel,subSist,id_cat,Tierra,Hormigon,Entubado,PVC,Chapa,`Sin dato`,geometry) 

redMaterial <- redRiegoMat %>% group_by(CodigoCauce, subdel,subSist) %>%
                summarize_if(is.numeric, sum, na.rm=T)

redMaterial %>% filter(subdel=='Rio Tunuyan Superior')

redSub <- redRiegoMat %>% group_by(subdel) %>%
          summarize_if(is.numeric, sum, na.rm=T)

redsubSist <- redRiegoMat %>% filter(subdel=='Rio Tunuyan Superior') %>%
  group_by(subSist) %>%
  summarize_if(is.numeric, sum, na.rm=T)
redsubSist <- redsubSist %>% select(everything(),-codigo)

redMaterialSub <- as.data.frame(redRiegoMat) 
redMaterialSub <- redMaterialSub %>% select(subdel,Tierra,Hormigon,Entubado,PVC,Chapa,`Sin dato`) %>% 
  group_by(subdel) %>% summarize_if(is.numeric, sum, na.rm=T)
redMaterialSub <- redMaterialSub[,-1]
redMaterialSub <- cbind(`subdel`= c('Atuel','Diamante','Malargüe','Mendoza','Tun. Inferior','Tun.Superior'),
                  redMaterialSub)

rios    <- st_read("/Users/SebastianRiera/Downloads/cursos_de_agua/cursos_de_agua.shp")

## Make sure they have the same projection
redRiegoCat <- st_transform(redRiegoCat, crs = st_crs(cuenca))



# Data para tabla de resumen
{redtotaltip <- redMaterialSub %>% group_by(subdel,Tierra,Hormigon,Entubado,PVC,Chapa,`Sin dato`) %>%
  summarize_if(is.numeric, sum, na.rm=T)
redtotaltip <- redtotaltip[,-1]
  
totSubdel <-  rowSums(redtotaltip)
names(totSubdel) <- c('Atuel','Diamante','Malargüe','Mendoza','Tun. Inferior','Tun.Superior')
totTipo   <- colSums(redtotaltip)
redtotaltip <- cbind(redtotaltip, `Total`=totSubdel)
redtotaltip   <- rbind(redtotaltip, totTipo)

wrtMaterial   <- as.matrix(redtotaltip[7,] * 100 / rowSums(redtotaltip[7,], na.rm = T))  #label_percent(big.mark = ".", suffix = " %")(redtotaltip[7,] / rowSums(redtotaltip[7,], na.rm = T)) 
wrtSubdel     <- as.matrix(redtotaltip[,7] * 100 / colSums(redtotaltip[,7], na.rm = T) )
names(wrtMaterial) <- c('Tierra','Hormigon','Entubado','PVC','Chapa','Sin dato','Total')
redtotaltip   <- cbind(redtotaltip, `porcSub` = wrtSubdel)
redtotaltip   <- rbind(redtotaltip, `porcMat` = wrtMaterial)
redtotaltip[7,7] <- colSums(redtotaltip[,7], na.rm = T)
redtotaltip[8,7] <- rowSums(redtotaltip[8,], na.rm = T)
redtotaltip[7,8] <- colSums(redtotaltip[,8], na.rm = T)

# redtotaltip => numeric
# redtotaltipo => character

redtotaltipo <- cbind(`Subdelegacion`=c('Atuel','Diamante','Malargüe','Mendoza','Tun. Inferior','Tun.Superior','Total','% total'),redtotaltip)

kmconv <- function(x){
  as.numeric(round(x/1000,2))
}
separator <- function(x){
  format(as.numeric(x), big.mark = ".", decimal.mark = ",")
}

options(digits = 2, scipen=999)
redtotaltipo[ 1:7, 2:8 ] <- lapply(redtotaltipo[ 1:7, 2:8], kmconv)
redtotaltipo[ , 2:ncol(redtotaltipo)] <- lapply(redtotaltipo[ , 2:ncol(redtotaltipo)], separator)

# acá está el quilombo
redtotaltipo[8,] <- as.list(paste0(redtotaltipo[8, ],"%"))
redtotaltipo[,9] <- paste0(t(redtotaltipo[ , 9 ]),"%" )
redtotaltipo[8,9] <- '-'
redtotaltipo[8,1] <- '% total'
}

## @knitr redMaterialSub
options(digits = 1, scipen=999)
redtotaltipo %>% 
  kable("latex", caption = "\\label{redMaterial}Condiciones sistema de riego (km)", align = c("l", rep("r", 8)),
  row.names = F, booktabs = TRUE, col.names = c("Subdelegacion","Tierra",'Hormigon','Entubado','PVC','Chapa','Sin dato','Total','% total'),linesep = "") %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  #collapse_rows(columns = 7, latex_hline = "custom") %>%
  footnote( general = "Elab. propia en base a DGI (2018, 2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
 footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)

## @knitr redMaterialSubSistema
options(digits = 1, scipen=999)
redMaterialSubSistema <- as.data.frame(redsubSist) %>% select(everything(),-geometry)
redMaterialSubSistema %>%
  kable("latex", caption = "\\label{rmss}Condiciones sub-sistemas de riego (metros)", align = c("l", rep("r", 8)),
        row.names = F, booktabs = TRUE, col.names = c("Subsistema","Tierra",'Hormigon','Entubado','PVC','Chapa','Sin dato'),linesep = "") %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  #collapse_rows(columns = 7, latex_hline = "custom") %>%
  footnote( general = "Elab. propia en base a DGI (2018, 2020)", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)

## @knitr figPrelim

#ggplot() + geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o TunuyÃ¡n Superior",], alpha = 0.8, col = "gray10") + # fill = "black", 
 # geom_sf(data=rios, alpha=.3,fill = "#05E9FF",col = "white", lwd=0.7) +
#  geom_sf(data = redRiego[ redRiego$subdel=="Rio Tunuyan Superior",], aes(fill=id_mat, colour=id_mat)) + 
 # geom_sf(data = cuenca[ cuenca$CUENCA!="RÃ­o TunuyÃ¡n Superior",], alpha = 1, fill = "white",col='white') +
#  theme_bw() + theme(legend.position = c(.21,.2), axis.title = element_text(size = 8), legend.title = element_blank(), legend.text = element_text(size = 8)) +
 # labs( title = "Cuenca Tunuyán Superior", subtitle = "Material Sist. de Riego") +  xlim(-69.6,-68.82) + ylim(-34.3,-33.1)

redRiego %>% select(subdel,CodigoCauce, subSist, denominacion,id_mat) %>%
  filter(subdel=='Rio Tunuyan Superior' & subSist!='otro') %>%
  ggplot() + geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o TunuyÃ¡n Superior", ],fill = "white",col='white') +
  geom_sf(data = redRiego[ redRiego$subdel=="Rio Tunuyan Superior",], aes(fill=id_mat, colour=id_mat)) + 
  geom_sf(data=rios, fill = "#05E9FF",col = "blue", alpha=0.3) + #lwd=0.7,
  xlim(-69.6,-68.82) + ylim(-34.3,-33.1) +
  theme_bw() + theme(legend.position = 'bottom',legend.title = element_blank()) +
  geom_sf(data = cuenca[ cuenca$CUENCA!="RÃ­o TunuyÃ¡n Superior",],col='grey90') +
  guides(fill = guide_legend(ncol = 3))
ggsave('figure/tunsup.pdf', height = 5, width = 4, units = 'in')
    

# Sub-sistemas ----

## @knitr subSistemas
ptosMapa <- rbind(`Dique Valle de Uco`= c(-33.759477, -69.212469),
                  `Dique Las Tunas`= c(-33.375452, -69.373016))
colnames(ptosMapa) <- c('latitud', 'longitud')

## @knitr subsist_control

# Gráfico con los subsistemas
redRiego %>% select(subdel,CodigoCauce, subSist, denominacion,id_mat) %>%
  filter(subdel=='Rio Tunuyan Superior' & subSist!='otro') %>%
  ggplot() + geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o TunuyÃ¡n Superior", ],fill = "white",col='white') +
  geom_sf(aes(fill= subSist,colour=subSist)) + 
  geom_sf(data=rios, fill = "#05E9FF",col = "blue", alpha=0.3) + #lwd=0.7,
  xlim(-69.6,-68.82) + ylim(-34.3,-33.1) +
  theme_bw() + theme(legend.position = 'bottom',legend.title = element_blank()) +
  geom_sf(data = cuenca[ cuenca$CUENCA!="RÃ­o TunuyÃ¡n Superior",],col='grey90') +
  guides(fill = guide_legend(ncol = 2))
ggsave('figure/tsSubsist.pdf', height = 5, width = 4, units = 'in')

# polygon fill Currently we need to manually merge the two together
#datapoly <- merge(values, positions, by = c("id"))
#p <- ggplot(datapoly, aes(x = x, y = y)) +  geom_polygon(aes(fill = value, group = id))


# Gráfico control subsistemas
redRiego %>% select(subdel,CodigoCauce, subSist, denominacion,id_mat) %>%
  filter(subdel=='Rio Tunuyan Superior' & grepl('claro',denominacion,ignore.case = TRUE)) %>% # & subSist=='otro') %>%
  ggplot() + geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o TunuyÃ¡n Superior",], alpha = 0.8, fill = "black", col = "gray10") + 
  geom_sf(aes(fill= subSist,colour=subSist )) + theme(legend.position = 'bottom',legend.title = element_blank())
#  geom_sf(aes(fill= CodigoCauce,colour=CodigoCauce ))
#ggsave('figure/tsSubsist2bf.pdf', height = , width = 5, units = 'in')
#summarise(subSist)


# Arranging Data ----


## @knitr ahorroSist2

redCodigo <- redRiegoCat %>% #filter(subdel=='Rio Tunuyan Superior') %>%
  group_by(codigo,subdel,subSist,CodigoCauce) %>%
  summarize_if(is.numeric, sum, na.rm=T) #%>% arrange(CodigoCauce)

tsMaterial <- redRiegoMat %>% select(CodigoCauce,subdel,subSist,Tierra:`Sin dato`) %>%
  filter(subdel=='Rio Tunuyan Superior') %>%
  group_by(CodigoCauce,subSist) %>%
  summarize_if(is.numeric, sum, na.rm=T) %>% arrange(CodigoCauce)

redCodigo$CodigoCauce <- as.numeric(as.character(redCodigo$CodigoCauce))

arrange(OfertaSup,CodigoCauce)

# unir datos geoespaciales con estimaciones económicas
OfertaSup <- merge(x= OfertaSup, 
    y= redCodigo[redCodigo$subdel=='Rio Tunuyan Superior',c("CodigoCauce","subSist","Canal","Hijuela","Arroyo","Rama","Ramo","Rio")], 
    by = c('CodigoCauce'), all.x = TRUE)
OfertaSup <- merge(x= OfertaSup, 
    y= tsMaterial[,c("CodigoCauce","Tierra","Hormigon","Entubado","PVC","Chapa","Sin dato")], 
    by = c('CodigoCauce'), all.x = TRUE)

OfertaSup$subSist <- ifelse( (OfertaSup$CodigoCauce==9774 | OfertaSup$CodigoCauce==9705 | OfertaSup$CodigoCauce==9713), 
    OfertaSup$subSist==1, OfertaSup$subSist)

OfertaSup$subSist[c(30:31,36,41)] <- 1
OfertaSup$subSist <- factor(OfertaSup$subSist, levels = c(1:9,999),
    labels = c('Las Tunas','Arroyo Grande','Diq. Valle de Uco','Yaucha-Aguanda','Mendoza','T.Inferior','Malargue','Diamante','Atuel','otro'))

OfertaSup$totalMat  <- rowSums(OfertaSup[,24:29], na.rm = T)
OfertaSup$totalTipo <- rowSums(OfertaSup[,17:21], na.rm = T)
OfertaSup <- OfertaSup[,c(1:28,30:31)]
colnames(OfertaSup)[22] <- 'geometry'
arrange(OfertaSup,valuePerdBis)

OfertaSup %>% select(CodigoCauce,subSist,Obra) %>% filter(is.na(subSist))

ahorroSist <- OfertaSup %>% select(valuePerdBis,AAcum,Obra,subSist) %>%
  ggplot(.) + geom_step(aes(y= valuePerdBis, x=AAcum/1000000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/1000000, label = Obra, angle=0),
  vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T) + #,nudge_x = -10, nudge_y = -10) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum, digits = 0)) #+ xlim(0,10) + ylim(0,1) 

## @knitr ahorroSist1
OfertaSup %>% select(valuePerdBis,AAcum,Obra,subSist) %>%
  ggplot(.) + geom_step(aes(y= valuePerdBis, x=AAcum/1000000,colour=subSist)) + #color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/1000000, label = Obra, angle=0,colour=subSist),
    vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T) + #,nudge_x = -10, nudge_y = -10) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum/1000000, digits = 0)) + # xlim(0,10) + 
  ylim(0,1) +
  theme_bw() + theme(legend.position = c(0.25,0.75), legend.title = element_blank())

#+ facet_wrap(~subSist, nrow = 1)

## @knitr ahorroSist
ahorroSist + theme(axis.text.x = element_text(size = 8, angle=0, vjust = .4), 
  axis.text.y = element_text(size = 8),
  panel.background = element_rect(fill = "white"), 
  axis.title = element_text(size = 9)) + scale_x_continuous(breaks = c(seq(0,40,20))) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("metros cúbicos anuales") + ylab("Dólares por m3 anual ahorrado") + 
  facet_wrap(~subSist, nrow = 1)
ggsave('DgiData/Graphs/ofertaSist.png', height = 4, width = 12)




## @knitr GraphStuff
longCanalesTipo <- as.data.frame(redRiegoMat %>% select(CodigoCauce,subdel,id_cat,Tierra:`Sin dato`) %>%
              group_by(CodigoCauce,subdel) %>% summarise_if(is.numeric,sum,na.rm=T)) #filter(subdel=="Rio Tunuyan Superior") %>% 

longCanalesTipo <- longCanalesTipo %>% select(CodigoCauce:`Sin dato`)
write_csv(longCanalesTipo, 'DgiData/longCanalesTipo.csv', na = "NA", append = FALSE, quote_escape = "double")

OfertaSup %>% select(subSist,CodigoCauce,AAcum, valuePerdBis) %>% summarise(valuePerdBis,subSist)

# Red por denominación. (para ubicar zonas)
redRiego %>% filter(subdel=="Rio Tunuyan Superior") %>%
  ggplot() + geom_sf(alpha=0.8, aes(fill=denominaci, col=denominaci)) +  
  theme(legend.title = element_blank(),legend.position ='bottom')
ggsave('figure/tunsupDenom.pdf', height = 5, width = 4, units = 'in')

redMaterial %>%
  filter(subdel=="Rio Tunuyan Superior") %>%
  ggplot( col=CodigoCauce) + #geom_sf() + 
  geom_sf_label( aes( label= CodigoCauce), alpha=0.2) +
  geom_sf(data = rios, alpha=0.5,fill = "#05E9FF", col = "#05E9FF") + 
  xlim(-69.6,-68.82) + ylim(-34.3,-33.1)
# st_point_on_surface.sfc(sf::st_zm(x)) :

redRiegoLong %>%
  filter(subdel=="Rio Tunuyan Superior") %>%  ggplot( ) + 
  geom_sf(aes(fill=id_mat, colour=id_mat)) + theme_bw() +
  geom_sf(data = rios, alpha=0.5,fill = "#05E9FF", col = "#05E9FF") + 
  xlim(-69.15,-68.95) + ylim(-33.95,-33.82)
  

redRiegoLong %>% filter(subdel=="Rio Mendoza") %>%
  group_by(CodigoCauce) %>% ggplot( ) + 
  geom_sf(aes(fill=id_mat, colour= id_mat)) + #
  geom_sf(data = rios, alpha=0.5,fill = "#05E9FF", col = "#05E9FF") + 
  xlim( -68.994664,-68.719274) + ylim(-33.406994,-33.051441) + theme_bw() 

redRiego %>% select(subdel,CodigoCauce, subSist, denominacion,id_mat) %>%
  filter(subdel=='Rio Mendoza') %>%
  ggplot() + geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o Mendoza", ],fill = "white",col='white') +
  geom_sf(aes(fill= subSist,colour=subSist)) + 
  geom_sf(data=rios, fill = "#05E9FF",col = "blue", alpha=0.3) + #lwd=0.7,
  xlim( -68.994664,-68.719274) + ylim(-33.406994,-33.051441) +
  theme_bw() + theme(legend.position = 'bottom',legend.title = element_blank()) +
  geom_sf(data = cuenca[ cuenca$CUENCA!="RÃ­o Mendoza",],col='grey90') +
  guides(fill = guide_legend(ncol = 2))
ggsave('figure/mzSubsist.pdf', height = 5, width = 4, units = 'in')


redMaterial %>%
  filter(subdel=="Rio Mendoza") %>%
  ggplot( col=CodigoCauce) + #geom_sf() + 
  geom_sf_label( aes( label= CodigoCauce), alpha=0.2) +
  geom_sf(data = rios, alpha=0.5,fill = "#05E9FF", col = "#05E9FF") + 
  #xlim( -68.994664,-68.719274) + ylim(-33.406994,-33.051441) + 
  theme_bw() 
#+  geom_sf(redMaterial$subdel=="Rio Tunuyan Superior", aes(geometry=geometry)) 
 # geom_sf_text(redMaterial$subdel=="Rio Tunuyan Superior", aes(geometry=geometry, label= CodigoCauce),    alpha=0.3) 

geom_text(data = nariño_points, aes(x=X, y=Y,label = ID_2), size = 2) +
  scale_fill_distiller(name="ID_2", palette = "YlGn", breaks = pretty_breaks(n = 5))+

redRiegoLong %>% filter(subdel=="Rio Tunuyan Superior") %>%
  group_by(CodigoCauce) %>% ggplot( col=CodigoCauce) + geom_sf() + 
  geom_sf(data=rios, alpha=0.5,fill = "#05E9FF",col = "#05E9FF") + xlim(-69.6,-68.82) + ylim(-34.3,-33.1)

redRiegoLong %>% filter(subdel=="Rio Tunuyan Superior") %>%
  group_by(CodigoCauce) %>%
  ggplot() + geom_sf(aes(colour=CodigoCauce), col=CodigoCauce) #+  geom_sf_label( aes( label= CodigoCauce)) 



ggplot(data = redRiegoLong[ redRiegoLong$subdel=="Rio Tunuyan Superior",], ) +
  #geom_sf( aes( fill=id_mat, colour=id_mat),lwd=0.9) + 
  geom_sf_label( aes( label= CodigoCauce)) 


ggplot(data = redRiegoLong[ redRiegoLong$CodigoCauce == c(5700:5772),], ) + 
       geom_sf( aes( fill=CodigoCauce, colour=CodigoCauce)) #+ geom_sf_label( aes( label= CodigoCauce)) 
       
        

ggplot() + 
 # geom_sf(data = cuenca[ cuenca$CUENCA=="RÃ­o TunuyÃ¡n Superior",], alpha = 0.8, fill = "black", col = "gray10") + 
#  geom_sf(data= rios, alpha=.3,fill = "#05E9FF",col = "#05E9FF", lwd=0.7) +#05E9FF , lwd=1.5
  geom_sf(data = redRiegoLong[ redRiegoLong$subdel=="Rio Tunuyan Superior",], aes(fill=id_mat, colour=id_mat),lwd=0.9) + 
  geom_sf_label( aes( label= CodigoCauce))  #+
  #ggplot(mpg, aes(displ, hwy)) +   geom_text(aes(label = model), check_overlap = TRUE)
# geom_sf(data = cuenca[ cuenca$CUENCA!="RÃ­o TunuyÃ¡n Superior",], alpha = 1, fill = "white",col='white') +
  labs( title = "Cuenca Tunuyán Superior", subtitle = "Material Sist. de Riego") +
  xlim(-69.6,-68.82) + ylim(-34.3,-33.1)


layout(title = "Main Source for News", xaxis = xaxis, yaxis = yaxis, margin = margin,
       autosize = FALSE,
       showlegend = FALSE,
       annotations = television_1)
labs(title = "Mileage by engine displacement",
     subtitle = "Data from 1999 and 2008",
     caption = "Source: EPA (http://fueleconomy.gov)",
     x = "Hectómetros cúbicos anuales",
     y = "Dólares por Hm3 ahorrado", axis(OfertaMza$AhObraAnual)
) + 
  x <- seq(0.01, .99, length.out = 100)
df <- data.frame(
  x = rep(x, 2),
  y = c(qlogis(x), 2 * qlogis(x)),
  group = rep(c("a","b"),
              each = 100)
)
p <- ggplot(df, aes(x=x, y=y, group=group))
# These work
p + geom_line(linetype = 2)
p + geom_line(aes(colour = group), linetype = 2)
p + geom_line(aes(colour = x))
# But this doesn't
should_stop(p + geom_line(aes(colour = x), linetype=2))

ggplot(redRiegoLong)

plot(Mendoza$AhObraAnual,Mendoza$UsdHm3Obra)

#Mza17$AhorroUsd   <- Mza17[, AhorroM3Seg / InvUsd ]

supply <- Hmisc::bezier(y = Mendoza$UsdHm3Obra*10,
                        x = (Mendoza$AhObraAnual*1000)) %>%
  as_data_frame()

ggplot(supply, aes(x = x, y = y)) + 
  geom_path(color = "#0073D9", size = 1) + 
  theme_classic() + 
  coord_equal()

#install.packages("plotly")
library(plotly)

y <- Mendoza$UsdHm3Obra*10
x <- Mendoza$AhObraAnual*1000


p <- plot_ly(x = ~x) %>%
  add_lines(y = ~y, name = "linear", line = list(shape = "linear")) %>%
  add_lines(y = y + 5, name = "spline", line = list(shape = "spline")) %>%
  add_lines(y = y + 10, name = "vhv", line = list(shape = "vhv")) %>%
  add_lines(y = y + 15, name = "hvh", line = list(shape = "hvh")) %>%
  add_lines(y = y + 20, name = "vh", line = list(shape = "vh")) %>%
  add_lines(y = y + 25, name = "hv", line = list(shape = "hv"))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="line-interp")
chart_link


# Extract a column from as a data frame as a vector or scalar.
# E.g. starwars %>% filter(gender=="female") %>% pull(height)





# A Simple Theory of Regulatory Effort with Spillover (Static Version)
# https://www.rpubs.com/tanglePillowHolder/315910


# Cumulative innovation & dynamics
# http://economics.mit.edu/files/12082

# Cost relative to effort:
# C_i(e_i) = Beta_i (e_iˆ2/n + e_i)

# Benefits for improving quality
#  = log(1 + \frac{e_{i}}{s_{i}} + \gamma_{i} \times e_{j})
#           - marginal improvement when + spill-over effect

# Total benefit 

# throughout, we will have the endogeneous vector (Effort), e, and
# the exogenous parameters, th.

# e  is (1. e1, # effort level agent 1 used to calibrate MgCost=MgBenefit
#        2. e2)

# th is (1. a1,
#        2. a2,
#        3. b1,
#        4. b2,
#        5. g1, 
#        6. g2,
#        7. s1,
#        8. s2,
#        9. l)

#   ____________________________________________________________________________


# Sources ----

# Summary statistics 
## @knitr Sources
library(qwraps2)
options(qwraps2_markup = "markdown")
gapminder <- as.data.frame(gapminder)
summary_statistics <-
  list(
    "Life Expectancy" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(lifeExp, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(lifeExp, na_rm = TRUE),
        "min" = ~min(lifeExp, na.rm = TRUE),
        "max" = ~max(lifeExp, na.rm = TRUE),
        "Missing" = ~sum(is.na(lifeExp))
      ),
    "Population" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(pop, na_rm = TRUE),
        "median (Q1, Q3)" = ~qwraps2::median_iqr(pop, na_rm = TRUE),
        "min" = ~min(pop, na.rm = TRUE),
        "max" = ~max(pop, na.rm = TRUE),
        "Missing" = ~sum(is.na(pop))
      ),
    "GDP per Capita" =
      list(
        "High GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "high"),
        "Low GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "low"),
        "Missing" = ~sum(is.na(gdpPercap))
      )
  )

summary_table(gapminder, summary_statistics)
library(table1)
table1::label(gapminder$lifeExp) <- "Life Expectancy"
table1::label(gapminder$pop) <- "Population"
table1::label(gapminder$gdpPercap) <- "Gdp Per Capita"

table1::table1(~lifeExp + pop + gdpPercap | continent, data = gapminder)


# Regulatory Effort ###

## @knitr foc1   
foc1 <- function (e, th)
{
  
  # step 1: assign -e1 if boundary condition is met
  if(th[3] > (th[1] * (th[9] / (th[7] * (1 + th[5] * e[2])) + (1 - th[9]) / 2)))
    return(-e[1])
  else
  {
    # step 2: if boundary condition not met, set MC = MB
    
    ch1 <- th[1] * (th[9] / (th[7] * (1 + e[1] / th[7] + th[5] * e[2])))
    ch2 <- th[1] * (1 - th[9]) / 2
    ch3 <- th[3] * (1 + e[1])
    return(ch1 + ch2 - ch3)
    
  }
  
}


foc2 <- function (e, th)
{
  
  # step 1: assign -e2 if boundary condition is met
  if(th[4] > (th[2] * (th[9] / (th[8] * (1 + th[6] * e[1])) + (1 - th[9]) / 2)))
    return(-e[2])
  else
  {
    # step 2: if boundary condition not met, set MC = MB
    
    ch1 <- th[2] * (th[9] / (th[8] * (1 + e[2] / th[8] + th[6] * e[1])))
    ch2 <- th[2] * (1 - th[9]) / 2
    ch3 <- th[4] * (1 + e[2])
    return(ch1 + ch2 - ch3)
  }
  
}

# wrap it into a model
## @knitr Model
model <- function (e, th)
  c(F1 = foc1(e, th),
    f2 = foc2(e, th))




