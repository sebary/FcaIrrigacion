

## @knitr SetUp2
RegulatoryEffort <- c("foreach",#"rootSolve",
                      "ggthemes","kableExtra","qwraps2","tidyverse","data.table","tidyr",
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
indxx <- c("TierraQ0","TierraQ0rep","TierraQ0medio","TierraQl","TierraQlrep","TierraQlmedio","Distancia","KmInvert","EfcTierra","LongMedia","EfTierraLong","EfCanales","EfHijuelas","KmCanales")
EfMendoza[indxx] <- lapply(EfMendoza[indxx], function(x) as.numeric(as.character(x)))
EfMendoza$Q0   <- EfMendoza[,ifelse(!is.na("TierraQ0rep"),"TierraQ0medio",
                                ifelse(!is.na("TierraQ0rep") <= !is.na("TierraQ0medio"), "TierraQ0rep",
                                  ifelse(!is.na("TierraQ0medio") <= !is.na("TierraQ0"),"TierraQ0medio","TierraQ0rep")))]
EfMendoza$Q1   <- EfMendoza[, ifelse((!is.na("TierraQlmedio") >= !is.na("TierraQl") & !is.na("TierraQlmedio") >= !is.na("TierraQlrep")),"TierraQlmedio",
                                     ifelse((!is.na("TierraQl") >= !is.na("TierraQlmedio") & !is.na("TierraQl") >= !is.na("TierraQlrep")),"TierraQl",
                                           ifelse(!is.na("TierraQlrep"),"TierraQl","TierraQlrep")))]
glimpse(EfMendoza)
}


Caudal19 <- c(590,490,140,850,460,460)
names(Caudal19) <- c("Atuel","Diamante","Malargüe","Mendoza","Tun.Inferior","Tun.Superior")
  
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

CaudalWeb <-
  CaudalWeb %>%
  clean_names() %>%
  #as_tibble() %>%
  mutate(fecha_desde = ymd(fecha_desde)) %>%
  mutate(fecha_fin = ymd(fecha_fin))
colnames(CaudalWeb)[2] <- "CodigoCauce"
glimpse(CaudalWeb)


CaudalWebQ <- aggregate(CaudalWeb$valor_promedio_relevado/1000, by=list(CaudalWeb$CodigoCauce), FUN=sum, na.rm=T)
colnames(CaudalWebQ) <- c("CodigoCauce","Qweb") # m3/s
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
SummaryObras <-
  list(
    "Inversión (USD)" =
      list(
        "promedio (desv.std.)" = ~qwraps2::gmean(InvUsd, na_rm = TRUE),#"mediana (Q1, Q3)" = ~qwraps2::median_iqr(inversion, na_rm = TRUE),
        "min" = ~min(InvUsd, na.rm = TRUE),
        "max" = ~max(InvUsd, na.rm = TRUE) #, "sin datos" = ~sum(is.na(inversion))
      ),
   "Metros revestidos" =
      list(
        "promedio" = ~qwraps2::gmean(metros, na_rm = TRUE), #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(metros, na.rm = TRUE),
        "max" = ~max(metros, na.rm = TRUE) 
      ),
    "Inv.(USD)/mt" =
      list(
        "promedio" = ~qwraps2::gmean(InvMtUsd, na_rm = TRUE), #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(InvMtUsd, na.rm = TRUE),
        "max" = ~max(InvMtUsd, na.rm = TRUE) ) )

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

indxx <- c("Ano","Codigo","CodigoCauce","Metros","Inversion","hectareas","Padrones") # Convert many columns in numeric #https://stackoverflow.com/questions/27528907/how-to-convert-data-frame-column-from-factor-to-numeric
DgiFull[indxx] <- lapply(DgiFull[indxx], function(x) as.numeric(as.character(x)))

class(DgiFull$Inversion)
names(DgiFull)[9] <-"metros"
names(DgiFull)[10]<-"inversion"

DgiFull$InvMt    <- as.numeric(round(DgiFull$inversion/DgiFull$metros, digits = 0))
DgiFull$InvUsd   <- as.numeric(ifelse(DgiFull$Ano==2017,round(DgiFull$inversion/17, digits = 1),
                               ifelse(DgiFull$Ano==2018,round(DgiFull$inversion/27.425, digits = 1),
                               ifelse(DgiFull$Ano==2019,round(DgiFull$inversion/43.8, digits = 1),
                               ifelse(DgiFull$Ano==2020,round(DgiFull$inversion/70, digits = 1),"NA"))))) 
DgiFull$InvMtUsd <- as.numeric(round(DgiFull$InvUsd/DgiFull$metros, digits = 1))
options(qwraps2_markup = "latex")
# Cuadro summary
ObrasFull <- as.data.frame(DgiFull)

DgiFull %>%
  filter(metros!= "Global" | metros!="-" | metros!="a determinar" | metros!="NA" ) %>%
  select(CodigoCauce, Subdelegacion, hectareas, inversion, metros, InvMtUsd,Ano) %>%
  mutate(coment= paste0(InvMtUsd," USD/metro revestido")) %>% 
  arrange(Subdelegacion,InvMtUsd) 

## @knitr SumFull
print(qwraps2::summary_table(  dplyr::group_by(ObrasFull, Subdelegacion),
                               SummaryObras), # rtitle = "Resumen de obras 2017-2019", 
      align = c("l",rep("r",6)),
      cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior"),
      caption = "\\label{SumFull}Resumen de obras 2017-2020"
)


## @knitr SummaryBaseFull
# List of lists to replicate the analysis
ObrasFull %>%
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
               y= EfMendoza[ , c(2:4,6,13,15,17:21,24:29) ], 
               by= c("CodigoCauce"), all.x=TRUE)
Mendoza <- merge(x= Mendoza,
                 y= CaudalWebQ, 
                 by= c("CodigoCauce"), all.x=TRUE)

arrange(Mendoza,CodigoCauce)
#Mendoza <- Mendoza[-c(11,13),] # Canal Lunlunta duplicado

# Eficiencia post entubamiento ==1 & revestimiento 0.99
Mendoza$EfPost     <- Mendoza[, ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Mendoza$Obra), 1,
                                ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Mendoza$Obra), 0.99,
                                               0.98))]
Mendoza$EfAnte     <- Mendoza[, ifelse(!is.na(EfCanales) & grepl("C. | Canal | Canales | Can. | Canal. | Cl | canal", Mendoza$Obra), EfCanales,
                                ifelse(!is.na(EfHijuelas) & grepl("H. | Hijuela | Hij. | Hij | Hj | hijuela | HIJUELA | HIJ.", Mendoza$Obra), EfHijuelas,
                                       EfUnidadManejo))]


# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento

Mendoza$PerdidaxKm        <- round(Mendoza[, (Q0) * EfTierraLong/KmTierra ], digits = 3) # EfTierraLong/KmTierra es eficienci x kilómetro en la UM
Mendoza$DeltaPerdida      <- round(Mendoza[, (PerdidaxKm * EfPost * metros/1000 * 2073600)], digits=0) # segundos al ano (4 turnos mensuales x 8 meses) / conversión a '000)]


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
Mendoza$ACaudalUm       <- round(ifelse( Mendoza$inversion == 845000,       
                                (Mendoza$Q0) * (Mendoza$EfPost - Mendoza$EfAnte) / (Mendoza$KmCanales), Mendoza$ACaudalUm), digits = 3) # KmTierra # total inspección 


Mendoza$ACaudalAnualUm  <- round(Mendoza[, ACaudalUm * metros/1000 * 2764800], digits=0) 

Mendoza$valuePerd  <- round(Mendoza[, InvUsd / DeltaPerdida], digits = 3)
Mendoza$valueEf    <- round(Mendoza[, InvUsd / ACaudalAnualUm], digits = 3)

# adicional plus obras x administración
Mendoza$valuePerdBis <- round(Mendoza[, ifelse( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA",
                                                  valuePerd, valuePerd * 1.32)], digits = 2)

Mendoza$valueEfBis <- round(Mendoza[, ifelse(Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", #grepl("Licitacion | Licitación | LICITADA", Mendoza$Modalidad),
                                             valueEf, valueEf * 1.32)], digits = 2)

Mendoza <- Mendoza[order(Ano,valuePerdBis)]

write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")


MzaTableComparacion <- as.data.frame(Mendoza[ c(1:7,9:27), c(9,12,8,4:5,29,28,31,33:37)]) %>% #   Mendoza[c(1:18), c(2,12,4,6,16,28,25,29,31,30,32:34)]) %>% # 
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTableComparacion}Río Mendoza - Comparación metodologías", align = c("l", "c",rep("r", 12)),
        row.names = FALSE, booktabs = TRUE,  
        col.names = c("Obra","Zona","Modalidad","Metros","USD/mt","ex-ante","ex-post","Delta pérdida","Delta EfC","Pérdida","EfC","Pérdida","EfC")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  add_header_above(c(" "=5,"Ef.Conducción" =2, "Ahorro m3/km" = 2, "USD/m3" = 2, "USD/m3 (ajuste)" = 2)) %>%
  pack_rows("2017", 1,7) %>%
  pack_rows("2018", 8,10) %>%
  pack_rows("2019", 11,16) %>%
  pack_rows("2020", 17,26) %>%
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
  kable(format = "latex",caption = "\\label{tab:MzaTable}Río Mendoza - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 8)),
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
Mendoza %>% select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  #select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  mutate(GananciaEfC= EfPost-EfAnte) %>%
  arrange(Ano) %>%
  select(CodigoCauce, Obra,Q0,CaudalDiseno, Qweb,GananciaEfC) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupCaudales}Mendoza - Caudales disponibles por obra", align = c("c", "l",rep("r", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Código Cauce","Obra", "C.Utilizado","C. diseño","C.Web","Ganancia EfC")) %>% 
  landscape()


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
OfertaMza1$AAcum   <- cumsum((OfertaMza$DeltaPerdida)) 

CProm <- round(mean(OfertaMza1$valuePerdBis, na.rm=T), digits = 1)
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
AhorroMza1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10),
                   panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  scale_y_continuous(breaks = c(seq(0,12,3),20,30,40,50,60)) + 
  theme(axis.line = element_line(colour = "grey50")) + 
#  geom_path(data = demand, color = "#FF4036", size = 1,aes(x, y)) + #
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}

#  caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years", 40), collapse = "\n")#  geom_text(aes(label = "Costo promedio USD 14.1/m3"), vjust = "inward", hjust = "inward")
#  ggplot(economics, aes(date, unemploy)) +   geom_line() +
#    geom_text( aes(x, y, label = caption), data = data.frame(x = xrng[1], y = yrng[2], caption = caption),hjust = 0, vjust = 1, size = 4)
    
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

EfSuperior <- as.data.frame(read.csv("DgiData/EfConduccion/TunuyanSuperior/EfCondTunSup.csv", sep = ",")) #, header = TRUE, sep=",")
indxx <- c("CodigoCauce","Superficie","LongTotal","LongRevest","EfRevest","LongHijuelas","PorcRevest","EfTierra",
           "EfGlobal","Caudal","CoefMoritz","PerdidaPorcentaje","TiempoMojado","PerdidaTiempo","EfUM")
EfSuperior[indxx] <- lapply(EfSuperior[indxx], function(x) as.numeric(as.character(x)))

## @knitr SuperiorTable

Superior <- rbind(tSup17,tSup18[c(1:9,11),],tSup19[c(1:9,11),],tSup20, fill=TRUE)
Superior <- Superior[ !is.na(CodigoCauce),]
Superior <- merge(x= Superior,
                 y= EfSuperior[ , c(1:8,10:14,17:19) ], 
                 by= c("CodigoCauce"), all.x=TRUE)
arrange(Superior,CodigoCauce)
Superior <- Superior %>% select(everything()) %>% distinct(inversion, Obra, .keep_all = TRUE)
#Superior %>% filter(is.na(EfRevest))

# Eficiencia post entubamiento ==1 & revestimiento 0.99
Superior$EfPost     <- Superior[, ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Superior$Obra), 1,
                        ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Superior$Obra), 0.99,
                        ifelse(grepl("Mej. | Mejora", Superior$Obra), 0.98,  0.976)))]
Superior$EfAnte     <- ifelse(!is.na(Superior$EfTierra),Superior$EfTierra,Superior$EfGlobal)
Superior$EfAnte     <- round(ifelse(is.na(Superior$EfAnte),mean(Superior$EfAnte,na.rm = T),Superior$EfAnte), digits = 2)

#[, ifelse(!is.na(EfCanales) & grepl("C. | Canal | Canales | Can. | Canal. | Cl | canal", Superior$Obra), EfCanales,ifelse(!is.na(EfHijuelas) & grepl("H. | Hijuela | Hij. | Hij | Hj | hijuela | HIJUELA | HIJ.", Superior$Obra), EfHijuelas, EfUM))]


# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento
Superior <- merge(x= Superior,                   y= CaudalWebQ,                   by= c("CodigoCauce"), all.x=TRUE)
#glimpse(Superior)


Superior$Q0               <- Superior$Caudal
Superior$Q0               <- ifelse(is.na(Superior$Caudal) & !is.na(Superior$CaudalDiseno),
                                    Superior$CaudalDiseno/1000,Superior$Q0)
# reemplazamos el caudal x la media!
Superior$Q0               <- ifelse(is.na(Superior$Caudal),
                                    mean(Superior$Q0,na.rm = T),Superior$Caudal) 
summary(Superior$Q0)

Superior$PerdidaxKm        <- round(Superior[, (Q0) * EfAnte/LongTotal ], digits = 3) # EfTierraLong/KmTierra es eficienci x kilómetro en la UM
Superior$DeltaPerdida      <- round(Superior[, (PerdidaxKm * EfPost * metros/1000 * 2073600)], digits=0) # segundos al ano (4 turnos mensuales x 8 meses) / conversión a '000)]

# Cálculo en base a EfC
# 1ro: Caudal de entrada x (ganancia de eficiencia) / distancia del aforo en km
# 2do: kilómetros revestidos (metros/1000)
# 3ro: x segundos anuales de riego (1 turno semanal durante 8 meses)
#Superior$ACaudal       <- round(Superior[, (Q0) * (EfPost - EfAnte) / (Distancia)], digits = 2) # KmTierra # total inspección 
#Superior$ACaudalAnual  <- round(Superior[, (Q0) * (EfPost - EfAnte) / (Distancia)  *  metros/1000 * 2764800], digits=1) 

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo
Superior$ACaudalUm       <- round(Superior[, Q0 * ((EfRevest * LongRevest) + (EfTierra * LongHijuelas))/ (LongRevest + LongHijuelas) ], digits = 3)
summary(Superior$ACaudalUm)

#ifelse(!is.na(EfCanales) & !is.na(KmCanales) & grepl("Canal | Canales | Can. | Canal. | Cl | canal | C.", Superior$Obra),
 #(Q0) * (EfPost - EfAnte) / (KmCanales),ifelse(!is.na(KmHijuela) & grepl("Hijuela | Hij. | Hij | Hj | hijuela | H.", Superior$Obra),
#(Q0) * (EfPost - EfAnte) / (KmHijuela),(Q0) * EfTierraLong / KmTierra ))], digits = 3) # KmTierra # total inspección 

Superior$ACaudalAnualUm  <- round(Superior[, ACaudalUm * metros/1000 * 1036800], digits=0) 

Superior$valuePerd  <- round(Superior[, InvUsd / DeltaPerdida], digits = 2)
Superior$valueEf    <- round(Superior[, InvUsd / ACaudalAnualUm], digits = 2)

Superior$valuePerdBis <- round(Superior[, ifelse(Modalidad=="Lic." | Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA",
                                                valuePerd, valuePerd * 1.32)], digits = 2)

Superior$valueEfBis <- round(Superior[, ifelse(Modalidad=="Lic." | Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA", #grepl("Licitacion | Licitación | LICITADA", Superior$Modalidad),
                                             valueEf, valueEf * 1.32)], digits = 2)

#Superior %>%  select(Obra,Cauce, PerdidaxKm, ACaudalUm, DeltaPerdida,valuePerd,valueEf,Modalidad,Ano,valuePerdBis,valueEfBis) %>% arrange(Modalidad, valuePerdBis,valuePerd)
# Q0, EfcTierra, ,ACaudalAnualUm,


Superior <- Superior[order(Ano,valueEfBis)]

write_csv(Superior, 'DgiData/Estimaciones/SupAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")


SupTableComparacion <- as.data.frame(Superior[ ,c(9,8,4,6,26,25,29,31:35) ]) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTableComparacion}Tunuyán Superior - Comparación metodologías", align = c("l", "c",rep("r", 12)),
        row.names = FALSE, booktabs = TRUE,  
        col.names = c("Obra","Modalidad","Metros","USD/mt","ex-ante","ex-post","Delta pérdida","Delta EfC","Pérdida","EfC","Pérdida","EfC")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  add_header_above(c(" "=4,"Ef.Conducción" =2, "Ahorro m3/km" = 2, "USD/m3" = 2, "USD/m3 (ajuste)" = 2), align = "c") %>%
  pack_rows("2017", 1,7) %>%
  pack_rows("2018", 8,11) %>%
  pack_rows("2019", 12,14) %>%
  pack_rows("2020", 15,17) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

SupTable <- Superior[ , c(9,4,5,26,25,30,31,6,35) ] %>% 
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTable}Tunuyán Superior - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Obra","Metros","Inv.(USD)","EfC(0)","EfC(1)","A(m3/seg)", "A(m3/Obra)","Inv.(USD/mt)","USD/m3")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=10) %>% # latex_options = c("striped", "scale_down")
  pack_rows("2017", 1,7) %>%
  pack_rows("2018", 8,11) %>%
  pack_rows("2019", 12,14) %>%
  pack_rows("2020", 15,17) %>%
  footnote( general = "Elab. propia en base DGI (2020).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Superior extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


## @knitr SupCaudales
  Superior %>% select(Ano,CodigoCauce, Obra,CaudalDiseno, Caudal,Qweb,CaudalBalance,CaudalObrador,Q0,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  #select(Ano,CodigoCauce, Cauce, Obra,Q0,CaudalDiseno, Qweb,KmTierra,metros,PerdidaxKm, ACaudalUm,EfPost,EfAnte) %>%
  mutate(GananciaEfC= EfPost-EfAnte) %>%
  arrange(Ano) %>%
  select(CodigoCauce, Obra,CaudalDiseno, Caudal,Qweb,CaudalBalance,CaudalObrador,Q0,GananciaEfC) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupCaudales}Tunuyán Superior - Caudales disponibles por obra", align = c("l", "c",rep("r", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Código Cauce","Obra","C. diseño","C.Informe","C.Web","C.Balance 2015","C.Obrador", "C.Utilizado","Ganancia EfC")) %>%
    landscape()

## @knitr SupTableComparacion
SupTableComparacion %>% landscape()

## @knitr SupTable
SupTable

## @knitr SupTableLandscape
SupTable %>% 
  landscape()

## @knitr SuperiorPlots
OfertaSup <- Superior[order(valueEfBis)]
OfertaSup$AAcum   <- cumsum((OfertaSup$ACaudalAnual)) 
OfertaSup1 <- Superior[order(valuePerdBis)]
OfertaSup1$AAcum   <- cumsum((OfertaSup$DeltaPerdida)) 
#require(greppel)
AhorroSup <- ggplot(OfertaSup) + # , color=Zona
  geom_step(aes(y= valueEfBis, x=AAcum/10000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueEfBis, x=AAcum/10000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum/10000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaSup$AhAcum

AhorroSup1 <- ggplot(OfertaSup1) + # , color=Zona
  geom_step(aes(y= valuePerdBis, x=AAcum/10000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum/10000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=-.2, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaSup1$AAcum/10000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaSup$AhAcum

## @knitr AhorroSup
AhorroSup + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                  axis.text.y = element_text(size = 10),
                  panel.background = element_rect(fill = "white"), 
                  axis.title = element_text(size = 9)) + 
#  scale_y_continuous(breaks = c(seq(0,80,5))) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaSup.png', height = 4, width = 12)

## @knitr AhorroSupPerd
AhorroSup1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10),
                   panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  scale_y_continuous(breaks = c(-5,seq(0,12,3),20,30,40,50,60)) + 
  theme(axis.line = element_line(colour = "grey50")) +
  #geom_text(nudge_x = -.1, nudge_y = 0.2) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaSupPerd.png', height = 4, width = 12)


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
        row.names = TRUE, booktabs = TRUE, col.names = c("2017","2018","2019","2020","Total") #,"Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
        ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elab. propia en base a DGI (2020)", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)


## @knitr GraphStuff
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
## @knitr MendozaPlot


# Arranging Data ----

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




