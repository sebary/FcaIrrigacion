####
# Proyecto FCA - IRRIGACIÓN
# códigos para análisis económico
# 
####

## @knitr SetUp2
RegulatoryEffort <- c("foreach","ggthemes","kableExtra","qwraps2","tidyverse","data.table","tidyr",
                      "utils","scales","matrixStats","readr","directlabels","dplyr","janitor","lubridate")  # included in tidyverse: "dplyr","tidyr","ggplot2","pandoc","table1", "summarytools"
library(rmsfuns)
load_pkg(RegulatoryEffort)

# Caudales

CaudalWeb <- read.csv("/Users/SebastianRiera/Google Drive/Biblio/Base de datos/DGI/CaudalesWeb.csv", sep = ",")

CaudalWeb <-  CaudalWeb %>%
  mutate(fecha_desde = ymd(fecha_desde), fecha_fin = ymd(fecha_fin))
CaudalWeb <-  CaudalWeb %>%
  mutate(fecha_desde = ymd(fecha_desde), fecha_fin = ymd(fecha_fin))
colnames(CaudalWeb)[2] <- "CodigoCauce"
glimpse(CaudalWeb)


CaudalWebQ <- do.call(data.frame, round(aggregate(CaudalWeb$valor_promedio_relevado/1000, 
  by=list(CaudalWeb$CodigoCauce), 
  FUN = function(x) c(Qwebav = mean(x), Qwebyear = sum(x) ))
  ,3)) 
colnames(CaudalWebQ) <- c("CodigoCauce","Qwebav","Qwebyear") # m3/s
#write_csv(CaudalWebQ, 'DgiData/CaudalWebQ.csv', na = "NA", append = FALSE, quote_escape = "double")



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
    ifelse(grepl("Ramo ", DgiFull$Obra, ignore.case = TRUE),4,5))))
DgiFull$clasif <- ifelse(grepl("Hiejuelas|Hijuela|Hij.|H.", DgiFull$Obra, ignore.case = TRUE),3,
   ifelse(grepl("Canal|Can.|C.|Cnl", DgiFull$Obra, ignore.case = TRUE),1,
    ifelse(grepl("Rama|Ram|R.", DgiFull$Obra, ignore.case = TRUE),2,
    ifelse(grepl("Ramo", DgiFull$Obra, ignore.case = TRUE),4,5))))
DgiFull$clasif <- factor(DgiFull$clasif, levels = c(1:5),
       labels = c('Canal','Rama','Hijuela','Ramo','otro'))

#DgiFull %>% select(CodigoCauce,clasif,Obra,Subdelegacion) %>% filter(Subdelegacion=='Tun. Superior') %>% arrange(CodigoCauce,clasif)

DgiFull$InvMt    <- as.numeric(round(DgiFull$inversion/DgiFull$metros, digits = 0))
DgiFull$InvUsd   <- as.numeric(DgiFull$inversion/DgiFull$TdC)
DgiFull$InvMtUsd <- as.numeric(round(DgiFull$InvUsd/DgiFull$metros, digits = 1))

options(qwraps2_markup = "latex")

# Cuadro summary
#ObrasFull <- DgiFull %>% select(Ano,CodigoCauce,Subdelegacion,inversion,InvUsd,InvMtUsd,metros,Padrones,hectareas) %>%
 #            filter(InvUsd>0 & metros>0 & InvMtUsd>0)

## @knitr SumFull
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

options(digits = 1)
print(qwraps2::summary_table(dplyr::group_by(DgiFull, Subdelegacion),
                               SummaryObras), 
  align = c("l",rep("r",6)),
  cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior"),
  caption = "\\label{SumFull}Resumen de obras 2017-2021 (USD)", #footnote("T. Superior e Inferior obras 1999-2020")
)


## @knitr SummaryBaseFull
# List of lists to replicate the analysis
DgiFull %>%
  dplyr::select(.data$inversion, .data$Padrones, .data$hectareas, .data$InvMt) %>%
  qsummary(.)

#ejFull  <- summary_table(dplyr::group_by(DgiFull[DgiFull$Ano==2018,], Subdelegacion), SummaryObras)


# Mendoza ----

## @knitr MendozaTable
options(digits = 2)
EfMendoza <- as.data.frame(read.csv("DgiData/EfCond.csv", sep = ",")) #, header = TRUE, sep=",")
indxx <- c("TierraQ0","TierraQ0rep","TierraQ0medio","TierraQl","TierraQlrep","TierraQlmedio",
           "Distancia","KmInvert","EfcTierra","LongMedia","EfTierraLong","EfCanales","EfHijuelas","KmCanales",
           "EfUnidadManejo","ha","litroSeg","horas","Q1_","caudalAnual","VolHa","VolHa20")
EfMendoza[indxx] <- lapply(EfMendoza[indxx], function(x) as.numeric(as.character(x)))
EfMendoza$Q0   <- EfMendoza[,ifelse(!is.na("TierraQ0medio"),"TierraQ0medio",
      ifelse(!is.na("TierraQ0rep") <= !is.na("TierraQ0medio"), "TierraQ0rep",
      ifelse(!is.na("TierraQ0medio") <= !is.na("TierraQ0"),"TierraQ0","TierraQ0medio")))]

Mendoza <- DgiFull %>% filter(Subdelegacion=='Mendoza' & metros>0) %>%
  select(CodigoCauce,metros,InvUsd,InvMtUsd,Ano,Modalidad,Obra) %>%
  arrange(Ano,CodigoCauce)

Mendoza <- merge(x= Mendoza,
    y= EfMendoza[ , c(2:4,6,13,15,17:21,24:28,33:35) ],
    by= c("CodigoCauce"), all.x=TRUE)
Mendoza %>% select(CodigoCauce,Inspeccion,VolHa,Q0) %>% arrange(Q0)
Mendoza <- merge(x= Mendoza,y= CaudalWebQ, 
    by= c("CodigoCauce"), all.x=TRUE)
# Info de volúmenes alternativo. adaptamos la info de plan de inversiones y algunos supuestos para obras 2021
umendoza <- read.csv('DgiData/SanchezSuperficiesAlternativo.csv', sep = ",")
# Original: read.csv('DgiData/SanchezSuperficies.csv', sep = ",")
glimpse(umendoza)

Mendoza <- merge(x= Mendoza,
    y= umendoza, 
    by= c("CodigoCauce"), all.x=TRUE)
Mendoza <- Mendoza %>% distinct(Obra, .keep_all = TRUE) %>%  select(everything())

library(sf)
cuenca  <- st_read("/Users/SebastianRiera/Downloads/cuencas_unidades_de_manejo/cuencas_unidades_de_manejo.shp")
#ctunsup <- cuenca %>% select(CUENCA, TIPO,NOMBRE,SUP_HA,ALT_MEDIA,PP_MED_A,P___MEDIA) %>% filter(CUENCA=="RÃ­o TunuyÃ¡n Superior")# & TIPO=='Cuenca')
{umendoza <- cuenca %>% filter(CUENCA=="RÃ­o Mendoza" & TIPO=='UM') %>% 
  select(NOMBRE,SUP_HA) %>% arrange(NOMBRE)

umendoza$Inspeccion <- as.numeric(as.character(NA))

umendoza$Inspeccion[1] <- 7 # 1ro Vistalba => Compuertas
umendoza$Inspeccion[2] <- 11 # Lujan oeste
umendoza$Inspeccion[30] <- 26  # Luján Sur unif
umendoza$Inspeccion[6] <- 3 # Bajada de Araujo
umendoza$Inspeccion[10] <- 5 # "Céspedes"
umendoza$Inspeccion[13] <- 6 # chacras de coria 
umendoza$Inspeccion[15] <- 25 # COLONIA
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
umendoza$Inspeccion[46] <- 28 # Progreso
umendoza$Inspeccion[47] <- 27 # Reina
umendoza$Inspeccion[51] <- 21 # san pedro y sp 
umendoza$Inspeccion[53] <- 29  # Santa Rita alternativa: Jocolí
umendoza$Inspeccion[55] <- 23  # Sobremonte
umendoza$Inspeccion[57] <- 24 # "Tulumaya"
umendoza$Inspeccion[58] <- 24 # "Tulumaya"
umendoza$Inspeccion <- ifelse(is.na(umendoza$Inspeccion),1,umendoza$Inspeccion)

umendoza$Inspeccion <- factor(umendoza$Inspeccion, 
  levels = c(1,2,3,5,6,7,9,11,13,14,15,17,18,20,21,23,24,25,26,27,28,29),
  labels = c('otro','Algarrobal',"Bajada de Araujo",'Céspedes','Compuertas','Chacras de Coria','Jocolí','Luján Oeste',
             'Lunlunta','Marienhoff','Mathus Hoyos','Morales Villanueva','Naciente Chachingo','Ortega','San Pedro y San Pablo','Sobremonte','Tulumaya',
             'Colonia','Luján Sur Unificada','Reina','Progreso','Santa Rita'))
}

Mendoza <- merge(x= Mendoza, y= umendoza[,c(2,4)], 
          by= c("Inspeccion"), all.x=TRUE)
Mendoza <- Mendoza %>% distinct(Obra, .keep_all = TRUE) %>%  select(everything(),-geometry)
arrange(Mendoza,CodigoCauce)


# Eficiencia post entubamiento ==1 & revestimiento 0.99
Mendoza$EfPost     <- ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Mendoza$Obra), 1,
  ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Mendoza$Obra), 0.995,
  0.99))
Mendoza$EfAnte     <-  ifelse(!is.na(Mendoza$EfCanales) & grepl("C.|Canal|Canales | Can. | Canal. | Cl | canal", Mendoza$Obra), Mendoza$EfCanales,
  ifelse(!is.na(Mendoza$EfHijuelas) & grepl("H.| Hijuela | Hij. | Hij | Hj | hijuela | HIJUELA | HIJ.", Mendoza$Obra), Mendoza$EfHijuelas,
  Mendoza$EfUnidadManejo))


Mendoza %>% select(CodigoCauce,Inspeccion,metros,VolHa,Q0)
# Cálculo en base a "Pérdida x km": 
# Diferencia de caudales en la distancia medida ponderada x la eficiencia de la UM en cauces revestidos
# 1ro: Ganancia de caudal en la distancia relevada con la EfC de revestimiento

# EfTierraLong/KmTierra es eficiencia x kilómetro en la UM
Mendoza$PerdidaxKm        <- round( (Mendoza$Q0) * Mendoza$EfTierraLong/Mendoza$KmTierra , digits = 3) 
Mendoza$DeltaPerdida      <- round( (Mendoza$PerdidaxKm * Mendoza$EfPost * Mendoza$metros/1000 * 4147200), digits=0) 

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200

# Volumen anual de agua que pasa por los canales en Hm3 (HECTÓMETROS CÚBICOS)
# enfoque de distancia media
options(digits = 4)
Mendoza <- Mendoza %>% 
  mutate(ahorroAgua = (VolHa) * 1.65 * SUP_HA * EfTierraLong/KmTierra * EfPost * metros/1000,
         ahorroAgua1 = round( (VolHa) * 1.65 * SUP_HA * (EfPost - EfTierraLong)/KmTierra * metros/1000, digits = 4),
         ahorroAgua2 = round( (VolHa) * 1.65 * Hectareas * (EfTierraLong)/KmTierra * EfPost * metros/1000, digits = 4))

# ahorroAgua: metodología normal con valores de superficie según mapas
# ahorroAgua1: metodología diferencia en EfC con valores de superficie según mapas
# ahorroAgua2: metodología normal con valores de superficie según datos de Carlos Sanchez

# Cálculo en base a EfC
# 1ro: Caudal de entrada x (ganancia de eficiencia) / distancia del aforo en km
# 2do: kilómetros revestidos (metros/1000)
# 3ro: x segundos anuales de riego (1 turno semanal durante 8 meses)
#Mendoza$ACaudal       <- round(Mendoza[, (Q0) * (EfPost - EfAnte) / (Distancia)], digits = 2) # KmTierra # total inspección 
#Mendoza$ACaudalAnual  <- round(Mendoza[, (Q0) * (EfPost - EfAnte) / (Distancia)  *  metros/1000 * 2764800], digits=1) 

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo
Mendoza$ACaudalUm       <- round( ifelse(!is.na(Mendoza$EfCanales) & !is.na(Mendoza$KmCanales) & grepl("Canal | Canales | Can. | Canal. | Cl | canal | C.", Mendoza$Obra),
    (Mendoza$Q0) * (Mendoza$EfPost - Mendoza$EfAnte) / (Mendoza$KmCanales),
    ifelse(!is.na(Mendoza$KmHijuela) & grepl("Hijuela | Hij. | Hij | Hj | hijuela | H.", Mendoza$Obra),
    (Mendoza$Q0) * (Mendoza$EfPost - Mendoza$EfAnte) / (Mendoza$KmHijuela),
    (Mendoza$Q0) * Mendoza$EfTierraLong / Mendoza$KmTierra )), digits = 3) # KmTierra # total inspección 
# Obra Compuertas
Mendoza$ACaudalUm <- round(ifelse( Mendoza$Ano == 2001 & Mendoza$metros==385,       
    (Mendoza$Q0) * (Mendoza$EfPost - Mendoza$EfAnte) / (Mendoza$KmCanales), Mendoza$ACaudalUm), digits = 3) # KmTierra # total inspección 

Mendoza$ACaudalAnualUm  <- round(Mendoza$ACaudalUm * Mendoza$metros/1000 * 4147200, digits=0) 

# 48 turnos anuales. un turno cada 5 días de 24 horas cada turno
# segundos x minutos x horas x turnos
#  60" x 60' x 24hs x 80 = 6912000
#  60" x 60' x 24hs x 48 = 4147200

# Valorización en dólares
Mendoza <- Mendoza %>% mutate(valuePerd=round(InvUsd/DeltaPerdida,3),
                   valueEf = round(InvUsd/ACaudalAnualUm,3),
                   valueAho = round(InvUsd/ahorroAgua,3),
                   valueAho1 = round(InvUsd/ahorroAgua1,3),
                   valueAho2 = round(InvUsd/ahorroAgua2,3))
# adicional plus obras x administración
summary(Mendoza$valuePerd)

Mendoza <- Mendoza %>% 
  mutate(valuePerdBis = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"),valuePerd,valuePerd*1.32),
        valueEfBis = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"),valueEf,valueEf*1.32),
       valueAhorro = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"),valueAho,valueAho*1.32),
      valueAhorro1 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"),valueAho1,valueAho1*1.32),
      valueAhorro2 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"),valueAho2,valueAho2*1.32))

Mendoza <- Mendoza %>% arrange(Ano,valueAhorro2)

write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")
#write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorroNov.csv', na = "NA", append = FALSE, quote_escape = "double")


MzaTableComparacion <- as.data.frame(Mendoza %>% arrange(Ano) %>% 
  select(Obra,UM,Modalidad,metros,InvMtUsd,EfAnte,EfPost,DeltaPerdida,ACaudalAnualUm,ahorroAgua2,valuePerdBis,valueEfBis,valueAhorro2)) %>%
  filter(!is.na(valueAhorro2)) %>% mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTableComparacion}Río Mendoza - Comparación metodologías", align = c("l", "c",rep("r", 12)),
    row.names = FALSE, booktabs = TRUE,  
    col.names = c("Obra","Zona","Modalidad","Metros","USD/mt","ex-ante","ex-post","pérdida","EfC",'nuevo',"perd","efc","nuevo")) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>% # latex_options = c("striped", "scale_down")
  add_header_above(c(" "=5,"Ef.Conducción" =2, "Ahorro m3/km" = 3,  "USD/m3 (ajuste)" = 3)) %>%
  pack_rows("2017", 1,7) %>% pack_rows("2018", 9,10) %>% pack_rows("2019", 11,16) %>% #pack_rows("2020", 17,17) %>% # Arroyo Morteritos es Alta montaña y no hay datos
  pack_rows("2021", 18,30) %>%
  footnote( general = "Elab. propia en base DGI (2021).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

MzaTablePpt <- as.data.frame(Mendoza %>% arrange(Ano) %>% filter(!is.na(ahorroAgua2)) %>%
  select(Obra,Zona,Modalidad,Q0,metros,InvMtUsd,ahorroAgua2,valueAhorro2)) %>% 
  mutate_all(linebreak) %>% #caption = "\\label{tab:MzaTablePpt}Río Mendoza - Estimaciones preliminares", align = c("l", "c","c",rep("r", 4)),
  kable(format = "latex", #longtable=T, "repeat header",
    row.names = FALSE, booktabs = TRUE,  
    col.names = c("Obra","Zona","Modalidad","Caudal (m3/s)","Metros","USD/mt","Ahorro (m3)","USD/m3")) %>%
        #add_header_above(c(" "=5, "USD/m3 (ajuste)" = 2)) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center",full_width = FALSE, font_size=10) %>% # , full_width = FALSE
  pack_rows("2017", 1,7) %>% pack_rows("2018", 8,10) %>% pack_rows("2019", 11,16) %>% pack_rows("2021", 17,30) %>%
  footnote( general = "Elab. propia en base DGI (2021).", general_title = "Fuente: ", title_format = "italic", 
    footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


MzaTable <- Mendoza %>% arrange(Ano) %>% filter(!is.na(ahorroAgua2)) %>%
    select(Obra,UM,metros,InvUsd,InvMtUsd,EfAnte,EfPost,ahorroAgua2,valueAhorro2) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTable}Río Mendoza - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 10)),
    row.names = FALSE, booktabs = TRUE,  
    col.names = c("Cauce","Zona","Metros","Inv.(USD)","Inv.(USD/mt)","EfC(0)","EfC(1)","A(m3/Obra)","USD/m3")
  ) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=12) %>% # latex_options = c("striped", "scale_down")
  pack_rows("2017", 1,7) %>% pack_rows("2018", 8,10) %>% pack_rows("2019", 11,16) %>% #pack_rows("2020", 18,18) %>% 
  pack_rows("2021", 17,30) %>%
  footnote( general = "Elab. propia en base DGI (2021).", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
    footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

## @knitr MzaCaudales
Mendoza %>% select(Ano,CodigoCauce, Obra,Q0, Qwebav,Qwebyear,KmTierra,metros,PerdidaxKm, ACaudalUm,VolHa,EfPost,EfAnte) %>%
  mutate(GananciaEfC= paste0(GananciaEfC= (EfPost-EfAnte)*100,"%")) %>%
  arrange(Ano) %>% 
  select(CodigoCauce, Obra,VolHa,GananciaEfC) %>% filter(!is.na(VolHa)) %>%
  mutate_all(linebreak) %>%
  mutate_all(funs(replace_na(., "-"))) %>%
  kable(format = "latex",caption = "\\label{tab:MzaCaudales}Mendoza - Caudal por obra información disponible (m3/s)", align = c("c", "l",rep("c", 10)),
    row.names = FALSE, booktabs = TRUE,  col.names = c("Código","Obra", #'estimado m3/s','web m3/s',
                                                       'm3/año',"GananciaEfC")) %>% 
footnote( general = "Elaboración propia en base a DGI (2015), Datos abiertos (2019) y entrevistas (2020,2021).", general_title = "Fuente: ", title_format = "italic",
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

OfertaMza <- Mendoza %>% select(Obra,metros,SUP_HA,Hectareas,InvUsd,InvMtUsd,metros,valueEfBis,ACaudalAnualUm,valuePerdBis,DeltaPerdida,valueAhorro,ahorroAgua,valueAhorro2,ahorroAgua2) %>% 
  arrange(valueEfBis)

round(mean(OfertaMza$valueAhorro2, na.rm=T), digits = 3)
CProm <- round(mean(OfertaMza$valueAhorro2, na.rm=T), digits = 3)


## Curva Costo marginal enfoque de diferencias en EfC
OfertaMza <- OfertaMza %>% select(everything()) %>% arrange(valueEfBis) %>%
 mutate(AAcum = cumsum(OfertaMza$ACaudalAnualUm))
AhorroMza <- ggplot(OfertaMza) + # , color=Zona
  geom_step(aes(y= valueEfBis, x=AAcum/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueEfBis, x=AAcum/1000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum


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
ggsave('DgiData/Graphs/OfertaMza_efi.png', height = 4, width = 12)

## Curva Costo marginal enfoque de eficiencia en distancia revestida (pérdida)
## @knitr AhorroMzaPerd
 
OfertaMza <- arrange(OfertaMza,valuePerdBis)
OfertaMza$AAcum1 = cumsum(OfertaMza$DeltaPerdida)

AhorroMza1 <- ggplot(OfertaMza) + 
  geom_step(aes(y= valuePerdBis, x=AAcum1/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valuePerdBis, x=AAcum1/1000, label = Obra, angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum1/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum
AhorroMza1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                  axis.text.y = element_text(size = 10),
                  panel.background = element_rect(fill = "white"), 
                  axis.title = element_text(size = 9)) + 
  scale_y_continuous(breaks = c(seq(0,12,3),20,30,40,50,60)) + 
  theme(axis.line = element_line(colour = "grey50")) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMza_perd.png', height = 4, width = 12)


## Curva CMg: eficiencia en distancia revestida (pérdida) volúmen anual de agua con superficie según mapas
## @knitr AhorroMzaPerd2

OfertaMza <- arrange(OfertaMza,valueAhorro2) 
OfertaMza$AAcum2   <- cumsum(OfertaMza$ahorroAgua2) 
OfertaMza$InvAcum2  <- cumsum(OfertaMza$InvUsd) 
media2   <- round(mean(OfertaMza$valueAhorro2,na.rm = T), 3)
mediana2 <- round(median(OfertaMza$valueAhorro2,na.rm = T),2)


AhorroMza2 <- ggplot(OfertaMza) + 
  geom_step(aes(y= valueAhorro2, x=AAcum2/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueAhorro2, x=AAcum2/1000, label = Obra, angle=0,hjust=-.3,vjust=-.3), 
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum2/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroMza2 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
  axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
  axis.title = element_text(size = 9)) + 
  geom_text(aes(1,media2, label = paste('promedio',media2), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=11/.pt) + 
  geom_text(aes(1,mediana2, label = paste('mediana',mediana2), vjust = -1,hjust=-.1), color = "#E69F00",  size=4) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana2, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media2, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaMza_Ahorro2.png', height = 10, width = 12)


## Curva CMg: eficiencia en distancia revestida (pérdida) volúmen anual de agua con superficie según mapas
## @knitr AhorroMzaPerd3
OfertaMza          <- arrange(OfertaMza,valueAhorro) 
OfertaMza$AAcum3   <- cumsum(OfertaMza$ahorroAgua) 
OfertaMza$InvAcum3  <- cumsum(OfertaMza$InvUsd) 
media3   <- round(mean(OfertaMza$valueAhorro,na.rm = T), 3)
mediana3 <- round(median(OfertaMza$valueAhorro,na.rm = T),2)

AhorroMza3 <- ggplot(OfertaMza) + 
  geom_step(aes(y= valueAhorro, x=AAcum3/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueAhorro, x=AAcum3/1000, label = Obra, angle=0), 
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum3/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroMza3 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
  axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
  axis.title = element_text(size = 9)) + theme(axis.line = element_line(colour = "grey50")) + 
  geom_text(aes(1,media3, label = paste('promedio',media3), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=11/.pt) + 
  geom_text(aes(1,mediana3, label = paste('mediana',mediana3), vjust = -1,hjust=-.1), color = "#E69F00",  size=4) + 
  geom_hline(yintercept = mediana3, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media3, color = "#E79F01", linetype="dashed", size=0.5) +
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMza_Ahorro.png', height = 10, width = 12)

## Curva CMg: eficiencia en distancia revestida (pérdida) volúmen anual de agua con superficie según mapas
## @knitr AhorroMzaSuperficie
OfertaMza          <- OfertaMza %>% select(everything()) %>% filter(!is.na(SUP_HA)) %>% arrange(valueAhorro) 
OfertaMza$AAcum4   <- cumsum(OfertaMza$SUP_HA) 
OfertaMza$InvAcum4  <- cumsum(OfertaMza$InvUsd) 
#media4   <- round(mean(OfertaMza$valueAhorro,na.rm = T), 3)
#mediana4 <- round(median(OfertaMza$valueAhorro,na.rm = T),2)

AhorroMza4 <- ggplot(OfertaMza) + 
  geom_step(aes(y= valueAhorro, x=AAcum4/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= valueAhorro, x=AAcum4/1000, label = Obra, angle=0), 
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum4/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroMza4 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("superficie beneficiada ('000 ha)") + ylab("Dólares por m3 anual ahorrado")

## @knitr CtoPromedio
cat(CProm)


## @knitr MendozaInvTables
OfertaMzaInv          <- as.data.frame(OfertaMza %>% arrange(AAcum2))
OfertaMzaInv$InvAcum  <- cumsum(OfertaMzaInv$InvUsd) 

SumInv <- OfertaMzaInv %>%
  dplyr::select(.data$InvAcum, .data$metros, .data$AAcum2) %>%
  qsummary(.)


MzaSum <-  rbind(round(OfertaMzaInv$InvAcum[[18]], digits = 0),
                  round(OfertaMzaInv$AAcum2[[18]]/1000,digits = 1),
                  round(OfertaMzaInv$AAcum2[[18]]/2073600,digits = 3))
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

Mendoza %>% select(CodigoCauce,ahorroAgua2,valueAhorro2,valueAhorro,Obra) 
summary(Mendoza$valueAhorro2)
summary(Mendoza$ahorroAgua2)
median(Mendoza$valueAhorro2,na.rm = T)*median(Mendoza$ahorroAgua2,na.rm =T)

summary(OfertaMza$valueAhorro2)
summary(OfertaMza$ahorroAgua2)
median(OfertaMza$valueAhorro2,na.rm = T)*median(OfertaMza$ahorroAgua2,na.rm =T)
OfertaMza %>% select(ahorroAgua2,valueAhorro2,AAcum2,InvAcum2,InvMtUsd,Obra) %>% arrange(AAcum2)

# T.Superior ----

## @knitr SupEf
{options(digits = 3)
EfSuperior <- as.data.frame(read.csv("DgiData/EfConduccion/TunuyanSuperior/EfCondTunSup.csv", sep = ",")) #, header = TRUE, sep=",")
indxx <- c("CodigoCauce","Superficie","LongTotal","LongRevest","EfRevest","LongHijuelas","PorcRevest","EfTierra",
           "EfGlobal","Caudal","CoefMoritz","PerdidaPorcentaje","TiempoMojado","PerdidaTiempo","EfUM")
EfSuperior[indxx] <- lapply(EfSuperior[indxx], function(x) round(as.numeric(as.character(x)), 3))

supAgua <- as.data.frame(read.csv("DgiData/TunSuperior_ObrasObredor.csv", sep = ",")) 
supAgua <- supAgua %>% select(CodigoCauce,metros,ha,VolHa) %>% rename(longitud=metros, haObrador=ha)
indxx <- c("CodigoCauce","longitud",'haObrador','VolHa')
supAgua[indxx] <- lapply(supAgua[indxx], function(x) round(as.numeric(as.character(x)), 3))

supCanales <- EfSuperior %>% filter(Clasificacion=='canal') %>%
  select(CodigoCauce,LongTotal,LongHijuelas,EfTierra,Superficie) %>%
  rename( canLong= LongTotal, canLongTierra= LongHijuelas, canEfc= EfTierra,canSup= Superficie)
supHij <- EfSuperior %>% filter(Clasificacion=='hijuela') %>%
  select(CodigoCauce,LongTotal,LongHijuelas,EfTierra,Superficie) %>%
  rename( hijLong= LongTotal, hijLongTierra= LongHijuelas, hijEfc=EfTierra, hijSup=Superficie)
supTierra <- EfSuperior %>% filter(Clasificacion=='r-tierra') %>%
  select(CodigoCauce,LongTotal,LongHijuelas,EfTierra,Superficie) %>%
  rename( tieLong= LongTotal, tieLongTierra= LongHijuelas, tieEfc=EfTierra, tieSup=Superficie)
supRama <- EfSuperior %>% filter(Clasificacion=='rama') %>%
  select(CodigoCauce,LongTotal,LongHijuelas,EfTierra,Superficie) %>%
  rename( ramLong= LongTotal, ramLongTierra= LongHijuelas, ramEfc=EfTierra, ramSup=Superficie)

efSup <- full_join(supRama,supTierra, by="CodigoCauce", all.y=T) #%>% arrange(CodigoCauce)#all.x=TRUE)
efSup <- full_join(efSup,supHij,by="CodigoCauce")
efSup <- full_join(efSup,supCanales,by="CodigoCauce")
efSup <- left_join(efSup,supAgua,by='CodigoCauce',all.x=T)
efSup <- efSup %>% distinct(CodigoCauce, .keep_all = TRUE)
remove(supHij,supTierra,supRama,supCanales)
}
glimpse(efSup)
#write_csv(efSup, 'DgiData/EfConduccion/efSuperior.csv', na = "NA", append = FALSE, quote_escape = "double")

Superior <- DgiFull %>% filter(Subdelegacion=="Tun. Superior", metros>0, !is.na(CodigoCauce)) %>%
          select(Ano,CodigoCauce,Obra,Modalidad,metros,InvUsd,InvMtUsd,hectareas,clasif)

Superior <- merge(x= Superior, y=efSup,#y= EfSuperior[ , c(1:7,9:10,15:23)], 
             by= c("CodigoCauce"), all.x=TRUE)
Superior <- Superior %>% select(everything(),-tieLong,-tieLongTierra,-tieEfc,-tieSup) # not used

longCanalesTipo <- as.data.frame(read.csv('DgiData/longCanalesTipo.csv', sep = ","))
colnames(longCanalesTipo)[3:8] <- c('mtTierra','mtHormigon','mtEntubado','mtPvc','mtChapa','mtSD')
glimpse(longCanalesTipo)
class(longCanalesTipo$CodigoCauce)
longCanalesTipo$CodigoCauce <- as.factor(longCanalesTipo$CodigoCauce)
longCanalesTipo <- longCanalesTipo %>% mutate(kmTotal=((mtTierra + mtHormigon + mtEntubado + mtPvc + mtChapa + mtSD)/1000))
Superior <- merge(x= Superior, y= longCanalesTipo[, c(1,9)], by = c("CodigoCauce"), all.x = TRUE)
glimpse(Superior)

# Eficiencia post entubamiento ==1 & revestimiento 0.99
Superior$EfPost  <- ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Superior$Obra), 0.995,
       ifelse(grepl("Mej. | Mejora", Superior$Obra), 0.99, 
       ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Superior$Obra), 1,  0.98)))

# Estimación de ahorro por menor infiltración
#write_csv(Superior, 'DgiData/Superior.csv', na = "NA", append = FALSE, quote_escape = "double")
Superior <- Superior %>% mutate(ahorroAgua1 = if_else(clasif!='Hijuela' & (!is.na(canEfc)), 
  VolHa * canSup * canEfc/canLongTierra * EfPost * (metros/1000),VolHa * hijSup * hijEfc/hijLongTierra * EfPost * (metros/1000)))
Superior <- Superior %>% mutate(ahorroAgua1 = if_else(is.na(ahorroAgua1) & !is.na(canEfc),
  VolHa * canSup * canEfc/canLongTierra * EfPost * (metros/1000), ahorroAgua1 ))
Superior <- Superior %>% mutate(ahorroAgua1= if_else(is.na(ahorroAgua1) & (clasif=='Hijuela' | clasif=='otro'),
  VolHa * canSup * canEfc/canLongTierra * EfPost * (metros/1000),ahorroAgua1))
Superior <- Superior %>% mutate(ahorroAgua1= if_else(is.na(ahorroAgua1) & is.na(canSup),
  VolHa * (hectareas/4) * canEfc/canLong * EfPost * (metros/1000),ahorroAgua1))

Superior <- Superior %>% mutate(ahorroAgua2 = if_else(clasif!='Hijuela' & (!is.na(canEfc)), VolHa * hectareas * canEfc/kmTotal * 0.99 * (metros/1000),
                      VolHa * hectareas * hijEfc/kmTotal * EfPost * (metros/1000)))
Superior <- Superior %>% mutate(ahorroAgua3 = if_else(clasif!='Hijuela' & (!is.na(canEfc)), VolHa * hectareas * canEfc/canLongTierra * EfPost * (metros/1000),
                      VolHa * hectareas * hijEfc/hijLongTierra * EfPost * (metros/1000)))
Superior <- Superior %>% mutate(ahorroAgua4 = if_else(clasif!='Hijuela' & (!is.na(canEfc)), VolHa * haObrador * canEfc/longitud * EfPost * (metros/1000),
         VolHa * haObrador * hijEfc/longitud * EfPost * (metros/1000)))

# Valor del agua ahorrada
Superior <- Superior %>% mutate(vAhorro1 = round(InvUsd/ahorroAgua1,3),
                                vAhorro2 = round(InvUsd/ahorroAgua2,3),
                                vAhorro3 = round(InvUsd/ahorroAgua3,3),
                                vAhorro4 = round(InvUsd/ahorroAgua4,3))
# adicional plus obras x administración
Superior <- Superior %>% 
  mutate(vAhorro1 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro1, vAhorro1*1.32),
         vAhorro2 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro2, vAhorro2*1.32),
         vAhorro3 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro3, vAhorro3*1.32),
         vAhorro4 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro4, vAhorro4*1.32))
Superior <- Superior %>% arrange(Ano,vAhorro1)
#write_csv(Superior, 'DgiData/Estimaciones/Ahorro.csv', na = "NA", append = FALSE, quote_escape = "double")


Superior %>% select(CodigoCauce,InvUsd,InvMtUsd,VolHa,hectareas,canSup,hijSup,metros,kmTotal,ahorroAgua1,vAhorro1,ahorroAgua4,vAhorro4,Obra) %>% arrange(ahorroAgua1)
summary(Superior$vAhorro1)

SupTableComparacion <- Superior %>% select(Ano,Obra, Modalidad, metros,InvMtUsd,ahorroAgua1,vAhorro1) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTableComparacion}Tunuyán Superior", align = c("l", "l",rep("r", 12)),
        row.names = FALSE, booktabs = TRUE,  
        col.names = c("Año","Obra","Modalidad","Metros","USD/mt","Ahorro m3/km","USD/m3")) %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=11) %>%
  column_spec(1,bold = F) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  footnote( general = "Elab. propia en base DGI (2021).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 

SupTable <- Superior %>% arrange(InvUsd) %>% 
  select(Obra, metros,InvUsd,ahorroAgua1,InvMtUsd,vAhorro1) %>% filter(!is.na(ahorroAgua1)) %>%
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:SupTable}Tunuyán Superior - Valores por obra de revestimiento ejecutada", align = c("l", "c",rep("r", 4)),
        booktabs = TRUE,  col.names = c("Obra","Metros","Inv (USD)","A(m3/km)", "Inv.(USD/mt)","USD/m3")) %>%
  kable_styling(latex_options = c("scale_down","HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  column_spec(1,bold = F) %>% collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
  footnote( general = "Elab. propia en base DGI (2021).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) 


## @knitr SupCaudales
options(scipen=999) 
Superior %>% select(Ano,CodigoCauce, Obra,metros, ahorroAgua1,EfPost,hijEfc,canEfc,VolHa,clasif,canLongTierra,canSup,hijLongTierra) %>%
  mutate(GananciaEfC= paste0(GananciaEfC= 
     round(if_else(clasif!='Hijuela' & !is.na(canEfc),(EfPost-canEfc)*100,(EfPost-hijEfc)*100),4),'%'),
     ahorroAgua1=ahorroAgua1/1000) %>%
  arrange(Ano,CodigoCauce) %>% filter(ahorroAgua1!='NA') %>% #mutate_all(funs(replace_na(., "-"))) 
  select(CodigoCauce, Obra, VolHa,ahorroAgua1,GananciaEfC) %>%
  mutate_all(linebreak) %>% 
  kable(format = "latex",caption = "\\label{tab:SupCaudales}Tunuyán Superior - Caudal promedio por obra información disponible (m3/s)", align = c("c", "l",rep("c", 8)),
        row.names = FALSE, booktabs = TRUE,  col.names = c("Código","Obra","Caudal (m3/ha/año)","ahorro '000 (m3/año)","GananciaEfC")) %>%
  kable_styling(latex_options = "scale_down") %>%
  footnote( general = "Elaboración propia en base a DGI (2015), Datos abiertos (2019) y entrevistas (2020, 2021).", general_title = "Fuente: ", title_format = "italic", 
            footnote_as_chunk=TRUE, escape=FALSE, threeparttable = T)

## @knitr SupTableComparacion
SupTableComparacion %>% landscape()

## @knitr SupTable
SupTable

## @knitr SupTableLandscape
SupTable %>% 
  landscape()

## Curva Costo marginal enfoque de eficiencia en distancia revestida (pérdida)
## @knitr ahorroSup1

OfertaSup           <- Superior %>% select(CodigoCauce,Obra,metros,hectareas,InvUsd,InvMtUsd,metros,ahorroAgua1:ahorroAgua4,vAhorro1:vAhorro4) %>% arrange(vAhorro1)
OfertaSup$AAcum1    <- cumsum(OfertaSup$ahorroAgua1) 
OfertaSup$InvAcum1  <- cumsum(OfertaSup$InvUsd) 
media1              <- round(mean(OfertaSup$vAhorro1,na.rm = T), 3)
mediana1            <- round(median(OfertaSup$vAhorro1,na.rm = T),2)

AhorroSup1 <- ggplot(OfertaSup) + 
  geom_step(aes(y= vAhorro1, x=AAcum1/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro1, x=AAcum1/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum1/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroSup1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media1,   label = paste('prom.', media1), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana1, label = paste('med.', mediana1), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana1, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media1, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaSup_Ahorro1.png', height = 10, width = 12)

## Curva CMg: eficiencia en distancia revestida (pérdida) volúmen anual de agua con superficie según mapas

## @knitr AhorroSup4

OfertaSup <- OfertaSup %>% arrange(vAhorro4)
OfertaSup$AAcum4    <- cumsum(OfertaSup$ahorroAgua4) 
OfertaSup$InvAcum4  <- cumsum(OfertaSup$InvUsd) 
media4              <- round(mean(OfertaSup$vAhorro4,na.rm = T), 3)
mediana4            <- round(median(OfertaSup$vAhorro4,na.rm = T),2)

AhorroSup4 <- ggplot(OfertaSup) + 
  geom_step(aes(y= vAhorro4, x=AAcum4/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro4, x=AAcum4/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum4/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroSup4 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media4,   label = paste('prom.', media4), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana4, label = paste('med.', mediana4), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,201) + 
  geom_hline(yintercept = mediana4, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media4, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaSup_Ahorro4.png', height = 10, width = 12)





# estancado
ctunsup <- cuenca %>% select(CUENCA, TIPO,NOMBRE,SUP_HA,ALT_MEDIA) %>% filter(CUENCA=="RÃ­o TunuyÃ¡n Superior")
#umendoza <- cuenca %>% filter(CUENCA=="RÃ­o Mendoza" & TIPO=='UM') %>% select(NOMBRE,SUP_HA) %>% arrange(NOMBRE)
# umendoza$Inspeccion <- as.numeric(as.character(NA))
#  umendoza$Inspeccion[58] <- 24 # "Tulumaya"
#  umendoza$Inspeccion <- ifelse(is.na(umendoza$Inspeccion),1,umendoza$Inspeccion)
#  umendoza$Inspeccion <- factor(umendoza$Inspeccion, levels = c(1,2),labels = c('otro','Algarrobal'))
# colnames(EfSuperior)[20:23] <- c('caudalCanal','caudalHijuela','caudalTierra','caudalRama')



# T. Inferior ----

## Orden de datos ----
## @knitr InfEf
tInf <- as.data.table(DgiFull %>%
        filter(Subdelegacion=="Tun. Inferior" & Estado!="NO EJECUTADA" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
        select(CodigoCauce, InvUsd,InvMtUsd,metros, Ano, Modalidad, Obra, hectareas) %>% # , CodigoCauceAlt
        arrange(Ano,CodigoCauce)) 

EfInferior <- as.data.frame(read.csv("DgiData/EfConduccion/TunuyanInferior/EfCondInferior.csv", sep = ",")) #, header = TRUE, sep=",")
EfInferior <- EfInferior[,c(1:12)]
indxx <- c("UM","Cauce","CodigoCauce","EfC","haBalance2015","LongitudRed","Revestida","SinRevestir",
           'hasLuqui','caudalLuqui','volHaLuqui',"RevestPorc")
EfInferior[indxx] <- lapply(EfInferior[indxx], function(x) as.numeric(as.character(x)))

## @knitr InferiorTable

Inferior <- tInf[ !is.na(CodigoCauce),]
Inferior <- merge(x= Inferior,y= EfInferior,
                  by= c("CodigoCauce"), all.x=TRUE)
arrange(Inferior,CodigoCauce)
Inferior <- Inferior %>% select(everything()) %>% distinct(Obra, .keep_all = TRUE)

# Eficiencia post entubamiento ==1 & revestimiento 0.99
Inferior$EfPost     <- Inferior[, ifelse(grepl("Ent. | Entubado | Entubamiento | entubado | entubamiento | ENTUBADO | ENTUBAMIENTO", Inferior$Obra), 1,
                            ifelse(grepl("Rev. | Revestimiento | revestimiento | Canalización | REVESTIMIENTO", Inferior$Obra), 0.995,
                            ifelse(grepl("Mej. | Mejora", Inferior$Obra), 0.987,  0.98)))]
Inferior$EfAnte     <- Inferior$EfC/100 # ifelse(!is.na(Inferior$EfTierra),Inferior$EfTierra,Inferior$EfGlobal)

## Ahorro Inferior ----

# ahorro agua 1, volumen, superficie según Luqui. Extension canales (Balance 2015)
Inferior <- Inferior %>% mutate(ahorroAgua1= 
    volHaLuqui * hasLuqui * EfAnte/(SinRevestir/1000) * EfPost * (metros/1000))
# ahorro agua 2, volumen según Luqui. Superficie (plan de obras) y extension canales (Balance 2015)
Inferior <- Inferior %>% mutate(ahorroAgua2= 
    volHaLuqui * hectareas * EfAnte/(SinRevestir/1000) * EfPost * (metros/1000))
# ahorro agua 3, volumen según Luqui. Superficie irrigada y extension canales (Balance 2015)
Inferior <- Inferior %>% mutate(ahorroAgua3 = 
    volHaLuqui * haBalance2015 * EfAnte/(SinRevestir/1000) * EfPost * (metros/1000))

#if_else(is.na(ahorroAgua1) & is.na(canInf),VolHa * (hectareas/4) * canEfc/canLong * EfPost * (metros/1000),ahorroAgua1))

## Valoración ahorro ----
# Valor del agua ahorrada

Inferior <- Inferior %>% mutate(vAhorro1 = round(InvUsd/ahorroAgua1,3),
                                vAhorro2 = round(InvUsd/ahorroAgua2,3),
                                vAhorro3 = round(InvUsd/ahorroAgua3,3))
# adicional plus obras x administración
Inferior <- Inferior %>% 
  mutate(vAhorro1 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro1, vAhorro1*1.32),
         vAhorro2 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro2, vAhorro2*1.32),
         vAhorro3 = if_else(( Modalidad=="Licitación" | Modalidad=="Licitacion" | Modalidad=="LICITADA" | Modalidad=="Lic." | Modalidad=="Lic"), vAhorro3, vAhorro3*1.32))
Inferior <- Inferior %>% arrange(Ano,vAhorro1)

Inferior %>% select(CodigoCauce,volHaLuqui,hasLuqui,SinRevestir,Ano,InvMtUsd,metros,EfAnte,EfPost,ahorroAgua1:vAhorro3) %>% arrange(vAhorro1)

## Curva Costo marginal enfoque de eficiencia en distancia revestida (pérdida) ----
## @knitr ahorroInf1
OfertaInf           <- Inferior %>% select(CodigoCauce,Obra,metros,hectareas,hasLuqui,haBalance2015,InvUsd,InvMtUsd,metros,ahorroAgua1:ahorroAgua3,vAhorro1:vAhorro3) %>% arrange(vAhorro1)
OfertaInf$AAcum1    <- cumsum(OfertaInf$ahorroAgua1) 
OfertaInf$InvAcum1  <- cumsum(OfertaInf$InvUsd) 
media1              <- round(mean(OfertaInf$vAhorro1,na.rm = T), 3)
mediana1            <- round(median(OfertaInf$vAhorro1,na.rm = T),2)

AhorroInf1 <- ggplot(OfertaInf) + 
  geom_step(aes(y= vAhorro1, x=AAcum1/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro1, x=AAcum1/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaInf$AAcum1/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroInf1 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media1,   label = paste('prom.', media1), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana1, label = paste('med.', mediana1), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana1, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media1, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaInf_Ahorro1.png', height = 10, width = 12)

## @knitr ahorroInf2
OfertaInf           <- OfertaInf %>% arrange(vAhorro2)
OfertaInf$AAcum2    <- cumsum(OfertaInf$ahorroAgua2) 
OfertaInf$InvAcum2  <- cumsum(OfertaInf$InvUsd) 
media2              <- round(mean(OfertaInf$vAhorro2,na.rm = T), 3)
mediana2            <- round(median(OfertaInf$vAhorro2,na.rm = T),2)

AhorroInf2 <- ggplot(OfertaInf) + 
  geom_step(aes(y= vAhorro2, x=AAcum2/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro2, x=AAcum2/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaInf$AAcum2/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroInf2 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media2,   label = paste('prom.', media2), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana2, label = paste('med.', mediana2), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana2, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media2, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaInf_Ahorro2.png', height = 10, width = 12)

## @knitr ahorroInf3
OfertaInf           <- OfertaInf %>% arrange(vAhorro3)
OfertaInf$AAcum3    <- cumsum(OfertaInf$ahorroAgua3) 
OfertaInf$InvAcum3  <- cumsum(OfertaInf$InvUsd) 
media3              <- round(mean(OfertaInf$vAhorro3,na.rm = T), 3)
mediana3            <- round(median(OfertaInf$vAhorro3,na.rm = T),2)

AhorroInf3 <- ggplot(OfertaInf) + 
  geom_step(aes(y= vAhorro3, x=AAcum3/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro3, x=AAcum3/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaInf$AAcum3/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))

AhorroInf3 + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media3,   label = paste('prom.', media3), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana3, label = paste('med.', mediana3), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana3, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media3, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado")
ggsave('DgiData/Graphs/OfertaInf_Ahorro3.png', height = 10, width = 12)

# hay que revisar código de cauce de las obras viejas, consultar pendientes y agregar volúmenes anuales x código

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
DgiFull %>% filter(Ano>=2017) %>% group_by(Subdelegacion) %>% summarise(totCuenca=sum(metros,na.rm = T))

revest <- aggregate(DgiFull$metros, by= list(DgiFull$Ano,DgiFull$Subdelegacion), FUN=sum, na.rm= TRUE)
colnames(revest) <- c("Ano","Subdelegacion","metros") # m3/s

revest <- revest %>% select(Ano, Subdelegacion,metros) %>%
  spread(. , key='Subdelegacion',metros)
revestim <- revest %>% filter(Ano>=2017)
revAno    <- DgiFull %>% filter(Ano>=2017) %>% group_by(Ano) %>% summarise(totAno=sum(metros,na.rm = T))
#rowSums(revestim, na.rm = T) 
revCuenca <- DgiFull %>% filter(Ano>=2017) %>% group_by(Subdelegacion) %>% summarise(totCuenca=sum(metros,na.rm = T))
#colSums(revestim[,c(2:7)], na.rm = T)
rownames(revestim) <- revestim$Ano
revestim <- revestim[,c(2:7)]
revestim <- cbind(t(revestim),revCuenca)
revestim <- revestim[,c(1:5,7)]
revestim <- rbind(revestim,as.character(cbind(t(revAno[,2]),'-')))

Revestimiento <- revestim
rownames(Revestimiento) <- c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior",'Total')
colnames(Revestimiento) <- c(2017:2021,'Total')
#library(scales)
FormatoNum <- number_format(big.mark = ".", decimal.mark = ",")



## @knitr Revestimiento
Revestimiento %>% 
  kable("latex",caption = "\\label{Revestimiento}Metros revestidos por cuenca", align = c("l", rep("r", 6)),
        row.names = TRUE, booktabs = TRUE, col.names = c("2017","2018","2019","2020",'2021',"Total"),linesep = "", 
        ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elab. propia en base a DGI (2021)", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)



# river maps ----

## @knitr spdataLoad

# probar estimar extension de 

rios    <- st_read("/Users/SebastianRiera/Downloads/cursos_de_agua/cursos_de_agua.shp")

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

## Sub-sistemas -----

redRiego$subSist <- ifelse(
  # Las Tunas
  grepl("ANCHAYUYO | LAS TUNAS | GUALTALLARY | ARROYO VILLEGAS | CANAL ESQUINA | CANAL ANCON | ANCON | ANCÓN | ESQUINA | TUPUNGATO | RIO DE LA PAMPA | QUEBRADA DE GUEVARA | EL PERAL | HIJ. PALMA | PALMA | EL INGENIO | ARROYO ALTO VERDE | ARROYO GUIÐAZU | ARROYO GUIÑAZU | ACEQUIA DEL DIABLO | Sauce | CALLE QUINTANA | ARROYO CIENEGAS | ARROYO TORRECITAS | MATRIZ SUR", redRiego$denominacion,ignore.case = TRUE), 1,
  # Arroyo Grande y Diq. Valle de Uco
  ifelse(grepl("ARROYO GRANDE | SALAS CAROCA | LA PIRCA | ARROYO LA RIOJA | ARROYO LA BARRANCA | ARROYO SILVA O MANANTIALES | La Quebrada | CENTRO PRINCIPAL O RIO VIEJO | DESAGÜES PREDIOS PARTICULARES DE TUNUYAN | ARROYO CLARO - | CANAL UCO | CANAL DE UCO | MELOCOTON | RAMA QUIROGA | ELVIRA BUSTOS | MARGEN DERECHA | MATRIZ VALLE DE UCO | CONSULTA | CAPACHO | CANAL CAÐADA DE LAS ROSAS | DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS | PEÐALOZA | CANAL MANZANO | MANZANO | CANAL VISTA FLORES | CANAL RINCON | RINCÓN | ARROYO CLARO | QUEBRADA DE LAS CASAS", redRiego$denominacion,ignore.case = TRUE), 2,
  # Diq. Valle de Uco
  #ifelse(grepl("CANAL UCO | CANAL DE UCO | MELOCOTON | RAMA QUIROGA | ELVIRA BUSTOS | MARGEN DERECHA | MATRIZ VALLE DE UCO | CONSULTA | CAPACHO | CANAL CAÐADA DE LAS ROSAS | DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS | PEÐALOZA | CANAL MANZANO | MANZANO | CANAL VISTA FLORES | CANAL RINCON | RINCÓN | ARROYO CLARO|QUEBRADA DE LAS CASAS", redRiego$denominacion,ignore.case = TRUE), 3,
  # Yaucha - Aguanda
  ifelse(grepl("MATRIZ YAUCHA | MATRIZ AGUANDA | YAUCHA | AGUANDA | CANAL MATRIZ YAUCHA | ARROYO LA SALAMANCA", redRiego$denominacion,ignore.case = TRUE), 4,
  ifelse(grepl("RIO MENDOZA",redRiego$subdelegacion,ignore.case = TRUE),5,
  ifelse(grepl("RIO TUNUYAN INFERIOR",redRiego$subdelegacion,ignore.case = TRUE),6,
  ifelse(grepl("RIO MALARGUE",redRiego$subdelegacion,ignore.case = TRUE),7,
  ifelse(grepl("RIO DIAMANTE",redRiego$subdelegacion,ignore.case = TRUE),8,
  ifelse(grepl("RIO ATUEL",redRiego$subdelegacion,ignore.case = TRUE),9,
         999))))))))

# controlando por los espacios
redRiego$subSist <- ifelse(
  # Las Tunas
  grepl("ANCHAYUYO|LAS TUNAS|GUALTALLARY|ARROYO VILLEGAS|CANAL ESQUINA|CANAL ANCON|ANCON|ANCÓN|ESQUINA|TUPUNGATO|RIO DE LA PAMPA|QUEBRADA DE GUEVARA|EL PERAL|HIJ. PALMA|PALMA|EL INGENIO|ARROYO ALTO VERDE|ARROYO GUIÐAZU|ARROYO GUIÑAZU|ACEQUIA DEL DIABLO|Sauce|CALLE QUINTANA|ARROYO CIENEGAS|ARROYO TORRECITAS|MATRIZ SUR", redRiego$denominacion,ignore.case = TRUE), 1,
  # Arroyo Grande y Diq. Valle de Uco
  ifelse(grepl("ARROYO GRANDE|SALAS CAROCA|LA PIRCA|ARROYO LA RIOJA|ARROYO LA BARRANCA|ARROYO SILVA O MANANTIALES|La Quebrada|CENTRO PRINCIPAL O RIO VIEJO|DESAGÜES PREDIOS PARTICULARES DE TUNUYAN|ARROYO CLARO -|CANAL UCO|CANAL DE UCO|MELOCOTON|RAMA QUIROGA|ELVIRA BUSTOS|MARGEN DERECHA|MATRIZ VALLE DE UCO|CONSULTA|CAPACHO|CANAL CAÐADA DE LAS ROSAS|DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS|PEÐALOZA|CANAL MANZANO|MANZANO|CANAL VISTA FLORES|CANAL RINCON|RINCÓN|ARROYO CLARO|QUEBRADA DE LAS CASAS", redRiego$denominacion,ignore.case = TRUE), 2,
  # Diq. Valle de Uco
  #ifelse(grepl("CANAL UCO|CANAL DE UCO|MELOCOTON|RAMA QUIROGA|ELVIRA BUSTOS|MARGEN DERECHA|MATRIZ VALLE DE UCO|CONSULTA|CAPACHO|CANAL CAÐADA DE LAS ROSAS|DESAGÜES PREDIOS PARTICULARES DE SAN CARLOS|PEÐALOZA|CANAL MANZANO|MANZANO|CANAL VISTA FLORES|CANAL RINCON|RINCÓN|ARROYO CLARO", redRiego$denominacion,ignore.case = TRUE), 3,
  # Yaucha - Aguanda
  ifelse(grepl("MATRIZ YAUCHA|MATRIZ AGUANDA|YAUCHA|AGUANDA|CANAL MATRIZ YAUCHA|ARROYO LA SALAMANCA", redRiego$denominacion,ignore.case = TRUE), 4,
  ifelse(grepl("RIO MENDOZA",redRiego$subdelegacion,ignore.case = TRUE),5,
  ifelse(grepl("RIO TUNUYAN INFERIOR",redRiego$subdelegacion,ignore.case = TRUE),6,
  ifelse(grepl("RIO MALARGUE",redRiego$subdelegacion,ignore.case = TRUE),7,
  ifelse(grepl("RIO DIAMANTE",redRiego$subdelegacion,ignore.case = TRUE),8,
  ifelse(grepl("RIO ATUEL",redRiego$subdelegacion,ignore.case = TRUE),9,
          999))))))))

redRiego$subSist <- ifelse( (redRiego$codigo==9774 | redRiego$codigo==9705 | redRiego$codigo==9713), 1, redRiego$subSist)
redRiego$subSist <- factor(redRiego$subSist, levels = c(1:2,4:9,999),
  labels = c('Las Tunas','Arroyo Grande y Diq. Valle de Uco',#'Diq. Valle de Uco',
   'Yaucha-Aguanda','Mendoza','T.Inferior','Malargue','Diamante','Atuel','otro'))

## @knitr subSistemas
ptosMapa <- rbind(`Dique Valle de Uco`= c(-33.759477, -69.212469),
                  `Dique Las Tunas`= c(-33.375452, -69.373016))
colnames(ptosMapa) <- c('latitud', 'longitud')


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
ggsave('figure/tsSubsistemas.pdf', height = 5, width = 4, units = 'in')

## @knitr subsist_control
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


## @knitr ahorroSist2

redRiegoCat <- spread(redRiego, key= 'id_cat', long_m)
redRiegoCat <- redRiegoCat %>% 
  select(CodigoCauce,codigo,subdel,subSist,id_mat,Canal,Hijuela,`Puente Canal`,Rama,Sifon,`Linea Auxiliar`, Proyectado,everything()) %>%
  relocate(geometry, .after = last_col())

summary(redRiegoCat$id_mat)  
redCodigo <- redRiegoCat %>% 
  group_by(codigo,subdel,subSist,CodigoCauce) %>%
  summarize_if(is.numeric, sum, na.rm=T) 

#redCodigo$CodigoCauce <- redCodigo$codigo
redCodigo$CodigoCauce <- as.numeric(as.character(redCodigo$CodigoCauce))
#redCodigo$totalTipo <- rowSums(redCodigo[,c(5:11,13:21)], na.rm = T)
arrange(redCodigo,CodigoCauce)
arrange(OfertaSup,CodigoCauce)

## Estimaciones ahora geoespaciales ----
OfertaSup <- merge(x= OfertaSup, 
 y= redCodigo[redCodigo$subdel=='Rio Tunuyan Superior',c("CodigoCauce","subSist","Canal","Hijuela","Puente Canal",
 "Rama","Sifon","Linea Auxiliar","Proyectado","Arroyo", "Canal Viejo","Desagüe","Ramo","Rio","Hormigon")], 
 by = c('CodigoCauce'), all.x = TRUE)

OfertaSup$totalTipo <- rowSums(OfertaSup[,20:32], na.rm = T)
colnames(OfertaSup)[33] <- 'geometry'
arrange(OfertaSup,vAhorro1)

OfertaSup$subSist <- ifelse((OfertaSup$CodigoCauce==9774 | OfertaSup$CodigoCauce==9705 | OfertaSup$CodigoCauce==9713), 
                      1, OfertaSup$subSist)
OfertaSup <- OfertaSup %>% 
  mutate(subSist = if_else((CodigoCauce==9724 & is.na(subSist)), 2, subSist))

OfertaSup$subSist <- factor(OfertaSup$subSist, levels = c(1:3),
    labels = c('Las Tunas','Arroyo Grande y Diq. Valle de Uco','Yaucha-Aguanda'))


ahorroSist <- OfertaSup %>% select(vAhorro1,AAcum1,Obra,subSist) %>%
  ggplot(.) + geom_step(aes(y= vAhorro1, x=AAcum1/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro1, x=AAcum1/1000, label = Obra, angle=0),
            vjust=-1, hjust=1, size=3, check_overlap = T, inherit.aes = T,nudge_x = -10, nudge_y = 0) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum1/1000, digits = 0), guide = guide_axis(check.overlap = TRUE))


## @knitr ahorroSist
ahorroSist + theme(axis.text.x = element_text(size = 9, angle=75, vjust = .4), 
                   axis.text.y = element_text(size = 10), panel.background = element_rect(fill = "white"), 
                   axis.title = element_text(size = 9)) + 
  geom_text(aes(1, media1,   label = paste('prom.', media1), vjust = -1,hjust=-.1), color = "#E69F00", fontface='plain',size=8/.pt) + 
  geom_text(aes(1, mediana1, label = paste('med.', mediana1), vjust = -1,hjust=-.1), color = "#E69F00",  size=3) + 
  ylim(-.1,2) + 
  geom_hline(yintercept = mediana1, color = "#E69F00", linetype="dashed", size=0.5) +
  geom_hline(yintercept = media1, color = "#E79F01", linetype="dashed", size=0.5) +
  theme(axis.line = element_line(colour = "grey50")) + 
  xlab("metros cúbicos anuales ('000)") + ylab("Dólares por m3 anual ahorrado") + facet_wrap(~subSist, nrow = 1)
ggsave('DgiData/Graphs/ofertaSist1.png', height = 4, width = 12)


## @knitr ahorroSist1
OfertaSup %>% select(vAhorro1,AAcum1,Obra,subSist) %>%
  ggplot(.) + geom_step(aes(y= vAhorro1, x=AAcum1/1000,colour=subSist)) + #color = "#0073D9", size = 1) + 
  geom_text(aes(y= vAhorro1, x=AAcum1/1000, label = Obra, angle=0,colour=subSist),
            vjust=3, hjust=.2, size=3, check_overlap = T, inherit.aes = T) + #,nudge_x = -10, nudge_y = -10) +
  scale_x_continuous(breaks= round(OfertaSup$AAcum1/1000, digits = 0), guide = guide_axis(check.overlap = TRUE)) + 
  ylim(0,1) + theme_bw() + 
  theme(legend.position = 'bottom',#c(0.25,0.75), legend.direction = 'horizontal',
        legend.title = element_blank()) + 
  facet_wrap(~subSist, nrow = 1)
ggsave('DgiData/Graphs/ofertaSist2.png', height = 4, width = 12)


## @knitr spdataLoad2

# Suma los metros por categoria y material de acuerdo al código de cauce. 3033 observations
redRiegoLong <- redRiego %>% #filter(long_m!=0) %>%  # filtra los valores mapeados con long_metros ==0
 group_by(CodigoCauce,id_cat,id_mat,subdel,subSist,codigo,denominacion) %>% summarise_at(c("long_m"), sum, na.rm = TRUE) #%>%
#write_csv(redRiegoLong, 'DgiData/redRiegoLong.csv', na = "NA", append = FALSE, quote_escape = "double")
glimpse(redRiegoLong)


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

## Make sure they have the same projection
redRiegoCat <- st_transform(redRiegoCat, crs = st_crs(cuenca))


tsMaterial <- redRiegoMat %>% select(CodigoCauce,subdel,subSist,Tierra:`Sin dato`) %>%
  filter(subdel=='Rio Tunuyan Superior') %>%
  group_by(CodigoCauce,subSist) %>%
  summarize_if(is.numeric, sum, na.rm=T) %>% arrange(CodigoCauce)


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
    


# Arranging Data ----



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
  qsummary(.)

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


