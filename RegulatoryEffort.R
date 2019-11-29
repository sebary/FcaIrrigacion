

## @knitr SetUp2
RegulatoryEffort <- c("foreach","rootSolve","ggthemes","kableExtra","qwraps2","tidyverse","data.table","scales","matrixStats","readr","directlabels","dplyr") 
# included in tidyverse: "dplyr","tidyr","ggplot2","pandoc","table1", "summarytools"
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
CaudalMen <- c(16.4, 18.5, 17.5, 26.3, 26.5, 39.9, 66.4, 61.2, 43.2, 26.1, 21.1, 16.9, 31.7)
names(CaudalMen) <- c("Jul","Ago","Sep","Oct","Nov","Dic","Ene","Feb","Mar","Abr","May","Jun","Módulo")
sapply(CaudalMen, class)

EfMendoza <- as.data.frame(read.csv("DgiData/EfCond.csv", sep = ";")) #, header = TRUE, sep=",")
indxx <- c("TierraQ0","TierraQ0rep","TierraQ0medio","TierraQl","TierraQlrep","TierraQlmedio","Distancia","KmInvert","EfcTierra","LongMedia","EfTierraLong","EfCanales","EfHijuelas","KmCanales")
EfMendoza[indxx] <- lapply(EfMendoza[indxx], function(x) as.numeric(as.character(x)))
view(EfMendoza)
}

Caudal19 <- c(590,490,140,850,460,460)
names(Caudal19) <- c("Atuel","Diamante","Malargüe","Mendoza","Tun.Inferior","Tun.Superior")
  
# Working with databases        ####

# Obras 2017 ####

## @knitr DataObras17

Dgi2017 <- read.csv("DgiData/DgiObras2017.csv", sep = ";") #, header = TRUE, sep=",")
head(Dgi2017,3)
indxx <- c("Codigo","Metros","MetrosAvance","CodigoCauce","CodigoCauceAlt","Obra","inversion","Ano")
Dgi2017[indxx] <- lapply(Dgi2017[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2017$inversion)

Dgi2017$metros <- ifelse(Dgi2017$MetrosAvance > Dgi2017$Metros, Dgi2017$MetrosAvance, Dgi2017$Metros)

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
           select(Codigo, CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano) %>% # , CodigoCauceAlt
           arrange(CodigoCauce)) 

# Obras 2018 ####

## @knitr DataObras18

Dgi2018 <- read.csv("DgiData/DgiObras2018.csv", sep = ";") #, header = TRUE, sep=",")

indxx <- c("Codigo","MONTO","MONTO.CONTRATO","hectareas","Padrones","LONGITUD","Ano") # Convert many columns in numeric #https://stackoverflow.com/questions/27528907/how-to-convert-data-frame-column-from-factor-to-numeric
Dgi2018[indxx] <- lapply(Dgi2018[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2018$MONTO)

Dgi2018$inversion <- ifelse(is.na(Dgi2018$MONTO),
                            ifelse(is.na(Dgi2018$MONTO.CONTRATO),
                              ifelse(is.na(Dgi2018$Presupuesto), " ", Dgi2018$Presupuesto),
                              Dgi2018$MONTO.CONTRATO),Dgi2018$MONTO)
# Rename
#Dgi2018       <- Dgi2018 %>% rename(metros = LONGITUD) #, Inversion = MONTO)
names(Dgi2018)[11]<-"metros"
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
    "Inversión" =
      list(
        "promedio (desv.std.)" = ~qwraps2::gmean(InvUsd, na_rm = TRUE),
        #"mediana (Q1, Q3)" = ~qwraps2::median_iqr(inversion, na_rm = TRUE),
        "min" = ~min(InvUsd, na.rm = TRUE),
        "max" = ~max(InvUsd, na.rm = TRUE) #, "sin datos" = ~sum(is.na(inversion))
      ),
   # "Padrones" =
    #  list(
     #   "promedio (d.s.)" = ~qwraps2::gmean(round(Padrones,digits = 0), na_rm = TRUE),
      #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(Padrones, na_rm = TRUE),
      #  "min" = ~min(Padrones, na.rm = TRUE),
       # "max" = ~max(Padrones, na.rm = TRUE) #,         "Missing" = ~sum(is.na(Padrones))
      #),
  #  "Superficie" =
   #   list( "promedio (d.s.)" = ~qwraps2::gmean(hectareas, na_rm = TRUE),
     #   "mediana (Q1, Q3)" = ~qwraps2::median_iqr(hectareas, na_rm = TRUE),
    #    "min" = ~min(hectareas, na.rm = TRUE),
     #   "max" = ~max(hectareas, na.rm = TRUE) #,         "Missing" = ~sum(is.na(Padrones))
        #  "High GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "high"), "Low GDP per Capita" = ~qwraps2::n_perc(na.omit(gdpPercap) %in% "low"), "Missing" = ~sum(is.na(gdpPercap))
      #),
    "Metros revestidos" =
      list(
        "promedio" = ~qwraps2::gmean(metros, na_rm = TRUE),
        #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(metros, na.rm = TRUE),
        "max" = ~max(metros, na.rm = TRUE) 
      ),
    "Inversion/mt" =
      list(
        "promedio" = ~qwraps2::gmean(InvMtUsd, na_rm = TRUE),
      #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(InvMtUsd, na.rm = TRUE),
        "max" = ~max(InvMtUsd, na.rm = TRUE) ) )

Dgi2018 %>%
  filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & ESTADO!="NO EJECUTADA") %>%
  select(CodigoCauce, Subdelegacion, hectareas, Padrones, inversion, metros, InvMtUsd,Ano) %>%
  mutate(coment= paste0(InvMtUsd," USD/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(Subdelegacion,InvMtUsd) 

Mza18 <- as.data.table(Dgi2018 %>%
              filter(Subdelegacion=="Mendoza" & ESTADO!="NO EJECUTADA" & (metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
              select(Codigo, CodigoCauce,Cauce,inversion, metros, InvUsd,InvMtUsd,Ano) %>% # , CodigoCauceAlt
              arrange(CodigoCauce)) 

#Mza18 <- merge(x= Mza18, y= EfMendoza[ , c(2,6,9,12,15,18:19,24,27) ], by= c("CodigoCauce"), all.x= TRUE)
#Mza18$EfPost     <- Mza18[,ifelse(EfcTierra >= !is.na(EfCondPromedio), #& is.na(EfCondPromedio),  EfcTierra, ifelse(!is.na(EfCondPromedio), EfCondPromedio, ifelse(!is.na(EfUnidadManejo), EfUnidadManejo, EfcTierra)))]
#Mza18$EfAnte     <- Mza18[,ifelse( !is.na(EfHijuelas),     EfHijuelas,ifelse(!is.na(EfCanales),EfCanales, EfcTierra))]
#Mza18$AhorroM3Seg <- Mza18[, metros * (TierraQ0medio) * (EfPost - EfAnte)] 
#Mza18$Ahorro      <- round(Mza18[, AhorroM3Seg * 3153600/ 1000], digits=3) # segundos al ano / conversión a '000

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
#ejemplo1 <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryBase)
ejemplo2 <- summary_table(Obras18,SummaryObras)

#Obras18 %>%

AltSum18 <- Dgi2018 %>%
  dplyr::select(.data$inversion, .data$InvMt, .data$hectareas) %>%
  qsummary(.)#,
           #numeric_summaries = list("Minimum" = "~ min(%s)",
            #                        "Maximum" = "~ max(%s)"),
           #n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))

## @knitr PrintSumBase18
print(ejemplo,
      rtitle = "Resumen de obras 2018",
      cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tunuyán Inf.","Tunuyán Sup.")
      )

## @knitr PrintAltSum18
AltSum18


# Obras 2019 ####
## @knitr DataObras19

Dgi2019 <- read.csv("DgiData/DgiObras2019.csv", sep = ";", stringsAsFactors=FALSE) #, header = TRUE, sep=",")

Dgi2019$metros <- as.numeric(as.character(Dgi2019$Metros))

Dgi2019$inversion <- as.numeric(as.character(Dgi2019$Inversion)) 
Dgi2019$InvMt     <- as.numeric(round(Dgi2019$inversion/Dgi2019$metros, digits = 1))
Dgi2019$InvUsd   <- as.numeric(round(Dgi2019$inversion/43.8, digits = 1)) # $Ars/Usd 43.8
Dgi2019$InvMtUsd <- as.numeric(round(Dgi2019$InvUsd/Dgi2019$metros, digits = 1))
view(Dgi2019)

Dgi2019 %>%
  filter((metros!="NA") & (inversion!="NA") & Status!="No se ejecuta") %>% #(Inversion!="" & Inversion!="-")) %>%
  select(Codigo,CodigoCauce, Subdelegacion, Cauce, Inspeccion,Obra,inversion, metros, Tipologia,InvMtUsd,Ano) %>% #, hectareas, Padrones
  mutate(coment= paste0(InvMtUsd," USD/metro revestido 2019")) %>%
  arrange(CodigoCauce)

Mza19 <- as.data.table(Dgi2019 %>%
               filter(Subdelegacion=="Mendoza" & Status!="No se ejecuta" &( metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA")) %>%
               select(Codigo,CodigoCauce, Cauce,inversion, metros, InvUsd,InvMtUsd,Ano) %>% # , CodigoCauceAlt
               arrange(CodigoCauce)) 

## @knitr Sum2019
print(qwraps2::summary_table(
  dplyr::group_by(Dgi2019, Subdelegacion),
  SummaryObras
),
rtitle = "Resumen de obras 2019",
cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tunuyán Inf.","Tunuyán Sup.")
)

# Mendoza ####

## @knitr MdzEf
Dgi2018 %>%
  filter(metros!= "Global" | metros!="-" | metros!="a determinar" | metros!="NA" & Subdelegacion=="Mendoza") %>%
  select(Codigo, hectareas, inversion, metros, InvMt, InvMtUsd, Nombre_Obra) %>% #, Padrones
  #mutate(coment= paste0(InvMtUsd," USD/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(InvMtUsd) 

## @knitr MendozaTable


Mendoza <- rbind(Mza17,Mza18,Mza19)

Mendoza <- merge(x= Mendoza,
               y= EfMendoza[ , c(2,6,9,12:13,15,18:19,24,26:27) ], 
               by= c("CodigoCauce"), all.x=TRUE)
arrange(Mendoza,Codigo)
#Mendoza <- Mendoza[-c(11,13),] # Canal Lunlunta duplicado


Mendoza$EfPost     <- Mendoza[,ifelse(EfcTierra >= !is.na(EfCondPromedio), #& is.na(EfCondPromedio), 
                                  EfcTierra,
                                  ifelse(!is.na(EfCondPromedio), EfCondPromedio, EfcTierra))]
Mendoza$EfAnte     <- Mendoza[,ifelse( !is.na(EfHijuelas),     EfHijuelas,     
                                   ifelse(!is.na(EfCanales),     EfCanales, EfcTierra))]

# 1er Cálculo ahorro Valores de EfConducción relevado con mayor proximidad

# [Caudal x ahorro (-pérdida)] / distancia cálculo x segundos (tiempo) x metros (distancia)
# [m3/seg x (mejora Ef.C.)]    / (km aforo)        x segundos x (metros/1000)

Mendoza$APunto        <- Mendoza[, (TierraQ0medio) * (EfPost - EfAnte) / Distancia] # KmTierra # total inspección 
Mendoza$APuntoAnual   <- round(Mendoza[, APunto * 2073600/ 1000], digits=1) # segundos al ano (3 turnos mensuales x 8 meses) / conversión a '000
Mendoza$AObra         <- Mendoza[, metros/1000 * APunto ] 
Mendoza$AObraAnual    <- round(Mendoza[, AObra    * 2764800], digits=3) # segundos al ano (4 turnos mensuales x 8 meses) / conversión a M3
Mendoza$UsdM3Obra    <- round(Mendoza[, InvUsd / AObraAnual], digits = 2)

# 2do Cálculo ahorro Valores de Ef.Conducción de Unidad de Manejo

# [Caudal x ahorro (-pérdida)] / distancia tierra UM x segundos (tiempo) x metros (distancia)
# [m3/seg x (Ef.C. rev - UM )] / (km tierra UM)      x segundos x (metros/1000)
Mendoza$AObraAnualUM    <- round(Mendoza[,  
                                (TierraQ0medio) * (EfPost - EfUnidadManejo) / Distancia 
                                 * (metros/1000)    * 2764800], digits=3) # segundos al ano (4 turnos mensuales x 8 meses) / conversión a M3
Mendoza$UsdM3ObraUM    <- round(Mendoza[, InvUsd / AObraAnualUM], digits = 3)


Mendoza <- Mendoza[order(Ano)]

write_csv(Mendoza, 'DgiData/Estimaciones/MzaAhorro.csv', na = "NA", append = FALSE, quote_escape = "double")

MzaTable <- Mendoza[, c(3,9,5,6:7,18,17,19,22,23)] %>% 
  mutate_all(linebreak) %>%
  kable(format = "latex",caption = "\\label{tab:MzaTable}Valores por obra de revestimiento ejecutada", align = c("l", rep("r", 6)),
        row.names = TRUE, booktabs = TRUE,  col.names = c("Cauce","Zona","Metros","Inv.\n(USD)","Inv.\n(USD/mt)","Ef.Cond\n(0)","Ef.Cond.\n(1)","A\n(m3/seg)", "A\n(Hm3/Obra)","USD/Hm3")
        #col.names = linebreak(c("Cauce","Zona","Metros","Inversión\\\\(USD)","Inversión \\\\(USD/mt)","Ef.Cond\n (previa)","Ef.Cond.\n(post)","Ahorro\n($$m^3/seg$$)", "Ahorro\n(Hm3/Obra)","USD/Hm3"), 
         #           align = "c", linebreaker = "\n", double_escape = F) #,"Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
  ) %>%
  kable_styling(latex_options = c("HOLD_position","scale_down"), position = "center", full_width = FALSE, font_size=10) %>% # latex_options = c("striped", "scale_down")
  pack_rows("2017", 1,4) %>%
  pack_rows("2018", 5,7) %>%
  pack_rows("2019", 8,13) %>%
  footnote( general = "Elab. propia en base a información suministrada por DGI.", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T) #, longtable=T
## @knitr MzaTable
MzaTable

## @knitr MzaTableLandscape
MzaTable %>% 
  landscape()



## @knitr MendozaPlots
OfertaMza <- Mendoza[order(UsdM3Obra)]
OfertaMza$AAcum   <- cumsum((OfertaMza$AObraAnual)) 
OfertaMza$AAcumUm <- cumsum((OfertaMza$AObraAnualUM)) 
#qplot(seq_along(OfertaMza$UsdM3Obra), OfertaMza$AObraAnual, geom="step")
#qplot(OfertaMza$AhAcum, OfertaMza$UsdHm3Obra,  geom="step")
#require(greppel)
AhorroMza <- ggplot(OfertaMza) + # , color=Zona
  geom_step(aes(y= UsdM3Obra, x=AAcum/1000),color = "#0073D9", size = 1) + 
  geom_text(aes(y= UsdM3Obra, x=AAcum/1000, label = paste(Cauce, sprintf('\u2191')), angle=0), # , sprintf('\u2191') # trying to add an arrow
            vjust=3, hjust=.2, size=2, check_overlap = F, inherit.aes = T) +
  #xlim(-100,1500) +
  scale_x_continuous(breaks= round(OfertaMza$AAcum/1000, digits = 0)) #+ #c(2,4,6,8,10,12,14) ) + # OfertaMza$AhAcum
 # scale_y_continuous(breaks = OfertaMza$UsdM3Obra)#c(seq(0,3,.500),4.5,6,7.5))   # 
  #directlabels::geom_dl(aes(x=seq_along(AhObraAnual), y= UsdHm3Obra ,label = Cauce), method = "smart.grid") +
#AhorroMza

InvMza <- ggplot(OfertaMza) +
  geom_step(aes(y=InvUsd, x=AAcum/1000),color = "#0073D9", size = 1) + #    , colour = Cauce)) +, color=factor(Zona)
  geom_text(aes(x=c(AAcum,AAcum), y= InvUsd ,label = (Cauce + sprintf('\u2192'))), # 
            vjust="top", hjust="right", size=1,check_overlap = T, inherit.aes = T) +
  #directlabels::geom_dl(aes(x=seq_along(AhObraAnual), y= UsdHm3Obra ,label = Cauce), method = "smart.grid") +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por M3 ahorrado") + theme_classic() #+
 # scale_y_continuous(breaks = seq(0,5000,500)) #c( 267.9,  338.4,  726.7,  827.9,  945.4, 1076.9, 1214.5, 1254.9, 1335.3, 1360.9, 1393.7, 1419.3, 1429.1)
#  scale_x_continuous(breaks = NULL) + #c(2,4,6,8,10,12,14) ) + 
ggsave('DgiData/Graphs/InvMza.png', height = 4, width = 6.9)

#head(OfertaMza,5)
#plot(OfertaMza$AhObraAnual,OfertaMza$InvUsd)

## @knitr AhorroMza
AhorroMza + theme(axis.text.x = element_text(size = 8, angle=75, vjust = .4), 
            axis.text.y = element_text(size = 8),
            panel.background = element_rect(fill = "white"), 
            axis.title = element_text(size = 8)) + 
            scale_y_continuous(breaks = OfertaMza$UsdM3Obra) + #c(seq(0,3,.500),4.5,6,7.5)) +
  theme(axis.line = element_line(colour = "grey50")) +
  xlab("Metros cúbicos anuales ('000)") + ylab("Dólares por m3 ahorrado")  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMza.png', height = 4, width = 9)

## @knitr AhorroMzaUM
AhorroMza + theme(axis.text.x = element_text(size = 8, angle=75, vjust = .4), 
                  axis.text.y = element_text(size = 8),
                  panel.background = element_rect(fill = "white"), 
                  axis.title = element_text(size = 8)) +
  xlim(100,1200) +
  theme(axis.line = element_line(colour = "grey50"))+
  xlab("Metros cúbicos anuales") + ylab(expression("Dólares por %m^{3}% ahorrado"))  # Hm^{3}
ggsave('DgiData/Graphs/OfertaMzaZoom.png', height = 4, width = 9)



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



# General Summary table ####

## @knitr MainTables

#Rev2015 <- aggregate(Dgi2015$metros, by= list(Dgi2015$Subdelegacion), FUN=sum, na.rm= TRUE)
#Rev2016 <- aggregate(Dgi2016$metros, by= list(Dgi2016$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2017 <- aggregate(Dgi2017$metros, by= list(Dgi2017$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2018 <- aggregate(Dgi2018$metros, by= list(Dgi2018$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2019 <- aggregate(Dgi2019$metros, by= list(Dgi2019$Subdelegacion), FUN=sum, na.rm= TRUE)

#library(scales)
FormatoNum <- number_format(big.mark = ".", decimal.mark = ",")

Revestimiento <- data.frame(
  #`2015`=            FormatoNum(Rev2015$x),
  #`2016`=            FormatoNum(Rev2016$x),
  `2017`=            FormatoNum(Rev2017$x),
  `2018`=            FormatoNum(Rev2018$x),
  `2019`=            FormatoNum(Rev2019$x),
  `Total`=           FormatoNum(Rev2017$x + Rev2018$x + Rev2019$x)#,#colSums(Revestimiento[,1:2])),
  #`Ef.Cond.`=        FormatoNum(round(c(80,80,80,round(mean(EfMendoza[["EfUnidadManejo"]],na.rm = T),digits=2),80,80),digits = 3)),
  #`Q_m3.año`=        FormatoNum(c(0,0,0,sum(CaudalMen),0,0)),
  #`Ahorro_Hm3.año`= FormatoNum(round((Rev2017$x + Rev2018$x + Rev2019$x)* mean(EfMendoza[["EfUnidadManejo"]], na.rm = T)* sum(CaudalMen)/1000000,digits=3))
)
#colnames(Revestimiento) <- c(#"Subdelegacion", "2015","2016","2017",   "2018","2019","Total","Ef.Cond.","Q_(m3/año)","Ahorro (Hm3/año)")
rownames(Revestimiento) <- c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior")


## @knitr Revestimiento

Revestimiento[1:6,] %>% 
  kable("latex",caption = "\\label{Revestimiento}Metros revestidos por cuenca", align = c("l", rep("r", 6)),
        row.names = TRUE, booktabs = TRUE, col.names = c("2017","2018","2019","Total") #,"Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
        ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elab. propia", general_title = "Fuente: ", title_format = "italic", #Datos de caudal del Río Mendoza extrapolados
            footnote_as_chunk=TRUE, escape=FALSE,threeparttable = T)

# Arranging Data              ####

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

# Sources ####

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




