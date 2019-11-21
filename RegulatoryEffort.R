

## @knitr SetUp2
RegulatoryEffort <- c("foreach","rootSolve","ggthemes","kableExtra","qwraps2","tidyverse","data.table","scales") # included in tidyverse: "dplyr","tidyr","ggplot2","pandoc","table1", "summarytools",
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

EfMendoza <- read.csv("DgiData/EfCond.csv", sep = ";") #, header = TRUE, sep=",")
indxx <- c("TierraQ0","TierraQ0rep","TierraQ0medio","TierraQl","TierraQlrep","TierraQlmedio","Distancia","KmInvert","EfcTierra","LongMedia","EfTierraLong","EfCanales","EfHijuelas","KmCanales")
EfMendoza[indxx] <- lapply(EfMendoza[indxx], function(x) as.numeric(as.character(x)))
view(EfMendoza)
}

Caudal19 <- c(590,490,140,850,460,460)
names(Caudal19) <- c("Atuel","Diamante","Malargüe","Mendoza","Tun.Inferior","Tun.Superior")
  
# Working with databases        ####

# Obras 2018 ####

## @knitr DataObras18

Dgi2018 <- read.csv("DgiData/DgiObras2018.csv", sep = ";") #, header = TRUE, sep=",")
# Convert many columns in numeric #https://stackoverflow.com/questions/27528907/how-to-convert-data-frame-column-from-factor-to-numeric

indxx <- c("MONTO","MONTO.CONTRATO","hectareas","Padrones","LONGITUD")
Dgi2018[indxx] <- lapply(Dgi2018[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2018$MONTO)

#Dgi2018$MONTO <- as.numeric(as.character(Dgi2018$MONTO))

Dgi2018$inversion <- ifelse(is.na(Dgi2018$MONTO),
                            ifelse(is.na(Dgi2018$MONTO.CONTRATO),
                              ifelse(is.na(Dgi2018$Presupuesto), " ", Dgi2018$Presupuesto),
                              Dgi2018$MONTO.CONTRATO),Dgi2018$MONTO)
# Rename
#Dgi2018       <- Dgi2018 %>% rename(metros = LONGITUD) #, Inversion = MONTO)
names(Dgi2018)[9]<-"metros"
Dgi2018$metros

Dgi2018$InvMt    <- as.numeric(round(Dgi2018$inversion/Dgi2018$metros, digits = 0))
Dgi2018$InvUsd   <- as.numeric(round(Dgi2018$inversion/27.425, digits = 1)) # $Ars/Usd 27.425
Dgi2018$InvMtUsd <- as.numeric(round(Dgi2018$InvUsd/Dgi2018$metros, digits = 1))


#Dgi2018 %>%
 # filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA") %>%
#  select(Codigo, Subdelegacion, hectareas, Padrones, inversion, metros, InvMt) %>%
 # mutate(InvUsd = round(inversion/27.425, digits = 1), # $Ars/Usd 27.425
  #       InvMtUsd = round(InvMt/27.425, digits = 1),
   #      coment= paste0(InvMtUsd," U$S/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
#  arrange(InvMt)

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
        "promedio (d.s.)" = ~qwraps2::gmean(metros, na_rm = TRUE),
        #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(metros, na.rm = TRUE),
        "max" = ~max(metros, na.rm = TRUE) 
      ),
    "Inversion/mt" =
      list(
        "promedio (d.s.)" = ~qwraps2::gmean(InvMtUsd, na_rm = TRUE),
      #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE),
        "min" = ~min(InvMtUsd, na.rm = TRUE),
        "max" = ~max(InvMtUsd, na.rm = TRUE) 
      )
  )


## @knitr Ver2018
Dgi2018 %>%
  filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA") %>%
  select(Codigo, Subdelegacion, hectareas, Padrones, inversion, metros, InvMt) %>%
  mutate(coment= paste0(InvMtUsd," U$S/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(InvMt) 

## @knitr Sum2018
print(qwraps2::summary_table(
  dplyr::group_by(Obras18, Subdelegacion),
  SummaryObras
),
rtitle = "Resumen de obras 2018",
cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tunuyán Inf.","Tunuyán Sup.")
)


## @knitr OtherSum2018
library(Amisc)
pander::pandoc.table(Amisc::describeBy(
  data = Obras18,
  var.names = c("inversion", "Padrones", "hectareas","InvMt"),
  by1 = "Subdelegacion",
  #dispersion = "sd", Missing = TRUE,
  stats = "non-parametric"
),
split.tables = Inf
)


## @knitr SummaryBase
# List of lists to replicate the analysis
SummaryBase <-
  list("Inversión" =
         list("media" = ~ qwraps2::mean(inversion)),
              #"min" = ~ min(inversion),
              "max" = ~ max(inversion), "Padrones" = list("media (d.s.)" = ~ qwraps2::mean(Padrones)), 
              #"min" = ~ min(Padrones), "max" = ~ max(Padrones),  "Superficie" = list("media (d.s)" = ~ qwraps2::mean(hectareas),
              #"min" = ~ min(hectareas),  "max" = ~ max(hectareas)),
       "Metros revestidos" =
         list(
           "promedio (d.s.)" = ~qwraps2::gmean(metros, na_rm = TRUE),
           #  "mediana (Q1, Q3)" = ~qwraps2::median_iqr(InvMt, na_rm = TRUE), "min" = ~min(metros, na.rm = TRUE),
           "max" = ~max(metros, na.rm = TRUE) 
         ),
       "U$S/metro" =
         list("media (d.s)" = ~ qwraps2::mean(InvMtUsd),
              #"min" = ~ min(InvMt), 
              "max" = ~ max(InvMtUsd))
  )



Obras18 %>%
  dplyr::select(.data$inversion, .data$Padrones, .data$hectareas, .data$InvMt) %>%
  qsummary(.)

ejemplo  <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryObras)
#ejemplo1 <- summary_table(dplyr::group_by(Obras18, Subdelegacion), SummaryBase)
ejemplo2 <- summary_table(Obras18,SummaryObras)


#Obras18 %>%
 # dplyr::select(.data$inversion, .data$InvMt) %>%
 #summary_table(.) # qsummary(.) #

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

## @knitr Ver2019
Dgi2019 %>%
  filter((metros!="NA") & (inversion!="NA")) %>% #(Inversion!="" & Inversion!="-")) %>%
  select(Codigo, Subdelegacion, Cauce, Inspeccion,Obra,inversion, metros, Tipologia,InvMtUsd) %>% #, hectareas, Padrones
  mutate(coment= paste0(InvMtUsd," U$S/metro revestido 2019")) %>%
  arrange(InvMtUsd)

## @knitr Sum2019
print(qwraps2::summary_table(
  dplyr::group_by(Dgi2019, Subdelegacion),
  SummaryObras
),
rtitle = "Resumen de obras 2019",
cnames = c("Atuel", "Diamante", "Malargüe","Mendoza","Tunuyán Inf.","Tunuyán Sup.")
)

# Obras 2017 ####

## @knitr DataObras17

Dgi2017 <- read.csv("DgiData/DgiObras2017.csv", sep = ";") #, header = TRUE, sep=",")
head(Dgi2017)
indxx <- c("Metros","MetrosAvance","CodigoCauce","CodigoCauceAlt","Codigo","Obra","inversion")
Dgi2017[indxx] <- lapply(Dgi2017[indxx], function(x) as.numeric(as.character(x)))

class(Dgi2017$inversion)

Dgi2017$metros <- ifelse(Dgi2017$MetrosAvance > Dgi2017$Metros, Dgi2017$MetrosAvance, Dgi2017$Metros)

Dgi2017$InvMt    <- as.numeric(round(Dgi2017$inversion/Dgi2017$metros, digits = 0))
Dgi2017$InvUsd   <- as.numeric(round(Dgi2017$inversion/17, digits = 1)) # $Ars/Usd 17
Dgi2017$InvMtUsd <- as.numeric(round(Dgi2017$InvUsd/Dgi2017$metros, digits = 1))

options(qwraps2_markup = "latex")
Obras17 <- as.data.frame(Dgi2017)

## @knitr Ver2017
Dgi2017 %>%
  filter(metros!= "global" | metros!="-" | metros!="a determinar" | metros!="NA" & Status!="No se ejecuta") %>%
  select(CodigoCauce, Subdelegacion, inversion, metros, InvMt, InvMtUsd) %>% #, hectareas, Padrones
  mutate(coment= paste0(InvMtUsd," U$S/metro revestido 2017")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(InvMt) 

# from qwraps to kable
#qable(x, rtitle, rgroup, rnames = rownames(x), cnames = colnames(x),
 #     markup = getOption("qwraps2_markup", "latex")#, other table components
  #    )



# Mendoza ####

## @knitr MdzEf
Dgi2018 %>%
  filter(metros!= "Global" | metros!="-" | metros!="a determinar" | metros!="NA" & Subdelegacion=="Mendoza") %>%
  select(Codigo, hectareas, inversion, metros, InvMt, InvMtUsd, Nombre_Obra) %>% #, Padrones
  #mutate(coment= paste0(InvMtUsd," U$S/metro revestido 2018")) %>% #InvMt18 = round(inversion/metros, digits = 2),  
  arrange(InvMtUsd) 



# General Summary table ####

## @knitr MainTables

#Rev2015 <- aggregate(Dgi2015$metros, by= list(Dgi2015$Subdelegacion), FUN=sum, na.rm= TRUE)
#Rev2016 <- aggregate(Dgi2016$metros, by= list(Dgi2016$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2017 <- aggregate(Dgi2017$metros, by= list(Dgi2017$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2018 <- aggregate(Dgi2018$metros, by= list(Dgi2018$Subdelegacion), FUN=sum, na.rm= TRUE)
Rev2019 <- aggregate(Dgi2019$metros, by= list(Dgi2019$Subdelegacion), FUN=sum, na.rm= TRUE)

#library(scales)
FormatoNum <- number_format(big.mark = ",", decimal.mark = ".")

Revestimiento <- data.frame(
  #`2015`=            FormatoNum(Rev2015$x),
  #`2016`=            FormatoNum(Rev2016$x),
  `2017`=            FormatoNum(Rev2017$x),
  `2018`=            FormatoNum(Rev2018$x),
  `2019`=            FormatoNum(Rev2019$x),
  `Total`=           FormatoNum(Rev2017$x + Rev2018$x + Rev2019$x),#colSums(Revestimiento[,1:2])),
  `Ef.Cond.`=        FormatoNum(round(c(80,80,80,mean(EfMendoza[["EfCond"]]),80,80),digits = 1)),
  `Q_m3.año`=        FormatoNum(c(0,0,0,sum(CaudalMen),0,0)),
  `Ahorro_Hm3.año`= FormatoNum(round((Rev2017$x + Rev2018$x + Rev2019$x)* mean(EfMendoza[["EfCond"]])* sum(CaudalMen)/1000000,digits=0))
)
#colnames(Revestimiento) <- c(#"Subdelegacion", "2015","2016","2017",   "2018","2019","Total","Ef.Cond.","Q_(m3/año)","Ahorro (Hm3/año)")
rownames(Revestimiento) <- c("Atuel", "Diamante", "Malargüe","Mendoza","Tun. Inferior","Tun. Superior")
Revestimiento

## @knitr Revestimiento

Revestimiento[1:6,] %>% 
  kable("latex",caption = "\\label{Revestimiento}Ahorro estimado y metros revestidos por cuenca", align = c("l", rep("r", 6)),
        row.names = TRUE, booktabs = TRUE, col.names = c("2017","2018","2019","Total","Ef.Cond.","Q (m3/año)","A (Hm3/año)") 
        ) %>%
  kable_styling(latex_options = c("HOLD_position"), position = "center", full_width = FALSE, font_size=10) %>%
  footnote( general = "Elab. propia. Datos de caudal del Río Mendoza extrapolados", general_title = "Fuente: ", title_format = "italic",
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




