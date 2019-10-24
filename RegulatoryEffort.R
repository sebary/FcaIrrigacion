
# A Simple Theory of Regulatory Effort with Spillover (Static Version)
# https://www.rpubs.com/tanglePillowHolder/315910


# Cumulative innovation & dynamics
# http://economics.mit.edu/files/12082

## @knitr SetUpPC2
RegulatoryEffort <- c("tidyverse","foreach","rootSolve","ggthemes") # included in tidyverse: "dplyr","tidyr","ggplot2"
library(rmsfuns)
load_pkg(RegulatoryEffort)

#library(tidyverse)      # thanks, Hadley
#library(foreach)        # more elegant loops
#library(rootSolve)      # utilities for finding zeroes
#library(ggthemes)       # prettier output options

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

# setting up the functions                                                ####
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


## @knitr DataObras18

Dgi2018 <- read.csv("DgiData/DgiObras2018.csv", sep = ";") #, header = TRUE, sep=",")
view(Dgi2018)

Dgi2018$metros <- as.numeric(Dgi2018$LONGITUD)

#replace(Dgi2018$MONTO, "Esperando cierre contable (90 dias)","0")
Dgi2018$Inversion <- as.numeric(Dgi2018$MONTO) #replace(Dgi2018$Inversion, "Esperando cierre contable (90 dias)","NA")

## @knitr Ver2018
Dgi2018 %>%
  filter(metros != "global" & metros!="-" & metros!="a determinar") %>%
  select(Codigo, Subdelegacion, hectareas, Padrones, Inversion, metros) %>%
  mutate(InvMt18 = Inversion/metros,  
         coment= paste0(InvMt18," $/metro revestido 2018")) %>%
  arrange(InvMt18)



## @knitr DataObras19

Dgi2019 <- read.csv("DgiData/DgiObras2019.csv", sep = ";") #, header = TRUE, sep=",")
view(Dgi2019)

Dgi2019$metros <- as.numeric(Dgi2019$Metros)

#replace(Dgi2018$MONTO, "Esperando cierre contable (90 dias)","0")
Dgi2019$Inversion <- as.numeric(Dgi2019$Inversion) #replace(Dgi2018$Inversion, "Esperando cierre contable (90 dias)","NA")

## @knitr Ver2019
Dgi2019 %>%
  filter((metros != "global" & metros!="-" & metros!="a determinar") | (Inversion!="" & Inversion!="-")) %>%
  select(Codigo, Subdelegacion, Inversion, metros) %>% #, hectareas, Padrones
  mutate(InvMt19 = Inversion/metros,  
         coment= paste0(InvMt19," $/metro revestido 2019")) %>%
  arrange(InvMt19)













