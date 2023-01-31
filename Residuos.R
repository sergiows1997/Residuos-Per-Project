## Librerias utilizadas
library(dplyr)
library(ggplot2)
library(forcats)

setwd("/Users/sergiowong/Desktop/R Programming/PortfolioProjects")
getwd()

## Crear directorio y descargar archivo
if (!file.exists("ResiduosProject")) {
  dir.create("/ResiduosProject")
}

getwd()

download.file("https://www.datosabiertos.gob.pe/sites/default/files/A.%20Generación%20Anual%20de%20residuos%20domiciliario_Distrital_2014_2021.csv","ResiduosProject/GeneracionResiduos.csv")


## Leer csv y crear data frame
residuos <- read.csv("GeneracionResiduos.csv", sep = ";")


## Filtrar variables que no se utilizaran
residuos <- residuos[,c(3:12)]


## Resumen general de la data

## Convertir tipo de datos a Numerico
residuos$QRESIDUOS_DOM <- gsub(",", ".",residuos$QRESIDUOS_DOM)
residuos$QRESIDUOS_DOM <- as.numeric(residuos$QRESIDUOS_DOM)



## Plotting población por region

totalpoblacionaño <- residuos %>% group_by(PERIODO) %>% 
  summarise(total = sum(POB_TOTAL, na.rm = TRUE))

pobregionaño <- residuos %>% group_by(PERIODO, REG_NAT) %>% 
  summarise(total = sum(POB_TOTAL,na.rm = TRUE))

ggplot(pobregionaño) + 
  geom_col(mapping = aes(x = PERIODO, y = total, fill = REG_NAT), position = "dodge")

ggplot(pobregionaño)+
  geom_line(mapping = aes(x = PERIODO, y = total)) +
  facet_wrap(~REG_NAT)

## OBS: INCREMENTO SIGNIFICATIVO DE LA POBLACION EN LA REGION COSTA, 
## SE OBSERVA LIGERO CRECIMIENTO EN LAS REGIONES SELVA Y SIERRA CON UN DEVACLE EN EL AÑO 2018



## Graficar Residuos por region

totalresiduosporaño <- residuos %>% group_by(PERIODO, REG_NAT) %>% 
  summarise(total = sum(QRESIDUOS_DOM, na.rm = TRUE))

resregionaño <- residuos %>% group_by(PERIODO, REG_NAT) %>% 
  summarise(total = sum(QRESIDUOS_DOM,na.rm = TRUE))

ggplot(resregionaño) +
  geom_col(mapping = aes(x = PERIODO, y = total, fill = REG_NAT), position ="dodge")

ggplot(resregionaño)+
  geom_line(mapping = aes(x = PERIODO, y = total)) +
  facet_wrap(~REG_NAT)


## OBS: AUMENTO DE RESIDUOS EN LAS TRES REGIONES (COSTA, SIERRA Y SELVA), CON UN INCREMENTO SIGNIFICATIVO EN LA REGION COSTA,
## REGIONES SELVA Y SIERRA CUENTAN CON UN LEVE INCREMENTO

             



## Subset data por año
## 2014
r2014<- residuos %>% filter(PERIODO == 2014)


## Crear variables de residuo total, poblacion total, porcentaje del total de residuos, porcentaje del total de la poblacion,
## ranking de departamentos por total de residuos y poblacion total y ratio del porcentaje total de residuos generados sobre el porcentaje total de la poblacion)
dep2014 <-  r2014 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)


## Añadir variable del año
dep2014$Año <- 2014

## Añadir vairbale de diferencia poblacional con respecto al año anterior
dep2014$dif_pob <- 0


## Gráfico de barras de residuos generados por DEPARTAMENTO
ggplot(dep2014, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

## Gráfico de barras de la población total por DEPARTAMENTO
ggplot(dep2014, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, pob_total))) +
  geom_col()


## Crear tabla resumen de cada distrito
dist2014 <- r2014 %>% group_by(DEPARTAMENTO, DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))


## Grafico de residuales POBLACION VS RESIDUOS GENERADOS
plot(dist2014$pob_total,dist2014$total)
abline(lm(dist2014$total~dist2014$pob_total), col = 'red')
summary(lm(dist2014$total~dist2014$pob_total))




## 2015
r2015<- residuos %>% filter(PERIODO == 2015)

dep2015 <-  r2015 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>%
  arrange(DEPARTAMENTO)

dep2015$dif_pob <- dep2015$pob_total - dep2014$pob_total

dep2015$Año <- 2015

ggplot(dep2015, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2015, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2015 <- r2015 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2015$pob_total,dist2015$total)
abline(lm(dist2015$total~dist2015$pob_total), col = 'red')
summary(lm(dist2015$total~dist2015$pob_total))



## 2016
r2016<- residuos %>% filter(PERIODO == 2016)

dep2016 <-  r2016 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2016$dif_pob <- dep2016$pob_total - dep2015$pob_total

dep2016$Año <- 2016

ggplot(dep2016, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2016, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2016 <- r2016 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2016$pob_total,dist2016$total)
abline(lm(dist2016$total~dist2016$pob_total), col = 'red')
summary(lm(dist2016$total~dist2016$pob_total))



## 2017
r2017<- residuos %>% filter(PERIODO == 2017)

dep2017 <-  r2017 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2017$dif_pob <- dep2017$pob_total - dep2016$pob_total

dep2017$Año <- 2017

ggplot(dep2017, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2017, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2017 <- r2017 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2017$pob_total,dist2017$total)
abline(lm(dist2017$total~dist2017$pob_total), col = 'red')
summary(lm(dist2017$total~dist2017$pob_total))



## 2018
r2018 <- residuos %>% filter(PERIODO == 2018)


dep2018 <-  r2018 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2018$dif_pob <- dep2018$pob_total - dep2017$pob_total

dep2018$Año <- 2018

ggplot(dep2018, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2018, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2018 <- r2018 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2018$pob_total,dist2018$total)
abline(lm(dist2018$total~dist2018$pob_total), col = 'red')
summary(lm(dist2018$total~dist2018$pob_total))


## 2019
r2019 <- residuos %>% filter(PERIODO == 2019)

dep2019 <-  r2019 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2019$dif_pob <- dep2019$pob_total - dep2018$pob_total

dep2019$Año <- 2019

ggplot(dep2019, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2019, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2019 <- r2019 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2019$pob_total,dist2019$total)
abline(lm(dist2019$total~dist2019$pob_total), col = 'red')
summary(lm(dist2019$total~dist2019$pob_total))


## 2020
r2020 <- residuos %>% filter(PERIODO == 2020)

dep2020 <-  r2020 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2020$dif_pob <- dep2020$pob_total - dep2019$pob_total

dep2020$Año <- 2020

ggplot(dep2020, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2020, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2020 <- r2020 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2020$pob_total,dist2020$total)
abline(lm(dist2020$total~dist2020$pob_total), col = 'red')
summary(lm(dist2020$total~dist2020$pob_total))


## 2021
r2021 <- residuos %>% filter(PERIODO == 2021)

dep2021 <-  r2021 %>% group_by(DEPARTAMENTO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  mutate(por_total = round(total/sum(total) *100,2), por_pob_total = round(pob_total/sum(pob_total)*100,2)) %>%
  mutate(rank_pob_total = rank(desc(pob_total)), rank_total = rank(desc(total)), ratio = round(por_total/ por_pob_total,2)) %>% 
  arrange(DEPARTAMENTO)

dep2021$dif_pob <- dep2021$pob_total - dep2020$pob_total


dep2021$Año <- 2021

ggplot(dep2021, aes(x = total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()

ggplot(dep2021, aes(x = pob_total, y = fct_reorder(DEPARTAMENTO, total))) +
  geom_col()


dist2021 <- r2021 %>% group_by(DISTRITO) %>% 
  summarise(total = sum(QRESIDUOS_DOM), pob_total = sum(POB_TOTAL)) %>%
  arrange(desc(total))

plot(dist2021$pob_total,dist2021$total)
abline(lm(dist2021$total~dist2021$pob_total), col = 'red')
summary(lm(dist2021$total~dist2021$pob_total))



## JUNTAR FILAS DE LOS DEPARTAMENTOS DE TODOS LOS AÑOS
departamentos <- rbind(dep2014,dep2015,dep2016,dep2017,dep2018,dep2019,dep2020,dep2021)

departamentos$rank_pob_total <- as.factor(departamentos$rank_pob_total)
departamentos$rank_total <- as.factor(departamentos$rank_total)

ggplot(departamentos)+
  geom_line(mapping = aes(x = Año, y = dif_pob, color = DEPARTAMENTO )) +
  facet_wrap(~DEPARTAMENTO)

ggplot(departamentos)+
  geom_line(mapping = aes(x = Año, y = ratio, color = DEPARTAMENTO )) +
  facet_wrap(~DEPARTAMENTO)

names(departamentos) <- c("DEPARTAMENTO","RESIDUOSTOTAL","POBTOTAL","PORCENTAJERESIDUOSTOTAL","PORCENTAJEPOBLACION","RANKINGPOBLACIONTOT","RANKINGRESIDUOS","RATIO","PERIODO","DIFERENCIAPOB")

departamentos <- departamentos %>% mutate(ratio = round(PORCENTAJERESIDUOSTOTAL/PORCENTAJEPOBLACION,2))




## Analisis de Lima

lima <- residuos %>% filter(DEPARTAMENTO == "LIMA", PROVINCIA == "LIMA")
lima <- lima[,5:10]
lima <- lima[,c(1:2,5:6)]


## 2014
lima2014 <- lima %>% filter(PERIODO == 2014) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2)) %>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2014$dif_pob <- 0

ggplot(lima2014) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2014) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2015
lima2015 <- lima %>% filter(PERIODO == 2015) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2)) %>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2015$dif_pob <- lima2015$POB_TOTAL - lima2014$POB_TOTAL

ggplot(lima2015) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2015) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2016
lima2016 <- lima %>% filter(PERIODO == 2016) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2016$dif_pob <- lima2016$POB_TOTAL - lima2015$POB_TOTAL

ggplot(lima2016) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2016) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2017
lima2017 <- lima %>% filter(PERIODO == 2017) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2017$dif_pob <- lima2017$POB_TOTAL - lima2016$POB_TOTAL

ggplot(lima2017) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2017) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2018
lima2018 <- lima %>% filter(PERIODO == 2018) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2018$dif_pob <- lima2018$POB_TOTAL - lima2017$POB_TOTAL

ggplot(lima2018) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2018) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2019
lima2019 <- lima %>% filter(PERIODO == 2019) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2019$dif_pob <- lima2019$POB_TOTAL - lima2018$POB_TOTAL

ggplot(lima2019) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2019) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## 2020
lima2020 <- lima %>% filter(PERIODO == 2020) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2020$dif_pob <- lima2020$POB_TOTAL - lima2019$POB_TOTAL

ggplot(lima2020) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2020) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))

## 2021
lima2021 <- lima %>% filter(PERIODO == 2021) %>% 
  mutate(por_pob_total = round(POB_TOTAL/sum(POB_TOTAL)*100,2), por_total = round(QRESIDUOS_DOM/sum(QRESIDUOS_DOM)*100,2)) %>% 
  mutate(ratio = round(por_total/por_pob_total,2))%>% 
  mutate(rank_pob_total = rank(desc(POB_TOTAL)), rank_total = rank(desc(QRESIDUOS_DOM))) %>% 
  arrange(DISTRITO)

lima2021$dif_pob <- lima2021$POB_TOTAL - lima2020$POB_TOTAL

ggplot(lima2021) + 
  geom_col(mapping = aes(x = QRESIDUOS_DOM, y = fct_reorder(DISTRITO, QRESIDUOS_DOM)))

ggplot(lima2021) +
  geom_col(mapping = aes(x = POB_TOTAL, y= fct_reorder(DISTRITO, POB_TOTAL)))


## Juntar filas de todos los años
lima <- rbind(lima2014,lima2015, lima2016, lima2017, lima2018, lima2019, lima2020, lima2021)


ggplot(lima) + 
  geom_line(mapping = aes(x = PERIODO, y = ratio)) +
  facet_wrap(~DISTRITO)


ggplot(lima) +
  geom_line(mapping = aes(x = PERIODO, y = QRESIDUOS_DOM)) + 
  facet_wrap(~DISTRITO)


names(lima) <- c("DISTRITO","POBTOTAL","RESIDUOSTOTAL","PERIODO","PORCENTAJEPOBLACION","PORCENTAJERESIDUOSTOTAL","RATIO","RANKINGPOBLACIONTOT","RANKINGRESIDUOS","DIFERENCIAPOB")




write.csv(lima, file = "ResiduosProject/limaresiduos.csv")
write.csv(departamentos, file = "ResiduosProject/departamentosresiduos.csv")



library(dplyr)


distritos <- lima %>% group_by(PERIODO, DISTRITO) %>% 
  summarise(avgratio = mean(RATIO)) %>% 
  mutate(rank_distritos = round(rank((avgratio),0)))

write.csv(distritos, file = "ResiduosProject/distritos.csv")










