library(tidyverse)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(outliers)
library(car)
library(dotwhisker)

setwd("O:\\16. Equipe\\Weverton\\9 - ESCOLAR e IDEB\\Bases\\Bases IDEB\\ideb passado")

banco_final_passado <- read_excel("serie historica anos finais - espirito santo - v1.xlsx")
banco_inicial_passado <- read_excel("serie historica anos iniciais - espirito santo - v1.xlsx")
banco_medio_passado <- read_excel("serie historica ensino medio - espirito santo - v1.xlsx")

setwd("O:\\16. Equipe\\Weverton\\9 - ESCOLAR e IDEB\\Bases\\Bases IDEB\\ideb atual\\Escolas")

banco_final_atual <- read_excel("ideb atual anos finais v1.xlsx")
banco_inicial_atual <- read_excel("ideb atual anos inicais v1.xlsx")
banco_medio_atual <- as.data.frame(read_excel("ideb atual ensino medio v1.xlsx"), stringAsFactors = false)

(names(banco_medio_atual)[17] <- "IDEB")

setwd("O:\\16. Equipe\\Weverton\\9 - ESCOLAR e IDEB\\Bases\\Bases EscoLAR")

banco_SEDUdigital <- read_excel("DASHBOARD EDUCACIONAL ESPIRITO SANTO.xlsx")

banco_APNPS <- read_excel("ESCOLAR_BANCO_APNPS.xlsx")

## unindo base de dados

base_final_teste <- left_join(banco_medio_atual, banco_SEDUdigital, by = c("C?digo da Escola" = "cd_escola"))


## tratando como n?merica 
base_final_teste$IDEB <- as.numeric(as.character(base_final_teste$IDEB))
base_final_teste$`Indicador Engajamento Aluno` <- as.numeric(as.character(base_final_teste$`Indicador Engajamento Aluno`))


## teste t
#base_final_teste %>%
 # filter(!is.na(Classifica??o),
  #       !is.na(IDEB)) %>%
  #group_by(Classifica??o) %>%
  # summarise(mediana = median(IDEB),
            #media = mean(IDEB),
            #desvio = sd(IDEB),
            #n = n())

# t.test(IDEB ~ Classifica??o, data = base_final_teste)

# p-value < 0,05 
# 0 não está no intervalo de confiança, haverá diferença com significância estatística

## teste de correla??o

cor.test(base_final_teste$ideb, base_final_teste$`Indicador Ado??o Aluno`)

cor.test(base_final_teste$ideb, base_final_teste$`Indicador Engajamento Aluno`)

#base_fim <- base_final_teste %>%
  # filter(!is.na(APNPS_FORMA_INTERDISCIPLINAR),
         #!is.na(TV))

## teste de normalidade. O teorema do limite central nos diz que n?o importa qual distribui??o as coisas tenham, 
# a distribui??o amostral tende a ser normal se a amostra for grande o suficiente (n > 30).

# Como podemos ver, o teste nos indica que o vetor A n?o possui distribui??o normal (p < 0.05). 
# No entanto, n?o conseguimos rejeitar a hip?tese nula para o vetor B (p > 0.05), 
# ou seja, este conjunto de dados foi extra?do de uma popula??o com distribui??o normal.

## teste de normalidade

shapiro.test(base_final_teste$`Indicador Engajamento Aluno`[0:5000])

shapiro.test(base_final_teste$`Indicador Ado??o Aluno`[0:5000])

shapiro.test(base_final_teste$IDEB[0:5000])

hist(base_final_teste$IDEB)
hist(base_final_teste$`Indicador Engajamento Aluno`)
hist(base_final_teste$`Indicador Ado??o Aluno`)

# representa??es gr?ficas bivariadas

ggplot(base_final_teste, aes('Indicador Engajamento Aluno', IDEB)) +
  geom_jitter()+
  theme_minimal()


ggplot(base_final_teste, aes('Indicador Ado??o Aluno', IDEB)) +
  geom_jitter()+
  theme_minimal()

ggplot(base_final_teste, aes('Indicador Ado??o Aluno', 'Indicador Engajamento Aluno', color = IDEB)) +
  geom_jitter(size = 4)+
  theme_minimal() 

#ggplot(base_final_teste, aes(IDEB, 'Indicador Engajamento Aluno')) +
  #geom_point() +
  #geom_smooth(method = "lm")

## regress?es

regressao <- lm(base_final_teste$IDEB ~ base_final_teste$`Indicador Ado??o Aluno`, data = base_final_teste)

summary(regressao)

confint(regressao, level = 0.99)

regressao1 <- lm(base_final_teste$IDEB ~ base_final_teste$`Indicador Engajamento Aluno`, data = base_final_teste)

summary(regressao1)

confint(regressao1, level = 0.99)

regressao2 <- lm(base_final_teste$IDEB ~ base_final_teste$`Indicador Engajamento Aluno` + base_final_teste$`Indicador Ado??o Aluno`, data = base_final_teste)
summary(regressao2)

confint(regressao2)

## verificando pressupostos da regress?o

plot(regressao, 1) #Idealmente, esse gr?fico n?o apresentar? nenhum padr?o. Ou seja, a linha vermelha dever? ser 
# horizontal pr?xima da linha pontilhada que cruza o eixo y no valor 0.

plot(regressao1, 1)

plot(regressao2, 1)

plot(regressao, 3)

plot(regressao1, 3)

plot(regressao2, 3)

acf(regressao$residuals) #Neste gr?fico, o resultado ideal ? que as linhas verticais estejam dentro do intervalo delimitado pelas linhas azuis tracejadas horizontais. 
# A exce??o ? a primeira linha vertical, que sempre ser? alta e estar? fora desse intervalo.

acf(regressao1$residuals)
acf(regressao2$residuals)

plot(regressao, 2)
plot(regressao1, 2)
plot(regressao2, 2)


plot(regressao, 4)
plot(regressao1, 4)
plot(regressao2, 4)

plot(regressao, 5)
plot(regressao1, 5)
plot(regressao2, 5)

base_outliers <- base_final_teste[-c(72, 112, 124, 150, 195), ]

regressao3 <- lm(base_outliers$IDEB ~ base_outliers$`Indicador Engajamento Aluno` + base_outliers$`Indicador Ado??o Aluno`, data = base_outliers)

summary(regressao3)

confint(regressao3)

dwplot(regressao3)

plot(regressao3, 1)

plot(regressao3, 2)

plot(regressao3, 3)

plot(regressao3, 4)

plot(regressao3, 5)

acf(regressao3$residuals)

vif(regressao3)

#Quanto maior o valor, maior a multicolinearidade. N?o existe um valor objetivo para definir o qu?o grave ? a multicolinearidade. 
# Alguns afirmam que ? preocupante valores acima de 10, outros que valores acima de 4 j? ? o suficiente para termos problemas.

outlierTest(regressao3)

base_final_outliers <- left_join(banco_APNPS, base_outliers, by = c("COD_ESCOLA" = "C?digo da Escola"))

base_outliers_filtrada <- base_final_outliers %>%
  filter('IDEB Divulgado' != "NA")

glimpse(base_outliers_filtrada)

ggplot(base_final_outliers, aes(base_final_outliers$'Indicador Engajamento Aluno', fill = base_final_outliers$'IDEB Divulgado')) +
  geom_density(alpha = 0.3)

ggplot(base_final_outliers, aes(base_final_outliers$'Indicador Ado??o Aluno', fill = base_final_outliers$'IDEB Divulgado')) +
  geom_density(alpha = 0.3)

ggplot(base_final_outliers, aes(base_final_outliers$'GOOGLE_CLASSROM', fill = base_final_outliers$'IDEB Divulgado')) +
  geom_density(alpha = 0.3)

base_outliers_filtrada %>%
  filter(!is.na(`IDEB Divulgado`)) %>%
ggplot(aes(IMPRESSAS, fill = `IDEB Divulgado`)) +
  geom_density(position = "stack") +
  scale_x_continuous(name = "APNPS Impressas", limits = c(0, 500)) +
  scale_y_continuous(name = "Contagem", limits = c(0, 0.05))

base_outliers_filtrada %>%
  filter(!is.na(`IDEB Divulgado`)) %>% 
  ggplot(aes(x = `IDEB Divulgado`, 
             fill = `IDEB Divulgado`)) +
geom_bar()


regressao4 <- lm(base_outliers_filtrada$IDEB ~ base_outliers_filtrada$`Indicador Engajamento Aluno` + base_outliers_filtrada$`Indicador Ado??o Aluno` + 
                 base_outliers_filtrada$`YOUTUBE` + base_outliers_filtrada$`TV`+ base_outliers_filtrada$`IMPRESSAS`,
                 data = base_outliers_filtrada)

summary(regressao4)

confint(regressao4)

dwplot(regressao4)

plot(regressao4, 1)

plot(regressao4, 2)

plot(regressao4, 3)

plot(regressao4, 4)

plot(regressao4, 5)

acf(regressao4$residuals)

vif(regressao4)

outlierTest(regressao4)

