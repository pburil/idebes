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

base_final_teste <- left_join(banco_medio_atual, banco_SEDUdigital, by = c("Código da Escola" = "cd_escola"))

base_final <- left_join(base_final_teste, banco_APNPS, by = c("Código da Escola" = "COD_ESCOLA"))


## tratando como númerica 
base_final$IDEB <- as.numeric(as.character(base_final$IDEB))
base_final$`Indicador Engajamento Aluno` <- as.numeric(as.character(base_final$`Indicador Engajamento Aluno`))


base_final_teste$IDEB <- as.numeric(as.character(base_final$IDEB))
base_final_teste$`Indicador Engajamento Aluno` <- as.numeric(as.character(base_final$`Indicador Engajamento Aluno`))
## teste t
#base_final_teste %>%
 # filter(!is.na(Classificação),
  #       !is.na(IDEB)) %>%
  #group_by(Classificação) %>%
  # summarise(mediana = median(IDEB),
            #media = mean(IDEB),
            #desvio = sd(IDEB),
            #n = n())

# t.test(IDEB ~ Classificação, data = base_final_teste)

# p-value < 0,05 
# 0 nÃ£o estÃ¡ no intervalo de confianÃ§a, haverÃ¡ diferenÃ§a com significÃ¢ncia estatÃ­stica

## teste de correlação

cor.test(base_final$IDEB, base_final$`Indicador Adoção Aluno`)

cor.test(base_final$IDEB, base_final$`Indicador Engajamento Aluno`)

#base_fim <- base_final_teste %>%
  # filter(!is.na(APNPS_FORMA_INTERDISCIPLINAR),
         #!is.na(TV))

## teste de normalidade. O teorema do limite central nos diz que não importa qual distribuição as coisas tenham, 
# a distribuição amostral tende a ser normal se a amostra for grande o suficiente (n > 30).

# Como podemos ver, o teste nos indica que o vetor A não possui distribuição normal (p < 0.05). 
# No entanto, não conseguimos rejeitar a hipótese nula para o vetor B (p > 0.05), 
# ou seja, este conjunto de dados foi extraído de uma população com distribuição normal.

## teste de normalidade

shapiro.test(base_final$`Indicador Engajamento Aluno`[0:5000])

shapiro.test(base_final$`Indicador Adoção Aluno`[0:5000])

shapiro.test(base_final$IDEB[0:5000])

hist(base_final$IDEB)
hist(base_final$`Indicador Engajamento Aluno`)
hist(base_final$`Indicador Adoção Aluno`)

# representações gráficas bivariadas

ggplot(base_final, aes('Indicador Engajamento Aluno', IDEB)) +
  geom_jitter()+
  theme_minimal()


ggplot(base_final, aes('Indicador Adoção Aluno', IDEB)) +
  geom_jitter()+
  theme_minimal()

ggplot(base_final, aes('Indicador Adoção Aluno', 'Indicador Engajamento Aluno', color = IDEB)) +
  geom_jitter(size = 4)+
  theme_minimal() 

#ggplot(base_final, aes(IDEB, 'Indicador Engajamento Aluno')) +
  #geom_point() +
  #geom_smooth(method = "lm")

## regressões

regressao <- lm(base_final$IDEB ~ base_final$`Indicador Adoção Aluno`, data = base_final)

summary(regressao)

confint(regressao, level = 0.99)

dwplot(regressao)

regressao1 <- lm(base_final$IDEB ~ base_final$`Indicador Engajamento Aluno`, data = base_final)

summary(regressao1)

confint(regressao1, level = 0.99)

dwplot(regressao1)


regressao2 <- lm(IDEB ~ `Indicador Engajamento Aluno` + `Indicador Adoção Aluno` + 
                   YOUTUBE + TV+ IMPRESSAS,
                 data = base_final)

summary(regressao2)

confint(regressao2)

dwplot(regressao2, by2_sd = TRUE)

dwplot(regressao2,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

## verificando pressupostos da regressão

plot(regressao, 1) #Idealmente, esse gráfico não apresentará nenhum padrão. Ou seja, a linha vermelha deverá ser 
# horizontal próxima da linha pontilhada que cruza o eixo y no valor 0.

plot(regressao1, 1)

plot(regressao2, 1)

plot(regressao, 3)

plot(regressao1, 3)

plot(regressao2, 3)

acf(regressao$residuals) #Neste gráfico, o resultado ideal é que as linhas verticais estejam dentro do intervalo delimitado pelas linhas azuis tracejadas horizontais. 
# A exceção é a primeira linha vertical, que sempre será alta e estará fora desse intervalo.

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

base_outliers <- base_final[-c(72, 112, 124, 150, 195), ]

regressao3 <- lm(base_outliers$IDEB ~ base_outliers$`Indicador Engajamento Aluno` + base_outliers$`Indicador Adoção Aluno`, data = base_outliers)

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

#Quanto maior o valor, maior a multicolinearidade. Não existe um valor objetivo para definir o quão grave é a multicolinearidade. 
# Alguns afirmam que é preocupante valores acima de 10, outros que valores acima de 4 já é o suficiente para termos problemas.

outlierTest(regressao3)

base_final_outliers <- left_join(banco_APNPS, base_outliers, by = c("COD_ESCOLA" = "Código da Escola"))

base_outliers_filtrada <- base_final_outliers %>%
  filter('IDEB Divulgado' != "NA")

(names(base_final_outliers)[35] <- "IDEB")
(names(base_final_outliers)[36] <- "IDEB Divulgado?")

glimpse(base_outliers_filtrada)

ggplot(base_final_outliers, aes(`Indicador Engajamento Aluno`, fill = `IDEB Divulgado?`)) +
  geom_density(alpha = 0.3)

ggplot(base_final_outliers, aes(`Indicador Adoção Aluno`, fill = `IDEB Divulgado?`)) +
  geom_density(alpha = 0.3)

ggplot(base_final_outliers, aes(`GOOGLE_CLASSROM.x`, fill = `IDEB Divulgado?`)) +
  geom_density(alpha = 0.3)

base_final_outliers %>%
  filter(!is.na(`IDEB Divulgado?`)) %>%
ggplot(aes(IMPRESSAS.x, fill = `IDEB Divulgado?`)) +
  geom_density(position = "stack") +
  scale_x_continuous(name = "APNPS Impressas", limits = c(0, 500)) +
  scale_y_continuous(name = "Contagem", limits = c(0, 0.05))

base_final_outliers %>%
  filter(!is.na(`IDEB Divulgado?`)) %>% 
  ggplot(aes(x = `IDEB Divulgado?`, 
             fill = `IDEB Divulgado?`)) +
geom_bar()


regressao4 <- lm(IDEB ~ `Indicador Engajamento Aluno` + `Indicador Adoção Aluno` + 
                `YOUTUBE.x` + `TV.x`+ `IMPRESSAS.x`,
                 data = base_final_outliers)

summary(regressao4)

confint(regressao4)

dwplot(regressao4, by_2sd = TRUE)

plot(regressao4, 1)

plot(regressao4, 2)

plot(regressao4, 3)

plot(regressao4, 4)

plot(regressao4, 5)

acf(regressao4$residuals)

vif(regressao4)

outlierTest(regressao4)

