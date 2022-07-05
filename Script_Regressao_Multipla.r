###### Regressão Linear Multipla ######

### Abrir arquivos ###

setwd("c:/R/SHP_Brasil/Dados_Dengue")

Teste <-read.csv(file = "dados_dengue_cluster_3_rs.csv",header=TRUE,sep=";",dec = ",")

Teste

### Transformar variáveis em núméricas e estandardizar ###

Teste$dens_casos<- as.numeric(Teste$dens_casos)

Teste$cosseno <- scale(as.numeric(Teste$cosseno))

Teste$seno <- scale(as.numeric(Teste$seno))

Teste$mei <- scale(as.numeric(Teste$mei)) 

Teste$precipitacao <- scale(as.numeric(Teste$precipitacao))

Teste$temperatura <- scale(as.numeric(Teste$temperatura))

str(Teste)

summary(Teste)

# Colonearidade

cor.test(Teste$temperatura,Teste$cosseno, method = "spearman") #forma nao visual

## Selecionar várias colunas

teste_1 <- Teste[c("dens_casos", "cosseno", "seno", "mei", "temperatura", "precipitacao")] 

names(teste_1)

m1 <- lm (dens_casos ~ seno + cosseno + mei + temperatura + precipitacao,teste_1)

summary(m1)

m2 <- lm (dens_casos ~ seno + mei + a_temp, teste_1)

summary(m2)

m3 <- lm (dens_casos ~ seno + mei, teste_1)

summary(m3)

AIC(m1, m2, m3)

### Gerar Gráficos ###

library(ggplot2)

ggplot(teste_1, aes(x=mei, y=dens_casos)) +
  geom_point()  + 
  geom_smooth(method = "lm", linetype = "dashed", col = "red") +
  theme_classic() +
  xlab("MEI") + ylab("Taxa incidência Dengue/100 mil hab.") +
  theme(text = element_text(size=16, family="TT Times New Roman"))