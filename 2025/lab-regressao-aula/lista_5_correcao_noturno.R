## lista 5 - noturno correção

# Carregue o pacote
library(PNADcIBGE)
# Importe os dados desejados
# data <- get_pnadc(year=2017,
#                   quarter=4,
#                   vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
#                     design=FALSE,
#                   savedir=tempdir())

# saveRDS(data, file="dados_pnad17.rds")

#importa banco de dados da PNAd que foi salvo localmente
data <- readRDS(file="dados_pnad17.rds")

write.csv2(data, file="dados_pnad17.csv")


## 2
library(tidyverse)
library(tidylog)
data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)
# Renomeie as variáveis:
data <- data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)

reg <- lm(Renda ~Sexo, data=data)
summary(reg)

data %>%
  group_by(Sexo) %>%
  summarise(mean(Renda, na.rm=T))

reg1 <- lm(Renda ~Horas_trabalhadas, data)
summary(reg1)

# IC a 95%
28.9009 + 1.96*0.4535
28.9009 -1.96*0.4535
# IC é [28.01204, 29.78976]