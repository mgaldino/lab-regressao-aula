## aula 2

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse

glimpse(pib_cid)

head(pib_cid)

View(pib_cid)

# data Wrangling
library(tidyverse)

# digamos que quero o pib total médio e o pib per capita médio
# basta usar o comando summarise, que resume os dados e escolher a função mean.
# média do vetor 1,2,3,4
mean(1:4)

x <- c(1,2,3,NA)
mean(x)
mean(x, na.rm=TRUE)

# média do pib_total no banco pib_cid
mean(pib_cid$pib_total)

# média do pib_total no banco pib_cid com tidyverse/dplyr
summarise(pib_cid, mean(pib_total))

# média do pib_total no banco pib_cid com tidyverse/dplyr
summarise(pib_cid, media_pib = mean(pib_total))

# média do pib_total no banco pib_cid com tidyverse/dplyr e pipe
pib_cid %>%
  summarise(pib_medio = mean(pib_total))

# média do pib e pib per capita
pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))


df <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))

df %>% 
  glimpse()


# se eu quiser a soma dos pibs municipais
df <- pib_cid %>%
  summarise(soma_pib = sum(pib_total)) %>%
  head()

df <- pib_cid %>%
  filter(nome_munic == "São Paulo")

df_sao <- pib_cid %>%
  filter(sigla_uf == "SP") %>%
  filter(grepl("São", nome_munic))

glimpse(df_sao)

df_sao$nome_munic
View(df_sao)


# maior e menos pibs e pibs per capita entre municípios

df <- pib_cid %>%
  summarise(pib_max = max(pib_total),
            pib_min = min(pib_total),
            pib_per_capita_max = max(pib_per_capita),
            pib_per_capita_min = min(pib_per_capita)) 

# criar variáveis
df <- pib_cid %>%
  mutate(pib_max = max(pib_total),
         pib_min = min(pib_total))

View(df)
# linha cujo pib é máximo
df %>%
  filter(pib_total == pib_max)

# linha cujo pib é mínimo
df %>%
  filter(pib_total == pib_min)

df %>%
  filter(pib_total %in% c(pib_max, pib_min))

# filtrando pib pc max, min tbm
df <- pib_cid %>%
  mutate(pib_max = max(pib_total),
         pib_min = min(pib_total),
         pib_pc_max = max(pib_per_capita),
         pib_pc_min = min(pib_per_capita)) %>%
  filter(pib_total %in% c(pib_max, pib_min) | pib_per_capita %in% c(pib_pc_max, pib_pc_min))

# outra forma de filtras + selecionando variáveis
pib_cid %>%
  filter(pib_total == max(pib_total)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)

# validando que pib per capita mínimo é a mesma cidade de pib total mínimo
pib_cid %>%
  filter(pib_total == min(pib_total)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)

pib_cid %>%
  filter(pib_per_capita == min(pib_per_capita)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)


## group by
df <- pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio_uf = mean(pib_total))

View(df)

df <- pib_cid %>%
  group_by(sigla_uf, amazonia_legal) %>%
  summarise(pib_medio_uf = median(pib_total))

View(df)

df <- pib_cid %>%
  group_by(sigla_uf) %>%
  mutate(max_pib_uf = max(pib_total))

View(df)

# visualização

# gráficos

library(ggplot2)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point()


# gráficos  mais bonitos


pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme_light() + theme(text=element_text(size=20)) +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")


# gráficos  mais bonitos

# install.packages("remotes")
remotes::install_github("MatthewBJane/theme_park", force=T)
library(ThemePark)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme(text=element_text(size=20)) + theme_barbie() +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")

