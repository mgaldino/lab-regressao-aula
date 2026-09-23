# Validação docente dos dados e da resolubilidade da Lista 1.
# Execute da raiz 2026/lab-regressao-aula; não distribua como gabarito.
Sys.setenv(OMP_NUM_THREADS = "1")
options(encoding = "UTF-8", scipen = 999)
suppressPackageStartupMessages(library(dplyr))
base <- file.path("listas", "lista_01")
saida <- file.path(base, "docente", "validacao")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
checks <- list()
verificar <- function(nome, condicao) {
  checks[[length(checks) + 1L]] <<- data.frame(
    verificacao = nome, resultado = if (isTRUE(condicao)) "PASS" else "FAIL"
  )
  if (!isTRUE(condicao)) stop(nome)
}
igual <- function(x, y, tol = 1e-10) max(abs(x - y)) < tol
v <- function(x) mean((x - mean(x))^2)
cv <- function(x, y) mean((x - mean(x)) * (y - mean(y)))
eqm <- function(y, g) mean((y - g)^2)
gravar <- function(x, nome) write.csv(x, file.path(saida, nome), row.names = FALSE, fileEncoding = "UTF-8")

pop <- read.csv2(file.path(base, "dados", "populacao_municipios_2020.csv"),
                 fileEncoding = "latin1", stringsAsFactors = FALSE)
verificar("Municipios: dimensoes e colunas", nrow(pop) == 5570 && ncol(pop) == 3 &&
            identical(names(pop), c("uf", "nome_munic", "populacao")))
verificar("Municipios: 27 UFs, chave e ausentes", length(unique(pop$uf)) == 27 &&
            !anyDuplicated(pop[c("uf", "nome_munic")]) && !anyNA(pop))
verificar("Municipios: populacoes inteiras positivas", is.numeric(pop$populacao) &&
            all(pop$populacao > 0 & pop$populacao == floor(pop$populacao)))
verificar("Municipios: acentos e apostrofos preservados", "São Paulo" %in% pop$nome_munic &&
            any(grepl("'", pop$nome_munic, fixed = TRUE)) && !any(grepl("�", pop$nome_munic)))
sp <- dplyr::filter(pop, uf == "SP")
verificar("SP: 645 municipios e extremos plausiveis", nrow(sp) == 645 &&
            min(sp$populacao) > 0 && max(sp$populacao) == 12325232)
verificar("Variancia: denominador n versus n-1", igual(v(sp$populacao), var(sp$populacao) * 644 / 645, 1e-4))
verificar("Mudanca de unidade: media, desvio e variancia", 
            igual(mean(sp$populacao / 1000), mean(sp$populacao) / 1000) &&
            igual(sqrt(v(sp$populacao / 1000)), sqrt(v(sp$populacao)) / 1000) &&
            igual(v(sp$populacao / 1000), v(sp$populacao) / 1000^2, 1e-7))
por_uf <- pop |> group_by(uf) |> summarise(n = n(), total = sum(populacao),
                                        media = mean(populacao), .groups = "drop") |> arrange(desc(media))
gravar(por_uf, "medias_por_uf.csv")
verificar("Esperanca iterada por UF", igual(weighted.mean(por_uf$media, por_uf$n), mean(pop$populacao)))
gravar(data.frame(n = nrow(sp), menor = min(sp$populacao),
                  media = mean(sp$populacao), mediana = median(sp$populacao),
                  sd = sd(sp$populacao), variancia_n = v(sp$populacao),
                  menos_50mil = sum(sp$populacao < 50000),
                  proporcao_menos_50mil = mean(sp$populacao < 50000)), "resumo_sp.csv")
gravar(sp |> dplyr::filter(populacao > 1e6), "sp_mais_1milhao.csv")

agna <- read.csv(file.path(base, "dados", "brasil_convergencia_china_1997_2016.csv"),
                 fileEncoding = "UTF-8", stringsAsFactors = FALSE)
agna$data <- as.Date(agna$data)
variaveis_analise <- agna |> dplyr::select(rcid, data, ano, periodo_2009, pos_2009,
                                        tema, voto_brasil, voto_china, convergente)
verificar("AGNU: chave, dimensoes e ausentes de analise", nrow(agna) == 1762 && !anyDuplicated(agna$rcid) && !anyNA(variaveis_analise))
gravar(data.frame(variavel = names(agna), ausentes = colSums(is.na(agna))), "ausentes_agna.csv")
verificar("AGNU: datas, janela e periodos", all(agna$ano %in% 1997:2016) &&
            all(as.integer(format(agna$data, "%Y")) == agna$ano) &&
            all(agna$pos_2009 == as.integer(agna$ano >= 2009)) &&
            all((agna$periodo_2009 == "2009-2016") == (agna$pos_2009 == 1)))
verificar("AGNU: indicadores e igualdade de votos", all(agna$convergente %in% 0:1) &&
            all(agna$convergente == as.integer(agna$voto_brasil == agna$voto_china)))
x <- agna$pos_2009; y <- agna$convergente; p <- mean(y)
gravar(as.data.frame(table(x, y)), "tabela_conjunta.csv")
verificar("Binaria: primeiro e segundo momentos e variancia", igual(mean(y^2), p) && igual(v(y), p * (1-p)))
set.seed(6183)
sim <- rbinom(10000, size = 1, prob = p)
sim_res <- do.call(rbind, lapply(c(100, 1000, 10000), function(n) data.frame(n = n, media = mean(sim[seq_len(n)]), variancia = v(sim[seq_len(n)]))))
gravar(sim_res, "simulacao.csv")
verificar("Simulacao: suporte e tamanho", length(sim) == 10000 && all(sim %in% 0:1))
cef <- agna |> group_by(pos_2009) |> summarise(n = n(), media = mean(convergente), .groups = "drop")
pred <- agna |> left_join(cef |> dplyr::select(pos_2009, media), by = "pos_2009")
verificar("Juncao por periodo: linhas e ausentes", nrow(pred) == 1762 && !anyNA(pred$media))
verificar("Esperanca iterada AGNU", igual(weighted.mean(cef$media, cef$n), p))
verificar("Desvio CEF: media condicional zero", max(abs(tapply(y-pred$media, x, mean))) < 1e-12)
beta <- cov(x, y) / var(x); alpha <- p - beta * mean(x)
verificar("BLP binario coincide com CEF", igual(alpha+beta*x, pred$media))
verificar("BLP: inclinacao igual diferenca", igual(beta, diff(cef$media)))
verificar("BLP: centro e denominadores cancelados", igual(alpha+beta*mean(x), p) && igual(beta, cv(x,y)/v(x)))
ct <- agna |> group_by(tema, pos_2009) |> summarise(n = n(), previsao_tema = mean(convergente), .groups = "drop")
com_tema <- pred |> left_join(ct, by = c("tema", "pos_2009"))
verificar("Tema: particao e juncao preservam votos", sum(ct$n) == 1762 && nrow(com_tema) == 1762 && !anyNA(com_tema$previsao_tema))
exibir <- ct |> group_by(tema) |> dplyr::filter(n() == 2, min(n) >= 30) |> ungroup()
verificar("Figura: ha temas elegiveis nos dois periodos", nrow(exibir) > 0 && all(exibir$n >= 30))
comparacao <- data.frame(preditor = c("0.5", "1", "media geral", "CEF periodo", "BLP periodo", "CEF tema-periodo"),
                        eqm = c(eqm(y,.5), eqm(y,1), eqm(y,p), eqm(y,pred$media), eqm(y,alpha+beta*x), eqm(y,com_tema$previsao_tema)))
verificar("EQM: ordem e igualdade CEF-BLP", comparacao$eqm[1] > comparacao$eqm[2] && comparacao$eqm[2] > comparacao$eqm[3] && comparacao$eqm[3] > comparacao$eqm[4] && igual(comparacao$eqm[4], comparacao$eqm[5]) && comparacao$eqm[6] <= comparacao$eqm[4])
verificar("Decomposicao EQM para quatro candidatos", all(vapply(list(rep(.5,length(y)),rep(1,length(y)),rep(p,length(y)),alpha+beta*x), function(g) igual(eqm(y,g),eqm(y,pred$media)+mean((pred$media-g)^2)), logical(1))))
verificar("Decomposicao variancia e ganho preditivo", igual(v(y), mean(pred$media*(1-pred$media)) + v(pred$media)) && igual(eqm(y,p)-eqm(y,pred$media), v(pred$media)))
gravar(comparacao, "eqm.csv"); gravar(cef, "cef_periodo.csv"); gravar(ct, "cef_tema_periodo.csv"); gravar(exibir,"temas_elegiveis.csv")
gravar(data.frame(alpha = alpha, beta = beta, covariancia_n = cv(x,y), correlacao = cor(x,y)), "blp.csv")

# Distribuicoes exatas do exercicio 8; sem aproximacao Monte Carlo.
a <- c(-1,0,1); b <- a^2; u <- b-mean(b)
verificar("Nao linear: covariancia zero e EQMs distintos", igual(cv(a,b),0) && igual(eqm(b,mean(b)),2/9) && igual(eqm(b,b),0))
verificar("Desvio BLP: ortogonal, mas nao centro condicional zero", igual(mean(u),0) && igual(cv(a,u),0) && any(abs(u) > 0))
momentos <- function(z,w) {
  m <- sum(z*w); v0 <- sum(w*(z-m)^2)
  c(media=m, variancia=v0, m3=sum(w*(z-m)^3)/v0^1.5,
    m4=sum(w*(z-m)^4)/v0^2, cauda=sum(w[abs(z-m)>=1.5*sqrt(v0)]))
}
ma <- momentos(c(-1,1), c(.5,.5)); mb <- momentos(c(-sqrt(3),0,sqrt(3)), c(1/6,2/3,1/6))
verificar("Momentos: mesmas medias/variancias, curtoses distintas", igual(ma[1:3],mb[1:3]) && igual(ma[4],1) && igual(mb[4],3))
verificar("Chebyshev: probabilidades exatas respeitam limite", ma[5] <= 1/1.5^2 && mb[5] <= 1/1.5^2)
w <- c(1/2,1/3,1/6); ea <- sum(w*a); eb <- sum(w*b)
betaw <- sum(w*(a-ea)*(b-eb))/sum(w*(a-ea)^2)
alphaw <- eb-betaw*ea
verificar("BLP muda com pesos mantendo CEF", igual(betaw,-1/5) && igual(alphaw,3/5))
gravar(data.frame(distribuicao=c("A","B"),rbind(ma,mb),check.names=FALSE),"momentos_opcional.csv")
gravar(do.call(rbind, checks), "checks.csv")
capture.output(sessionInfo(), file = file.path(saida, "sessionInfo.txt"))
message(sprintf("PASS: %d verificacoes da Lista 1.", length(checks)))
