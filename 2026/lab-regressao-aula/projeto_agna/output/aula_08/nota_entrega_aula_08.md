# Aula 8: entrega e reprodução

Preparação: 20/10/2026. Data da aula: 21/10/2026.

## Arquivos

- Slides editáveis: `projeto_agna/laboratorios/slides_aula_08_diagnosticos.Rmd`.
- Apresentação: `projeto_agna/output/pdf/slides_aula_08_diagnosticos.pdf`, 27 páginas, 16:9.
- Cálculos e gráficos: `projeto_agna/scripts/23_preparar_aula8_diagnosticos.R`.
- Verificações analíticas: `projeto_agna/scripts/24_validar_aula8_diagnosticos.R`.
- Resultados: `resultados_aula_08.rds`; objetos dos gráficos em `graficos_aula_08.rds`; nove figuras de apoio em PDF e PNG na pasta `figuras/`, sete delas utilizadas nos slides; seis tabelas auxiliares e o diagnóstico eleitoral em CSV.
- Registro de execução: `validacao.txt` e `sessao_R.txt`.

O YAML preserva tema Madrid, aspecto 16:9, classe 10pt, cores, blocos, rodapé e captions numeradas da Aula 7. Os cálculos ficam no script R; o Rmd importa resultados e gráficos e formata as tabelas.

## Organização didática

| Slides | Conteúdo |
|---|---|
| 1–6 | Objetivos da análise; erro populacional e resíduo amostral; identidade da diferença; erro de projeção; ortogonalidade amostral |
| 7–11 | Curvatura, termos não lineares e heterocedasticidade; consequências para estimação e inferência |
| 12–18 | Derivação da matriz hat, alavancagem, influência, sensibilidade, derivação da dependência dos resíduos e dependência nos erros |
| 19–24 | Aplicação eleitoral, observação do Exterior, treino e teste |
| 25–27 | Diagnósticos e decisões, exemplo de código e leituras |

A apresentação concentra-se na aproximação da CEF, na projeção linear, no diagnóstico dos resíduos e na previsão. A discussão de erros-padrão é uma preparação conceitual para a Aula 9, sem testes de hipóteses ou desenvolvimento dos estimadores HC.

Na notação, `u` é erro em torno da CEF, `epsilon` é erro da projeção linear e `e_hat` é resíduo amostral. As identidades amostrais de MQO não são confundidas com esperança condicional zero. A fórmula da variância dos resíduos explicita a matriz de covariância condicional `sigma^2 I`, incluindo a ausência de correlação entre os erros.

## Dados sintéticos

Todos os exemplos desta seção são simulações, sem interpretação eleitoral empírica.

- Semente 618320: 450 observações com X uniforme em [-2,2], erro normal independente com desvio-padrão 0,6 e CEF `2 + 1,5 X - 0,8 (X^2 - 4/3)`. O BLP populacional é `2 + 1,5 X`.
- Semente 618321: 600 observações com CEF `1 + 1,5 X`, X uniforme em [0,4] e desvio-padrão condicional do erro `0,25 + 0,5 X`.
- Semente 618322: 35 pontos de base e uma observação adicional por cenário: resíduo grande, alavancagem alta e ambos. A influência é avaliada pela mudança na inclinação.
- Semente 618323: 220 observações com erro AR(1), coeficiente 0,8 e inovações normais independentes com variância 1. A ordem aleatória é uma visualização dos mesmos resíduos, sem alterar a análise original.
- Semente 618324: 3.000 observações, X uniforme em [-2,2], regressor binário D com distribuição Bernoulli(0,5), independente de X, e probabilidade `p(D,X) = 0,15 + 0,6 plogis(1,5 X) + 0,12 D`. A resposta binária tem essa média condicional. A Figura 6 mostra erros da CEF conhecida, não resíduos de uma regressão estimada. O exemplo é usado apenas para discutir a distribuição e a variância dos erros de uma resposta binária.
- Semente 618325: erros exponenciais de taxa 1, centrados em zero, independentes de X normal. Distribuição de 4.000 inclinações, com n = 160 em cada nova amostra, em torno do coeficiente populacional 1,5. O gráfico não estabelece um tamanho universal suficiente à aproximação normal.
- Semente 618327: 20.000 novas respostas com X fixa, para conferir a variância condicional dos resíduos. Esse cálculo é suporte docente e não acrescenta um gráfico à apresentação.

## Aplicação eleitoral observada

Entrada: `projeto_agna/data/eleicoes_2022_2026/processed/presidente_2022_zonas_com_exterior.csv`. Fonte: TSE, snapshot preparado e documentado no curso para a Aula 7. A proveniência, os denominadores e os filtros estão no README e no dicionário daquele diretório. Nenhuma coleta nova foi realizada para esta aula.

O arquivo tem 2.637 linhas: 2.636 zonas das 27 UFs e uma observação agregada do Exterior, ZZ. Cada percentual usa todos os votos válidos do respectivo turno. Todos os ajustes e erros desta aula dão peso igual a cada zona; não constituem uma agregação ponderada nacional.

Os modelos comparáveis usam as mesmas 2.636 zonas brasileiras, com resposta igual à porcentagem de Lula no segundo turno de 2022. Os preditores são a porcentagem no primeiro turno, com ou sem indicadoras de UF. A inclinação comum com UF é aproximadamente 1,0111 p.p. por 1 p.p. do primeiro turno. O exemplo é descritivo e preditivo.

ZZ permanece no arquivo original e nos resultados. No ajuste separado que inclui a categoria Exterior, sua única observação tem alavancagem 1 e resíduo numericamente zero. Isso é uma consequência da indicadora exclusiva. Não se calcula HC2, HC3 ou exclusão desse ponto por divisão por `1-h`. A categoria serve como exemplo de falta de informação interna para estimar a variância do erro, apesar do ajuste perfeito.

A tabela de influência ordena as mudanças absolutas da inclinação comum, mas mostra mudanças com sinal: coeficiente completo menos coeficiente sem a zona. Essa medida não caracteriza toda a influência possível sobre outros coeficientes ou previsões.

## Avaliação preditiva

Semente 618326. Divisão única, aproximadamente 80%/20%, aleatória dentro de UF: 2.121 zonas de treino e 515 de teste. As especificações foram fixadas antes da comparação. O teste é de zonas reservadas da eleição de 2022; não avalia transporte entre eleições nem dependência espacial ou por grupos. A divisão única tem variabilidade.

| Modelo | REQM treino (p.p.) | REQM teste (p.p.) | EAM teste (p.p.) |
|---|---:|---:|---:|
| Primeiro turno | 1,506600 | 1,394471 | 1,044632 |
| Primeiro turno + UF | 1,204913 | 1,141295 | 0,846864 |
| Primeiro turno + quadrado + UF | 1,201279 | 1,142658 | 0,848064 |

O termo quadrático reduz o erro de treino, mas não melhora o erro de teste nesta divisão. A apresentação não generaliza essa comparação a toda divisão ou aplicação futura. Os slides arredondam os erros a duas casas decimais.

## Fontes conceituais

Consultadas em 20/10/2026:

- Hansen, Bruce E., *Econometrics*, seções 4.10–4.15, indicadas no programa. Complementos: seções 2.18, 2.25 e 2.30; 3.19–3.21; 4.16. Cópia local em Downloads, manuscrito de 2021 com a numeração das seções utilizada no curso. [Página do autor](https://users.ssc.wisc.edu/~behansen/econometrics/).
- Shalizi, Cosma R., *The Truth about Linear Regression*, seções 6.1 e 6.5, indicadas no programa. Cópia local de 06/05/2024; prefácio também conferido na [versão disponibilizada pelo autor](https://www.stat.cmu.edu/~cshalizi/TALR/TALR.pdf), datada de 20/10/2025.
- Galdino, Manoel, *Regressão*, capítulo “Checagem”, fonte local `10-Checagem.Rmd` do projeto `Livros meus/Regression/book-regression`. Os exemplos dos slides foram reconstruídos em scripts próprios. O manuscrito do livro não foi editado.
- Angrist e Pischke, *Mostly Harmless Econometrics*, seções 3.2 e 3.4.2; Angrist (2001), [Estimation of Limited Dependent Variable Models With Dummy Endogenous Regressors: Simple Strategies for Empirical Practice](https://economics.mit.edu/sites/default/files/publications/Estimation%20of%20Limited%20Dependent%20Variable%20Models%20Wi.pdf).

## Reprodução

Executar a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/23_preparar_aula8_diagnosticos.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/24_validar_aula8_diagnosticos.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e 'rmarkdown::render("projeto_agna/laboratorios/slides_aula_08_diagnosticos.Rmd", output_format="beamer_presentation", output_file="slides_aula_08_diagnosticos.pdf", output_dir="projeto_agna/output/pdf", output_options=list(keep_tex=TRUE), clean=TRUE)'
```

Dependências: R; `ggplot2`, `dplyr`, `patchwork`, `rmarkdown`, `knitr`, `here`; Pandoc e XeLaTeX. Os PDFs dos gráficos usam o dispositivo PDF padrão do R e os PNGs são exportados a 200 dpi. Os resultados numéricos são determinados pelas sementes; metadados de criação dos PDFs podem variar entre execuções.

## Verificações executadas

O script 24 retornou `VALIDACAO_AULA8_OK`. Conferiu tipos, ausências, chaves, limites e processos geradores; fórmula escalar de MQO e ortogonalidade; influência sintética contra reajustes; invariância à ordem das linhas; suporte e probabilidades da resposta binária; diferença de médias contra o coeficiente; média de Monte Carlo contra o alvo com tolerância pelo erro de simulação; variância dos resíduos contra a fórmula condicional; cobertura, denominadores e integridade da entrada eleitoral; inclinação com UF por centralização independente de `lm()`; exclusão de zonas contra reajustes; previsão e métricas de teste por álgebra matricial; existência das nove figuras em PDF e PNG.

A compilação XeLaTeX foi concluída. As 30 páginas da versão inicial foram renderizadas e inspecionadas. Na revisão de 20/10 sobre erro e resíduo, um slide foi substituído por três, levando o PDF a 32 páginas. Os slides 4–6 foram renderizados e examinados após a revisão; a conferência de coordenadas de todas as 32 páginas encontrou zero palavras fora das páginas. Figuras 1–9 e Tabelas 1–5 estão numeradas em sequência. Não foi realizado teste de duração ou compreensão em sala.

## Revisão sobre erro e resíduo, 20/10/2026

Os então slides 4–6 explicitam que o erro populacional depende dos parâmetros populacionais, em geral desconhecidos, enquanto o resíduo é calculado com os coeficientes estimados. O então slide 4 distingue o vetor de parâmetros `beta` do valor previsto `x_i' beta` e qualifica a observabilidade: quando `beta` é conhecido, o erro é calculável, inclusive na projeção linear de uma população finita inteiramente observada em um censo. A derivação apresenta `e_hat_i = epsilon_i - x_i' (beta_hat - beta)`. O slide seguinte distingue a minimização do EQM populacional no BLP da estimação por MQO na amostra e decompõe o erro de projeção em erro da CEF e erro de aproximação. A identidade entre erro, resíduo e erro de estimação também vale para o BLP quando a CEF não é linear. Nenhum cálculo, gráfico ou dado foi alterado por esta revisão.

## Corte do slide 3, 20/10/2026

Removido o slide “CEF, projeção linear e efeito causal”, a pedido do professor. A apresentação passa de 32 para 31 páginas. Os slides sobre erro e resíduo passam a ocupar as páginas 3–5. A comparação do texto das páginas restantes com a versão anterior verifica a preservação do conteúdo, descontando a renumeração do rodapé.

## Esclarecimento da construção do BLP no slide 7, 20/10/2026

O slide 7 explicita que o BLP `2 + 1,5 X` foi preservado pela escolha da distribuição simétrica de X e pela centralização do termo quadrático em `E[X^2] = 4/3`. Como `Cov(X,X^2) = 0`, o termo adicional tem média zero e é ortogonal a X. Isso não é uma regra geral de eliminar termos não lineares da CEF. A legenda distingue a CEF conhecida da reta estimada na amostra; os coeficientes desta última são aproximadamente 2,046 e 1,552. Os dados, os cálculos e o gráfico foram preservados; a revisão altera a explicação, a legenda, o tamanho do gráfico no slide e um comentário no script de preparação. O PDF permanece com 31 páginas.

## Heterocedasticidade como caso geral, slide 11, 20/10/2026

O slide 11 foi refeito para apresentar a variância condicional como função dos regressores e a homocedasticidade como uma restrição particular. A discussão de viés foi retirada. O slide trata das consequências para erros-padrão convencionais e robustos e explicita independência e condições para amostras grandes na validade dos robustos. A compilação terminou com 31 páginas. A comparação do texto de todas as páginas confirmou alteração apenas na página 11, que também foi renderizada e inspecionada visualmente. Nenhuma palavra ficou fora dos limites das páginas. Não houve alteração de dados, cálculos ou gráficos.

## Derivação da matriz hat, slide 12, 20/10/2026

Inserida antes da discussão de alavancagem a derivação que parte da fórmula matricial de MQO, substitui Y = X beta + epsilon e simplifica beta_hat = beta + (X'X)^(-1) X' epsilon. A multiplicação por X identifica H nos valores ajustados, e o slide explicita Y_hat = H Y. O antigo slide 12 passa à página 13, preservando seu conteúdo. O PDF foi recompilado com 32 páginas; as páginas 12 e 13 foram renderizadas e inspecionadas visualmente. A comparação do texto das demais páginas, descontando os rodapés, confirmou sua preservação. A conferência de coordenadas não encontrou palavras fora das páginas. Dados, cálculos e gráficos permanecem iguais.

## Duas fontes de dependência, slide 17, 21/10/2026

O slide 17 foi organizado em dois blocos: dependência entre resíduos criada pelo ajuste e dependência entre erros no processo gerador. O primeiro usa a identidade matricial e o exemplo da regressão apenas com intercepto, esclarecendo que essa dependência dos resíduos não exige, por si só, erros-padrão agrupados. O segundo retoma o processo temporal do slide anterior. As recomendações de erros-padrão e reamostragem são vinculadas explicitamente à segunda situação. O PDF foi recompilado com 32 páginas. A comparação do texto de todas as páginas confirmou mudança apenas na página 17, que foi renderizada e inspecionada visualmente. A conferência de coordenadas não encontrou palavras fora das páginas. Dados, cálculos e gráficos não foram alterados.

## Derivação do exemplo com intercepto, slides 17–18, 21/10/2026

O antigo slide 17 foi desenvolvido em dois. O slide 17 apresenta o modelo Y_i = mu + epsilon_i, a estimação do intercepto pela média amostral e a substituição completa que produz e_hat_i = epsilon_i - epsilon_bar. O bloco final interpreta a média compartilhada, a soma zero e a identidade matricial geral. O slide 18 trata separadamente da dependência nos erros e de suas consequências para a inferência. A apresentação foi recompilada com 33 páginas. Os dois slides foram renderizados e inspecionados visualmente; a distribuição dos elementos foi ajustada para evitar cortes. A comparação das demais páginas, descontando os rodapés, confirmou a preservação de seu conteúdo. A conferência final de coordenadas não encontrou palavras fora das páginas. Dados, cálculos e gráficos não foram alterados.

## Retirada da discussão de causalidade, 21/10/2026

Retirados os antigos slides 20 e 21, sobre experimento com resposta binária e alcance da interpretação causal. Foram removidas as demais menções à causalidade na capa, objetivos, curvatura, aplicação eleitoral, síntese e leituras. O exemplo de resposta binária permanece apenas como ilustração do suporte e da variância do erro; D é definido como regressor binário. O slide 6 explicita que os valores ajustados de uma regressão dos resíduos nos mesmos regressores são zero por construção, enquanto sua média condicional pode variar. As identidades amostrais não testam média condicional zero do erro populacional. Não se afirma que a média condicional dos resíduos seja sempre zero nem que toda restrição de forma funcional seja intestável. A apresentação foi recompilada com 31 páginas. As nove páginas afetadas foram renderizadas e inspecionadas visualmente. A comparação das demais páginas, descontando os rodapés, confirmou a preservação de seu conteúdo. Não há termos de causalidade no fonte ou no texto do PDF; Figuras 1–9 e Tabelas 1–5 mantêm numeração sequencial; nenhuma palavra ficou fora das páginas. Os cálculos, os gráficos e os dados preservados não foram alterados, e sua integridade foi conferida pelo manifesto. Os registros históricos desta nota documentam versões anteriores.

## Caminhos para inferência com dependência serial, slide 18, 21/10/2026

O slide 18 passa a indicar erros-padrão HAC, como Newey–West, para séries temporais com heterocedasticidade e autocorrelação, sob condições de dependência fraca e amostras grandes. Explicita a ordenação temporal, a escolha e a sensibilidade às defasagens e a manutenção do ajuste de MQO ao mudar a estimação da incerteza. Mantém erros-padrão agrupados para dependência dentro de grupos e reamostragem por grupos ou blocos. Foi acrescentada uma linha de R, sqrt(diag(sandwich::NeweyWest(m))), com seleção automática de defasagens. Fontes primárias consultadas em 21/10/2026: https://sandwich.r-forge.r-project.org/reference/NeweyWest.html e https://sandwich.r-forge.r-project.org/reference/vcovCL.html. A chamada de R foi executada com sandwich 3.1.1 no modelo y ~ x das 220 observações do exemplo temporal, cuja ordem foi conferida: os erros-padrão retornados foram aproximadamente 0,346356 para o intercepto e 0,103269 para a inclinação. Esse cálculo serve apenas para conferir o exemplo de código. O PDF foi recompilado com 31 páginas; apenas o texto da página 18 mudou. A página foi renderizada e inspecionada visualmente, e a conferência de todas as páginas não encontrou palavras fora dos limites. Nenhum dado, gráfico ou resultado analítico preservado foi alterado.

## Corte dos slides 19–21 sobre distribuição e normalidade, 21/10/2026

Retirados os slides “Resposta binária e distribuição do erro”, “Distribuição dos erros e do estimador” e “Normalidade como hipótese específica”, a pedido do professor. A linha sobre não normalidade também foi removida da tabela de síntese. O slide 18, sobre dependência e inferência, passa diretamente ao novo slide 19, sobre ajuste e previsão fora da amostra. O PDF foi recompilado com 28 páginas. A comparação das páginas restantes, descontando rodapés e numeração de figuras, confirmou alteração apenas na síntese. As Figuras 1–7 e as Tabelas 1–5 têm numeração sequencial. A transição e a síntese foram renderizadas e inspecionadas visualmente. A conferência de todas as páginas não encontrou palavras fora dos limites. A discussão de normalidade não aparece mais no texto do PDF. Os dados, os cálculos e os nove arquivos de figuras de apoio foram preservados, incluindo os dois exemplos que deixaram de integrar os slides.

## Corte do slide 19 sobre ajuste e previsão fora da amostra, 21/10/2026

Retirado o slide “Ajuste e previsão fora da amostra”, a pedido do professor. A apresentação foi recompilada com 27 páginas. O novo slide 19 é “Aplicação: eleições de 2022”, que segue diretamente a discussão de dependência e inferência. A comparação do texto das demais páginas, descontando os rodapés, confirmou sua preservação. O novo slide 19 foi renderizado e inspecionado visualmente. A conferência de todas as páginas não encontrou palavras fora dos limites, e as Figuras 1–7 e Tabelas 1–5 mantêm numeração sequencial. Nenhum cálculo, dado ou gráfico foi alterado.
