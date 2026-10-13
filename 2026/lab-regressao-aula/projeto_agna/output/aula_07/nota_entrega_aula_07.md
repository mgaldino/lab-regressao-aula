# Aula 7: entrega e reprodução

Preparação: 13/10/2026. Data da aula: 14/10/2026.

## Arquivos

- Fonte dos slides: `projeto_agna/laboratorios/slides_aula_07_propriedades_mqo.Rmd`.
- PDF: `projeto_agna/output/pdf/slides_aula_07_propriedades_mqo.pdf`, 22 páginas, 16:9.
- Cálculos e figuras: `projeto_agna/scripts/12_simulacoes_aula7_propriedades_mqo.R`.
- Verificação analítica: `projeto_agna/scripts/13_validar_aula7_propriedades_mqo.R`.
- Resultados, amostras, estimativas e figuras: nesta pasta. O Rmd lê os resultados e importa as figuras. Os cálculos permanecem no script R.

## Conteúdo e notação

Sequência: BLP e estimador de MQO; duas amostras da mesma população; distribuição amostral; viés; variância e EQM; consistência; condições para estimar o BLP; demonstração da consistência; média condicional zero; demonstração do não viés; variância condicional; versão clássica de Gauss–Markov e prova; Monte Carlo com diferentes tamanhos de amostra e estruturas do erro; alvo populacional da projeção; bootstrap de pares; síntese e leituras.

Os slides 5–7 esclarecem que o viés é definido pela esperança na distribuição amostral do estimador. A expressão “não viés para n fixo” significa que essa esperança coincide com o parâmetro sem tomar um limite assintótico. O erro de uma estimativa observada é uma realização da diferença entre estimador e parâmetro. A consistência descreve uma sequência quando n cresce.

Notação preservada: `alpha`, `beta_j` e o vetor de coeficientes contendo o intercepto; `epsilon` como erro da projeção linear; `e_hat` como resíduo amostral; transposta com linha. O erro `u` da regra geradora no slide 17 é explicitamente distinto do erro da projeção linear. O MQO converge para o coeficiente do BLP, 2,3, nesse cenário, enquanto a regra original inclui o parâmetro 1,5.

O tema Madrid, as cores, a tipografia, os blocos e o rodapé foram copiados do YAML vigente da Aula 6. O número de páginas considera a nota sobre o ritmo observado em 30/09: meta de até 20–22 páginas para conteúdo misto. Não foi criado um roteiro discente de laboratório.

## Simulações

Dados inteiramente sintéticos. Processo principal: X normal com média 10 e desvio-padrão 3, Y = 5 + 1,5 X + erro, com erro normal independente e desvio-padrão 2. São 5.000 amostras independentes por cenário e 5.000 reamostras bootstrap da primeira amostra observada. As sementes são 618307–618311, documentadas nos scripts.

Os cenários incluem n = 40, 160 e 640; aumento do desvio-padrão do erro para 6; heterocedasticidade com variância condicional `2 * (1 + ((X - 10)/3)^2)` e variância incondicional igual a 4; regra geradora com `u = 0,8 * (X - 10) + v`, cujo BLP tem intercepto -3 e inclinação 2,3. A normalidade faz parte desses exemplos e não é uma condição necessária para a versão clássica de Gauss–Markov apresentada.

As médias e dispersões de Monte Carlo são aproximações. A variância amostral populacional e o viés exato são propriedades teóricas sob as hipóteses indicadas. O erro-padrão bootstrap também é uma estimativa e varia conforme a amostra observada.

## Fontes

Programa vigente, entrada da Aula 7 em `syllabus.Rmd`. Aronow e Miller (2019), *Foundations of Agnostic Statistics*, seção 3.2.2, pp. 102–105; complemento para bootstrap, seção 3.4.3, pp. 130–132. Hansen, *Econometrics*, seções 4.4–4.8 e 7.2, com a seção 4.7 sustentando a ressalva sobre existência dos momentos do estimador. Conteúdo conferido nas cópias locais dos livros em Downloads em 13/10/2026. A apresentação usa a versão clássica do teorema de Gauss–Markov, restrita a estimadores lineares e não viesados, e a identifica expressamente.

## Reprodução

Executar a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/12_simulacoes_aula7_propriedades_mqo.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/13_validar_aula7_propriedades_mqo.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e 'rmarkdown::render("projeto_agna/laboratorios/slides_aula_07_propriedades_mqo.Rmd", output_format = "beamer_presentation", output_file = "slides_aula_07_propriedades_mqo.pdf", output_dir = "projeto_agna/output/pdf", output_options = list(keep_tex = TRUE), clean = TRUE)'
```

Dependências: R, ggplot2, dplyr, rmarkdown, knitr, here, Pandoc e XeLaTeX. As figuras vetoriais usam o dispositivo PDF do R, sem depender de Cairo/X11.

## Verificações executadas

O validador retornou `VALIDACAO_AULA7_OK`. Conferiu tipos, ausências, duplicidades e número de repetições; a fórmula escalar contra `lm()`; equações normais; decomposição empírica EQM = variância + viés ao quadrado; médias e variâncias de Monte Carlo contra os alvos e a variância exata do MQO no caso normal homocedástico; esperança e covariância condicionais com X fixa; reamostragem dos pares; estimativas bootstrap selecionadas contra `lm()`; e a diferença positiva semidefinida de covariâncias da prova de Gauss–Markov. Os testes de Monte Carlo usam tolerâncias proporcionais ao erro de simulação.

Compilação XeLaTeX concluída. Todas as 22 páginas do PDF final foram renderizadas e examinadas. As páginas 3, 5, 7, 14, 17 e 20 também foram inspecionadas em resolução maior. A extração de coordenadas do texto encontrou zero palavras fora das páginas. As legendas duplicadas das Figuras 1 e 5 e a ordem dos painéis da Figura 6 foram corrigidas antes da compilação final. A revisão pedagógica conferiu a sequência, as definições de símbolos e as transições entre propriedades do estimador, hipóteses e demonstrações. Não houve teste empírico com dados observacionais.

SHA-256 do PDF entregue: `d55ee4d9c22fa3615925b6afccb68f6554e226a828517ca269b46d4147a310cb`.
