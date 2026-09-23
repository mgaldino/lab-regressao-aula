# Nota de entrega — Aula 5

## Escopo confirmado no syllabus

- Data: 23 de setembro de 2026.
- Tema: Regressão I — estimação por mínimos quadrados ordinários, caso bivariado e interpretação.
- Leituras: Aronow e Miller, capítulo 4, seção 4.1; Hansen, seções 3.3 a 3.8.
- Conhecimentos acumulados: probabilidade e simulação; esperança, variância, covariância, correlação e EQM; CEF, melhor preditor e BLP; resultados potenciais, identificação, atribuição aleatória, ignorabilidade, viés de seleção, variáveis pós-tratamento e distinção entre descrição, associação, previsão e causalidade.
- Produto AGNA: modelo linear simples e interpretação da associação entre o ano e a convergência média anual dos votos de Brasil e China.

## Arquivos entregues

- `projeto_agna/laboratorios/slides_aula_05_mqo_bivariado.Rmd`
- `projeto_agna/output/pdf/slides_aula_05_mqo_bivariado.pdf`
- `projeto_agna/scripts/06_laboratorio_aula5_mqo_bivariado.R`
- `projeto_agna/output/aula_05/hashes_protegidos_antes.txt`
- `projeto_agna/output/aula_05/hashes_protegidos_depois.txt`

## Fluxo do laboratório

O script lê diretamente o único banco autorizado, valida 1.762 resoluções, agrega explicitamente para 20 anos, preserva denominadores e numeradores, cria `anos_desde_1997`, constrói o gráfico em quatro etapas, calcula manualmente os coeficientes com covariância e variância, estima a mesma reta com `lm()`, compara os resultados, recupera valores ajustados e resíduos e encerra com interpretação, exercício de recentralização em 2009 e limites causais.

Resultados descritivos: intercepto de 0,767151656, interpretado como 76,715% previstos em 1997, e inclinação de 0,003891086, equivalente a associação de +0,389 ponto percentual por ano. Os 20 anos recebem peso igual, embora seus denominadores sejam diferentes.

## Testes e revisão

- Syllabus e leituras: PASS.
- Derivação de MQO: PASS.
- Conexão entre BLP populacional e MQO amostral: PASS.
- Unidades e interpretação dos coeficientes: PASS.
- Separação entre associação e causalidade: PASS.
- Ausência de inferência antecipada: PASS.
- Simplicidade e legibilidade do script: PASS.
- Numeração, legendas, unidades, denominadores e fontes: PASS.
- Compilação XeLaTeX e inspeção visual das 44 páginas da entrega inicial: PASS.
- Parse e execução integral do script: PASS.
- Coeficientes manuais e de `lm()` iguais com tolerância de 10^-12: PASS.
- Inexistência de `Rplots.pdf` e outputs acidentais: PASS.
- Integridade de autoria dos materiais anteriores: PASS; nenhuma frente da Aula 5 editou arquivos anteriores. A comparação byte a byte encontrou uma mudança concorrente da Aula 4, registrada abaixo e preservada sem restauração.

Warnings visíveis: `dplyr` e `ggplot2` foram compilados sob R 4.4.3, enquanto o runtime usado foi R 4.4.2. As mensagens normais de masking permaneceram visíveis. Não houve warning substantivo de dados, estimação ou compilação.

## Limitações e divergências

- A pasta `2026/lab-regressao-aula` não contém `.git` e não é reconhecida como repositório; portanto, não foi possível auditar `git status` ou produzir diff Git local. Nenhum commit ou push foi realizado.
- O syllabus, os dados e o produto da Aula 5 são consistentes: a base permite exatamente 20 observações anuais entre 1997 e 2016.
- Durante o trabalho, ocorreu um fluxo autoral concorrente da Aula 4. O arquivo `projeto_agna/scripts/05_laboratorio_aula4_causalidade.R` mudou entre os registros de hashes, e surgiram `slides_aula_04_causalidade_identificacao.Rmd` e seu PDF. Esses arquivos não pertencem à entrega da Aula 5, não foram editados pelas duas frentes e foram preservados no estado encontrado.
- O modelo descreve associação linear em uma amostra de 20 anos; não identifica efeito do tempo, de 2009, do comércio ou da China e não sustenta extrapolação além de 1997–2016.

## Atualização pedagógica de 22 de setembro de 2026

Após o parecer pedagógico e a discussão em sala planejada sobre o princípio plug-in, o arquivo-fonte e o PDF dos slides foram atualizados. A versão atual tem 54 páginas e começa com a regra intuitiva, a média e a variância plug-in, o contraste entre viés e consistência, condições de regularidade e a definição formal $\widehat\theta=T(\widehat F)$, antes de introduzir regressão.

- O gráfico dos 20 pontos anuais, ainda sem reta, abre a parte de regressão; a diferença entre os extremos observados e a inclinação do MQO é retomada após o ajuste.
- Os quatro pares do exemplo aparecem antes da derivação; duas pausas curtas interrompem o bloco algébrico e a pausa sobre recentralização foi deslocada para junto da explicação correspondente.
- As tabelas passaram a usar vírgula decimal. O ano bruto recebeu a notação $A_i$, distinta de $X_i=A_i-1997$. O gráfico da AGNU usa eixo de 0% a 100% e distingue os 20 anos dos denominadores anuais das taxas, que variam de 61 a 112 resoluções.
- A chamada de `lm()` nos slides usa os mesmos nomes de objetos e variáveis do roteiro do laboratório. A atividade final de interpretação passou a explicitar as unidades, e um slide indica três momentos para discutir as saídas práticas.

A atualização foi compilada com R Markdown e XeLaTeX. Foram conferidos o texto extraído do PDF, a paginação e a legibilidade das páginas alteradas. Os valores de 1997 (79,3%), 2016 (69,6%) e da inclinação (+0,39 ponto percentual por ano) foram confrontados com a base da aula. O roteiro do laboratório não foi modificado nesta atualização.
