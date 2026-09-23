# Prompt para preparar os materiais da Aula 4

Prepare os materiais da Aula 4 da disciplina FLS 6183 — Métodos Quantitativos de Pesquisa II, no projeto de 2026.

Antes de produzir qualquer material, leia o `syllabus.Rmd` atualizado. Confirme no início do relatório de entrega:

1. o tema exato da Aula 4;
2. a leitura correspondente em Aronow e Miller;
3. os conhecimentos acumulados nas Aulas 1–3;
4. o produto do projeto cumulativo AGNA previsto para a Aula 4;
5. qualquer divergência entre o syllabus, os dados e os materiais existentes.

O syllabus atualmente indica:

- **tema:** causalidade — resultados potenciais, identificação, atribuição aleatória, ignorabilidade, viés de seleção e variáveis pós-tratamento;
- **leitura:** Aronow e Miller, cap. 7, seções 7.1.1, 7.1.4, 7.1.5 e 7.1.7;
- **produto AGNA:** diagrama e explicação dos limites de uma comparação descritiva entre períodos.

Não use versões antigas do syllabus e não deduza o conteúdo pela numeração de arquivos. Se o syllabus tiver sido alterado, siga a versão atual e registre a diferença.

## Organização do trabalho

Separe o trabalho em duas frentes independentes e simultâneas:

- **Frente 1:** slides teóricos em RMarkdown/Beamer;
- **Frente 2:** script simples do laboratório em R.

A frente do laboratório é a prioridade operacional. As duas frentes não devem editar os mesmos arquivos.

Antes de editar:

- verifique o estado do Git e identifique corretamente a raiz versionada;
- preserve integralmente mudanças locais do professor;
- não altere o `syllabus.Rmd`;
- não altere os materiais ou scripts das Aulas 2 e 3;
- registre hashes dos arquivos protegidos antes e depois do trabalho;
- não faça commit, push, envio de e-mail ou alteração de arquivos autorais sem solicitação explícita.

## Frente 1 — slides teóricos

Produza um RMarkdown compilável em PDF com Beamer. Não produza PowerPoint.

Use Aronow e Miller como referência principal, detalhando os pontos que o livro apresenta rapidamente. Os alunos devem chegar à aula dominando:

- probabilidade, variáveis aleatórias e simulação;
- esperança, variância, EQM, covariância e correlação;
- CEF, melhor preditor e BLP;
- a distinção entre descrição, associação, previsão e identificação causal.

Comece com um mapa conceitual:

- qual é a pergunta causal;
- por que observar uma associação não identifica um efeito;
- como a aula se conecta à CEF e à previsão da Aula 3;
- o que os alunos saberão fazer ao final.

Para cada conceito central, use a sequência:

1. definição em palavras;
2. intuição;
3. definição formal;
4. exemplo numérico completamente calculado;
5. representação gráfica ou diagrama, quando útil;
6. interpretação substantiva;
7. erro comum ou limite.

Introduza e defina, sem saltos:

- unidade, tratamento/exposição e resultado;
- resultados potenciais `Y_i(1)` e `Y_i(0)`;
- efeito causal individual e problema fundamental da inferência causal;
- efeito médio do tratamento;
- identificação versus estimação;
- decomposição da diferença observada e viés de seleção;
- atribuição aleatória;
- ignorabilidade e a diferença entre condicionar em variáveis pré-tratamento e pós-tratamento;
- riscos de controlar mediadores ou colisores pós-tratamento.

Derive resultados não triviais passo a passo, em mais de um slide quando necessário. Defina cada símbolo na primeira aparição. Use uma ideia principal por slide e perguntas adequadas à pós-graduação.

Inclua pelo menos um exemplo em que associação e efeito causal tenham sinais ou magnitudes diferentes. Mostre explicitamente que um bom preditor do resultado não é necessariamente uma variável de ajuste válida.

Todas as figuras e tabelas devem ser numeradas, ter legenda e informar unidade, denominador e natureza hipotética ou observada dos valores. Use português correto, UTF-8 e acentos.

Compile o PDF e inspecione visualmente todas as páginas: fórmulas, subscritos, resultados potenciais, setas do diagrama, títulos, cortes, sobreposições, tamanho do texto e sequência pedagógica.

## Frente 2 — laboratório em R

Continue o mesmo projeto cumulativo AGNA. Não crie um exercício desconectado e não estime efeitos causais nesta aula.

A pergunta do laboratório é:

> O que seria necessário para interpretar causalmente a diferença observada na convergência Brasil–China entre períodos, e por que os dados atuais não fornecem essa identificação?

Use a comparação descritiva já construída nas Aulas 2 e 3 apenas como ponto de partida. Não apresente o período pós-2009 como tratamento automaticamente bem definido. Não use regressão, matching, painel, DiD, `china_principal_destino_paper` ou variáveis de comércio para estimar efeitos.

O produto do laboratório deve conter:

1. recuperação simples da diferença descritiva entre períodos;
2. formulação de uma pergunta causal hipotética e explicitamente bem definida;
3. identificação da unidade, exposição, resultado e momento temporal;
4. diagrama causal simples, construído progressivamente;
5. explicação de caminhos de confundimento plausíveis;
6. distinção entre variáveis pré-tratamento, mediadores e variáveis pós-tratamento;
7. uma tabela numerada que separe o que é observado, não observado e assumido;
8. conclusão explícita de que a comparação disponível permanece descritiva.

O diagrama deve ser substantivamente defensável e parcimonioso. Cada nó e cada seta devem ser explicados. Não acrescente variáveis apenas para tornar o desenho mais complexo.

### Simplicidade do script

O código será projetado ao vivo. Portanto:

- mantenha uma sequência curta, linear e comentada;
- carregue pacotes com `library()` e deixe mensagens visíveis;
- use `data.table`, `dplyr`, `ggplot2` e `here`, acrescentando outro pacote somente se indispensável e justificando-o;
- leia diretamente, com `fread(here(...))`, apenas o banco efetivamente usado;
- use sempre `dplyr::select()` ao selecionar colunas;
- não use funções auxiliares, loops, busca automática de arquivos ou programação defensiva extensa;
- não use `suppressPackageStartupMessages()`, `warning = FALSE`, `options(warn = -1)` ou equivalentes;
- dê nomes claros em português aos objetos;
- explique o propósito de cada objeto novo antes de criá-lo;
- faça uma transformação por etapa e mostre o resultado intermediário;
- numere e forneça legenda para todas as tabelas e figuras.

Siga aproximadamente este roteiro:

1. objetivo substantivo;
2. carregamento dos pacotes;
3. leitura da base;
4. unidade de análise e validações simples;
5. recuperação da diferença descritiva;
6. pergunta causal e resultados potenciais;
7. construção incremental do diagrama;
8. classificação temporal das variáveis;
9. exercício curto para modificar uma seta ou variável e defender a decisão;
10. limites e conclusão sem alegação causal.

## Validação e entrega

Teste os materiais sem aumentar artificialmente a complexidade do script didático.

Entregue:

- o RMarkdown dos slides;
- o PDF Beamer compilado;
- o script R simples do laboratório;
- somente os arquivos adicionais estritamente necessários;
- uma nota curta com tema, leitura, arquivos, fluxo do laboratório, testes, warnings, limitações e divergências encontradas.

Exija uma revisão independente final com veredito `PASS` ou `FAIL` para:

- fidelidade ao syllabus;
- correção da notação de resultados potenciais;
- separação entre associação e causalidade;
- coerência temporal das variáveis;
- simplicidade e legibilidade do script;
- numeração e legenda de tabelas e figuras;
- integridade dos materiais anteriores.

Não finalize enquanto houver um `FAIL` material.
