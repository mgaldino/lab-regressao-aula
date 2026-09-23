# Prompt para preparar os materiais da Aula 5

Prepare os materiais da Aula 5 da disciplina FLS 6183 — Métodos Quantitativos de Pesquisa II, no projeto de 2026.

Antes de produzir qualquer material, leia o `syllabus.Rmd` atualizado. Confirme no início do relatório de entrega:

1. o tema exato da Aula 5;
2. as leituras correspondentes;
3. os conhecimentos acumulados nas Aulas 1–4;
4. o produto do projeto cumulativo AGNA previsto para a Aula 5;
5. qualquer divergência entre o syllabus, os dados e os materiais existentes.

O syllabus atualmente indica:

- **data:** 23 de setembro de 2026;
- **tema:** Regressão I — estimação por mínimos quadrados ordinários, caso bivariado e interpretação;
- **leituras:** Aronow e Miller, cap. 4, seção 4.1; Hansen, seções 3.3 a 3.8;
- **produto AGNA:** modelo linear simples e interpretação da associação com o ano.

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
- não altere os materiais ou scripts das Aulas 2, 3 e 4;
- registre hashes dos arquivos protegidos antes e depois do trabalho;
- não faça commit, push, envio de e-mail ou alteração de arquivos autorais sem solicitação explícita.

## Frente 1 — slides teóricos

Produza um RMarkdown compilável em PDF com Beamer. Não produza PowerPoint.

Use Aronow e Miller e Hansen como referências principais, detalhando os pontos apresentados rapidamente. Os alunos devem chegar à aula dominando:

- esperança, variância, covariância, correlação e EQM;
- CEF, melhor preditor e BLP;
- resultados potenciais, identificação, atribuição aleatória, ignorabilidade, viés de seleção e variáveis pós-tratamento;
- a distinção entre descrição, associação, previsão e causalidade.

Comece com um mapa conceitual:

- qual problema o MQO resolve;
- como o MQO amostral se conecta ao BLP populacional da Aula 3;
- por que estimação e identificação são problemas diferentes;
- o que os alunos saberão calcular e interpretar ao final.

Para cada conceito central, use a sequência:

1. definição em palavras;
2. intuição;
3. definição formal;
4. exemplo numérico completamente calculado;
5. representação gráfica, quando útil;
6. interpretação substantiva;
7. erro comum ou limite.

Introduza e defina, sem saltos:

- variável resposta, preditor e unidade de análise;
- reta de regressão amostral;
- valor ajustado e resíduo;
- soma dos quadrados dos resíduos e EQM;
- estimador de mínimos quadrados;
- condições de primeira ordem;
- fórmulas da inclinação e do intercepto;
- interpretação da inclinação nas unidades originais;
- interpretação do intercepto e o problema de `X = 0` sem significado substantivo;
- centralização do preditor para produzir um intercepto interpretável;
- propriedades mecânicas dos resíduos com intercepto;
- associação linear, valores ajustados e limites de extrapolação;
- diferença entre o BLP populacional e a reta estimada em uma amostra.

Derive passo a passo:

\[
(\widehat\alpha,\widehat\beta)
=
\arg\min_{a,b}\sum_{i=1}^{n}(Y_i-a-bX_i)^2,
\]

e mostre como as condições de primeira ordem produzem:

\[
\widehat\beta
=
\frac{\sum_i(X_i-\bar X)(Y_i-\bar Y)}
     {\sum_i(X_i-\bar X)^2},
\qquad
\widehat\alpha=\bar Y-\widehat\beta\bar X.
\]

Inclua um exemplo pequeno, com todas as contas visíveis, que permita verificar manualmente a inclinação, o intercepto, os valores ajustados, os resíduos e a soma dos quadrados dos resíduos. Mostre graficamente por que outra reta produz erro quadrático maior.

Explique explicitamente:

- uma inclinação positiva descreve associação linear positiva, não efeito causal;
- `lm()` estima uma relação amostral; não fornece identificação causal por si só;
- nesta aula não se interpretam valores-p, testes ou intervalos de confiança;
- nesta aula não se ensinam ainda regressão múltipla, diagnóstico de resíduos, erros-padrão robustos ou propriedades amostrais do estimador.

Use “EQM” de forma consistente. Defina cada símbolo na primeira aparição, mantenha uma ideia principal por slide e formule perguntas adequadas à pós-graduação.

Todas as figuras e tabelas devem ser numeradas, ter legenda e informar unidade, denominador e fonte. Use português correto, UTF-8 e acentos.

Compile o PDF e inspecione visualmente todas as páginas: fórmulas, subscritos, chapéus, barras, títulos, tabelas, gráficos, cortes, sobreposições, tamanho do texto e sequência pedagógica.

## Frente 2 — laboratório em R

Continue o mesmo projeto cumulativo AGNA. Não crie um exercício desconectado.

A pergunta do laboratório é:

> Qual é a associação linear entre o ano da votação e a convergência média anual dos votos de Brasil e China na AGNU?

Trate o resultado como associação descritiva. Não interprete a inclinação como efeito do tempo, de 2009, do comércio ou da China.

Use somente `brasil_convergencia_china_1997_2016.csv`. Comece na unidade resolução e agregue explicitamente para a unidade **ano**, mostrando que o banco analítico passa a ter 20 observações. Em cada ano, mantenha:

- número de resoluções válidas;
- número de votos convergentes;
- taxa média de convergência.

Crie um preditor centralizado, como `anos_desde_1997 = ano - 1997`, para que o intercepto represente a convergência prevista em 1997. Explique por que usar o ano bruto tornaria o intercepto matematicamente válido, mas substantivamente pouco útil.

O laboratório deve conter:

1. leitura e validação simples da base;
2. agregação transparente por ano;
3. gráfico de dispersão mínimo;
4. cálculo manual da inclinação e do intercepto com covariância e variância;
5. estimação do mesmo modelo com `lm()`;
6. comparação numérica entre os coeficientes manuais e os de `lm()`;
7. valores ajustados e resíduos apenas para verificar a construção da reta, sem antecipar diagnóstico;
8. gráfico construído em camadas com a reta ajustada;
9. interpretação da inclinação em pontos percentuais por ano;
10. exercício curto e discussão dos limites.

Não use `summary(modelo)` como atalho para ensinar inferência. Nesta aula, extraia e interprete somente coeficientes, valores ajustados, resíduos necessários à demonstração e medidas descritivas do ajuste que tenham sido apresentadas nos slides. Não interprete valores-p, erros-padrão ou intervalos de confiança.

### Simplicidade do script

O código será projetado ao vivo. Portanto:

- mantenha uma sequência curta, linear e comentada;
- carregue normalmente `data.table`, `dplyr`, `ggplot2` e `here`;
- deixe mensagens e warnings visíveis;
- leia diretamente o único banco com `fread(here(...))`;
- use sempre `dplyr::select()` ao selecionar colunas;
- não use funções auxiliares, loops, busca automática de arquivos ou programação defensiva extensa;
- não use `suppressPackageStartupMessages()`, `warning = FALSE`, `options(warn = -1)` ou equivalentes;
- dê nomes claros em português aos objetos;
- explique o propósito de cada objeto novo antes de criá-lo;
- faça uma transformação por etapa e mostre o resultado intermediário;
- numere e forneça legenda para todas as tabelas e figuras.

Construa o gráfico progressivamente:

1. `grafico_basico`: pontos com ano no eixo horizontal e convergência média no vertical;
2. `grafico_com_reta`: adição da reta estimada;
3. `grafico_com_rotulos`: título, subtítulo, eixos, fonte e nota de interpretação;
4. `grafico_final`: tema visual simples e escalas legíveis.

Explique o que cada camada acrescenta. Não inclua personalizações sem função didática ou substantiva.

Siga aproximadamente este roteiro:

1. objetivo substantivo;
2. carregamento dos pacotes;
3. leitura da base;
4. unidade de análise, ausências, datas e regras lógicas;
5. agregação por ano e mudança explícita da unidade de análise;
6. gráfico básico;
7. cálculo manual do MQO;
8. estimação com `lm()` e verificação dos coeficientes;
9. construção incremental do gráfico final;
10. interpretação substantiva;
11. exercício;
12. limites sem alegação causal.

O exercício pode pedir que os alunos recentralizem o ano em 2009 e expliquem:

- por que a inclinação permanece igual;
- por que o intercepto muda;
- o que o novo intercepto representa;
- por que nenhuma das duas parametrizações identifica um efeito causal de 2009.

## Validação e entrega

Teste os materiais sem aumentar artificialmente a complexidade do script didático.

Entregue:

- o RMarkdown dos slides;
- o PDF Beamer compilado;
- o script R simples do laboratório;
- somente os arquivos adicionais estritamente necessários;
- uma nota curta com tema, leituras, arquivos, fluxo do laboratório, testes, warnings, limitações e divergências encontradas.

Valide pelo menos:

- dimensões e unicidade da base original;
- datas entre 1997 e 2016;
- ausência de valores incompatíveis;
- exatamente 20 linhas na base anual;
- soma dos denominadores anuais igual ao total de resoluções utilizadas;
- taxa anual entre zero e um;
- igualdade, com tolerância numérica, entre os coeficientes manuais e os de `lm()`;
- construção bem-sucedida do gráfico final;
- inexistência de `Rplots.pdf` ou outputs acidentais.

Exija uma revisão independente final com veredito `PASS` ou `FAIL` para:

- fidelidade ao syllabus e às leituras;
- correção da derivação de MQO;
- conexão correta entre BLP e MQO amostral;
- interpretação das unidades dos coeficientes;
- separação entre associação e causalidade;
- ausência de inferência antecipada;
- simplicidade e legibilidade do script;
- numeração e legenda de tabelas e figuras;
- integridade dos materiais anteriores.

Não finalize enquanto houver um `FAIL` material.
