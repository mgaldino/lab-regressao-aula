# Revisão pedagógica da Lista 1

Data: 05/09/2026. Skill aplicada: `pedagogy-review`, com suas dimensões adaptadas de slides para uma lista. O pedido explícito do professor de realizar ajustes prevalece sobre a orientação genérica da skill de apenas emitir parecer.

## Diagnóstico e escolhas

A lista anterior dedica 21 páginas a orientações de instalação e RMarkdown e três páginas a sete exercícios municipais. Seu núcleo de importação, manipulação, resumo e gráficos é útil, mas deixa sem prática explícita conteúdos já ensinados até a Aula 3. A revisão preserva esse núcleo em quatro exercícios, acrescenta cinco exercícios cumulativos usando os dados do laboratório e separa o exemplo não linear em uma questão própria.

Tabela 1. Avaliação pedagógica qualitativa da lista anterior e da revisão.

| Dimensão | Anterior | Revisada | Ajuste e limite |
|---|---|---|---|
| Narrativa | C | A | Pergunta comum: como descrição e informação ajudam a prever? Dois contextos: municípios e AGNU. |
| Notação | B | A | Variáveis, CEF, BLP e EQM definidos antes do uso; distingue função, valor e variável aleatória; denominadores n e n-1 explícitos. |
| Ritmo | C | B | Três sessões sugeridas; 9 exercícios centrais. Tempos de 90–120 minutos por sessão são estimativas editoriais, sem cronometragem com estudantes. |
| Progressão | B | A | Inspecionar, calcular, interpretar e comparar; exemplo binário antes do caso não linear; dica para a demonstração da CEF. |
| Participação | C | A | Antecipar resultados antes de executar, explicar erros, escolher medidas, escrever síntese com denominadores e limites. |

As notas são juízo editorial, não medidas de aprendizagem ou resultado de teste com alunos.

## Cobertura e sequência

Tabela 2. Conteúdo efetivamente usado para ajustar o escopo.

| Fonte | Conteúdo | Exercícios revisados |
|---|---|---|
| Syllabus, Aula 1, 19/08 | Probabilidade e introdução à simulação em R | 5b–e: conjunta, marginais, condicionais e Bernoulli com semente. |
| Teoria e laboratório da Aula 2, 26/08 | Esperança, momentos, variância, escala, EQM, covariância/correlação | 2, 3, 5d, 6c, 7a e 8b. |
| Slides e script da Aula 3, 02/09 | CEF, esperança iterada, otimalidade sob EQM e BLP | 4, 6, 7 e 8; o exercício 9 aplica a informação temática. |
| Lista anterior, pp. 22–24 | Importação, SP, momentos, gráficos, escala logarítmica e médias por UF | Preservados e condensados em 1–4. |

Mapa de ritmo: sessão 1 (1–4) = dados e descrição; sessão 2 (5–6) = probabilidades e CEF; sessão 3 (7–9) = BLP, exemplo não linear, informação temática e síntese. O item 6e é o principal aumento de abstração e contém dica de condicionamento. A questão 8 constrói em etapas a distinção entre covariância zero e independência e termina comparando os desvios do BLP com os da CEF.

Não são exigidos MQO com `lm()`, testes, erros-padrão, regressão múltipla ou identificação causal. O lembrete de que descrição não identifica causalidade acompanha o material da Aula 3. O menor EQM por tema e período é atribuído a informação adicional; não é evidência isolada de não linearidade.

## Principais melhorias

1. Transferir o extenso tutorial de instalação para material de apoio e tornar os objetivos e produtos de cada etapa visíveis.
2. Fazer o aluno prever e justificar os resultados, mantendo a continuidade com os dados e comandos das aulas.
3. Fechar a diferença entre o conteúdo ensinado e o praticado, com aprofundamentos opcionais para controlar a carga.

## Validação e dados

`validar_lista_01.R` passou em 29 verificações: dimensões, chaves, valores inteiros positivos, acentos, datas e períodos, suporte binário, igualdade de votos, momentos, simulação, esperança iterada, junções, identidade CEF–BLP, ordenação de EQMs, decomposições e exemplos discretos exatos. As bases têm 5.570 municípios e 1.762 votações; SP tem 645 municípios. Os 353 ausentes em `voto_importante` são documentados e não causam exclusão de linhas, pois a variável não entra na análise.

O verificador usa leitura base R independente do `fread()` recomendado ao aluno. Uma execução com locale inválido/C falhou na conversão Latin-1 do arquivo municipal; a repetição com `LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8` passou. O comando reproduzível abaixo explicita essa condição. O aviso de pacote dplyr compilado sob R 4.4.3 não impediu a execução.

As oito páginas do PDF foram renderizadas e inspecionadas. Corrigido um nome de arquivo que excedia a margem na página inicial. Não houve ensaio com estudantes, avaliação de tempo real, distribuição no Moodle ou alteração do tutor.

## Fontes locais

- Raiz do curso: `Lista 1 MÃ©todos II.pdf`, pp. 22–24; o original permanece intacto.
- `2026/lab-regressao-aula/syllabus.Rmd`, calendário das Aulas 1–3.
- `2026/worktrees/aula2_teoria/materials_aula_02/aula_02_teoria_distribuicoes.Rmd`.
- `2026/lab-regressao-aula/projeto_agna/laboratorios/slides_aula_02_convergencia_brasil_china.Rmd`.
- `2026/lab-regressao-aula/projeto_agna/laboratorios/slides_aula_03_cef_blp.Rmd`.
- `2026/lab-regressao-aula/projeto_agna/scripts/04_laboratorio_aula3.R`.
- Dados municipais copiados de `2026/agentes_mvp/dados/populacao_municipios_2020.csv`; recorte AGNU copiado da área de dados processados do projeto AGNA, com igualdade de bytes conferida. Sem nova coleta externa.

## Reproduzir

Da raiz `2026/lab-regressao-aula`, com R, dplyr, rmarkdown, Pandoc e XeLaTeX:

```sh
LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 OMP_NUM_THREADS=1 Rscript --vanilla listas/lista_01/docente/validar_lista_01.R
LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8 OMP_NUM_THREADS=1 Rscript --vanilla listas/lista_01/docente/renderizar_lista_01.R
```

O diretório docente contém resultados de conferência e não integra o pacote estudantil. O pacote de alunos contém somente enunciado PDF e bases originais. Fonte editável e scripts permanecem na área de trabalho; esta raiz não é repositório Git e não foi criado commit.
