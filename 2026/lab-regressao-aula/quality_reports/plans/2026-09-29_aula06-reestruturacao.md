# Plano: Aula 6 — reestruturação para 2 h de teoria e 1h30 de laboratório

**Status**: COMPLETED (29/09/2026: slides de 21 páginas e laboratório de 90 min; comércio em escala original recuperado do repositório RDD Trade; revisão independente adjudicada e corrigida)
**Data**: 2026-09-29
**Aula**: 30/09/2026
**Parecer de base**: `quality_reports/2026-09-29_revisao-aula06-slides.md`
**Versão do Codex preservada**: commit `a97ec95` (proposta: tag `aula06-v1`)

## Objetivo

Reescrever os slides da Aula 6 com a teoria das leituras (Hansen 2.21–2.25) e cerca de 21 páginas, e ajustar o laboratório para 1h30.

## Abordagem

1. Tag `aula06-v1` no commit `a97ec95` (feito).
2. Slides, na ordem da tabela "Proposta: 21 páginas" do parecer:
   - BLP com dois preditores → equações normais como plug-in → interpretação condicional;
   - decomposição do coeficiente, com derivação e o exemplo de quatro unidades;
   - regressão curta e longa: fórmula, derivação, sinais e aplicação;
   - categórica com k níveis: continente, médias por grupo, armadilha das indicadoras, `relevel()`;
   - especificações M1–M4 (opcional), `lm()`, laboratório, síntese e referências.
3. Números calculados no próprio Rmd, como na Aula 5, sem `source()` do roteiro dos alunos.
4. Regras do professor: sem avisos causais, títulos nominais, notação da Aula 5 ($\alpha$, $\beta_j$, $\varepsilon$, $\widehat e_i$, til nos candidatos), sem "AGNA" e sem `synth_data.rds` visível, "variável resposta".
5. Laboratório com as cinco atividades do parecer (10 + 20 + 20 + 20 + 20 min), sem `n_pares_validos` e sem a pergunta causal.

## Decisões pendentes do professor

- Aplicação do viés de variável omitida nos slides: Brasil–China por resolução (ano + indicadora de conflito palestino, em p.p.) ou painel de países (comércio + hiato de poder, em DP).
- Existe a série original de participação no comércio com a China? Se sim, os coeficientes passam a ter unidade substantiva.
- Definição exata da amostra de 96 países (Brasil + 95 doadores em que a China nunca foi o principal destino comercial em 1997–2016).

## Arquivos a modificar

- [x] `projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd` — reescrita
- [x] `projeto_agna/output/pdf/slides_aula_06_regressao_multipla.pdf` — recompilar
- [x] `projeto_agna/scripts/09_laboratorio_aula6_regressao_multipla.R` — atividades novas
- [x] `projeto_agna/scripts/10_validar_aula6_regressao_multipla.R` — validar os números novos
- [x] `projeto_agna/output/aula_06/nota_entrega_aula_06.md` — registrar a revisão

## Verificação

- [x] Compilar com XeLaTeX (`LC_ALL=pt_BR.UTF-8`); contar páginas (meta: 20–22).
- [x] Inspecionar todas as páginas renderizadas (cortes, rótulos, legibilidade).
- [x] `grep` por causal/identifica/confundidor/associacional/AGNA/synth nos textos visíveis: zero ocorrências.
- [x] Números: 0,309 / 0,448 / 23,2 / −0,0060; 0,80 / 1,66 / 5,01 / −0,171; resíduos (0; −0,5; 0,5; 0) e $\widehat\beta_1=2$; médias por continente.
- [x] Rodar o laboratório e a validação sem gerar `Rplots.pdf`.
