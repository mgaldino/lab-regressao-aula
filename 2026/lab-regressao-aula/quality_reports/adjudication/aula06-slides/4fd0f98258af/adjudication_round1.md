# Adjudicação, rodada 1: slides da Aula 6 reescritos

## 1. Identidade

- Artefato: `projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd`, SHA-256 `4fd0f98258af11b55d2014976e82c49fc57ba3788f0e77b76a8b86c3abdde5e6`; PDF de 21 páginas compilado dessa versão.
- Parecer: `quality_reports/2026-09-29_revisao-independente-aula06-v2.md` (revisor independente, R1).
- Contrato argumentativo: não exigido (deck de aula).
- Registro JSON: `adjudication_round1.json` (validado).

## 2. Disposição

18 achados (o item 8 do parecer foi separado em dois): 15 confirmados, 3 parciais, 0 refutados, 0 não resolvidos, 1 decisão reservada ao professor. Veredicto: **READY_FOR_IMPLEMENTATION**.

## 3. Achados

| ID | Local | Achado | Status | Correção |
|---|---|---|---|---|
| F001 | p. 13 | Legenda diz −0,60 p.p./ano; a reta desenhada (não ponderada) tem −0,57 | CONFIRMED | segura: ponderar pelo nº de votações/ano |
| F002 | pp. 15–16 | Médias com uma casa não reproduzem os coeficientes (−4,1 vs −4,2) | CONFIRMED | segura: duas casas |
| F003 | p. 10 | $5=2+3\times1$ usa $\widehat\delta=1$, que não aparece | CONFIRMED | segura |
| F004 | pp. 7–9 | "Regressão auxiliar" nomeia X1 em X2 e X2 em X1 | CONFIRMED | segura |
| F005 | p. 19 | Item 4 manda comparar com a regressão da parte 3; o certo é M2 | CONFIRMED | segura |
| F006 | p. 20 | $k$ com dois sentidos; falta "demais preditores fixos" | CONFIRMED | segura |
| F007 | p. 14 | Parêntese sobre controle sintético | PARTIAL | segura: remover e acrescentar "série completa" |
| F008 | p. 12 | Linhas são votações nominais, não resoluções | CONFIRMED | **decisão do professor**: manter a convenção do syllabus |
| F009 | p. 12 | $X_2=0$ inclui votações sem tema (22,3%) | CONFIRMED | segura |
| F010 | p. 10 | Identidade amostral exige intercepto e mesma amostra | CONFIRMED | segura |
| F011 | p. 6 | "Aditivo" explica só a invariância em $x_2$ | CONFIRMED | segura |
| F012 | p. 17 | Falta a fórmula com três indicadoras omitidas | PARTIAL | segura: uma linha |
| F013 | p. 5 | Vetores em negrito sem definição | CONFIRMED | segura |
| F014 | pp. 4, 10 | Espaço fino depois de "/" | CONFIRMED | segura |
| F015 | p. 17 | Retas de Américas e Ásia sobrepostas | CONFIRMED | segura |
| F016 | p. 14 | Novo $Y$ só na nota | CONFIRMED | segura |
| F017 | pp. 10, 19–21 | Goldberger fora das referências; "país–ano"; "sobre"/"em" | CONFIRMED | segura |
| F018 | p. 15 | Aula 3 usava $\delta_k$ para indicadoras | PARTIAL | sem alteração |

## 4. Evidência

- F001: `lm(share ~ ano)` não ponderado −0,569; ponderado pelo nº de votações −0,600; $100\widehat\delta$ na base por resolução −0,600.
- F002: médias 70,02/65,86/65,33/50,03; diferenças exatas −4,16/−4,70/−19,99.
- F003: `lm(x2 ~ x1)` no exemplo: $\widehat X_2=0{,}5+1\cdot X_1$.
- F008/F009: 1.762 rcid; 1.468 símbolos distintos (6 vazios); 22,3% sem codificação temática.
- F012: $0{,}1873=-0{,}1121+0{,}2994$, com $0{,}2994=\sum_g\widehat\beta_g\widehat\delta_g$.
- F018: Aula 3, linha 825: $g_{\mathrm{sat}}(X)=\alpha+\sum_k\delta_k\mathbf 1\{X=x_k\}$; a p. 15 da Aula 6 cita o conceito, sem o símbolo.
- F007 (parte refutada): o parêntese descreve a origem da amostra; não é aviso sobre interpretação causal.

## 5. Decisões reservadas e correções inseguras

- F008: chamar cada votação nominal de "resolução" é a convenção do syllabus ("a unidade de análise será a resolução votada") e das Aulas 2–5. Fica como está; registrado para o professor.
- Nenhuma correção proposta foi classificada como insegura.

## 6. Não resolvidos

Nenhum.

## 7. Veredicto

READY_FOR_IMPLEMENTATION: aplicar F001–F007, F009–F017; manter F008 e F018.

## 8. Implementação (29/09/2026)

Aplicados F001–F007 e F009–F017; mantidos F008 (convenção do syllabus) e F018 (sem alteração).

- Novo Rmd: SHA-256 `a12d2ac92dd2e5b49d25541068d484870f42cb26ca42dd341c7035c92f92f655`; novo PDF (21 páginas): `5b8bcbe13a9a150953ec85f30cf789d2a0ad703a1152fc48472eb2016a20ad4b`.
- Verificação mecânica: `stopifnot()` no Rmd confere que a reta anual ponderada reproduz $100\widehat\delta$ (F001) e que $\widehat\gamma_1=\widehat\beta_1+\sum_g\widehat\beta_g\widehat\delta_g$ (F012); busca no texto do PDF encontrou cada correção; inspeção visual das páginas 4–21 renderizadas; `10_validar_aula6_regressao_multipla.R` retorna `VALIDACAO_AULA6_OK`; nenhuma ocorrência de termos proibidos no PDF.
- A verificação pós-implementação foi feita pelo implementador, sem segunda revisão independente.
