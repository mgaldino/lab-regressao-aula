# Revisão independente: Aula 6 reescrita (21 páginas)

- **Revisor**: subagente independente (general-purpose), 29/09/2026.
- **Artefato**: `projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd` (SHA-256 `4fd0f98258af11b55d2014976e82c49fc57ba3788f0e77b76a8b86c3abdde5e6`) e PDF compilado de 21 páginas (SHA-256 `b8df5f71fb2d9aa6683b4ce1e09d2aac05baa5f9fa8a954813f27c3481bbc7b5`).
- **Parecer transcrito na íntegra.**

## FAIL

1. p13, caption ≠ plot. Caption says −0,60 p.p./ano (=100·δ̂); the plotted geom_smooth is an unweighted fit to 20 annual shares, slope −0,57. Fix: add resolucoes = dplyr::n() to agenda_anual and aes(weight = resolucoes) in geom_smooth (→ −0,60). Nonbreaking space before "p.p.".
2. pp15–16, rounding. Rounded means contradict coefficients: 65,9−70,0=−4,1 (slide −4,2); +4,1 (+4,2); 65,3−65,9=−0,6 (−0,5); 50,0−65,9=−15,9 (−15,8). Fix: two decimals (70,02/65,86/65,33/50,03; −4,16/−4,70/−19,99; 65,86/+4,16/−0,53/−15,83) plus note "diferenças calculadas sem arredondamento".
3. p10, gap. "5 = 2 + 3×1" needs δ̂=1, never shown; p9 shows only X̂1 = 0,5X2 (plugging 0,5 gives 3,5). Fix: "No exemplo, X̂2 = 0,5 + X1, logo δ̂ = 1."
4. "Regressão auxiliar" names two regressions: X1 em X2 (pp7, 9) and X2 em X1 (pp12, 18, 19). Fix: pp7/9 → "Regressão de X1 em X2".
5. p19 item 4. "comparar com a regressão longa" points to item 3's model (exportações + Europa, −0,129); the continent residual reproduces M2 (−0,112). Fix: "comparar com M2 (exportações e continente)".
6. p20 item 4. k already counts predictors (p3); "Uma categoria com k níveis" is wrong; no holding-fixed clause. Fix: "Um preditor categórico com G categorias entra com G−1 indicadoras; cada coeficiente é a diferença de valor ajustado em relação à referência, com os demais preditores fixos."
7. p14 footnote, causal language: "(grupo de comparação de um desenho de controle sintético)". Fix: delete; add "e com série completa" (PROVENIENCIA).
8. p12, unit and X2. Rows are roll calls (1.762 rcid; 1.467 resolution symbols); 22% lack a topic code, so X2=0 ≠ "não trata". Fix: "votação nominal (1.762 votações)"; "X2i: 1 quando a votação tem o tema Palestinian conflict no unvotes".

## FAIL (minor)

- p10: qualify "a identidade é exata": "com intercepto nas três regressões e as mesmas observações".
- p6: "aditivo" explains only invariance in x2 → "linear em X1 e sem interação".
- p17: three omitted indicators need γ̂1 = β̂1 + Σg β̂g δ̂g; add one line (p3 claims "qualquer k").
- p5: define bold Y, X1, X2 (bold X was an n-vector in Aula 5).
- pp4, 10: "/\operatorname{V}" adds a thin space; use {\operatorname{V}}.
- p17 figure: Américas/Ásia lines (0,5 p.p. apart) overlap; vary linetype.
- p14: new Y defined only in footnote.
- Goldberger (1991) absent from Referências; "país–ano" → "país-ano"; "sobre" → "em" (pp19–20).
- Aula 3 used δk for indicator coefficients; p15 cites Aula 3 while δ now is the auxiliary slope.

## PASS

- BLP FOCs, uniqueness, normal equations, (X'X)⁻¹X'Y, rank condition.
- FWL and 3-step derivation.
- Four-unit example: r̂=(0; −0,5; 0,5; 0), β̂1=1/0,5=2, short slope 5, 5=2+3·1.
- Short/long derivation; sign table.
- Categorical: reference, saturation, trap (NA for europa), relevel keeps fitted values.
- All other numbers reproduce; committed PDF matches current Rmd.
- Titles nominal, ≤5 words; no R², AGNA, listed causal words, strawman negations.
- Decimal commas; no overfull boxes or clipping.
- Hansen 2.23/2.24/3.18, syllabus Aulas 9–11, lab items/Tabela 3 verified.
- Pacing: 21 pages ≈ 11 pages/h target; no overloaded slide.
