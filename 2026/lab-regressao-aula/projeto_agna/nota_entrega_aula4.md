# Nota de entrega — Aula 4 revisada

## Decisão pedagógica

A aula foi reorganizada em torno de uma única pergunta: **como as unidades chegaram ao grupo tratado e ao grupo de controle?** O corpo principal deixou de começar por descrição, associação, previsão, SUTVA, ignorabilidade e DAGs. A sequência agora é:

1. três correlações que convidam uma interpretação causal: tênis e longevidade, sobrenome compartilhado e divórcio, contato de campanha e apoio eleitoral;
2. tirinha “Correlation”, do xkcd, como pausa visual entre a associação observada e a pergunta sobre seu mecanismo;
3. pergunta sobre como os grupos foram formados, ainda sem vocabulário técnico;
4. tabela de terminologia para `X`/`D` e `Y`, seguida da escolha de “variável de tratamento” e “variável resposta” para a aula;
5. autoseleção, seleção por terceiros e agência orientada por consequências esperadas;
6. resultados potenciais, diferença entre potencial e grupo observado, quatro médias, resultado observado e problema fundamental da inferência causal;
7. somente depois dessa base, discussão de Imai sobre efeitos causais de características imutáveis e quatro estratégias de reformulação: percepção, reinterpretação, redefinição e intervenção no grupo;
8. aplicação dessa distinção ao tema da resolução, separando rótulo percebido, enquadramento textual e categoria substantiva;
9. contraste entre “direitos humanos versus desarmamento” e um estimando de enquadramento que fixa unidade, versões da intervenção, variável resposta, população e momento;
10. efeito individual, ATE e ATT;
11. exemplo da Médica Perfeita e do Médico Ruim, depois da introdução da notação;
12. decomposição da Diferença Simples de Médias (SDO) em ATT e viés de seleção;
13. atribuição aleatória como mecanismo de seleção independente dos resultados potenciais;
14. retorno ao AGNA e diagnóstico da SDO descritiva entre períodos.

O PDF contém 46 páginas: 41 páginas no fluxo principal, incluindo capa e referências, e cinco páginas de apêndice. A decomposição centrada no ATE, independência condicional, consistência e variáveis posteriores ao tratamento foram preservadas no apêndice.

## Fontes verificadas dos exemplos

- ABC News (2018), *Tennis tops list of sports for increasing life expectancy*: a matéria apresenta a associação de 9,7 anos e a explicação baseada em contato social, mas também registra que o estudo observacional não demonstra causa e efeito. O slide usa um print do cabeçalho da matéria e mantém Schnohr et al. (2018) como fonte do estudo.
- Polymarket (2026), post no X indicado pelo docente: o post afirma que casais com sobrenomes separados teriam cerca de 50% mais divórcios e se separariam aproximadamente 30% antes.
- Stone (2026), *Can Sharing a Last Name Save Your Marriage? It Depends*: a análise do Institute for Family Studies que sustenta os números do post usa dados de divórcios do Texas e a pesquisa Pew de 2023. O próprio autor descreve grandes vieses de seleção, reconhece que a escolha do sobrenome está longe de ser aleatória e não apresenta a associação como efeito causal identificado.
- Munroe (2009), xkcd nº 552, *Correlation*: a tirinha foi incorporada integralmente em um slide de transição e creditada sob a licença CC BY-NC 2.5.
- Arceneaux (2007), *I'm Asking for Your Support*: o artigo explica que campanhas direcionam contatos a apoiadores potenciais e usa um experimento aleatório para separar efeito do contato e direcionamento estratégico.
- Cunningham, *Causal Inference: The Remix*, capítulo 4: valores da Médica Perfeita, Médico Ruim, decomposição e interpretação da atribuição independente dos resultados potenciais.
- Imai (2021), *Potential Outcomes*, slide 8: a discussão sobre efeitos causais de características imutáveis e as estratégias de característica percebida, reinterpretação, redefinição e intervenção no grupo.
- Holland (1986), *Statistics and Causal Inference*: referência para o problema fundamental da inferência causal, agora nomeado explicitamente no fluxo principal.
- Galdino, *Regression for Social Scientists*, slide “Terminology”: tabela autoral reutilizada para apresentar os nomes de `X`/`D` e `Y`; a aula escolhe “variável de tratamento” e “variável resposta”.

Consultas online realizadas em 16/09/2026:

- <https://pubmed.ncbi.nlm.nih.gov/30193744/>
- <https://www.abc.net.au/news/health/2018-12-08/tennis-tops-list-of-sports-for-increasing-life-expectancy/10459480>
- <https://x.com/Polymarket/status/2099882730271961547>
- <https://ifstudies.org/blog/can-sharing-a-last-name-save-your-marriage-it-depends>
- <https://xkcd.com/552/>
- <https://isps.yale.edu/resource/im-asking-for-your-support-the-effects-of-personally-delivered-campaign-messages-on-voting>
- <https://mixtape.scunning.com/04-potential_outcomes_and_randomization>
- <https://imai.fas.harvard.edu/teaching-files/potential_outcomes.pdf>
- <https://github.com/mgaldino/regression-summer-ipsa>

## Arquivos principais

- `laboratorios/slides_aula_04_causalidade_identificacao.Rmd`
- `output/pdf/slides_aula_04_causalidade_identificacao.pdf`
- `scripts/05_laboratorio_aula4_causalidade.R`
- `assets/aula_04/abc_tennis_headline.png`
- `assets/aula_04/polymarket_sobrenome_tweet.png`
- `assets/aula_04/xkcd_correlation.png`

Os dois exemplos reais da motivação usam prints das fontes públicas. O exemplo fictício de contato de campanha permanece esquemático, sem print, para preservar a distinção entre evidência observada e ilustração didática. A tirinha do xkcd aparece depois da comparação dos três exemplos e antes da pergunta sobre como os grupos foram formados.

O laboratório gera ainda:

- `output/aula_04/pacientes_resultados_potenciais.csv`
- `output/aula_04/resumo_mecanismos_selecao.csv`
- `output/aula_04/atribuicoes_aleatorias.csv`
- `output/aula_04/diagnostico_intervencao_agna.csv`
- `output/aula_04/resumo_agna_periodos.csv`
- `output/aula_04/distribuicao_diferencas_aleatorias.pdf`

## Fluxo do laboratório

1. Constrói a tabela-oráculo de dez pacientes e calcula `tau`, ATE, ATT e ATU.
2. Aplica a regra da Médica Perfeita, `D = 1` quando `Y(1) > Y(0)`.
3. Verifica numericamente a identidade `-0,4 = 4,4 - 4,8`.
4. Realiza um sorteio completo com cinco pacientes tratados.
5. Enumera todas as 252 atribuições possíveis e mostra que a SDO média é 0,6, exatamente o ATE, enquanto os componentes médios de viés são zero.
6. Constrói um exemplo de ciência política no qual a campanha contata apoiadores prováveis. A SDO é 5, o ATT é 1 e o viés de seleção é 4.
7. Retorna ao AGNA para comparar duas formulações: “direitos humanos versus desarmamento”, que não define o que permanece fixo, e o enquadramento da mesma proposta, que define um estimando, mas não é identificado pelo banco atual.
8. Propõe uma atividade em que os estudantes escolhem uma das estratégias de Imai e precisam explicitar unidade, duas versões da intervenção, variável resposta, população-alvo, momento e resultados potenciais.
9. Executa sete validações lógicas no banco e recupera a SDO descritiva de 3,94 pontos percentuais.

## Verificações executadas

- Script R: execução integral atual `PASS` com `set.seed(20260916)` e verificações `stopifnot()` para dimensões, ausências, duplicidades, identidades causais, enumeração das 252 atribuições, duas formulações de intervenção e resultados AGNA.
- Médica Perfeita: ATE `0,6`, ATT `4,4`, SDO `-0,4` e viés de seleção `-4,8`.
- Atribuição aleatória: médias sobre as 252 atribuições iguais a ATE `0,6`, ATT `0,6`, ATU `0,6`, viés de seleção `0` e viés de heterogeneidade `0`.
- Campanha: ATE `1`, ATT `1`, SDO `5` e viés de seleção `4`.
- AGNA: sete validações com zero problema; 1.080 resoluções em 1997–2008, 682 em 2009–2016 e diferença de 3,94 pontos percentuais.
- Slides: recompilação Beamer/XeLaTeX `PASS`; PDF 16:9 com 46 páginas. Os slides 3 e 4 incorporam os dois prints, o slide 5 fictício permanece sem imagem e o slide 7 usa a tirinha do xkcd.
- Integridade do PDF: arquivo não criptografado, 46 caixas de página idênticas, texto extraível em todas as páginas e nenhuma página vazia.
- Inspeção visual: todas as 46 páginas examinadas em contact sheet; os slides 9, 18, 27, 28, 36 e 37 também foram revisados individualmente em resolução ampliada. Não foram encontrados cortes, sobreposições, fontes ausentes ou fórmulas quebradas. No slide 28, somente os dois termos somado e subtraído aparecem em azul; o último termo permanece preto.
- Warnings não materiais: `dplyr` e `ggplot2` foram compilados sob R 4.4.3; mensagens usuais de masking permaneceram visíveis. O Poppler emitiu aviso de cache do Fontconfig, mas produziu corretamente as imagens usadas na inspeção.

## Limites e estado do projeto

- Os exemplos da Médica Perfeita e da campanha são populações fictícias usadas para tornar observáveis os dois resultados potenciais. O script e os slides os identificam como hipotéticos.
- O banco AGNA não mede uma intervenção binária nem versões alternativas da mesma proposta. A formulação de um estimando de enquadramento esclarece a pergunta, mas não cria a estratégia de identificação; a diferença entre períodos permanece descritiva.
- O diretório ativo `2026/lab-regressao-aula` não contém metadados `.git`. A entrega foi versionada em um worktree dedicado do repositório `mgaldino/lab-regressao-aula`, no commit `3c47442a690286f1235a13466a9b694abb8f619b`, e enviada para a branch `codex/aula4-causalidade`.
- O PDF publicado está disponível em <https://raw.githubusercontent.com/mgaldino/lab-regressao-aula/refs/heads/codex/aula4-causalidade/projeto_agna/output/pdf/slides_aula_04_causalidade_identificacao.pdf>; a resposta HTTP `200` e o tamanho de `503371` bytes foram verificados depois do push.

## Integridade dos arquivos protegidos

Os arquivos do syllabus permaneceram idênticos aos hashes registrados antes da revisão:

```text
25688393b49d304dcf41d480627baf066f614a939e38cc0181abb51900116958  syllabus.Rmd
32e09fffd140f2401ecfd6c3ef765f945dca69798cdd8a169fa548208238a75b  syllabus.pdf
```

Hashes finais dos principais artefatos:

```text
b05afd9795bcbd34fc7c48a518063c0a072e097ab77a8e0b12c451281f041414  laboratorios/slides_aula_04_causalidade_identificacao.Rmd
480cab1e43581ec5164dec38bd034091c9ac884d534b8f01584012a0983477fd  output/pdf/slides_aula_04_causalidade_identificacao.pdf
a41037a9d09a91e1613362b1adc3e5dd1acf1e253d23e2cfac91f2ee9f4d1c44  scripts/05_laboratorio_aula4_causalidade.R
fea3977db8231a5c5e6faaa764508fcb387464ca68ef218544d194f615a875dc  assets/aula_04/abc_tennis_headline.png
f690f4ee911fc57566042de6dafadd6f01c06d7f89cc93718bb61464bbe82fce  assets/aula_04/polymarket_sobrenome_tweet.png
336875fda073fd7771546fce99da2efc1a182aca6c6160bc8fe78be8e2c4856d  assets/aula_04/xkcd_correlation.png
381ee6ab3694c9a91d5549141b9893682061b23641666a764f98c8da9fbad6a6  output/aula_04/diagnostico_intervencao_agna.csv
59a8b90a4fcbadd95fcfbd04935bec8929a4e8ca80f0a82492221de071b31932  output/aula_04/distribuicao_diferencas_aleatorias.pdf
```
