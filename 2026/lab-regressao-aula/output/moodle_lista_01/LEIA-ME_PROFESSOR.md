# Lista 1: o que incluir no Moodle

## Material pronto

O diretório `publicar/` contém os três anexos da atividade:

1. **lista_01_revisada_2026.pdf**: enunciado, 9 exercícios centrais, conteúdo até a Aula 3.
2. **lista_01_kit_alunos.zip**: enunciado, orientações, bases, projeto R, modelo de relatório e scripts sem soluções. Este ZIP substitui, para publicação, o pacote básico produzido anteriormente.
3. **orientacoes_lista_01.pdf**: guia de início, geração de PDF, entrega e dicionário das variáveis. Também está dentro do kit, mas a cópia avulsa permite ler antes do download.

Os dois PDFs aparecem também no kit, de propósito: o aluno pode baixar um único arquivo e trabalhar sem buscar anexos em outros lugares.

Use o título **Lista 1 - dos dados à previsão (Aulas 1 a 3)** para a atividade de entrega.

## Textos para copiar

- `textos_moodle/descricao_atividade.md`: texto legível da descrição da atividade.
- `textos_moodle/descricao_atividade.html`: mesmo texto como fragmento HTML, para colar no modo HTML do editor Moodle, se disponível. Não é um arquivo para importar um curso nem contém links locais.
- `textos_moodle/instrucoes_envio.txt`: lembrete para o campo de instruções da submissão, quando disponível, ou para acrescentar à descrição.
- `textos_moodle/aviso_divulgacao.txt`: texto pronto de aviso. Confira datas e modalidade antes de publicar o aviso. Nenhuma mensagem foi enviada.

## Configuração da atividade

Use uma atividade **Tarefa**, com envio de arquivos. Anexe os três arquivos de `publicar/` como materiais de apoio/adicionais. O local e a tradução dos campos podem variar conforme a versão instalada. O roteiro foi conferido com a documentação oficial do Moodle; não foi testado na instância USP.

Para acomodar PDF, fonte e scripts auxiliares, permita ao menos três arquivos (sugestão: dez). Se restringir extensões, inclua `.pdf`, `.rmd`, `.qmd`, `.r` e, se desejar receber scripts agrupados, `.zip`. Um limite de 20 MB por arquivo costuma acomodar relatórios com figuras; confira o limite permitido pelo curso e ajuste ao material efetivamente produzido.

Antes de tornar a atividade visível, preencha conforme sua decisão:

- prazo e horário de entrega;
- modalidade individual ou em grupo e, neste último caso, o agrupamento correto;
- nota máxima, categoria e peso da Lista 1 no conjunto de listas;
- política de atraso e eventual data de encerramento do envio.

Essas decisões não foram inventadas. O syllabus atribui 50% da avaliação ao **conjunto de listas**, mas não define o peso desta lista nem sua data ou modalidade. O prazo normal e a data de encerramento são campos distintos; só defina encerramento se quiser impedir envios depois dessa data. Use as regras já anunciadas à turma para tentativas e confirmação final do envio.

Os critérios qualitativos da descrição refletem o enunciado: validação, cálculos, interpretação e reprodução. Não foi criada uma rubrica com pontuação nova.

## Tutor: apoio complementar

`apoio_tutor/guia_tutor_acesso_gemini.pdf` é a cópia do guia preparado na etapa anterior. Ele ainda remete ao código divulgado no Moodle, porque o valor não foi informado. Publique o código real em texto restrito à turma ou peça sua inclusão no PDF antes de divulgar esse apoio.

O tutor não é necessário para fazer ou entregar a lista. O guia registra o escopo documentado do piloto de importação e a compatibilidade restrita do conector Gemini. O projeto do tutor não foi alterado. Esses pontos não impedem a publicação dos três arquivos centrais da lista.

## Conferência como aluno

Abra a atividade na visualização de aluno. Confira acesso aos anexos, prazo, modalidade e formatos permitidos. Baixe o ZIP, extraia e confira o enunciado, as duas bases e o projeto. Um teste de compilação do modelo foi feito localmente em pasta isolada; ele confere a infraestrutura do modelo, não resolve os exercícios.

Não publique a pasta `docente/` do projeto nem os resultados de validação como material de alunos. O ZIP estudantil foi montado com uma lista explícita de arquivos e não contém soluções. Este pacote completo é para sua organização: para os alunos, publique somente os três anexos de `publicar/` e, quando desejar, o apoio do tutor.

## Fontes técnicas consultadas em 05/09/2026

- [Moodle: Assignment settings](https://docs.moodle.org/en/Assignment_settings).
- [R Markdown: Installation](https://bookdown.org/yihui/rmarkdown/installation.html).
- [TinyTeX](https://yihui.org/tinytex/).

## Reprodução do pacote

A fonte está em `listas/lista_01/moodle/`. Execute `python3 listas/lista_01/moodle/scripts/montar_pacote.py` a partir da raiz `2026/lab-regressao-aula`. Requer Python com ReportLab, Pandoc e as fontes Arial/Andale Mono do macOS. O script não acessa contas nem publica no Moodle. O manifesto registra os hashes de todos os arquivos de saída.
