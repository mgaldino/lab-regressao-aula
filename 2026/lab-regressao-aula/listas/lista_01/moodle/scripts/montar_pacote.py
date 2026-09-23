"""Monta os materiais de Moodle com lista explícita de arquivos públicos."""
from pathlib import Path
import hashlib, json, shutil, zipfile, subprocess
from reportlab.platypus import SimpleDocTemplate, Paragraph, PageBreak, Preformatted, Table, TableStyle
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont

SRC = Path(__file__).resolve().parents[1]
LISTA = SRC.parent
LAB = LISTA.parents[1]
OUT = LAB/'output/moodle_lista_01'
PUB = OUT/'publicar'
PUB.mkdir(parents=True, exist_ok=True)
FONT = Path('/System/Library/Fonts/Supplemental')
for name, filename in [('Arial','Arial.ttf'),('Arial-Bold','Arial Bold.ttf'),('Arial-Italic','Arial Italic.ttf'),('Mono','Andale Mono.ttf')]:
    pdfmetrics.registerFont(TTFont(name,str(FONT/filename)))
pdfmetrics.registerFontFamily('Arial',normal='Arial',bold='Arial-Bold',italic='Arial-Italic',boldItalic='Arial-Bold')
styles = {
 'title': ParagraphStyle('title',fontName='Arial-Bold',fontSize=24,leading=29,spaceAfter=16,textColor=colors.HexColor('#163f51')),
 'h': ParagraphStyle('h',fontName='Arial-Bold',fontSize=12.5,leading=16,spaceBefore=11,spaceAfter=7,keepWithNext=True,textColor=colors.HexColor('#163f51')),
 'p': ParagraphStyle('p',fontName='Arial',fontSize=10.5,leading=15,spaceAfter=9),
 'small': ParagraphStyle('small',fontName='Arial',fontSize=8.5,leading=12,spaceAfter=7),
 'code': ParagraphStyle('code',fontName='Mono',fontSize=8.8,leading=12,spaceAfter=10),
 'cell': ParagraphStyle('cell',fontName='Arial',fontSize=8.2,leading=11),
}
story=[]
def p(text,style='p'): story.append(Paragraph(text,styles[style]))
def code(text): story.append(Preformatted(text,styles['code']))
def page(title):
    if story: story.append(PageBreak())
    p(title,'title')
def table(rows,widths):
    t=Table([[Paragraph(v,styles['cell']) for v in row] for row in rows],colWidths=widths,repeatRows=1,hAlign='LEFT')
    t.setStyle(TableStyle([('BACKGROUND',(0,0),(-1,0),colors.HexColor('#e7eff2')),('VALIGN',(0,0),(-1,-1),'TOP'),('LINEBELOW',(0,0),(-1,0),0.5,colors.HexColor('#6e8e9b')),('BOTTOMPADDING',(0,0),(-1,-1),5),('TOPPADDING',(0,0),(-1,-1),5),('LINEBELOW',(0,1),(-1,-1),0.25,colors.HexColor('#d0dce1'))]))
    story.append(t)

page('Lista 1<br/>Como começar')
p('<b>FLS 6183 · Métodos Quantitativos de Pesquisa II · 2026</b><br/>Este guia acompanha o enunciado. O kit contém as duas bases e modelos facultativos para organizar os cálculos e o relatório.')
p('1. Abra o projeto','h')
p('Baixe <b>lista_01_kit_alunos.zip</b> no Moodle e extraia a pasta inteira. Abra <b>Lista_01.Rproj</b> no RStudio. Assim, os caminhos começam na pasta do projeto. Não trabalhe diretamente dentro do ZIP.')
p('Abra <b>scripts/00_conferir_ambiente.R</b> e clique em <b>Source</b>. Ele confere os pacotes, os arquivos de dados e as ferramentas para gerar o PDF; não instala programas.')
p('2. Confira os programas e pacotes','h')
p('Use o R e o RStudio preparados para o curso. O modelo usa R Markdown. Se a conferência indicar pacotes ausentes, instale somente os necessários no <b>Console do R</b>. Por exemplo:')
code('install.packages(c("data.table", "dplyr", "ggplot2",\n                   "here", "rmarkdown", "knitr"))')
p('Para gerar PDF é necessária uma instalação de LaTeX. Se você ainda não tiver uma, pode instalar TinyTeX no Console do R:')
code('install.packages("tinytex")\ntinytex::install_tinytex()')
p('Essa instalação é feita uma vez e exige internet. Reinicie o RStudio depois de concluí-la. Uma instalação de LaTeX já funcional pode ser mantida. Se faltar Pandoc, use o RStudio do curso ou procure o monitor.')
p('3. Conheça os arquivos','h')
p('<b>lista_01_revisada_2026.pdf:</b> enunciado e itens a responder.<br/><b>dados/:</b> bases originais; mantenha os arquivos intactos.<br/><b>scripts/01_respostas.R:</b> espaço para os seus cálculos.<br/><b>respostas.Rmd:</b> espaço para resultados e interpretação.<br/><b>Lista_01.Rproj:</b> projeto do RStudio.')
p('Comece conferindo se o modelo gera PDF. Ele ainda não contém respostas; a compilação inicial apenas testa o ambiente. Você também pode usar seu próprio projeto e documento Quarto, mantendo os mesmos requisitos de entrega.')
p('Referências de instalação: <a href="https://bookdown.org/yihui/rmarkdown/installation.html" color="#126483">R Markdown: Installation</a> e <a href="https://yihui.org/tinytex/" color="#126483">TinyTeX</a>, consultadas em 05/09/2026.','small')

page('Do script<br/>ao PDF de respostas')
p('1. Calcule no script, interprete no relatório','h')
p('Escreva e teste os comandos em <b>scripts/01_respostas.R</b>, na ordem dos exercícios. Use caminhos relativos e <b>dplyr::select()</b> ao selecionar colunas. A importação municipal faz parte da investigação: registre sua tentativa, examine o resultado e justifique a correção.')
p('O relatório executa esse script quando é compilado. Você pode guardar uma tabela ou um gráfico na lista <b>resultados</b> e mostrá-lo no documento. O final do modelo explica como conectar esses objetos às tabelas e figuras; adapte os nomes aos objetos que criou.')
p('Escreva suas respostas aos itens a, b, c etc. sob o exercício correspondente. Mostre os resultados relevantes, os denominadores e a interpretação. Evite imprimir bases completas. As legendas devem dizer o que é mostrado, a unidade e a fonte.')
p('2. Gere e confira o PDF','h')
p('Abra <b>respostas.Rmd</b> e clique em <b>Knit</b>. Outra opção é executar no Console, com o projeto aberto:')
code('rmarkdown::render("respostas.Rmd")')
p('Antes de entregar, reinicie o R e compile novamente. Se um objeto não for encontrado, confira se ele foi criado pelo script. Se um arquivo não for encontrado, confira a pasta do projeto e o caminho. Leia as mensagens de aviso; não as oculte para fazer o documento parecer correto.')
p('3. Entregue no Moodle','h')
p('Envie <b>respostas.pdf</b>, a fonte <b>respostas.Rmd</b> (ou sua fonte .qmd) e <b>01_respostas.R</b>. Inclua os demais scripts necessários se tiver dividido os cálculos em vários arquivos. Preserve a organização de pastas nos caminhos escritos no código; não é necessário reenviar as bases fornecidas.')
p('Identifique a autoria conforme a modalidade anunciada na atividade. Confira prazo, horário e estado final da submissão no Moodle. O modelo é facultativo; o enunciado determina o que responder.')
p('4. Faça a última leitura','h')
p('Resolva os exercícios <b>1 a 9</b>. Confira se cada resposta identifica os dados usados e seus limites. Diferencie proporções de pontos percentuais. Remova do PDF as orientações e os exemplos vazios do modelo antes de entregar.')
p('Se usar o <a href="https://mgaldino.pythonanywhere.com/" color="#126483">tutor</a>, explique o que tentou e como verificou a orientação recebida. O piloto documentado do tutor cobre importação; sua instalação não é requisito para fazer a lista. Problemas com o Gemini não impedem a resolução no RStudio.')

page('Conheça as bases')
p('Tabela 1. Variáveis da base municipal. Cada linha representa um município; a referência da população é 1º de julho de 2020.','small')
table([['<b>Variável</b>','<b>Significado</b>'],['uf','Unidade da Federação.'],['nome_munic','Nome do município. Use a UF em conjunto para identificar municípios.'],['populacao','Estimativa da população residente, em habitantes.']], [112,385])
p('Fonte: IBGE; arquivo didático preservado pelo curso. A cópia fornecida tem 5.570 linhas e três variáveis. Investigue sua importação sem alterar o arquivo original.','small')
p('Tabela 2. Variáveis do recorte Brasil–China da AGNU. Cada linha representa uma votação nominal com os dois votos observados, em 1997–2016.','small')
rows=[['<b>Variável</b>','<b>Significado</b>'],['rcid','Identificador da votação nominal.'],['data / ano','Data da votação e ano correspondente.'],['periodo_2009','Rótulo: 1997–2008 ou 2009–2016.'],['pos_2009','Indicador: 0 no primeiro período, 1 no segundo.'],['sessao','Número da sessão da Assembleia Geral; não é usado nos cálculos pedidos.'],['simbolo_resolucao','Identificação da resolução na fonte; não é usada nos cálculos pedidos.'],['tema','Tema ou combinação de temas. Preserve cada texto completo como uma categoria, inclusive a categoria sem codificação temática.'],['voto_importante','Indicador mantido da fonte original; não é usado nesta lista. Valores ausentes aqui não justificam excluir votações.'],['voto_brasil / voto_china','Votos registrados: yes (sim), no (não) ou abstain (abstenção).'],['convergente','Vale 1 se os votos dos dois países coincidem e 0 caso contrário.']]
table(rows,[112,385])
p('Fonte: recorte didático do projeto AGNA, a partir de unvotes 0.3.0. A base tem 1.762 linhas e 12 variáveis. Mantida a mesma cópia usada nas Aulas 2 e 3.','small')
p('Os dois arquivos foram copiados dos materiais locais do curso em 05/09/2026, sem mudança de conteúdo. Não é necessário repetir a coleta externa. As análises da lista descrevem essas observações; não identificam efeitos causais.','small')

def footer(c,d):
    c.setFont('Arial',8); c.setFillColor(colors.HexColor('#49616c'))
    c.drawString(48,32,'FLS 6183 | Lista 1 | Orientações de trabalho')
    c.drawRightString(A4[0]-48,32,f'{d.page} / 3')
pdf=PUB/'orientacoes_lista_01.pdf'
SimpleDocTemplate(str(pdf),pagesize=A4,leftMargin=48,rightMargin=48,topMargin=42,bottomMargin=55,title='Lista 1: orientações de trabalho',author='FLS 6183').build(story,onFirstPage=footer,onLaterPages=footer)
shutil.copy2(LAB/'output/pdf/lista_01_revisada_2026.pdf',PUB/'lista_01_revisada_2026.pdf')
shutil.copy2(LAB.parents[1]/'2026/instrucoes_tutor/output/pdf/guia_tutor_acesso_gemini.pdf',OUT/'apoio_tutor/guia_tutor_acesso_gemini.pdf')

# Apenas os itens desta lista entram no kit estudantil.
members={f'lista_01/{p.relative_to(SRC/"modelo").as_posix()}':p for p in sorted((SRC/'modelo').rglob('*')) if p.is_file()}
for name in ['populacao_municipios_2020.csv','brasil_convergencia_china_1997_2016.csv']:
    members[f'lista_01/dados/{name}']=LISTA/'dados'/name
for name in ['lista_01_revisada_2026.pdf','orientacoes_lista_01.pdf']:
    members[f'lista_01/{name}']=PUB/name
def archive(path,files):
    with zipfile.ZipFile(path,'w',zipfile.ZIP_DEFLATED) as z:
        for name,source in sorted(files.items()):
            info=zipfile.ZipInfo(name,date_time=(2026,9,5,12,0,0)); info.compress_type=zipfile.ZIP_DEFLATED
            z.writestr(info,source.read_bytes())
    with zipfile.ZipFile(path) as z: assert z.testzip() is None
archive(PUB/'lista_01_kit_alunos.zip',members)

# Descrição HTML para colar no editor Moodle, sem estilos ou links locais.
subprocess.run(['pandoc',str(SRC/'textos_moodle/descricao_atividade.md'),'-f','gfm','-t','html','--wrap=none','-o',str(SRC/'textos_moodle/descricao_atividade.html')],check=True)
for file in (SRC/'textos_moodle').glob('*'):
    dest=OUT/'textos_moodle'/file.name; dest.parent.mkdir(exist_ok=True); shutil.copy2(file,dest)
if (SRC/'LEIA-ME_PROFESSOR.md').exists(): shutil.copy2(SRC/'LEIA-ME_PROFESSOR.md',OUT/'LEIA-ME_PROFESSOR.md')
manifest={str(p.relative_to(OUT)): {'bytes':p.stat().st_size,'sha256':hashlib.sha256(p.read_bytes()).hexdigest()} for p in sorted(OUT.rglob('*')) if p.is_file() and p.name!='manifesto.json'}
(OUT/'manifesto.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2)+'\n')
archive(LAB/'output/lista_01_moodle_completo.zip',{f'moodle_lista_01/{p.relative_to(OUT).as_posix()}':p for p in sorted(OUT.rglob('*')) if p.is_file()})
print(f'Pacote estudantil: {len(members)} arquivos')
print(OUT)
