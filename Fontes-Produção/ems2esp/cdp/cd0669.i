/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def {1} temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

{utp/ut-liter.i Mensagem}
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

{utp/ut-liter.i Descri��o}
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
