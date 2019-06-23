/*--------------------------------------------------------------------------------------------------------------
Nome..........: ESAPI001.P
Cria‡Æo.......: log339640 - Julho/2008
Parƒmetros....: Entrada: Tipo - 1 Î Contrato ; 2 Î Item Contrato; 3 Î MÆo de Obra; 4 Î Despesas de Importa‡Æo
                         Rowid documento - rowid da tabela necess ria para o movimento
                         Action - 1 Î Inclui; 2 Î Modifica; 3 - Elimina
                TT-ERRO (Erros ocorridos na gera‡Æo do Movimento)
Objetivo......: Gerar os movimentos
--------------------------------------------------------------------------------------------------------------*/
/*Vari veis locais*/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

DEF temp-table tt-erro no-undo
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

{utp/ut-liter.i Descri‡Æo}
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).

define temp-table tt-erro-aux like tt-erro.

def new global shared var c-motivo-alter  like hist-alter.des-motivo-alter no-undo.
def new global shared var c-alter-origem  as character format "x(76)" no-undo.
def new global shared var c-alterado      as character format "x(76)" no-undo.
def new global shared var c-tipo-alter    as character format "!" initial "C" no-undo.
def new global shared var c-texto-orig    like hist-tex-ori.alter-texto-origem no-undo.
def new global shared var c-texto         like hist-tex-des.alter-texto-destino no-undo.
def new global shared var gr-contrato-for as rowid no-undo.

DEF BUFFER b-hist-alter FOR hist-alter.

procedure pi-principal:
    DEFINE INPUT  PARAMETER pType   AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER pRowid  AS ROWID      NO-UNDO.
    DEFINE INPUT  PARAMETER pAction AS INTEGER    NO-UNDO.
    DEFINE output PARAMETER table for tt-erro.

    DEFINE VARIABLE cod-empresa      AS char       NO-UNDO.
    DEFINE VARIABLE l-relacionamento AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE i-seq            AS INTEGER    NO-UNDO.
    define buffer bfhist-alter for hist-alter.
    DEF BUFFER b-hist-alter  FOR hist-alter.
    define buffer bfcontrole-inv-esp for controle-inv-esp.

    assign c-motivo-alter  = ""
           c-alter-origem  = ""
           c-alterado      = ""
           c-tipo-alter    = ""
           c-texto-orig    = ""
           c-texto         = "".

    for each tt-erro:
        delete tt-erro.
    end.

    DO ON ERROR UNDO, LEAVE:
        if pType = 1 then do: /*Contrato*/
            {esp\esapi001-cn.i pRowid pAction}
        end.
        else if pType = 2 then do: /*Item do contrato*/
            {esp\esapi001-it.i pRowid pAction}
        end.
        else if pType = 3 then do: /*MÆo de Obra*/
            {esp\esapi001-mo.i pRowid pAction}
        end.
        else if pType = 4 then do: /*Despesa Embarque*/
            {esp\esapi001-de.i pRowid pAction}
        end.
        else if pType = 5 then do:
            {esp\esapi001-nf.i pRowid pAction}
        end.
        
        if return-value = "NOK" then do:
            RUN cdp\cd0669.w (INPUT TABLE tt-erro).
            return "NOK":U.
        end.
        return "OK":U.
    end.
    
end procedure.

procedure pi-atualiza-verba:
    define input  parameter p-acao        as   int  format "9"                    no-undo.
    define input  parameter p-ep-codigo   as   int  format ">>9"                  no-undo.
    define input  parameter p-num-ord-inv as   int  format ">>>>>,>>>"            no-undo.
    define input  parameter p-data        as   date format "99/99/9999"           no-undo.
    define input  parameter p-moeda       as   dec                                no-undo.
    define input  parameter p-valor-compr as   dec  format ">>>>>,>>>,>>>,>>9.99" no-undo.
    define input  parameter p-valor-reali as   dec  format ">>>>>,>>>,>>>,>>9.99" no-undo.
    define output parameter table for tt-erro.

    for each tt-erro:
        delete tt-erro.
    end.

    run inp/inapi048.p(p-acao                  , 
                       p-ep-codigo             ,
                       p-num-ord-inv           ,
                       p-data                  ,
                       p-moeda                 ,
                       p-valor-compr           , 
                       p-valor-reali           , 
                       output table tt-erro-aux).

    for each tt-erro-aux:
        create tt-erro.
        assign tt-erro.cd-erro  = tt-erro-aux.cd-erro
               tt-erro.mensagem = tt-erro-aux.mensagem.
    end.
end procedure.
