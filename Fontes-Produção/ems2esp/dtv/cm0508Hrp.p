/********************************************************************************
** Programa : cm0508Hrp
** Objetivo : 
** Autor    : Bruno Bertulli
** Data     : 30/04/2013
** Alterado : 
*********************************************************************************/

/****************** INCLUDE COM VARIæVEIS GLOBAIS *********************/
{include/i-prgvrs.i cm0508RP 12.1.13.000}
def new global shared var i-ep-codigo-usuario     like mguni.empresa.ep-codigo        no-undo.
def new global shared var l-implanta              as log    init no.
def new global shared var c-seg-usuario           as char format "x(12)"              no-undo.
def new global shared var i-num-ped-exec-rpw      as int                              no-undo.
def new global shared var i-pais-impto-usuario    as int format ">>9"                 no-undo.
def new global shared var l-rpc                   as log                              no-undo.
def new global shared var r-registro-atual        as rowid                            no-undo.
def new global shared var c-arquivo-log           as char  format "x(60)"             no-undo.
def new global shared var i-num-ped               as int                              no-undo.
def new global shared var v_cdn_empres_usuar      like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren      like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab      as handle                           no-undo.
def new global shared var v_cod_grp_usuar_lst     as char                             no-undo.
def new global shared var v_num_tip_aces_usuar    as int                              no-undo.
def new global shared var rw-log-exec             as rowid                            no-undo.
def new global shared var c-dir-spool-servid-exec as char                             no-undo.


def var v-num-reg-lidos       as   int    no-undo.
def var de-val-mater-1-tt-001 like mmv-movto-mater.val-mater-1 no-undo.
DEFINE VARIABLE de-valor-ult-medio    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-tt-valor-ult-medio AS DECIMAL     NO-UNDO.
DEFINE VARIABLE l-debito AS LOGICAL     NO-UNDO.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp as handle no-undo.

/* variaveis locais */
define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field desc-classifica       as char format "x(40)"
    field modelo-rtf            as char format "x(35)"
    field l-habilitaRtf         as LOG
    field parametro             as logical
    field formato               as integer
    field v_num_tip_aces_usuar  as integer
    field ep-codigo             as char 
    field da-dt-trans-ini       like movto-estoq.dt-trans
    field da-dt-trans-fim       like movto-estoq.dt-trans
    field i-nr-ord-produ-ini    like movto-estoq.nr-ord-produ
    field i-nr-ord-produ-fim    like movto-estoq.nr-ord-produ
    field c-cod-eqpto-ini       like mmv-movto-mater.cod-eqpto     
    field c-cod-eqpto-fim       like mmv-movto-mater.cod-eqpto     
    field c-cc-codigo-ini       like mmv-ord-manut.cc-codigo       
    field c-cc-codigo-fim       like mmv-ord-manut.cc-codigo       
    field c-ct-codigo-ini       like mmv-ord-manut.ct-codigo       
    field c-ct-codigo-fim       like mmv-ord-manut.ct-codigo       
    field c-it-codigo-ini       like mmv-movto-mater.it-codigo     
    field c-it-codigo-fim       like mmv-movto-mater.it-codigo     
    field c-cod-compon-ini      like mmv-ord-manut.cod-compon      
    field c-cod-compon-fim      like mmv-ord-manut.cod-compon      
    field i-cd-tipo-ini         like mmv-ord-manut.cd-tipo         
    field i-cd-tipo-fim         like mmv-ord-manut.cd-tipo         
    FIELD cod-estabel-ini       LIKE movto-estoq.cod-estabel
    FIELD cod-estabel-fim       LIKE movto-estoq.cod-estabel
    FIELD Frotas                AS LOGICAL 
    FIELD Manutencao            AS LOGICAL     
    .

create tt-param.
raw-transfer raw-param to tt-param.

form mmv-ord-manut.cod-eqpto      column-label "Eqpto"           format "x(12)"             at 001 
     mmv-ord-manut.ct-codigo      column-label "Conta"           format "x(8)"              at 014 
     mmv-ord-manut.cc-codigo      column-label "C Custo"         format "x(8)"              at 023 
     mmv-movto-mater.it-codigo    column-label "Item"            format "x(11)"             at 032 
     item.desc-item               column-label "Descri‡Æo"       format "x(20)"             at 044 
     movto-estoq.dt-trans         column-label "Transa‡Æo"       format "99/99/9999"        at 065 
     mmv-movto-mater.nr-ord-produ column-label "Ordem"           format ">>>,>>>,>>9"       at 076 
     mmv-movto-mater.qtd-item     column-label "Quantidade Item" format "->>>,>>>,>>9.9999" at 088 
     mmv-movto-mater.val-mater-1  column-label "Valor"           format "->>>,>>>,>>9.9999" at 106 
     de-valor-ult-medio           column-label "Vl Ult Med"      format "->>>,>>>,>>9.9999" at 124
     mmv-movto-mater.log-saida    column-label "D‚bito"          format "Sim/NÆo"           at 142 
     with down width 333 no-box stream-io frame f-relat.

FORM ord-manut.cd-equipto       column-label "Eqpto"           format "x(12)"             at 001
     ord-manut.ct-codigo        column-label "Conta"           format "x(8)"              at 014
     ord-manut.sc-codigo        column-label "C Custo"         format "x(8)"              at 023
     movto-estoq.it-codigo      column-label "Item"            format "x(11)"             at 032
     item.desc-item             column-label "Descri‡Æo"       format "x(20)"             at 044
     movto-estoq.dt-trans       column-label "Transa‡Æo"       format "99/99/9999"        at 065
     movto-estoq.nr-ord-produ   column-label "Ordem"           format ">>>,>>>,>>9"       at 076
     movto-estoq.quantidade     column-label "Quantidade Item" format "->>>,>>>,>>9.9999" at 088
     movto-estoq.valor-mat-m[1] column-label "Valor"           format "->>>,>>>,>>9.9999" at 106
     de-valor-ult-medio         column-label "Vl Ult Med"      format "->>>,>>>,>>9.9999" at 124
     l-debito                   column-label "D‚bito"          format "Sim/NÆo"           at 142 
     with down width 333 no-box stream-io frame f-relat2.

def new shared stream str-rp.

{include/tt-edit.i}
{include/i-rpvar.i}

assign c-programa     = "cm05088"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "CUSTOS EQUIPAMENTO"
       c-sistema      = "Frotas".

find first mguni.empresa no-lock
     where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
assign c-empresa = if  avail mguni.empresa then mguni.empresa.razao-social else "".

find first param-global no-lock no-error.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Processando").

{include/i-rpout.i &STREAM="stream str-rp"}


/** Logica do Programa **/

IF tt-param.Manutencao THEN DO:     

    for each movto-estoq NO-LOCK
        where movto-estoq.dt-trans     >= tt-param.da-dt-trans-ini
        and   movto-estoq.dt-trans     <= tt-param.da-dt-trans-fim
        and   movto-estoq.cod-estabel  >= tt-param.cod-estabel-ini
        and   movto-estoq.cod-estabel  <= tt-param.cod-estabel-fim
        and   movto-estoq.it-codigo    >= tt-param.c-it-codigo-ini
        and   movto-estoq.it-codigo    <= tt-param.c-it-codigo-fim
        and   movto-estoq.nr-ord-produ >= tt-param.i-nr-ord-produ-ini
        and   movto-estoq.nr-ord-produ <= tt-param.i-nr-ord-produ-fim,
        each ord-manut NO-LOCK 
       where ord-manut.nr-ord-produ = movto-estoq.nr-ord-produ
         and ord-manut.sc-codigo   >= tt-param.c-cc-codigo-ini
         and ord-manut.sc-codigo   <= tt-param.c-cc-codigo-fim
         and ord-manut.ct-codigo   >= tt-param.c-ct-codigo-ini
         and ord-manut.ct-codigo   <= tt-param.c-ct-codigo-fim
         and ord-manut.cd-tipo     >= tt-param.i-cd-tipo-ini
         and ord-manut.cd-tipo     <= tt-param.i-cd-tipo-fim
         and ord-manut.cd-equipto  >= tt-param.c-cod-eqpto-ini
         and ord-manut.cd-equipto  <= tt-param.c-cod-eqpto-fim,
        FIRST ITEM OF movto-estoq NO-LOCK
        break by ord-manut.cd-equipto
              by ord-manut.nr-ord-produ:

        IF movto-estoq.valor-mat-m[1] = 0 THEN DO:
            FIND LAST pr-it-per NO-LOCK 
                WHERE pr-it-per.it-codigo   = movto-estoq.it-codigo
                AND   pr-it-per.cod-estabel = movto-estoq.cod-estabel
                NO-ERROR.
            IF AVAILABLE pr-it-per THEN DO:
                IF pr-it-per.val-unit-mat-m[1] = 0 THEN DO:
                    FIND FIRST item-uni-estab NO-LOCK
                        WHERE item-uni-estab.it-codigo   = movto-estoq.it-codigo
                        AND   item-uni-estab.cod-estabel = movto-estoq.cod-estabel
                        NO-ERROR.
                    IF AVAILABLE item-uni-estab THEN
                        ASSIGN de-valor-ult-medio = item-uni-estab.preco-ul-ent.
                END.
                ELSE DO:
                    ASSIGN de-valor-ult-medio = pr-it-per.val-unit-mat-m[1].
                END.
            END.
            ELSE DO:
                FIND FIRST item-uni-estab NO-LOCK
                    WHERE item-uni-estab.it-codigo   = movto-estoq.it-codigo
                    AND   item-uni-estab.cod-estabel = movto-estoq.cod-estabel
                    NO-ERROR.
                IF AVAILABLE item-uni-estab THEN
                    ASSIGN de-valor-ult-medio = item-uni-estab.preco-ul-ent.
            END.

            IF de-valor-ult-medio <> 0 THEN
                ASSIGN de-valor-ult-medio = de-valor-ult-medio * (IF movto-estoq.tipo-trans = 2 THEN movto-estoq.quantidade ELSE movto-estoq.quantidade * -1).
        END.
        ELSE DO:
            ASSIGN de-valor-ult-medio = (IF movto-estoq.tipo-trans = 2 THEN movto-estoq.valor-mat-m[1] ELSE movto-estoq.valor-mat-m[1] * -1).
        END.

        /* bno - 14/06/2013 solicitado pela Camila */
        IF movto-estoq.esp-docto = 01 THEN NEXT.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input "Registros Lidos : " + string(v-num-reg-lidos)).

        if first-of(ord-manut.cd-equipto) then
            assign de-val-mater-1-tt-001 = 0
                   de-tt-valor-ult-medio = 0.

        assign de-val-mater-1-tt-001 = de-val-mater-1-tt-001 + (IF movto-estoq.tipo-trans = 2 THEN movto-estoq.valor-mat-m[1] ELSE movto-estoq.valor-mat-m[1] * -1)
               de-tt-valor-ult-medio = de-tt-valor-ult-medio + de-valor-ult-medio.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.

        disp stream str-rp
            ord-manut.cd-equipto
            ord-manut.ct-desp
            ord-manut.sc-desp
            movto-estoq.it-codigo
            item.desc-item
            movto-estoq.dt-trans
            movto-estoq.nr-ord-produ
            (IF movto-estoq.tipo-trans = 2 THEN movto-estoq.quantidade ELSE movto-estoq.quantidade * -1) @ movto-estoq.quantidade
            de-valor-ult-medio
            (IF movto-estoq.tipo-trans = 2 THEN YES ELSE NO) @ l-debito
            with stream-io frame f-relat2.
            down stream str-rp with frame f-relat2.

        if  last-of(ord-manut.cd-equipto) then do:
            disp stream str-rp "-----------------" @ movto-estoq.valor-mat-m[1]
                with stream-io frame f-relat2.
            down stream str-rp with frame f-relat2.

            put stream str-rp "Total por equipamento:" at 083 /*de-val-mater-1-tt-001 format "->>>,>>>,>>9.9999" to 122*/.
            put stream str-rp de-tt-valor-ult-medio format "->>>,>>>,>>9.9999" to 122.
            put stream str-rp unformatted skip(1).
            put stream str-rp unformatted skip(1).
        end.
    END.
END.


view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.


IF tt-param.frota THEN DO:     

    IF tt-param.manutencao THEN
       PAGE.

    for each movto-estoq NO-LOCK
        where movto-estoq.dt-trans     >= tt-param.da-dt-trans-ini
        and   movto-estoq.dt-trans     <= tt-param.da-dt-trans-fim
        and   movto-estoq.it-codigo    >= tt-param.c-it-codigo-ini
        and   movto-estoq.it-codigo    <= tt-param.c-it-codigo-fim
        and   movto-estoq.cod-estabel  >= tt-param.cod-estabel-ini
        and   movto-estoq.cod-estabel  <= tt-param.cod-estabel-fim
        and   movto-estoq.nr-ord-produ >= tt-param.i-nr-ord-produ-ini
        and   movto-estoq.nr-ord-produ <= tt-param.i-nr-ord-produ-fim,
        each mmv-movto-mater NO-LOCK 
       where mmv-movto-mater.nr-trans   = movto-estoq.nr-trans
         and mmv-movto-mater.cod-eqpto >= tt-param.c-cod-eqpto-ini
         and mmv-movto-mater.cod-eqpto <= tt-param.c-cod-eqpto-fim,
        each mmv-ord-manut NO-LOCK 
       where mmv-ord-manut.nr-ord-produ = mmv-movto-mater.nr-ord-produ
         and mmv-ord-manut.cc-codigo   >= tt-param.c-cc-codigo-ini
         and mmv-ord-manut.cc-codigo   <= tt-param.c-cc-codigo-fim
         and mmv-ord-manut.cod-compon  >= tt-param.c-cod-compon-ini
         and mmv-ord-manut.cod-compon  <= tt-param.c-cod-compon-fim
         and mmv-ord-manut.ct-codigo   >= tt-param.c-ct-codigo-ini
         and mmv-ord-manut.ct-codigo   <= tt-param.c-ct-codigo-fim
         and mmv-ord-manut.cd-tipo     >= tt-param.i-cd-tipo-ini
         and mmv-ord-manut.cd-tipo     <= tt-param.i-cd-tipo-fim,
        FIRST ITEM OF movto-estoq NO-LOCK
        break by mmv-movto-mater.cod-eqpto
              by mmv-movto-mater.nr-ord-produ
              by mmv-ord-manut.cod-compon
              by mmv-movto-mater.log-saida:

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input "Registros Lidos : " + string(v-num-reg-lidos)).

        IF mmv-movto-mater.val-mater-1 = 0 THEN DO:
            FIND LAST pr-it-per NO-LOCK 
                WHERE pr-it-per.it-codigo   = movto-estoq.it-codigo
                AND   pr-it-per.cod-estabel = movto-estoq.cod-estabel
                NO-ERROR.
            IF AVAILABLE pr-it-per THEN DO:
                IF pr-it-per.val-unit-mat-m[1] = 0 THEN DO:
                    FIND FIRST item-uni-estab NO-LOCK
                        WHERE item-uni-estab.it-codigo   = movto-estoq.it-codigo
                        AND   item-uni-estab.cod-estabel = movto-estoq.cod-estabel
                        NO-ERROR.
                    IF AVAILABLE item-uni-estab THEN
                        ASSIGN de-valor-ult-medio = item-uni-estab.preco-ul-ent.
                END.
                ELSE DO:
                    ASSIGN de-valor-ult-medio = pr-it-per.val-unit-mat-m[1].
                END.
            END.
            ELSE DO:
                FIND FIRST item-uni-estab NO-LOCK
                    WHERE item-uni-estab.it-codigo   = movto-estoq.it-codigo
                    AND   item-uni-estab.cod-estabel = movto-estoq.cod-estabel
                    NO-ERROR.
                IF AVAILABLE item-uni-estab THEN
                    ASSIGN de-valor-ult-medio = item-uni-estab.preco-ul-ent.
            END.

            IF de-valor-ult-medio <> 0 THEN
                ASSIGN de-valor-ult-medio = de-valor-ult-medio * (IF movto-estoq.tipo-trans = 2 THEN mmv-movto-mater.qtd-item ELSE mmv-movto-mater.qtd-item * -1).
        END.
        ELSE DO:
            ASSIGN de-valor-ult-medio = (IF movto-estoq.tipo-trans = 2 THEN mmv-movto-mater.val-mater-1 ELSE mmv-movto-mater.val-mater-1 * -1).
        END.

        if  first-of(mmv-movto-mater.cod-eqpto) then
            assign de-val-mater-1-tt-001 = 0
                   de-tt-valor-ult-medio = 0.

        assign de-val-mater-1-tt-001 = de-val-mater-1-tt-001 + (IF movto-estoq.tipo-trans = 2 THEN mmv-movto-mater.val-mater-1 ELSE mmv-movto-mater.val-mater-1 * -1)
               de-tt-valor-ult-medio = de-tt-valor-ult-medio + de-valor-ult-medio.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.

        disp stream str-rp
            mmv-ord-manut.cod-eqpto
            mmv-ord-manut.ct-codigo
            mmv-ord-manut.cc-codigo
            mmv-movto-mater.it-codigo
            item.desc-item
            movto-estoq.dt-trans
            mmv-movto-mater.nr-ord-produ
            (IF movto-estoq.tipo-trans = 2 THEN mmv-movto-mater.qtd-item ELSE mmv-movto-mater.qtd-item * -1) @ mmv-movto-mater.qtd-item
            de-valor-ult-medio
            mmv-movto-mater.log-saida
            with stream-io frame f-relat.
            down stream str-rp with frame f-relat.

        if  last-of(mmv-movto-mater.cod-eqpto) then do:
            disp stream str-rp "-----------------" @ mmv-movto-mater.val-mater-1
                with stream-io frame f-relat.
            down stream str-rp with frame f-relat.

            put stream str-rp "Total por equipamento:" at 083 /*de-val-mater-1-tt-001 format "->>>,>>>,>>9.9999" to 122*/.
            put stream str-rp de-tt-valor-ult-medio format "->>>,>>>,>>9.9999" to 122.
            put stream str-rp unformatted skip(1).
            put stream str-rp unformatted skip(1).
        end.
    END.
END.

/** Fim da l½gica **/

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

{include/pi-edit.i}

/** fim do programa */

