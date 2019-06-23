/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCN001RP 2.06.00.000}

{utp/ut-glob.i}

/* prÇprocessador para ativar ou n∆o a sa°da para RTF */
/* prÇprocessador para setar o tamanho da p†gina */
/* definiá∆o das temp-tables para recebimento de parÉmetros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    FIELD c-cod-estabel-ini   LIKE estabelec.cod-estabel
    FIELD c-cod-estabel-fim   LIKE estabelec.cod-estabel
    FIELD i-fornec-ini        LIKE emitente.cod-emitente
    FIELD i-fornec-fim        LIKE emitente.cod-emitente
    FIELD i-nr-contrato-ini   LIKE medicao-contrat.nr-contrato
    FIELD i-nr-contrato-fim   LIKE medicao-contrat.nr-contrato    
    FIELD c-item-ini          LIKE item.it-codigo
    FIELD c-item-fim          LIKE item.it-codigo
    FIELD dt-data-med-ini     LIKE medicao-contrat.dat-medicao
    FIELD dt-data-med-fim     LIKE medicao-contrat.dat-medicao
    FIELD i-seq-item-ini      LIKE medicao-contrat.num-seq-item
    FIELD i-seq-item-fim      LIKE medicao-contrat.num-seq-item
    FIELD i-seq-medicao-ini   LIKE medicao-contrat.num-seq-medicao
    FIELD i-seq-medicao-fim   LIKE medicao-contrat.num-seq-medicao
    FIELD i-transacao-ini     LIKE ext-medicao-contrat.nr-trans
    FIELD i-transacao-fim     LIKE ext-medicao-contrat.nr-trans
    FIELD l-liberada          AS LOGICAL 
    FIELD l-pendente          AS LOGICAL 
    FIELD l-servico           AS LOGICAL 
    FIELD l-compra            AS LOGICAL 
    FIELD l-beneficiamento    AS LOGICAL 
    FIELD l-imprime-matriz    AS LOGICAL 
    FIELD l-imprime-narrativa AS LOGICAL 
    FIELD l-imprime-param     AS LOGICAL
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG.
    /*Fim alteracao 15/02/2005*/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita NO-UNDO
        field raw-digita        as raw.
        
/* recebimento de parÉmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEF VAR c-unid-negoc     AS CHARACTER FORMAT "x(03)"   NO-UNDO.
DEF VAR c-conta-contabil AS CHARACTER                  NO-UNDO.
DEF VAR c-descr-contabil AS CHARACTER                  NO-UNDO.
DEF VAR de-perc-rateio   AS DECIMAL                    NO-UNDO.
DEF VAR de-valor-rateio  AS DECIMAL                    NO-UNDO.
DEF VAR i-empresa        AS CHARACTER                  NO-UNDO.
DEF VAR c-contrato       AS CHARACTER  FORMAT "X(9)"   NO-UNDO.
DEF VAR des-contrato     LIKE contrato-for.des-contrat NO-UNDO.
DEF VAR c-seq-item       AS CHARACTER  FORMAT "X(5)"   NO-UNDO.
DEF VAR c-it-codigo      LIKE item.it-codigo           NO-UNDO.
DEF VAR desc-item        AS CHARACTER  FORMAT "X(40)"  NO-UNDO.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
define variable h-acomp             as handle     no-undo.

/* definiá∆o de frames do relat¢rio */

FORM c-contrato           COLUMN-LABEL "Contrato"
     des-contrato         COLUMN-LABEL "Descriá∆o"
     c-seq-item           COLUMN-LABEL "Seq"
     c-it-codigo          COLUMN-LABEL "Item"
     desc-item            COLUMN-LABEL "Descriá∆o"
     ext-medicao-contrat.nr-trans       COLUMN-LABEL "Mediá∆o"
     medicao-contrat.dat-prev-medicao   COLUMN-LABEL "Data"
     medicao-contrat.val-medicao        COLUMN-LABEL "Valor" 
     medicao-contrat.sld-val-medicao    COLUMN-LABEL "Sld Receber" SKIP
     WITH DOWN WIDTH 188 STREAM-IO NO-BOX FRAME f-principal.

FORM "Obs:"                 AT 3 medicao-contrat.observacao NO-LABEL
    WITH WIDTH 188 SIDE-LABELS FRAME fObs STREAM-IO. 

FORM c-unid-negoc           AT  52 FORMAT "x(03)"
     c-conta-contabil       AT  56 FORMAT "x(16)"
     c-descr-contabil       AT  73 FORMAT "x(32)"
     de-perc-rateio         AT 106 FORMAT ">>9.99"  "%"
     de-valor-rateio        AT 116 FORMAT ">>>,>>>,>>9.9999" 
     WITH STREAM-IO NO-BOX NO-LABEL WIDTH 188 FRAME f-rateio.

FORM /*PARÙMETRO*/
     SKIP(5)
     "SELEÄ«O"                          AT 09      
     SKIP(1)
     "Estabelecimento:"                 AT 15
     tt-param.c-cod-estabel-ini         NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.c-cod-estabel-fim         NO-LABEL  AT 70 SKIP
     "Fornecedor:"                      AT 15
     tt-param.i-fornec-ini              NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.i-fornec-fim              NO-LABEL  AT 70 SKIP
     "Contrato:"                        AT 15
     tt-param.i-nr-contrato-ini         NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.i-nr-contrato-fim         NO-LABEL  AT 70 SKIP
     "Item:"                            AT 15
     tt-param.c-item-ini                NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.c-item-fim                NO-LABEL  AT 70 SKIP
     "Data:"                            AT 15
     tt-param.dt-data-med-ini           NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.dt-data-med-fim           NO-LABEL  AT 70 SKIP
     "Seq Item:"                        AT 15
     tt-param.i-seq-item-ini            NO-LABEL  AT 35
     "|< >|":U                          AT 60
     tt-param.i-seq-item-fim            NO-LABEL  AT 70
     SKIP(1)
     "PAR∂METROS"                       AT 09 
     SKIP(1)
     "Liberadas:"                       AT 15
     tt-param.l-liberada                NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Pendentes:"                       AT 15
     tt-param.l-pendente                NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Serviáo:"                         AT 15
     tt-param.l-servico                 NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Compra:"                          AT 15
     tt-param.l-compra                  NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Beneficiamento:"                  AT 15
     tt-param.l-beneficiamento          NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Imprime Matriz Rateio:"           AT 15
     tt-param.l-imprime-matriz          NO-LABEL  FORMAT "Sim/N∆o" AT 47 SKIP
     "Imprime Obs/Narrativa Medicao:"   AT 15
     tt-param.l-imprime-narrativa       NO-LABEL  FORMAT "Sim/N∆o" AT 47
     SKIP(1)
     "IMPRESS«O"                        AT 09
     SKIP(1)
     "Destino:"                         AT 15 " - " tt-param.arquivo NO-LABEL
     "Usu†rio:"                         AT 15 " - " tt-param.usuario NO-LABEL
     "Imprime P†gina Impress∆o:"        AT 15
     tt-param.l-imprime-param           NO-LABEL  FORMAT "Sim/N∆o" AT 40 SKIP
     WITH WIDTH 188 SIDE-LABELS FRAME fParam STREAM-IO. 

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i &pagesize=88}
 

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
/****************************************************************************
**
**  I-RPCAB.I - Form do Cabeáalho Padr∆o e RodapÇ, (ex-CD9500.F)
**                              
** {&STREAM} - indica o nome da stream (opcional)
****************************************************************************/

Define Variable vPagina  As Char No-undo.
Define Variable vPeriodo As Char No-undo.
Define Variable vTo As Char format "x(2)" No-undo.

{utp/ut-liter.i P†gina: *}
Assign vPagina = Return-value.

{utp/ut-liter.i Periodo: *}
Assign vPeriodo = Return-value.

{utp/ut-liter.i a RPCAB}
Assign vTo = Return-value.


   form header
        fill("-":U, 165) format "x(165)":U skip
        c-empresa c-titulo-relat at 83
        vPagina  format "x(07)" at 153 page-number  at 161 format ">>>>9" skip
        fill("-":U, 143) format "x(143)":U today format "99/99/9999":U
        "-" string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 165 no-labels no-box page-top frame f-cabec.
    
    form header
        fill("-":U, 165) format "x(165)":U skip
        c-empresa c-titulo-relat at 83
        vPagina  format "x(07)" at 153 page-number  at 161 format ">>>>9" skip
        vPeriodo i-numper-x at 10 "-":U
        da-iniper-x at 15 vTo da-fimper-x
        fill("-":U, 143) format "x(143)":U today format "99/99/9999":U
        "-" string(time, "HH:MM:SS":U) skip(1)
        with stream-io width 165 no-labels no-box page-top frame f-cabper.

&IF "{&STREAM}":U <> "":U &THEN
&GLOBAL-DEFINE STREAM_ONLY {&STREAM}
&ENDIF
    
c-rodape = "DATASUL - ":U + c-sistema + " - ":U + c-prg-obj + " - V:":U + c-prg-vrs.
c-rodape = fill("-":U, 165 - length(c-rodape)) + c-rodape.

form header
    c-rodape format "x(165)":U
    with stream-io width 165 no-labels no-box page-bottom frame f-rodape.

/* I-RPCAB.I */


/* bloco principal do programa */
ASSIGN  c-programa          = "ESCN001RP"
            c-versao        = "2.06"
            c-revisao       = "00.000"
            c-empresa       = STRING(v_nom_razao_social)
        c-titulo-relat  = "Mediá‰es".

find first param-global no-lock no-error.

assign i-empresa  = param-global.empresa-prin.

/* para n∆o visualizar cabeáalho/rodapÇ em sa°da RTF */
IF tt-param.destino <> 4 THEN DO:
    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.
END.

/* executando de forma persistente o utilit†rio de acompanhamento */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i "Imprimindo" "*"}
 
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* corpo do relat¢rio */

FOR EACH contrato-for
   WHERE contrato-for.cod-estabel  >= tt-param.c-cod-estabel-ini
     AND contrato-for.cod-estabel  <= tt-param.c-cod-estabel-fim
     AND contrato-for.cod-emitente >= tt-param.i-fornec-ini
     AND contrato-for.cod-emitente <= tt-param.i-fornec-fim
     AND contrato-for.nr-contrato  >= tt-param.i-nr-contrato-ini
     AND contrato-for.nr-contrato  <= tt-param.i-nr-contrato-fim NO-LOCK:

    ASSIGN c-contrato   = string(contrato-for.nr-contrato)
           des-contrato = contrato-for.des-contrat.
    
    IF contrato-for.natureza = 1 AND tt-param.l-compra         = NO THEN NEXT.
    IF contrato-for.natureza = 2 AND tt-param.l-servico        = NO THEN NEXT.
    IF contrato-for.natureza = 3 AND tt-param.l-beneficiamento = NO THEN NEXT.

    FOR EACH item-contrat
       WHERE item-contrat.nr-contrato  =  contrato-for.nr-contrato
         AND item-contrat.it-codigo    >= tt-param.c-item-ini
         AND item-contrat.it-codigo    <= tt-param.c-item-fim
         AND item-contrat.num-seq-item >= tt-param.i-seq-item-ini
         AND item-contrat.num-seq-item <= tt-param.i-seq-item-fim NO-LOCK:

        FIND FIRST item
             WHERE ITEM.it-codigo = item-contrat.it-codigo NO-LOCK NO-ERROR.

        ASSIGN c-seq-item  = string(item-contrat.num-seq-item)
               c-it-codigo = item-contrat.it-codigo
               desc-item   = ITEM.desc-item. 

        FOR EACH medicao-contrat
           WHERE medicao-contrat.nr-contrato      = contrato-for.nr-contrato
             AND medicao-contrat.num-seq-item     = item-contrat.num-seq-item 
             AND medicao-contrat.dat-medicao     >= tt-param.dt-data-med-ini
             AND medicao-contrat.dat-medicao     <= tt-param.dt-data-med-fim
             AND medicao-contrat.num-seq-medicao >= tt-param.i-seq-medicao-ini
             AND medicao-contrat.num-seq-medicao <= tt-param.i-seq-medicao-fim NO-LOCK:

            IF medicao-contrat.ind-sit-medicao = 1 AND tt-param.l-pendente = NO THEN NEXT.
            IF medicao-contrat.ind-sit-medicao = 2 AND tt-param.l-liberada = NO THEN NEXT.
            
            FIND FIRST ext-medicao-contrat 
                 WHERE ext-medicao-contrat.nr-contrato      =  medicao-contrat.nr-contrato    
                   AND ext-medicao-contrat.num-seq-item     =  medicao-contrat.num-seq-item   
                   AND ext-medicao-contrat.numero-ordem     =  medicao-contrat.numero-ordem   
                   AND ext-medicao-contrat.num-seq-event    =  medicao-contrat.num-seq-event  
                   AND ext-medicao-contrat.num-seq-medicao  =  medicao-contrat.num-seq-medicao 
                   AND ext-medicao-contrat.nr-trans         >= tt-param.i-transacao-ini 
                   AND ext-medicao-contrat.nr-trans         <= tt-param.i-transacao-fim NO-LOCK NO-ERROR.
            
            IF AVAIL ext-medicao-contrat THEN DO:
                DISPLAY c-contrato  
                        des-contrato
                        c-seq-item  
                        c-it-codigo 
                        desc-item   
                        ext-medicao-contrat.nr-trans
                        medicao-contrat.dat-prev-medicao
                        medicao-contrat.val-medicao
                        medicao-contrat.sld-val-medicao
                        WITH FRAME f-principal.
                        PUT SKIP.

                ASSIGN c-contrato   = ""
                       des-contrato = ""
                       c-seq-item   = ""
                       c-it-codigo  = ""
                       desc-item    = "".

                IF tt-param.l-imprime-narrativa = YES THEN DO:
                    DISPLAY
                    medicao-contrat.observacao
                    WITH FRAME fObs.
                    PUT SKIP(1).
                END.

                IF tt-param.l-imprime-matriz = YES THEN DO:
                    for each matriz-rat-contr where
                             matriz-rat-contr.nr-contrato = contrato-for.nr-contrato no-lock:
                        
                        find first cta_ctbl no-lock
                             where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                               and cta_ctbl.cod_cta_ctbl       = matriz-rat-contr.ct-codigo no-error.
                        if  avail cta_ctbl then
                            disp cta_ctbl.des_tit_ctbl.
                        
                        assign c-conta-contabil = matriz-rat-contr.ct-codigo
                               c-descr-contabil = if avail cta_ctbl then cta_ctbl.des_tit_ctbl else ""
                               de-perc-rateio   = matriz-rat-contr.perc-rateio
                               de-valor-rateio  = (matriz-rat-contr.perc-rateio * contrato-for.dec-2) / 100.
                                   
                        display c-unid-negoc
                                c-conta-contabil
                                c-descr-contabil
                                de-perc-rateio
                                de-valor-rateio
                                with frame f-rateio.
                                down with frame f-rateio.
                    END.
                END.
            END.
        END.
    END.
    
    IF c-contrato = "" THEN
        PUT SKIP(1).
END.

IF tt-param.l-imprime-param = YES THEN DO:

     DISPLAY
     tt-param.c-cod-estabel-ini
     tt-param.c-cod-estabel-fim
     tt-param.i-fornec-ini
     tt-param.i-fornec-fim
     tt-param.i-nr-contrato-ini
     tt-param.i-nr-contrato-fim
     tt-param.c-item-ini
     tt-param.c-item-fim
     tt-param.dt-data-med-ini
     tt-param.dt-data-med-fim
     tt-param.i-seq-item-ini
     tt-param.i-seq-item-fim
     tt-param.l-liberada
     tt-param.l-pendente
     tt-param.l-servico
     tt-param.l-compra
     tt-param.l-beneficiamento
     tt-param.l-imprime-matriz
     tt-param.l-imprime-narrativa
     tt-param.arquivo
     tt-param.usuario
     tt-param.l-imprime-param
     WITH FRAME fParam.
END.

/* fechamento do output do relat¢rio */
{include/i-rpclo.i}
 
RUN pi-finalizar IN h-acomp.
return "OK":U.
