/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCN0002RP 2.00.00.001 } /*** 010001 ***/
/*******************************************************************************
**
**       Programa: ESCN0002RP
**
**       Data....: Agosto de 2016
**
**       Objetivo: Desativa‡Æo de Contratos
**
**       Versao..: 1.00.000
**
*******************************************************************************/
{cdp/cdcfgmat.i}

&GLOBAL-DEFINE RTF NO

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG
    /*Fim alteracao 15/02/2005*/
    FIELD nr-contrato-ini  LIKE contrato-for.nr-contrato
    FIELD nr-contrato-fim  LIKE contrato-for.nr-contrato
    FIELD dt-ter-validade-ini  AS DATE FORMAT "99/99/9999"
    FIELD dt-ter-validade-fim  AS DATE FORMAT "99/99/9999"
    FIELD nr-dias-vencido  AS INT
    FIELD l-verifica       AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

{include/i-rpvar.i}

DEF NEW GLOBAL SHARED VAR c-motivo-alter        LIKE hist-alter.des-motivo-alter        NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-alter-origem        AS CHARACTER FORMAT "x(76)"             NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-alterado            AS CHARACTER FORMAT "x(76)"             NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-tipo-alter          AS CHARACTER FORMAT "!" INITIAL "C"     NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-texto-orig          LIKE hist-tex-ori.alter-texto-origem    NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-texto               LIKE hist-tex-des.alter-texto-destino   NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-contrato-for       AS ROWID                                NO-UNDO.

DEF VAR h-acomp             AS HANDLE                       NO-UNDO.
DEF VAR c-destino           AS CHAR                         NO-UNDO.
DEF VAR c-situacao-ordem    AS CHAR COLUMN-LABEL "Situa‡Æo" NO-UNDO.
DEF VAR c-sit-medicao       AS CHAR COLUMN-LABEL "Situa‡Æo" NO-UNDO.
DEF VAR dt-vencimento-aux   AS DATE                         NO-UNDO.
DEF VAR i-count             AS INT                          NO-UNDO.
DEF VAR l-relacionamento    AS LOG                          NO-UNDO.

FORM contrato-for.nr-contrato                           AT 5
     contrato-for.des-contrat                           AT 17
     contrato-for.cod-emitente                          AT 50
     contrato-for.log-libera      FORMAT "Sim/NÆo"      AT 60 
     contrato-for.dt-ter-validade     FORMAT "99/99/9999"   AT 66
     contrato-for.dt-ter-validade FORMAT "99/99/9999"   AT 78 SKIP
    WITH STREAM-IO NO-BOX WIDTH 132 FRAME f-contrato.
    
FORM ordem-compra.num-pedido                    AT 8
     ordem-compra.numero-ordem                  AT 18
     ordem-compra.it-codigo                     AT 28
     ordem-compra.num-seq-item                  AT 45
     ordem-compra.qt-solic                      AT 52
     ordem-compra.preco-fornec                  AT 69
     ordem-compra.num-ord-inv                   AT 89
     c-situacao-ordem          FORMAT "x(10)"   AT 101 SKIP
    WITH STREAM-IO DOWN NO-BOX WIDTH 132 FRAME f-ordem.

FORM medicao-contrat.num-seq-medicao                      AT 8
     medicao-contrat.num-seq-item                         AT 16 
     medicao-contrat.dat-prev-medicao FORMAT "99/99/9999" AT 23 
     medicao-contrat.qtd-prevista                         AT 34 
     medicao-contrat.val-medicao                          AT 54 
     medicao-contrat.sld-val-medicao                      AT 71
     medicao-contrat.sld-rec-medicao                      AT 89
     c-sit-medicao                    FORMAT "x(10)"      AT 107 SKIP
    WITH STREAM-IO DOWN NO-BOX WIDTH 132 FRAME f-medicao.

find first param-global no-lock no-error.

assign c-programa 	  = "ESCN0002RP"
       c-versao	      = "1.00"
       c-revisao	  = ".00.000"
       c-empresa      = param-global.grupo
       c-sistema	  = "Contratos"
       c-titulo-relat = "Desativa‡Æo de Contratos".

{include/i-rpcab.i}
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input return-value).

FOR FIRST tt-param:

    FOR EACH contrato-for NO-LOCK
        WHERE contrato-for.nr-contrato >= tt-param.nr-contrato-ini
        AND   contrato-for.nr-contrato <= tt-param.nr-contrato-fim
        AND   contrato-for.dt-ter-validade >= tt-param.dt-ter-validade-ini
        AND   contrato-for.dt-ter-validade <= tt-param.dt-ter-validade-fim:

        IF tt-param.nr-dias-vencido <> 0 THEN DO:
            ASSIGN dt-vencimento-aux = TODAY - tt-param.nr-dias-vencido.
            
            IF contrato-for.dt-ter-validade > dt-vencimento-aux THEN
                NEXT.
        END.

        IF NOT tt-param.l-verifica THEN
            RUN pi-desativa-contrato.
        
        PUT UNFORMATTED 
            SKIP(1)
            FILL("=", 112) AT 5 SKIP.

        DISP contrato-for.nr-contrato
             contrato-for.des-contrat
             contrato-for.cod-emitente
             contrato-for.log-libera
             contrato-for.dt-ter-validade
             contrato-for.dt-ter-validade
             WITH FRAME f-contrato.
        
        DOWN WITH FRAME f-contrato.

        /* Imprime o relat¢rio detalhado somente em modo de Verifica‡Æo. */
        IF tt-param.l-verifica THEN DO:
        
            IF CAN-FIND(FIRST ordem-compra 
                        WHERE ordem-compra.nr-contrato  = contrato-for.nr-contrato) THEN
                PUT SKIP(1).
    
            FOR EACH ordem-compra NO-LOCK
                WHERE ordem-compra.nr-contrato  = contrato-for.nr-contrato:
    
                CASE ordem-compra.situacao:
                    WHEN 1 THEN
                        ASSIGN c-situacao-ordem = "NÆo Confirmada".
                    WHEN 2 THEN
                        ASSIGN c-situacao-ordem = "Confirmada".
                    WHEN 3 THEN
                        ASSIGN c-situacao-ordem = "Cotada".
                    WHEN 4 THEN
                        ASSIGN c-situacao-ordem = "Eliminada".
                    WHEN 5 THEN
                        ASSIGN c-situacao-ordem = "Em Cota‡Æo".
                    WHEN 6 THEN
                        ASSIGN c-situacao-ordem = "Recebida".
                END CASE.
    
                DISP ordem-compra.num-pedido
                     ordem-compra.numero-ordem
                     ordem-compra.it-codigo
                     ordem-compra.num-seq-item
                     ordem-compra.qt-solic
                     ordem-compra.preco-fornec
                     ordem-compra.num-ord-inv
                     c-situacao-ordem
                     WITH FRAME f-ordem.
                DOWN WITH FRAME f-ordem.
    
            END.
    
            IF CAN-FIND(FIRST medicao-contrat
                        WHERE medicao-contrat.nr-contrato  = contrato-for.nr-contrato) THEN
                PUT SKIP(1).
    
            FOR EACH item-contrat NO-LOCK
                WHERE item-contrat.nr-contrato = contrato-for.nr-contrato
                BREAK BY nr-contrato
                      BY num-seq-item:
    
                FOR EACH medicao-contrat NO-LOCK
                    WHERE medicao-contrat.nr-contrato  = item-contrat.nr-contrato
                    AND   medicao-contrat.num-seq-item = item-contrat.num-seq-item
                    BREAK BY medicao-contrat.nr-contrato
                          BY medicao-contrat.num-seq-item
                          BY medicao-contrat.num-seq-med:
                    
                    CASE medicao-contrat.ind-sit-medicao:
                        WHEN 1 THEN
                            ASSIGN c-sit-medicao = "Pendente".
                        OTHERWISE
                            ASSIGN c-sit-medicao = "Liberada".
                    END CASE.
    
                    DISP medicao-contrat.num-seq-medicao
                         medicao-contrat.num-seq-item
                         medicao-contrat.dat-prev-medicao
                         medicao-contrat.qtd-prevista
                         medicao-contrat.val-medicao
                         medicao-contrat.sld-val-medicao
                         medicao-contrat.sld-rec-medicao
                         c-sit-medicao
                        WITH FRAME f-medicao.
                END.
    
                DOWN WITH FRAME f-medicao.
            END.
        END.

        IF LINE-COUNTER >= 62 THEN
            PAGE.   
    END.
END.

view frame f-cabec.
view frame f-rodape.
/************************************************/

/** IMPRESSÇO PARAMETROS **/
page.

case tt-param.destino:
   when 1 then
       assign c-destino = "Impressora".
   when 2 then
       assign c-destino = "Arquivo".
   when 3 then
       assign c-destino = "Terminal".
end case.

put unformatted
    "SELE€ÇO"               at 01                                                                                                               skip(1)
    "           Contrato: " at 10  tt-param.nr-contrato-ini "|< >| "                       at 45 tt-param.nr-contrato-fim                       skip
    "       Data EmissÆo: " at 10  STRING(tt-param.dt-ter-validade-ini, "99/99/9999") "|< >| " at 45 STRING(tt-param.dt-ter-validade-fim, "99/99/9999") skip(1).

put unformatted
    "PAR¶METROS"            at 01                                           skip(1)
    "    Nr Dias Vencido: " at 10  tt-param.nr-dias-vencido                 skip
    "           Verifica: " at 10  STRING(tt-param.l-verifica, "Sim/NÆo")   skip(1).

put unformatted
    "IMPRESSÇO"             at 01                                                       skip(1)
    "            Destino: " at 10  c-destino          " - "    tt-param.arquivo         skip
    "            Usu rio: " at 10  tt-param.usuario                                     skip
    "Imprimir Parƒmetros: " at 10  "Sim". 

/************************************************/

{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

PROCEDURE pi-desativa-contrato:

    FIND CURRENT contrato-for EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN contrato-for.log-libera = NO.

    /*** GERAR ADITIVO ***/
    ASSIGN c-tipo-alter     = "C"
           c-texto-orig     = ""
           c-texto          = ""
           c-alter-origem   = STRING(YES)
           c-alterado       = STRING(NO)
           gr-contrato-for  = ROWID(contrato-for)
           l-relacionamento = YES.

    {utp/ut-liter.i Desativa‡Æo_Contrato}
    ASSIGN c-motivo-alter = TRIM(RETURN-VALUE).
    
    RUN cnp/cnapi002.p ("contrato-for",
                        ROWID(contrato-for),
                        "contrato-for.log-libera",
                        l-relacionamento).

    ASSIGN c-motivo-alter = "".

    FOR EACH item-contrat NO-LOCK
        WHERE item-contrat.nr-contrato = contrato-for.nr-contrato:

        FOR EACH item-fornec EXCLUSIVE-LOCK
            WHERE item-fornec.it-codigo    = item-contrat.it-codigo
            AND   item-fornec.cod-emitente = contrato-for.cod-emitente:

            ASSIGN item-fornec.perc-compra = 0.
        END.
    END.

    FOR EACH ordem-compra
             WHERE ordem-compra.nr-contrato = contrato-for.nr-contrato AND
                   ordem-compra.situacao    = 2 /* Confirmada */
             EXCLUSIVE-LOCK:
        IF NOT CAN-FIND(FIRST recebimento OF ordem-compra NO-LOCK) THEN DO:
           ASSIGN ordem-compra.situacao = 4. /* Eliminada */ 

           FOR EACH prazo-compra OF ordem-compra
                    EXCLUSIVE-LOCK:
              ASSIGN prazo-compra.situacao = 4. /* Eliminada */
           END.
        END.
        ELSE DO:
           ASSIGN ordem-compra.situacao = 6. /* Recebida */ 

           FOR EACH prazo-compra OF ordem-compra
                    EXCLUSIVE-LOCK:
              ASSIGN prazo-compra.situacao = 6. /* Recebida */
           END.
        END.
    END.

    FIND CURRENT contrato-for NO-LOCK NO-ERROR.

END PROCEDURE.
