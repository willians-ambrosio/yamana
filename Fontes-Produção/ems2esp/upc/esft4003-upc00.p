/*****************************************************************************
 ** PROGRAMA..: esft4003-upc00.p
 ** OBJETIVO..: UPC Recebimento - ft4003
 ** AUTOR.....: DKP 
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001   
 ** ALTERAÄÂES:
 ******************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*----- DEFINICAO DE FUNCOES -----*/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}
DEFINE NEW GLOBAL SHARED VARIABLE rtToolBar     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE btCalcula     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-critica     AS LOGICAL     NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-esft4003-btCalcula       AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esft4003-btCalcula-fs    AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esft4003-seq-wt-docto    AS WIDGET-HANDLE  NO-UNDO.

DEFINE VARIABLE h_proc          AS HANDLE      NO-UNDO.

DEFINE VARIABLE wh-objet AS HANDLE      NO-UNDO.
/* ASSIGN wh-objet = p-wgh-frame:first-child. */
DEF                   VAR c-handle-obj                AS CHARACTER      NO-UNDO.

/* OUTPUT TO D:\temp\dkp\ft4003.txt APPEND.                                               */
/*     PUT UNFORMATTED 'p-ind-event               = "' p-ind-event               '"' SKIP */
/*                     'p-ind-object              = "' p-ind-object              '"' SKIP */
/*                     'p-wgh-object:PRIVATE-DATA = "' p-wgh-object:PRIVATE-DATA '"' SKIP */
/*                     'p-wgh-frame:NAME          = "' p-wgh-frame:NAME          '"' SKIP */
/*                     'p-cod-table               = "' p-cod-table               '"' SKIP */
/*                     'p-row-table               = "' STRING(p-row-table)       '"' SKIP */
/*                     '-------------------------'                               SKIP.    */
/* OUTPUT CLOSE.                                                                          */
/*                                                                                        */
/* MESSAGE "p-ind-event               = " p-ind-event                SKIP  */
/*          "p-ind-object              = " p-ind-object               SKIP */
/*          "p-wgh-object:PRIVATE-DATA = " p-wgh-object:PRIVATE-DATA  SKIP */
/*          "p-wgh-frame:NAME          = " p-wgh-frame:NAME           SKIP */
/*          "p-cod-table               = " p-cod-table                SKIP */
/*          "p-row-table               = " STRING(p-row-table)        SKIP */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
/* h_proc = SOURCE-PROCEDURE:HANDLE. */

if p-ind-event  = "Before-Initialize" and
   p-ind-object = "Container" then  do:  

    
    ASSIGN c-handle-obj               = fc-handle-obj("btCalcula,seq-wt-docto", p-wgh-frame).
    ASSIGN  wh-esft4003-btCalcula     = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN  wh-esft4003-seq-wt-docto  = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.  

     IF VALID-HANDLE(wh-esft4003-btCalcula) THEN DO:

        ASSIGN wh-esft4003-btCalcula-fs     = fc-falso(wh-esft4003-btCalcula, wh-esft4003-btCalcula:FRAME, "").

/*                wh-esft4003-btConf-fs:MOVE-AFTER(wh-esft4003-btConf). */

        ASSIGN wh-esft4003-btCalcula:SENSITIVE = NO
               wh-esft4003-btCalcula:TAB-STOP  = NO.
               wh-esft4003-btCalcula-fs:LOAD-IMAGE-UP (wh-esft4003-btCalcula:IMAGE).
               wh-esft4003-btCalcula-fs:MOVE-TO-TOP().
/*                wh-esft4003-btCalcula-fs:FLAT-BUTTON = YES    . */
               ON 'CHOOSE':U OF wh-esft4003-btCalcula-fs PERSISTENT RUN upc\esft4003-upc00.p("CHOOSE",
                                                                                        "btCalcula",
                                                                                         ?,
                                                                                        ?,
                                                                                        "",
                                                                                        p-row-table).

    END.


    
/*     DO WHILE VALID-HANDLE(wh-objet):                          */
/*                                                               */
/*         CASE wh-objet:NAME:                                   */
/*             WHEN "btCalcula"    THEN btCalcula    = wh-objet. */
/*             WHEN "rtToolBar"    THEN rtToolBar    = wh-objet. */
/*         END CASE.                                             */
/*                                                               */
/*         IF wh-objet:TYPE = 'Field-group' THEN                 */
/*             /*....e vai para o primeiro campo*/               */
/*             wh-objet = wh-objet:first-child.                  */
/*         ELSE /*Caso contrario vai para o proximo.*/           */
/*             wh-objet = wh-objet:NEXT-SIBLING.                 */
/*     END.                                                      */
END.

IF p-ind-event  = "CHOOSE" AND
   p-ind-object = "btCalcula" THEN DO:

    
    l-critica = NO.
        RELEASE WT-DOCTO.
    RELEASE nota-fisc-adc.

/*     FIND WT-DOCTO WHERE ROWID(WT-DOCTO) = p-row-table NO-LOCK NO-ERROR. */
    FIND FIRST wt-docto NO-LOCK
        WHERE wt-docto.seq-wt-docto = int(wh-esft4003-seq-wt-docto:SCREEN-VALUE) NO-ERROR.
    
    IF AVAILABLE WT-DOCTO THEN DO:
        
        FIND natur-oper WHERE natur-oper.nat-operacao = WT-DOCTO.nat-operacao NO-LOCK NO-ERROR.
        FIND cfop-natur OF natur-oper NO-LOCK NO-ERROR.
        FIND mgesp.es-cfop-val-nf OF cfop-natur NO-LOCK NO-ERROR.

        
        IF AVAILABLE mgesp.es-cfop-val-nf AND mgesp.es-cfop-val-nf.log-valcon-nf = YES THEN DO:
                        FIND FIRST nota-fisc-adc NO-LOCK 
                WHERE nota-fisc-adc.idi-tip-dado  = 21
                AND   nota-fisc-adc.cod-estab     = WT-DOCTO.cod-estabel
                AND   nota-fisc-adc.cod-serie     = WT-DOCTO.serie
                AND   nota-fisc-adc.cod-nota-fisc = STRING(WT-DOCTO.seq-wt-docto)
                AND   nota-fisc-adc.cod-livre-1   <> "" NO-ERROR.
            IF AVAIL nota-fisc-adc THEN DO: 
                l-critica = NO.
                            END.
            ELSE DO: 
            
                l-critica = YES.
            END.
        END.
    END.

    IF l-critica THEN DO:
    
        RUN utp/ut-msgs.p (INPUT "show":u,                                      
                           INPUT "17242",                                       
                           INPUT "C†lculo da Nota n∆o permitido!~~" +           
                           "Processo/Ato Concess¢rio parametrizado como obrigat¢rio no Cadastro CFOP n∆o localizado neste documento." ).
        RETURN "NOK":U.
    END.
    ELSE DO:
        APPLY "choose" TO wh-esft4003-btCalcula.
    END.
END.

/* ON "CHOOSE" OF btCalcula PERSISTENT                          */
/*     RUN upc\esft4003-upc03.p(INPUT h_proc, INPUT l-critica). */
