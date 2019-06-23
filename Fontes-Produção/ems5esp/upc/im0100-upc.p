/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i IM0100-UPC 2.00.00.000}  /*** 010000 ***/
{utp/ut-glob.i}

/************************* Defini»’o das Variaveis Globais ********************/
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-estabel-IM0100        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-pto-contr-IM0100      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-embarque-IM0100           AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-estabel-IM0100        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-emitente-IM0100       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-nf-simples-remessa-IM0100 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-classif-nf-IM0100         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-frame1                    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-frame0                    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-objeto                    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-child                     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-objeto-im0100             AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-fembarq                   AS WIDGET-HANDLE NO-UNDO.

/***************** Defini»’o de Parametros ************************************/
DEFINE INPUT  PARAMETER  p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT  PARAMETER  p-ind-object  AS CHARACTER      NO-UNDO. 
DEFINE INPUT  PARAMETER  p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT  PARAMETER  p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT  PARAMETER  p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT  PARAMETER  p-row-table   AS ROWID          NO-UNDO.


/*********************** Defini»’o de Variÿveis Locais ************************/
DEFINE VARIABLE c-char                AS CHARACTER     NO-UNDO.

/*ASSIGN c-char     = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").*/

/*MESSAGE  "p-ind-event: "  p-ind-event   skip
         "p-ind-object: " p-ind-object  skip
         "p-wgh-object: " p-wgh-object  skip
         "p-wgh-frame: "  p-wgh-frame:NAME   SKIP
         "c-char: " c-char
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
/*MESSAGE p-ind-event
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    
IF p-ind-event  = "AFTER-LOAD-FRAME" THEN DO:

    ASSIGN wgh-objeto = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wgh-objeto):
        IF wgh-objeto:NAME = "cembarque" THEN
            ASSIGN wgh-embarque-IM0100 = wgh-objeto.

        IF wgh-objeto:NAME = "ccod-estabel" THEN
            ASSIGN wgh-cod-estabel-IM0100 = wgh-objeto.

        IF wgh-objeto:NAME = "ccod-emitente" THEN
            ASSIGN wgh-cod-emitente-IM0100 = wgh-objeto.

        IF wgh-objeto:TYPE = 'field-group'
        THEN
            ASSIGN wgh-objeto = wgh-objeto:FIRST-CHILD.
        ELSE 
            ASSIGN wgh-objeto = wgh-objeto:NEXT-SIBLING.
    END.

END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "TOGGLE-BOX",     /*** Type ***/                                             
                  INPUT "l-nf-simples-remessa", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,            /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,             /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-nf-simples-remessa-IM0100).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "RADIO-SET",     /*** Type ***/                                             
                  INPUT "rs-classif-nf", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,            /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,             /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-classif-nf-IM0100).
END.

IF p-ind-event = "CHANGE-PAGE" AND
   p-wgh-frame:NAME = "f-pg-cla" THEN DO:
    IF VALID-HANDLE(wgh-embarque-IM0100) THEN DO:
        ASSIGN wgh-classif-nf-IM0100:SENSITIVE = YES.
        FIND FIRST embarque-imp
            WHERE embarque-imp.cod-estabel = wgh-cod-estabel-IM0100:SCREEN-VALUE
              AND embarque-imp.embarque    = wgh-embarque-IM0100:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAIL embarque-imp AND TRIM(SUBSTRING(embarque-imp.char-1,21,20)) <> "" THEN DO:
            FOR EACH historico-embarque
                WHERE historico-embarque.cod-estabel = embarque-imp.cod-estabel
                  AND historico-embarque.embarque    = TRIM(SUBSTRING(embarque-imp.char-1,21,20)) NO-LOCK:
                FIND FIRST itinerario 
                    WHERE itinerario.cod-itiner      = historico-embarque.cod-itiner
                      AND itinerario.pto-desembarque = historico-embarque.cod-pto-contr NO-LOCK NO-ERROR.
                IF AVAIL itinerario THEN DO:
                    FIND FIRST ext-historico-embarque
                        WHERE ext-historico-embarque.cod-estabel = historico-embarque.cod-estabel
                          AND ext-historico-embarque.embarque    = historico-embarque.embarque
                          AND ext-historico-embarque.cod-itiner  = historico-embarque.cod-itiner
                          AND ext-historico-embarque.sequencia   = historico-embarque.sequencia NO-LOCK NO-ERROR.
                    IF AVAIL ext-historico-embarque AND ext-historico-embarque.log-simples-remessa THEN DO:
                        FOR FIRST embarque-imp
                            WHERE embarque-imp.cod-estabel = wgh-cod-estabel-IM0100:SCREEN-VALUE
                              AND embarque-imp.embarque = TRIM(SUBSTRING(embarque-imp.char-1,21,20)) NO-LOCK:
                            ASSIGN wgh-nf-simples-remessa-IM0100:SENSITIVE = NO
                                   wgh-nf-simples-remessa-IM0100:CHECKED = YES
                                   wgh-classif-nf-IM0100:SCREEN-VALUE = "2".
                            APPLY "VALUE-CHANGED":U TO wgh-classif-nf-IM0100.
                            ASSIGN wgh-classif-nf-IM0100:SENSITIVE = NO.
                        END.
                    END.
                END.
            END.
        END.
        ELSE DO:
            FOR EACH historico-embarque
                WHERE historico-embarque.cod-estabel = embarque-imp.cod-estabel
                  AND historico-embarque.embarque    = embarque-imp.embarque NO-LOCK:
                FIND FIRST itinerario 
                    WHERE itinerario.cod-itiner      = historico-embarque.cod-itiner
                      AND itinerario.pto-desembarque = historico-embarque.cod-pto-contr NO-LOCK NO-ERROR.
                IF AVAIL itinerario THEN DO:
                    FIND FIRST ext-historico-embarque
                        WHERE ext-historico-embarque.cod-estabel = historico-embarque.cod-estabel
                          AND ext-historico-embarque.embarque    = historico-embarque.embarque
                          AND ext-historico-embarque.cod-itiner  = historico-embarque.cod-itiner
                          AND ext-historico-embarque.sequencia   = historico-embarque.sequencia NO-LOCK NO-ERROR.
                    IF AVAIL ext-historico-embarque AND ext-historico-embarque.log-simples-remessa THEN DO:
                        FIND FIRST docum-est
                            WHERE docum-est.cod-emitente = INT(wgh-cod-emitente-IM0100:SCREEN-VALUE)
                              AND TRIM(SUBSTRING(docum-est.char-1,1,12)) = ext-historico-embarque.embarque
                              AND docum-est.idi-nf-simples-remes = 1 NO-LOCK NO-ERROR.
                        IF AVAIL docum-est THEN
                            ASSIGN wgh-nf-simples-remessa-IM0100:SENSITIVE = NO
                                   wgh-nf-simples-remessa-IM0100:CHECKED = YES
                                   wgh-classif-nf-IM0100:SCREEN-VALUE = "2".
                        ELSE
                            ASSIGN wgh-nf-simples-remessa-IM0100:SENSITIVE = NO
                                   wgh-nf-simples-remessa-IM0100:CHECKED = YES
                                   wgh-classif-nf-IM0100:SCREEN-VALUE = "1".
                        APPLY "VALUE-CHANGED":U TO wgh-classif-nf-IM0100.
                        ASSIGN wgh-classif-nf-IM0100:SENSITIVE = NO.
                    END.
                END.
            END.
        END.
    END.
END.
  
PROCEDURE tela-upc:

    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.
    DEFINE INPUT  PARAMETER  pAux         AS INTEGER       NO-UNDO.
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.
    
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE i-aux   AS INTEGER       NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD
           i-aux   = 0.

    DO WHILE VALID-HANDLE(wgh-obj):              
        
        IF pApresMsg = YES THEN
            MESSAGE "Nome do Objeto" wgh-obj:NAME SKIP
                    "Type do Objeto" wgh-obj:TYPE SKIP
                    "P-Ind-Event"    pIndEvent VIEW-AS ALERT-BOX.
        
        IF wgh-obj:TYPE = pObjType AND
           wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE
                   i-aux = i-aux + 1.

            IF i-aux = pAux THEN
                LEAVE.
        END.
        IF wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE 
            ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.
    END.

END PROCEDURE.
