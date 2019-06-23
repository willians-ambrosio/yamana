/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i IM0055A-UPC 2.00.00.000}  /*** 010000 ***/

{utp/ut-glob.i}

/************************* Defini»’o das Variaveis Globais ********************/
DEFINE NEW GLOBAL SHARED VARIABLE wgh-v01di095                 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-embarque-simples-IM0055A AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-id-meio-transp-IM0055A   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-estabel-IM0055A      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-itiner-IM0055A       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-cod-pto-contr-IM0055A    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-embarque-IM0055A         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-seq-IM0055A              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-frame1                   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-frame0                   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-objeto                   AS WIDGET-HANDLE NO-UNDO.

/***************** Defini»’o de Parametros ************************************/
DEFINE INPUT  PARAMETER  p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT  PARAMETER  p-ind-object  AS CHARACTER      NO-UNDO. 
DEFINE INPUT  PARAMETER  p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT  PARAMETER  p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT  PARAMETER  p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT  PARAMETER  p-row-table   AS ROWID          NO-UNDO.


/*********************** Defini»’o de Variÿveis Locais ************************/
DEFINE VARIABLE c-char                AS CHARACTER     NO-UNDO.

ASSIGN c-char       = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/")
       wgh-v01di095 = p-wgh-frame.

/*MESSAGE  "p-ind-event: "  p-ind-event   skip
         "p-ind-object: " p-ind-object  skip
         "p-wgh-object: " p-wgh-object  skip
         "p-wgh-frame: "  p-wgh-frame:NAME   SKIP
         "c-char: " c-char
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
/*MESSAGE p-ind-event
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    
IF p-ind-event  = "BEFORE-INITIALIZE" THEN DO:
    /*RUN tela-upc (INPUT  p-wgh-frame,
                  INPUT  p-ind-event,
                  INPUT  "frame",           /** Type **/
                  INPUT  "fpage0",          /** Name **/
                  INPUT  NO,                /** Apresenta Mensagem dos Objetos **/
                  INPUT  1,                 /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-frame1).*/

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",          /*** Type ***/                                             
                  INPUT "id-meio-transp",  /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,                /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,                 /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-id-meio-transp-IM0055A).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",     /*** Type ***/                                             
                  INPUT "cod-estabel", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,            /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,             /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-cod-estabel-IM0055A).

    RUN tela-upc (INPUT p-wgh-frame,                                                              
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",    /*** Type ***/                                             
                  INPUT "cod-itiner", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,           /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,            /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-cod-itiner-IM0055A).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",       /*** Type ***/                                             
                  INPUT "cod-pto-contr", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,              /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,               /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-cod-pto-contr-IM0055A).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",  /*** Type ***/                                             
                  INPUT "embarque", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,         /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,          /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-embarque-IM0055A).

    RUN tela-upc (INPUT p-wgh-frame,                                                               
                  INPUT p-ind-event,                                                      
                  INPUT "FILL-IN",   /*** Type ***/                                             
                  INPUT "sequencia", /*** NOME DO CAMPO DESEJADO ***/                                             
                  INPUT NO,          /*** Apresenta Mensagem dos Objetos ***/                   
                  INPUT 1,           /*** Quando existir mais de um objeto com o mesmo nome ***/ 
                  OUTPUT wgh-seq-IM0055A).

    CREATE TOGGLE-BOX wgh-embarque-simples-IM0055A
     ASSIGN FRAME = p-wgh-frame
            WIDTH      = 23
            HEIGHT     = 0.88
            ROW        = wgh-id-meio-transp-IM0055A:ROW
            COL        = wgh-id-meio-transp-IM0055A:COL + 23
            SENSITIVE  = NO
            NAME       = "wgh-embarque-simples-IM0055A"
            LABEL      = "Embarque Simples Remessa"
            VISIBLE    = YES .
END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
    IF VALID-HANDLE(wgh-embarque-simples-IM0055A) THEN DO:
        FIND FIRST ext-historico-embarque
             WHERE ext-historico-embarque.cod-estabel    = wgh-cod-estabel-IM0055A:input-value
               and ext-historico-embarque.cod-itiner     = wgh-cod-itiner-IM0055A:input-value
               and ext-historico-embarque.cod-pto-contr  = wgh-cod-pto-contr-IM0055A:input-value
               and ext-historico-embarque.embarque       = wgh-embarque-IM0055A:input-value
               and ext-historico-embarque.sequencia      = wgh-seq-IM0055A:input-value NO-ERROR.

        IF AVAIL ext-historico-embarque THEN 
            ASSIGN wgh-embarque-simples-IM0055A:CHECKED = ext-historico-embarque.log-simples-remessa.
        ELSE 
            ASSIGN wgh-embarque-simples-IM0055A:CHECKED = NO.
    END.
END.

if p-ind-event = "AFTER-DISPLAY" THEN DO:
    IF VALID-HANDLE(wgh-embarque-simples-IM0055A) THEN DO:
        FIND FIRST ext-historico-embarque
             WHERE ext-historico-embarque.cod-estabel    = wgh-cod-estabel-IM0055A:input-value
               and ext-historico-embarque.cod-itiner     = wgh-cod-itiner-IM0055A:input-value
               and ext-historico-embarque.cod-pto-contr  = wgh-cod-pto-contr-IM0055A:input-value
               and ext-historico-embarque.embarque       = wgh-embarque-IM0055A:input-value
               and ext-historico-embarque.sequencia      = wgh-seq-IM0055A:input-value NO-ERROR.
        IF AVAIL ext-historico-embarque THEN 
            ASSIGN wgh-embarque-simples-IM0055A:CHECKED = ext-historico-embarque.log-simples-remessa.
        ELSE 
            ASSIGN wgh-embarque-simples-IM0055A:CHECKED = NO.
    END.
END.

if p-ind-event  = "BEFORE-ENABLE" THEN DO:
    FIND FIRST itinerario 
         WHERE itinerario.cod-itiner      = wgh-cod-itiner-IM0055A:input-value
           AND itinerario.pto-desembarque = wgh-cod-pto-contr-IM0055A:input-value NO-LOCK NO-ERROR.
    
    IF VALID-HANDLE(wgh-embarque-simples-IM0055A) 
       AND AVAIL itinerario THEN
        ASSIGN wgh-embarque-simples-IM0055A:SENSITIVE = YES.
END.

if p-ind-event  = "AFTER-CANCEL" THEN DO:
   IF VALID-HANDLE(wgh-embarque-simples-IM0055A) THEN
       ASSIGN wgh-embarque-simples-IM0055A:SENSITIVE = NO.
END.

if p-ind-event  = "AFTER-DISABLE" THEN DO:
   IF VALID-HANDLE(wgh-embarque-simples-IM0055A) THEN
       ASSIGN wgh-embarque-simples-IM0055A:SENSITIVE = NO.
END.

if p-ind-event  = "AFTER-SAVE-FIELDS" THEN DO:
    IF VALID-HANDLE(wgh-embarque-simples-IM0055A) THEN DO:
        FIND FIRST ext-historico-embarque
             WHERE ext-historico-embarque.cod-estabel    = wgh-cod-estabel-IM0055A:input-value
               and ext-historico-embarque.cod-itiner     = wgh-cod-itiner-IM0055A:input-value
               and ext-historico-embarque.embarque       = wgh-embarque-IM0055A:input-value
               and ext-historico-embarque.sequencia      = wgh-seq-IM0055A:input-value NO-ERROR.
        IF AVAIL ext-historico-embarque THEN 
            ASSIGN ext-historico-embarque.log-simples-remessa = wgh-embarque-simples-IM0055A:input-value.
        ELSE DO:
            CREATE ext-historico-embarque.
            ASSIGN ext-historico-embarque.cod-estabel         = wgh-cod-estabel-IM0055A:input-value       
                   ext-historico-embarque.cod-itiner          = wgh-cod-itiner-IM0055A:input-value   
                   ext-historico-embarque.cod-pto-contr       = wgh-cod-pto-contr-IM0055A:input-value
                   ext-historico-embarque.embarque            = wgh-embarque-IM0055A:input-value          
                   ext-historico-embarque.sequencia           = wgh-seq-IM0055A:input-value
                   ext-historico-embarque.log-simples-remessa = wgh-embarque-simples-IM0055A:input-value.
        END.
    END.
END.

PROCEDURE tela-upc :
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
