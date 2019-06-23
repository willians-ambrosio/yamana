
/*-----------------------------------------------------------------------------------
    PROGRAMA : 
    OBJETIVO : 
    AUTOR    : Rog‚rio Dias (DSC)
    DATA     : 
-----------------------------------------------------------------------------------*/


/* -----------------[      Defini‡Æo de Parƒmetros    ]----------------------*/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.  
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* -----------------[    Defini‡Æo de Vari veis Globais     ]----------------------*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-cd-trib-pis     AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-aliq-pis      AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-calc-pis      AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-vl-pis        AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-cd-trib-cofins  AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-aliq-cofins   AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-calc-cofins   AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001b2-d-vl-cofins     AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario               AS CHARACTER     NO-UNDO.

DEFINE BUFFER bf_param-re     FOR param-re.
DEFINE BUFFER bf_ext-param-re FOR ext-param-re.

/* -----------------[  Defini‡Æo de Vari veis  ]----------------------*/
DEF VAR wh-objeto AS WIDGET-HANDLE   NO-UNDO.
DEF VAR wh-frmpage4 AS WIDGET-HANDLE   NO-UNDO.
DEF VAR wh-objeto2 AS WIDGET-HANDLE   NO-UNDO.

/*
MESSAGE   "Rotina"
          "p-ind-event..:" p-ind-event                       SKIP
          "p-ind-object.:" p-ind-object                      SKIP
          "p-cod-table..:" STRING(p-cod-table)               SKIP
          "p-wgh-object.:" p-wgh-object:PRIVATE-DATA         SKIP
          "p-wgh-frame..:" STRING(p-wgh-frame:name)          SKIP
          "P-row-table..:" STRING(p-row-table) VIEW-AS ALERT-BOX INFO BUTTONS OK.                         
*/

/* -----------------[   Criando Objetos   ]----------------------*/
IF p-ind-event               = 'BEFORE-INITIALIZE' AND 
   p-ind-object              = 'CONTAINER'           THEN DO:

    ASSIGN wh-objeto = p-wgh-frame:FIRST-CHILD.
    DO WHILE VALID-HANDLE(wh-objeto):
        IF wh-objeto:NAME = 'Fpage4' THEN DO:
            ASSIGN wh-objeto2 = wh-objeto:FIRST-CHILD
                   wh-frmpage4 = wh-objeto.
            DO WHILE VALID-HANDLE(wh-objeto2):
                IF wh-objeto2:NAME = 'cd-trib-pis'  THEN DO:    
                    ASSIGN wh-re1001b2-cd-trib-pis = wh-objeto2.
                END.
                IF wh-objeto2:NAME = 'd-aliq-pis'  THEN DO:
                    ASSIGN wh-re1001b2-d-aliq-pis = wh-objeto2.
                END.
                IF wh-objeto2:NAME = 'd-calc-pis'    THEN DO:
                    ASSIGN wh-re1001b2-d-calc-pis = wh-objeto2.                               
                END.
                IF wh-objeto2:NAME = 'd-vl-pis'   THEN DO:
                    ASSIGN wh-re1001b2-d-vl-pis = wh-objeto2.
                END.
                IF wh-objeto2:NAME = 'cd-trib-cofins'     THEN DO:
                    ASSIGN wh-re1001b2-cd-trib-cofins = wh-objeto2.
                END.
                IF wh-objeto2:NAME = 'd-aliq-cofins'     THEN DO:
                    ASSIGN wh-re1001b2-d-aliq-cofins = wh-objeto2.
                END.
                IF wh-objeto2:NAME = 'd-calc-cofins'     THEN DO:
                    ASSIGN wh-re1001b2-d-calc-cofins = wh-objeto2.
                END.                                            
                IF wh-objeto2:NAME = 'd-vl-cofins'     THEN DO:
                    ASSIGN wh-re1001b2-d-vl-cofins = wh-objeto2.
                END.
                IF wh-objeto2:TYPE = "field-group" THEN DO:
                    ASSIGN wh-objeto2 = wh-objeto2:FIRST-CHILD.
                END.
                ELSE DO:
                    ASSIGN wh-objeto2 = wh-objeto2:NEXT-SIBLING.
                END.
            END.
        END.

        IF wh-objeto:TYPE = "field-group" THEN DO:
           ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
        END.
        ELSE DO:
           ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
        END. 
    END.
END.

IF p-ind-event               = 'BEFORE-CHANGE-PAGE' AND 
   p-ind-object              = 'CONTAINER'             THEN DO:

    FIND FIRST ext-param-re WHERE ext-param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL ext-param-re THEN DO:
        IF NOT ext-param-re.pd-cd-pis THEN DO:
            ASSIGN wh-re1001b2-cd-trib-pis:SENSITIVE = NO.
        END.

        IF NOT ext-param-re.pd-aliq-pis THEN
            ASSIGN wh-re1001b2-d-aliq-pis:SENSITIVE = NO.

        IF NOT ext-param-re.pd-vl-base-pis THEN
            ASSIGN wh-re1001b2-d-calc-pis:SENSITIVE = NO.

        IF NOT ext-param-re.pd-vl-pis THEN
            ASSIGN wh-re1001b2-d-vl-pis:SENSITIVE = NO.

        IF NOT ext-param-re.pd-cd-cofins THEN
            ASSIGN wh-re1001b2-cd-trib-cofins:SENSITIVE = NO.

        IF NOT ext-param-re.pd-aliq-cofins THEN
            ASSIGN wh-re1001b2-d-aliq-cofins:SENSITIVE = NO.

        IF NOT ext-param-re.pd-vl-base-cofins THEN
            ASSIGN wh-re1001b2-d-calc-cofins:SENSITIVE = NO.

        IF NOT ext-param-re.pd-vl-cofins THEN
            ASSIGN wh-re1001b2-d-vl-cofins:SENSITIVE = NO.
    END.   
END.


