
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-cd-pis         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-aliq-pis       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-vl-base-pis    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-vl-pis         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-cd-cofins      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-aliq-cofins    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-vl-base-cofins AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re0101-pd-vl-cofins      AS WIDGET-HANDLE NO-UNDO.

DEFINE BUFFER bf_param-re     FOR param-re.
DEFINE BUFFER bf_ext-param-re FOR ext-param-re.

/* -----------------[  Defini‡Æo de Vari veis  ]----------------------*/
DEF VAR wh-objeto AS WIDGET-HANDLE   NO-UNDO.

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
   p-ind-object              = 'VIEWER'            AND 
   p-wgh-object:FILE-NAME    = 'invwr/v02in292.w' THEN DO:
    
    ASSIGN wh-objeto = p-wgh-frame:FIRST-CHILD.
    DO WHILE VALID-HANDLE(wh-objeto):

        IF wh-objeto:NAME = 'pd-aliq-icms'  THEN DO:
            ASSIGN wh-re0101-pd-cd-pis = wh-objeto.
            CREATE TOGGLE-BOX wh-re0101-pd-cd-pis
            ASSIGN COL        = 70.5
                   ROW        = wh-objeto:ROW
                   WIDTH      = 12
                   FRAME      = wh-objeto:FRAME
                   SENSITIVE  = NO
                   NAME       = "pd-cd-pis"
                   LABEL      = "Trib PIS".

            wh-re0101-pd-cd-pis:SCREEN-VALUE = STRING(NO).

        END.
        IF wh-objeto:NAME = 'pd-cd-icms'  THEN DO:
            ASSIGN wh-re0101-pd-aliq-pis = wh-objeto.
            CREATE TOGGLE-BOX wh-re0101-pd-aliq-pis
            ASSIGN COL        = 70.5
                   ROW        = wh-objeto:ROW
                   WIDTH      = 12
                   FRAME      = wh-objeto:FRAME
                   SENSITIVE  = NO
                   NAME       = "pd-aliq-pis"
                   LABEL      = "Aliq PIS".

            wh-re0101-pd-aliq-pis:SCREEN-VALUE = STRING(NO).

        END.
        IF wh-objeto:NAME = 'pd-aliq-ipi'    THEN DO:
                ASSIGN wh-re0101-pd-vl-base-pis = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-vl-base-pis
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-vl-base-pis"
                       LABEL      = "Base PIS".

                wh-re0101-pd-vl-base-pis:SCREEN-VALUE  = STRING(NO).                
        END.


        IF wh-objeto:NAME = 'pd-cd-ipi'   THEN DO:
                ASSIGN wh-re0101-pd-vl-pis = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-vl-pis
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-vl-pis"
                       LABEL      = "Vl PIS".
                
                wh-re0101-pd-vl-pis:SCREEN-VALUE  = STRING(NO).
        END.

        IF wh-objeto:NAME = 'ped-imp-nota'     THEN DO:
                ASSIGN wh-re0101-pd-cd-cofins = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-cd-cofins
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-cd-cofins"
                       LABEL      = "Trib COFINS".
                
                wh-re0101-pd-cd-cofins:SCREEN-VALUE  = STRING(NO).
        END.
        IF wh-objeto:NAME = 'pd-cf-item'     THEN DO:
                ASSIGN wh-re0101-pd-aliq-cofins = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-aliq-cofins
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-aliq-cofins"
                       LABEL      = "Aliq COFINS".
                
                wh-re0101-pd-aliq-cofins:SCREEN-VALUE  = STRING(NO).
        END.
        IF wh-objeto:NAME = 'perc-red-ipi'  THEN DO:
                ASSIGN wh-re0101-pd-vl-base-cofins = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-vl-base-cofins
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-vl-base-cofins"
                       LABEL      = "Base COFINS".
                
                wh-re0101-pd-vl-base-cofins:SCREEN-VALUE  = STRING(NO).
        END.
        IF wh-objeto:NAME = 'perc-red-icm'    THEN DO:
                ASSIGN wh-re0101-pd-vl-cofins = wh-objeto.
                CREATE TOGGLE-BOX wh-re0101-pd-vl-cofins
                ASSIGN COL        = 70.5
                       ROW        = wh-objeto:ROW
                       WIDTH      = 12
                       FRAME      = wh-objeto:FRAME
                       SENSITIVE  = NO
                       NAME       = "pd-vl-cofins"
                       LABEL      = "Vl COFINS".
                
                wh-re0101-pd-vl-cofins:SCREEN-VALUE  = STRING(NO).
        END.

                  
        IF wh-objeto:TYPE = "field-group" THEN DO:
           ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
        END.
        ELSE DO:
           ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
        END. 
    END.
END.


IF p-ind-event               = 'DISPLAY' AND 
   p-ind-object              = 'VIEWER'            AND 
   p-wgh-object:FILE-NAME    = 'invwr/v02in292.w' THEN DO:

    FIND FIRST bf_param-re WHERE ROWID(bf_param-re) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL bf_param-re THEN DO:
        FIND FIRST bf_ext-param-re OF bf_param-re EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL bf_ext-param-re THEN DO:
            CREATE bf_ext-param-re.
            ASSIGN bf_ext-param-re.usuario = bf_param-re.usuario.
        END.
    END.

    IF AVAIL bf_ext-param-re THEN DO:        
        ASSIGN wh-re0101-pd-cd-pis        :SCREEN-VALUE = STRING(bf_ext-param-re.pd-cd-pis)
               wh-re0101-pd-aliq-pis      :SCREEN-VALUE = STRING(bf_ext-param-re.pd-aliq-pis)
               wh-re0101-pd-vl-base-pis   :SCREEN-VALUE = STRING(bf_ext-param-re.pd-vl-base-pis)
               wh-re0101-pd-vl-pis        :SCREEN-VALUE = STRING(bf_ext-param-re.pd-vl-pis)
               wh-re0101-pd-cd-cofins     :SCREEN-VALUE = STRING(bf_ext-param-re.pd-cd-cofins)
               wh-re0101-pd-aliq-cofins   :SCREEN-VALUE = STRING(bf_ext-param-re.pd-aliq-cofins)
               wh-re0101-pd-vl-base-cofins:SCREEN-VALUE = STRING(bf_ext-param-re.pd-vl-base-cofins)
               wh-re0101-pd-vl-cofins     :SCREEN-VALUE = STRING(bf_ext-param-re.pd-vl-cofins).
    END.
END.

IF p-ind-event               = 'ENABLE' AND 
   p-ind-object              = 'VIEWER'            AND 
   p-wgh-object:FILE-NAME    = 'invwr/v02in292.w' THEN DO:
    
   ASSIGN wh-re0101-pd-cd-pis        :SENSITIVE = YES 
          wh-re0101-pd-aliq-pis      :SENSITIVE = YES 
          wh-re0101-pd-vl-base-pis   :SENSITIVE = YES 
          wh-re0101-pd-vl-pis        :SENSITIVE = YES 
          wh-re0101-pd-cd-cofins     :SENSITIVE = YES 
          wh-re0101-pd-aliq-cofins   :SENSITIVE = YES 
          wh-re0101-pd-vl-base-cofins:SENSITIVE = YES 
          wh-re0101-pd-vl-cofins     :SENSITIVE = YES.
   
END.

IF p-ind-event               = 'ASSIGN' AND 
   p-ind-object              = 'VIEWER'            AND 
   p-wgh-object:FILE-NAME    = 'invwr/v02in292.w' THEN DO:

    IF NOT AVAIL bf_ext-param-re THEN DO:
        FIND FIRST bf_param-re WHERE ROWID(bf_param-re) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL bf_param-re THEN DO:
            FIND FIRST bf_ext-param-re OF bf_param-re EXCLUSIVE-LOCK NO-ERROR.
        END.
    END.

    IF AVAIL bf_ext-param-re THEN DO:        
        ASSIGN bf_ext-param-re.pd-cd-pis         = LOGICAL(wh-re0101-pd-cd-pis        :SCREEN-VALUE) 
               bf_ext-param-re.pd-aliq-pis       = LOGICAL(wh-re0101-pd-aliq-pis      :SCREEN-VALUE) 
               bf_ext-param-re.pd-vl-base-pis    = LOGICAL(wh-re0101-pd-vl-base-pis   :SCREEN-VALUE) 
               bf_ext-param-re.pd-vl-pis         = LOGICAL(wh-re0101-pd-vl-pis        :SCREEN-VALUE) 
               bf_ext-param-re.pd-cd-cofins      = LOGICAL(wh-re0101-pd-cd-cofins     :SCREEN-VALUE) 
               bf_ext-param-re.pd-aliq-cofins    = LOGICAL(wh-re0101-pd-aliq-cofins   :SCREEN-VALUE) 
               bf_ext-param-re.pd-vl-base-cofins = LOGICAL(wh-re0101-pd-vl-base-cofins:SCREEN-VALUE) 
               bf_ext-param-re.pd-vl-cofins      = LOGICAL(wh-re0101-pd-vl-cofins     :SCREEN-VALUE).
    END.
END.
IF p-ind-event               = 'DISABLE' AND 
   p-ind-object              = 'VIEWER'            AND 
   p-wgh-object:FILE-NAME    = 'invwr/v02in292.w' THEN DO:
    
   ASSIGN wh-re0101-pd-cd-pis        :SENSITIVE = NO 
          wh-re0101-pd-aliq-pis      :SENSITIVE = NO 
          wh-re0101-pd-vl-base-pis   :SENSITIVE = NO 
          wh-re0101-pd-vl-pis        :SENSITIVE = NO 
          wh-re0101-pd-cd-cofins     :SENSITIVE = NO 
          wh-re0101-pd-aliq-cofins   :SENSITIVE = NO 
          wh-re0101-pd-vl-base-cofins:SENSITIVE = NO 
          wh-re0101-pd-vl-cofins     :SENSITIVE = NO .
   
END.


