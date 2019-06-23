 /*-----------------------------------------------------------------------
    File        : escd0606
    Purpose     : Inluir FLAG de gera DES
    Syntax      :

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i escd0182-u01.p 2.06.00.000}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

/* ###########           Parametros obrigatorios para UPC.        ########## */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.
/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

/* ###########           Definicao das Variavel Globais.         ########## */
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-perc-red-ipi     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-aliquota-icm     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-nat-operacao     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-cb-trib-icms     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-subs-trib        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-perc-red-icms    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-ind-it-icms      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-escd0606-btSave           AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-txt-CST-IPI               AS WIDGET-HANDLE NO-UNDO.    
DEFINE NEW GLOBAL SHARED VARIABLE wh-bt-Save                   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-txt-CST-ICMS              AS WIDGET-HANDLE NO-UNDO.    
DEFINE NEW GLOBAL SHARED VARIABLE wh-cst-icms                  AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cst-ipi                   AS HANDLE        NO-UNDO.

/* ##### ##### ##### ##### ##### #####  ##### ##### ##### ##### ##### ##### */

DEFINE VARIABLE c-handle-obj AS CHAR NO-UNDO.

/* ###########           Mapeador de EndereØos de Objeos         ########## */

/*##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####*/
/*      MESSAGE "p-ind-event..:" p-ind-event                  SKIP */
/*              "p-ind-object.:" p-ind-object                 SKIP */
/*              "p-cod-table..:" STRING(p-cod-table)          SKIP */
/*              "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
/*              "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
/* ????????????????????????????????????????????????????????????????*/

FIND FIRST param-global no-lock no-error.
IF  param-global.empresa-prin <> "211" THEN RETURN.

 IF  p-ind-object = "CONTAINER"        AND 
     p-ind-event = "BEFORE-INITIALIZE" THEN DO:

     ASSIGN c-handle-obj = fc-handle-obj("perc-red-ipi,aliquota-icm,nat-operacao,cb-trib-icms,subs-trib,perc-red-icm,ind-it-icms,btSave",p-wgh-frame).
     ASSIGN wh-escd0606-perc-red-ipi   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
            wh-escd0606-aliquota-icm   = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
            wh-escd0606-nat-operacao   = WIDGET-HANDLE(ENTRY(3,c-handle-obj))
            wh-escd0606-cb-trib-icms   = WIDGET-HANDLE(ENTRY(4,c-handle-obj))
            wh-escd0606-subs-trib      = WIDGET-HANDLE(ENTRY(5,c-handle-obj)) 
            wh-escd0606-perc-red-icms  = WIDGET-HANDLE(ENTRY(6,c-handle-obj))
            wh-escd0606-ind-it-icms    = WIDGET-HANDLE(ENTRY(7,c-handle-obj))
            wh-escd0606-btSave         = WIDGET-HANDLE(ENTRY(8,c-handle-obj)).
    
    CREATE TEXT wh-txt-CST-IPI
    ASSIGN ROW          = wh-escd0606-perc-red-ipi:ROW + 0.15
           COLUMN       = wh-escd0606-perc-red-ipi:COLUMN + 15
           FRAME        = wh-escd0606-perc-red-ipi:FRAME
           DATA-TYPE    = "CHARACTER"
           FORMAT       = "x(5)"
           SCREEN-VALUE = "CST:"
           VISIBLE      = TRUE.

    CREATE fill-in wh-cst-ipi
    ASSIGN ROW       = wh-escd0606-perc-red-ipi:ROW
           COLUMN    = wh-txt-CST-IPI:COLUMN + 4
           WIDTH     = 3
           HEIGHT    = 0.88
           DATA-TYPE = "INTEGER"
           FORMAT    = '99'
           FRAME     = wh-escd0606-perc-red-ipi:FRAME
           SIDE-LABEL-HANDLE = wh-txt-CST-IPI
           SENSITIVE = NO
           VISIBLE   = TRUE
           TOOLTIP   = "Informar o C¢digo CST do IPI"
    TRIGGERS:
        ON 'mouse-select-dblclick':U PERSISTENT RUN upc\escd0606-U04.p.
        ON 'F5':U                    PERSISTENT RUN upc\escd0606-U04.p.
    end triggers.

    CREATE TEXT wh-txt-CST-ICMS
    ASSIGN ROW          = wh-escd0606-aliquota-icm:ROW + 0.15
           COLUMN       = wh-escd0606-aliquota-icm:COLUMN + 10
           FRAME        = wh-escd0606-aliquota-icm:FRAME
           DATA-TYPE    = "CHARACTER"
           FORMAT       = "x(5)"
           SCREEN-VALUE = "CST:"
           VISIBLE      = TRUE.

    CREATE fill-in wh-cst-icms
    ASSIGN ROW       = wh-escd0606-aliquota-icm:ROW
           COLUMN    = wh-txt-CST-ICMS:COLUMN + 4
           WIDTH     = 3
           HEIGHT    = 0.88
           DATA-TYPE = "INTEGER"
           FORMAT    = '99'
           FRAME     = wh-escd0606-aliquota-icm:FRAME
           SIDE-LABEL-HANDLE = wh-txt-CST-ICMS
           SENSITIVE = NO
           VISIBLE   = TRUE
           TOOLTIP   = "Informar o C¢digo CST do ICMS"
        TRIGGERS:
            ON 'mouse-select-dblclick':U PERSISTENT RUN upc\escd0606-U03.p.
            ON 'F5':U                    PERSISTENT RUN upc\escd0606-U03.p.
        end triggers.

    IF VALID-HANDLE(wh-cst-icms) THEN
       if wh-cst-icms:LOAD-MOUSE-POINTER("image/lupa.cur") THEN.

    IF VALID-HANDLE(wh-cst-ipi) THEN
       if wh-cst-ipi:LOAD-MOUSE-POINTER("image/lupa.cur") THEN.

   CREATE BUTTON wh-bt-Save
   ASSIGN ROW               = wh-escd0606-btSave:ROW
          COLUMN            = wh-escd0606-btSave:COLUMN
          WIDTH             = wh-escd0606-btSave:WIDTH
          HEIGHT            = wh-escd0606-btSave:height
          FRAME             = wh-escd0606-btSave:FRAME
          label             = wh-escd0606-btSave:LABEL
          SENSITIVE         = wh-escd0606-btSave:SENSITIVE
          VISIBLE           = wh-escd0606-btSave:VISIBLE
          TOOLTIP           = wh-escd0606-btSave:TOOLTIP.

   ON CHOOSE OF wh-bt-Save PERSISTENT RUN upc/escd0606-u02.p (INPUT "VALIDATE",
                                                              INPUT 'CONTAINER',
                                                              INPUT p-wgh-object,
                                                              INPUT p-wgh-frame,
                                                              INPUT p-cod-table,
                                                              INPUT p-row-table).
   wh-bt-save:LOAD-IMAGE-UP("image\im-sav.bmp").
   wh-bt-save:LOAD-IMAGE-INSENSITIVE("image\ii-sav.bmp").

   assign wh-escd0606-btSave:visible = no.

   /* wh-bt-save:LOAD-IMAGE-UP("image\im-inf.bmp").    */
END.

IF  p-ind-object = "CONTAINER"      AND 
    p-ind-event  = "BEFORE-ENABLE"  THEN DO:

    ASSIGN wh-cst-ipi:SENSITIVE  = YES
           wh-cst-icms:SENSITIVE = YES
           wh-bt-Save:SENSITIVE  = wh-escd0606-btSave:SENSITIVE.

END.

IF  p-ind-object = "CONTAINER"      AND 
    p-ind-event  = "AFTER-DISPLAY"  THEN DO:

    /*** ICMS ***/
    FIND FIRST ext-nat-oper-sefaz no-lock
         WHERE ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
           AND ext-nat-oper-sefaz.imposto      = 1 NO-ERROR.
    if avail ext-nat-oper-sefaz THEN
        ASSIGN wh-cst-icms:SCREEN-VALUE = STRING(ext-nat-oper-sefaz.codigo).
    ELSE
       ASSIGN wh-cst-icms:SCREEN-VALUE = '00'.

    /*** IPI ***/
    FIND FIRST ext-nat-oper-sefaz no-lock
         WHERE ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
           AND ext-nat-oper-sefaz.imposto      = 2 NO-ERROR.
    if avail ext-nat-oper-sefaz THEN
       ASSIGN wh-cst-ipi:SCREEN-VALUE  = STRING(ext-nat-oper-sefaz.codigo).
    ELSE
       ASSIGN wh-cst-ipi:SCREEN-VALUE  = '00'.
END.

IF  p-ind-object = "CONTAINER"          AND 
    p-ind-event  = "VALIDATE" THEN DO:

/*     MESSAGE 'Nat: ' wh-escd0606-nat-operacao:SCREEN-VALUE SKIP              */
/*             'Trib: ' wh-escd0606-cb-trib-icms:SCREEN-VALUE SKIP             */
/*             'Subs: '  wh-escd0606-subs-trib:SCREEN-VALUE SKIP               */
/*             /*'Perc: '  DEC(wh-escd0606-perc-red-icms:SCREEN-VALUE) SKIP */ */
/*             'Sub Ant: ' wh-escd0606-ind-it-icms:SCREEN-VALUE SKIP           */
/*             'IPI: ' wh-cst-ipi:SCREEN-VALUE SKIP                            */
/*             'ICMS: ' wh-cst-icms:SCREEN-VALUE                               */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */

    FIND FIRST es-cst-sefaz NO-LOCK
         WHERE es-cst-sefaz.codigo  = INT(wh-cst-icms:SCREEN-VALUE)
           AND es-cst-sefaz.imposto = 1 NO-ERROR.
    IF NOT AVAIL es-cst-sefaz THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "CST Inv†lida" + "~~" +
                           "CST " + wh-cst-icms:SCREEN-VALUE + " n∆o cadastrada.":u).
        RETURN "NOK":U.    
    END.

    FIND FIRST es-cst-sefaz NO-LOCK
         WHERE es-cst-sefaz.codigo  = INT(wh-cst-ipi:SCREEN-VALUE)
           AND es-cst-sefaz.imposto = 2 NO-ERROR.
    IF NOT AVAIL es-cst-sefaz THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "CST Inv†lida" + "~~" +
                           "CST " + wh-cst-ipi:SCREEN-VALUE + " n∆o cadastrada.":u).
        RETURN "NOK":U.    
    END.

    CASE INT(wh-cst-icms:SCREEN-VALUE):
        WHEN 00 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE <> 'Tributado' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 00 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a TRIBUTADO.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 10 THEN DO:
            IF wh-escd0606-subs-trib:SCREEN-VALUE     = "N∆o"       OR
               wh-escd0606-cb-trib-icms:SCREEN-VALUE <> 'Tributado' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 10 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a TRIBUTADO e Substituiá∆o Tribut†ria selecionado.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 20 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE       <> 'Tributado' OR
               DEC(wh-escd0606-perc-red-icms:SCREEN-VALUE) = 0     THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 20 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a TRIBUTADO e % Reduá∆o ICMS diferente de 0 .":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 30 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE  <> 'Isento' OR
               wh-escd0606-subs-trib:SCREEN-VALUE      = "N∆o"    THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 30 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a ISENTO e Substituiá∆o Tribut†ria selecionado.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 40 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE  <> 'Isento' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 40 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a ISENTO.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 51 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE  <> 'Diferido' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 51 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a DIFERIDO.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 60 THEN DO:
            IF wh-escd0606-ind-it-icms:SCREEN-VALUE  = 'N∆o' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 60 pode ser utilizado somente quando Item ICMS Cobrado Subs Tribut†ria estiver selecionado.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 70 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE       <> 'Tributado' OR
               DEC(wh-escd0606-perc-red-icms:SCREEN-VALUE) <> 0           OR
               wh-escd0606-subs-trib:SCREEN-VALUE           = "N∆o"       THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 70 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a TRIBUTADO e houver Reduá∆o ICMS e Subs Tribut†ria estiver selecionado.":u).
                RETURN "NOK":U.    
            END.
        END.
        WHEN 90 THEN DO:
            IF wh-escd0606-cb-trib-icms:SCREEN-VALUE       <> 'Outros' THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U, 
                                   INPUT 17006, 
                                   INPUT "CST Inv†lida para ICMS" + "~~" +
                                   "CST 90 pode ser utilizado somente quando C¢d Tributaá∆o ICMS igual a OUTROS.":u).
                RETURN "NOK":U.    
            END.
        END.
    END CASE.
    
    /*** ICMS ***/
    FIND FIRST ext-nat-oper-sefaz EXCLUSIVE-LOCK
         WHERE ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
           AND ext-nat-oper-sefaz.imposto      = 1 NO-ERROR.
    if not avail ext-nat-oper-sefaz then DO:
       CREATE ext-nat-oper-sefaz.
       ASSIGN ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
              ext-nat-oper-sefaz.imposto      = 1
              ext-nat-oper-sefaz.codigo       = INT(wh-cst-icms:SCREEN-VALUE).
    END.
    ELSE 
        ASSIGN ext-nat-oper-sefaz.codigo      = INT(wh-cst-icms:SCREEN-VALUE). 

    /*** IPI ***/
    FIND FIRST ext-nat-oper-sefaz EXCLUSIVE-LOCK
         WHERE ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
           AND ext-nat-oper-sefaz.imposto      = 2 NO-ERROR.
    if not avail ext-nat-oper-sefaz then DO:
       CREATE ext-nat-oper-sefaz.
       ASSIGN ext-nat-oper-sefaz.nat-operacao = wh-escd0606-nat-operacao:SCREEN-VALUE
              ext-nat-oper-sefaz.imposto      = 2
              ext-nat-oper-sefaz.codigo       = INT(wh-cst-ipi:SCREEN-VALUE).
    END.
    ELSE
        ASSIGN ext-nat-oper-sefaz.codigo       = INT(wh-cst-ipi:SCREEN-VALUE). 

    APPLY "choose" TO wh-escd0606-btSave.
END.

IF  p-ind-object = "CONTAINER"     AND 
    p-ind-event  = "AFTER-DISABLE" THEN DO:

    ASSIGN wh-cst-ipi:SENSITIVE  = NO
           wh-cst-icms:SENSITIVE = NO
           wh-bt-Save:SENSITIVE  = wh-escd0606-btSave:SENSITIVE.

END.
