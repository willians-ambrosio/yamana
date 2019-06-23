/**============================================================**
** Alteraá∆o...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Manutená∆o CFOP e modelo NFe/CTe
** ............:  
**=============================================================**/
{include/i-prgvrs.i upcre1001b1-u01 11.5.11.002}
{utp/ut-glob.i}

/** ParÉmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE VARIABLE hnumero-ordem   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcod-cfop-saida AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcod-model      AS HANDLE      NO-UNDO.
DEFINE VARIABLE haliquota-icm   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hemit-crt       AS HANDLE      NO-UNDO.
DEFINE VARIABLE htxt-Label      AS HANDLE      NO-UNDO.
DEFINE VARIABLE htxt-Label-dif  AS HANDLE      NO-UNDO.
DEFINE VARIABLE hint-1          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbenef-icms     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbenef-piscof   AS HANDLE      NO-UNDO.

DEFINE VARIABLE hbtOK           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtOK_esp       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtSave         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtSave_esp     AS HANDLE      NO-UNDO.

DEFINE BUFFER bitem-doc-est FOR item-doc-est.

/** Global **/
DEFINE NEW GLOBAL SHARED VARIABLE wh-numero-ordem-re1001b1   AS HANDLE      NO-UNDO.

/**** Main Block ****/
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */

IF p-ind-event  = "AFTER-INITIALIZE"  AND
   p-ind-object = "CONTAINER"         AND
   p-cod-table  = "item-doc-est"      THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    /* Cria bot∆o especifico para validar */
    IF VALID-HANDLE(wh-numero-ordem-re1001b1) THEN DO:
        CREATE TEXT htxt-label-dif
         ASSIGN ROW          = wh-numero-ordem-re1001b1:ROW    + 0.2
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN + 51.6
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                FORMAT       = "x(10)"
                SENSITIVE    = NO
                VISIBLE      = YES
                FONT         = 1
                WIDTH        = 8
                SCREEN-VALUE = "Aliq ICMS:".
         CREATE FILL-IN haliquota-icm
         ASSIGN DATA-TYPE    = "DECIMAL"
                FORMAT       = ">>9.99"
                ROW          = wh-numero-ordem-re1001b1:ROW   
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN + 59
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                SENSITIVE    = YES 
                VISIBLE      = YES
                FONT         = 1
                HEIGHT-CHARS = 0.88
                WIDTH        = 7
                NAME         = "aliquota-icm"
                HELP         = "*Al°quota ICMS DANFE para calc dif aliquota"
                TOOLTIP      = "*Al°quota ICMS DANFE para calc dif aliquota"
                SIDE-LABEL-HANDLE = htxt-Label-dif
                LABEL             = "Aliq ICMS:".

        CREATE TEXT htxt-label
         ASSIGN ROW          = wh-numero-ordem-re1001b1:ROW    + 1.2
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN - 11.6
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                FORMAT       = "x(13)"
                SENSITIVE    = NO
                VISIBLE      = YES
                FONT         = 1
                WIDTH        = 11.5
                SCREEN-VALUE = "CFOP NFe/CTe:".
         CREATE FILL-IN hcod-cfop-saida
         ASSIGN DATA-TYPE    = "CHARACTER"
                FORMAT       = "x(10)"
                ROW          = wh-numero-ordem-re1001b1:ROW   + 1
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN 
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                SENSITIVE    = YES 
                VISIBLE      = YES
                FONT         = 1
                HEIGHT-CHARS = 0.88
                WIDTH        = 11.57
                NAME         = "cod-cfop-saida"
                SIDE-LABEL-HANDLE = htxt-Label
                LABEL             = "CFOP NFe/CTe:".

        CREATE TEXT htxt-label
         ASSIGN ROW          = wh-numero-ordem-re1001b1:ROW    + 1.2
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN + 13.57
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                FORMAT       = "x(15)"
                SENSITIVE    = NO
                VISIBLE      = YES
                FONT         = 1
                WIDTH        = 13.5
                SCREEN-VALUE = "Modelo NFe/CTe:".

         CREATE RADIO-SET hcod-model
             ASSIGN ROW               = (wh-numero-ordem-re1001b1:ROW    + 1.2)
                    COLUMN            = (wh-numero-ordem-re1001b1:COLUMN + 26.57)
                    FRAME             = wh-numero-ordem-re1001b1:FRAME
                    SENSITIVE         = YES
                    VISIBLE           = NO
                    HEIGHT-CHARS      = 0.88
                    WIDTH-CHARS       = 35
                    NAME              = "cod-model-nfe-eletro"
                    HORIZONTAL        = TRUE
                    AUTO-RESIZE       = TRUE
                    RADIO-BUTTONS     = 'N∆o Aplic†vel,,NFe,55,CTe,57'
                    SIDE-LABEL-HANDLE = htxt-Label
                    LABEL             = "Modelo NFe/CTe:"
                .
         CREATE COMBO-BOX hemit-crt
            ASSIGN ROW               = (wh-numero-ordem-re1001b1:ROW    + 1.2)
                   COL               = (hcod-model:COLUMN + 28)
                   FRAME             = wh-numero-ordem-re1001b1:FRAME
                   WIDTH             = 12
                   DATA-TYPE         = "INTEGER":U
                   FORMAT            = "99":U
                   LIST-ITEM-pairs   = 'Trib Simples,1,Trib Normal,3,Red. ICMS,4'
                   SENSITIVE         = TRUE
                   VISIBLE           = TRUE
                   NAME              = "emit-crt".
         ASSIGN hemit-crt:SCREEN-VALUE = "3".
         ASSIGN hcod-model:VISIBLE = TRUE.

         hcod-cfop-saida:MOVE-AFTER-TAB(hint-1).
         hcod-model:MOVE-AFTER-TAB(hcod-cfop-saida).


        CREATE TEXT htxt-label
         ASSIGN ROW          = wh-numero-ordem-re1001b1:ROW    + 5.2
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN 
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                FORMAT       = "x(43)"
                SENSITIVE    = NO
                VISIBLE      = YES
                FONT         = 1
                WIDTH        = 35.5
                SCREEN-VALUE = "Benef°cios de ICMS informados na NF:". 
                                                                          
         CREATE RADIO-SET hbenef-icms
             ASSIGN ROW               = (wh-numero-ordem-re1001b1:ROW    + 5.2)
                    COLUMN            = (wh-numero-ordem-re1001b1:COLUMN + 31.5)
                    FRAME             = wh-numero-ordem-re1001b1:FRAME
                    SENSITIVE         = YES
                    VISIBLE           = NO
                    HEIGHT-CHARS      = 0.88
                    WIDTH-CHARS       = 35
                    NAME              = "benef-icms"
                    HORIZONTAL        = TRUE
                    AUTO-RESIZE       = TRUE
                    RADIO-BUTTONS     = 'N∆o,No,Sim,Yes'
                    SIDE-LABEL-HANDLE = htxt-Label
                    LABEL             = "Benef°cios de ICMS informados na NF:"
                .

        CREATE TEXT htxt-label
         ASSIGN ROW          = wh-numero-ordem-re1001b1:ROW    + 6.2
                COLUMN       = wh-numero-ordem-re1001b1:COLUMN 
                FRAME        = wh-numero-ordem-re1001b1:FRAME
                FORMAT       = "x(43)"
                SENSITIVE    = NO
                VISIBLE      = YES
                FONT         = 1
                WIDTH        = 35.5
                SCREEN-VALUE = "Benef°cios de PIS/COFINS informados na NF:".
        CREATE RADIO-SET hbenef-piscof
            ASSIGN ROW               = (wh-numero-ordem-re1001b1:ROW    + 6.2)
                   COLUMN            = (wh-numero-ordem-re1001b1:COLUMN + 31.5)
                   FRAME             = wh-numero-ordem-re1001b1:FRAME
                   SENSITIVE         = YES
                   VISIBLE           = NO
                   HEIGHT-CHARS      = 0.88
                   WIDTH-CHARS       = 35
                   NAME              = "benef-piscof"
                   HORIZONTAL        = TRUE
                   AUTO-RESIZE       = TRUE
                   RADIO-BUTTONS     = 'N∆o,No,Sim,Yes'
                   SIDE-LABEL-HANDLE = htxt-Label
                   LABEL             = "Benef°cios de PIS/COFINS informados na NF:"
               .
        ASSIGN hbenef-icms  :SCREEN-VALUE = 'NO'
               hbenef-piscof:SCREEN-VALUE = 'NO'
               hbenef-icms  :VISIBLE      = TRUE
               hbenef-piscof:VISIBLE      = TRUE.

        /* Cria bot∆o especifico para validar */
        IF VALID-HANDLE(hbtOK) AND NOT VALID-HANDLE(hbtOK_esp)  THEN DO:
            CREATE BUTTON hbtOK_esp
                ASSIGN  ROW         = hbtOK:ROW  
                        COLUMN      = hbtOK:COLUMN
                        WIDTH       = hbtOK:WIDTH
                        HEIGHT      = hbtOK:HEIGHT
                        LABEL       = hbtOK:LABEL
                        FRAME       = hbtOK:FRAME
                        FLAT-BUTTON = hbtOK:FLAT-BUTTON
                        TOOLTIP     = "*" + hbtOK:TOOLTIP
                        HELP        = hbtOK:HELP
                        NAME        = "btOK_esp"
                        SENSITIVE   = hbtOK:SENSITIVE  
                        VISIBLE     = hbtOK:VISIBLE
                        CONVERT-3D-COLOR = hbtOK:convert-3D-COLOR
    /*                     NO-FOCUS    = hbtOK:NO-FOCUS */
                        .
            ON "CHOOSE" OF hbtOK_esp PERSISTENT RUN upc/upc-re1001b1-u01.p(INPUT "UPCRE1001B1-btOK_esp",
                                                                           INPUT p-ind-object,
                                                                           INPUT hbtOk,
                                                                           INPUT p-wgh-frame,
                                                                           INPUT p-cod-table,
                                                                           INPUT p-row-table).
            hbtOK_esp:LOAD-IMAGE-UP(hbtOK:IMAGE-UP).
            hbtOK_esp:LOAD-IMAGE-INSENSITIVE(hbtOK:IMAGE-INSENSITIVE).
            hbtOK_esp:MOVE-TO-TOP().
            hbtOK_esp:MOVE-AFTER-TAB-ITEM(hbtOK).
            ASSIGN hbtOK:TAB-STOP  = NO
                   hbtOK:SENSITIVE = NO
                   .
        END.

        /* Cria bot∆o especifico para validar */
        IF VALID-HANDLE(hbtSave) AND NOT VALID-HANDLE(hbtSave_esp)  THEN DO:
            CREATE BUTTON hbtSave_esp
                ASSIGN  ROW         = hbtSave:ROW  
                        COLUMN      = hbtSave:COLUMN
                        WIDTH       = hbtSave:WIDTH
                        HEIGHT      = hbtSave:HEIGHT
                        LABEL       = hbtSave:LABEL
                        FRAME       = hbtSave:FRAME
                        FLAT-BUTTON = hbtSave:FLAT-BUTTON
                        TOOLTIP     = "*" + hbtSave:TOOLTIP
                        HELP        = hbtSave:HELP
                        NAME        = "btSave_esp"
                        SENSITIVE   = hbtSave:SENSITIVE  
                        VISIBLE     = hbtSave:VISIBLE
                        CONVERT-3D-COLOR = hbtSave:convert-3D-COLOR
                        .
            ON "CHOOSE" OF hbtSave_esp PERSISTENT RUN upc/upc-re1001b1-u01.p(INPUT "UPCRE1001B1-btSave_esp",
                                                                           INPUT p-ind-object,
                                                                           INPUT hbtSave,
                                                                           INPUT p-wgh-frame,
                                                                           INPUT p-cod-table,
                                                                           INPUT p-row-table).
            hbtSave_esp:LOAD-IMAGE-UP(hbtSave:IMAGE-UP).
            hbtSave_esp:LOAD-IMAGE-INSENSITIVE(hbtSave:IMAGE-INSENSITIVE).
            hbtSave_esp:MOVE-TO-TOP().
            hbtSave_esp:MOVE-AFTER-TAB-ITEM(hbtSave).
            ASSIGN hbtSave:TAB-STOP  = NO
                   hbtSave:SENSITIVE = NO
                   .
        END.

        RUN pi-acao-return (INPUT p-wgh-frame) .

    END.
END. /* p-ind-event  = "BEFORE-INITIALIZE" */

/* Validar */
IF p-ind-event = "UPCRE1001B1-btSave_esp" OR
   p-ind-event = "UPCRE1001B1-btOK_esp"   THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    IF TRIM(hcod-cfop-saida:SCREEN-VALUE) = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "CFOP NFe/CTe inv†lido~~" +
                                 "CFOP NFe/CTe Ç obrigat¢rio e dever† ser informado um valor.").
        APPLY "ENTRY"  TO hcod-cfop-saida.
        RETURN "NOK".
    END.

    APPLY "CHOOSE" TO p-wgh-object.
END.

/* DETALHAR X SALVE */
IF p-ind-event  = "AFTER-SAVE-FIELDS" AND
   p-ind-object = "CONTAINER"         AND
   p-cod-table  = "item-doc-est"      THEN
DO:
    /*Begins 14/05/2018 - Para evitar o bloqueio de saldo nos contratos de mediá∆o dever† liberar o saldo do contrato de mediá∆o neste ponto */       
    FIND FIRST ordem-compra WHERE
               ordem-compra.numero-ordem =  INT(wh-numero-ordem-re1001b1:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAIL ordem-compra THEN
       FIND FIRST contrato-for where
                  contrato-for.nr-contrato = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
    IF AVAIL contrato-for THEN
    DO:            
        if contrato-for.ind-control-rec = 2 then
        do:        
            find item-contrat
                where item-contrat.num-seq-item = ordem-compra.num-seq-item
                  and item-contrat.nr-contrato  = ordem-compra.nr-contrato
                no-lock no-error.
        end.


        if contrato-for.ind-control-rec = 2 and avail item-contrat then
        do:
            for FIRST medicao-contrat use-index dat-prev
               where medicao-contrat.numero-ordem  = ordem-compra.numero-ordem
                 and medicao-contrat.ind-sit-medicao = 2
/*                      and medicao-contrat.dat-prev-medicao <= docum-est.dt-emissao */
                 and medicao-contrat.log-rec-medicao = NO EXCLUSIVE-LOCK:

                ASSIGN medicao-contrat.val-sdo-aloc-med = 0.

            END.
        END.
    END.    
    /* end 14/05/2018 */   
END.


/* Salvar */
IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    AND
   p-cod-table  = "item-doc-est" AND
   p-row-table <> ?              THEN DO:


        
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    FOR FIRST bitem-doc-est FIELDS(serie-docto nro-docto cod-emitente nat-operacao sequencia it-codigo)
        WHERE ROWID(bitem-doc-est) = p-row-table :

        FIND FIRST ordem-compra WHERE 
                   ordem-compra.numero-ordem = INT(wh-numero-ordem-re1001b1:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN
        DO:       
            /* Begins: 10/05/2018 - Gravando o numero da ordem, para poder utilizar no re1001 */
            ASSIGN bitem-doc-est.numero-ordem = INT(wh-numero-ordem-re1001b1:SCREEN-VALUE)
                   bitem-doc-est.num-pedido   = ordem-compra.num-pedido.

            FIND FIRST rat-ordem OF ordem-compra NO-LOCK NO-ERROR.
            IF AVAIL rat-ordem THEN
               ASSIGN bitem-doc-est.parcela = rat-ordem.parcela.
            /* end: 10/05/2018 - Gravando o numero da ordem, para poder utilizar no re1001 */
        END.

        FIND FIRST es-item-doc-est-natoper
            WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
              AND es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto
              AND es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto
              AND es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
              AND es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
              AND es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia NO-ERROR.
        IF NOT AVAIL es-item-doc-est-natoper THEN DO:
            CREATE es-item-doc-est-natoper.
            ASSIGN es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario       
                   es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto 
                   es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto   
                   es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
                   es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
                   es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia
                   es-item-doc-est-natoper.it-codigo    = bitem-doc-est.it-codigo.
        END.
        ASSIGN es-item-doc-est-natoper.cod-model-nf-eletro = hcod-model:SCREEN-VALUE
               es-item-doc-est-natoper.cod-cfop-saida      = hcod-cfop-saida:SCREEN-VALUE
               es-item-doc-est-natoper.emit-crt            = INT(hemit-crt:SCREEN-VALUE)
               es-item-doc-est-natoper.aliquota-icm        = DEC(haliquota-icm:SCREEN-VALUE).
        /* Gravar aliquota ICMS igual especifico */
        IF DEC(haliquota-icm:SCREEN-VALUE) > 0 OR (INT(hemit-crt:SCREEN-VALUE) = 1 AND DEC(haliquota-icm:SCREEN-VALUE) = 0)  THEN
            ASSIGN bitem-doc-est.aliquota-icm = DEC(haliquota-icm:SCREEN-VALUE).

        IF hbenef-icms:SCREEN-VALUE = "YES" THEN DO:
            FOR FIRST es-natoper-rec 
                WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK:
                ASSIGN es-item-doc-est-natoper.cod-beneficio-icms = es-natoper-rec.cod-benef-icms-rec.
            END.
        END.
        ELSE
            ASSIGN es-item-doc-est-natoper.cod-beneficio-icms = 0. 
        IF hbenef-piscof:SCREEN-VALUE = "YES" THEN DO:
            FOR FIRST es-natoper-rec 
                WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK:
                ASSIGN es-item-doc-est-natoper.cod-beneficio-piscof = es-natoper-rec.cod-benef-pis-cofins-rec.
            END.
        END.
        ELSE
            ASSIGN es-item-doc-est-natoper.cod-beneficio-piscof = 0. 

    END. /* FIRST bitem-doc-est */

    ASSIGN hcod-cfop-saida:SCREEN-VALUE = ""
           hcod-model:SCREEN-VALUE      = ""
           hemit-crt:SCREEN-VALUE       = "3"
           haliquota-icm:SCREEN-VALUE   = ""
           hbenef-icms:SCREEN-VALUE     = "NO"
           hbenef-piscof:SCREEN-VALUE   = "NO".


END. /* p-ind-event  = "AFTER-ASSIGN" */

RETURN "OK".
/**** END Main Block ****/

PROCEDURE pi-LocalizaCampo:
    DEFINE INPUT  PARAMETER ph_frame AS HANDLE     NO-UNDO.

    ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    blk_frame:
    DO WHILE ph_frame <> ?:
       IF ph_frame:TYPE <> "field-group" THEN DO:
          CASE ph_frame:TYPE:
             WHEN "frame" THEN DO:
                 IF  ph_frame:NAME = "fPage1" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
/*                  IF ph_frame:NAME = "fPage1" THEN LEAVE blk_frame. */
             END.
             WHEN "FILL-IN" THEN DO:
                 IF ph_frame:NAME = "numero-ordem"   THEN ASSIGN wh-numero-ordem-re1001b1     = ph_frame.
                 IF ph_frame:NAME = "int-1"          THEN ASSIGN hint-1            = ph_frame.
                 IF ph_frame:NAME = "cod-cfop-saida" THEN ASSIGN hcod-cfop-saida   = ph_frame.
                 IF ph_frame:NAME = "aliquota-icm"   THEN ASSIGN haliquota-icm     = ph_frame.
             END.
             WHEN "RADIO-SET" THEN DO:
                 IF ph_frame:NAME = "cod-model-nfe-eletro"  THEN ASSIGN hcod-model    = ph_frame.
                 IF ph_frame:NAME = "benef-icms"            THEN ASSIGN hbenef-icms   = ph_frame.
                 IF ph_frame:NAME = "benef-piscof"          THEN ASSIGN hbenef-piscof = ph_frame.
             END.
             WHEN "COMBO-BOX" THEN DO:
                 IF ph_frame:NAME = "emit-crt"  THEN ASSIGN hemit-crt = ph_frame.
             END.
             WHEN "BUTTON" THEN DO:
                 IF ph_frame:NAME = "btOK"        THEN ASSIGN hbtOK       = ph_frame.
                 IF ph_frame:NAME = "btOK_esp"    THEN ASSIGN hbtOK_esp   = ph_frame.
                 IF ph_frame:NAME = "btSave"      THEN ASSIGN hbtSave     = ph_frame.
                 IF ph_frame:NAME = "btSave_esp"  THEN ASSIGN hbtSave_esp = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */

PROCEDURE pi-acao-return:
    DEFINE INPUT  PARAMETER ph_frame AS HANDLE     NO-UNDO.

    ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    blk_frame:
    DO WHILE ph_frame <> ?:
       IF ph_frame:TYPE <> "field-group" THEN DO:
          CASE ph_frame:TYPE:
             WHEN "frame" THEN DO:
                RUN pi-acao-return(INPUT ph_frame).                 
             END.
             WHEN "FILL-IN" THEN DO:
                ON "RETURN" OF ph_frame PERSISTENT RUN upc/upc-re1001b1-u01.p(INPUT "UPCRE1001B1-btSave_esp",
                                                                               INPUT p-ind-object,
                                                                               INPUT hbtSave,
                                                                               INPUT p-wgh-frame,
                                                                               INPUT p-cod-table,
                                                                               INPUT p-row-table).

             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */

