/**============================================================**
** Altera‡Æo...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar Natureza de Opera‡Æo
** ............:  
**=============================================================**/
{include/i-prgvrs.i U03-RE1001B2 11.5.11.002}
{utp/ut-glob.i}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE VARIABLE hnum-pedido     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcod-cfop-saida AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcod-model      AS HANDLE      NO-UNDO.
DEFINE VARIABLE htxt-Label      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hemit-crt       AS HANDLE      NO-UNDO.
DEFINE VARIABLE haliquota-icm   AS HANDLE      NO-UNDO.
DEFINE VARIABLE htxt-Label-dif  AS HANDLE      NO-UNDO.
DEFINE VARIABLE hnumero-ordem   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hobjeto         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbenef-icms     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbenef-piscof   AS HANDLE      NO-UNDO.

DEFINE BUFFER bitem-doc-est FOR item-doc-est.


/** Global **/


/**** Main Block ****/
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */


IF p-ind-event  = "BEFORE-INITIALIZE" AND
   p-ind-object = "CONTAINER"         AND
   p-cod-table  = "item-doc-est"      THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    IF VALID-HANDLE(hnumero-ordem) THEN DO:
        ASSIGN hobjeto              = hnumero-ordem:SIDE-LABEL-HANDLE.
        ASSIGN hnumero-ordem:COLUMN = hnumero-ordem:COLUMN - 30
               hobjeto:COLUMN       = hobjeto:COLUMN - 30.
        
    END.
        
    /* Cria botÆo especifico para validar */
    IF VALID-HANDLE(hnum-pedido) THEN DO:
        CREATE TEXT htxt-label
         ASSIGN ROW          = hnum-pedido:ROW    + 1.2
                COLUMN       = hnum-pedido:COLUMN - 11.6 
                FRAME        = hnum-pedido:FRAME
                FORMAT       = "x(13)"
                SENSITIVE    = NO
                VISIBLE      = NO
                FONT         = 1
                WIDTH        = 11.5
                SCREEN-VALUE = "CFOP NFe/CTe:".
        
         CREATE FILL-IN hcod-cfop-saida
         ASSIGN DATA-TYPE    = "CHARACTER"
                FORMAT       = "x(10)"
                ROW          = hnum-pedido:ROW   + 1
                COLUMN       = hnum-pedido:COLUMN 
                FRAME        = hnum-pedido:FRAME
                SENSITIVE    = NO 
                VISIBLE      = NO
                FONT         = 1
                HEIGHT-CHARS = 0.88
                WIDTH        = 8
                NAME         = "cod-cfop-saida"
                SIDE-LABEL-HANDLE = htxt-Label
                LABEL             = "CFOP NFe/CTe:".
        
        CREATE TEXT htxt-label
         ASSIGN ROW          = hnum-pedido:ROW    + 1.2
                COLUMN       = hnum-pedido:COLUMN + 8.3
                FRAME        = hnum-pedido:FRAME
                FORMAT       = "x(15)"
                SENSITIVE    = NO
                VISIBLE      = NO
                FONT         = 1
                WIDTH        = 12.5
                SCREEN-VALUE = "Modelo NFe/CTe:".

         CREATE RADIO-SET hcod-model
             ASSIGN ROW               = (hnum-pedido:ROW    + 1.2)
                    COLUMN            = (hnum-pedido:COLUMN + 21)
                    FRAME             = hnum-pedido:FRAME
                    SENSITIVE         = NO
                    VISIBLE           = NO
                    HEIGHT-CHARS      = 0.88
                    WIDTH-CHARS       = 35
                    NAME              = "cod-model-nfe-eletro"
                    HORIZONTAL        = TRUE
                    AUTO-RESIZE       = TRUE
                    RADIO-BUTTONS     = 'NÆo Aplic vel,,NFe,55,CTe,57'
                    SIDE-LABEL-HANDLE = htxt-Label
                    LABEL             = "Modelo NFe/CTe:"
                .
         CREATE COMBO-BOX hemit-crt
            ASSIGN ROW               = (hnum-pedido:ROW    + 1)
                   COL               = (hcod-model:COLUMN + 26.3)
                   FRAME             = hnum-pedido:FRAME
                   WIDTH             = 12
                   DATA-TYPE         = "INTEGER":U
                   FORMAT            = "99":U
                   LIST-ITEM-pairs   = 'Trib Simples,1,Trib Normal,3,Red. ICMS,4'
                   SENSITIVE         = NO
                   VISIBLE           = NO
                   NAME              = "emit-crt".
         ASSIGN hemit-crt:SCREEN-VALUE = "3".
/*          ASSIGN hcod-model:VISIBLE = TRUE. */

        CREATE TEXT htxt-label-dif
         ASSIGN ROW          = hnumero-ordem:ROW    + 0.2
                COLUMN       = hnumero-ordem:COLUMN + 15.6
                FRAME        = hnumero-ordem:FRAME
                FORMAT       = "x(10)"
                SENSITIVE    = NO
                VISIBLE      = NO
                FONT         = 1
                WIDTH        = 7
                SCREEN-VALUE = "Aliq ICMS:".
         CREATE FILL-IN haliquota-icm
         ASSIGN DATA-TYPE    = "DECIMAL"
                FORMAT       = ">>9.99"
                ROW          = hnumero-ordem:ROW   
                COLUMN       = hnumero-ordem:COLUMN + 23
                FRAME        = hnumero-ordem:FRAME
                SENSITIVE    = NO 
                VISIBLE      = NO
                FONT         = 1
                HEIGHT-CHARS = 0.88
                WIDTH        = 7
                NAME         = "aliquota-icm"
                HELP         = "*Al¡quota ICMS DANFE para calc dif aliquota"
                TOOLTIP      = "*Al¡quota ICMS DANFE para calc dif aliquota"
                SIDE-LABEL-HANDLE = htxt-Label-dif
                LABEL             = "Aliq ICMS:".

        CREATE TOGGLE-BOX hbenef-icms
            ASSIGN FRAME              = hnumero-ordem:FRAME                           
                   FORMAT             = "Sim/NÆo" 
                   WIDTH              = 12
                   HEIGHT             = hnumero-ordem:HEIGHT
                   ROW                = hnumero-ordem:ROW 
                   COL                = hnumero-ordem:COL + 31
                   LABEL              = "Benef ICMS"
                   HELP               = "Benef¡cios de ICMS informados na NF"
                   TOOLTIP            = "Benef¡cios de ICMS informados na NF"
                   VISIBLE            = NO 
                   SENSITIVE          = NO
                   CHECKED            = FALSE
                   NAME               = "benef-icms".
        CREATE TOGGLE-BOX hbenef-piscof
            ASSIGN FRAME              = hnumero-ordem:FRAME                           
                   FORMAT             = "Sim/NÆo" 
                   WIDTH              = 14
                   HEIGHT             = hnumero-ordem:HEIGHT
                   ROW                = hnumero-ordem:ROW + 1
                   COL                = hnumero-ordem:COL + 31
                   LABEL              = "Benef PIS/COF"
                   HELP               = "Benef¡cios de PIS/COFINS informados na NF"
                   TOOLTIP            = "Benef¡cios de PIS/COFINS informados na NF"
                   VISIBLE            = NO 
                   SENSITIVE          = NO
                   CHECKED            = FALSE
                   NAME               = "benef-piscof".
                
    END.
    
END. /* p-ind-event  = "BEFORE-INITIALIZE" */

/* Atualizar dados na tela */
IF p-ind-event  = "BEFORE-DISPLAY" AND
   p-ind-object = "CONTAINER"    AND
   p-cod-table  = "item-doc-est" AND
   p-row-table <> ?              THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    ASSIGN hcod-model     :VISIBLE = TRUE
           hcod-cfop-saida:VISIBLE = TRUE
           hemit-crt      :VISIBLE = TRUE 
           haliquota-icm  :VISIBLE = TRUE
           hbenef-icms    :VISIBLE = TRUE
           hbenef-piscof  :VISIBLE = TRUE.
END.
/* Atualizar dados na tela */
IF p-ind-event  = "AFTER-DISPLAY" AND
   p-ind-object = "CONTAINER"    AND
   p-cod-table  = "item-doc-est" AND
   p-row-table <> ?              THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    ASSIGN hcod-model:SCREEN-VALUE      = ""
           hcod-cfop-saida:SCREEN-VALUE = ""
           hbenef-icms  :CHECKED        = NO
           hbenef-piscof:CHECKED        = NO.
    FOR FIRST bitem-doc-est FIELDS(serie-docto nro-docto cod-emitente nat-operacao sequencia it-codigo)
        WHERE ROWID(bitem-doc-est) = p-row-table NO-LOCK:
            
        FIND FIRST es-item-doc-est-natoper
            WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
              AND es-item-doc-est-natoper.serie-docto  = bitem-doc-est.serie-docto
              AND es-item-doc-est-natoper.nro-docto    = bitem-doc-est.nro-docto
              AND es-item-doc-est-natoper.cod-emitente = bitem-doc-est.cod-emitente
              AND es-item-doc-est-natoper.nat-operacao = bitem-doc-est.nat-operacao
              AND es-item-doc-est-natoper.sequencia    = bitem-doc-est.sequencia NO-LOCK NO-ERROR.

        IF AVAIL es-item-doc-est-natoper THEN
            ASSIGN hcod-model:SCREEN-VALUE      = es-item-doc-est-natoper.cod-model-nf-eletro
                   hcod-cfop-saida:SCREEN-VALUE = es-item-doc-est-natoper.cod-cfop-saida
                   hemit-crt:SCREEN-VALUE       = IF es-item-doc-est-natoper.emit-crt = 1 THEN STRING(es-item-doc-est-natoper.emit-crt)
                                                  ELSE IF es-item-doc-est-natoper.emit-crt = 3 THEN '3' ELSE '4'
                   haliquota-icm:SCREEN-VALUE   = STRING(es-item-doc-est-natoper.aliquota-icm)
                   hbenef-icms  :CHECKED        = es-item-doc-est-natoper.cod-beneficio-icms   > 0
                   hbenef-piscof:CHECKED        = es-item-doc-est-natoper.cod-beneficio-piscof > 0.
        IF NOT AVAIL es-item-doc-est-natoper OR
           (AVAIL es-item-doc-est-natoper AND es-item-doc-est-natoper.nat-operacao-orig = "") THEN
            ASSIGN hcod-model     :SENSITIVE = TRUE
                   hcod-cfop-saida:SENSITIVE = TRUE
                   hemit-crt      :SENSITIVE = TRUE 
                   haliquota-icm  :SENSITIVE = TRUE
                   hbenef-icms    :SENSITIVE = TRUE
                   hbenef-piscof  :SENSITIVE = TRUE.
    END. /* FIRST bitem-doc-est */
    

END. /* p-ind-event  = "AFTER-DISPLAY" */
/* Validar */
IF p-ind-event  = "BTOK"   AND
   p-ind-object = "BUTTON" AND
   p-row-table <> ?        THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    IF  hcod-cfop-saida:SENSITIVE               AND
        TRIM(hcod-cfop-saida:SCREEN-VALUE) = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "CFOP NFe/CTe inv lido~~" +
                                 "CFOP NFe/CTe ‚ obrigat¢rio e dever  ser informado um valor.").
        APPLY "ENTRY"  TO hcod-cfop-saida.
        RETURN "NOK".
    END.

END.
/* Salvar */
IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    AND
   p-cod-table  = "item-doc-est" AND
   p-row-table <> ?              THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    FOR FIRST bitem-doc-est FIELDS(serie-docto nro-docto cod-emitente nat-operacao sequencia it-codigo)
        WHERE ROWID(bitem-doc-est) = p-row-table:

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

        IF haliquota-icm:SENSITIVE AND
           es-item-doc-est-natoper.aliquota-icm > 0  THEN
            ASSIGN bitem-doc-est.aliquota-icm = es-item-doc-est-natoper.aliquota-icm.

        IF hbenef-icms:CHECKED THEN DO:
            FOR FIRST es-natoper-rec 
                WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK:
                ASSIGN es-item-doc-est-natoper.cod-beneficio-icms = es-natoper-rec.cod-benef-icms-rec.
            END.
        END.
        ELSE
            ASSIGN es-item-doc-est-natoper.cod-beneficio-icms = 0. 
        IF hbenef-piscof:CHECKED THEN DO:
            FOR FIRST es-natoper-rec 
                WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK:
                ASSIGN es-item-doc-est-natoper.cod-beneficio-piscof = es-natoper-rec.cod-benef-pis-cofins-rec.
            END.
        END.
        ELSE
            ASSIGN es-item-doc-est-natoper.cod-beneficio-piscof = 0. 
    END. /* FIRST bitem-doc-est */

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
                 IF  ph_frame:NAME = "fPage0" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
/*                  IF ph_frame:NAME = "fPage1" THEN LEAVE blk_frame. */
             END.
             WHEN "FILL-IN" THEN DO:
                 IF ph_frame:NAME = "numero-ordem"   THEN ASSIGN hnumero-ordem   = ph_frame.
                 IF ph_frame:NAME = "num-pedido"     THEN ASSIGN hnum-pedido     = ph_frame.
                 IF ph_frame:NAME = "cod-cfop-saida" THEN ASSIGN hcod-cfop-saida = ph_frame.
                 IF ph_frame:NAME = "aliquota-icm"   THEN ASSIGN haliquota-icm   = ph_frame.
             END.
             WHEN "RADIO-SET" THEN DO:
                 IF ph_frame:NAME = "cod-model-nfe-eletro"  THEN ASSIGN hcod-model = ph_frame.
             END.
             WHEN "COMBO-BOX" THEN DO:
                 IF ph_frame:NAME = "emit-crt"  THEN ASSIGN hemit-crt = ph_frame.
             END.
             WHEN "TOGGLE-BOX" THEN DO:
                 IF ph_frame:NAME = "benef-icms"   THEN ASSIGN hbenef-icms   = ph_frame.
                 IF ph_frame:NAME = "benef-piscof" THEN ASSIGN hbenef-piscof = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */

