/*****************************************************************
 * DSC                                                           *
 *---------------------------------------------------------------*
 * Programa....: esfas701aa-u00.p                                *
 * Data........: 09/2007                                         *
 * Responsavel.: Maria Aparecida Ladaga Nogueira                 *
 * Funcao......: Programa UPC de Bens (Salvage Value )           *
 *****************************************************************/

/********************************************************************
     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
*********************************************************************/

/* {include/i-prgvrs.i esfas701aa-u00 5.04.00.000} */
/*----- DEFINICAO DE FUNCOES -----*/
/*{tools/fc-handle-obj.I}*/
/*********/

DEF INPUT PARAM p_evento AS CHAR          NO-UNDO.
DEF INPUT PARAM p_objeto AS CHAR          NO-UNDO.
DEF INPUT PARAM p_handle AS HANDLE        NO-UNDO.
DEF INPUT PARAM p_frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p_tabela AS CHAR          NO-UNDO.
DEF INPUT PARAM p_recid  AS Recid         NO-UNDO.

/*variaveis global*/
DEF NEW GLOBAL SHARED VAR wh-salvage-value    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-salvage-value AS WIDGET-HANDLE NO-UNDO.

/*variavel local*/
DEF VAR c-handle-obj  AS CHAR          NO-UNDO.
DEF VAR h-objeto      AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-rw-bem-pat AS recid.

IF p_tabela = "bem_pat"  AND p_recid <> ? THEN
   ASSIGN wh-rw-bem-pat = p_recid.

IF p_evento = "display" AND p_objeto = "viewer" THEN DO:
   ASSIGN h-objeto = p_frame:first-child
          h-objeto = h-objeto:FIRST-CHILD.

   do while valid-handle(h-objeto):
      IF h-objeto:TYPE <> "FIELD-GROUP" THEN DO:
         IF h-objeto:NAME = "num_bem_pat" THEN do: /*"fpage1"*/
            ASSIGN wh-salvage-value = h-objeto.
            LEAVE.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   IF VALID-HANDLE(wh-salvage-value) THEN DO:
      CREATE BUTTON  wh-bt-salvage-value
      ASSIGN FRAME     = wh-salvage-value:FRAME
             WIDTH     = wh-salvage-value:WIDTH + 8
             HEIGHT    = wh-salvage-value:HEIGHT
             ROW       = wh-salvage-value:ROW
             COLUMN    = wh-salvage-value:COLUMN /*+ wh-salvage-value:WIDTH*/ + 25
             TOOLTIP   = "Salvage Value"
             HELP      = ""
             NAME      = "bt-salvage"
             SENSITIVE = YES
             VISIBLE   = YES
             LABEL     = "Salvage Value"
             FONT      = wh-salvage-value:FONT
      TRIGGERS:
          ON 'choose':U PERSISTENT RUN upc/esfas701aa-u01.p(input "CHOOSE",
                                                            input "salvage-value",
                                                            INPUT p_handle,
                                                            input p_frame:FRAME,
                                                            input ?,
                                                            input wh-rw-bem-pat).
     END triggers.
   END.
END.

/*
PROCEDURE pi-mensagem:
    MESSAGE "p-ind-event  " p-ind-event  SKIP
            "p-ind-object " p-ind-object SKIP
            "p-wgh-object " p-wgh-object SKIP
            "p-wgh-frame  " p-wgh-frame  SKIP
            "p-cod-table  " p-cod-table  SKIP
            "p-row-table  " string(p-row-table)  SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.
*/
