/*****************************************************************
 * DATASUL S.A                                                   *
 *---------------------------------------------------------------*
 * Programa....: esfas701aa-u01                                  *
 * Data........:                                                 *
 * Responsavel.: Raimundo C. Soares                              *
 * Funcao......: Programa UPC de itens de recebimento            *
 *****************************************************************/

/********************************************************************
     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
*********************************************************************/
DEF INPUT PARAM p_evento AS CHAR          NO-UNDO.
DEF INPUT PARAM p_objeto AS CHAR          NO-UNDO.
DEF INPUT PARAM p_handle AS HANDLE        NO-UNDO.
DEF INPUT PARAM p_frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p_tabela AS CHAR          NO-UNDO.
DEF INPUT PARAM p_recid  AS Recid         NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-rw-bem-pat       AS Recid.

DEF VAR wh-rw-bem-pat AS recid.

IF p_evento = "CHOOSE" AND  p_objeto = "salvage-value" THEN DO:
    ASSIGN wh-rw-bem-pat = p_recid.
    FIND FIRST bem_pat
         WHERE Recid(bem_pat) = wh-rw-bem-pat NO-ERROR.
    IF AVAIL bem_pat  THEN
       RUN esp/esbem01.w.
END.
