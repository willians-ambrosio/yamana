/*****************************************************************
 * DATASUL S.A                                                   *
 *---------------------------------------------------------------*
 * Programa....: esfas715aa-u01                                  *
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

/*variaveis global*/
DEF NEW GLOBAL SHARED VAR wh-salvage-value     AS WIDGET-HANDLE NO-UNDO.
/* DEF     GLOBAL        VAR wh-bt-salvage-value  AS WIDGET-HANDLE NO-UNDO. */
DEF NEW GLOBAL SHARED VAR wh-rw-bem-pat        AS Recid.
DEF NEW GLOBAL SHARED VAR wh-rw-incorp-bem-pat AS Recid.

DEF NEW GLOBAL SHARED VAR wh-cod-cta-pat       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-bem-pat       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-seq-bem-pat   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-br-bas-incorp     AS WIDGET-HANDLE NO-UNDO.

DEF VAR wh-rw-bem-pat             AS recid.
DEF VAR wh-rw-incorp-bem-pat      AS recid.

DEF NEW GLOBAL SHARED VAR wh-repres_fin-br-cond AS WIDGET-HANDLE NO-UNDO.

/*
MESSAGE  "xxx"
    wh-cod-cta-pat    :SCREEN-VALUE
    wh-num-bem-pat    :SCREEN-VALUE
    wh-num-seq-bem-pat:SCREEN-VALUE
    string(wh-br-bas-incorp) VIEW-AS ALERT-BOX.
*/

IF p_evento = "CHOOSE" AND  p_objeto = "salvage-value" THEN DO:
   /*ASSIGN p_objeto:sensitive = YES .*/
    FIND last bem_pat
        WHERE bem_pat.cod_cta_pat     = string(wh-cod-cta-pat:SCREEN-VALUE)
          AND bem_pat.num_bem_pat     = INT(wh-num-bem-pat:SCREEN-VALUE)
          AND bem_pat.num_seq_bem_pat = INT(wh-num-seq-bem-pat:SCREEN-VALUE).
    IF AVAIL bem_pat THEN
       ASSIGN wh-rw-bem-pat = RECID(bem_pat).

    /* ASSIGN wh-rw-incorp-bem-pat = p_recid.
    FIND FIRST incorp_bem_pat
         WHERE Recid(incorp_bem_pat) = wh-rw-incorp-bem-pat NO-ERROR.*/
    IF AVAIL bem_pat THEN
       RUN esp/esbem03.w.
END.
