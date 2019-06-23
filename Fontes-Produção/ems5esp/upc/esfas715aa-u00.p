/*****************************************************************
 * DSC                                                           *
 *---------------------------------------------------------------*
 * Programa....: esfas715aa-u00.p                                *
 * Data........: 02/2008                                         *
 * Responsavel.: Maria Aparecida Ladaga Nogueira                 *
 * Funcao......: Programa UPC de Bens -                          *
 *               Incorpora‡Æo         (Salvage Value )           *
 *****************************************************************/

/********************************************************************
     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
*********************************************************************/
/* {include/i-prgvrs.i esfas715aa-u00 5.04.00.000} */
/*----- DEFINICAO DE FUNCOES -----*/
/*{upc/fc-handle-obj.I}*/
/*********/

DEF INPUT PARAM p_evento AS CHAR          NO-UNDO.
DEF INPUT PARAM p_objeto AS CHAR          NO-UNDO.
DEF INPUT PARAM p_handle AS HANDLE        NO-UNDO.
DEF INPUT PARAM p_frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p_tabela AS CHAR          NO-UNDO.
DEF INPUT PARAM p_recid  AS Recid         NO-UNDO.

/*variaveis global*/
DEF NEW GLOBAL SHARED VAR wh-salvage-value      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-salvage-value   AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cod-cta-pat        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-bem-pat        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-seq-bem-pat    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-br-bas-incorp      AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR gr-br-bas-incorp      AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-repres_fin-br-cond AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-manut-query        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-manut-buffer       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-manut-val-comis    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-manut-ind-sit      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-manut-sequencia    AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ sequencia */
DEF NEW GLOBAL SHARED VAR wh-manut-estabelec    AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ estabelecimento */
DEF NEW GLOBAL SHARED VAR wh-manut-documento    AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ documento */
DEF NEW GLOBAL SHARED VAR wh-manut-titulo       AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ titulo */
DEF NEW GLOBAL SHARED VAR wh-manut-parcela      AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ parcela */
DEF NEW GLOBAL SHARED VAR wh-bt-enter           AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ parcela */
DEF NEW GLOBAL SHARED VAR wh-bt-zoo             AS WIDGET-HANDLE NO-UNDO. /* Buffer p/ parcela */

/*variavel local*/
DEF VAR c-handle-obj         AS CHAR NO-UNDO.
DEF VAR h-objeto             AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-rw-bem-pat        AS recid.

DEF VAR wh-br-bas-incorp-xxx AS recid.

IF p_tabela = "bem_pat"  AND p_recid <> ? THEN
   ASSIGN wh-rw-bem-pat = p_recid.

/*
MESSAGE p_evento
        p_objeto
        p_handle
        p_frame
        p_tabela
        p_recid
    VIEW-AS ALERT-BOX.
*/

IF p_evento = "enable" AND p_objeto = "viewer" THEN  DO:
/* IF NOT VALID-HANDLE(wh-salvage-value) THEN DO:*/
   ASSIGN h-objeto = p_frame:first-child
          h-objeto = h-objeto:FIRST-CHILD.

   do while valid-handle(h-objeto):
      IF h-objeto:TYPE <> "FIELD-GROUP" THEN DO:
         IF h-objeto:NAME = "bt_ran2" THEN do:
            ASSIGN wh-salvage-value = h-objeto.
            /*LEAVE.*/
         END.
         ELSE
         IF h-objeto:NAME = "cod_cta_pat" THEN do:
            ASSIGN wh-cod-cta-pat = h-objeto.
            /*LEAVE.*/
         END.
         ELSE
         IF h-objeto:NAME = "num_bem_pat" THEN do:
            ASSIGN wh-num-bem-pat = h-objeto.
            /*LEAVE.*/
         END.
         ELSE
         IF h-objeto:NAME = "num_seq_bem_pat" THEN do:
            ASSIGN wh-num-seq-bem-pat = h-objeto.
            /*LEAVE.*/
         END.
         IF h-objeto:NAME = "br_bas_incorp_bem_pat1" THEN do:
            ASSIGN wh-br-bas-incorp = h-objeto.
           /*LEAVE.*/
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
   /*********************
IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "BROWSER" AND
   p-wgh-object:FILE-NAME  = "pdp/pd0508-b01.w"
    /* AND c-habilita = YES */
    THEN DO:

    ASSIGN wh-pd0508-browser = WIDGET-HANDLE(f-handle-obj('br-table', p-wgh-frame))
           wh-pd0508-bt-modificar = WIDGET-HANDLE(f-handle-obj('bt-modificar', p-wgh-frame))
           wh-pd0508-bt-cancelar = WIDGET-HANDLE(f-handle-obj('bt-cancelar', p-wgh-frame))
           wh-pd0508-bt-eliminar = WIDGET-HANDLE(f-handle-obj('bt-eliminar', p-wgh-frame))
           wh-pd0508-bt-alocar = WIDGET-HANDLE(f-handle-obj('bt-alocacao', p-wgh-frame)).

    ASSIGN wh-objeto          = wh-pd0508-browser:FIRST-COLUMN
           wh-pd0508-br-table = p-wgh-object
           wh-pd0508-query    = wh-pd0508-browser:QUERY.

    DO WHILE VALID-HANDLE(wh-objeto):
        IF wh-objeto:TYPE = "field-group" THEN DO:
            ASSIGN wh-objeto = wh-objeto:FIRST-COLUMN.
            NEXT.
        END.

        IF wh-objeto:NAME = "c-seq" THEN
           ASSIGN wh-pd0508-c-seq = wh-objeto
                  wh-pd0508-c-seq1 = wh-objeto.
        IF wh-objeto:NAME = "it-codigo" THEN
           ASSIGN wh-pd0508-it-codigo = wh-objeto.
        IF wh-objeto:NAME = "cod-refer" THEN
           ASSIGN wh-pd0508-cod-refer = wh-objeto.

        IF wh-objeto:NAME = "dt-entrega" THEN
            ASSIGN wh-pd0508-dt-entrega = wh-objeto.

        IF wh-objeto:NAME = "qt-pedida" THEN
           ASSIGN wh-pd0508-qt-pedida = wh-objeto.
        IF wh-objeto:NAME = "qt-atendida" THEN
           ASSIGN wh-pd0508-qt-atendida = wh-objeto.
        IF wh-objeto:NAME = "vl-preuni" THEN
           ASSIGN wh-pd0508-vl-preuni = wh-objeto.
        IF wh-objeto:NAME = "vl-liq-abe" THEN
           ASSIGN wh-pd0508-vl-liq-abe = wh-objeto.

        ASSIGN wh-objeto = wh-objeto:NEXT-COLUMN.
    END.
****************/

   IF VALID-HANDLE(wh-salvage-value) THEN DO:
      CREATE BUTTON  wh-bt-salvage-value
      ASSIGN FRAME     = wh-salvage-value:FRAME
             WIDTH     = wh-salvage-value:WIDTH + 13
             HEIGHT    = wh-salvage-value:HEIGHT
             ROW       = wh-salvage-value:ROW
             COLUMN    = wh-salvage-value:COLUMN /*+ wh-salvage-value:WIDTH*/ + 17
             TOOLTIP   = "Savage Value"
             HELP      = ""
             NAME      = "bt-salvage"
             SENSITIVE = YES
             VISIBLE   = YES
             LABEL     = "Savage Value"
             FONT      = wh-salvage-value:FONT
      TRIGGERS:
        ON 'choose':U PERSISTENT RUN upc/esfas715aa-u01.p(input "CHOOSE",
                                                          input "salvage-value",
                                                          INPUT p_handle,
                                                          input p_frame:FRAME,
                                                          input ?,
                                                          input wh-rw-bem-pat).
      END triggers.
   END.
END.

/***********
/* Cria tres colunas no browse */
IF p_evento = "INITIALIZE" AND
   p_objeto = "VIEWER"     THEN DO:
    assign c-handle-obj          = fc-handle-obj("br_bas_incorp_bem_pat1",p_frame).
           wh-repres_fin-br-cond = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
END.

/* Localiza o buffer dos campos constantes no browser */
/*ASSIGN wh-manut-query     = wh-repres_fin-br-cond:QUERY                         NO-ERROR. /* Localiza o handle da query*/ */
ASSIGN wh-manut-buffer    = wh-manut-query:GET-BUFFER-HANDLE(1)                 NO-ERROR. /* Localiza o buffer da query */
ASSIGN wh-manut-val-comis = wh-manut-buffer:BUFFER-FIELD("incorp")              NO-ERROR. /* Armazena o valor da comissao */
ASSIGN wh-manut-sequencia = wh-manut-buffer:BUFFER-FIELD("cenario")             NO-ERROR. /* Armazena a sequencia  */
ASSIGN wh-manut-estabelec = wh-manut-buffer:BUFFER-FIELD("cod_estab")           NO-ERROR. /* Armazena o estabelecimento */
ASSIGN wh-manut-documento = wh-manut-buffer:BUFFER-FIELD("cod_espec_docto")     NO-ERROR. /* Armazena o documento */
ASSIGN wh-manut-titulo    = wh-manut-buffer:BUFFER-FIELD("cod_tit_acr")         NO-ERROR. /* Armazena o titulo */
ASSIGN wh-manut-parcela   = wh-manut-buffer:BUFFER-FIELD("cod_parcela")         NO-ERROR. /* Armazena o codigo da parcela */
ASSIGN wh-manut-ind-sit   = wh-manut-buffer:BUFFER-FIELD("ind_sit_movto_comis") NO-ERROR. /* Armazena a situacao da comissao */

MESSAGE "xxxx  " wh-manut-val-comis:SCREEN-VALUE.
****************/
