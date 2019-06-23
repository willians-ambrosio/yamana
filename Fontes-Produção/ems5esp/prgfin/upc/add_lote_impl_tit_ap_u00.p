/****************************************************************************************** 
** 	   Programa: add_item_lote_impl_tit_ap_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/08/2018
** Change/Chamado: 
**      Objetivo: Solicitar dados para pagamento de Tributos - DARF, GPS e FGTS
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: add_item_lote_impl_tit_ap, mod_item_lote_impl_ap_base
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID     NO-UNDO.

DEF BUFFER bf_item_lote_impl_ap FOR item_lote_impl_ap.

/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

IF p-ind-event  = "VALIDATE" AND 
   p-ind-object = "VIEWER" THEN DO:

    FIND bf_item_lote_impl_ap NO-LOCK WHERE
         RECID(bf_item_lote_impl_ap) = p-row-table NO-ERROR.
    IF NOT AVAIL bf_item_lote_impl_ap THEN RETURN.

    FIND ext_espec_docto NO-LOCK WHERE
         ext_espec_docto.cod_espec_docto = bf_item_lote_impl_ap.cod_espec_docto NO-ERROR.
    IF NOT AVAIL ext_espec_docto THEN RETURN.

    IF NOT ext_espec_docto.log_fgts AND NOT ext_espec_docto.log_gps AND NOT ext_espec_docto.log_imposto THEN RETURN.

    RUN prgfin\esp\ap006-w01.w (INPUT (IF ext_espec_docto.log_fgts THEN 1 ELSE (IF ext_espec_docto.log_gps THEN 3 ELSE (IF ext_espec_docto.log_imposto THEN 2 ELSE 0))), 
                                INPUT p-row-table) "1".
END.
