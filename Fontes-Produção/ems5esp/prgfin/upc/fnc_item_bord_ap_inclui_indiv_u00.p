/****************************************************************************************** 
** 	   Programa: fnc_item_bord_ap_inclui_indiv_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 21/08/2018
** Change/Chamado: 
**      Objetivo: Criar campo "imposto" e solicitar o c¢digo do tributo qdo for verdadeiro - Usado apenas qdo o 
                  bordorì n∆o for de DARF
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: fnc_lote_impl_tit_ap_atualiza
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID     NO-UNDO.

DEF BUFFER bf_item_bord_ap FOR ITEM_bord_ap.

/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

IF p-ind-event = "ASSIGN" THEN DO:

     FIND bf_item_bord_ap NO-LOCK WHERE
            RECID(bf_item_bord_ap) = p-row-table NO-ERROR.
     IF NOT AVAIL bf_item_bord_ap THEN RETURN.

     FIND ext_espec_docto NO-LOCK WHERE                                                                                                                                  
          ext_espec_docto.cod_espec_docto = bf_item_bord_ap .cod_espec_docto NO-ERROR.                                                                               
     IF NOT AVAIL ext_espec_docto THEN RETURN.                                                                                                                           
                                                                                                                                                                         
     IF NOT ext_espec_docto.log_fgts AND NOT ext_espec_docto.log_gps AND NOT ext_espec_docto.log_imposto THEN RETURN.
                                                                                                                                                                     
     RUN prgfin\esp\ap006-w01.w (INPUT (IF ext_espec_docto.log_fgts THEN 1 ELSE (IF ext_espec_docto.log_gps THEN 3 ELSE (IF ext_espec_docto.log_imposto THEN 2 ELSE 0))),
                                 INPUT p-row-table) "2".
END.
