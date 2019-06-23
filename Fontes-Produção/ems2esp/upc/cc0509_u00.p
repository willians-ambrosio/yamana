/****************************************************************************************** 
** 	   Programa: cc0509_u00.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 13/09/2018
** Change/Chamado: 
**      Objetivo: Bloquear a aba "Coment†rio" do folder se o usu†rio n∆o tiver permiss∆o
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS:  CC0509
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID NO-UNDO.

/*                                                        */
/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

IF p-ind-object = "container" AND 
   p-ind-event = "INITIALIZE" THEN DO:
   
    DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE      NO-UNDO.
    DEFINE VARIABLE page-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.
    {utp\ut-glob.i}    
    
    /* Verifica se usu†rio tem permiss∆o para acessar a pag de narrativa - coment†rios */
    FOR EACH usuar_grp_usuar NO-LOCK WHERE
             usuar_grp_usuar.cod_usuario = v_cod_usuar_corren:

        IF NOT CAN-FIND(FIRST es_grp_usuar WHERE
                              es_grp_usuar.cod_grp_usu = usuar_grp_usuar.cod_grp_usu) THEN 
            NEXT.

        ASSIGN l-ok = YES. /* usu†rio possui permiss∆o para viasualizar coment†rios */
        LEAVE.
    END.

    IF l-ok THEN RETURN.

    RUN get-link-handle IN adm-broker-hdl (p-wgh-object, 'PAGE-SOURCE',OUTPUT page-hdl).
    RUN delete-folder-page IN page-hdl (5).
END.
   
