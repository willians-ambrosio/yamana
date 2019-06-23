/****************************************************************************************** 
** 	   Programa: apl007ea-u03.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Zoom de Contrato MÆe utilizado na Operacao Financ - InclusÆo
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var wh-fill-2    as widget-handle no-undo.
def new global shared var wh-window  as handle no-undo.
def new global shared var v_rec_contrat_apf as RECID format ">>>>>>9" initial ? no-undo.

run prgfin/upc/apl007ea-u04.p /*prg_sea_contrat_apf*/.
if  v_rec_contrat_apf <> ?
then do:
    find contrat_apf where recid(contrat_apf) = v_rec_contrat_apf no-lock no-error.
    IF AVAILABLE(contrat_apf) THEN DO:
       ASSIGN wh-fill:SCREEN-VALUE = contrat_apf.cod_contrat_apf  
              wh-fill-2:SCREEN-VALUE = contrat_apf.des_contrat_apf.

    END.
    ELSE ASSIGN wh-fill:SCREEN-VALUE = ""
                wh-fill-2:SCREEN-VALUE = "".
end /* if */.
    
