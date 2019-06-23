/****************************************************************************************** 
** 	   Programa: apl007ca-u05.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Zoom do Contrato m∆e na operaá∆o financeira 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/* def var wh-pesquisa as widget-handle.                          */
/* def new global shared var l-implanta as logical init no.       */
/* def new global shared var wh-fill    as widget-handle no-undo. */
/* def new global shared var wh-window  as handle no-undo.        */
/* def new global shared var adm-broker-hdl as handle no-undo.    */
/*                                                                */
/* {include/zoomvar.i &prog-zoom=prgfin/apl/apya501-z01.w         */
/*                    &proghandle=wh-window                       */
/*                    &campohandle=wh-fill                        */
/*                    &campozoom=cod_contrat_apf}                 */

def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var wh-fill-2    as widget-handle no-undo.

find contrat_apf where contrat_apf.cod_contrat_apf = wh-fill:SCREEN-VALUE no-lock no-error.
IF AVAILABLE(contrat_apf) THEN DO:
   ASSIGN wh-fill:SCREEN-VALUE = contrat_apf.cod_contrat_apf  
          wh-fill-2:SCREEN-VALUE = contrat_apf.des_contrat_apf.

END.
ELSE ASSIGN wh-fill:SCREEN-VALUE = ""      
            wh-fill-2:SCREEN-VALUE = "".

    
    
