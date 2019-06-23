/****************************************************************************************** 
** 	   Programa: cn0601-upc.P 
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 19/11/2018
** Change/Chamado: 
**      Objetivo: Cria aba Garantia de contratos
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: cn0201a
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include/i-prgvrs.i cn0601-upc00.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.


/* MESSAGE p-ind-event   skip             */
/*         p-ind-object  skip             */
/*         p-wgh-object  skip             */
/*         p-wgh-frame   skip             */
/*         p-cod-table   skip             */
/*         string(p-row-table)            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


/* Global Variable Definitions **********************************************/
define new global shared var adm-broker-hdl as handle no-undo.
define new global shared var h-bot_garantia as handle no-undo.
define new global shared var h-viewer       as handle no-undo.

/* Variable Definitions *****************************************************/
define var c-folder       as character no-undo.
define var c-objects      as character no-undo.
define var h-object       as handle    no-undo.
define var i-objects      as integer   no-undo.
define var l-record       as logical   no-undo initial no.
define var l-group-assign as logical   no-undo initial no.

define new global shared VARIABLE views-folder AS HANDLE EXTENT 3 NO-UNDO. /*pega as viewers dos folders*/

define NEW GLOBAL SHARED VAR row-contrato-for  as rowid         no-undo.

{include/i_fclpreproc.i} /* Include que define o processador do Facelift ativado ou n∆o. */ 

&IF "{&aplica_facelift}" = "YES" &THEN
	{include/i_fcldef.i}
&endif

define variable c-handle-obj        as character no-undo.
define variable c-objeto            as character no-undo. 

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                  p-wgh-object:FILE-NAME,'/').



/* Main Block ***************************************************************/
/* MESSAGE p-ind-event   SKIP             */
/*         c-objeto      SKIP             */
/*                                        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF p-ind-event = "DISPLAY" AND p-ind-object = "VIEWER" AND p-cod-table = "contrato-for" then DO:

    row-contrato-for = p-row-table.

END.



IF p-ind-event = "INITIALIZE" AND p-ind-object = "CONTAINER" AND NOT VALID-HANDLE(h-bot_garantia) then do:
  
    CREATE BUTTON h-bot_garantia
        ASSIGN 
           FRAME       = p-wgh-frame
           WIDTH       = 10
           HEIGHT      = 0.88
           LABEL       = "Garantia"
           ROW         = 3
           COLUMN      = 71
           TOOLTIP     = "GaraTeste"
/*            HELP        = h-bt-ok:HELP */
           NAME        = "Garantia"
           SENSITIVE   = YES
           VISIBLE     = YES
           FONT        = 1
        TRIGGERS:
           ON 'choose':U PERSISTENT RUN upc\cn0601-upc00.p (INPUT "EXIBIR_GARANTIA", 
                                                            input  p-ind-object, 
                                                            input  p-wgh-object, 
                                                            input  p-wgh-frame,  
                                                            input  p-cod-table,  
                                                            input  p-row-table 
                                                             ).
        END triggers.

/*     h-bt-ok:SENSITIVE = NO.                               */
/*     h-bot_garantia:MOVE-AFTER-TAB-ITEM(h-bt-ok) NO-ERROR. */
    

end.   

IF p-ind-event = "EXIBIR_GARANTIA" THEN DO:

   RUN cnp\escn0601-w01.w (INPUT row-contrato-for).

END.
