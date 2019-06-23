/****************************************************************************************** 
** 	   Programa: apl007aa-u01.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Cria o bot∆o para vincular contrato ROF e CÉmbio na operaá∆o Financeira
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var tx-label   as widget-handle no-undo.
def new global shared var wh-fill-2    as widget-handle no-undo.
def new global shared var tx-label-2   as widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR wh-apl007aa-bt-cad-fil AS widget-handle no-undo.

def var c-objeto as char no-undo.

{include/i_fclpreproc.i} /* Include que define o processador do Facelift ativado ou n∆o. */

&IF "{&aplica_facelift}" = "YES" &THEN	{include/i_fcldef.i}
&endif

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").


/* MESSAGE "p-ind-event " p-ind-event        SKIP */
/*         "p-ind-object" p-ind-object       SKIP */
/*         "p-wgh-object" p-wgh-object:NAME  SKIP */
/*         "p-wgh-frame " p-wgh-frame        SKIP */
/*         "p-cod-table " p-cod-table        SKIP */
/*         "c-objeto    " c-objeto                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.    */


if  p-ind-event = "INITIALIZE"
and p-ind-object = "VIEWER"
 then do: 
    
   CREATE BUTTON  wh-apl007aa-bt-cad-fil     
       ASSIGN FRAME       = p-wgh-frame
              FLAT-BUTTON = YES 
              WIDTH       = 4.0
              HEIGHT      = 1.1
              ROW         = 1.1
              COL         = 60
              SENSITIVE   = YES
              VISIBLE     = YES
              LABEL       = "OK"
              TOOLTIP     = "Informaá‰es Adicionais Contrato (ROF / Cambio)"

              TRIGGERS:
                ON CHOOSE PERSISTENT RUN prgfin/upc/apl007aa-u02.p ("CHOOSE",
                                                                   "wh-apl007aa-bt-cad-fil",
                                                                   p-wgh-object,
                                                                   p-wgh-frame ,
                                                                   p-cod-table ,
                                                                   p-row-table ).

              END TRIGGERS.

      wh-apl007aa-bt-cad-fil:LOAD-IMAGE-UP ("prgfin\image\contrato.png").
end.
