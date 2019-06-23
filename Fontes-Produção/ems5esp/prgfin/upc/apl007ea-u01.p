/****************************************************************************************** 
** 	   Programa: apl007ea-u01.p
**   	  Autor: Unknow
** 	 Fornecedor: DKP
**         Data: ?/2017
** Change/Chamado: 
**      Objetivo: Cria as vari†veis (Cadastro Operacao Financ - Inclus∆o) Validar a criaá∆o de campo para informar o contrato m∆e na tela da alteraá∆o/modificaá∆o da operaá∆o financeira
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
def var c-objeto as char no-undo.
def new global shared VAR wh-cotacao    as widget-handle no-undo.
def new global shared var tx-label-3   as widget-handle no-undo.

DEF BUFFER b_operac_financ FOR operac_financ.

{include/i_fclpreproc.i} /* Include que define o processador do Facelift ativado ou n∆o. */

&IF "{&aplica_facelift}" = "YES" &THEN	{include/i_fcldef.i}
&endif


assign c-objeto   = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").


/* MESSAGE "p-ind-event " p-ind-event        SKIP */
/*         "p-ind-object" p-ind-object       SKIP */
/*         "p-wgh-object" p-wgh-object:NAME  SKIP */
/*         "p-wgh-frame " p-wgh-frame        SKIP */
/*         "p-cod-table " p-cod-table        SKIP */
/*         "c-objeto    " c-objeto                */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.    */


if  p-ind-event = "INITIALIZE"
and p-ind-object = "VIEWER" then do: 

    create text tx-label
        assign frame        = p-wgh-frame
               format       = "x(17)"
               width        = 6.2
               screen-value = "Contrato:"
               row          = 4.8
               col          = 35
               fgcolor      = 1
               visible      = yes.   

    create fill-in wh-fill
        assign frame              = p-wgh-frame
               format             = "x(40)" 
               side-label-handle  = tx-label:handle
               width              = 40
               height             = 0.88
               row                = 4.8
               col                = 42
               TOOLTIP            = "F5 ou MOUSE duplo click para Zoom"
               visible            = yes
               sensitive          = no
               triggers:
                   ON F5 PERSISTENT RUN prgfin/upc/apl007ea-u03.p.
                   ON "mouse-select-dblclick" PERSISTENT RUN prgfin/upc/apl007ea-u03.p.
                   ON "leave" PERSISTENT RUN prgfin/upc/apl007ea-u05.p.
               end triggers.
/*     &IF "{&aplica_facelift}" = "YES" &THEN */
/*         {include/i_fcldin.i wh-fill}       */
/*     &endif                                 */

    create text tx-label-2
        assign frame        = p-wgh-frame
               format       = "x(17)"
               width        = 7.2
               screen-value = "Descriá∆o:"
               row          = 5.8
               col          = 35
               fgcolor      = 1
               visible      = yes.   

    create fill-in wh-fill-2
        assign frame              = p-wgh-frame
               format             = "x(50)" 
               side-label-handle  = tx-label-2:handle
               width              = 45
               height             = 0.88
               row                = 5.8
               col                = 42
               visible            = yes
               sensitive          = no
               triggers:
                   
               end triggers.
    &IF "{&aplica_facelift}" = "YES" &THEN
        {include/i_fcldin.i wh-fill-2}
    &endif  
    
    create text tx-label-3
    assign frame        = p-wgh-frame
           format       = "x(23)"
           width        = 14
           screen-value = "Cotaá∆o Contrato:"
           row          = 7.99
           col          = 63
           fgcolor      = 1
           visible      = yes.   

    create fill-in wh-cotacao
    assign frame              = p-wgh-frame
           DATA-TYPE          = "decimal"
           format             = ">>9.9999" 
           side-label-handle  = tx-label-3:handle
           width              = 10
           height             = 0.88
           row                = 7.88
           col                = 76
           visible            = yes
           sensitive          = YES.

end.

if  p-ind-event = "ENABLE" 
and p-ind-object = "VIEWER"
 then do:
     /* Inativa o campo do contrato caso a operaá∆o esteja efetivada */
   FIND b_operac_financ NO-LOCK WHERE
        RECID(b_operac_financ) = p-row-table NO-ERROR.
   IF AVAIL b_operac_financ AND b_operac_financ.ind_sit_operac_financ_apl = "encerrada"
        THEN wh-fill:SENSITIVE = NO.
    ELSE ASSIGN wh-fill:SENSITIVE = YES.
end.

if  p-ind-event = "DISABLE" 
and p-ind-object = "VIEWER"
 then do:
    ASSIGN wh-fill:SENSITIVE = NO.
end.

/* DPC - Display Fields para n∆o habilitar o campo qdo a operaá∆o estiver vencida ou atualizada */
if  p-ind-event = "DISPLAY" 
and p-ind-object = "VIEWER"
 then do:
    /* Inativa o campo do contrato caso a operaá∆o esteja efetivada */
   FIND b_operac_financ NO-LOCK WHERE
        RECID(b_operac_financ) = p-row-table NO-ERROR.
   IF AVAIL b_operac_financ AND b_operac_financ.ind_sit_operac_financ_apl = "encerrada"
        THEN wh-fill:SENSITIVE = NO.
   ELSE ASSIGN wh-fill:SENSITIVE = YES.
end.
