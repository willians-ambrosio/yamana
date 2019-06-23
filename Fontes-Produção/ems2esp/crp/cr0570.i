  
/**********************************************************
** Include para verificar fun»’o para mostrar portador 
** de deposito do cheque
** INCLUDE UTILIZADA NO EMS 2 
**********************************************************/
    
DEFINE VARIABLE l-func-bx-ch-recib AS LOGICAL INITIAL NO.

&IF DEFINED(BF_FIN_BX_CH_RECEBIDO) &THEN
    assign l-func-bx-ch-recib = yes.
&else
        if  can-find(first funcao where
                           funcao.cd-funcao = "SPP-bx-ch-recib" and
                           funcao.ativo     = yes) then do:
            ASSIGN l-func-bx-ch-recib = YES.
        END.
&endif

/* fim crp/cr0570.i */
