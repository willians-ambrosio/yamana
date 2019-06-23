/*****************************************************************************
**
**  I-RPVAR.I - Variaveis para Impress’o do Cabecalho Padr’o (ex-CD9500.I)
**
*****************************************************************************/

{include/i_dbvers.i}

define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var c-sistema       as character format "x(25)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var c-rodape        as character                     no-undo.
define var v_num_count     as integer                       no-undo.
define var c-arq-control   as character                     no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define var c-impressora   as character                      no-undo.
define var c-layout       as character                      no-undo.

/*Defini‡äes inclu¡das para corrigir problema de vari veis j  definidas pois */
/*as vari veis e temp-tables eram definidas na include --rpout.i que pode ser*/
/*executada mais de uma vez dentro do mesmo programa (FO 1.120.458) */
/*11/02/2005 - Ed‚sio <tech14207>*/
/*-------------------------------------------------------------------------------------------*/
DEF VAR h-procextimpr                               AS HANDLE   NO-UNDO. 
DEF VAR i-num_lin_pag                               AS INT      NO-UNDO.    
DEF VAR c_process-impress                           AS CHAR     NO-UNDO.   
DEF VAR c-cod_pag_carac_conver                      AS CHAR     NO-UNDO.   

/*tech14207
FO 1663218
Inclu¡das as defini‡äes das vari veis e fun‡äes
*/
&IF "{&mguni_version}" >= "2.04" &THEN
    &IF "{&PDF-RP}" <> "YES" &THEN /*tech868*/
    
        /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito valida‡Æo para verificar se a variavel h_pdf_controller j  foi definida 
                                                       anteriormente, evitando erro de duplicidade*/

        &IF defined(def_pdf_controller) = 0 &THEN         
            DEFINE VARIABLE h_pdf_controller     AS HANDLE NO-UNDO.
    
            &GLOBAL-DEFINE def_pdf_controller YES
    
            DEFINE VARIABLE v_cod_temp_file_pdf  AS CHAR   NO-UNDO.
    
            DEFINE VARIABLE v_cod_relat          AS CHAR   NO-UNDO.
            DEFINE VARIABLE v_cod_file_config    AS CHAR   NO-UNDO.
    
            FUNCTION allowPrint RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION allowSelect RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION useStyle RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION usePDF RETURNS LOGICAL IN h_pdf_controller.
       
            FUNCTION getPrintFileName RETURNS CHARACTER IN h_pdf_controller.
       
            RUN btb/btb920aa.p PERSISTENT SET h_pdf_controller.
        &ENDIF
        /*Alteracao 03/04/2008 - tech40260 - FO 1746516 -  Feito valida‡Æo para verificar se a variavel h_pdf_controller j  foi definida 
                                                       anteriormente, evitando erro de duplicidade*/
    &ENDIF /*tech868*/
    
&endif
/*tech14207*/
/*tech30713 - fo:1262674 - Defini‡Æo de no-undo na temp-table*/
DEFINE TEMP-TABLE tt-configur_layout_impres_inicio NO-UNDO
    FIELD num_ord_funcao_imprsor    LIKE configur_layout_impres.num_ord_funcao_imprsor
    FIELD cod_funcao_imprsor        LIKE configur_layout_impres.cod_funcao_imprsor
    FIELD cod_opc_funcao_imprsor    LIKE configur_layout_impres.cod_opc_funcao_imprsor
    FIELD num_carac_configur        LIKE configur_tip_imprsor.num_carac_configur
    INDEX ordem num_ord_funcao_imprsor .

/*tech30713 - fo:1262674 - Defini‡Æo de no-undo na temp-table*/
DEFINE TEMP-TABLE tt-configur_layout_impres_fim NO-UNDO
    FIELD num_ord_funcao_imprsor    LIKE configur_layout_impres.num_ord_funcao_imprsor
    FIELD cod_funcao_imprsor        LIKE configur_layout_impres.cod_funcao_imprsor
    FIELD cod_opc_funcao_imprsor    LIKE configur_layout_impres.cod_opc_funcao_imprsor
    FIELD num_carac_configur        LIKE configur_tip_imprsor.num_carac_configur
    INDEX ordem num_ord_funcao_imprsor .
/*-------------------------------------------------------------------------------------------*/

define buffer b_ped_exec_style for ped_exec.
define buffer b_servid_exec_style for servid_exec.
&IF "{&SHARED}" = "YES":U &THEN
    define shared stream str-rp.
&ELSE
    define new shared stream str-rp.
&ENDIF
{include/i-lgcode.i}
/* i-rpvar.i */
/*Altera‡Æo 20/07/2007 - tech1007 - Defini‡Æo da vari vel utilizada para impressÆo em PDF*/
DEFINE VARIABLE v_output_file        AS CHAR   NO-UNDO.
