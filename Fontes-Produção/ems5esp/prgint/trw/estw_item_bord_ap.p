/****************************************************************************************** 
** 	   Programa: estw_item_bord_ap.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 30/07/2018
** Change/Chamado: 
**      Objetivo: Valida se ‚ t¡tulo de imposto pela esp‚cie e solicita o c¢digo do tributo
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: item_bord_ap
******************************************************************************************/

{include/i-prgvrs.i estw_item_bord_ap 12.1.17.000}
    
{utp/ut-glob.i} 

DEFINE PARAMETER BUFFER p-table     FOR item_bord_ap.
DEFINE PARAMETER BUFFER p-old-table FOR ITEM_bord_ap.

DEFINE BUFFER bf-item_bord_ap FOR item_bord_ap.

/* --- MAIN BLOCK --- */
IF NEW(p-table) THEN DO: 

    FIND ext_espec_docto NO-LOCK 
        WHERE ext_espec_docto.cod_espec_docto = p-table.cod_espec_docto NO-ERROR.
    IF NOT AVAIL ext_espec_docto 
        THEN RETURN.

    IF NOT ext_espec_docto.log_fgts AND NOT ext_espec_docto.log_gps AND NOT ext_espec_docto.log_imposto 
        THEN RETURN.
        
    /* valida ja existe um especie para o bordero, nao permitindo duas diferentes */
    FIND FIRST bf-item_bord_ap NO-LOCK 
        WHERE  bf-item_bord_ap.cod_estab_bord   =  p-table.cod_estab_bord 
          AND  bf-item_bord_ap.cod_portador     =  p-table.cod_portador   
          AND  bf-item_bord_ap.num_bord_ap      =  p-table.num_bord_ap  
          AND  ROWID(bf-item_bord_ap)          <> ROWID(p-table)  
          AND  bf-item_bord_ap.cod_espec_docto <> p-table.cod_espec_docto NO-ERROR.
    IF AVAIL bf-item_bord_ap THEN DO:
       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 17006,
                          INPUT "Esp‚cie inv lida!~~J  existe t¡tulo inserido com a esp‚cie " + bf-item_bord_ap.cod_espec_docto + ", border“ nÆo pode ter duas esp‚cies diferentes").
       RETURN 'NOK'.
    END.

    
    /* Chama tabela espec¡fica para parƒmetros de tributos */
    RUN prgfin\esp\ap006-w01.w (INPUT (IF ext_espec_docto.log_fgts THEN 1 ELSE (IF ext_espec_docto.log_gps THEN 3 ELSE (IF ext_espec_docto.log_imposto THEN 2 ELSE 0))), 
                                INPUT RECID(p-table)) "2".
 END.

RETURN "OK".
