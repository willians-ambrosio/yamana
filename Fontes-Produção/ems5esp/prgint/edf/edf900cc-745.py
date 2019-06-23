/****************************************************************************************** 
** 	   Programa: edf900cc-745.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 09/07/2018
** Change/Chamado: 
**      Objetivo: Retorna a conta cosmo
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: Mapa do EDI - Citibank Exportaá∆o 2 - Mapa 200113
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{prgint\edf\edf900.i}

/************************ Parameter Definition Begin ************************/

def Input param p_cdn_mapa_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param p_cdn_segment_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param p_cdn_element_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param table 
    for tt_param_program_formul.


/****************************** Main Code Begin *****************************/
/* Retorna c¢digo da conta cosmos para header do lote */

/*
output to c:\temp\prog.txt append.

for each tt_param_program_formul:

    put unformatted 
        tt_param_program_formul.tta_cdn_segment_edi ';'
        tt_param_program_formul.tta_cdn_element_edi ';'
        tt_param_program_formul.ttv_des_contdo skip(01).

end.

output close.
*/

 find tt_param_program_formul
   where tt_param_program_formul.tta_cdn_segment_edi = 288
     and tt_param_program_formul.tta_cdn_element_edi = 3903 no-error.

 /* Retorna a conta Cosmo */
 find ext-cta_corren no-lock where
      ext-cta_corren.cod_cta_corren = trim(tt_param_program_formul.ttv_des_contdo) no-error.
 if avail ext-cta_corren
     then return trim(ext-cta_corren.cod_cta_cosmo).

 /* Caso n∆o encontre, retorna a conta cosmos */
 return trim(tt_param_program_formul.ttv_des_contdo).

