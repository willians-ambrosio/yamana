&ANALYZE-SUSPEND _VERSION-NUMBER WDT_v2r1 Web-Object
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _CUSTOM Definitions
/*------------------------------------------------------------------------
File.............: REL01CC.p
Description......: CONTRATOS DE COMPRAS - VIGÒNCIA
Author...........: DATASUL S.A.
Created..........: 22/09/06 - 11:38 - 20099921
OBS..............: Este fonte foi gerado pelo Data Viewer
------------------------------------------------------------------------*/
def buffer empresa for ems2cadme.empresa.

/* ***************************  Definitions  ************************** */

def var c-param as char no-undo initial "add".
def var r-chave as rowid no-undo.
def var c-return as char no-undo.
def var v-cod-prog-i-rprun as char no-undo.
def var c-versao-mg97 as char format "x(08)" no-undo.
def var i_cont  as int  no-undo.
def var c_aux_1 as char no-undo.
def var c_aux_2 as char no-undo.
def var c_aux_3 as char no-undo.
def var h-boun082 as handle no-undo.
def var h-boun091 as handle no-undo.
def var h-boun136 as handle no-undo.
def var c-programa-mg97 as char no-undo initial "cdr0372".
def var lt-arquivo    as char no-undo.
def var lt-impressora as char no-undo.
def var hid_oper as char no-undo initial "add".

/* Local Variable Definitions ---                                       */

def var w_cod_servid_exec           as char no-undo.
def var w_c_arquivo                 as char no-undo.
def var w_rs_destino                as int  initial 1 no-undo.
def var w_rs_formato                as int  initial 1 no-undo.
def var w_tb_parametro              as log  no-undo.
def var w_dat_inic_exec_servid_exec as date initial today no-undo.
def var w_hra_inic_exec_servid_exec as char no-undo.


def var w_da-dt-ini-validade-ini like contrato-for.dt-ini-validade no-undo initial 01/01/1800.
def var w_da-dt-ini-validade-fim like contrato-for.dt-ini-validade no-undo initial 12/31/9999.
def var w_da-dt-ter-validade-ini like contrato-for.dt-ter-validade no-undo initial 01/01/1800.
def var w_da-dt-ter-validade-fim like contrato-for.dt-ter-validade no-undo initial 12/31/9999.

/* Temporary Table Definitions ---                                      */

{unbo/boun082.i tt-imprsor_usuar}
{unbo/boun091.i tt-layout_impres}

{btb/btb912zb.i}

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            like empresa.ep-codigo
    field da-dt-ini-validade-ini like contrato-for.dt-ini-validade
    field da-dt-ini-validade-fim like contrato-for.dt-ini-validade
    field da-dt-ter-validade-ini like contrato-for.dt-ter-validade
    field da-dt-ter-validade-fim like contrato-for.dt-ter-validade
.


/* Transfer Definitions */

def var raw-param        as raw no-undo.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Web-Object

&ANALYZE-RESUME

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _INCLUDED-LIBRARIES
/* Included Libraries --- */
 {src/web/method/wrap-cgi.i}
&ANALYZE-RESUME _END-INCLUDED-LIBRARIES

&ANALYZE-SUSPEND _CODE-BLOCK _CUSTOM "Main Code Block" 

/* ************************  Main Code Block  *********************** */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   RUN dispatch ('destroy':U).

/* Process the latest WEB event. */
RUN process-web-request.

/* Run the local/adm-destroy procedures, if the procedure is ending.    */
IF NOT THIS-PROCEDURE:PERSISTENT THEN RUN dispatch ('destroy':U).
&ANALYZE-RESUME
/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE folderImpressao 
PROCEDURE folderImpressao :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

    {&out}
      '<br>'
      '<div align="center"><center>':U skip
      '<table border="0" cellpadding="0" cellspacing="1" width="80%" class="tableForm" align="center">':U skip
      .
    assign c_aux_1 = "CONTRATOS DE COMPRAS - VIG~&ecirc;NCIA".
    {&out}
      '<tr>':U skip
      '<td align="center" colspan="3" class="barratitulo">' c_aux_1 '</td>':U skip
      '</tr>':U skip
      '<tr>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
      .
    assign c_aux_1 = "Sele~&ccedil;~&atilde;o".
    assign c_aux_2 = "Par~&acirc;metros".
    assign c_aux_3 = "Impress~&atilde;o".
    {&out}
      'class="linhaForm"><a href="#folder1">' c_aux_1 '</a></td>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
      'class="linhaForm"><strong>' c_aux_2 '</strong></td>':U skip
      '<td align="center" width="33%" nowrap class="linhaForm"><table border="1"':U skip
      'cellpadding="0" cellspacing="1" width="100%"':U skip
      'bgcolor="#C2C2C2">':U skip
      '<tr>':U skip
      '<td align="center" nowrap':U skip
      'class="linhaForm"><a href="#folder3">' c_aux_3 '</a></td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</center></div>':U skip
   .

    assign c_aux_1 = "Destino".
    assign c_aux_2 = "Impressora".
    assign c_aux_3 = "Arquivo".
    {&out}
      '<table border="1" cellpadding="0" cellspacing="1" width="80%" height="310" bgcolor="#C0C0C0" align="center">':U skip
      '<tr>':U skip
      '<td align="center" colspan="5" width="100%">':U skip
      '<table border="0" width="60%" cellpadding="0" cellspacing="0">':U skip
      '<tr>':U skip
      '<th class="linhaForm"><fieldset>':U skip
      '<legend>' c_aux_1 ':</legend>':U skip
      '<table border="0" width="100%">':U skip
      '<tr>':U skip
      '<th class="linhaForm"><label for="R11"><input type="radio" name="w_rs_destino"':U skip
      'value="1" id="R11" onClick="habImpressora()"':U (if w_rs_destino = 1 then ' checked' else '') 
      '>' c_aux_2 '</label></th>':U skip
      '<th class="linhaForm"><label for="R12"><input type="radio" name="w_rs_destino"':U skip
      'value="2" id="R12" onClick="habArquivo()"':U (if w_rs_destino = 2 then ' checked' else '')
      '>' c_aux_3 '</label></td>':U skip
      '</tr>':U skip
      '<tr>':U skip
      '<th class="linhaForm" colspan="2" nowrap><input type="text" size="60" maxlength="256" name="w_c_arquivo" value="':U
      w_c_arquivo '">':U skip
      '<a href="javascript:abreZoom(~'web/wutp/wu-impr.p~',530,600,~'tt-layout_impres.cod_layout_impres,w_c_arquivo~')">':U skip
      '<img src="/ems20web/wimages/ii-sea2.gif" align="absmiddle" border="0" width="23" height="23">':U skip
      '</a>':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</fieldset></td>':U skip
   .

    assign c_aux_1 = "Execu~&ccedil;~&atilde;o Batch".
    assign c_aux_2 = "Servidor Execu~&ccedil;~&atilde;o".
    assign c_aux_3 = "Data In~&iacute;cio Execu~&ccedil;~&atilde;o".
    {&out}
      '</tr>':U skip
      '<tr><td>&nbsp;</td></tr>':U skip
      '<tr>':U skip
      '<th class="linhaForm"><fieldset>':U skip
      '<legend>' c_aux_1 ':</legend>':U skip
      '<table border="0" width="100%">':U skip
      '<tr>':U skip
      '<th class="linhaForm" align="right">' c_aux_2 ':</td>':U skip
      '<td><input type="text" name="w_cod_servid_exec" size="8" maxlength="8" value="':U
      w_cod_servid_exec '">':U skip
      '<a href="javascript:abreZoom(~'web/btb/wbtb012ka.p~',850,330,~'tt-servid_exec.cod_servid_exec,w_cod_servid_exec~')">':U skip
      '<img src="/ems20web/wimages/ii-sea2.gif" align="middle" border="0" width="23" height="23">':U skip
      '</a>':U skip
      '</td>':U skip
      '</tr>':U skip
      '<tr>':U skip
      '<th class="linhaForm" align="right">' c_aux_3 ':</td>':U skip
      '<td nowrap>':U skip
      '<input type="text" name="w_dat_inic_exec_servid_exec" size="10" maxlength="10" value="':U
      string(w_dat_inic_exec_servid_exec,"99/99/9999") '">':U skip
      '<input type="text" name="w_hra_inic_exec_servid_exec" size="8" maxlength="8" value="':U
      string(w_hra_inic_exec_servid_exec,"99:99:99") '">':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</fieldset></td>':U skip
   .

    assign c_aux_1 = "Par~&acirc;metros de Impress~&atilde;o".
    assign c_aux_2 = "Imprimir P~&aacute;gina de Par~&acirc;metros".
    {&out}
      '</tr>':U skip
      '<tr><td>&nbsp;</td></tr>':U skip
      '<tr>':U skip
      '<th class="linhaForm"><fieldset>':U skip
      '<legend>' c_aux_1 ':</legend>':U skip
      '<table border="0" width="100%">':U skip
      '<tr>':U skip
      '<th class="linhaForm" colspan="2"><label for="C2"><input type="checkbox"':U skip
      'name="w_tb_parametro" id="C2"':U (if w_tb_parametro = true then ' checked' else '')
      '>' c_aux_2 '</label></td>':U skip
      '</tr>':U skip
      '<tr>':U skip
   .

    assign c_aux_1 = "80 Colunas".
    assign c_aux_2 = "132 Colunas".
    {&out}
      '<th align="left" class="linhaForm"><label for="R31">&nbsp;&nbsp;&nbsp;<input type="radio" name="w_rs_formato"':U skip
      'value="1" id="R31"' (if w_rs_formato = 1 then ' checked' else '') 'disabled>' c_aux_1 '</label></th>':U skip
      '<th align="left" class="linhaForm"><label for="R32"><input type="radio" name="w_rs_formato"':U skip
      'value="2" id="R32"' (if w_rs_formato = 2 then ' checked' else '') '>' c_aux_2 '</label></th>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</fieldset>':U skip
      '</td>':U skip
      '<tr>':U skip
      '</table>':U skip
      '<br>':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
   .
END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE folderParametros 
PROCEDURE folderParametros :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    assign c_aux_1 = "CONTRATOS DE COMPRAS - VIG~&ecirc;NCIA".
    {&out}
      '<br>'
      '<div align="center"><center>':U skip
      '<table border="0" cellpadding="0" cellspacing="1" width="80%" class="tableForm" align="center">':U skip
      '<tr>':U skip
      '<td align="center" colspan="3" class="barratitulo">' c_aux_1 '</td>':U skip
      '</tr>':U skip
      '<tr>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
   .

    assign c_aux_1 = "Sele~&ccedil;~&atilde;o".
    assign c_aux_2 = "Par~&acirc;metros".
    assign c_aux_3 = "Impress~&atilde;o".
    {&out}
      'class="linhaForm"><a href="#folder1">' c_aux_1 '</a></td>':U skip
      '<td align="center" width="33%" nowrap class="linhaForm"><table border="1"':U skip
      'cellpadding="0" cellspacing="1" width="100%"':U skip
      'bgcolor="#C2C2C2">':U skip
      '<tr>':U skip
      '<td align="center" width="100%" nowrap':U skip
      'class="linhaForm"><a href="#folder2">' c_aux_2 '</a></td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</td>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
      'class="linhaForm"><a href="#folder3">' c_aux_3 '</a></td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</center></div>':U skip
   .

    {&out}
      '<table border="1" cellpadding="0" cellspacing="1" width="80%" bgcolor="#C0C0C0" height="310" align="center">':U skip
      '<tr>':U skip
      '<td align="center" valign="top" colspan="1"':U skip
      'width="100%" nowrap class="linhaForm"><br>':U skip
      '<table border="0" cellpadding="0" cellspacing="1">':U skip
      '<tr>':U skip
   .

   .

    {&out}
      '</tr>':U skip
      '</table>':U skip
      '<br>':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
   .

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE folderSelecao 
PROCEDURE folderSelecao :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    assign c_aux_1 = "CONTRATOS DE COMPRAS - VIG~&ecirc;NCIA".
    {&out}
      '<br>'
      '<div align="center"><center>':U skip
      '<table border="0" cellpadding="0" cellspacing="1" width="80%" align="center" class="tableForm">':U skip
      '<tr>':U skip
      '<td align="center" colspan="3" class="barratitulo">' c_aux_1 '</td>':U skip
      '</tr>':U skip
      '<tr>':U skip
      '<td align="center" width="33%" nowrap class="linhaForm">':U skip
      '<table border="1" cellpadding="0" cellspacing="1" width="100%" bgcolor="#C2C2C2">':U skip
      '<tr>':U skip
      '<td align="center" width="100%" nowrap':U skip
   .

    assign c_aux_1 = "Sele~&ccedil;~&atilde;o".
    assign c_aux_2 = "Par~&acirc;metros".
    assign c_aux_3 = "Impress~&atilde;o".
    {&out}
      'class="linhaForm"><a href="#folder1">' c_aux_1 '</a></td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</td>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
      'class="linhaForm"><strong>' c_aux_2 '</strong></td>':U skip
      '<td align="center" width="33%" bgcolor="#DCDCDC" nowrap':U skip
      'class="linhaForm"><a href="#folder3">' c_aux_3 '</a></td>':U skip
      '</tr>':U skip
      '</table>':U skip
      '</center></div>':U skip
   .

    {&out}
      '<table border="1" cellpadding="0" cellspacing="1" width="80%" bgcolor="#C0C0C0" height="310" align="center">':U skip
      '<tr>':U skip
      '<td align="center" valign="top" colspan="5" width="100%" nowrap class="linhaForm"><br>':U skip
      '<table border="0" cellpadding="0" cellspacing="1">':U skip
   .

    assign c_aux_1 = "Data In~&iacute;cio Validade".
    {&out}
      '<tr>':U skip
      '<th align="right" nowrap class="linhaForm">' c_aux_1 ':</th>':U skip
      '<td nowrap class="linhaForm"><input type="text" size="10" maxlength="10" name="w_da-dt-ini-validade-ini"':U
      'value="':U string(w_da-dt-ini-validade-ini,"99/99/9999") '"></td>':U skip
      '<td><img src="/ems20web/wimages/im-fir.gif" width="20" height="20"></td>':U skip
      '<td><img src="/ems20web/wimages/im-las.gif" width="20" height="20"></td>':U skip
      '<td nowrap class="linhaForm"><input type="text" size="10" maxlength="10" name="w_da-dt-ini-validade-fim"':U
      'value="':u string(w_da-dt-ini-validade-fim,"99/99/9999") '"></td>':U skip
      '</tr>':U skip
   .

    assign c_aux_1 = "Data T~&eacute;rmino".
    {&out}
      '<tr>':U skip
      '<th align="right" nowrap class="linhaForm">' c_aux_1 ':</th>':U skip
      '<td nowrap class="linhaForm"><input type="text" size="10" maxlength="10" name="w_da-dt-ter-validade-ini"':U
      'value="':U string(w_da-dt-ter-validade-ini,"99/99/9999") '"></td>':U skip
      '<td><img src="/ems20web/wimages/im-fir.gif" width="20" height="20"></td>':U skip
      '<td><img src="/ems20web/wimages/im-las.gif" width="20" height="20"></td>':U skip
      '<td nowrap class="linhaForm"><input type="text" size="10" maxlength="10" name="w_da-dt-ter-validade-fim"':U
      'value="':u string(w_da-dt-ter-validade-fim,"99/99/9999") '"></td>':U skip
      '</tr>':U skip
   .

    {&out}
      '</table>':U skip
      '<br>':U skip
      '</td>':U skip
      '</tr>':U skip
      '</table>':U skip
   .

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE output-header 
PROCEDURE output-header :

  output-content-type ("text/html":U).

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE output-javascript 
PROCEDURE output-javascript :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {&out}
     '<script language="JavaScript">'
     /*----------------------------------------------------------------------
        File        : WI-ABRZO.I
       --------------------------------------------------------------------*/
     '   function abreZoom(nome_prog_zoom, lar, alt, retorno) ~{ ' skip
     /* definição de variáveis para documentação */
     '      i_loop = 0 ; ' skip    /* controle de loop */
     '      c_loop = 0 ; ' skip    /* controle de loop */
     '      retorno2 = "" ; ' skip /* parâmetro javascritp traduzido para parâmetro URL */
     /* varre parâmetro retorno para saber quais campos serão usados no zoom */
     '      while ( i_loop < retorno.length && i_loop >= 0 ) ~{ ' skip
     '         f_element = retorno.indexOf(",",i_loop) ; ' skip
     '         if (f_element==-1) ~{f_element=retorno.length} ; ' skip
     '         aux = retorno.substring(i_loop,f_element) ; ' skip
     '         retorno2 = retorno2 + aux + "," ;' skip
     '         i_loop = f_element + 1 ; ' skip
     '         f_element = retorno.indexOf(",",i_loop) ; ' skip
     '         if (f_element==-1) ~{f_element=retorno.length} ; ' skip
     '         aux = retorno.substring(i_loop,f_element) ; ' skip
     '         for (c_loop=0;c_loop < document.forms[0].elements.length;c_loop++) ~{ ' skip
     '            if (document.forms[0].elements[c_loop].name == aux) ~{ ' skip
     '               retorno2 = retorno2 + c_loop + "," ; ' skip
     '               c_loop = document.forms[0].elements.length ; ' skip
     '            } ' skip
     '         } ' skip
     '         i_loop = f_element + 1 ; ' skip
     '      } ' skip
     '      retorno2 = retorno2.substring(0,retorno2.length-1) ; ' skip
     '      nome_prog_zoom += "?retorno=" + retorno2 + "&param_ini=" + document.forms[0].w_c_arquivo.value + "&servid=" + document.forms[0].w_cod_servid_exec.value ; ' skip
     /* ativa página de zoom com parâmetros na URL */
     '      var zWindow = window.open("' + appURL + '/" + nome_prog_zoom, "zoomWindow","top=0,left=0,width=" + lar + ",height=" + alt + ",scrollbars=no,toolbar=no,location=no,directories=no,status=no,resizable=yes"); ' skip
     '   } ' skip
     '</script>'
 {&end}

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE pi-executar 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   def var c-arq-aux    as char no-undo.

    {web/winclude/wi-vglob.i}

    {utp/ut-liter.i Impressora *}
   assign lt-impressora = return-value.

    {utp/ut-liter.i Arquivo *}
   assign lt-arquivo = return-value.
   if w_rs_destino = 2 then do:
      run utp/ut-vlarq.p (input w_c_arquivo).
      if return-value = "NOK" then do:
         run utp/ut-msgs.p (input "msg",
                            input 73,
                            input "" ).
         c-return = return-value.
         return c-return.
      end.
   end.

   create tt-param.
   assign tt-param.usuario              = v_cod_usuar_corren
          tt-param.destino              = w_rs_destino
          tt-param.data-exec            = w_dat_inic_exec_servid_exec
          tt-param.hora-exec            = integer(w_hra_inic_exec_servid_exec)
          tt-param.parametro            = w_tb_parametro
          tt-param.formato              = w_rs_formato
          tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
          tt-param.ep-codigo            = v_cod_empres_usuar
          tt-param.arquivo              = w_c_arquivo

          tt-param.da-dt-ini-validade-ini = w_da-dt-ini-validade-ini
          tt-param.da-dt-ini-validade-fim = w_da-dt-ini-validade-fim
          tt-param.da-dt-ter-validade-ini = w_da-dt-ter-validade-ini
          tt-param.da-dt-ter-validade-fim = w_da-dt-ter-validade-fim
.

/*** In¡cio i-rpexb.i ***/

def var c-impressora as char no-undo.
def var c-layout as char no-undo.

if tt-param.destino = 1 then do:
   if num-entries(tt-param.arquivo,":") = 2 then do:
     assign c-impressora = substring(w_c_arquivo,1,index(w_c_arquivo,":") - 1).
     assign c-layout = substring(w_c_arquivo,index(w_c_arquivo,":") + 1,length(w_c_arquivo) - index(w_c_arquivo,":")).
     run findSrvdxcmp_id in h-boun136 (w_cod_servid_exec, c-impressora, output c-return).
     if c-return <> "" then do:
         run utp/ut-msgs.p (input "msg",
                            input 4853,
                            input "" ).
         c-return = return-value.
         return c-return.
     end.
     run findLytmprs_id in h-boun091 (c-impressora, c-layout, output c-return).
     if c-return <> "" then do:
         run utp/ut-msgs.p (input "msg",
                            input 4306,
                            input "" ).
         c-return = return-value.
         return c-return.
     end.
  end.
  else do:
     assign tt-param.arquivo = w_c_arquivo.
     assign c-impressora = entry(1,w_c_arquivo,":").
     assign c-layout = if  num-entries(w_c_arquivo,":") < 2 then ? else entry(2,w_c_arquivo,":").
     run findSrvdxcmp_id in h-boun136 (w_cod_servid_exec, c-impressora, output c-return).
     if c-return <> "" then do:
         run utp/ut-msgs.p (input "msg",
                            input 4853,
                            input "" ).
         c-return = return-value.
         return c-return.
     end.       
     run findLytmprs_id in h-boun091 (c-impressora, c-layout, output c-return).
     if c-return <> "" then do:
         run utp/ut-msgs.p (input "msg",
                            input 4306,
                            input "" ).
         c-return = return-value.
         return c-return.
     end.       
  end.
end.  

/*** Fim i-rpexb.i ***/

   raw-transfer tt-param to raw-param.

   create tt_param_segur.
   assign tt_param_segur.tta_num_vers_integr_api      = 3
          tt_param_segur.tta_cod_aplicat_dtsul_corren = "TEC"
          tt_param_segur.tta_cod_empres_usuar         = v_cod_empres_usuar
          tt_param_segur.tta_cod_grp_usuar_lst        = v_cod_grp_usuar_lst
          tt_param_segur.tta_cod_idiom_usuar          = v_cod_idiom_usuar
          tt_param_segur.tta_cod_modul_dtsul_corren   = "crb"
          tt_param_segur.tta_cod_pais_empres_usuar    = v_pais_impto_usuario
          tt_param_segur.tta_cod_usuar_corren         = v_cod_usuar_corren
          tt_param_segur.tta_cod_usuar_corren_criptog = v_cod_usuar_corren_criptog.

   create tt_ped_exec.
   assign tt_ped_exec.tta_num_seq                = 1
          tt_ped_exec.tta_cod_usuario            = v_cod_usuar_corren
          tt_ped_exec.tta_cod_prog_dtsul         = "REL01CC"
          tt_ped_exec.tta_cod_prog_dtsul_rp      = "dtv/REL01C3.p"
          tt_ped_exec.tta_cod_release_prog_dtsul = "2.00.00.001"
          tt_ped_exec.tta_dat_exec_ped_exec      = w_dat_inic_exec_servid_exec
          tt_ped_exec.tta_hra_exec_ped_exec      = w_hra_inic_exec_servid_exec
          tt_ped_exec.tta_cod_servid_exec        = w_cod_servid_exec
          tt_ped_exec.tta_cdn_estil_dwb          = 97.

   create tt_ped_exec_param.
   assign tt_ped_exec_param.tta_num_seq              = 1
          tt_ped_exec_param.tta_cod_dwb_file         = "dtv/REL01C3.p"
          tt_ped_exec_param.tta_cod_dwb_output       = (if w_rs_destino=1 then lt-impressora else lt-arquivo)
          tt_ped_exec_param.tta_nom_dwb_printer      = w_c_arquivo
          tt_ped_exec_param.tta_cod_dwb_print_layout = w_c_arquivo.
          raw-transfer tt-param to tt_ped_exec_param.tta_raw_param_ped_exec.

        
   run btb/btb912zb.p (input-output table tt_param_segur,
                       input-output table tt_ped_exec,
                       input table tt_ped_exec_param,
                       input table tt_ped_exec_param_aux,
                       input table tt_ped_exec_sel).

  if return-value = "OK" then do:
      find first tt_ped_exec no-lock.
      run utp/ut-msgs.p (input "msg",
                         input 4169,
                         input string(tt_ped_exec.tta_num_ped_exec)).
      assign c-return = return-value.
  end.
  else do:
      find first tt_ped_exec no-lock.
      if tt_ped_exec.ttv_num_msg_erro <> 0 then 
      assign c-return = string(tt_ped_exec.ttv_num_msg_erro) + ": " + tt_ped_exec.ttv_cod_msg_parameters.
      else do:
         find first tt_param_segur no-lock.
         assign c-return = string(tt_param_segur.ttv_num_msg_erro) + ": " + tt_param_segur.ttv_des_msg.
      end.
  end.
  return c-return.

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE process-web-request 
PROCEDURE process-web-request :

   RUN unbo/boun091.p persistent set h-boun091.
   RUN unbo/boun136.p persistent set h-boun136.
   RUN output-header.

   if request_method = "POST" then do:
      run recuperaValores.
      run pi-executar.
   end.
   else assign w_hra_inic_exec_servid_exec = replace(string(time,"HH:MM:SS"), ":", "").

   run output-javascript.

    {web/winclude/wi-jscrp.i "<Programa de GoTo>" "<largura>" "<altura>" "<Programa de Search>?funcao=search&prog_requis=<nome deste programa>" "<largura>" "<altura>"}

    {&out}
      '<html>':U skip
      '<head>':U skip
      '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">':U skip
      '<meta name="GENERATOR" content="DataViewer 2.03">':U skip
      '<link rel="StyleSheet" type="text/css" href="/ems20web/padrao.css">':U skip
   .
    {&out}
      '<script language="JavaScript">':U skip
      'var vImpr="";':U skip
      'var vArq="";':U skip
      'parent.document.title = "";':U skip
      'function inicializa() ~{':U skip
      'parent.panel("UOHE","REL01CC 2.00.00.001");':U skip
      'document.forms[0].w_c_arquivo.readOnly = true;':U skip
      '}':U skip
      'function habArquivo() ~{':U skip
      'vImpr = document.forms[0].w_c_arquivo.value;':U skip
      'document.forms[0].w_c_arquivo.value = vArq;':U skip
      'document.forms[0].w_c_arquivo.readOnly = false;':U skip
      '}':U skip
      'function habImpressora() ~{':U skip
      'vArq = document.forms[0].w_c_arquivo.value;':U skip
      'document.forms[0].w_c_arquivo.value = vImpr;':U skip
      'document.forms[0].w_c_arquivo.readOnly = true;':U skip
      '}':U skip
      '</script>':U skip
   .
    {&out}
      '</head>':U skip
      '<body topmargin="0" leftmargin="0" onLoad="inicializa()">':U skip
      '<form method="post">':U skip
   .

    {&out} '<a name="folder1"></a>':U skip .
   run folderSelecao.
    {&out} '<br><br><br><br><br><br><br><br><br><br><br><br><br>':U skip .
    {&out} '<a name="folder3"></a>':U skip .
   run folderImpressao.
    {&out} '<br><br><br><br><br><br><br><br><br><br><br><br><br>':U skip 
          '</form>':U skip.

   if c-return <> "" then do:
       {&out}
         '<script language="javascript">':U skip
         'alert("' c-return '");':U skip
         '</script>':U skip
      .
   end.

    {&out}
      '</body>':U skip
      '</html>':U skip
   .
  delete procedure h-boun136.
  delete procedure h-boun091.

END PROCEDURE.
&ANALYZE-RESUME
&ANALYZE-SUSPEND _CODE-BLOCK _PROCEDURE recuperaValores 
PROCEDURE recuperaValores :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

assign
   w_cod_servid_exec           = get-value("w_cod_servid_exec")
   w_c_arquivo                 = get-value("w_c_arquivo")
   w_dat_inic_exec_servid_exec = date(get-value("w_dat_inic_exec_servid_exec"))
   w_hra_inic_exec_servid_exec = replace(get-value("w_hra_inic_exec_servid_exec"), ":", "")
   w_tb_parametro              = (if get-value("w_tb_parametro") <> "" then true else false)
   w_rs_destino                = integer(get-value("w_rs_destino"))
   w_rs_formato                = integer(get-value("w_rs_formato"))
   w_da-dt-ini-validade-ini = date(get-value("w_da-dt-ini-validade-ini"))
   w_da-dt-ini-validade-fim = date(get-value("w_da-dt-ini-validade-fim"))
   w_da-dt-ter-validade-ini = date(get-value("w_da-dt-ter-validade-ini"))
   w_da-dt-ter-validade-fim = date(get-value("w_da-dt-ter-validade-fim"))
.

END PROCEDURE.
&ANALYZE-RESUME

