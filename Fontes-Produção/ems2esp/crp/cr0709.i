def var l-mostra-titulo-cliente              as log init no no-undo.
def var l-mostra-faixa-titulo                as log init no no-undo.
def var i-cont                               as int init 0  no-undo.
def var l-chamado-cons-cli                   as log init no no-undo.

find first funcao
     where funcao.cd-funcao = "spp-mostra-titulo-cliente" no-lock no-error.
if avail funcao then
    assign l-mostra-titulo-cliente = yes.


find first funcao
     where funcao.cd-funcao = "spp-mostra-faixa-titulo" no-lock no-error.
if avail funcao then
    assign l-mostra-faixa-titulo = yes.

assign l-chamado-cons-cli = no.
do i-cont = 0 to 20:
  if program-name(i-cont) matches("*b55ad264.w*") /*Consulta Cliente*/ 
  or program-name(i-cont) matches("*b56ad264.w*") /*Consulta Matriz*/  
  or program-name(i-cont) matches("*b66ad264.w*") /*Vincula AN's DP's*/
  or program-name(i-cont) matches("*b10ad183.w*") /*Consulta Movtos CR*/  
  or program-name(i-cont) matches("*b11ad264.w*") /*Consulta Border“*/
  or program-name(i-cont) matches("*cb0105n-b01.w*") /*Consulta Fluxo Caixa*/
  or program-name(i-cont) matches("*b39ad264.w*") /*consulta Estabelecimento*/
  or program-name(i-cont) matches("*b41ad264.w*") /*Consulta Empresa*/
  or program-name(i-cont) matches("*cb0403d.w*")  /*Movimentos Conta Corrente*/
  or program-name(i-cont) matches("*cd0007.p*")
  or program-name(i-cont) matches("*cr0709ha.p*")
  or program-name(i-cont) matches("*b06di235.w*") /*Informa‡äes Embarque*/
  or program-name(i-cont) matches("*ve0901-b01.w*") /*Negocia‡Æo Vendor*/
  or program-name(i-cont) matches("*ve0901-b02.w*") /*Negocia‡Æo Vendor*/
  or program-name(i-cont) matches("*ve0901-b03.w*") /*Negocia‡Æo Vendor*/
  or program-name(i-cont) matches("*arg512-b01.w*")
  or program-name(i-cont) matches("*chi512-b01.w*")
  or program-name(i-cont) matches("*cd0132c.w*") /*encontro contas*/
  or program-name(i-cont) matches("*cr0770a-b01.w*") /*Detalje Cheque*/
  or program-name(i-cont) matches("*ct0702c-b04.w*") /*Movimentos Contas Contabeis*/
  or program-name(i-cont) matches("*ec0015c.p*") /*consulta Contrato Exporta‡Æo*/
  or program-name(i-cont) matches("*ec0018.w*") /*Consulta T¡tulo Exporta‡Æo*/
  or program-name(i-cont) matches("*rc0301.w*") /*Consulta comissäes*/    then do:
     
      assign l-chamado-cons-cli = yes.
      leave.

  end.
end.

if l-chamado-cons-cli then
   assign l-mostra-faixa-titulo = no. 
