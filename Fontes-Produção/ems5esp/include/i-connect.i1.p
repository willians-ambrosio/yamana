/*************************************************************************
**
**          i-conect.I01 - Definiá∆o Vari†veis para Impress∆o
**
**************************************************************************/

def var c-cabec1     as char format "X(132)".
def var c-cabec2     as char format "X(132)".          
def var c-textolabel as char no-undo.
def var l-erro-nota  as log initial no no-undo.

assign c-cabec1 = "".
        
{utp/ut-field.i mgind docum-est serie-docto 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,5,"character") + " ".

{utp/ut-field.i mgind docum-est nro-docto 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,16,"character") + " ".

{utp/ut-field.i mgind docum-est cod-emitente 1}    
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,8,"character") + "   ".

{utp/ut-field.i mgind docum-est nat-operacao 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,8,"character") + "  ".

{utp/ut-liter.i Erro * R}        
assign c-cabec1 = c-cabec1 + trim(return-value) + "    ".
{utp/ut-liter.i Mensagem * R}        
assign c-cabec1 = c-cabec1 + trim(return-value).
assign c-cabec2 = "----- ---------------- ---------  --------  ------  --------------------------------------------------------------------------------".
    
/* fim do include */
