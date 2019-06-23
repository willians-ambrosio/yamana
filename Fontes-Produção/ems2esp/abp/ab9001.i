/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/*------------------------------------------------------------------------------
  Purpose:     piConverteMoeda
  Parameters:  entrada pPadrao = Valor moeda PadrÆo
               sa¡da   pAlter1 = Valor moeda Alternativa 1
               sa¡da   pAlter2 = Valor moeda Alternativa 2
               entrada pMovto  = Data de Movimento do apontamento
  Notes:       Faz o processamento necess rio para que o campo de valor seja 
               convertido para moedas alternativas.
------------------------------------------------------------------------------*/
procedure piConverteMoeda:

define input  parameter pPadrao like mab-item-lubrific.val-mater-1  no-undo.
define output parameter pAlter1 like mab-item-lubrific.val-mater-2  no-undo.
define output parameter pAlter2 like mab-item-lubrific.val-mater-3  no-undo.
define input  parameter pMovto  like mab-abastec-lubrific.dat-movto no-undo.

define variable dePadrao         as decimal no-undo.
define variable deConvertido     as decimal no-undo.

assign dePadrao = pPadrao.

for first param-estoq fields(tem-moeda1 tem-moeda2) no-lock:
    /** Converte o campo da moeda alternativa (1) **/
    if param-estoq.tem-moeda1 then do:
        run cdp/cd0813.p(0, 1, 
                         dePadrao, pMovto,
                         output deConvertido).
        assign pAlter1 = if deConvertido = ?
                         then 0
                         else deConvertido.

    end.
    /** Converte o campo da moeda alternativa (1) **/
    if param-estoq.tem-moeda2 then do:
        run cdp/cd0813.p(0, 2, 
                         dePadrao, pMovto,
                         output deConvertido).
        assign pAlter2 = if deConvertido = ?
                         then 0
                         else deConvertido.
    end.
end.

return "OK":U.



END PROCEDURE.


/*------------------------------------------------------------------------------
  Purpose:     piRetornMoeda
  Parameters:  sa¡da   p-Moeda = Descri‡Æo do tipo de Moeda encontrado
            
  Notes:       Faz o processamento necess rio para buscar o tipo de moeda utilizado.
------------------------------------------------------------------------------*/
procedure piRetornaMoeda:

define output parameter p-moeda AS CHAR FORMAT "x(80)" no-undo.
define buffer bfmoeda for moeda.
define variable  c-moeda     as CHAR FORMAT "x(80)" no-undo.

/**Busca Moeda ***/
for moeda where
         moeda.mo-codigo = 0 NO-LOCK:
    /*** Moeda 0 - moeda corrente ***/
    assign c-moeda   = '0 - ' + moeda.descricao.
      /** Se encontra moeda = 0, busca nos parƒmetros e de estoque e verifica se existe moeda 1 e 2 ***/
    for first param-estoq no-lock:
        if  param-estoq.tem-moeda1 then do:
            for bfmoeda where
                 bfmoeda.mo-codigo = param-estoq.moeda1 NO-LOCK:
                /*** Descri‡Æo da moeda 1 ***/
            assign c-moeda = c-moeda + ',1 - ' + bfmoeda.descricao.
                end.
        end.
        if  param-estoq.tem-moeda2 then do:
            for bfmoeda where
                 bfmoeda.mo-codigo = param-estoq.moeda2 NO-LOCK:
                /*** Descri‡Æo da Moeda 2 ***/
            assign c-moeda = c-moeda + ',2 - ' + bfmoeda.descricao.
            end.
        end.
   end.
    ASSIGN p-moeda = c-moeda.
end.


return "OK":U.

END PROCEDURE.
