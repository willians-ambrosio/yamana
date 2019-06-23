
/**============================================================**
** Altera‡Æo...: Cleilton Conte
** Empresa.....: DSC
** Data........: 25/02/2015
** Objetivo....: Bloquear ativa‡Æo de itens sem ser pelos programas espec¡ficos
**=============================================================**/
{include/i-prgvrs.i tw-item-uni-estab 11.5.11.000}

/* Variaveis de Parƒmetros */ 
DEF PARAMETER BUFFER p-table     FOR item-uni-estab.
DEF PARAMETER BUFFER p-old-table FOR item-uni-estab.

IF NOT NEW p-table               AND
   p-old-table.cod-obsoleto  = 4 AND
   p-table.cod-obsoleto     <> 4 THEN DO:

    IF NOT SELF:FRAME:WINDOW:INSTANTIATING-PROCEDURE:NAME MATCHES "*YMOF0107*" AND           
       NOT SELF:FRAME:WINDOW:INSTANTIATING-PROCEDURE:NAME MATCHES "*YMOF0117*" AND           
       NOT SELF:FRAME:WINDOW:INSTANTIATING-PROCEDURE:NAME MATCHES "*YMcd0206*" AND           
       NOT SELF:FRAME:WINDOW:INSTANTIATING-PROCEDURE:NAME MATCHES "*YMcd0202rp*" AND         
       NOT SELF:FRAME:WINDOW:INSTANTIATING-PROCEDURE:NAME MATCHES "*YMCD0202*" THEN DO: 

        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Ativa‡Æo do item nÆo permitida~~" + 
                                 "A ativa‡Æo do item para o estabelecimento " + QUOTER(p-table.cod-estabel) + " s¢ pode ser executada atrav‚s do programa YMOF0107.").

        RETURN "NOK".
    END.

END.

RETURN "OK".
