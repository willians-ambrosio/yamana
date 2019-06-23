/**============================================================**
** Altera‡Æo...: Cleilton Conte
** Empresa.....: DSC
** Data........: 25/02/2015
** Objetivo....: Bloquear ativa‡Æo de itens sem ser pelos programas espec¡ficos
**=============================================================**/
/**============================================================**
** Altera‡Æo...: Felipe Vieira
** Empresa.....: Grupo DKP
** Data........: 20/03/2019
** Objetivo....: Salvar a data da altera‡Æo na tabela ext_manut, quando campos alguns campos da tabela item forem alterados (os campos estao contidos na lista da variavel liscam)
**=============================================================**/
{include/i-prgvrs.i tw-item 11.5.11.000}

/* Variaveis de Parƒmetros */ 
DEF PARAMETER BUFFER p-table     FOR item.
DEF PARAMETER BUFFER p-old-table FOR item.

DEFINE VARIABLE lisCamAlt AS CHARACTER   NO-UNDO. /*salva os campos que foram alterados*/
DEFINE VARIABLE lisCam    AS CHARACTER   NO-UNDO. /*armazena os campos de devem ser considerados para alteracao na tabela especifica ext_item*/
DEFINE VARIABLE i-aux     AS INTEGER     NO-UNDO. /*contador*/

lisCam = "descricao-1,descricao-2,tipo-con-est,lote-economi,data-obsol".
FUNCTION verificaCampos RETURNS LOG (INPUT lisCam AS CHAR, INPUT lisCamAlt AS CHAR ) FORWARD.

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
                           input "Ativa‡Æo de item nÆo permitida~~" + 
                                 "A ativa‡Æo de um item s¢ pode ser executada atrav‚s do programa YMOF0107.").

        RETURN "NOK".
    END.

END.

IF NOT NEW p-table THEN DO:

    BUFFER-COMPARE p-table TO p-old-table
        SAVE RESULT IN lisCamAlt.

    IF lisCamAlt <> "" THEN DO:
       IF verificaCampos(lisCam,lisCamAlt) THEN DO:

           FIND FIRST ext_item EXCLUSIVE-LOCK
               WHERE ext_item.it-codigo = p-table.it-codigo NO-ERROR.

           IF AVAIL ext_item THEN
               ASSIGN ext_item.dataAlt  = NOW
                      ext_item.situacao = 1.

       END.
    END.

END.

FUNCTION verificaCampos RETURNS LOG (INPUT lisCam AS CHAR, INPUT lisCamAlt AS CHAR ):

   REPEAT i-aux = 1 TO NUM-ENTRIES(lisCam):
       IF LOOKUP(ENTRY(i-aux,lisCam),lisCamAlt) <> 0 THEN
         RETURN TRUE.
   END.
    
   RETURN FALSE.

END FUNCTION.

RETURN "OK".
