/* i-versao-ra.i
** definicao de versao para o recebimento automatico
** {include\i-versao-ra.i}
**/

DEF VAR c-versao        AS CHAR     NO-UNDO.
    
ASSIGN c-versao = " - RA 07.02.01".
    
&IF "{1}" = "0" &THEN /* --- SmartWindow --- */
    ASSIGN W-Win:TITLE    = W-Win:TITLE + c-versao.
&ELSEIF "{1}" = "1" &THEN /* --- SmartWindow --- */
    ASSIGN W-Win1:TITLE   = W-Win1:TITLE + c-versao.
&ELSEIF "{1}" = "2" &THEN /* --- SmartWindow --- */
    ASSIGN W-Win2:TITLE   = W-Win2:TITLE + c-versao.
&ELSEIF "{1}" = "3" &THEN /* --- SmartWindow --- */
    ASSIGN W-Win3:TITLE   = W-Win3:TITLE + c-versao.
&ELSEIF "{1}" = "4" &THEN /* --- SmartWindow --- */
    ASSIGN W-Win4:TITLE   = W-Win4:TITLE + c-versao.
&ELSEIF "{1}" = "5" &THEN /* --- SmartDialog --- */
    ASSIGN D-Dialog:TITLE = D-Dialog:TITLE + c-versao.
&ENDIF
