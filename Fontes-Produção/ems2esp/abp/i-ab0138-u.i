/***************************************************************************
**     i-ab0138-u.i 
***************************************************************************/

{include/i-lgcode.i}

&IF "{&LANGUAGE-CODE}" = "POR" &THEN
&glob val1 Padr∆o
&glob val2 Transferància
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ESP" &THEN
&glob val1 Est†ndar
&glob val2 Transferencia
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ING" &THEN
&glob val1 DEFAULT Group
&glob val2 Transference Group
&ENDIF

{include/ind01-10.i {1} {2}}

/* Fim */
