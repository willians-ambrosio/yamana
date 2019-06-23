/***************************************************************************
**     i2-ab0138-u.i 
***************************************************************************/

{include/i-lgcode.i}

&IF "{&LANGUAGE-CODE}" = "POR" &THEN
&glob val1 Simples
&glob val2 Autom tico
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ESP" &THEN
&glob val1 SIMPLE
&glob val2 Autom tico
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ING" &THEN
&glob val1 SIMPLE Group
&glob val2 AUTOMATIC Group
&ENDIF

{include/ind01-10.i {1} {2}}

/* Fim */
