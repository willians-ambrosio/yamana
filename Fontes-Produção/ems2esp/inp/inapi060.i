/*******************************************************************************
**
**   INAPI060.i: Prepara chamada da INAPI060.p
**               Valida‡Æo do movimento X conta cont bil informada
**
*******************************************************************************/

&IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
    
    IF  NOT AVAIL param-global THEN
        FIND FIRST param-global NO-LOCK NO-ERROR.
    
    IF  param-global.modulo-in AND {2} <> ? THEN DO: /* ct-codigo */
        RUN inp/inapi060.p(INPUT  {1},               /* c-esta-exec */
                           INPUT  {2},               /* ct-codigo */
                           INPUT  {3},               /* sc-codigo */
                           INPUT  {4},               /* i-num-ordem */
                           INPUT  {5},               /* l-log-valida */
                           OUTPUT TABLE {6}).        /* tt-erro */
    END.
&ENDIF
