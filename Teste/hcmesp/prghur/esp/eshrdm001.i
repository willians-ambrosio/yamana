/*-----------------------------------------------------------------
Programa: eshrdm001.i
Objetivo: Definicao da tt-param
Autor:    Joao B. C. Bisneto
-------------------------------------------------------------------*/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG

    FIELD ini-empresa      AS CHAR 
    FIELD fim-empresa      AS CHAR
    FIELD ini-cdn-estab    AS CHAR
    FIELD fim-cdn-estab    AS CHAR
    FIELD ini-cdn-func     AS INT
    FIELD fim-cdn-func     AS INT
    FIELD ini-usuar-alter  AS CHAR
    FIELD fim-usuar-alter  AS CHAR
                                      
    FIELD l-cta-duplic    AS LOG
    FIELD l-sem-avaliac   AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEF TEMP-TABLE tt-func-desligto NO-UNDO LIKE func_desligto
  FIELD l-considera AS LOG.
