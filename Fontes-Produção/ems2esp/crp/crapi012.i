/*********************************************************************************
**
**  CRAPI010.I
**
**  Defini‡Æo das temp-tableïs de parƒmetros e erros da consulta de portadores.
**
**********************************************************************************/

def temp-table tt-mov-tit no-undo
   field gr-mov-tit         as rowid
   field cod-versao-integ   as integer
   field l-vid-rel          as logical.
   
def temp-table tt-conta-contabil        
    field tt-cod-conta-contabil     like conta-contab.conta-contabil
    field tt-titulo-conta           like conta-contab.titulo
    field tt-natureza               as character format "XX"
    field tt-valor                  like titulo.vl-original.
                 
def temp-table tt-erro no-undo
    field i-cod-erro        as integer format "9999999"
    field c-desc-erro       as char format "x(70)"
    field c-arquivo-erro    as char format "x(100)".
