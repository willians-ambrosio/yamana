/******************************************************************************************************************************************
** Programa: esp/esdeps4002rp.p
** Data    : 06-11-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo:
********************************************************************************************************************************************/
ASSIGN CURRENT-LANGUAGE = CURRENT-LANGUAGE.
/* include de controle de vers’o */
{include/i-prgvrs.i alteralocaliz "TOTVS"}
    {utp/ut-glob.i}

/* ---------------------CONFIGURA EXCEL-------------------------- */
DEFINE VARIABLE chexcelApplication        AS COM-HANDLE.
DEFINE VARIABLE chWorkbook                AS COM-HANDLE.
DEFINE VARIABLE chWorksheet               AS COM-HANDLE.
DEFINE VARIABLE chChart                   AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange          AS COM-HANDLE.
DEFINE VARIABLE chWorkSheetRageSetup      AS COM-HANDLE.
DEFINE VARIABLE iCount                    AS INTEGER.
DEFINE VARIABLE cRange                    AS CHARACTER.
DEFINE VARIABLE i-linha                   AS INTEGER INITIAL 4.
/* -------------------------------------------------------------- */

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    FIELD arquivo          AS CHAR
    field hora-exec        as integer
    FIELD l-exec           AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de par³metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*Carregando a tt-digita*/
create tt-param.
raw-transfer raw-param to tt-param.


/* carregando tt-digita */
For Each tt-raw-digita:
    Create tt-digita.
    Raw-transfer tt-raw-digita.raw-digita To tt-digita.
End.     

define variable h-acomp as handle no-undo.
DEFINE VARIABLE c-situacao AS CHARACTER   NO-UNDO.


DEFINE BUFFER b-ITEM FOR ITEM.

DEFINE TEMP-TABLE tt-altera-localiz
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD cod-localiz  LIKE ITEM.cod-localiz.

 define temp-table tt-paramCD0205 no-undo
     field destino          as integer
     field arquivo          as char
     field usuario          as char
     field data-exec        as date
     field hora-exec        as integer
     field classifica       as integer
     field l-todos          as logical.

 define temp-table tt-elimina no-undo
     field it-codigo     like item.it-codigo
     field un            like item.un
     field descricao     as char FORMAT "x(60)".

 define temp-table tt-digitaCD0205 no-undo
     field it-codigo     like item.it-codigo
     field descricao     as char FORMAT "x(60)"
     field un            like item.un
     field new-it-codigo like item.it-codigo
     field new-un        like item.un
     field fator-conv    as decimal FORMAT ">,>>9.99999"
     field ge-codigo     like item.ge-codigo
     field old-ge-codigo like item.ge-codigo
     field tipo-operacao as char FORMAT "x".

 def temp-table tt-raw-digitaCD0205
    field raw-digita      as raw.

 def temp-table tt-raw-eliminaCD0205
     field raw-elimina      as raw.

 DEFINE TEMP-TABLE tt-erro
     FIELD it-codigo    LIKE ITEM.it-codigo
     FIELD desc-erro    AS CHAR.



 run utp/ut-acomp.p persistent set h-acomp.
 run pi-inicializar in h-acomp (input "Deleta Itens").
    
 INPUT FROM VALUE(tt-param.arq-entrada).
      
      REPEAT:
          CREATE tt-altera-localiz.
          IMPORT DELIMITER ';' tt-altera-localiz.
      END.

IF tt-param.l-exec THEN
    RUN pi-altera-localiz.

RUN pi-finalizar IN h-acomp.



PROCEDURE pi-altera-localiz:

    DISABLE TRIGGERS FOR LOAD OF ITEM.
    DISABLE TRIGGERS FOR LOAD OF item-uni-estab.

    DEFINE BUFFER b-item-localiz           FOR ITEM.
    DEFINE BUFFER b-localiz-item-uni-estab FOR item-uni-estab.


        FOR EACH tt-altera-localiz:
    
            FOR FIRST b-item-localiz FIELDS(cod-localiz)
                WHERE b-item-localiz.it-codigo = tt-altera-localiz.it-codigo:
            END.
            IF AVAIL(b-item-localiz) THEN
                ASSIGN b-item-localiz.cod-localiz = tt-altera-localiz.cod-localiz.



            FOR EACH estabelec NO-LOCK
               WHERE estabelec.ep-codigo = v_cdn_empres_usuar:

                FOR EACH b-localiz-item-uni-estab FIELDS(cod-localiz)
                   WHERE b-localiz-item-uni-estab.it-codigo = tt-altera-localiz.it-codigo
                     AND b-localiz-item-uni-estab.cod-estabel = estabelec.cod-estab:
                    ASSIGN b-localiz-item-uni-estab.cod-localiz = tt-altera-localiz.cod-localiz.
                END.
            END.

        END.

        
        /* Alterar localiza‡Æo data 17/12/2015*/

END PROCEDURE.
