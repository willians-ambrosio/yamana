/******************************************************************************************************************************************
** Programa: esp/esdeps4002rp.p
** Data    : 06-11-2015
** Autor   : Mauricio Cerqueira Miranda
** Objetivo: Relatorio de Representante DEPS
********************************************************************************************************************************************/
ASSIGN CURRENT-LANGUAGE = CURRENT-LANGUAGE.
/* include de controle de vers’o */
{include/i-prgvrs.i deletaitem "TOTVS"}
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
DEFINE TEMP-TABLE tt-deleta-item
    FIELD it-codigo    LIKE ITEM.it-codigo.

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
          CREATE tt-deleta-item.
          IMPORT DELIMITER ';' tt-deleta-item.
      END.

IF tt-param.l-exec THEN
    RUN pi-deleta.

RUN pi-finalizar IN h-acomp.

IF CAN-FIND(FIRST tt-erro) THEN DO:
   OUTPUT TO value(SESSION:TEMP-DIRECTORY + "erro_deleta_item.csv").
   FOR EACH tt-erro:
       EXPORT DELIMITER ';' tt-erro.
   END.
   OUTPUT CLOSE.
   MESSAGE "Foi gerado um arquivo de erros em: " + SESSION:TEMP-DIRECTORY + "erro_deleta_item.csv"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.




PROCEDURE pi-deleta:

    FOR EACH tt-deleta-item NO-LOCK
         WHERE tt-deleta-item.it-codigo <> "":
     

          FIND FIRST b-ITEM NO-LOCK
               WHERE b-ITEM.it-codigo = tt-deleta-item.it-codigo NO-ERROR.


          run pi-acompanhar in h-acomp(input "ITEM: " + string(tt-deleta-item.it-codigo)).

          
              IF AVAIL(b-item) THEN DO:
                 
                  CREATE tt-elimina.
                  ASSIGN tt-elimina.it-codigo  = b-item.it-codigo
                         tt-elimina.un         = b-item.un
                         tt-elimina.descricao  = b-item.Desc-item.
                  
              END.
              ELSE DO:
                  CREATE tt-erro.
                  ASSIGN tt-erro.it-codigo = tt-deleta-item.it-codigo
                         tt-erro.desc-erro = "Item inexistente " + tt-deleta-item.it-codigo.
              END.
          

      END.
       IF CAN-FIND(FIRST tt-elimina) THEN
          RUN pi-cd0205rp.
    

END PROCEDURE.

PROCEDURE pi-cd0205rp:


    DEFINE VARIABLE raw-paramCD0205 AS RAW      NO-UNDO.
    DEF VAR prog-elimina AS HANDLE NO-UNDO.

    EMPTY TEMP-TABLE tt-paramCD0205.
    EMPTY TEMP-TABLE tt-digitaCD0205.
                                    
    create tt-paramCD0205.
    assign tt-paramCD0205.usuario    = c-seg-usuario
           tt-paramCD0205.destino    = 2 /* Arquivo*/
           tt-paramCD0205.data-exec  = TODAY 
           tt-paramCD0205.hora-exec  = TIME 
           tt-paramCD0205.l-todos    = NO 
           /*tt-paramCD0205.arquivo    = tt-paramCD0205.arquivo + "/CD0205" + STRING(TIME) + ".carga".*/
           tt-paramCD0205.arquivo    = SESSION:TEMP-DIRECTORY + "/CD0205" + STRING(TIME) + ".carga" .


    for each tt-elimina:
        create tt-digitaCD0205.
        assign tt-digitaCD0205.it-codigo     = tt-elimina.it-codigo   
               tt-digitaCD0205.un            = tt-elimina.un          
               tt-digitaCD0205.descricao     = tt-elimina.descricao
               tt-digitaCD0205.tipo-operacao = "E".
    end.

    /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

    raw-transfer tt-paramCD0205    to raw-paramCD0205.

    for each tt-raw-digitaCD0205:
        delete tt-raw-digitaCD0205.
    end.
    for each tt-digitaCD0205:
        create tt-raw-digitaCD0205.
        raw-transfer tt-digitaCD0205 to tt-raw-digitaCD0205.raw-digita.
    end.  
    
    run cdp/cd0205rp.p (input raw-paramCD0205,
                        input table tt-raw-digitaCD0205).

END PROCEDURE.
