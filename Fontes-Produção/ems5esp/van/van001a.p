/****************************************************************************************** 
** 	   Programa: van001a.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 12/09/2018
** Change/Chamado: 
**      Objetivo: Retorna a lista de arquivos para Lista arquivos das pastas do EDI, compacta e retorna para envio para o WebService da Accesstage.
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: Id do Tipo de servi‡o que ser  consumido, nome do servi‡o que ser  consumido
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
    FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
    FIELD desc-erro AS CHAR    FORMAT "x(70)".

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(100)"
    FIELD lSearch   AS LOGICAL.

DEFINE INPUT  PARAMETER ipc-dirlocaliz AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt-listFiles.
DEFINE OUTPUT PARAMETER l-ok           AS LOGICAL     NO-UNDO INITIAL YES.

DEFINE VARIABLE h-zip          AS HANDLE NO-UNDO.
DEFINE VARIABLE c-arquivo-zip  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE c-dir_arq      AS CHARACTER   NO-UNDO FORMAT "X(250)".
DEFINE VARIABLE encdmptr       AS MEMPTR      NO-UNDO.
DEFINE VARIABLE encdlngc       AS LONGCHAR    NO-UNDO.
                                 
FILE-INFO:FILE-NAME = ipc-dirlocaliz.
    
/* IF NOT FILE-INFO:FILE-TYPE = "DRW" THEN DO: */
/*                                             */
/*     ASSIGN l-ok = NO.                       */
/*     RETURN.                                 */
/* END.                                        */


INPUT FROM OS-DIR(ipc-dirlocaliz).

REPEAT: 
    IMPORT c-dir_arq.

    FILE-INFO:FILE-NAME = ipc-dirlocaliz + "\" + c-dir_arq.

    IF SEARCH(FILE-INFO:FILE-NAME) = ? THEN NEXT.

    /* Despresa os diretor¢rios */
    IF FILE-INFO:FILE-TYPE = "DRW" THEN NEXT.
    
    CREATE tt-listFiles.
    ASSIGN tt-listFiles.cFile = FILE-INFO:FILE-NAME.

END. 

INPUT CLOSE.

ASSIGN l-ok = YES.
