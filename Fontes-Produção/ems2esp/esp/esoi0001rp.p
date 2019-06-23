&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{include/i-prgvrs.i ESOI0001RP 12.01.19.000}
/** Temps Padr∆o*/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    /* -------------------------------- */
    FIELD numero-ordem     AS INTEGER.

def NEW shared temp-table t-invest
    field ep-codigo      like ordem-compra.ep-codigo
    field numero-ordem   like ordem-compra.numero-ordem
    field num-ord-magnus like ordem-compra.num-ord-inv 
    field cod-estabel    like ordem-compra.cod-estabel
    field parcela        like prazo-compra.parcela
    field cod-emitente   like ordem-compra.cod-emitente
    field data-cotacao   like cotacao-item.data-cotacao
    field ord-par        as char format "x(01)"
    field tipo           as char format "x(01)".

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEF TEMP-TABLE tt-it-sem-ord /*WA MMPS  29/10/04*/
    FIELD tt-item LIKE ITEM.it-codigo.

DEFINE INPUT  PARAMETER raw-param AS RAW  NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

DEF VAR h-acomp          AS HANDLE NO-UNDO .

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}
/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
ASSIGN c-programa         = "ESOI0001RP"
       c-versao           = "12.01"
       c-revisao          = ".20.000"
       c-empresa          = "YAMANA"
       c-sistema          = "INP"
       c-titulo-relat     = "ATUALIZA ORDEM INVESTIMENTO".

EMPTY TEMP-TABLE t-invest.

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN  utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Inicializando, Aguarde...").

RUN pi-processar.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-imprimir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprimir Procedure 
PROCEDURE pi-imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processar Procedure 
PROCEDURE pi-processar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST ordem-compra WHERE
           ordem-compra.numero-ordem = tt-param.numero-ordem NO-LOCK NO-ERROR.
if avail ordem-compra then 
do:

   RUN  pi-acompanhar IN  h-acomp (INPUT "ORDEM: " + STRING(ordem-compra.numero-ordem)).
   create t-invest.
   assign t-invest.ep-codigo      = ordem-compra.ep-codigo
          t-invest.numero-ordem   = ordem-compra.numero-ordem
          t-invest.num-ord-magnus = ordem-compra.num-ord-inv
          t-invest.cod-estabel    = ordem-compra.cod-estabel
          t-invest.tipo           = "I"
          t-invest.ord-par        = "O".

    run inp/in2110b.p. /* descarrega tabela */


    PUT UNFORMATTED "ORDEM DE COMPRA: " + STRING(ordem-compra.numero-ordem) + " ATUALIZADA NO M‡DULO DE INVESTIMENTO." SKIP.

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

