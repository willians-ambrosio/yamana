&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i esau001rp 2.06.00.000} 
{utp/ut-glob.i} 

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD data-ini         as date
    FIELD data-fim         as date
    FIELD it-codigo-ini    LIKE ITEM.it-codigo
    FIELD it-codigo-fim    LIKE ITEM.it-codigo
    FIELD fm-codigo-ini    LIKE ITEM.fm-codigo
    FIELD fm-codigo-fim    LIKE ITEM.fm-codigo.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE temp-table tt-raw-digita
    field raw-digita as raw.

DEF INPUT PARAM raw-param as raw no-undo.
DEF INPUT PARAM table for tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param to tt-param.

/* Ativar somente quando tiver (tt-digita)      */
/* FOR EACH tt-raw-digita:                      */
/*     create tt-digita.                        */
/*     RAW-TRANSFER tt-raw-digita to tt-digita. */
/* END.                                         */
/*                                              */


/*
DEF TEMP-TABLE tt-montagem NO-UNDO
    FIELD it-codigo LIKE ITEM.it-codigo
    FIELD desc-item LIKE ITEM.desc-item 
    INDEX codigo
          it-codigo.
*/
{include/i-rpvar.i} 

DEF VAR h_acomp_rp     as   handle                  no-undo.

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

find first empresa no-lock no-error.

ASSIGN c-programa     = "esau001rp"
       c-versao       = "2.06"
       c-revisao      = "00.000"
       c-empresa      =  empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Auditoria de Fornecedor".
/*    
form header
    "Codigo           Descricao"            AT   1
    FILL("-",132) FORMAT "x(132)"
    with stream-io width 132 no-labels no-box page-top frame f-cabec-1.
*/
                   
{include\i-rpout.i &PAGESIZE=0}
{include\i-rpcab.i}

RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.
RUN pi-inicializar IN h_acomp_rp (INPUT "Lendo os Movimentos").

/* Se tiver uma montagem de temp-table */
/* RUN pi-monta. */
RUN pi-imprime.

run pi-finalizar in h_acomp_rp.

{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime Procedure 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* 
VIEW FRAME f-cabec.
VIEW FRAME f-cabec-1.
VIEW FRAME f-rodape.
*/

PUT "Data;Hora;Usuario;Nome;Programa;Item;Descricao Item;Campo;Familia;Valor Anterior;Valor Atual" SKIP.

FOR EACH ext-audit-item-fam USE-INDEX ch-data NO-LOCK 
   WHERE /*ext-audit-item-fam.campo      = "res-for-comp"
     AND*/ ext-audit-item-fam.data      >= tt-param.data-ini
     AND ext-audit-item-fam.data      <= tt-param.data-fim
     AND ext-audit-item-fam.it-codigo >= tt-param.it-codigo-ini 
     AND ext-audit-item-fam.it-codigo <= tt-param.it-codigo-fim
     AND ext-audit-item-fam.fm-codigo >= tt-param.fm-codigo-ini 
     AND ext-audit-item-fam.fm-codigo <= tt-param.fm-codigo-fim:

    FIND item WHERE 
         item.it-codigo = ext-audit-item-fam.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT.

    RUN pi-acompanhar IN h_acomp_rp (INPUT "Item: " + string(ext-audit-item-fam.it-codigo)).

    FIND FIRST usuar_mestre NO-LOCK WHERE
               usuar_mestre.cod_usuario = ext-audit-item-fam.usuario NO-ERROR.

    PUT  ext-audit-item-fam.data   ";"
         STRING(ext-audit-item-fam.hora,"hh:mm:ss")   ";"
         ext-audit-item-fam.usuario    ";"
         IF AVAIL usuar_mestre THEN usuar_mestre.nom_usuario ELSE ''    ';'
         ext-audit-item-fam.programa   ";"
         ext-audit-item-fam.it-codigo  ";"
         IF item.it-codigo <> "" THEN ITEM.desc-item  ELSE ""              ";"
         ext-audit-item-fam.campo      ";"
         ext-audit-item-fam.fm-codigo  ";"
         ext-audit-item-fam.valor-ant  ";"
         ext-audit-item-fam.valor-atu  SKIP.

END. /* FOR EACH ext-audit-item-fam */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-monta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta Procedure 
PROCEDURE pi-monta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Montagem da temp-table      
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

