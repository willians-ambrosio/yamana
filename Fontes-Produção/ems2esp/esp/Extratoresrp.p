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
def buffer empresa     for ems2cadme.empresa.

/* ***************************  Definitions  ************************** */

{include/i-prgvrs.i extrat-connectRP 2.06.00.000} 
/* Temporary Table Definitions ---                                      */

/**

l-contaContabil  
l-cotacao        
l-emitente       
l-empresa        
l-estab          

**/

define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field desc-classifica       as char format "x(40)"
    FIELD tipo-periodo          AS INTEGER
    FIELD dias-extrat           AS INTEGER
    FIELD data-inicial          AS DATE
    FIELD data-final            AS DATE
    FIELD dir-extrat            AS CHAR
    FIELD l-centroCusto         AS LOGICAL 
    FIELD l-banco                AS LOGICAL 
    FIELD l-conta               AS LOGICAL
/*     FIELD l-contaContabil       AS LOGICAL */
    FIELD l-contrato            AS LOGICAL
/*     FIELD l-cotacao             AS LOGICAL */
    FIELD l-departamento        AS LOGICAL
    FIELD l-departamentoItem    AS LOGICAL
    FIELD l-deposito            AS LOGICAL
    FIELD l-sub-div-ordem       AS LOGICAL
    FIELD l-empresa             AS LOGICAL
/*     FIELD l-estab               AS LOGICAL */
    FIELD l-familiaItem         AS LOGICAL
    FIELD l-importacao          AS LOGICAL
    FIELD l-impostosimportacao  AS LOGICAL
    FIELD l-item                AS LOGICAL
    FIELD l-itemDeposito        AS LOGICAL
    FIELD l-moeda               AS LOGICAL
    FIELD l-movtoEstoq          AS LOGICAL
    FIELD l-naturOper           AS LOGICAL
    FIELD l-ordemCompra         AS LOGICAL
    FIELD l-ordem-import        AS LOGICAL
    FIELD l-ordemInvest         AS LOGICAL
    FIELD l-pedidoCompra        AS LOGICAL
    FIELD l-prazoCompra         AS LOGICAL
    FIELD l-recebimento         AS LOGICAL
    FIELD l-Ccustos             AS LOGICAL
    FIELD l-tipoTransacao       AS LOGICAL
    FIELD l-unidade             AS LOGICAL
    FIELD l-usuarioMaterial     AS LOGICAL
    FIELD l-sl-it-per           AS LOGICAL.

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
         HEIGHT             = 5.54
         WIDTH              = 32.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find first empresa no-lock no-error.

ASSIGN c-programa     = "ExtratoresRP"
       c-versao       = "2.06"
       c-revisao      = "00.000"
       c-empresa      =  empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Extrator Connect".    

{include\i-rpout.i}
{include\i-rpcab.i}

RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.
RUN pi-inicializar IN h_acomp_rp (INPUT "Inicializando").




/* acertando o periodo */


IF  tt-param.tipo-periodo = 1 THEN

ASSIGN tt-param.data-inicial = TODAY - tt-param.dias-extrat
       tt-param.data-final   = TODAY.

RUN pi-execute.

run pi-finalizar in h_acomp_rp.

{include\i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-execute) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-execute Procedure 
PROCEDURE pi-execute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO  ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:
    
        IF  tt-param.l-centroCusto THEN
            RUN esp\CentroCusto.p NO-ERROR.    
    
        IF  tt-param.l-conta THEN
            RUN esp\Contas.p NO-ERROR.          
        
        IF  tt-param.l-banco THEN
            RUN esp\banco.p NO-ERROR.          
        /**
        IF  tt-param.l-contaContabil THEN
            RUN esp\ContaContabil.p NO-ERROR.           
        **/
        IF  tt-param.l-contrato THEN
            RUN esp\Contrato.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
        /**
        IF  tt-param.l-cotacao THEN
            RUN esp\Cotacao.p NO-ERROR.
        **/
        IF  tt-param.l-departamento THEN
            RUN esp\Departamento.p NO-ERROR.
    
        IF  tt-param.l-departamentoItem THEN
            RUN esp\DepartamentoItem.p NO-ERROR.
    
        IF  tt-param.l-deposito THEN
            RUN esp\Deposito.p NO-ERROR.
        
        IF  tt-param.l-sub-div-ordem THEN
            RUN esp\subdivordem.p NO-ERROR.
        
        IF  tt-param.l-empresa THEN
            RUN esp\Empresa.p NO-ERROR.
        /*
        IF  tt-param.l-estab THEN
            RUN esp\Estabelecimento.p NO-ERROR.
        **/
            
        IF  tt-param.l-familiaItem THEN
            RUN esp\FamiliaItem.p NO-ERROR.
    
        IF  tt-param.l-importacao THEN
            RUN esp\Importacao.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-impostosImportacao THEN
            RUN esp\impostoImportacao.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-item THEN
            RUN esp\ITEM.p NO-ERROR.           
    
        IF  tt-param.l-itemDeposito THEN
            RUN esp\itemDeposito.p NO-ERROR.           
    
        IF  tt-param.l-moeda THEN
            RUN esp\Moeda.p NO-ERROR.          
    
        IF  tt-param.l-movtoEstoq THEN
            RUN esp\MovtoEstoq.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-naturOper THEN
            RUN esp\NaturezaOper.p NO-ERROR.      
    
        IF  tt-param.l-ordemCompra THEN
            RUN esp\OrdemCompra.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.    
    
        IF  tt-param.l-ordem-import THEN
            RUN esp\OrdemEmbarque.p NO-ERROR.    
    
        IF  tt-param.l-ordemInvest THEN
            RUN esp\OrdemInvest.p NO-ERROR.    
    
        IF  tt-param.l-pedidoCompra THEN
            RUN esp\PedidoCompra.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-prazoCompra THEN
            RUN esp\PrazoCompra.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-recebimento THEN
            RUN esp\Recebimento.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
        IF  tt-param.l-Ccustos THEN
            RUN esp\Ccustos.p NO-ERROR.
    
        IF  tt-param.l-tipoTransacao THEN
            RUN esp\TipoTransacao.p NO-ERROR.
    
        IF  tt-param.l-unidade THEN
            RUN esp\Unidade.p NO-ERROR.
    
        IF  tt-param.l-usuarioMaterial THEN
            RUN esp\UsuarioMaterial.p NO-ERROR.
    
        IF  tt-param.l-sl-it-per THEN
            RUN esp\sl-it-per.p (INPUT tt-param.data-inicial, INPUT tt-param.data-final, INPUT h_acomp_rp) NO-ERROR.
    
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

