/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRI0404RP 2.00.00.025 } /*** 010025 ***/
 
&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ri0404rp MRI}
&ENDIF

{include/i_fnctrad.i}
/********************************************************************************
**  Programa: RI0404RP
**  Objetivo: Relatorio auxiliar do bem
**  Autor...: Datasul Logistica
**  Data....: Mar‡o/2005
**  Alterado: Novembro/2010
*******************************************************************************/

/* Parameters Definitions ---                                           */
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR    FORMAT "x(35)"
    FIELD usuario           AS CHAR    FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD tipo-sel          AS INT
&IF "{&mguni_version}" >= "2.071" &THEN
    FIELD cod-estabel-ini    AS CHAR    format "x(05)"
    FIELD cod-estabel-fim    AS CHAR    format "x(05)"
&ELSE
    FIELD cod-estabel-ini    AS CHAR    format "x(03)"
    FIELD cod-estabel-fim    AS CHAR    format "x(03)"
&ENDIF
    FIELD i-tip-relat       AS INTEGER /*1-Resumido 2-Detalhado*/
    FIELD cod-grupo-ini     AS INTEGER FORMAT ">>>>>9"
    FIELD bem-ini           LIKE ri-bem.id-bem
    FIELD bem-fim           LIKE ri-bem.id-bem
    FIELD serie-ini         AS CHAR    FORMAT "x(03)"
    FIELD serie-fim         AS CHAR    FORMAT "x(03)"
    FIELD nr-patrimonio-ini LIKE ri-bem.nr-patrimonio
    FIELD nr-patrimonio-fim LIKE ri-bem.nr-patrimonio
    FIELD nr-doc-fis-ini    AS CHAR    FORMAT "x(16)"
    FIELD nr-doc-fis-fim    AS CHAR    FORMAT "x(16)"
    FIELD nat-oper-ini      AS CHAR    FORMAT "x(06)"
    FIELD nat-oper-fim      AS CHAR    FORMAT "x(06)"
    FIELD cod-emitente-ini  AS INT     FORMAT ">>>>>>>>9"
    FIELD cod-emitente-fim  AS INT     FORMAT ">>>>>>>>9"
    FIELD nr-ord-produ-ini  AS INT     FORMAT ">>>,>>>,>>9"
    FIELD nr-ord-produ-fim  AS INT     FORMAT ">>>,>>>,>>9"
    FIELD dt-docto-ini      AS DATE    FORMAT "99/99/9999"
    FIELD dt-docto-fim      AS DATE    FORMAT "99/99/9999"
    FIELD dt-corte          AS DATE    FORMAT "99/99/9999"
    FIELD l-gera-planilha   AS LOG  INIT NO
    FIELD c-arq-planilha    AS CHAR INIT ''.

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

/*--- Defini‡Æo de Parƒmetros ---*/
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

/*--- Defini‡Æo de Vari veis Locais ---*/
DEFINE VARIABLE h-acomp       AS HANDLE                             NO-UNDO.
DEF VAR i-nr-parc-cred               AS   INTE                      NO-UNDO.
DEF VAR de-vl-tot-cred               LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-parcela                LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-tot-recuperado         LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-tot-nao-recuperado     LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-tot-baixa              LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-saldo-cred             LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-saldo-cp               LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR de-vl-saldo-lp               LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-cred             LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-parcela              LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-valor-atual-bem         LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-recuperado       LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-nao-recuperado   LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-baixa            LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-cred           LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-cp             LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-lp             LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-cred-2           LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-parcela-2            LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-recuperado-2     LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-nao-recuperado-2 LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-tot-baixa-2          LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-cred-2         LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-cp-2           LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR t-de-vl-saldo-lp-2           LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR i-qtd-meses-lg-prazo         AS INTEGER                     NO-UNDO.
DEF VAR i-qtd-meses-ct-prazo         AS INTEGER                     NO-UNDO.
DEF VAR l-ajusta-parcela             AS LOGICAL                     NO-UNDO.   
DEF VAR i-tamanho-linha              AS INTEGER                     NO-UNDO. 
DEF VAR c-valor-aux                  AS CHARACTER                   NO-UNDO.
DEF VAR de-valor-atual-bem           LIKE ri-valor-bem.val-imposto  NO-UNDO.
DEF VAR dat-inic-cred                LIKE ri-bem.dat-inic-cred      NO-UNDO. 
DEF VAR l-icms                       AS LOGICAL                     NO-UNDO. 
DEF VAR l-baixado-no-periodo         AS  LOGICAL                    NO-UNDO.

DEFINE VARIABLE de-valor-atual-bem-aux        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-parcela-aux             AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-cred-aux            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-recuperado-aux      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-nao-recuperado-aux  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-baixa-aux           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-cred-aux          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-cp-aux            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-lp-aux            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-val-contabil-aux           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-val-contabil               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE t-de-val-contabil             LIKE ri-valor-bem.val-imposto  NO-UNDO.

/*Documento*/
DEFINE VARIABLE de-val-contabil-doc           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-valor-atual-bem-doc        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-parcela-doc             AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-cred-doc            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-recuperado-doc      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-nao-recuperado-doc  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-tot-baixa-doc           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-cred-doc          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-cp-doc            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-saldo-lp-doc            AS DECIMAL     NO-UNDO.
/*Documento*/


DEF STREAM s-planilha.

/*--- Defini‡Æo de Forms ---*/

DEF FRAME f-bem
     ri-estab-grupos.cod-estabel     FORMAT "x(3)"           COLUMN-LABEL "Estab" 
     ri-bem.id-bem              FORMAT ">>>>>>>>9"      COLUMN-LABEL "Id Bem" 
     c-valor-aux                FORMAT "x(35)"          COLUMN-LABEL "Descri‡Æo"
     ri-bem.nr-nota-fis         FORMAT "x(07)"          COLUMN-LABEL "Nr Docto"
     ri-bem.nr-seq-docto        FORMAT ">>>9"           COLUMN-LABEL "/Seq"
     dat-inic-cred              FORMAT "99/99/99"       COLUMN-LABEL "Dt Inic!Credito" 
     i-nr-parc-cred             FORMAT ">9"             COLUMN-LABEL "Pa"
     de-valor-atual-bem         FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"        
     de-vl-parcela              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"        
     de-vl-tot-cred             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"        
     de-vl-tot-recuperado       FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"      
     de-vl-tot-nao-recuperado   FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"  
     de-vl-tot-baixa            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"         
     de-vl-saldo-cred           FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"     
     de-vl-saldo-cp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo" 
     de-vl-saldo-lp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo" 
     /* NÆo retirar serve para nÆo colocar label no final da impressÆo */
     de-val-contabil                 NO-LABEL
     ri-bem.id-bem                   NO-LABEL
     i-nr-parc-cred                  NO-LABEL

     WITH WIDTH 230 NO-BOX STREAM-IO DOWN.
                  
DEF FRAME f-docto
     ri-estab-grupos.cod-estabel FORMAT "x(3)"           COLUMN-LABEL "Estab" 
     c-valor-aux                 FORMAT "x(13)"          COLUMN-LABEL "Nr Docto"
     /*ri-bem.id-bem               FORMAT ">>>>>>>>9"      COLUMN-LABEL "Id Bem" */
     dat-inic-cred               FORMAT "99/99/99"       COLUMN-LABEL "Dt Inic!Credito" 
     /*i-nr-parc-cred              FORMAT ">9"             COLUMN-LABEL "Pa"*/
     de-val-contabil             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Valor Contabil"
     de-valor-atual-bem          FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"        
     de-vl-parcela               FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"        
     de-vl-tot-cred              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"        
     de-vl-tot-recuperado        FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"      
     de-vl-tot-nao-recuperado    FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"  
     de-vl-tot-baixa             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"         
     de-vl-saldo-cred            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"     
     de-vl-saldo-cp              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo" 
     de-vl-saldo-lp              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo" 
     /* NÆo retirar serve para nÆo colocar label no final da impressÆo */
     ri-bem.nr-nota-fis          NO-LABEL
     ri-bem.nr-seq-docto         NO-LABEL
     ri-bem.id-bem               NO-LABEL
     i-nr-parc-cred              NO-LABEL
     WITH WIDTH 230 NO-BOX STREAM-IO DOWN.

/*
DEF FRAME f-patrimonio
     ri-estab-grupos.cod-estabel FORMAT "x(3)"           COLUMN-LABEL "Estab"                    
     c-valor-aux                FORMAT "x(17)"          COLUMN-LABEL "Patrimonio / Seq"
     ri-bem.id-bem              FORMAT ">>>>>>>>9"      COLUMN-LABEL "Id Bem"
     dat-inic-cred              FORMAT "99/99/99"       COLUMN-LABEL "Dt Inic!Credito" 
     i-nr-parc-cred             FORMAT ">9"             COLUMN-LABEL "Pa"
     de-valor-atual-bem         FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"        
     de-vl-parcela              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"        
     de-vl-tot-cred             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"        
     de-vl-tot-recuperado       FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"      
     de-vl-tot-nao-recuperado   FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"  
     de-vl-tot-baixa            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"         
     de-vl-saldo-cred           FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"     
     de-vl-saldo-cp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo" 
     de-vl-saldo-lp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo" 
     /* NÆo retirar serve para nÆo colocar label no final da impressÆo */
     ri-bem.nr-nota-fis          NO-LABEL
     ri-bem.nr-seq-docto         NO-LABEL
     de-val-contabil             NO-LABEL
     WITH WIDTH 230 NO-BOX STREAM-IO DOWN.

DEF FRAME f-ord-prod
     ri-estab-grupos.cod-estabel FORMAT "x(3)"           COLUMN-LABEL "Estab."               
     c-valor-aux                FORMAT "x(07)"          COLUMN-LABEL "Ord Prod"
     dat-inic-cred              FORMAT "99/99/99"       COLUMN-LABEL "Dt Inic!Credito" 
     i-nr-parc-cred             FORMAT ">9"             COLUMN-LABEL "Pa"
     de-valor-atual-bem         FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"        
     de-vl-parcela              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"        
     de-vl-tot-cred             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"        
     de-vl-tot-recuperado       FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"      
     de-vl-tot-nao-recuperado   FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"  
     de-vl-tot-baixa            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"         
     de-vl-saldo-cred           FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"     
     de-vl-saldo-cp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo" 
     de-vl-saldo-lp             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo" 
     /* NÆo retirar serve para nÆo colocar label no final da impressÆo */
     ri-bem.nr-nota-fis          NO-LABEL
     ri-bem.nr-seq-docto         NO-LABEL
     ri-bem.id-bem               NO-LABEL
     de-val-contabil             NO-LABEL
     WITH WIDTH 230 NO-BOX STREAM-IO DOWN.                                                   
*/     
/*Fim forms detalhado*/

/*Forms Resumido*/
DEF FRAME f-bem-resum
     ri-estab-grupos.cod-estabel     FORMAT "x(3)"           COLUMN-LABEL "Estab"      
     ri-bem-grupo.id-bem             FORMAT ">>>>>>>>9"      COLUMN-LABEL "Id Bem" 
     i-nr-parc-cred                  FORMAT ">9"             COLUMN-LABEL "Pa"
     de-valor-atual-bem              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"        
     de-vl-parcela                   FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"        
     de-vl-tot-cred                  FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"        
     de-vl-tot-recuperado            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"      
     de-vl-tot-nao-recuperado        FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"  
     de-vl-tot-baixa                 FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"         
     de-vl-saldo-cred                FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"     
     de-vl-saldo-cp                  FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo" 
     de-vl-saldo-lp                  FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo" 
     /* NÆo retirar serve para nÆo colocar label no final da impressÆo */
     de-val-contabil                 NO-LABEL
     WITH WIDTH 230 NO-BOX STREAM-IO DOWN.
                                      
DEF FRAME f-docto-resum
     ri-estab-grupos.cod-estabel FORMAT "x(3)"           COLUMN-LABEL "Estab"                        
     de-val-contabil             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Cont bil"                          
     de-valor-atual-bem          FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Credito!Atual"                     
     de-vl-parcela               FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Val Parcela!Mensal"                    
     de-vl-tot-cred              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Tot Credito!Entrada"                   
     de-vl-tot-recuperado        FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Recuperado"             
     de-vl-tot-nao-recuperado    FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!NÆo Recup"              
     de-vl-tot-baixa             FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Total!Baixado"                
     de-vl-saldo-cred            FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo a!Apropriar"            
     de-vl-saldo-cp              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Curto Prazo"            
     de-vl-saldo-lp              FORMAT ">>,>>>,>>9.99"  COLUMN-LABEL "Saldo!Longo Prazo"            
     ri-bem-grupo.id-bem         NO-LABEL    
     ri-bem.nr-nota-fis          NO-LABEL
     ri-bem.nr-seq-docto         NO-LABEL
     ri-bem.id-bem               NO-LABEL
     i-nr-parc-cred              NO-LABEL
     WITH WIDTH 210 NO-BOX STREAM-IO DOWN.                                                           
/*Fim Forms Resumido*/                                                                               
                                                                                                       
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/*--- Includes Padräes ---*/
{include/i-rpvar.i}  /*Vari veis Padräes */

/***************************{include/i-rpcab.i}  /*Cabe‡alho e Rodap‚*/*/

/****************************************************************************
**
**  I-RPCAB.I - Form do Cabe‡alho PadrÆo e Rodap‚ (ex-CD9500.F)
**                              
** {&STREAM} - indica o nome da stream (opcional)
****************************************************************************/

&IF "{&LANGUAGE-CODE}":U = "ING":U &THEN 
    &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "Page:":U at 164 page-number  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "Page:":U at 164 page-number  at 169 format ">>>>9" skip
            "Period:":U i-numper-x at 09 "-"
            da-iniper-x at 14 "to" da-fimper-x
            fill("-", 74) format "x(72)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ELSE
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "Page:":U at 164 page-number({&STREAM})  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "Page:":U at 164 page-number({&STREAM})  at 169 format ">>>>9" skip
            "Period:":U i-numper-x at 10 "-"
            da-iniper-x at 14 "to":U da-fimper-x
            fill("-", 74) format "x(72)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ENDIF
&ELSEIF "{&LANGUAGE-CODE}" = "ESP":U &THEN
    &IF  "{&STREAM}" = "" &THEN
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 162 page-number  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 162 page-number  at 169 format ">>>>9" skip
            "Periodo:":U i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta" da-fimper-x
            fill("-", 70) format "x(68)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ELSE
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 162 page-number({&STREAM})  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 162 page-number({&STREAM})  at 169 format ">>>>9" skip
            "Periodo:":U i-numper-x at 10 "-"
            da-iniper-x at 15 "hasta":U da-fimper-x
            fill("-", 70) format "x(68)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ENDIF
&ELSE
    &IF "{&STREAM}" = "" &THEN
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 161 page-number  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 161 page-number  at 169 format ">>>>9" skip
            "Periodo:":U i-numper-x at 10 "-"
            da-iniper-x at 15 "a":U da-fimper-x
            fill("-", 74) format "x(72)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ELSE
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 161 page-number({&STREAM})  at 169 format ">>>>9" skip
            fill("-", 151) format "x(151)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabec.
        
        form header
            fill("-", 173) format "x(173)" skip
            c-empresa c-titulo-relat at 50
            "P gina:":U at 161 page-number({&STREAM})  at 169 format ">>>>9" skip
            "Periodo:":U i-numper-x at 10 "-"
            da-iniper-x at 15 "a":U da-fimper-x
            fill("-", 74) format "x(72)" today format "99/99/9999"
            "-" string(time, "HH:MM:SS":U) skip(1)
            with stream-io width 173 no-labels no-box page-top frame f-cabper.
    &ENDIF
&ENDIF

c-rodape = "DATASUL - ":U + c-sistema + " - " + c-prg-obj + " - V:":U + c-prg-vrs.
c-rodape = fill("-", 173 - length(c-rodape)) + c-rodape.

form header
    c-rodape format "x(173)"
    with stream-io width 173 no-labels no-box page-bottom frame f-rodape.

/* I-RPCAB.I */
/***********************************************************************/


FIND FIRST param-global NO-LOCK NO-ERROR.

FIND mguni.empresa NO-LOCK WHERE
     mguni.empresa.ep-codigo = param-global.empresa-prin NO-ERROR.
FIND FIRST ri-estabelecimento WHERE ri-estabelecimento.cod-estabel >= tt-param.cod-estabel-ini
                              AND   ri-estabelecimento.cod-estabel <= tt-param.cod-estabel-fim NO-LOCK NO-ERROR.
FIND FIRST ri-param           WHERE ri-param.cod-estabel           >= tt-param.cod-estabel-ini 
                              AND   ri-param.cod-estabel           <= tt-param.cod-estabel-fim NO-LOCK NO-ERROR.

    case tt-param.tipo-sel:
        when 1 then assign c-valor-aux = "Por Bem - ".
        when 2 then assign c-valor-aux = "Por Docum. - ".
        when 3 then assign c-valor-aux = "Por Patrim - ".
        when 4 then assign c-valor-aux = "Por Ordem Pr - ".
    end case.        
    CASE tt-param.i-tip-relat:
        WHEN 1 THEN assign c-valor-aux = c-valor-aux + "Resumido - ".
        WHEN 2 THEN assign c-valor-aux = c-valor-aux + "Detalhado - ".
    END CASE.

ASSIGN c-programa     = "RI0404"
       c-versao       = "2.00"
       c-revisao      = "000"
       c-titulo-relat = "MRI (" 
                      + trim(c-valor-aux)
                      /*
                      + "Estab: " 
                      + tt-param.cod-estabel-ini + " a " + tt-param.cod-estabel-fim*/
                      + " Dt Corte: " + string(tt-param.dt-corte,"99/99/99") 
                      + ")"
       c-sistema      = "Espec¡fico MRI"
       c-empresa      = IF AVAIL empresa THEN empresa.razao-social ELSE "".

{include/i-rpout.i}

IF tt-param.l-gera-planilha THEN DO:
    
    OUTPUT STREAM s-planilha  TO VALUE(tt-param.c-arq-planilha).

    FOR FIRST ri-grupos NO-lOCK
        WHERE ri-grupos.cod-grupo = cod-grupo-ini:    
        PUT STREAM s-planilha UNFORMATTED  
            ri-grupos.cod-grupo "-" ri-grupos.desc-grupo SKIP.          
    END. 

    PUT STREAM s-planilha UNFORMATTED
        "Estab;".

    IF tt-param.i-tip-relat = 1 /*Resumido*/ 
        AND (tt-param.tipo-sel = 1 /*bem*/ OR tt-param.tipo-sel = 2 /*Docto*/ ) THEN DO:

        IF tt-param.tipo-sel = 1 THEN

            PUT STREAM s-planilha UNFORMATTED            
                "Val Credito Atual;"
                "Val Parcela Mensal;"
                "Tot Credito Entrada;"
                "Total Recuperado;"
                "Total Nao Recup;"
                "Total Baixado;"
                "Saldo a Apropriar;"
                "Saldo Curto Prazo;"   
                "Saldo Longo Prazo". 

        ELSE IF tt-param.tipo-sel = 2 THEN
            PUT STREAM s-planilha UNFORMATTED            
                "Val Contabil;"
                "Val Credito Atual;"
                "Val Parcela Mensal;"
                "Tot Credito Entrada;"
                "Total Recuperado;"
                "Total Nao Recup;"
                "Total Baixado;"
                "Saldo a Apropriar;"
                "Saldo Curto Prazo;"   
                "Saldo Longo Prazo".
    END.
    ELSE DO:    
        CASE tt-param.tipo-sel:

        WHEN 1 /*Bem*/ THEN
            PUT STREAM s-planilha UNFORMATTED
                "Id Bem;"              
                "Descri‡ao;"
                "Nr Docto/Seq;"
                "Data Inic Cred;".
        WHEN 2 /*Documento*/ THEN 
            PUT STREAM s-planilha UNFORMATTED
                "Nr Docto;"
                /*"Id Bem;"*/
                "Data Inic Cred;"
                /*"Pa;"*/
                "Val Cont bil;"
                "Val Credito Atual;"
                "Val Parcela Mensal;"
                "Tot Credito Entrada;"
                "Total Recuperado;"
                "Total Nao Recup;"
                "Total Baixado;"
                "Saldo a Apropriar;"
                "Saldo Curto Prazo;"   
                "Saldo Longo Prazo".
        WHEN 3 /*Patrim“nio*/ THEN
            PUT STREAM s-planilha UNFORMATTED
                "Patrimonio / Seq;"
                "Id Bem;"
                "Data Inic Cred;".
        WHEN 4 /*Ordem Produ‡Æo*/ THEN
            PUT STREAM s-planilha UNFORMATTED
                "Ord Prod;"
                "Id Bem;"
                "Data Inic Cred;".
        END CASE.

        IF tt-param.tipo-sel <> 2 THEN
            PUT STREAM s-planilha UNFORMATTED
                "Pa;"
                "Val Credito Atual;"
                "Val Parcela Mensal;"
                "Tot Credito Entrada;"
                "Total Recuperado;"
                "Total Nao Recup;"
                "Total Baixado;"
                "Saldo a Apropriar;"
                "Saldo Curto Prazo;"   
                "Saldo Longo Prazo".   
    END.
    PUT STREAM s-planilha SKIP.
END.


VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Imprimindo...").

/**/
ASSIGN t-de-vl-tot-cred            = 0
       t-de-vl-parcela             = 0
       t-de-vl-tot-recuperado      = 0
       t-de-vl-tot-nao-recuperado  = 0
       t-de-vl-tot-baixa           = 0
       t-de-vl-saldo-cred          = 0
       t-de-vl-saldo-cp            = 0
       t-de-vl-saldo-lp            = 0.
    
    FOR FIRST ri-grupos NO-lOCK
        WHERE ri-grupos.cod-grupo = cod-grupo-ini:    
        PUT UNFORMATTED ri-grupos.cod-grupo "-" ri-grupos.desc-grupo.          
    END.

    CASE tt-param.tipo-sel:

    WHEN 1 /*Bem*/ THEN

        FOR EACH ri-estab-grupos NO-LOCK
            WHERE ri-estab-grupos.cod-estabel >= tt-param.cod-estabel-ini
            AND   ri-estab-grupos.cod-estabel <= tt-param.cod-estabel-fim
            AND   ri-estab-grupos.cod-grupo   = tt-param.cod-grupo-ini,
            
            EACH ri-bem NO-LOCK
            WHERE ri-bem.cod-estabel   = ri-estab-grupos.cod-estabel
            AND   ri-bem.id-bem        >= tt-param.bem-ini
            AND   ri-bem.id-bem        <= tt-param.bem-fim,

            EACH ri-bem-grupo NO-LOCK
            WHERE ri-bem-grupo.id-bem    = ri-bem.id-bem
            AND   ri-bem-grupo.cod-grupo = ri-estab-grupos.cod-grupo
            AND   ri-bem-grupo.data-1 <> ?
            AND   ri-bem-grupo.data-1 <= tt-param.dt-corte
            
            {esp/esri0404rp.i "ri-bem.id-bem"  "f-bem" "ri-estab-grupos.cod-estabel" "ri-bem-grupo.id-bem"} 
        END.

    WHEN 2 /*Documento*/ THEN 

        FOR EACH ri-estab-grupos NO-LOCK
            WHERE ri-estab-grupos.cod-estabel >= tt-param.cod-estabel-ini
            AND   ri-estab-grupos.cod-estabel <= tt-param.cod-estabel-fim
            AND   ri-estab-grupos.cod-grupo   = tt-param.cod-grupo-ini,
        
            EACH ri-bem NO-LOCK
            WHERE ri-bem.serie         >= tt-param.serie-ini        
            AND   ri-bem.serie         <= tt-param.serie-fim        
            AND   ri-bem.cod-estabel    = ri-estab-grupos.cod-estabel
            AND   ri-bem.nr-nota-fis   >= tt-param.nr-doc-fis-ini   
            AND   ri-bem.nr-nota-fis   <= tt-param.nr-doc-fis-fim   
            AND   ri-bem.cod-emitente  >= tt-param.cod-emitente-ini 
            AND   ri-bem.cod-emitente  <= tt-param.cod-emitente-fim  
            AND   ri-bem.nat-operacao  >= tt-param.nat-oper-ini     
            AND   ri-bem.nat-operacao  <= tt-param.nat-oper-fim
            AND   ri-bem.dat-entrada   >= tt-param.dt-docto-ini 
            AND   ri-bem.dat-entrada   <= tt-param.dt-docto-fim,

            EACH ri-bem-grupo NO-LOCK
            WHERE ri-bem-grupo.id-bem    = ri-bem.id-bem
            AND   ri-bem-grupo.cod-grupo = ri-estab-grupos.cod-grupo
            AND   ri-bem-grupo.data-1 <> ?
            AND   ri-bem-grupo.data-1 <= tt-param.dt-corte
            
            {esp/esri0404rp.i "ri-bem.nr-nota-fis" "f-docto" "ri-estab-grupos.cod-estabel" "ri-bem-grupo.id-bem"} 
        END.

/*
    WHEN 3 /*Patrim“nio*/ THEN

        FOR EACH ri-estab-grupos NO-LOCK
            WHERE ri-estab-grupos.cod-estabel >= tt-param.cod-estabel-ini
            AND   ri-estab-grupos.cod-estabel <= tt-param.cod-estabel-fim
            AND   ri-estab-grupos.cod-grupo   = tt-param.cod-grupo-ini,
        
            EACH ri-bem NO-LOCK
            WHERE ri-bem.cod-estabel    = ri-estab-grupos.cod-estabel
            AND   ri-bem.nr-patrimonio >= tt-param.nr-patrimonio-ini 
            AND   ri-bem.nr-patrimonio <= tt-param.nr-patrimonio-fim,

            EACH ri-bem-grupo NO-LOCK
            WHERE ri-bem-grupo.id-bem    = ri-bem.id-bem
            AND   ri-bem-grupo.cod-grupo = ri-estab-grupos.cod-grupo
            AND   ri-bem-grupo.data-1 <> ?
            AND   ri-bem-grupo.data-1 <= tt-param.dt-corte

            {esp/esri0404rp.i "ri-bem.nr-patrimonio"  "f-patrimonio" "ri-estab-grupos.cod-estabel" "ri-bem-grupo.id-bem"} 
        END.

    WHEN 4 /*Ordem Produ‡Æo*/ THEN

        FOR EACH ri-estab-grupos NO-LOCK
            WHERE ri-estab-grupos.cod-estabel >= tt-param.cod-estabel-ini
            AND   ri-estab-grupos.cod-estabel <= tt-param.cod-estabel-fim
            AND   ri-estab-grupos.cod-grupo   = tt-param.cod-grupo-ini,
        
            EACH ri-bem NO-LOCK
                WHERE ri-bem.cod-estabel    = ri-estab-grupos.cod-estabel
                AND   ri-bem.nr-ord-produ  >= tt-param.nr-ord-produ-ini 
                AND   ri-bem.nr-ord-produ  <= tt-param.nr-ord-produ-fim,

            EACH ri-bem-grupo NO-LOCK
            WHERE ri-bem-grupo.id-bem    = ri-bem.id-bem
            AND   ri-bem-grupo.cod-grupo = ri-estab-grupos.cod-grupo
            AND   ri-bem-grupo.data-1 <> ?
            AND   ri-bem-grupo.data-1 <= tt-param.dt-corte

            {esp/esri0404rp.i "ri-bem.nr-patrimonio" "f-ord-prod" "ri-estab-grupos.cod-estabel" "ri-bem-grupo.id-bem"} 
        END.
*/
END CASE.

RUN pi-finalizar IN h-acomp.

{include/i-rpclo.i}

IF tt-param.l-gera-planilha THEN
   OUTPUT STREAM s-planilha CLOSE.

RETURN "OK".

PROCEDURE pi-gerar-dados-extrato :
    def input param p-string as char no-undo.
    if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
        output to value(c-arquivo-log) append.
             put UNFORMATTED "==> " STRING(TIME,"HH:MM:SS") " - " ENTRY(1,PROGRAM-NAME(2), "") " - " p-string SKIP.
        output close. 
    end.
END PROCEDURE.

{esp/esri0404rp.i3} /* pi-cacula-valores-bem */

{esp/esri0404rp.i4} /* pi-retorna-cp-lp */

{esp/esri0404rp.i5} /* pi-calcula-valor-atual-bem */
    
{esp/esri0404rp.i6} /* pi-calcula-valor-parcela-mensal */

{esp/esri0404rp.i7} /* pi-gera-historico-transf */
