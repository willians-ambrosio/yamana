&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ymof0109RP 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/****************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD c-item-ini       AS CHAR
    FIELD c-item-fim       AS CHAR
    FIELD c-fam-ini       AS CHAR
    FIELD c-fam-fim       AS CHAR
    FIELD c-ge-ini        AS INT
    FIELD c-ge-fim        AS INT
    FIELD c-estab-ini     AS CHAR
    FIELD c-estab-fim     AS CHAR
    FIELD c-ben-ini       AS CHAR
    FIELD c-ben-fim       AS CHAR
    FIELD c-ncm-ini       AS CHAR
    FIELD c-ncm-fim       AS CHAR
    FIELD c-cfa-ini       AS CHAR
    FIELD c-cfa-fim       AS CHAR.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PARÂMETROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELEÇÃO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÃO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usuário * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

/* find empresa                                     */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.

DEF TEMP-TABLE tt-beneficio
    FIELD  cod-beneficio AS INT.



DEF TEMP-TABLE tt-beneficio2
    FIELD  cod-beneficio AS INT
    FIELD  cod-estabel   AS CHAR.




DEF TEMP-TABLE tt-beneficio-valid
    FIELD  cod-beneficio AS INT
    FIELD  cod-estabel  AS CHAR
    FIELD  it-codigo    AS CHAR FORMAT "x(16)"
    FIELD  classe       AS CHAR
    FIELD  r-ben-row    AS ROWID.


DEF TEMP-TABLE tt-msg
    FIELD cod-msg AS INT.


DEFINE VARIABLE c-coluna1   AS CHARACTER INIT "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az"  NO-UNDO.
DEFINE VARIABLE c-coluna    AS CHAR  NO-UNDO.

DEFINE VARIABLE i-coluna    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chExcelapp  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE      NO-UNDO.
DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.

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
   Type: Procedure Template
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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/*   {include/i-rpcab.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:
    {include/i-rpout.i}
    view frame f-cabec.
    view frame f-rodape.    
    run utp/ut-acomp.p persistent set h-acomp.  
    
    {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}
    
    run pi-inicializar in h-acomp (input "Processando...":U). 
    
   
       RUN pi-processa.

        run pi-acompanhar in h-acomp (input "xxxxxxxxxxxxxx":U).
   
    
    run pi-finalizar in h-acomp.
    {include/i-rpclo.i}
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-abre-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel Procedure 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN i-coluna = 0
       I-LINHA  = 0.
       i-linha = i-linha + 1.



 CREATE "Excel.Application" chExcelapp.
 chExcelapp:VISIBLE = TRUE.
 chWorkbook  = chExcelapp:Workbooks:ADD().
 chWorksheet = chExcelapp:Sheets:ITEM(1).

 chWorksheet:NAME = "Itens".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cabecario) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cabecario Procedure 
PROCEDURE pi-cabecario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
i-coluna = 0.
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE  = "Item"         .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE  = "Descri‡Æo"    .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "CFA"          . 
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "Descri‡Æo"     .  
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "NCM"     .  
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "Estab"         .                                                  
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE =  "Beneficio"    .                              
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE  = "Desc. Benef." .                                                                                
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE  = "Dt.Val.Ini"   .                           
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "Dt.Val.Fim"   .                           
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "ICMS - PRIORIDADE"                      . 
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "ICMS - Ben.ICMS Est"                    .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE = "ICMS - % Red Est"                       .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range(c-coluna + STRING(i-linha)):VALUE  = "ICMS - % Red Bc Est"                   .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,",").  chWorksheet:range( c-coluna + STRING(i-linha)):VALUE  =  "ICMS - NÆo Incide Est"                .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - Ben. ICMS Inter"               .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - % Red Int"                      .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - % Red Bc Int"                   .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - NÆo Incide Int"                 .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE  =  "ICMS - Ben.ICMS Imp"                  .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,",").  chWorksheet:range( c-coluna + STRING(i-linha)):VALUE  =  "ICMS - % Red Imp"                     .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - % Red Bc Imp"                  .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "ICMS - NÆo Incide Imp"                 .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "PIS/COFINS - PRIORIDADE"               .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "PIS/COFINS - Compr Nac"                .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE  =  "PIS/COFINS - Compr Imp"               .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,",").  chWorksheet:range( c-coluna + STRING(i-linha)):VALUE  =  "Compras - Comprador ICMS"             .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "Compras - Comprador Pis/Cofins"       .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "Compras - Forn ICMS"                   .
i-coluna = i-coluna + 1. c-coluna = ENTRY(i-coluna,c-coluna1,","). chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  "Compras - Forn Pis/Cofins"             .
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-monta-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-excel Procedure 
PROCEDURE pi-monta-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-beneficio-valid WHERE tt-beneficio-valid.cod-beneficio >= int(tt-param.c-ben-ini)
                            AND   tt-beneficio-valid.cod-beneficio <= int(tt-param.c-ben-fim),
    EACH ITEM WHERE ITEM.it-codigo = tt-beneficio-valid.it-codigo:

    i-coluna = 0.
    i-coluna = i-coluna + 1.
    i-linha  = i-linha + 1.
   
        run pi-acompanhar in h-acomp (INPUT tt-beneficio-valid.cod-beneficio ).

    FIND FIRST es-cfa  WHERE es-cfa.classe  = trim(tt-beneficio-valid.classe) 
                        NO-LOCK NO-ERROR.

    FIND es-beneficio WHERE es-beneficio.cod-beneficio = tt-beneficio-valid.cod-beneficio NO-LOCK NO-ERROR.
    
    FIND es-ben-estab WHERE ROWID(es-ben-estab) = tt-beneficio-valid.r-ben-row NO-LOCK  NO-ERROR.


     c-coluna = ENTRY(i-coluna,c-coluna1,",").
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = ITEM.it-codigo.  
     
     i-coluna = i-coluna + 1.
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                   
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =ITEM.desc-item. 
    
     i-coluna = i-coluna + 1.                                              
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                             
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF AVAIL es-cfa THEN es-cfa.classe ELSE "9999". 

     i-coluna = i-coluna + 1.                                                                                              
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                             
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF AVAIL es-cfa THEN es-cfa.descricao ELSE "NÆo Cadastrado".  


       i-coluna = i-coluna + 1.                                                                                      
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                     
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = string(ITEM.class-fiscal,"9999.99.99")  .             

         
     i-coluna = i-coluna + 1.                                                                                                                              
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                                             
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = tt-beneficio-valid.cod-estabel. 
     
     i-coluna = i-coluna + 1.                                                                    
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                               
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = tt-beneficio-valid.cod-beneficio.

     i-coluna = i-coluna + 1.                                                                
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                               
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF AVAIL es-beneficio  THEN es-beneficio.desc-beneficio ELSE "NÆo Cadastrado". 

     i-coluna = i-coluna + 1.                                                                                                               
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                              
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF AVAIL es-ben-estab THEN  string(es-ben-estab.dt-ini-vald,"99/99/9999") ELSE "00/00/0000" .     
         
     i-coluna = i-coluna + 1.                                                                                                                                 
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                                            
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF AVAIL es-ben-estab THEN  string(es-ben-estab.dt-fim-vald,"99/99/9999") ELSE "00/00/0000" .
     i-coluna = i-coluna + 1.                                                                                                                                          
     c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                                                             
     chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.icms-prior.         
                                                                                                                                                                       
      i-coluna = i-coluna + 1.                                                           
      c-coluna = ENTRY(i-coluna,c-coluna1,",").                                          
      chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.icms-ben-icms-est THEN "SIM" ELSE "" .     
          
      i-coluna = i-coluna + 1.                                                                                         
      c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                           
      chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  es-ben-estab.icms-ben-icms-est-perc-redu . 
      
      i-coluna = i-coluna + 1.                                                                           
      c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                             
      chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  es-ben-estab.icms-ben-icms-est-red-bc.              

      i-coluna = i-coluna + 1.                                                                             
      c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                            
      chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  IF es-ben-estab.icms-ben-icms-est-nao-incide THEN "SIM" ELSE "".          

      i-coluna = i-coluna + 1.                                                                                                    
      c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                 
      chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.icms-ben-icms-inter THEN "SIM" ELSE "".    

       i-coluna = i-coluna + 1.                                                                                          
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                       
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  es-ben-estab.icms-ben-icms-inter-perc-redu . 

       i-coluna = i-coluna + 1.                                                                              
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                             
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  es-ben-estab.icms-ben-icms-inter-red-bc. 

       i-coluna = i-coluna + 1.                                                                         
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                        
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.icms-ben-icms-inter-nao-incide THEN "SIM" ELSE "".  

       i-coluna = i-coluna + 1.                                                                                                  
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                  
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.icms-ben-icms-imp THEN "SIM" ELSE "" . 

       i-coluna = i-coluna + 1.                                                                                                                  
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                                 
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.icms-ben-icms-imp-perc-redu.       
         
       i-coluna = i-coluna + 1.                                                                              
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                               
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.icms-ben-icms-imp-red-bc.  

       i-coluna = i-coluna + 1.                                                                     
       c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                    
       chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.icms-ben-icms-imp-nao-incide THEN "SIM"  ELSE "".  
       
        i-coluna = i-coluna + 1.                                                                                                 
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                 
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.pis-cofins-prioridade.  

        i-coluna = i-coluna + 1.                                                                                                 
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                 
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE =  IF es-ben-estab.pis-cofins-ben-pis-cof-compr-nac THEN "SIM" ELSE "".    

        i-coluna = i-coluna + 1.                                                                                                     
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                                    
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = IF es-ben-estab.pis-cofins-ben-pis-cof-imp  THEN "SIM" ELSE "".

        i-coluna = i-coluna + 1.                                                                                               
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                                               
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.compras-comprador-icms. 

        i-coluna = i-coluna + 1.                                                                   
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                  
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.compras-comprador-pis-confis.

        i-coluna = i-coluna + 1.                                                                          
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                         
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.compras-fornecedor-icms.  
          
        i-coluna = i-coluna + 1.                                                                    
        c-coluna = ENTRY(i-coluna,c-coluna1,",").                                                   
        chWorksheet:range( c-coluna + STRING(i-linha)):VALUE = es-ben-estab.compras-fornecedor-pis-cofins.  
         
         
        IF es-ben-estab.compras-comprador-icms <> 0 THEN DO:
            CREATE tt-msg.
            ASSIGN tt-msg.cod-msg = es-ben-estab.compras-comprador-icm.
                    
        END.
        IF es-ben-estab.compras-comprador-pis-confis <> 0 THEN DO:                         
            CREATE tt-msg.                                                           
            ASSIGN tt-msg.cod-msg = es-ben-estab.compras-comprador-pis-confis.              
                                                                                     
        END.
        IF  es-ben-estab.compras-fornecedor-icms <> 0 THEN DO:          
            CREATE tt-msg.                                                  
            ASSIGN tt-msg.cod-msg = es-ben-estab.compras-fornecedor-icms.     
                                                                            
        END.                                                                
        IF  es-ben-estab.compras-fornecedor-pis-cofins <> 0 THEN DO:           
            CREATE tt-msg.                                               
            ASSIGN tt-msg.cod-msg = es-ben-estab.compras-fornecedor-pis-cofins.  
                                                                         
        END.                                                             


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-msg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-msg Procedure 
PROCEDURE pi-msg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN i-coluna = 0
           I-LINHA  = 0.
            i-linha = i-linha + 1.


    chWorksheet = chExcelapp:Sheets:ITEM(2).

     chWorksheet:NAME = "Mensagens".



    FOR EACH tt-msg BREAK BY tt-msg.cod-msg.

         IF FIRST-OF(tt-msg.cod-msg) THEN DO:

             FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = tt-msg.cod-msg NO-LOCK NO-ERROR.
             IF AVAIL es-mensagem-ben THEN DO:
                   chWorksheet:range( "a" + STRING(i-linha)):VALUE = es-mensagem-ben.cod-mensagem.
                   chWorksheet:range( "b" + STRING(i-linha)):VALUE = es-mensagem-ben.desc-mensagem.
                   chWorksheet:range( "c" + STRING(i-linha)):VALUE = es-mensagem-ben.narrativa.
             END.

              i-linha = i-linha + 1.

         END.




         


    END.






END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa Procedure 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    
FOR EACH ext-item-cfa NO-LOCK WHERE ext-item-cfa.it-codigo >= tt-param.c-item-ini
                              AND   ext-item-cfa.it-codigo <= tt-param.c-item-fim
                              AND   ext-item-cfa.classe >= tt-param.c-cfa-ini
                              AND   ext-item-cfa.classe <= tt-PARAM.c-cfa-fim,
    EACH ITEM WHERE ITEM.it-codigo = ext-item-cfa.it-codigo 
              AND   ITEM.fm-codigo >= tt-param.c-fam-ini
              AND   ITEM.fm-codigo <= tt-param.c-fam-fim
              AND   ITEM.class-fiscal >= tt-param.c-ncm-ini
              AND   ITEM.class-fiscal <= tt-param.c-ncm-fim NO-LOCK:

   EMPTY TEMP-TABLE tt-beneficio.

    run pi-acompanhar in h-acomp (INPUT ext-item-cfa.it-codigo).

    FOR EACH  es-beneficio-cfa  WHERE es-beneficio-cfa.classe = ext-item-cfa.classe NO-LOCK:
        CREATE tt-beneficio.
        ASSIGN tt-beneficio.cod-beneficio = es-beneficio-cfa.cod-beneficio.
    END.

    FOR EACH  es-beneficio-ncm WHERE  es-beneficio-ncm.class-fisc-ini >= ITEM.class-fisc
                               AND    es-beneficio-ncm.class-fisc-fim <= ITEM.class-fisc NO-LOCK:
         CREATE tt-beneficio.                                                   
          ASSIGN tt-beneficio.cod-beneficio = es-beneficio-ncm.cod-beneficio.
    END.

    FOR EACH es-cfa-emp WHERE es-cfa-emp.classe = ext-item-cfa.classe NO-LOCK  :
          FOR EACH tt-beneficio:
              CREATE tt-beneficio2.
              ASSIGN tt-beneficio2.cod-beneficio = tt-beneficio.cod-beneficio
                     tt-beneficio2.cod-estabel   = es-cfa-emp.cod-estabel.
          END.
    END.


















    FOR EACH tt-beneficio2:
        FIND FIRST es-ben-estab WHERE es-ben-estab.cod-beneficio = tt-beneficio2.cod-beneficio
                                AND   es-ben-estab.dt-ini-vald   <= TODAY    
                                AND   es-ben-estab.dt-fim-vald   >= TODAY    
                                AND   es-ben-estab.cod-estabel    = tt-beneficio2.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL es-ben-estab THEN DO:

            FIND FIRST tt-beneficio-valid 
                 WHERE tt-beneficio-valid.cod-beneficio = es-ben-estab.cod-beneficio
                   AND tt-beneficio-valid.cod-estabel   = es-ben-estab.cod-estabel 
                   AND tt-beneficio-valid.it-codigo     = ext-item-cfa.it-codigo NO-ERROR.
            IF NOT AVAIL(tt-beneficio-valid) THEN DO:
            CREATE tt-beneficio-valid.
            ASSIGN tt-beneficio-valid.cod-beneficio = es-ben-estab.cod-beneficio.
                   tt-beneficio-valid.cod-estabel   = es-ben-estab.cod-estabel.
                   tt-beneficio-valid.it-codigo     = ext-item-cfa.it-codigo.
                   tt-beneficio-valid.classe        = ext-item-cfa.classe.
                   tt-beneficio-valid.r-ben-row     = ROWID(es-ben-estab).
            END.

            
                       
        END.
    END.
END.

RUN pi-abre-excel.

RUN pi-cabecario.

RUN pi-monta-excel.

RUN pi-msg.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

