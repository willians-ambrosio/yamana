&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** Descricao: Relatorio de movimento de CFOP'S
** Autor: Gilberto Rissati - Datasul - GWA
** Data: 12/10/2003
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.
def buffer unid-feder  for ems2cadme.unid-feder.

{include/i-prgvrs.i ESOF0001RP 2.06.00.000}
/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)".
def var c-liter-sel                  as character format "x(10)".
def var c-liter-imp                  as character format "x(12)".    
def var c-destino                    as character format "x(15)".

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD cod-estabel      LIKE estabelec.cod-estabel
    FIELD ano              AS INTEGER
    FIELD mes              AS INTEGER
    FIELD tipo             AS INTEGER.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
          ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEFINE TEMP-TABLE tt_dados 
   FIELD estado              LIKE doc-fiscal.estado      
   FIELD no-estado           LIKE unid-feder.no-estado
   FIELD cfop                AS CHARACTER
   FIELD nome                LIKE natur-oper.denominacao
   FIELD dt-emis-doc         LIKE doc-fiscal.dt-emis-doc 
   FIELD tipo-nat            LIKE doc-fiscal.tipo-nat

   FIELD vl-bicms            LIKE doc-fiscal.vl-bicms    FORMAT ">>,>>>,>>9.99"
   FIELD vl-icmsnt           LIKE doc-fiscal.vl-icmsnt   FORMAT ">>,>>>,>>9.99" 
   FIELD vl-icmsou           LIKE doc-fiscal.vl-icmsou   FORMAT ">>,>>>,>>9.99"
   FIELD vl-cont-doc         LIKE doc-fiscal.vl-cont-doc FORMAT ">>,>>>,>>9.99"

   FIELD vl-bicms_contrib    LIKE doc-fiscal.vl-bicms    FORMAT ">>,>>>,>>9.99"
   FIELD vl-icmsnt_contrib   LIKE doc-fiscal.vl-icmsnt   FORMAT ">>,>>>,>>9.99"
   FIELD vl-icmsou_contrib   LIKE doc-fiscal.vl-icmsou   FORMAT ">>,>>>,>>9.99"
   FIELD vl-cont-doc_contrib LIKE doc-fiscal.vl-cont-doc FORMAT ">>,>>>,>>9.99"
   INDEX idx-tipo-nat IS UNIQUE PRIMARY  
                      tipo-nat 
                      estado
                      cfop.

DEFINE VARIABLE dt-inicial AS DATE       NO-UNDO.
DEFINE VARIABLE dt-final   AS DATE       NO-UNDO.

FORM ""
    WITH FRAME f-branco STREAM-IO NO-BOX.


FORM "-------VALOR CONTABIL------" AT 42
     "------BASE DE CALCULO------" AT 70
     "--ISENTAS/NÇO TRIBUTADAS---" AT 98
     "----------OUTRAS-----------" AT 126 SKIP
     WITH FRAME f-label-saidas WIDTH 320 STREAM-IO NO-BOX.

FORM HEADER 
    "PERÖODO" dt-inicial NO-LABEL  
    "AT" dt-final   NO-LABEL 
    WITH FRAME f-periodo WIDTH 320 STREAM-IO COL 104 NO-BOX PAGE-TOP.

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

/*FIND FIRST tt-param NO-LOCK NO-ERROR.*/

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR¶METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELE€ÇO * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESSÇO * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa NO-LOCK
     WHERE empresa.ep-codigo = param-global.empresa-prin
     NO-ERROR.

{utp/ut-liter.i Entradas e Saidas/CFOP * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = "Entradas e Saidas/CFOP".
assign c-empresa     = "Empresa - " + empresa.razao
       c-programa    = "{&programa}"
       c-versao      = "1.00"
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.

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
         HEIGHT             = 5.17
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


do on stop undo, LEAVE:

    ASSIGN dt-inicial = DATE(tt-param.mes,01,tt-param.ano)
           dt-final   = (dt-inicial + 35) - DAY(dt-inicial + 35).

    {include/i-rpout.i}
    view frame f-cabec.
    VIEW FRAME f-periodo.
    view frame f-rodape.    
    run utp/ut-acomp.p persistent set h-acomp.  
    
    run pi-inicializar in h-acomp (input "Verificando").
    
    /* --- Colocar aqui o codigo de impressão --- */

    FOR EACH doc-fiscal 
       WHERE doc-fiscal.tipo-nat >= 1
         AND doc-fiscal.tipo-nat <= 2
         AND doc-fiscal.cod-estabel  = tt-param.cod-estabel
         AND doc-fiscal.dt-docto  >= dt-inicial
         AND doc-fiscal.dt-docto  <= dt-final:

       IF tt-param.tipo = 1 AND
          doc-fiscal.estado <> "BA" THEN NEXT.

       IF tt-param.tipo = 2 AND 
          doc-fiscal.estado = "BA" THEN NEXT.

       RUN pi-acompanhar in h-acomp (input "Natureza: " + doc-fiscal.nat-operacao).
    
       /*IF doc-fiscal.nr-doc-fis = "0000199" THEN NEXT.*/

       FIND FIRST emitente NO-LOCK
            WHERE emitente.nome-abrev = doc-fiscal.nome-ab-emi 
            NO-ERROR.
       FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = doc-fiscal.nat-operacao
            NO-ERROR.

       FIND FIRST tt_dados
            WHERE tt_dados.estado       = doc-fiscal.estado
              AND tt_dados.cfop         = natur-oper.cod-cfop
              AND tt_dados.tipo-nat     = doc-fiscal.tipo-nat
              NO-ERROR.

       IF NOT AVAIL tt_dados THEN DO:
          FIND FIRST unid-feder NO-LOCK
               WHERE unid-feder.estado = doc-fiscal.estado
                 AND unid-feder.pais   = "Brasil"
                 NO-ERROR.
          /**
          FIND FIRST ped-curva NO-LOCK
               WHERE ped-curva.it-codigo = natur-oper.nat-operacao
               NO-ERROR.
          **/
          CREATE tt_dados.
          ASSIGN tt_dados.estado         = doc-fiscal.estado      
                 tt_dados.cfop           = IF AVAIL natur-oper THEN natur-oper.cod-cfop ELSE ""
                 tt_dados.tipo-nat       = doc-fiscal.tipo-nat
                 tt_dados.no-estado      = IF AVAIL unid-feder THEN unid-feder.no-estado ELSE ""
                 tt_dados.nome           = IF AVAIL natur-oper THEN natur-oper.denominacao ELSE "".
       END.
       IF NOT emitente.contrib-icms THEN
          ASSIGN tt_dados.vl-bicms       = tt_dados.vl-bicms    + doc-fiscal.vl-bicms    
                 tt_dados.vl-icmsnt      = tt_dados.vl-icmsnt   + doc-fiscal.vl-icmsnt   
                 tt_dados.vl-icmsou      = tt_dados.vl-icmsou   + doc-fiscal.vl-icmsou   
                 tt_dados.vl-cont-doc    = tt_dados.vl-cont-doc + doc-fiscal.vl-cont-doc.
       ELSE
          ASSIGN tt_dados.vl-bicms_contrib    = tt_dados.vl-bicms_contrib    + doc-fiscal.vl-bicms    
                 tt_dados.vl-icmsnt_contrib   = tt_dados.vl-icmsnt_contrib   + doc-fiscal.vl-icmsnt   
                 tt_dados.vl-icmsou_contrib   = tt_dados.vl-icmsou_contrib   + doc-fiscal.vl-icmsou   
                 tt_dados.vl-cont-doc_contrib = tt_dados.vl-cont-doc_contrib + doc-fiscal.vl-cont-doc.
    END.
    FIND FIRST tt_dados NO-LOCK NO-ERROR.
    IF AVAIL tt_dados THEN
       RUN Mostra_Dados.

    RUN pi-finalizar in h-acomp.
    {include/i-rpclo.i}
end.

PROCEDURE Mostra_Dados:

  DEFINE VARIABLE de_bicms_contrib    LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_icmsnt_contrib   LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_icmsou_contrib   LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_cont-doc_contrib LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
 
  DEFINE VARIABLE de_bicms            LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_icmsnt           LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_icmsou           LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.
  DEFINE VARIABLE de_cont-doc         LIKE tt_dados.vl-bicms_contrib FORMAT ">>,>>>,>>9.99" EXTENT 2   NO-UNDO.


  FOR EACH tt_dados
     BREAK
        BY tt_dados.tipo-nat
        BY tt_dados.estado
        BY tt_dados.cfop:
 
     IF FIRST-OF(tipo-nat) THEN DO:
        IF tt_dados.tipo = 1 THEN
           PUT "ENTRADAS".
        ELSE
           PUT "SAIDAS".

        IF tt-param.tipo = 1 THEN
           PUT " ESTADUAIS".
        IF tt-param.tipo = 2 THEN
           PUT " INTERESTADUAIS".

        PUT SKIP(2).

     END.

     IF FIRST-OF(tt_dados.estado) THEN
        PUT SKIP(1)
            tt_dados.estado 
            tt_dados.no-estado SKIP(1).

     IF tt_dados.tipo-nat = 1 THEN DO:
        DISP (tt_dados.cfop + " " + tt_dados.nome) FORMAT "x(40)" COLUMN-LABEL "Natureza"
             tt_dados.vl-cont-doc_contrib COLUMN-LABEL "Vl Contabil"    
             tt_dados.vl-bicms_contrib    COLUMN-LABEL "Base Calculo"   
             tt_dados.vl-icmsnt_contrib   COLUMN-LABEL "Isenta/N.Trib"        
             tt_dados.vl-icmsou_contrib   COLUMN-LABEL "Outros" 
             WITH FRAME f-entradas WIDTH 320 STREAM-IO NO-BOX DOWN.
        DOWN WITH FRAME f-entradas WIDTH 320.
     END.

     IF tt_dados.tipo-nat = 2 THEN DO:
        IF FIRST-OF(tt_dados.tipo-nat) THEN
           VIEW FRAME f-label-saidas.

        DISP (tt_dados.cfop + " " + tt_dados.nome) FORMAT "x(40)" COLUMN-LABEL "Natureza"
              tt_dados.vl-cont-doc_contrib COLUMN-LABEL "Contrib."                   
              tt_dados.vl-cont-doc         COLUMN-LABEL "NÆo Contrib."                     
              tt_dados.vl-bicms_contrib    COLUMN-LABEL "Contrib."                  
              tt_dados.vl-bicms            COLUMN-LABEL "NÆo Contrib."                    
              tt_dados.vl-icmsnt_contrib   COLUMN-LABEL "Contrib."                       
              tt_dados.vl-icmsnt           COLUMN-LABEL "NÆo Contrib."                         
              tt_dados.vl-icmsou_contrib   COLUMN-LABEL "Contrib."                
              tt_dados.vl-icmsou           COLUMN-LABEL "NÆo Contrib."                  
              WITH FRAME f-saidas WIDTH 320 STREAM-IO NO-BOX DOWN.                        
        DOWN WITH FRAME f-saidas.
     END.

     ASSIGN de_bicms_contrib[1]    = de_bicms_contrib[1]    + tt_dados.vl-bicms_contrib
            de_icmsnt_contrib[1]   = de_icmsnt_contrib[1]   + tt_dados.vl-icmsnt_contrib  
            de_icmsou_contrib[1]   = de_icmsou_contrib[1]   + tt_dados.vl-icmsou_contrib  
            de_cont-doc_contrib[1] = de_cont-doc_contrib[1] + tt_dados.vl-cont-doc_contrib.

     ASSIGN de_bicms_contrib[2]    = de_bicms_contrib[2]    + tt_dados.vl-bicms_contrib    
            de_icmsnt_contrib[2]   = de_icmsnt_contrib[2]   + tt_dados.vl-icmsnt_contrib   
            de_icmsou_contrib[2]   = de_icmsou_contrib[2]   + tt_dados.vl-icmsou_contrib   
            de_cont-doc_contrib[2] = de_cont-doc_contrib[2] + tt_dados.vl-cont-doc_contrib.

     ASSIGN de_bicms[1]    = de_bicms[1]    + tt_dados.vl-bicms
            de_icmsnt[1]   = de_icmsnt[1]   + tt_dados.vl-icmsnt  
            de_icmsou[1]   = de_icmsou[1]   + tt_dados.vl-icmsou  
            de_cont-doc[1] = de_cont-doc[1] + tt_dados.vl-cont-doc.

     ASSIGN de_bicms[2]    = de_bicms[2]    + tt_dados.vl-bicms    
            de_icmsnt[2]   = de_icmsnt[2]   + tt_dados.vl-icmsnt   
            de_icmsou[2]   = de_icmsou[2]   + tt_dados.vl-icmsou   
            de_cont-doc[2] = de_cont-doc[2] + tt_dados.vl-cont-doc.

     IF LAST-OF(tt_dados.estado) THEN DO:
        IF tt_dados.tipo-nat = 1 THEN DO:
           PUT "SUB-TOTAL ESTADO"    AT 25             
                de_cont-doc_contrib[1]  TO 54
                de_bicms_contrib[1]     TO 68           
                de_icmsnt_contrib[1]    TO 82                     
                de_icmsou_contrib[1]    TO 96.

           IF LAST-OF(tt_dados.tipo) THEN DO:
              PUT  SKIP(1)
                   "TOTAL"    AT 36
                   de_cont-doc_contrib[2]  TO 54          
                   de_bicms_contrib[2]     TO 68          
                   de_icmsnt_contrib[2]    TO 82                    
                   de_icmsou_contrib[2]    TO 96.

              ASSIGN de_bicms_contrib[2]    = 0
                     de_icmsnt_contrib[2]   = 0
                     de_icmsou_contrib[2]   = 0
                     de_cont-doc_contrib[2] = 0.
              PAGE.
              VIEW FRAME f-branco.
           END.
        END.
        ELSE DO:
           PUT "SUB-TOTAL ESTADO"       AT 25             
                de_cont-doc_contrib[1]  TO 54
                de_cont-doc[1]          TO 68
                de_bicms_contrib[1]     TO 82           
                de_bicms[1]             TO 96
                de_icmsnt_contrib[1]    TO 110                     
                de_icmsnt[1]            TO 124
                de_icmsou_contrib[1]    TO 138
                de_icmsou[1]            TO 152.

           IF LAST-OF(tt_dados.tipo) THEN DO:
              PUT  SKIP(1)
                   "TOTAL"                 AT 36
                   de_cont-doc_contrib[2]  TO 54          
                   de_cont-doc[2]          TO 68 
                   de_bicms_contrib[2]     TO 82          
                   de_bicms[2]             TO 96 
                   de_icmsnt_contrib[2]    TO 110                   
                   de_icmsnt[2]            TO 124
                   de_icmsou_contrib[2]    TO 138
                   de_icmsou[2]            TO 152 SKIP.

              ASSIGN de_bicms_contrib[2]    = 0
                     de_icmsnt_contrib[2]   = 0
                     de_icmsou_contrib[2]   = 0
                     de_cont-doc_contrib[2] = 0.

              ASSIGN de_bicms[2]    = 0
                     de_icmsnt[2]   = 0
                     de_icmsou[2]   = 0
                     de_cont-doc[2] = 0.
           END.
        END.
        ASSIGN de_bicms_contrib[1]    = 0
               de_icmsnt_contrib[1]   = 0
               de_icmsou_contrib[1]   = 0
               de_cont-doc_contrib[1] = 0.

        ASSIGN de_bicms[1]    = 0
               de_icmsnt[1]   = 0
               de_icmsou[1]   = 0
               de_cont-doc[1] = 0.
     END.
  END.


END PROCEDURE.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


