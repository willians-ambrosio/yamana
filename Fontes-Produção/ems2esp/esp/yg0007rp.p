&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*******************************************************************************
** Programa : ESIN0010RP
** Descriªío: IMPORTAÄ«O DE CONTRATOS ATRAVêS DE PLANILHA
** Autor    : Maurilio - 07-04-2016
*******************************************************************************/

{include/i-prgvrs.i YG0007RP 12.1.17.000}

/* ***************************  Definitions  ************************** */
{utp/ut-glob.i}

/*------------- Definiá∆o de Temp-Table --------------------------------------*/

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
    FIELD nome-planilha    AS CHAR
    FIELD i-acao           AS INTEGER
    FIELD l-simula         AS LOGICAL
    .

define temp-table tt-param-cn0206 no-undo
   field destino          as integer
   field arq-destino      as char
   field arq-entrada      as char
   field todos            as integer
   field usuario          as char
   field data-exec        as date
   field hora-exec        as integer
   field da-transacao     as date format "99/99/9999" 
   field c-todos          as char
   field c-destino        as char.


Define Temp-Table tt-raw-digita Field raw-digita As Raw.

/*-------------- Definiªío Par¸metros -----------------------------------*/
Define Input Parameter raw-param As Raw No-Undo.
Define Input Parameter Table For tt-raw-digita.

/*------------- DefiniØ o de Buffers ------------------------------------*/

/*--------------- DefiniØ o de Variaveis --------------------------------*/
def var h-acomp                AS HANDLE     NO-UNDO.
def var ct                     as int        NO-UNDO.

DEFINE VARIABLE i-linha                AS INTEGER    NO-UNDO.
DEFINE VARIABLE chexcelapplication     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet            AS COM-HANDLE NO-UNDO.

DEF STREAM s-txt.

DEF TEMP-TABLE tt-dados
    FIELD cod-emitente      LIKE emitente.cod-emitente
    FIELD nome-abrev         LIKE emitente.nome-abrev
    FIELD observacao        AS   CHARACTER FORMAT "x(2000)"    
    INDEX codigo  IS PRIMARY UNIQUE cod-emitente.

DEFINE VARIABLE nome-arq-txt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arq-temp AS CHARACTER   NO-UNDO.

DEFINE VARIABLE nr-contrato-aux  LIKE contrato-for.nr-contrato       NO-UNDO.
DEFINE VARIABLE num-pedido-aux   LIKE pedido-compr.num-pedido        NO-UNDO.
DEFINE VARIABLE numero-ordem-aux LIKE ordem-compra.numero-ordem      NO-UNDO.
DEFINE VARIABLE cod-estabel-aux  LIKE ordem-compra.cod-estabel       NO-UNDO.
DEFINE VARIABLE num-ord-inv-aux  LIKE contrato-for-ext.num-ord-inv   NO-UNDO.
DEFINE VARIABLE cod-empresa      LIKE param-global.empresa-prin no-undo.

def new global shared var r-num-ped     as rowid           no-undo.

DEF TEMP-TABLE tt-ord-inv-item NO-UNDO
    FIELD num-ord-inv    LIKE item-contrat-ext.num-ord-inv
    FIELD num-seq-item   LIKE item-contrat-ext.num-seq-item
    FIELD it-codigo      LIKE ITEM.it-codigo
    INDEX id01 num-seq-item.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


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
         HEIGHT             = 10.63
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
Create tt-param. 
Raw-Transfer raw-param To tt-param.
Find First tt-param No-Lock No-Error.
/*
FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.
*/

Find First param-global No-Lock No-Error.
/*Find ems5.empresa Where empresa.ep-codigo=param-global.empresa-prin No-Lock No-Error.*/
 
{include/i-rpvar.i}

c-arq-temp       = tt-param.arquivo.
tt-param.arquivo = entry(1,tt-param.arquivo,'.') + "-esp." + entry(2,tt-param.arquivo,'.').

{include/i-rpout.i /*&stream="stream str-rp"*/ &tofile=tt-param.arquivo}

Assign c-empresa      = "YAMANA" /*empresa.razao-social*/
       c-titulo-relat = "IMPORTAÄ«O DE CONTRATOS"
       c-sistema      = ""
       c-programa     = "ESCN0206RP"
       c-versao       = "12.10"
       c-revisao      = "1.000".

{include/i-rpcab.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.    
/*----------------- DefiniØ o de Frames ----------------------------------*/

/*---------------- Bloco Principal ---------------------------------------*/

Run utp/ut-acomp.p persistent set h-acomp.

RUN pi-inicializar IN h-acomp ('Iniciando processo').

RUN pi-executar.

RUN pi-finalizar IN h-acomp.

{include/i-rpclo.i}

RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-excel-finalizar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-finalizar Procedure 
PROCEDURE pi-excel-finalizar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* quando exportar, fechar excel */
    chexcelapplication:QUIT().
    
    RELEASE OBJECT chexcelapplication:Workbooks     NO-ERROR.
    RELEASE OBJECT chexcelapplication:chworksheet   NO-ERROR.
    RELEASE OBJECT chexcelapplication               NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-excel-inicializar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel-inicializar Procedure 
PROCEDURE pi-excel-inicializar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN pi-acompanhar IN h-acomp ("Abrindo Excel...").

    CREATE "Excel.Application" chexcelapplication.

    chexcelapplication:APPLICATION:DisplayAlerts = FALSE.
    chexcelapplication:VISIBLE                   = FALSE.

    chWorkbook = chExcelApplication:Workbooks:OPEN(FILE-INFO:FULL-PATHNAME). 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILE-INFO:FILE-NAME = tt-param.nome-planilha.
   IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      
      PUT "N«O FOI POSS÷VEL ABRIR PLANILHA DE DADOS !!!" SKIP
          "Verifique o nome da planilha/endereáo se s∆o v†lidos ou possui acesso de leitura" SKIP
          "Planilha: " tt-param.nome-planilha SKIP.
          
      RETURN.
   END.
   ELSE DO:
   
      RUN pi-excel-inicializar.
      RUN pi-leitura-dados.
      RUN pi-excel-finalizar.
      RUN pi-grava-dados.
      RUN pi-gera-log.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-gera-log) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-log Procedure 
PROCEDURE pi-gera-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN nome-arq-txt = SESSION:TEMP-DIRECTORY + "fornecedores_" + REPLACE(STRING(TODAY,"99/99/9999"),"/","") + "_" + REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".csv".
   
   OUTPUT STREAM s-txt TO value(nome-arq-txt) NO-CONVERT.

   PUT STREAM s-txt "Fornecedor;Nome;Observaá∆o" SKIP.
   
   FOR EACH tt-dados
            NO-LOCK:
      PUT STREAM s-txt UNFORMATTED 
          tt-dados.cod-emitente  ";"
          tt-dados.nome-abrev     ";"
          tt-dados.observacao    ";" SKIP.
   
   END.
   
   OUTPUT STREAM s-txt CLOSE.

   DOS SILENT START excel.exe VALUE(nome-arq-txt).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-leitura-dados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leitura-dados Procedure 
PROCEDURE pi-leitura-dados:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN pi-acompanhar IN h-acomp ('Leitura Planilha Fornecedores...').            
   
   chworksheet = chexcelapplication:sheets:ITEM('FORNECEDORES').
   
   i-linha = 2.
   DO WHILE chworksheet:Range("a" + string(i-linha)):VALUE <> ?   AND 
            chworksheet:Range("a" + string(i-linha)):VALUE <> ' ' AND
            chworksheet:Range("a" + string(i-linha)):VALUE <> 'C¢digo':
   
       RUN pi-acompanhar IN h-acomp ("Fornecedor - " + STRING(INTEGER(chworksheet:Range("a" + string(i-linha)):VALUE))).
   
       CREATE tt-dados.
       ASSIGN tt-dados.cod-emitente = INTEGER(chworksheet:Range("a" + string(i-linha)):VALUE)
              tt-dados.nome-abrev    = chworksheet:Range("b" + string(i-linha)):VALUE.
   
       i-linha = i-linha + 1.                                                   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pi-grava-dados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-dados Procedure 
PROCEDURE pi-grava-dados:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN pi-acompanhar IN h-acomp ('Gravando dados Fornecedores...').            

   blk:
   FOR EACH tt-dados
            EXCLUSIVE-LOCK:
      FIND FIRST emitente
           WHERE emitente.cod-emitente = tt-dados.cod-emitente
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(emitente) THEN DO:
         ASSIGN tt-dados.observacao = "Fornecedor " + STRING(tt-dados.cod-emitente) + "-" + RIGHT-TRIM(TRIM(SUBSTRING(tt-dados.nome-abrev,1,LENGTH(tt-dados.nome-abrev)))) + " n∆o encontrado.".
         NEXT blk.
      END.

      IF emitente.identif = 1 THEN DO:
         ASSIGN tt-dados.observacao = "Emitente " + STRING(tt-dados.cod-emitente) + "-" + RIGHT-TRIM(TRIM(SUBSTRING(tt-dados.nome-abrev,1,LENGTH(tt-dados.nome-abrev)))) + " n∆o Ç fornecedor.".
         NEXT blk.
      END.

      FIND FIRST dist-emitente 
           WHERE dist-emitente.cod-emitente = emitente.cod-emitente
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE(dist-emitente) THEN DO:
         IF (tt-param.i-acao = dist-emitente.idi-sit-fornec) THEN DO:
            ASSIGN tt-dados.observacao = "Fornecedor " + STRING(tt-dados.cod-emitente) + "-" + RIGHT-TRIM(TRIM(SUBSTRING(tt-dados.nome-abrev,1,LENGTH(tt-dados.nome-abrev)))) + " j† est† na situaá∆o " + ENTRY(dist-emitente.idi-sit-fornec,{diinc/i01di275.i 03}).
            NEXT blk.
         END.

         IF tt-param.l-simula = NO THEN DO:
            ASSIGN dist-emitente.idi-sit-fornec      = tt-param.i-acao
                   dist-emitente.dat-vigenc-inicial  = TODAY
                   dist-emitente.dat-vigenc-final    = 12/31/2999.

            ASSIGN tt-dados.observacao = "Alterada a situaá∆o do Fornecedor " + STRING(tt-dados.cod-emitente) + "-" + RIGHT-TRIM(TRIM(SUBSTRING(tt-dados.nome-abrev,1,LENGTH(tt-dados.nome-abrev)))) + " para:  " + ENTRY(dist-emitente.idi-sit-fornec,{diinc/i01di275.i 03}).
         END.
         ELSE DO:
            ASSIGN tt-dados.observacao = "Alterada a situaá∆o do Fornecedor " + STRING(tt-dados.cod-emitente) + "-" + RIGHT-TRIM(TRIM(SUBSTRING(tt-dados.nome-abrev,1,LENGTH(tt-dados.nome-abrev)))) + " para:  " + ENTRY(tt-param.i-acao,{diinc/i01di275.i 03}) + " (Simulaá∆o)".
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
