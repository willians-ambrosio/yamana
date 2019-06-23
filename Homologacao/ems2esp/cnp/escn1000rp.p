&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------*
    File        : ESCN1000RP.P                                *
    Purpose     : Relat¢rio Contratos (Detalhado)             *
    Author(s)   : Willians Ambrosio - DKP                     *
    Created     : NOV/2018                                    *
    Notes       :                                             *
  ------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
{include/i-prgvrs.i ESCN1000RP 12.01.21.000}
{utp/ut-glob.i} 
{include/i-rpvar.i} 
/*----- TEMP-TABLE DE PARAMETROS -----*/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    /* -------------------------------------- */
    field nr-contrato-ini     AS INTEGER
    field nr-contrato-fin     AS INTEGER
    field dt-vigenc-ini-ini   AS DATE 
    field dt-vigenc-ini-fin   AS DATE
    FIELD dt-vigenc-fin-ini   AS DATE 
    FIELD dt-vigenc-fin-fin   AS DATE 
    field cod-tip-control-ini AS INTEGER 
    field cod-tip-control-fin AS INTEGER
    field l-nao-emitido       AS LOGICAL
    field l-emitido           AS LOGICAL
    field l-cancelado         AS LOGICAL
    field l-atendido          AS LOGICAL

    field l-nao-emitido-contr AS LOGICAL
    field l-emitido-contr     AS LOGICAL
    field l-cancelado-contr   AS LOGICAL
    field l-atendido-contr    AS LOGICAL

    field l-ativo             AS LOGICAL.

DEFINE TEMP-TABLE tt-relatorio NO-UNDO
    FIELD nr-contrato         LIKE contrato-for.nr-contrato        /*A*/
    FIELD des-contrat         LIKE contrato-for.des-contrat        /*B*/
    FIELD cod-tipo-contr      LIKE contrato-for.cod-tipo-contrat   /*C*/
    FIELD tipo-contrato       LIKE tipo-contrat.des-tipo-contrat   /*C*/
    FIELD ind-sit-contrat     LIKE contrato-for.ind-sit-contrat    /*D*/
    FIELD desc-sit-contrat    AS CHARACTER                         /*D*/
    FIELD cod-emitente        LIKE contrato-for.cod-emitente       /*E*/
    FIELD nome-abrev          LIKE emitente.nome-abrev             /*F*/
    FIELD num-seq-item        LIKE item-contrat.num-seq-item       /*G*/
    FIELD it-codigo           LIKE item-contrat.it-codigo          /*H*/
    FIELD desc-item           LIKE ITEM.desc-item                  /*I*/
    FIELD val-total           LIKE item-contrat.val-total          /*J*/
    FIELD ind-sit-item        LIKE item-contrat.ind-sit-item       /*K*/
    FIELD desc-sit-item       AS CHARACTER                         /*K*/
    FIELD ativo               AS CHARACTER                         /*L*/
    FIELD ind-tipo-control    LIKE item-contrat.ind-tipo-control   /*M*/
    FIELD desc-tipo-control   AS CHARACTER                         /*M*/
    .

def temp-table tt-raw-digita
  field raw-digita    as raw.
/*----- DEFINICAO DE VARIAVEIS LOCAIS      -----*/
DEFINE VARIABLE h-acomp      AS HANDLE      NO-UNDO. 
DEFINE VARIABLE c-op-item    AS CHARACTER   NO-UNDO.

/*----  DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.
/*----- PREPARACAO DOS PARAMETROS          -----*/
create tt-param.
raw-transfer raw-param to tt-param.
/* ---  DEFINI€ÇO DAS VARIAVEIS LOCAIS     ----- */
DEFINE VARIABLE iRegistros    AS INTEGER     NO-UNDO.

/* Begins Variaveis utilizadas para rotinas do excel */
DEFINE VARIABLE ch-excel     AS COM-HANDLE       NO-UNDO.
DEFINE VARIABLE ch-book      AS COM-HANDLE       NO-UNDO.
DEFINE VARIABLE ch-sheet     AS COM-HANDLE       NO-UNDO.
DEFINE VARIABLE c-cel        AS CHARACTER        NO-UNDO.
DEFINE VARIABLE c-range      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE i-plan       AS INTEGER          NO-UNDO.
define variable i-col        AS INTEGER EXTENT 2 NO-UNDO.
DEFINE VARIABLE c-col        AS CHARACTER        NO-UNDO.
DEFINE VARIABLE i-linha      AS INTEGER          NO-UNDO.
DEFINE VARIABLE c-modelo     AS CHARACTER   FORMAT "x(256)"     NO-UNDO.
DEFINE VARIABLE c-modelo-aux AS CHARACTER        NO-UNDO.
DEFINE VARIABLE i-excel-version  AS INTEGER      NO-UNDO.
/* end Variaveis utilizadas para rotinas do excel */

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
         HEIGHT             = 10.38
         WIDTH              = 33.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

FIND FIRST tt-param NO-LOCK NO-ERROR.

RUN pi-inicializar IN h-acomp(INPUT "Processando Dados, Aguarde...").
RUN piProcessa.  


OS-DELETE VALUE(tt-param.arquivo) NO-ERROR.

ASSIGN c-modelo      = SEARCH("cnp/escn1000.xlsx")
       c-modelo-aux  = tt-param.arquivo.

OS-COPY VALUE(c-modelo) VALUE(tt-param.arquivo).

RUN pi-inicializar IN h-acomp(INPUT "Gerando Excel, Aguarde...").
RUN piGerarExcel.

RUN pi-finalizar IN h-acomp.
return "OK":U.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-piDefineColuna) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piDefineColuna Procedure 
PROCEDURE piDefineColuna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    assign i-col [1] = i-col [1] + 1.

    if i-col [1] > 90 Then assign i-col [2] = i-col [2] + 1
                                  i-col [1] = 65.

    assign c-col = ((if i-col [2] > 64 then trim(chr(i-col[2])) else "") + chr(i-col[1]))
           c-cel = c-col + trim(string(i-linha)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piGerarExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGerarExcel Procedure 
PROCEDURE piGerarExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
    /* Rotina para utilizar modelo excel no processamento */
    CREATE "Excel.Application" ch-Excel.   
    ch-Excel:Visible = No.  
    ch-Book          = ch-Excel:Workbooks:ADD(c-modelo-aux). 
    ch-sheet         = ch-excel:sheets:item(1).
    ch-sheet:name    = "ESCN1000". 

    Run pi-acompanhar in h-acomp (input "Iniciando Processo..."). 
    /********************************** BEGINS CABE€ALHO  *******************************/
    ASSIGN i-col   = 64
           i-linha = 2.
    /**********************************  END CABE€ALHO   *******************************/
    /**********************************  BEGINS CORPO  *******************************/
    FOR EACH tt-relatorio  NO-LOCK:

        RUN pi-acompanhar in h-acomp (INPUT "Gerando Planilha - Restam -> " + STRING(iRegistros)).         

        ASSIGN iRegistros = iRegistros - 1.

        PROCESS EVENTS.
        /* --------------------------------- CABE€ALHO --------------------------------- */
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.nr-contrato       ).                                         /*A*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.des-contrat       ).                                         /*B*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.tipo-contrato     ).                                         /*C*/        
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.desc-sit-contrat  ).                                         /*D*/        
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.cod-emitente      ).                                         /*E*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.nome-abrev        ).                                         /*F*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.num-seq-item      ).                                         /*G*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.it-codigo         ).                                         /*H*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.desc-item         ).                                         /*I*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   tt-relatorio.val-total                 .                                         /*J*/        
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.desc-sit-item     ).                                         /*K*/
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.ativo             ).                                         /*L*/        
        RUN piDefineColuna. ch-Sheet:Range(c-cel):VALUE =   string(tt-relatorio.desc-tipo-control ).                                         /*M*/
        
        ASSIGN i-linha  = i-linha + 1
               i-col    = 64.
    END.

    ch-Sheet:Range("J2:J" + STRING(i-linha)):select().
    ch-excel:Selection:NumberFormat = "###.###.##0,00".

    ch-Sheet:Range("A2"):select().

    ch-Sheet:Range("A:M"):EntireColumn:AutoFit.
    /**********************************  END CORPO *******************************/
    CASE tt-param.destino:
    
        WHEN 1 THEN 
        DO:    
            ch-sheet:printout(,,,,c-modelo-aux,,). 
            ch-excel:WORKBOOKS:CLOSE().  
        END.
        WHEN 2 THEN 
        DO:  
            ch-excel:Application:DefaultFilePath = SESSION:TEMP-DIRECTORY.
            ch-excel:DisplayAlerts  = NO.   
            c-modelo-aux = REPLACE(c-modelo-aux,"/","\").  
            ch-excel:ActiveWorkbook:SaveAs(c-modelo-aux,,,,,,1). 
            ch-excel:ActiveWindow:CLOSE(NO).
        END.
        OTHERWISE 
        DO:
            ch-excel:visible = true.       
        END.                    
    END CASE.  

    RELEASE OBJECT ch-excel.  /* excelApp.   */
    RELEASE OBJECT ch-book .  /* excelWorkB. */
    RELEASE OBJECT ch-sheet.  /* excelWorkS. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piProcessa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piProcessa Procedure 
PROCEDURE piProcessa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDescSituacao AS CHARACTER EXTENT 4  NO-UNDO.

ASSIGN iRegistros       = 0
       cDescSituacao[1] = "NÆo Emitido" 
       cDescSituacao[2] = "Emitido" 
       cDescSituacao[3] = "Cancelado" 
       cDescSituacao[4] = "Atendido".

FOR EACH contrato-for WHERE
         contrato-for.nr-contrato      >= tt-param.nr-contrato-ini     AND
         contrato-for.nr-contrato      <= tt-param.nr-contrato-fin     AND
         contrato-for.dt-ini-validade  >= tt-param.dt-vigenc-ini-ini   AND
         contrato-for.dt-ini-validade  <= tt-param.dt-vigenc-ini-fin   AND
         contrato-for.dt-ter-validade  >= tt-param.dt-vigenc-fin-ini   AND
         contrato-for.dt-ter-validade  <= tt-param.dt-vigenc-fin-fin   AND
         contrato-for.cod-tipo-contrat >= tt-param.cod-tip-control-ini AND
         contrato-for.cod-tipo-contrat <= tt-param.cod-tip-control-fin AND 
         contrato-for.log-libera        = tt-param.l-ativo             NO-LOCK
      BY contrato-for.nr-contrato:

   RUN pi-acompanhar IN h-acomp (INPUT "Contrato: " + STRING(contrato-for.nr-contrato)).

   IF (tt-param.l-nao-emitido-contr and contrato-for.ind-sit-contrat = 1  OR
       tt-param.l-emitido-contr     and contrato-for.ind-sit-contrat = 2  OR 
       tt-param.l-cancelado-contr   and contrato-for.ind-sit-contrat = 3  OR 
       tt-param.l-atendido-contr    and contrato-for.ind-sit-contrat = 4) THEN
   DO: 
       
       FOR EACH item-contrat WHERE 
                item-contrat.nr-contrato = contrato-for.nr-contrato NO-LOCK
             BY item-contrat.num-seq-item:    
       
           RUN pi-acompanhar IN h-acomp (INPUT "Contrato: " + STRING(contrato-for.nr-contrato) + " / Seq: " + STRING(item-contrat.num-seq-item)).
       
           IF (tt-param.l-nao-emitido and item-contrat.ind-sit-item = 1  OR
               tt-param.l-emitido     and item-contrat.ind-sit-item = 2  OR 
               tt-param.l-cancelado   and item-contrat.ind-sit-item = 3  OR 
               tt-param.l-atendido    and item-contrat.ind-sit-item = 4) THEN
           DO:
               FIND FIRST ITEM WHERE
                          ITEM.it-codigo                 = item-contrat.it-codigo NO-LOCK NO-ERROR.
           
               FIND FIRST tipo-contrat WHERE
                          tipo-contrat.cod-tipo-contr    = contrato-for.cod-tipo-contrat NO-LOCK NO-ERROR.
           
               FIND FIRST emitente WHERE
                          emitente.cod-emitente          = contrato-for.cod-emitente NO-LOCK NO-ERROR.
           
              CREATE tt-relatorio.
              ASSIGN tt-relatorio.nr-contrato         = contrato-for.nr-contrato        
                     tt-relatorio.des-contrat         = contrato-for.des-contrat        
                     tt-relatorio.cod-tipo-contr      = contrato-for.cod-tipo-contrat   
                     tt-relatorio.tipo-contrato       = IF AVAIL tipo-contrat THEN tipo-contrat.des-tipo-contrat  ELSE "Modalidade Inv lida"
                     tt-relatorio.ind-sit-contrat     = contrato-for.ind-sit-contrat    
                     tt-relatorio.desc-sit-contrat    = cDescSituacao[contrato-for.ind-sit-contrat]                      
                     tt-relatorio.cod-emitente        = contrato-for.cod-emitente       
                     tt-relatorio.nome-abrev          = IF AVAIL emitente THEN emitente.nome-abrev ELSE "Emitente inv lido"
                     tt-relatorio.num-seq-item        = item-contrat.num-seq-item       
                     tt-relatorio.it-codigo           = item-contrat.it-codigo          
                     tt-relatorio.desc-item           = IF AVAIL ITEM THEN ITEM.desc-item  ELSE "Item Inv lido"
                     tt-relatorio.val-total           = item-contrat.preco-fornec          
                     tt-relatorio.ind-sit-item        = item-contrat.ind-sit-item       
                     tt-relatorio.desc-sit-item       = cDescSituacao[item-contrat.ind-sit-item]                        
                     tt-relatorio.ativo               = IF item-contrat.log-libera THEN "SIM" ELSE "NÇO"
                     tt-relatorio.ind-tipo-control    = item-contrat.ind-tipo-control
                     iRegistros                       = iRegistros + 1.
           
              CASE item-contrat.ind-tipo-control:
                  WHEN 1 THEN ASSIGN tt-relatorio.desc-tipo-control = "Medi‡Æo".
                  WHEN 2 THEN ASSIGN tt-relatorio.desc-tipo-control = "Ordem".
                  WHEN 3 THEN ASSIGN tt-relatorio.desc-tipo-control = "Programa‡Æo".
              END CASE.
           END.
       END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

