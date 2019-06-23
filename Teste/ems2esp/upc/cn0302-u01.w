&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */
{cdp/cdcfgmat.i}
&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/*DEF TEMP-TABLE tt-matriz
   
   field num-seq-item              like medicao-contrat.num-seq-item 
   field numero-ordem              like medicao-contrat.numero-ordem 
   field num-seq-event             like medicao-contrat.num-seq-event 
   field num-seq-medicao           like medicao-contrat.num-seq-medicao
   field ep-codigo                 like medicao-contrat.ep-codigo 
   field cod-estabel               like medicao-contrat.cod-estabel 
   field seq-comp                  like medicao-contrat.seq-comp 
   field it-codigo                 like medicao-contrat.it-codigo
   field dat-prev-medicao          like medicao-contrat.dat-prev-medicao
   field qtd-prevista              like medicao-contrat.qtd-prevista    
   field un                        like medicao-contrat.un              
   field val-medicao               like medicao-contrat.val-medicao     
   field perc-medicao              like medicao-contrat.perc-medicao    
   field sld-val-medicao           like medicao-contrat.sld-val-medicao 
   field val-sdo-aloc-med          like medicao-contrat.val-sdo-aloc-med
   field responsavel               like medicao-contrat.responsavel     
   FIELD ct-codigo                 like matriz-rat-med.ct-codigo
   FIELD sc-codigo                 like usuar-mater.sc-codigo.*/

DEF TEMP-TABLE tt-matriz LIKE matriz-rat-med 
    FIELD integra-cn-in-medicao AS LOGICAL.
DEF TEMP-TABLE tt-excel NO-UNDO
    field nr-contrato        like medicao-contrat.nr-contrato
    field cod-estabel        like medicao-contrat.cod-estabel 
    FIELD numero-ordem       LIKE medicao-contrat.numero-ordem
    field seq-comp           like medicao-contrat.seq-comp 
    field it-codigo          like medicao-contrat.it-codigo
    field num-seq-medicao    like medicao-contrat.num-seq-medicao
    field ct-codigo          like matriz-rat-med.ct-codigo 
    field sc-codigo          like matriz-rat-med.sc-codigo 
    field cod-unid-negoc     like matriz-rat-med.cod-unid-negoc 
    field perc-rateioConta   like matriz-rat-med.perc-rateio.

DEFINE TEMP-TABLE tt-matriz-rat-med LIKE matriz-rat-med
    FIELD atualiza           AS LOGICAL.
    
define new global shared var w-rowid-ord-comp   AS Rowid          No-undo.    
    

define variable chWorksheet   as com-handle  no-undo.
define variable chExcel      as com-handle  no-undo.
define variable chWorkbook    as com-handle  no-undo.
DEFINE VARIABLE iContReg       AS INTEGER     NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR h-acomp AS HANDLE NO-UNDO.

DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.

def var c-permissao-1    as char    no-undo.
def var c-permissao-2    as char    no-undo.
DEF VAR c-permissao-3    AS CHAR    NO-UNDO.
DEF VAR c-permissao-4    AS CHAR    NO-UNDO.
DEF VAR l-permissao      AS LOGICAL NO-UNDO.
DEF BUFFER b-usuar-mater FOR usuar-mater.

{cdp/cd9731.i2} /*l-matriz-medicao*/

DEFINE BUFFER B-ordem-compra    FOR ordem-compra.
DEFINE BUFFER bf-matriz-rat-med FOR matriz-rat-med. 

DEFINE VARIABLE c-table-name AS CHARACTER  NO-UNDO.

/*DEFINE VARIABLE c-programa-mg97 AS CHARACTER   NO-UNDO.*/


PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-19 RECT-20 RECT-22 ~
rs-exporta-2 btInputFile cInputFile bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS rs-exporta rs-exporta-2 cInputFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok 
     LABEL "&Processar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btInputFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY .92.

DEFINE VARIABLE cInputFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 61.72 BY .88
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE rs-exporta AS CHARACTER INITIAL "I" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Exportar", "E",
"Importar", "I"
     SIZE 57 BY .75 NO-UNDO.

DEFINE VARIABLE rs-exporta-2 AS CHARACTER INITIAL "T" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Registro Atual", "A",
"Todos os Registros", "T"
     SIZE 61 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.75.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.5.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-exporta AT ROW 2.25 COL 5.14 NO-LABEL WIDGET-ID 10
     rs-exporta-2 AT ROW 4.5 COL 5 NO-LABEL WIDGET-ID 20
     btInputFile AT ROW 7.33 COL 64.57 HELP
          "Escolha do nome do arquivo" WIDGET-ID 2
     cInputFile AT ROW 7.38 COL 2.86 HELP
          "Nome do arquivo de entrada do relat¢rio" NO-LABEL WIDGET-ID 4
     bt-ok AT ROW 9.75 COL 3
     bt-cancela AT ROW 9.75 COL 14
     bt-ajuda AT ROW 9.75 COL 59
     "Resgitros" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.75 COL 6 WIDGET-ID 30
     "  Planilha Rateio Medi‡äes" VIEW-AS TEXT
          SIZE 19.43 BY .54 AT ROW 6.5 COL 4.57 WIDGET-ID 8
     "Matriz Excel" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 1.5 COL 6 WIDGET-ID 26
     rt-buttom AT ROW 9.5 COL 2
     RECT-19 AT ROW 6.88 COL 2 WIDGET-ID 6
     RECT-20 AT ROW 1.75 COL 2 WIDGET-ID 14
     RECT-22 AT ROW 4 COL 2 WIDGET-ID 28
     SPACE(0.28) SKIP(5.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "CN0302-u01 - Importa rateio Medi‡äes - 1.00.00.000"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET rs-exporta IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* CN0302-u01 - Importa rateio Medi‡äes - 1.00.00.000 */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* Processar */
DO:
    IF INPUT FRAME {&FRAME-NAME} rs-exporta = 'I' THEN
    do:
        IF INPUT FRAME {&FRAME-NAME} cInputFile = "" THEN
        DO:
            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006,
                              INPUT 'Arquivo nÆo informado.~~Favor informar um arquivo v lido.').
            RETURN NO-APPLY.
        END.

        FOR EACH contrato-for NO-LOCK,
            EACH ordem-compra NO-LOCK
            WHERE ROWID(ordem-compra) = w-rowid-ord-comp
            AND   ordem-compra.nr-contrato = contrato-for.nr-contrato,
            EACH medicao-contrat OF contrato-for NO-LOCK:

            {utp/ut-liter.i Modificar * r}
            ASSIGN c-permissao-1 = TRIM(RETURN-VALUE).
            {utp/ut-liter.i Medi‡äes * r}
            ASSIGN c-permissao-2 = TRIM(RETURN-VALUE).
            run cdp/cdapi181.p ( INPUT c-seg-usuario,
                                 INPUT 3,
                                 INPUT 2,
                                OUTPUT l-permissao).
    
            IF  NOT l-permissao THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 18728,
                                   INPUT c-permissao-1 + "~~" + c-permissao-2).
                RETURN NO-APPLY.
            END.
            /* Par³metro "Controle Contrato por CC" */
            FIND b-usuar-mater
                WHERE b-usuar-mater.cod-usuar = contrato-for.gestor-tecnico NO-LOCK NO-ERROR.
            /* Verifica se Gestor Tecnico eï valido */
            if  not avail b-usuar-mater then do:
                {utp/ut-field.i mgind contrato-for gestor-tecnico 1}
                run utp/ut-msgs.p (input "show":U, input 47, trim(return-value)).
                return "adm-error":U.
            end.
    
            IF  AVAIL usuar-mater   AND
                AVAIL b-usuar-mater THEN DO:
                IF  usuar-mater.cont-acesso-cc AND
                    usuar-mater.sc-codigo <> b-usuar-mater.sc-codigo THEN DO:
                    ASSIGN c-permissao-3 = string(c-permissao-1 + " " + c-permissao-2).
                    {utp/ut-liter.i de_outro_Centro_de_Custo * r}
                    ASSIGN c-permissao-4 = TRIM(RETURN-VALUE).
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 18728,
                                       INPUT c-permissao-3 + "~~":U + c-permissao-4).
                    RETURN NO-APPLY.
                END.
            END.
            /*-----*/
            /* Par³metro "Altera Contrato Outros" */
            IF  AVAIL usuar-mater   AND
                AVAIL b-usuar-mater AND
                usuar-mater.perm-alt-cont = NO AND
                usuar-mater.cod-usuario <> b-usuar-mater.cod-usuario THEN DO:
                ASSIGN c-permissao-3 = string(c-permissao-1 + " " + c-permissao-2).
                {utp/ut-liter.i de_outro_Usuÿrio * r}
                ASSIGN c-permissao-4 = TRIM(RETURN-VALUE).
    
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 18728,
                                   INPUT c-permissao-3 + "~~" + c-permissao-4).
                RETURN NO-APPLY.
            END.
            /*-----*/
            FIND FIRST item-contrat 
                 WHERE item-contrat.nr-contrato  = medicao-contrat.nr-contrato  
                   AND item-contrat.num-seq-item = medicao-contrat.num-seq-item 
                   /*AND (item-contrat.ind-tipo-control = 1 OR
                        item-contrat.ind-tipo-control = 2)*/
                   NO-LOCK NO-ERROR.
        
            IF  AVAIL item-contrat THEN DO:
                IF  item-contrat.ind-tipo-control = 2 AND /*ordem*/
                    l-rat-ord = NO THEN DO: /*rateio por ordem nao habilitado*/
                    RUN utp/ut-msgs.p (INPUT "show",
                                       INPUT 28582,
                                       INPUT "").
                    RETURN NO-APPLY.
                END.
                ELSE
                    IF  item-contrat.ind-tipo-control = 1 AND /*medicao*/
                        l-rat-med = NO THEN DO: /*rateio por medicao nao habilitado*/
                        RUN utp/ut-msgs.p (INPUT "show",
                                           INPUT 28583,
                                           INPUT "").
                        RETURN NO-APPLY.
                    END.
        
                    /* Unidade de Negocio Manutencao Industrial */
                    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                    IF item-contrat.ind-tipo-control = 2 then do:
                         find b-ordem-compra 
                             where b-ordem-compra.numero-ordem = evento-ped.numero-ordem no-lock no-error.
                         if avail b-ordem-compra 
                         AND b-ordem-compra.ordem-servic <> 0 THEN 
                         DO:
                            {utp/ut-table.i mgmnt ord-manut 1}
                    
                            ASSIGN c-table-name = RETURN-VALUE.
                            {utp/ut-table.i mgind b-ordem-compra 1}
                    
                            RUN utp/ut-msgs.p (INPUT "show",
                                               INPUT 34507,
                                               INPUT  RETURN-VALUE + "~~" + c-table-name).
                            RETURN NO-APPLY.
                        END.
                    END.
                    &ENDIF
                    /* Unidade de Negocio Manutencao Industrial */
        
        
                IF  CAN-FIND(item
                       WHERE item.it-codigo = item-contrat.it-codigo
                         AND item.tipo-contr <> 4) THEN DO:
                    RUN utp/ut-msgs.p (INPUT "show", /*tipo deve ser debito direto*/
                                       INPUT 28588,
                                       INPUT "").
                    RETURN NO-APPLY.
                END.
            END.
        
            IF  CAN-FIND(FIRST contrato-for
                         WHERE contrato-for.nr-contrato     = medicao-contrat.nr-contrato
                           AND contrato-for.ind-control-rec = 1 /*total da nota*/
                           AND l-rat-contr-tot = NO) THEN DO:   /*rateio total da nota nao marcado*/
        
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT 28584,
                                   INPUT "").
                RETURN NO-APPLY.
            END.
        
            /*
            IF  medicao-contrat.log-rec-medicao = YES OR
                medicao-contrat.sld-rec-medicao <> 0 THEN DO:
        
                /*Matriz nÆo pode ser alterada, pois jÿ possui recebimento*/
                RUN utp/ut-msgs.p(INPUT "show",
                                  INPUT 28369,
                                  INPUT "").
                RETURN NO-APPLY.
            END.
            */
        END.

        RUN pi-excel.
    END.
    ELSE DO:
        RUN pi-exporta.
    END.
    RUN utp/ut-msgs.p(INPUT 'show',
                      INPUT 15825,
                      INPUT 'Processo Finalizado~~Processo Finalizado.').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btInputFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btInputFile D-Dialog
ON CHOOSE OF btInputFile IN FRAME D-Dialog
DO:
    
    def var c-arq-conv  as char no-undo.
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.
    IF INPUT FRAME {&FRAME-NAME} rs-exporta = 'I' THEN
    do:
        assign c-arq-conv = replace(input frame {&frame-name} cInputFile, "/", "\").
        SYSTEM-DIALOG GET-FILE c-arq-conv
           FILTERS "*.xls*" "*.xls*",
                   "*.*" "*.*"
           DEFAULT-EXTENSION "xls*"
           INITIAL-DIR "spool" 
           MUST-EXIST
           USE-FILENAME
           UPDATE l-ok.
        if  l-ok = yes then do:
            assign cInputFile = replace(c-arq-conv, "\", "/").
            display cInputFile with frame {&frame-name}.
        end.
    END.
    ELSE DO:
        SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.xls*" "*.xls*",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "xls"
       INITIAL-DIR "spool"
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok  .
        if  l-ok = yes then do:
            assign cInputFile = replace(c-arq-conv, "\", "/").
            display cInputFile with frame {&frame-name}.
        end.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-exporta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-exporta D-Dialog
ON VALUE-CHANGED OF rs-exporta IN FRAME D-Dialog
DO:
  ASSIGN cInputFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  IF INPUT FRAME {&FRAME-NAME} rs-exporta = 'I' THEN
  do:
      ASSIGN  cInputFile:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  END.
  ELSE DO:
      ASSIGN  cInputFile:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY rs-exporta rs-exporta-2 cInputFile 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-19 RECT-20 RECT-22 rs-exporta-2 btInputFile cInputFile 
         bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

/*   {utp/ut9000.i "D99XX999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT VALID-HANDLE(h-acomp ) THEN
     run utp/ut-acomp.p persistent set h-acomp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel D-Dialog 
PROCEDURE pi-excel :
DEFINE VARIABLE i-cont          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-linha         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-aba           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-tot-aba       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-plan          AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE i-nr-contrato     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-cod-estabel     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i-numero-ordem    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-seq-comp        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-it-codigo       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i-num-seq-medicao AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-ct-codigo       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-sc-codigo       AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE tt-matriz-rat-med.
    EMPTY TEMP-TABLE tt-excel.

    IF NOT VALID-HANDLE(h-acomp ) THEN
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp (input "Processando arquivo, Aguarde...").

    CREATE "Excel.Application" chExcel NO-ERROR.
    
    c-plan      = SEARCH(INPUT FRAME {&FRAME-NAME} cInputFile).
    chWorkbook  = ChExcel:Workbooks:OPEN(c-plan).
    i-tot-aba   = ChWorkbook:Sheets:Count.
    
    chExcel:DisplayAlerts = FALSE.
    chExcel:VISIBLE       = NO.

    ASSIGN iContReg = 0.

    EMPTY TEMP-TABLE tt-excel.
    DO i-aba = 1 TO i-tot-aba:
    /*DO i-aba = 1 TO 3:*/
        chWorksheet = ChWorkbook:Sheets:ITEM(i-aba) NO-ERROR.

        RUN pi-inicializar IN h-acomp (input "Importando, Planilha " + STRING(i-aba) + " de " + STRING(i-tot-aba)).

        IF NOT ChWorkbook:Sheets:ITEM(i-aba):NAME BEGINS "ITEM" THEN NEXT.

        i-linha = 4.
        REPEAT:
            i-linha = i-linha + 1.
        
            IF chWorksheet:Range("A" + STRING(i-linha)):VALUE = ""  OR 
               chWorksheet:Range("A" + STRING(i-linha)):VALUE = ?   THEN LEAVE.
            
            RUN pi-acompanhar IN h-acomp (INPUT "Lendo Linha: " + STRING(i-linha) + " da Planinha: " + ChWorkbook:Sheets:ITEM(i-aba):NAME).

            ASSIGN
                i-nr-contrato     = INTEGER(ChWorkSheet:RANGE("A"       + STRING(i-linha)):VALUE)
                c-cod-estabel     = ChWorkSheet:RANGE("B"               + STRING(i-linha)):TEXT
                i-numero-ordem    = INTEGER(ChWorkSheet:RANGE("C"       + STRING(i-linha)):VALUE)
                i-seq-comp        = INTEGER(ChWorkSheet:RANGE("D"       + STRING(i-linha)):VALUE)
                c-it-codigo       = ChWorkSheet:RANGE("E"               + STRING(i-linha)):TEXT
                i-num-seq-medicao = INTEGER(ChWorkSheet:RANGE("F"       + STRING(i-linha)):VALUE)
                c-ct-codigo       = ChWorkSheet:RANGE("P"               + STRING(i-linha)):TEXT
                c-sc-codigo       = ChWorkSheet:RANGE("Q"               + STRING(i-linha)):TEXT.

            FIND FIRST tt-excel
                 WHERE tt-excel.nr-contrato      = i-nr-contrato    
                   and tt-excel.cod-estabel      = c-cod-estabel    
                   and tt-excel.numero-ordem     = i-numero-ordem   
                   and tt-excel.seq-comp         = i-seq-comp       
                   and tt-excel.it-codigo        = c-it-codigo      
                   and tt-excel.num-seq-medicao  = i-num-seq-medicao
                   and tt-excel.ct-codigo        = c-ct-codigo
                   and tt-excel.sc-codigo        = c-sc-codigo        NO-ERROR.   
            IF AVAIL tt-excel THEN
                   ASSIGN tt-excel.perc-rateioConta = tt-excel.perc-rateioConta + ROUND(DECIMAL(ChWorkSheet:RANGE("R" + STRING(i-linha)):VALUE), 2).
            ELSE
            DO:
                CREATE tt-excel.
                ASSIGN tt-excel.nr-contrato      = INTEGER(ChWorkSheet:RANGE("A"       + STRING(i-linha)):VALUE)
                       tt-excel.cod-estabel      = ChWorkSheet:RANGE("B"               + STRING(i-linha)):TEXT
                       tt-excel.numero-ordem     = INTEGER(ChWorkSheet:RANGE("C"       + STRING(i-linha)):VALUE)
                       tt-excel.seq-comp         = INTEGER(ChWorkSheet:RANGE("D"       + STRING(i-linha)):VALUE)
                       tt-excel.it-codigo        = ChWorkSheet:RANGE("E"               + STRING(i-linha)):TEXT
                       tt-excel.num-seq-medicao  = INTEGER(ChWorkSheet:RANGE("F"       + STRING(i-linha)):VALUE)
                       tt-excel.ct-codigo        = ChWorkSheet:RANGE("P"               + STRING(i-linha)):TEXT
                       tt-excel.sc-codigo        = ChWorkSheet:RANGE("Q"               + STRING(i-linha)):TEXT
                       tt-excel.cod-unid-negoc   = "00"
                       tt-excel.perc-rateioConta = ROUND(DECIMAL(ChWorkSheet:RANGE("R" + STRING(i-linha)):VALUE), 2).

                ASSIGN iContReg = iContReg + 1.
            END.
        END.
    END.

    IF TEMP-TABLE tt-excel:HAS-RECORDS THEN
    DO:   
       RUN pi-rateio.
       RUN pi-valida-rateio.
    END.

    IF VALID-HANDLE(ChExcel)     THEN ChExcel:ActiveWorkbook:CLOSE.
    IF VALID-HANDLE(ChExcel)     THEN ChExcel:QUIT.
    IF VALID-HANDLE(ChExcel)     THEN RELEASE OBJECT chExcel.      
    IF VALID-HANDLE(chWorkbook)  THEN RELEASE OBJECT chWorkbook.
    IF VALID-HANDLE(chWorksheet) THEN RELEASE OBJECT chWorksheet.

    RUN pi-finalizar IN h-acomp.

    IF CAN-FIND(FIRST tt-excel) THEN
       RUN pi-log.

    RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exporta D-Dialog 
PROCEDURE pi-exporta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    i-linha = 2.
    
    
    {utp/ut-liter.i exportando arquivo...  *}
    IF NOT VALID-HANDLE(h-acomp ) THEN
       run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input return-value).
    CREATE "Excel.Application" chexcel NO-ERROR.
    bl-excel:
    DO TRANSACTION ON ERROR UNDO, RETRY ON ENDKEY UNDO, RETRY ON STOP UNDO, RETRY:
        IF RETRY THEN DO:
            /* MESSAGE ERROR-STATUS:GET-MESSAGE(2) VIEW-AS ALERT-BOX ERROR. */
            if valid-handle(chexcel) then chexcel:ActiveWorkbook:CLOSE NO-ERROR.
            if valid-handle(chworkbook ) then RELEASE OBJECT chworkbook          NO-ERROR.       
            if valid-handle(chworksheet) then RELEASE OBJECT chworksheet         NO-ERROR.
            if valid-handle(chexcel) then RELEASE OBJECT chexcel  NO-ERROR.
            RUN pi-finalizar IN h-acomp.
            UNDO, RETURN NO-APPLY.
        END.
        
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE ERROR-STATUS:GET-MESSAGE(2) VIEW-AS ALERT-BOX ERROR. 
            if valid-handle(chexcel) then chexcel:ActiveWorkbook:CLOSE NO-ERROR.
            if valid-handle(chworkbook ) then RELEASE OBJECT chworkbook          NO-ERROR.       
            if valid-handle(chworksheet) then RELEASE OBJECT chworksheet         NO-ERROR.
            if valid-handle(chexcel) then RELEASE OBJECT chexcel  NO-ERROR.
            RUN pi-finalizar IN h-acomp.
            RETURN NO-APPLY.
        END.
        chexcel:VISIBLE = NO NO-ERROR.
        chexcel:DisplayAlerts = false NO-ERROR.
        
        ASSIGN chWorkbook = chexcel:Workbooks:ADD(search("modelo\matriz_rateio.xltx")).
        
        chWorksheet = ChWorkbook:Sheets:ITEM(1) NO-ERROR.
        IF INPUT FRAME {&FRAME-NAME} rs-exporta-2 = 'T' THEN
        do:
            FOR EACH contrato-for NO-LOCK,
                EACH ordem-compra NO-LOCK
                WHERE ordem-compra.nr-contrato = contrato-for.nr-contrato,
                EACH medicao-contrat OF contrato-for NO-LOCK,
                EACH matriz-rat-med OF medicao-contrat NO-LOCK.
                RUN pi-acompanhar IN h-acomp(INPUT "Contrato/Item " + STRING(contrato-for.nr-contrato) + "/" + STRING(medicao-contrat.it-codigo)).
                i-linha = i-linha + 1.
                ASSIGN ChWorkSheet:RANGE("A" + STRING(i-linha)):VALUE = ordem-compra.nr-contrato 
                       ChWorkSheet:RANGE("B" + STRING(i-linha)):value = ordem-compra.cod-estabel   
                       ChWorkSheet:RANGE("B" + STRING(i-linha)):NumberFormat = "@"
                       ChWorkSheet:RANGE("C" + STRING(i-linha)):value = ordem-compra.numero-ordem   
                       ChWorkSheet:RANGE("D" + STRING(i-linha)):value = ordem-compra.num-seq-item       
                       ChWorkSheet:RANGE("E" + STRING(i-linha)):value = ordem-compra.it-codigo      
                       ChWorkSheet:RANGE("e" + STRING(i-linha)):NumberFormat = "@"
                       ChWorkSheet:RANGE("F" + STRING(i-linha)):value = medicao-contrat.num-seq-medicao
                       ChWorkSheet:RANGE("G" + STRING(i-linha)):value = matriz-rat-med.ct-codigo       
                       ChWorkSheet:RANGE("G" + STRING(i-linha)):NumberFormat = "@"
                       ChWorkSheet:RANGE("H" + STRING(i-linha)):value = matriz-rat-med.sc-codigo       
                       ChWorkSheet:RANGE("H" + STRING(i-linha)):NumberFormat = "@"
                       ChWorkSheet:RANGE("I" + STRING(i-linha)):value = matriz-rat-med.cod-unid-negoc  
                       ChWorkSheet:RANGE("I" + STRING(i-linha)):NumberFormat = "@"
                       ChWorkSheet:RANGE("J" + STRING(i-linha)):value = matriz-rat-med.perc-rateio.    
                       .
                
            END.
        END.
        ELSE DO:
            
            FOR EACH ordem-compra NO-LOCK
                WHERE ROWID(ordem-compra) = w-rowid-ord-comp,
                EACH  contrato-for NO-LOCK
                WHERE contrato-for.nr-contrato = ordem-compra.nr-contrato,
                EACH medicao-contrat OF contrato-for NO-LOCK,
                EACH matriz-rat-med OF medicao-contrat NO-LOCK.
                RUN pi-acompanhar IN h-acomp(INPUT "Contrato/Item " + STRING(contrato-for.nr-contrato) + "/" + STRING(medicao-contrat.it-codigo)).
                i-linha = i-linha + 1.
                ASSIGN ChWorkSheet:RANGE("A" + STRING(i-linha)):VALUE = ordem-compra.nr-contrato    
                       ChWorkSheet:RANGE("B" + STRING(i-linha)):value = ordem-compra.cod-estabel   
                       ChWorkSheet:RANGE("C" + STRING(i-linha)):value = ordem-compra.numero-ordem   
                       ChWorkSheet:RANGE("D" + STRING(i-linha)):value = ordem-compra.num-seq-item       
                       ChWorkSheet:RANGE("E" + STRING(i-linha)):value = ordem-compra.it-codigo      
                       ChWorkSheet:RANGE("F" + STRING(i-linha)):value = medicao-contrat.num-seq-medicao
                       ChWorkSheet:RANGE("G" + STRING(i-linha)):value = matriz-rat-med.ct-codigo       
                       ChWorkSheet:RANGE("H" + STRING(i-linha)):value = matriz-rat-med.sc-codigo       
                       ChWorkSheet:RANGE("I" + STRING(i-linha)):value = matriz-rat-med.cod-unid-negoc  
                       ChWorkSheet:RANGE("J" + STRING(i-linha)):value = matriz-rat-med.perc-rateio.    
                       .
                
            END.
        END.
        chexcel:VISIBLE = YES NO-ERROR.
        
        RELEASE OBJECT chworkbook          NO-ERROR.       
        RELEASE OBJECT chworksheet         NO-ERROR.
        RELEASE OBJECT chexcel  NO-ERROR.
        
    END.
    
    RUN pi-finalizar IN h-acomp.

    RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-log D-Dialog 
PROCEDURE pi-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cArqLog AS CHARACTER   NO-UNDO.

ASSIGN cArqLog = SESSION:TEMP-DIRECTORY + "CN0302-LOG-IMPORT-" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".TXT".

OUTPUT TO VALUE(cArqLog) NO-CONVERT.
IF NOT CAN-FIND(FIRST tt-matriz-rat-med WHERE 
                      tt-matriz-rat-med.atualiza = NO) THEN
    PUT UNFORMATTED "IMPORTA€ÇO REALIZADA COM SUCESSO!" SKIP.
ELSE
DO:
   PUT UNFORMATTED "IMPORTA€åES CONCLUÖDAS COM ÒXITO, ABAIXO:" SKIP.

   IF CAN-FIND(FIRST tt-matriz-rat-med WHERE 
                     tt-matriz-rat-med.atualiza = YES) THEN
   DO:   
       FOR EACH tt-matriz-rat-med WHERE
                tt-matriz-rat-med.atualiza = YES:
    
           DISP tt-matriz-rat-med.nr-contrato     
                tt-matriz-rat-med.num-seq-item    
                tt-matriz-rat-med.numero-ordem    
                tt-matriz-rat-med.num-seq-event   
                tt-matriz-rat-med.num-seq-medicao 
                tt-matriz-rat-med.ct-codigo       
                tt-matriz-rat-med.sc-codigo       
                tt-matriz-rat-med.cod-unid-negoc
                tt-matriz-rat-med.perc-rateio 
               WITH SCROLLABLE.
       END.

       PUT UNFORMATTED SKIP(2).

   END.
    ELSE
       PUT UNFORMATTED "**** NENHUM REGISTRO IMPORTADO" SKIP(2).

   PUT UNFORMATTED "IMPORTA€åES COM ERRO POR EXCEDER LIMITE DE RATERIO, ABAIXO:" SKIP.

   FOR EACH tt-matriz-rat-med WHERE
            tt-matriz-rat-med.atualiza = NO:

           DISP tt-matriz-rat-med.nr-contrato     
                tt-matriz-rat-med.num-seq-item    
                tt-matriz-rat-med.numero-ordem    
                tt-matriz-rat-med.num-seq-event   
                tt-matriz-rat-med.num-seq-medicao 
                tt-matriz-rat-med.ct-codigo       
                tt-matriz-rat-med.sc-codigo       
                tt-matriz-rat-med.cod-unid-negoc
                tt-matriz-rat-med.perc-rateio 
               WITH SCROLLABLE.
   END.
END.    
OUTPUT CLOSE.

IF SEARCH(cArqLog) <> ? THEN
   RUN winexec (INPUT "notepad.exe" + CHR(32) + cArqLog, INPUT 1). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rateio D-Dialog 
PROCEDURE pi-rateio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    ASSIGN iCont = iContReg.

    EMPTY TEMP-TABLE tt-matriz-rat-med.

    RUN pi-inicializar IN h-acomp (input "Filtrando, Aguarde...").

    FOR EACH tt-excel:

        RUN pi-acompanhar IN h-acomp(INPUT "Restam: " + STRING(iCont)). 

        ASSIGN iCont = iCont - 1.

        FIND FIRST contrato-for WHERE
                   contrato-for.nr-contrato = tt-excel.nr-contrato NO-LOCK NO-ERROR.
        IF AVAIL contrato-for THEN
        DO:
            FOR EACH medicao-contrat OF contrato-for WHERE                                        
                     medicao-contrat.num-seq-item     = tt-excel.seq-comp        AND                     
                     medicao-contrat.num-seq-medicao  = tt-excel.num-seq-medicao NO-LOCK: 


                RUN pi-acompanhar IN h-acomp(INPUT "Restam: " + STRING(iCont) + "/" + string(medicao-contrat.nr-contrato    ) + "/" +  
                                                                                      string(medicao-contrat.num-seq-item   ) + "/" + 
                                                                                      string(medicao-contrat.numero-ordem   ) + "/" + 
                                                                                      string(medicao-contrat.num-seq-event  ) + "/" + 
                                                                                      string(medicao-contrat.num-seq-medicao)). 





                run cdp/cdapi181.p ( INPUT c-seg-usuario,
                                     INPUT 3,
                                     INPUT 2,
                                    OUTPUT l-permissao).
        
                IF  NOT l-permissao THEN DO:
                    NEXT.
                END.
                /* Par³metro "Controle Contrato por CC" */
                FIND b-usuar-mater
                    WHERE b-usuar-mater.cod-usuar = contrato-for.gestor-tecnico NO-LOCK NO-ERROR.
                /* Verifica se Gestor Tecnico eï valido */
                if  not avail b-usuar-mater then do:
                    NEXT.
                end.
        
                IF  AVAIL usuar-mater   AND
                    AVAIL b-usuar-mater THEN DO:
                    IF  usuar-mater.cont-acesso-cc AND
                        usuar-mater.sc-codigo <> b-usuar-mater.sc-codigo THEN DO:
                        NEXT.
                    END.
                END.
                /*-----*/
                /* Par³metro "Altera Contrato Outros" */
                IF  AVAIL usuar-mater   AND
                    AVAIL b-usuar-mater AND
                    usuar-mater.perm-alt-cont = NO AND
                    usuar-mater.cod-usuario <> b-usuar-mater.cod-usuario THEN DO:
                    NEXT.
                END.
                /*-----*/
            
                FIND FIRST item-contrat 
                     WHERE item-contrat.nr-contrato  = medicao-contrat.nr-contrato  
                       AND item-contrat.num-seq-item = tt-excel.seq-comp NO-LOCK NO-ERROR.            
                IF AVAIL item-contrat THEN 
                DO:            
                    IF  item-contrat.ind-tipo-control = 2 AND /*ordem*/
                        l-rat-ord = NO THEN DO: /*rateio por ordem nao habilitado*/
                        NEXT.
                    END.
                    ELSE
                        IF  item-contrat.ind-tipo-control = 1 AND /*medicao*/
                            l-rat-med = NO THEN DO: /*rateio por medicao nao habilitado*/
                            NEXT.
                        END.
            
                        /* Unidade de Negocio Manutencao Industrial */
                        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                        IF item-contrat.ind-tipo-control = 2 then do:
                             find ordem-compra 
                                 where ordem-compra.numero-ordem = evento-ped.numero-ordem no-lock no-error.
                             if avail ordem-compra 
                             AND ordem-compra.ordem-servic <> 0 THEN 
                             DO:
                                NEXT.
                            END.
                        END.
                        &ENDIF
                        /* Unidade de Negocio Manutencao Industrial */
            
            
                    IF  CAN-FIND(item
                           WHERE item.it-codigo = item-contrat.it-codigo
                             AND item.tipo-contr <> 4) THEN DO:
                        NEXT.
                    END.
                END.
            
                IF  CAN-FIND(FIRST contrato-for
                             WHERE contrato-for.nr-contrato     = medicao-contrat.nr-contrato
                               AND contrato-for.ind-control-rec = 1 /*total da nota*/
                               AND l-rat-contr-tot = NO) THEN DO:   /*rateio total da nota nao marcado*/
                    NEXT.
                END.
            
                IF  medicao-contrat.log-rec-medicao = YES OR
                    medicao-contrat.sld-rec-medicao <> 0 THEN DO:
            
                    NEXT.
                END.
                .
                FIND FIRST tt-matriz-rat-med EXCLUSIVE-LOCK
                     WHERE tt-matriz-rat-med.nr-contrato     = medicao-contrat.nr-contrato 
                     AND   tt-matriz-rat-med.num-seq-item    = medicao-contrat.num-seq-item 
                     AND   tt-matriz-rat-med.numero-ordem    = medicao-contrat.numero-ordem 
                     AND   tt-matriz-rat-med.num-seq-event   = medicao-contrat.num-seq-event 
                     AND   tt-matriz-rat-med.num-seq-medicao = medicao-contrat.num-seq-medicao
                     AND   tt-matriz-rat-med.ct-codigo       = tt-excel.ct-codigo     
                     AND   tt-matriz-rat-med.sc-codigo       = tt-excel.sc-codigo     
                     AND   tt-matriz-rat-med.cod-unid-negoc  = tt-excel.cod-unid-negoc NO-ERROR.
                IF NOT AVAIL tt-matriz-rat-med THEN 
                DO:
                    CREATE tt-matriz-rat-med.
                    ASSIGN tt-matriz-rat-med.nr-contrato     = medicao-contrat.nr-contrato 
                           tt-matriz-rat-med.num-seq-item    = medicao-contrat.num-seq-item 
                           tt-matriz-rat-med.numero-ordem    = medicao-contrat.numero-ordem 
                           tt-matriz-rat-med.num-seq-event   = medicao-contrat.num-seq-event 
                           tt-matriz-rat-med.num-seq-medicao = medicao-contrat.num-seq-medicao
                           tt-matriz-rat-med.ct-codigo       = tt-excel.ct-codigo     
                           tt-matriz-rat-med.sc-codigo       = tt-excel.sc-codigo     
                           tt-matriz-rat-med.cod-unid-negoc  = tt-excel.cod-unid-negoc
                           tt-matriz-rat-med.perc-rateio     = tt-excel.perc-rateioConta
                           tt-matriz-rat-med.atualiza        = NO. 
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-rateio D-Dialog 
PROCEDURE pi-valida-rateio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE de-perc-total     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-perc-total-AUX AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iContTot          AS INTEGER     NO-UNDO.

ASSIGN iContTot = iContReg.


RUN pi-inicializar IN h-acomp (input "Validando, Aguarde...").

FOR EACH tt-matriz-rat-med
    BREAK  BY tt-matriz-rat-med.nr-contrato       
           BY tt-matriz-rat-med.num-seq-item      
           BY tt-matriz-rat-med.numero-ordem      
           BY tt-matriz-rat-med.num-seq-event     
           BY tt-matriz-rat-med.num-seq-medicao:

        RUN pi-acompanhar IN h-acomp(INPUT "Restam: " + STRING(iContTot)). 
        ASSIGN iContTot = iContTot - 1.
                                     
/*     /* ZERO POR MEDI€ÇO DE CONTRATO */                                            */
/*     IF FIRST-OF(tt-matriz-rat-med.nr-contrato    ) AND                            */
/*        FIRST-OF(tt-matriz-rat-med.num-seq-item   ) AND                            */
/*        FIRST-OF(tt-matriz-rat-med.numero-ordem   ) AND                            */
/*        FIRST-OF(tt-matriz-rat-med.num-seq-event  ) AND                            */
/*        FIRST-OF(tt-matriz-rat-med.num-seq-medicao) THEN                           */
/*        ASSIGN de-perc-total-AUX = 0.                                              */
/*                                                                                   */
/*     /* TOTALIZA OS PERCENTUAIS DE RATEIO IMPORTADOS */                            */
/*     ASSIGN de-perc-total-AUX = de-perc-total-AUX + tt-matriz-rat-med.perc-rateio. */
/*                                                                                   */
/*     /* CASO SEJA SUPERIOR A 100% NÇO DEVE CONTINUAR */                            */
/*     IF de-perc-total-AUX > 100 THEN NEXT.                                         */

    /* VALIDA OS RATEIOS ATUALIZADOS */
    ASSIGN de-perc-total = 0.
    
    FOR EACH matriz-rat-med WHERE             
             matriz-rat-med.nr-contrato     = tt-matriz-rat-med.nr-contrato      AND
             matriz-rat-med.num-seq-item    = tt-matriz-rat-med.num-seq-item     AND
             matriz-rat-med.numero-ordem    = tt-matriz-rat-med.numero-ordem     AND
             matriz-rat-med.num-seq-event   = tt-matriz-rat-med.num-seq-event    AND
             matriz-rat-med.num-seq-medicao = tt-matriz-rat-med.num-seq-medicao  NO-LOCK:

        ASSIGN de-perc-total = de-perc-total + matriz-rat-med.perc-rateio.

    END.

    /* CASO Jµ TENHA O VALOR MµXIMO DE 100 DEVERµ IGNORAR OS PRàXIMOS */
    IF de-perc-total > 100 THEN NEXT.

    /* CASO O TOTAL DA IMPORTA€ÇO + O TOTAL DOS RATEIOS IMPORTADOS SEJA SUPERIOR A 100 DEVERµ IGNORAR */
    IF (tt-matriz-rat-med.perc-rateio + de-perc-total) > 100 THEN NEXT.

    /* SE CHEGAR NESTE PONTO PODERµ ATUALIZAR O REGISTRO */
    FIND FIRST bf-matriz-rat-med WHERE 
               bf-matriz-rat-med.nr-contrato     = tt-matriz-rat-med.nr-contrato     AND 
               bf-matriz-rat-med.num-seq-item    = tt-matriz-rat-med.num-seq-item    AND 
               bf-matriz-rat-med.numero-ordem    = tt-matriz-rat-med.numero-ordem    AND 
               bf-matriz-rat-med.num-seq-event   = tt-matriz-rat-med.num-seq-event   AND 
               bf-matriz-rat-med.num-seq-medicao = tt-matriz-rat-med.num-seq-medicao AND 
               bf-matriz-rat-med.ct-codigo       = tt-matriz-rat-med.ct-codigo       AND 
               bf-matriz-rat-med.sc-codigo       = tt-matriz-rat-med.sc-codigo       AND 
               bf-matriz-rat-med.cod-unid-negoc  = tt-matriz-rat-med.cod-unid-negoc  EXCLUSIVE-LOCK NO-ERROR.        
    IF NOT AVAIL bf-matriz-rat-med THEN 
    DO:
       CREATE bf-matriz-rat-med.
       ASSIGN bf-matriz-rat-med.nr-contrato     = tt-matriz-rat-med.nr-contrato     
              bf-matriz-rat-med.num-seq-item    = tt-matriz-rat-med.num-seq-item    
              bf-matriz-rat-med.numero-ordem    = tt-matriz-rat-med.numero-ordem    
              bf-matriz-rat-med.num-seq-event   = tt-matriz-rat-med.num-seq-event   
              bf-matriz-rat-med.num-seq-medicao = tt-matriz-rat-med.num-seq-medicao 
              bf-matriz-rat-med.ct-codigo       = tt-matriz-rat-med.ct-codigo       
              bf-matriz-rat-med.sc-codigo       = tt-matriz-rat-med.sc-codigo       
              bf-matriz-rat-med.cod-unid-negoc  = tt-matriz-rat-med.cod-unid-negoc.              
    END.

    ASSIGN bf-matriz-rat-med.perc-rateio     = bf-matriz-rat-med.perc-rateio + tt-matriz-rat-med.perc-rateio
           /* CASO ENTRE AQUI DEVERµ ATUALIZAR O REGISTRO PARA SABER QUE ESTE ITEM FOI IMPORTADO */
           tt-matriz-rat-med.atualiza     = YES.


    RELEASE bf-matriz-rat-med.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

