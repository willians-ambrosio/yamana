&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*****************************************************************************
** Programa..............: esfnd001-b01
** Descricao.............: Parametro para Programas
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Hilton Borba
** Criado em.............: 08/10/2014
*****************************************************************************/
{include/i-prgvrs.i esfnd001-b01 5.06.00.000}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

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
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v†ri†veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid no-undo.

DEF TEMP-TABLE tt-es-param-prog LIKE es-param-prog.

/*:T fim das variaveis utilizadas no estilo */

DEF VAR hShowMsg AS HANDLE NO-UNDO.
{method/dbotterr.i}
DEF TEMP-TABLE tt-RowErrors LIKE RowErrors.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-param-prog-pai
&Scoped-define FIRST-EXTERNAL-TABLE es-param-prog-pai


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-param-prog-pai.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-param-prog

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es-param-prog.cod-param ~
es-param-prog.txt-valor 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es-param-prog OF es-param-prog-pai WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es-param-prog OF es-param-prog-pai WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es-param-prog
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-param-prog


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-incluir bt-modificar bt-eliminar ~
bt-salvar bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod_prog_dtsul||y|ems5_esp.es-param-prog.cod_prog_dtsul
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod_prog_dtsul"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-param-prog
    FIELDS(es-param-prog.cod-param
      es-param-prog.txt-valor) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      es-param-prog.cod-param FORMAT "x(100)":U WIDTH 25
      es-param-prog.txt-valor FORMAT "x(1000)":U WIDTH 200
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 66 BY 9.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-incluir AT ROW 10.75 COL 1
     bt-modificar AT ROW 10.75 COL 11
     bt-eliminar AT ROW 10.75 COL 21
     bt-salvar AT ROW 10.75 COL 44.43 WIDGET-ID 2
     bt-recuperar AT ROW 10.75 COL 55.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: ems5_esp.es-param-prog-pai
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.92
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "ems5_esp.es-param-prog OF ems5_esp.es-param-prog-pai"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > ems5_esp.es-param-prog.cod-param
"es-param-prog.cod-param" ? ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ems5_esp.es-param-prog.txt-valor
"es-param-prog.txt-valor" ? ? "character" ? ? ? ? ? ? no ? no no "200" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
   RUN pi-eliminar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
  RUN pi-Incmod ('incluir':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-Incmod ('modificar':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar B-table-Win
ON CHOOSE OF bt-recuperar IN FRAME F-Main /* Recuperar */
DO:

    DEF VAR l-ok               AS LOGICAL NO-UNDO.
    DEF VAR c-arq-digita       AS CHAR    NO-UNDO.
    DEF VAR c-lista            AS CHAR    NO-UNDO.

    DEF VAR i-seq-alteracao    AS INT  INITIAL 1         NO-UNDO.
    DEF VAR i-seq-inclusao     AS INT  INITIAL 10000     NO-UNDO.
    DEF VAR i-seq-existe       AS INT  INITIAL 20000     NO-UNDO.

    DEF VAR c-mensagem         AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-FILE c-arq-digita
    FILTERS "*.csv" "*.csv",
            "*.*" "*.*"
    DEFAULT-EXTENSION "*.csv"
    MUST-EXIST
    USE-FILENAME
    UPDATE l-ok.

    {method/showmessage.i1}

    IF l-ok THEN DO:

        EMPTY TEMP-TABLE tt-es-param-prog.
        EMPTY TEMP-TABLE RowErrors.
        EMPTY TEMP-TABLE tt-RowErrors.
        
        INPUT FROM VALUE(c-arq-digita) NO-ECHO.
        REPEAT: 
            CREATE tt-es-param-prog.
            IMPORT DELIMITER ";" tt-es-param-prog.
        END. /* REPEAT */
        INPUT CLOSE.

        FOR EACH tt-es-param-prog WHERE
            tt-es-param-prog.cod-param = "" EXCLUSIVE-LOCK:
            DELETE tt-es-param-prog.
        END.

        /* Verifica se os dados importados j† existem cadadastraos,
           Se sim, questiona se gostaria de atualizar o valor
           (sim, n∆o, cancelar) */
        FOR EACH tt-es-param-prog:
            FIND FIRST es-param-prog WHERE
                             es-param-prog.cod-param      = tt-es-param-prog.cod-param     
                         AND es-param-prog.cod_prog_dtsul = tt-es-param-prog.cod_prog_dtsul
                         AND es-param-prog.num-seq        = tt-es-param-prog.num-seq NO-LOCK NO-ERROR.
            IF AVAIL es-param-prog THEN DO:
                BUFFER-COMPARE tt-es-param-prog TO es-param-prog
                    SAVE RESULT IN c-lista.

                IF c-lista <> "" THEN DO:
                    CREATE RowErrors.
                    ASSIGN
                        RowErrors.ErrorSequence    = i-seq-alteracao
                        RowErrors.ErrorNumber      = 27979
                        RowErrors.ErrorDescription = "(ALTERAÄ«O) C¢d. Programa: " + string(tt-es-param-prog.cod_prog_dtsul) + " \ C¢d. ParÉmetro: " + STRING(tt-es-param-prog.cod-param) + " \ Sequància: " + STRING(tt-es-param-prog.num-seq).
                        RowErrors.ErrorHelp        = "Valor TEXTO Cadastrado: " + STRING(es-param-prog.txt-valor) + CHR(10)
                                                   + "Novo Valor TEXTO: " + STRING(tt-es-param-prog.txt-valor) + CHR(10)
                                                   + "Valor RAW Cadastrado: " + STRING(es-param-prog.raw-valor) + CHR(10)
                                                   + "Novo Valor RAW: " + STRING(tt-es-param-prog.raw-valor).
                        ASSIGN i-seq-alteracao = i-seq-alteracao + 1.
                END.
                ELSE DO:
                    CREATE RowErrors.
                    ASSIGN
                        RowErrors.ErrorSequence    = i-seq-existe
                        RowErrors.ErrorNumber      = 15825
                        RowErrors.ErrorDescription = "(Jµ CADASTRADO) C¢d. Programa: " + string(tt-es-param-prog.cod_prog_dtsul) + " \ C¢d. ParÉmetro: " + STRING(tt-es-param-prog.cod-param) + " \ Sequància: " + STRING(tt-es-param-prog.num-seq).
                        RowErrors.ErrorHelp        = "Valor TEXTO: " + STRING(tt-es-param-prog.txt-valor) + CHR(10)
                                                   + "Valor RAW: " + STRING(tt-es-param-prog.raw-valor).
                        ASSIGN i-seq-existe = i-seq-existe + 1.
                        DELETE tt-es-param-prog.
                END.
            END.
            ELSE DO:
                CREATE RowErrors.
                ASSIGN
                    RowErrors.ErrorSequence    = i-seq-inclusao
                    RowErrors.ErrorNumber      = 15825
                    RowErrors.ErrorDescription = "(INCLUS«O) C¢d. Programa: " + string(tt-es-param-prog.cod_prog_dtsul) + " \ C¢d. ParÉmetro: " + STRING(tt-es-param-prog.cod-param) + " \ Sequància: " + STRING(tt-es-param-prog.num-seq).
                    RowErrors.ErrorHelp        = "Valor TEXTO: " + STRING(tt-es-param-prog.txt-valor) + CHR(10)
                                               + "Valor RAW: " + STRING(tt-es-param-prog.raw-valor).
                    ASSIGN i-seq-inclusao = i-seq-inclusao + 1.
            END.
        END.

        /* Mostar a mensagem em ordem do campo ErrorSequence */
        FOR EACH RowErrors BY RowErrors.ErrorSequence:
            CREATE tt-RowErrors.
            BUFFER-COPY RowErrors TO tt-RowErrors.
            DELETE RowErrors.
        END.

        FOR EACH tt-RowErrors BY tt-RowErrors.ErrorSequence:
            CREATE RowErrors.
            BUFFER-COPY tt-RowErrors TO RowErrors.
            DELETE tt-RowErrors.
        END.

        /* Existe mensagem em tela */
        {method/showmessage.i2 &MODAL = YES}

        IF i-seq-alteracao <> 1 OR  i-seq-inclusao <> 10000 THEN DO:

            ASSIGN c-mensagem = "Ser∆o: " + CHR(10) + CHR(10).
            IF i-seq-alteracao <> 1 THEN
                ASSIGN c-mensagem = c-mensagem + "Atualizado(s) " + STRING(i-seq-alteracao - 1) + " registro(s)." + CHR(10).
            IF i-seq-inclusao <> 10000 THEN
                ASSIGN c-mensagem = c-mensagem + "Incluido(s) " + STRING(i-seq-inclusao - 10000) + " registro(s)." + CHR(10).
            ASSIGN c-mensagem = c-mensagem + CHR(10) + "Confirma movimentaá∆o?".

            MESSAGE
                c-mensagem
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                TITLE "ATENÄ«O" UPDATE l-update AS LOGICAL.

             IF l-update THEN DO:
                 FOR EACH tt-es-param-prog:

                     IF NOT CAN-FIND(FIRST es-param-prog-pai WHERE
                                     es-param-prog-pai.cod_prog_dtsul = tt-es-param-prog.cod_prog_dtsul) THEN DO:
                         CREATE es-param-prog-pai.
                         ASSIGN es-param-prog-pai.cod_prog_dtsul = tt-es-param-prog.cod_prog_dtsul.
                     END.
                     
                     /* Sen∆o exisitir registro cria, caso contrario somente atualiza */
                     FIND FIRST es-param-prog WHERE
                                      es-param-prog.cod-param      = tt-es-param-prog.cod-param     
                                  AND es-param-prog.cod_prog_dtsul = tt-es-param-prog.cod_prog_dtsul
                                  AND es-param-prog.num-seq        = tt-es-param-prog.num-seq NO-ERROR.
                     IF NOT AVAIL es-param-prog THEN DO:
                         CREATE es-param-prog.
                     END.

                     BUFFER-COPY tt-es-param-prog TO es-param-prog.
                     RELEASE es-param-prog.
                 END.
            END.

            IF l-update THEN
                {&OPEN-QUERY-br-table}
        END.
    END. /* IF l-ok */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar B-table-Win
ON CHOOSE OF bt-salvar IN FRAME F-Main /* Salvar */
DO:

    DEF VAR l-ok               AS LOGICAL NO-UNDO.
    DEF VAR c-arq-digita       AS CHAR    NO-UNDO.
    DEF BUFFER b-es-param-prog FOR es-param-prog.

    IF NOT AVAIL(es-param-prog-pai) THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, INPUT 17006, INPUT "Erro~~Registro C¢digo do Programa n∆o esta dispon°vel.").
        RETURN NO-APPLY.
    END.

    SYSTEM-DIALOG GET-FILE c-arq-digita
        FILTERS "*.csv" "*.csv",
                "*.*" "*.*"
        ASK-OVERWRITE 
        DEFAULT-EXTENSION "*.csv"
        SAVE-AS             
        CREATE-TEST-FILE
        USE-FILENAME
        UPDATE l-ok.

    IF l-ok THEN DO:
        OUTPUT TO VALUE(c-arq-digita).

        FOR EACH b-es-param-prog OF es-param-prog-pai NO-LOCK:
           EXPORT DELIMITER ";" b-es-param-prog.
        END. /* FOR EACH tt-digita */
    
        OUTPUT CLOSE.
    END. /* IF l-ok */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-param-prog-pai"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-param-prog-pai"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod_prog_dtsul" "es-param-prog" "cod_prog_dtsul"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-param-prog-pai"}
  {src/adm/template/snd-list.i "es-param-prog"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

