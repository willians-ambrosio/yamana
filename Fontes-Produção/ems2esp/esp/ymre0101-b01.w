&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems2cadme        PROGRESS
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
DEFINE BUFFER empresa FOR ems2cadme.empresa.
{include/i-prgvrs.i YMRE0101-B01 11.5.11.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMRE0101-B01 MUT}
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

/*:T v ri veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/*:T vari veis de uso local */
def var v-row-table  as rowid no-undo.
DEFINE VARIABLE c-estabel  AS CHARACTER   NO-UNDO FORMAT "x(5)".
DEFINE VARIABLE c-cfa      AS CHARACTER   NO-UNDO COLUMN-LABEL "CFA Item" FORMAT "x(40)".
DEFINE VARIABLE c-operacao AS CHARACTER   NO-UNDO COLUMN-LABEL "Tipo !Opera‡Æo" FORMAT "x(13)".
DEFINE VARIABLE c-ben-icms AS CHARACTER   NO-UNDO COLUMN-LABEL "Benef¡cio / Icentivo!ICMS" FORMAT "x(30)".
DEFINE VARIABLE c-ben-pisc AS CHARACTER   NO-UNDO COLUMN-LABEL "Benef¡cio / Icentivo!PIS/COFINS" FORMAT "x(30)".
DEFINE VARIABLE c-modelo   AS CHARACTER   NO-UNDO COLUMN-LABEL "Modelo Docto" FORMAT "x(8)".


/*:T fim das variaveis utilizadas no estilo */

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
&Scoped-define EXTERNAL-TABLES empresa
&Scoped-define FIRST-EXTERNAL-TABLE empresa


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR empresa.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-rec-atrib-natoper

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es-rec-atrib-natoper.identificador ~
f-estabel() @ c-estabel f-cfa() @ c-cfa f-oper () @ c-operacao ~
f-ben-icms() @ c-ben-icms f-ben-pisc() @ c-ben-pisc f-modelo() @ c-modelo ~
es-rec-atrib-natoper.cod-cfop-saida es-rec-atrib-natoper.dif-aliq-icm ~
es-rec-atrib-natoper.l-rateio es-rec-atrib-natoper.nat-operacao ~
es-rec-atrib-natoper.observacoes 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es-rec-atrib-natoper OF empresa WHERE ~{&KEY-PHRASE} ~
      AND es-rec-atrib-natoper.ep-codigo = i-ep-codigo-usuario NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es-rec-atrib-natoper OF empresa WHERE ~{&KEY-PHRASE} ~
      AND es-rec-atrib-natoper.ep-codigo = i-ep-codigo-usuario NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es-rec-atrib-natoper
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-rec-atrib-natoper


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-incluir bt-modificar bt-copiar ~
bt-eliminar 

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
ep-codigo||y|ems5_esp.es-rec-atrib-natoper.ep-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "ep-codigo"':U).

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-ben-icms B-table-Win 
FUNCTION f-ben-icms RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-ben-pisc B-table-Win 
FUNCTION f-ben-pisc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-cfa B-table-Win 
FUNCTION f-cfa RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-estabel B-table-Win 
FUNCTION f-estabel RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-modelo B-table-Win 
FUNCTION f-modelo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-oper B-table-Win 
FUNCTION f-oper RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-copiar 
     LABEL "&Copiar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-rec-atrib-natoper SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      es-rec-atrib-natoper.identificador COLUMN-LABEL "Id" FORMAT ">>>,>>>,>>9":U
            WIDTH 5.43
      f-estabel() @ c-estabel COLUMN-LABEL "Estab" FORMAT "x(5)":U
            WIDTH 3
      f-cfa() @ c-cfa COLUMN-LABEL "CFA Item" FORMAT "x(40)":U
      f-oper () @ c-operacao COLUMN-LABEL "Tipo !Opera‡Æo" FORMAT "x(13)":U
      f-ben-icms() @ c-ben-icms COLUMN-LABEL "Benef¡cio / Incentivo!ICMS" FORMAT "x(30)":U
      f-ben-pisc() @ c-ben-pisc COLUMN-LABEL "Benef¡cio / Incentivo!PIS/COFINS" FORMAT "x(30)":U
      f-modelo() @ c-modelo COLUMN-LABEL "Modelo Docto" FORMAT "x(8)":U
            WIDTH 8
      es-rec-atrib-natoper.cod-cfop-saida FORMAT "x(200)":U WIDTH 10
      es-rec-atrib-natoper.dif-aliq-icm FORMAT ">>9.99":U
      es-rec-atrib-natoper.l-rateio FORMAT "yes/no":U
      es-rec-atrib-natoper.nat-operacao COLUMN-LABEL "Nat Oper Entrada" FORMAT "x(06)":U
      es-rec-atrib-natoper.observacoes FORMAT "x(400)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 88 BY 12.42
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-incluir AT ROW 13.46 COL 1
     bt-modificar AT ROW 13.46 COL 11
     bt-copiar AT ROW 13.46 COL 21 WIDGET-ID 2
     bt-eliminar AT ROW 13.46 COL 31
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: ems2cadme.empresa
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
         HEIGHT             = 13.63
         WIDTH              = 88.14.
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

ASSIGN 
       br-table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "ems5_esp.es-rec-atrib-natoper OF ems2cadme.empresa"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ems5_esp.es-rec-atrib-natoper.ep-codigo = i-ep-codigo-usuario"
     _FldNameList[1]   > ems5_esp.es-rec-atrib-natoper.identificador
"ems5_esp.es-rec-atrib-natoper.identificador" "Id" ? "integer" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"f-estabel() @ c-estabel" "Estab" "x(5)" ? ? ? ? ? ? ? no "C¢digo Estabelecimento" no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"f-cfa() @ c-cfa" "CFA Item" "x(40)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"f-oper () @ c-operacao" "Tipo !Opera‡Æo" "x(13)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"f-ben-icms() @ c-ben-icms" "Benef¡cio / Incentivo!ICMS" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"f-ben-pisc() @ c-ben-pisc" "Benef¡cio / Incentivo!PIS/COFINS" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"f-modelo() @ c-modelo" "Modelo Docto" "x(8)" ? ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ems5_esp.es-rec-atrib-natoper.cod-cfop-saida
"ems5_esp.es-rec-atrib-natoper.cod-cfop-saida" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = ems5_esp.es-rec-atrib-natoper.dif-aliq-icm
     _FldNameList[10]   = ems5_esp.es-rec-atrib-natoper.l-rateio
     _FldNameList[11]   > ems5_esp.es-rec-atrib-natoper.nat-operacao
"ems5_esp.es-rec-atrib-natoper.nat-operacao" "Nat Oper Entrada" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = ems5_esp.es-rec-atrib-natoper.observacoes
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


&Scoped-define SELF-NAME bt-copiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-copiar B-table-Win
ON CHOOSE OF bt-copiar IN FRAME F-Main /* Copiar */
DO:
    Copiar: DO TRANSACTION ON ERROR UNDO Copiar, RETURN "ADM-ERROR":U:
        GET CURRENT {&BROWSE-NAME}.
    
        IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 15217, INPUT "":U).
    
            RUN dispatch IN THIS-PROCEDURE ("OPEN-QUERY":U).
    
            RETURN "ADM-ERROR":U.
        END.
        ELSE IF CURRENT-CHANGED {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
                RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 15216, INPUT "":U).
    
                BROWSE {&BROWSE-NAME}:REFRESH().
    
                RETURN "ADM-ERROR":U.
        END.
    
        RUN get-attribute IN THIS-PROCEDURE (INPUT "ProgIncMod":U).
        IF RETURN-VALUE <> ? AND RETURN-VALUE <> "":U THEN DO:
            ASSIGN v-row-table  = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}})
                   v-row-parent = ROWID({&FIRST-EXTERNAL-TABLE}).
    
            IF (v-row-table = ? ) OR  (v-row-parent = ?) THEN 
                RETURN "ADM-ERROR":U.
    
            RUN VALUE(RETURN-VALUE) PERSISTENT SET wh-programa
                    (INPUT-OUTPUT v-row-table,
                     INPUT v-row-parent,
                     INPUT THIS-PROCEDURE,
                     INPUT "COPIAR").
    
            IF NOT VALID-HANDLE(wh-programa) THEN RETURN "ADM-ERROR":U.
    
            RUN dispatch IN wh-programa (INPUT "INITIALIZE":U).
    
            IF NOT VALID-HANDLE(wh-programa) THEN RETURN "ADM-ERROR":U.
    
            RUN pi-reposiciona IN wh-programa.
            RUN pi-desabilita-bts IN wh-programa.
            RUN notify IN wh-programa (INPUT "COPY-RECORD":U).
    
            RUN pi-entry IN wh-programa.
    
            WAIT-FOR CLOSE OF wh-programa.
            IF RETURN-VALUE = "NOK":U THEN
                UNDO Copiar, RETURN "ADM-ERROR":u.
        END.
    END.
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
  {src/adm/template/row-list.i "empresa"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "empresa"}

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
  {src/adm/template/sndkycas.i "ep-codigo" "es-rec-atrib-natoper" "ep-codigo"}

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
  {src/adm/template/snd-list.i "empresa"}
  {src/adm/template/snd-list.i "es-rec-atrib-natoper"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-ben-icms B-table-Win 
FUNCTION f-ben-icms RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.nenhum-ben-icms THEN
            RETURN "Nenhum".
        ELSE DO:
            FOR FIRST es-beneficio
                WHERE es-beneficio.cod-beneficio = es-rec-atrib-natoper.cod-beneficio-icms NO-LOCK:
                RETURN STRING(es-beneficio.cod-beneficio) + "-" + es-beneficio.desc-beneficio.
            END.
        END.

    END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-ben-pisc B-table-Win 
FUNCTION f-ben-pisc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.nenhum-ben-piscof THEN
            RETURN "Nenhum".
        ELSE DO:
            FOR FIRST es-beneficio
                WHERE es-beneficio.cod-beneficio = es-rec-atrib-natoper.cod-beneficio-piscof NO-LOCK:
                RETURN STRING(es-beneficio.cod-beneficio) + "-" + es-beneficio.desc-beneficio.
            END.
        END.

    END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-cfa B-table-Win 
FUNCTION f-cfa RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.classe-todos THEN
            RETURN "*".
        ELSE DO:
            RETURN es-rec-atrib-natoper.classe.
/*             FOR FIRST es-cfa                                               */
/*                 WHERE es-cfa.classe = es-rec-atrib-natoper.classe NO-LOCK: */
/*                 RETURN es-cfa.classe + "-" + es-cfa.descricao.             */
/*             END.                                                           */
        END.

    END.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-estabel B-table-Win 
FUNCTION f-estabel RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.estab-todos THEN
            RETURN "*".
        ELSE
            RETURN es-rec-atrib-natoper.cod-estabel.

    END.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-modelo B-table-Win 
FUNCTION f-modelo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.model-todos THEN
            RETURN "*".
        ELSE
            RETURN es-rec-atrib-natoper.cod-model-nf-eletro.
    END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-oper B-table-Win 
FUNCTION f-oper RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF AVAIL es-rec-atrib-natoper THEN DO:
        IF es-rec-atrib-natoper.operacao = 'I' THEN
            RETURN "Interestadual".
        ELSE
            RETURN "Estadual".
    END.
  RETURN "".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

