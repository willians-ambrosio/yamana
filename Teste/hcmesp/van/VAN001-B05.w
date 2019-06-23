&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          hresp            PROGRESS
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
{include/i-prgvrs.i B99XX999 9.99.99.999}
{utp/ut-glob.i}
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

/* Parameters Definitions ---                                           */

DEFINE NEW GLOBAL SHARED VARIABLE v_rec_parcei_edi AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_cta_corren AS RECID NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-pesquisa AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-browse AS HANDLE NO-UNDO.
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE c-dir    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-cancel AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i        AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es_param_ws_van
&Scoped-define FIRST-EXTERNAL-TABLE es_param_ws_van


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_param_ws_van.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es_param_dir_van

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table es_param_dir_van.psdid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH es_param_dir_van OF es_param_ws_van WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH es_param_dir_van OF es_param_ws_van WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table es_param_dir_van
&Scoped-define FIRST-TABLE-IN-QUERY-br_table es_param_dir_van


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 br_table bt-add bt-mod 
&Scoped-Define DISPLAYED-OBJECTS f-psdid c_cta_corren l-ativo c_rem c_ret ~
c_ok c_erro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 f-psdid c_cta_corren l-ativo c_rem BUTTON-1 BUTTON-4 ~
c_ret c_ok BUTTON-6 c_erro BUTTON-7 bt-conf bt-can 
&Scoped-define List-2 f-psdid 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nomeserv||y|hresp.es_param_dir_van.nomeserv
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nomeserv,tipo,cdn_empresa"':U).

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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 13" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-can 
     IMAGE-UP FILE "image/im-canc2.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-conf 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Button 12" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 14" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "IMAGE/im-mod.bmp":U
     LABEL "Modifica" 
     SIZE 4 BY 1.13 TOOLTIP "Altera registro".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 4" 
     SIZE 4 BY .88.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 6" 
     SIZE 4 BY .88.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 7" 
     SIZE 4 BY .88.

DEFINE VARIABLE c_cta_corren AS CHARACTER FORMAT "X(30)":U 
     LABEL "Conta Corrente" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE c_erro AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Erro" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_ok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Sucesso" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_rem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Remessa" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_ret AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Retorno" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE f-psdid AS INTEGER FORMAT "999999" INITIAL 0 
     LABEL "PSDID" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 8.

DEFINE VARIABLE l-ativo AS LOGICAL INITIAL no 
     LABEL "Inativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      es_param_dir_van SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      es_param_dir_van.psdid FORMAT "999999":U WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 15.29 BY 6.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.5 COL 3
     f-psdid AT ROW 1.75 COL 29 COLON-ALIGNED WIDGET-ID 56
     c_cta_corren AT ROW 1.75 COL 52.86 COLON-ALIGNED WIDGET-ID 74
     l-ativo AT ROW 1.79 COL 81 WIDGET-ID 76
     c_rem AT ROW 2.79 COL 29 COLON-ALIGNED WIDGET-ID 2
     BUTTON-1 AT ROW 2.79 COL 91.29 WIDGET-ID 16
     BUTTON-4 AT ROW 3.79 COL 91.29 WIDGET-ID 22
     c_ret AT ROW 3.83 COL 29 COLON-ALIGNED WIDGET-ID 4
     c_ok AT ROW 4.83 COL 29 COLON-ALIGNED WIDGET-ID 12
     BUTTON-6 AT ROW 4.88 COL 91.29 WIDGET-ID 26
     c_erro AT ROW 5.88 COL 29 COLON-ALIGNED WIDGET-ID 14
     BUTTON-7 AT ROW 5.96 COL 91.29 WIDGET-ID 32
     bt-add AT ROW 7.25 COL 31 WIDGET-ID 42
     bt-mod AT ROW 7.25 COL 35.14 WIDGET-ID 46
     bt-conf AT ROW 7.25 COL 43 WIDGET-ID 40
     bt-can AT ROW 7.25 COL 47 WIDGET-ID 38
     bt-del AT ROW 7.25 COL 54 WIDGET-ID 44
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: hresp.es_param_ws_van
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.08
         WIDTH              = 97.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table RECT-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-can IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bt-conf IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-7 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_cta_corren IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_erro IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_ok IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_rem IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_ret IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN f-psdid IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR TOGGLE-BOX l-ativo IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "hresp.es_param_dir_van OF hresp.es_param_ws_van"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > hresp.es_param_dir_van.psdid
"es_param_dir_van.psdid" ? ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DOWN OF br_table IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
   
   IF AVAIL es_param_dir_van THEN DO:
      ASSIGN c_erro:SCREEN-VALUE                         IN FRAME {&FRAME-NAME} = es_param_dir_van.dir_erro 
             c_ok:SCREEN-VALUE                           IN FRAME {&FRAME-NAME} = es_param_dir_van.dir_sucesso  
             c_rem:SCREEN-VALUE                          IN FRAME {&FRAME-NAME} = es_param_dir_van.dir_remessa
             c_ret:SCREEN-VALUE                          IN FRAME {&FRAME-NAME} = es_param_dir_van.dir_retorno
             c_cta_corren:SCREEN-VALUE                   IN FRAME {&FRAME-NAME} = es_param_dir_van.cod_cta_corren
             f-psdid:SCREEN-VALUE                        IN FRAME {&FRAME-NAME} = STRING(es_param_dir_van.psdid).

     IF INPUT FRAME {&FRAME-NAME} f-psdid  = 0 THEN 
       DISABLE bt-mod 
               bt-del
               WITH FRAME {&FRAME-NAME}. 
     ELSE
       ENABLE bt-mod
              bt-del
              WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
     ASSIGN  c_erro:SCREEN-VALUE                         IN FRAME {&FRAME-NAME} = ""
             c_ok:SCREEN-VALUE                           IN FRAME {&FRAME-NAME} = ""
             c_rem:SCREEN-VALUE                          IN FRAME {&FRAME-NAME} = ""
             c_ret:SCREEN-VALUE                          IN FRAME {&FRAME-NAME} = ""
             f-psdid:SCREEN-VALUE                        IN FRAME {&FRAME-NAME} = "".

     DISABLE bt-mod 
             bt-del
             WITH FRAME {&FRAME-NAME}.

   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add B-table-Win
ON CHOOSE OF bt-add IN FRAME F-Main /* Button 13 */
DO:
  ENABLE {&list-1}
         {&list-2}
          f-psdid 
          WITH FRAME {&FRAME-NAME}.
  DISABLE bt-mod
          bt-del
          WITH FRAME {&FRAME-NAME}.

  ASSIGN c_erro:SCREEN-VALUE          = ""
         c_ok:SCREEN-VALUE            = ""
         c_rem:SCREEN-VALUE           = ""
         c_ret:SCREEN-VALUE           = ""
         f-psdid:SCREEN-VALUE         = ""
         c_cta_corren:SCREEN-VALUE    = "".
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can B-table-Win
ON CHOOSE OF bt-can IN FRAME F-Main /* Button 11 */
DO:
  DISABLE {&list-1} 
          {&list-2}
          f-psdid
          WITH FRAME {&FRAME-NAME}.
  
  ENABLE bt-mod 
         bt-add
         WITH FRAME {&FRAME-NAME}.

  APPLY "VALUE-CHANGED" TO br_table.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf B-table-Win
ON CHOOSE OF bt-conf IN FRAME F-Main /* Button 12 */
DO:

  RUN pi-validate.

  IF RETURN-VALUE = "NOK" THEN 
      RETURN NO-APPLY.

/*   IF NOT CAN-FIND(FIRST bf_es_param_dir_van WHERE */
/*                         bf_es_param_dir_van. THEN */
  FIND FIRST es_param_dir_van 
        WHERE es_param_dir_van.cdn_empresa = es_param_ws_van.cdn_empresa 
          AND es_param_dir_van.tipo        = es_param_ws_van.tipo
          AND es_param_dir_van.nomeserv    = es_param_ws_van.nomeserv
          AND es_param_dir_van.psdid       = input frame {&frame-name} f-psdid NO-ERROR.
  IF NOT AVAIL es_param_dir_van THEN DO:
      CREATE es_param_dir_van.
      ASSIGN es_param_dir_van.cdn_empresa     = es_param_ws_van.cdn_empresa        
               es_param_dir_van.tipo            = es_param_ws_van.tipo               
               es_param_dir_van.nomeserv        = es_param_ws_van.nomeserv           
               es_param_dir_van.psdid           = input frame {&frame-name} f-psdid.
  END.
  
  ASSIGN es_param_dir_van.cod_cta_corren  = input frame {&frame-name} c_cta_corren
         es_param_dir_van.dir_remessa     = input frame {&frame-name} c_rem
         es_param_dir_van.dir_retorno     = input frame {&frame-name} c_ret
         es_param_dir_van.dir_erro        = input frame {&frame-name} c_erro
         es_param_dir_van.dir_sucesso     = input frame {&frame-name} c_ok 
         es_param_dir_van.cod_cta_corren  = INPUT FRAME {&FRAME-NAME} c_cta_corren
         es_param_dir_van.LOG_ativo       = NOT l-ativo:CHECKED IN FRAME {&FRAME-NAME}
         es_param_dir_van.origem          = (IF CONNECTED("hresp") THEN 2 ELSE 1) /* 1-Finaneiro 2-Folha*/.                                       

  DISABLE {&list-1} 
          {&list-2}
          f-psdid 
          WITH FRAME {&FRAME-NAME}.
  ENABLE  bt-add    
          WITH FRAME {&FRAME-NAME}.

  {&OPEN-QUERY-{&BROWSE-NAME}}

  APPLY "VALUE-CHANGED" TO br_table.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main /* Button 14 */
DO:
   MESSAGE "Deseja relamente apagar o registro?"
       VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-cancel AS LOGICAL.

   IF l-cancel THEN DO:

     GET CURRENT br_table EXCLUSIVE-LOCK.

     IF AVAIL es_param_dir_van THEN DELETE es_param_dir_van.
    
     {&OPEN-QUERY-{&BROWSE-NAME}}
    
     APPLY "VALUE-CHANGED" TO br_table.

   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod B-table-Win
ON CHOOSE OF bt-mod IN FRAME F-Main /* Modifica */
DO:
  ENABLE  {&list-1}
          WITH FRAME {&FRAME-NAME}.
  DISABLE bt-add 
          bt-mod 
          WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 B-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  
  c-dir = INPUT FRAME {&FRAME-NAME} c_rem.
    
  SYSTEM-DIALOG GET-DIR c-dir
       TITLE "Diret¢rio Arquivos de Remessa".

  ASSIGN c_rem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 B-table-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
  
  c-dir = INPUT FRAME {&FRAME-NAME} c_ret.
    
  SYSTEM-DIALOG GET-DIR c-dir
       TITLE "Diret¢rio Arquivos de Retorno".

  ASSIGN c_ret:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 B-table-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Button 6 */
DO:

  c-dir = INPUT FRAME {&FRAME-NAME} c_ok.

  SYSTEM-DIALOG GET-DIR c-dir
       TITLE "Diret¢rio Arquivos de Sucesso".

  ASSIGN c_ok:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 B-table-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Button 7 */
DO:

  c-dir = INPUT FRAME {&FRAME-NAME} c_erro.

  SYSTEM-DIALOG GET-DIR c-dir
       TITLE "Diret¢rio Arquivos de Erro".

  ASSIGN c_erro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

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
  {src/adm/template/row-list.i "es_param_ws_van"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_param_ws_van"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bloquear B-table-Win 
PROCEDURE Bloquear :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM bloq AS INT. 
/* DEFINE VARIABLE c-nomeserv      AS CHARACTER   NO-UNDO. */
/* DEFINE VARIABLE c-identificador AS CHARACTER   NO-UNDO. */

   CASE bloq:
       WHEN 1 THEN DO:    

           ASSIGN c_erro:SCREEN-VALUE           IN FRAME {&FRAME-NAME} = ""
                  c_ok:SCREEN-VALUE             IN FRAME {&FRAME-NAME} = ""
                  c_rem:SCREEN-VALUE            IN FRAME {&FRAME-NAME} = ""
                  c_ret:SCREEN-VALUE            IN FRAME {&FRAME-NAME} = ""
                  f-psdid:SCREEN-VALUE          IN FRAME {&FRAME-NAME} = "".

           DO i = NUM-RESULTS(br_table:QUERY:NAME) TO 1 BY -1:
                br_table:SELECT-ROW(i).
                br_table:DELETE-CURRENT-ROW().
           END.

           DISABLE bt-add
                   bt-mod
                   bt-del
                   WITH  FRAME {&FRAME-NAME}.

       END.
       WHEN 2 THEN DO:

            ENABLE bt-add
                   WITH FRAME {&FRAME-NAME}.
            {&OPEN-QUERY-{&BROWSE-NAME}}

       END.
   END CASE.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

/*    APPLY "VALUE-CHANGED" TO {&browse-name} IN FRAME {&FRAME-NAME}. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .


 APPLY "VALUE-CHANGED" TO {&browse-name} IN FRAME {&FRAME-NAME}.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF INPUT FRAME {&FRAME-NAME} f-psdid = 0 THEN DO:
      MESSAGE "PSDID n∆o pode ser igual a zero!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.
  IF can-find(FIRST es_param_dir_van 
        WHERE es_param_dir_van.cdn_empresa = es_param_ws_van.cdn_empresa 
          AND es_param_dir_van.tipo        = es_param_ws_van.tipo
          AND es_param_dir_van.nomeserv    = es_param_ws_van.nomeserv
          AND es_param_dir_van.psdid       = input frame {&frame-name} f-psdid) 
      AND f-psdid:SENSITIVE IN FRAME {&FRAME-NAME} = YES THEN DO:
      MESSAGE "J† existe um registro com este PSDID!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.


  FILE-INFO:FILE-NAME = INPUT FRAME {&FRAME-NAME} c_rem.
  IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Diret¢rio ~"Dir. Remessa~" inv†lido."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.
  FILE-INFO:FILE-NAME = INPUT FRAME {&FRAME-NAME} c_ret.
  IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Diret¢rio ~"Dir. Retorno~" inv†lido."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.

  FILE-INFO:FILE-NAME = INPUT FRAME {&FRAME-NAME} c_ok.
  IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Diret¢rio ~"Dir. Sucesso~" inv†lido."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.
  FILE-INFO:FILE-NAME = INPUT FRAME {&FRAME-NAME} c_erro.
  IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      MESSAGE "Diret¢rio ~"Dir. Erro~" inv†lido."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "NOK".
  END.

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
  {src/adm/template/sndkycas.i "nomeserv" "es_param_dir_van" "nomeserv"}

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
  {src/adm/template/snd-list.i "es_param_ws_van"}
  {src/adm/template/snd-list.i "es_param_dir_van"}

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

/*    APPLY "VALUE-CHANGED" TO {&browse-name} IN FRAME {&FRAME-NAME}. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

