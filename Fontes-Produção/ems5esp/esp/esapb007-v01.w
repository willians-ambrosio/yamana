&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

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
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def new global shared var v_rec_fornecedor
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_espec_docto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def var v_num_order
    as integer
    format ">>>>,>>9":U
    label "Ordem"
    column-label "Ordem"
    no-undo.

def new global shared var v_rec_plano_ccusto
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.


def new global shared var v_rec_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

DEFINE VARIABLE c-lista-categoria AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-relat-cat
&Scoped-define FIRST-EXTERNAL-TABLE es-relat-cat


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-relat-cat.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-relat-cat.cod_empresa ~
es-relat-cat.cdn_fornecedor es-relat-cat.cod_espec_docto ~
es-relat-cat.cod_plano_ccusto es-relat-cat.cod_ccusto ~
es-relat-cat.cod_plano_cta_ctbl es-relat-cat.cod_cta_ctbl ~
es-relat-cat.cat-code 
&Scoped-define ENABLED-TABLES es-relat-cat
&Scoped-define FIRST-ENABLED-TABLE es-relat-cat
&Scoped-Define ENABLED-OBJECTS rt-key RECT-2 
&Scoped-Define DISPLAYED-FIELDS es-relat-cat.cod_empresa ~
es-relat-cat.cdn_fornecedor es-relat-cat.cod_espec_docto ~
es-relat-cat.cod_plano_ccusto es-relat-cat.cod_ccusto ~
es-relat-cat.cod_plano_cta_ctbl es-relat-cat.cod_cta_ctbl ~
es-relat-cat.cat-code 
&Scoped-define DISPLAYED-TABLES es-relat-cat
&Scoped-define FIRST-DISPLAYED-TABLE es-relat-cat
&Scoped-Define DISPLAYED-OBJECTS desc-empresa nom-fornece nom-espec ~
des-plan-cc des-cc des-plan-conta des-conta desc-cat-code cb-categoria-cat 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-relat-cat.cod_empresa ~
es-relat-cat.cdn_fornecedor es-relat-cat.cod_espec_docto ~
es-relat-cat.cod_plano_ccusto es-relat-cat.cod_ccusto ~
es-relat-cat.cod_plano_cta_ctbl es-relat-cat.cod_cta_ctbl ~
es-relat-cat.cat-code 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cat-code||y|ems5_esp.es-relat-cat.cat-code
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cat-code"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-zoom 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 7" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-2 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 2" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-3 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 2" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-4 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 2" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-5 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 2" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom-6 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "bt zoom 2" 
     SIZE 4 BY .88.

DEFINE BUTTON bt-zoom1 
     IMAGE-UP FILE "image/im-zoo":U
     IMAGE-INSENSITIVE FILE "image/ii-zoo":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE VARIABLE cb-categoria-cat AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Categoria" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "",0
     DROP-DOWN-LIST
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE des-cc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.86 BY .88 NO-UNDO.

DEFINE VARIABLE des-conta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE des-plan-cc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.86 BY .88 NO-UNDO.

DEFINE VARIABLE des-plan-conta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.86 BY .88 NO-UNDO.

DEFINE VARIABLE desc-cat-code AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 45.57 BY .88 NO-UNDO.

DEFINE VARIABLE desc-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48.86 BY .88 NO-UNDO.

DEFINE VARIABLE nom-espec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46.29 BY .88 NO-UNDO.

DEFINE VARIABLE nom-fornece AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.38.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 7.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-relat-cat.cod_empresa AT ROW 1.25 COL 22 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     bt-zoom AT ROW 1.25 COL 29.72 WIDGET-ID 48
     desc-empresa AT ROW 1.25 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     es-relat-cat.cdn_fornecedor AT ROW 2.25 COL 22 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .88
     bt-zoom1 AT ROW 2.25 COL 38 WIDGET-ID 32
     nom-fornece AT ROW 2.25 COL 40.29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     es-relat-cat.cod_espec_docto AT ROW 3.25 COL 22 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     bt-zoom-2 AT ROW 3.25 COL 31.43 WIDGET-ID 34
     nom-espec AT ROW 3.25 COL 33.72 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     es-relat-cat.cod_plano_ccusto AT ROW 4.25 COL 22 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     bt-zoom-3 AT ROW 4.25 COL 36.43 WIDGET-ID 44
     des-plan-cc AT ROW 4.25 COL 38.86 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     es-relat-cat.cod_ccusto AT ROW 5.25 COL 22 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 15.14 BY .88
     bt-zoom-4 AT ROW 5.25 COL 39.29 WIDGET-ID 46
     des-cc AT ROW 5.25 COL 41.57 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     es-relat-cat.cod_plano_cta_ctbl AT ROW 6.25 COL 22 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     bt-zoom-5 AT ROW 6.25 COL 36.29 WIDGET-ID 40
     des-plan-conta AT ROW 6.25 COL 38.72 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     es-relat-cat.cod_cta_ctbl AT ROW 7.25 COL 22 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     bt-zoom-6 AT ROW 7.25 COL 45.43 WIDGET-ID 42
     des-conta AT ROW 7.25 COL 47.72 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     es-relat-cat.cat-code AT ROW 8.75 COL 22 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     desc-cat-code AT ROW 8.75 COL 39.43 COLON-ALIGNED HELP
          "Desc Cat Code" NO-LABEL WIDGET-ID 50
     cb-categoria-cat AT ROW 9.75 COL 22 COLON-ALIGNED WIDGET-ID 52
     rt-key AT ROW 1 COL 1
     RECT-2 AT ROW 8.63 COL 1 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es-relat-cat
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 10
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-zoom IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom-3 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom-4 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom-5 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom-6 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-zoom1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-relat-cat.cat-code IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cb-categoria-cat IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-relat-cat.cdn_fornecedor IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_ccusto IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_cta_ctbl IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_empresa IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_espec_docto IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_plano_ccusto IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-relat-cat.cod_plano_cta_ctbl IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN des-cc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN des-conta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN des-plan-cc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN des-plan-conta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-cat-code IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nom-espec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nom-fornece IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-zoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom V-table-Win
ON CHOOSE OF bt-zoom IN FRAME f-main /* bt zoom 7 */
DO:

    run prgint/utb/utb069ka.p /*prg_sea_empresa*/.
    if  v_rec_empresa <> ?
    then do:
        find ems5.empresa where recid(empresa) = v_rec_empresa no-lock no-error.
        assign es-relat-cat.cod_empresa:screen-value in frame {&FRAME-NAME} = string(empresa.cod_empresa)
               desc-empresa:screen-value in frame {&FRAME-NAME}             = empresa.nom_razao_social.
                

    end /* if */.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-2 V-table-Win
ON CHOOSE OF bt-zoom-2 IN FRAME f-main /* bt zoom 2 */
DO:
    run prgint/utb/utb090ka.p /*prg_sea_espec_docto*/.
    if  v_rec_espec_docto <> ?
    then do:
        find ems5.espec_docto where recid(espec_docto) = v_rec_espec_docto no-lock no-error.
        assign es-relat-cat.cod_espec_docto:screen-value in frame {&FRAME-NAME} = string(espec_docto.cod_espec_docto)
               nom-espec:screen-value in frame {&FRAME-NAME}                    = espec_docto.des_espec_docto.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-3 V-table-Win
ON CHOOSE OF bt-zoom-3 IN FRAME f-main /* bt zoom 2 */
DO:
    assign v_num_order = 1. /* plano_ccusto*/
    run pi_ctbz_por_ccusto (Input 5) /*pi_ctbz_por_ccusto*/.

    apply "entry" to es-relat-cat.cod_plano_ccusto in frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-4 V-table-Win
ON CHOOSE OF bt-zoom-4 IN FRAME f-main /* bt zoom 2 */
DO:
    assign v_num_order = 2. /* ccusto*/
    run pi_ctbz_por_ccusto (Input 5) /*pi_ctbz_por_ccusto*/.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-5 V-table-Win
ON CHOOSE OF bt-zoom-5 IN FRAME f-main /* bt zoom 2 */
DO:
    run prgint/utb/utb080nd.p /*prg_see_plano_cta_ctbl_primario*/.
        if  v_rec_plano_cta_ctbl <> ?
        then do:
            find plano_cta_ctbl where recid(plano_cta_ctbl) = v_rec_plano_cta_ctbl no-lock no-error.
            assign es-relat-cat.cod_plano_cta_ctbl:screen-value in frame {&FRAME-NAME} = string(plano_cta_ctbl.cod_plano_cta_ctbl)
                   des-plan-conta:screen-value in frame {&FRAME-NAME} =   plano_cta_ctbl.des_tit_ctbl.
        end /* if */.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom-6 V-table-Win
ON CHOOSE OF bt-zoom-6 IN FRAME f-main /* bt zoom 2 */
DO:

    run prgint/utb/utb080nc.p /*prg_see_cta_ctbl_plano*/.
    if  v_rec_cta_ctbl <> ? then do:
        find cta_ctbl where recid(cta_ctbl) = v_rec_cta_ctbl no-lock no-error.
        ASSIGN es-relat-cat.cod_cta_ctbl:screen-value IN FRAME {&FRAME-NAME} = cta_ctbl.cod_cta_ctbl
               des-conta:screen-value IN FRAME {&FRAME-NAME} = string(cta_ctbl.des_tit_ctbl).
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom1 V-table-Win
ON CHOOSE OF bt-zoom1 IN FRAME f-main /* Button 1 */
DO:
    run prgint/ufn/ufn003nb.p (Input (INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa)) /*prg_see_fornecedor_financ_empresa*/.
    if  v_rec_fornecedor <> ?
    then do:
        find ems5.fornecedor where recid(fornecedor) = v_rec_fornecedor no-lock no-error.
        assign es-relat-cat.cdn_fornecedor:screen-value in frame {&FRAME-NAME}  = string(fornecedor.cdn_fornecedor)
               nom-fornece:screen-value in frame {&FRAME-NAME}                  = fornecedor.nom_abrev.

    end /* if */.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cat-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cat-code V-table-Win
ON LEAVE OF es-relat-cat.cat-code IN FRAME f-main /* Cat Code */
DO:
    FIND es-cat-code WHERE
         es-cat-code.cat-code = INPUT FRAME {&FRAME-NAME} es-relat-cat.cat-code NO-LOCK NO-ERROR.
    IF AVAIL es-cat-code THEN
        ASSIGN desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-cat-code.desc-cat-code.
    ELSE DO:

        RUN utp/ut-msgs.p(INPUT 'show',
                  INPUT 17006 ,
                  INPUT "Cat Code.~~O Cat Code Digitado n∆o existe, para cadastrar acesse o programa esapb006").
        APPLY "entry" TO es-relat-cat.cat-code IN FRAME {&FRAME-NAME}.

        ASSIGN desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

        RETURN "NOK":U.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cdn_fornecedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cdn_fornecedor V-table-Win
ON LEAVE OF es-relat-cat.cdn_fornecedor IN FRAME f-main /* Fornecedor */
DO:
    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> "" AND
       INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> ? THEN DO:
        FIND FIRST ems5.fornecedor WHERE
                        fornecedor.cod_empresa    = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa      AND 
                        fornecedor.cdn_fornecedor = INPUT FRAME {&FRAME-NAME} es-relat-cat.cdn_fornecedor NO-LOCK NO-ERROR.
        IF AVAIL fornecedor THEN
            assign es-relat-cat.cdn_fornecedor:screen-value in frame {&FRAME-NAME}  = string(fornecedor.cdn_fornecedor)
                   nom-fornece:screen-value in frame {&FRAME-NAME}                  = fornecedor.nom_abrev.
        ELSE DO:
            IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cdn_fornecedor <> 0 THEN DO:
                RUN utp/ut-msgs.p(INPUT 'show',
                                  INPUT 17006 ,
                                  INPUT "Fornecedor.~~C¢digo Fornecedor Digitado n∆o existe").
                APPLY "entry" TO es-relat-cat.cdn_fornecedor IN FRAME {&FRAME-NAME}.

                assign es-relat-cat.cdn_fornecedor:screen-value in frame {&FRAME-NAME}  = ""
                       nom-fornece:screen-value in frame {&FRAME-NAME}                  = "".

                RETURN "NOK":U.
            END.
        END.
    END.
    ELSE DO:
        assign es-relat-cat.cdn_fornecedor:screen-value in frame {&FRAME-NAME}  = ?
               nom-fornece:screen-value in frame {&FRAME-NAME}                  = "".
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_ccusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_ccusto V-table-Win
ON LEAVE OF es-relat-cat.cod_ccusto IN FRAME f-main /* Centro Custo */
DO:
    IF input frame {&frame-name} es-relat-cat.cod_ccusto <> ? THEN DO:
        find first ems5.ccusto no-lock where
                   ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa     and
                   ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto and
                   ccusto.cod_ccusto       = input frame {&frame-name} es-relat-cat.cod_ccusto use-index ccusto_id no-error.
        if  avail ccusto THEN
            ASSIGN es-relat-cat.cod_ccusto:screen-value in frame {&FRAME-NAME} = string(ccusto.cod_ccusto)
                   des-cc:screen-value in frame {&FRAME-NAME} = string(ccusto.des_tit_ctbl).
        ELSE
            ASSIGN es-relat-cat.cod_ccusto:screen-value in frame {&FRAME-NAME} = ""
                   des-cc:screen-value in frame {&FRAME-NAME} = "".
    END.
    ELSE 
        ASSIGN des-cc:screen-value in frame {&FRAME-NAME} = ?.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_cta_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_cta_ctbl V-table-Win
ON LEAVE OF es-relat-cat.cod_cta_ctbl IN FRAME f-main /* Conta Cont·bil */
DO:

    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_cta_ctbl <> ? THEN DO:
        find cta_ctbl no-lock where 
             cta_ctbl.cod_plano_cta_ctbl    = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl and
             cta_ctbl.cod_cta_ctbl          = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_cta_ctbl no-error.
        IF AVAIL cta_ctbl THEN
            ASSIGN des-conta:screen-value IN FRAME {&FRAME-NAME} = string(cta_ctbl.des_tit_ctbl).
        ELSE
            ASSIGN des-conta:screen-value IN FRAME {&FRAME-NAME} = "".
    END.
    ELSE
         ASSIGN des-conta:screen-value IN FRAME {&FRAME-NAME} = ?.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_empresa V-table-Win
ON LEAVE OF es-relat-cat.cod_empresa IN FRAME f-main /* Empresa */
DO:

    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> "" and
       INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> ? THEN DO:

        FIND ems5.empresa WHERE
                 empresa.cod_empresa = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa NO-LOCK NO-ERROR.
        IF AVAIL empresa THEN
            ASSIGN desc-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  empresa.nom_razao_social.
        ELSE DO:
    
            ASSIGN desc-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    
            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006 ,
                              INPUT "Empresa.~~C¢digo da Empresa Digitada n∆o existe").
            RETURN "NOK":U.
        END.    
    END.
    ELSE DO:
        ASSIGN desc-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               es-relat-cat.cdn_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_espec_docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_espec_docto V-table-Win
ON LEAVE OF es-relat-cat.cod_espec_docto IN FRAME f-main /* EspÇcie Documento */
DO:
    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto <> ? THEN DO:
        FIND ems5.espec_docto where 
                  espec_docto.cod_espec_docto = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto no-lock no-error.
        IF AVAIL espec_docto THEN
            assign es-relat-cat.cod_espec_docto:screen-value in frame {&FRAME-NAME} = string(espec_docto.cod_espec_docto)
                   nom-espec:screen-value in frame {&FRAME-NAME}                    = espec_docto.des_espec_docto.
        
        ELSE
            assign es-relat-cat.cod_espec_docto:screen-value in frame {&FRAME-NAME} = ""
                   nom-espec:screen-value in frame {&FRAME-NAME}                    = "".
    END.
    ELSE 
        ASSIGN nom-espec:screen-value in frame {&FRAME-NAME}                    = ?.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_plano_ccusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_plano_ccusto V-table-Win
ON LEAVE OF es-relat-cat.cod_plano_ccusto IN FRAME f-main /* Plano Centros Custo */
DO:

    IF input frame {&frame-name} es-relat-cat.cod_plano_ccusto <> ? THEN DO:

        find first plano_ccusto no-lock where
                   plano_ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa and
                   plano_ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto use-index plnccst_id no-error.
        if  avail plano_ccusto THEN
            assign es-relat-cat.cod_plano_ccusto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(plano_ccusto.cod_plano_ccusto)
                   des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(plano_ccusto.des_tit_ctbl).
        ELSE
            assign es-relat-cat.cod_plano_ccusto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                   des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
    ELSE 
        ASSIGN des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-relat-cat.cod_plano_cta_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-relat-cat.cod_plano_cta_ctbl V-table-Win
ON LEAVE OF es-relat-cat.cod_plano_cta_ctbl IN FRAME f-main /* Plano Contas */
DO:
    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl <> ?  THEN DO:
        find plano_cta_ctbl WHERE
             plano_cta_ctbl.cod_plano_cta   =  INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl no-lock no-error.
        IF AVAIL plano_Cta_ctbl THEN
            assign es-relat-cat.cod_plano_cta_ctbl:screen-value in frame {&FRAME-NAME} = string(plano_cta_ctbl.cod_plano_cta_ctbl)
                   des-plan-conta:screen-value in frame {&FRAME-NAME} =   plano_cta_ctbl.des_tit_ctbl.
        ELSE
            assign es-relat-cat.cod_plano_cta_ctbl:screen-value in frame {&FRAME-NAME} = ""
                   des-plan-conta:screen-value in frame {&FRAME-NAME} =   "".
    END.
    ELSE
        ASSIGN des-plan-conta:screen-value in frame {&FRAME-NAME} = ?.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "es-relat-cat"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-relat-cat"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    ASSIGN es-relat-cat.categoria-code = cb-categoria-cat:INPUT-VALUE in FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then DO:
        disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.

    &endif
    
    ASSIGN bt-zoom:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
           bt-zoom1:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           bt-zoom-2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-zoom-3:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-zoom-4:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-zoom-5:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-zoom-6:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    DISABLE cb-categoria-cat WITH FRAME {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    FIND ems5.empresa WHERE
             empresa.cod_empresa = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa NO-LOCK NO-ERROR.
    IF AVAIL empresa THEN
        ASSIGN desc-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  empresa.nom_razao_social.
    ELSE 
        ASSIGN desc-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


    FIND FIRST ems5.fornecedor WHERE
                    fornecedor.cod_empresa    = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa      AND 
                    fornecedor.cdn_fornecedor = INPUT FRAME {&FRAME-NAME} es-relat-cat.cdn_fornecedor NO-LOCK NO-ERROR.
    IF AVAIL fornecedor THEN
        assign nom-fornece:screen-value in frame {&FRAME-NAME}          =   fornecedor.nom_abrev.
    ELSE                                                                    
        ASSIGN nom-fornece:screen-value in frame {&FRAME-NAME}                 =   "".


    FIND ems5.espec_docto where 
              espec_docto.cod_espec_docto = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto no-lock no-error.
    IF AVAIL espec_docto THEN
        assign nom-espec:screen-value in frame {&FRAME-NAME}            = espec_docto.des_espec_docto.
    
    ELSE
        ASSIGN  nom-espec:screen-value in frame {&FRAME-NAME}                   =  "".


    find first plano_ccusto no-lock where
               plano_ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa and
               plano_ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto use-index plnccst_id no-error.
    if  avail plano_ccusto THEN
        assign des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(plano_ccusto.des_tit_ctbl).
    ELSE
        ASSIGN des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} =   "".


    find first ems5.ccusto no-lock where
                   ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa     and
                   ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto and
                   ccusto.cod_ccusto       = input frame {&frame-name} es-relat-cat.cod_ccusto use-index ccusto_id no-error.
    if  avail ccusto THEN
        ASSIGN des-cc:screen-value in frame {&FRAME-NAME} = string(ccusto.des_tit_ctbl).
    ELSE
        ASSIGN des-cc:screen-value in frame {&FRAME-NAME} = "".

    find plano_cta_ctbl WHERE
         plano_cta_ctbl.cod_plano_cta   =  INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl no-lock no-error.
    IF AVAIL plano_Cta_ctbl THEN
        assign des-plan-conta:screen-value in frame {&FRAME-NAME} =   plano_cta_ctbl.des_tit_ctbl.
    ELSE
        assign des-plan-conta:screen-value in frame {&FRAME-NAME} =     "".

    find cta_ctbl no-lock where 
         cta_ctbl.cod_plano_cta_ctbl    = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl and
         cta_ctbl.cod_cta_ctbl          = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_cta_ctbl no-error.
    IF AVAIL cta_ctbl THEN
        ASSIGN des-conta:screen-value IN FRAME {&FRAME-NAME} = string(cta_ctbl.des_tit_ctbl).
    ELSE
        ASSIGN des-conta:screen-value IN FRAME {&FRAME-NAME} = "".


    FIND es-cat-code WHERE
         es-cat-code.cat-code = INPUT FRAME {&FRAME-NAME} es-relat-cat.cat-code NO-LOCK NO-ERROR.
    IF AVAIL es-cat-code THEN
        ASSIGN desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-cat-code.desc-cat-code.
    ELSE
        ASSIGN desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL es-relat-cat then
        cb-categoria-cat:SCREEN-VALUE IN FRAME {&FRAME-NAME}= string(es-relat-cat.categoria-code) NO-ERROR.
    ELSE
        cb-categoria-cat:SCREEN-VALUE IN FRAME {&FRAME-NAME}= ''.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes THEN DO:
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.

    END.
    &endif
    
    IF es-relat-cat.cod_empresa:SENSITIVE IN frame {&frame-name} = YES THEN DO:

        ASSIGN bt-zoom:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom1:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom-2:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom-3:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom-4:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom-5:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               bt-zoom-6:SENSITIVE IN FRAME {&FRAME-NAME} = YES.


    END.
    
    
    ENABLE cb-categoria-cat WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */

    c-lista-categoria = ',0'.    
    FOR EACH es-cat-code-categoria NO-LOCK:
        c-lista-categoria = c-lista-categoria + "," +
             STRING(es-cat-code-categoria.categoria-code) + " - " + es-cat-code-categoria.des-categoria +
              "," + STRING(es-cat-code-categoria.categoria-code).
    END.

    cb-categoria-cat:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = c-lista-categoria.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */

    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> "" AND
       INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa <> ? THEN DO:

        FIND ems5.empresa WHERE
                  empresa.cod_empresa = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa NO-LOCK NO-ERROR.
        IF NOT AVAIL empresa THEN DO:

            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006 ,
                              INPUT "Empresa.~~C¢digo da Empresa Digitada n∆o existe").
            RETURN "NOK":U.
        END. 

        FIND FIRST ems5.fornecedor WHERE
                        fornecedor.cod_empresa    = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa      AND 
                        fornecedor.cdn_fornecedor = INPUT FRAME {&FRAME-NAME} es-relat-cat.cdn_fornecedor NO-LOCK NO-ERROR.
        IF NOT AVAIL fornecedor THEN DO:

            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006 ,
                              INPUT "Fornecedor.~~O c¢digo de fornecedor n∆o existe com a chave informada").
            RETURN "NOK":U.
        END.

        IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa       <> ? AND
           INPUT frame {&frame-name} es-relat-cat.cod_plano_ccusto  <> ? THEN DO:
            find first plano_ccusto no-lock where
                       plano_ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa and
                       plano_ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto use-index plnccst_id no-error.
            if NOT avail plano_ccusto THEN DO:
                RUN utp/ut-msgs.p(INPUT 'show',
                                  INPUT 17006 ,
                                  INPUT "Plano de Centro de Custo.~~O c¢digo do plano de Centro de Custo n∆o existe com a chave informada").
                APPLY "entry" TO INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto.
                RETURN "NOK":U.
            END.
        END.

        IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa       <> ? AND
           input frame {&frame-name} es-relat-cat.cod_plano_ccusto  <> ? AND
           input frame {&frame-name} es-relat-cat.cod_ccusto        <> ? THEN DO:

            find first ems5.ccusto no-lock where
                       ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa     and
                       ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plano_ccusto and
                       ccusto.cod_ccusto       = input frame {&frame-name} es-relat-cat.cod_ccusto use-index ccusto_id no-error.
            if NOT avail ccusto THEN DO:

                RUN utp/ut-msgs.p(INPUT 'show',
                                  INPUT 17006 ,
                                  INPUT "Centro de Custo.~~O Centro de Custo n∆o existe com a chave informada").
                APPLY "entry" TO INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto.
                RETURN "NOK":U.
            END.
        END.

    END.



    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto <> ? THEN DO:
        FIND ems5.espec_docto where 
                  espec_docto.cod_espec_docto = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto no-lock no-error.
        IF NOT AVAIL espec_docto THEN DO:
            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006 ,
                              INPUT "Fornecedor.~~O c¢digo de fornecedor n∆o existe com a chave informada").
            APPLY "entry" TO INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto.
            RETURN "NOK":U.
        END.
    END.

    IF INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl <> ? THEN DO:
        find plano_cta_ctbl WHERE
             plano_cta_ctbl.cod_plano_cta   =  INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_plano_cta_ctbl no-lock no-error.
        IF NOT AVAIL plano_Cta_ctbl THEN DO:

            RUN utp/ut-msgs.p(INPUT 'show',
                              INPUT 17006 ,
                              INPUT "Plano de Conta Cont†bil.~~O Plano de Conta Cont†bil n∆o existe com a chave informada").
            APPLY "entry" TO INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto.
            RETURN "NOK":U.
    
        END.
    END.

    FIND es-cat-code WHERE
         es-cat-code.cat-code = INPUT FRAME {&FRAME-NAME} es-relat-cat.cat-code NO-LOCK NO-ERROR.
    IF NOT AVAIL es-cat-code THEN DO:
        RUN utp/ut-msgs.p(INPUT 'show',
                          INPUT 17006 ,
                          INPUT "Code Cat.~~O Code Cat informado n∆o existe").
        APPLY "entry" TO INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_espec_docto.
        RETURN "NOK":U.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_ctbz_por_ccusto V-table-Win 
PROCEDURE pi_ctbz_por_ccusto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/************************ Parameter Definition Begin ************************/

    def Input param p_num_funcao
        as integer
        format ">>>>,>>9"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_format_1
        as character
        format "x(8)":U
        no-undo.
    def var v_ind_criter_distrib_ccusto
        as character
        format "X(15)":U
        initial "Utiliza Todos" /*l_utiliza_todos*/
        view-as combo-box
        list-items "Nío Utiliza","Utiliza Todos","Definidos"
         /*l_nao_utiliza*/ /*l_utiliza_todos*/ /*l_definidos*/
        inner-lines 5
        bgcolor 15 font 2
        label "Crit≤rio Dist CCusto"
        column-label "Crit≤rio Dist CCusto"
        no-undo.
    def var v_cod_plano_ccusto_aux           as character       no-undo. /*local*/
    def var v_log_return                     as logical         no-undo. /*local*/


    run pi_ctbz_por_ccusto1 (Input 5) /*pi_ctbz_por_ccusto1*/.

    if avail plano_ccusto then
        assign v_rec_plano_ccusto = recid(plano_ccusto).

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_ctbz_por_ccusto1 V-table-Win 
PROCEDURE pi_ctbz_por_ccusto1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def Input param p_num_funcao
        as integer
        format ">>>>,>>9"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_format_1
        as character
        format "x(8)":U
        no-undo.
    def var v_ind_criter_distrib_ccusto
        as character
        format "X(15)":U
        initial "Utiliza Todos" /*l_utiliza_todos*/
        view-as combo-box
        list-items "Nío Utiliza","Utiliza Todos","Definidos"
         /*l_nao_utiliza*/ /*l_utiliza_todos*/ /*l_definidos*/
        inner-lines 5
        bgcolor 15 font 2
        label "Crit≤rio Dist CCusto"
        column-label "Crit≤rio Dist CCusto"
        no-undo.
    def var v_log_return                     as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/


    &if 'f_adp_01_cta_grp_clien' = 'f_dep_01_cta_grp_clien' &then
        if  v_num_order = 1
        then do: /* plano_ccusto*/
            find first plano_ccusto no-lock 
                 where plano_ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa
                   and plano_ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plan_ccusto
                   use-index plnccst_id no-error.
            if  avail plano_ccusto
            then do:
                assign v_rec_plano_ccusto = recid(plano_ccusto).
                if  search("prgint/utb/utb083ia.r") = ? and search("prgint/utb/utb083ia.p") = ? then do:
                    if  v_cod_dwb_user begins 'es_' then
                        return "Programa executˇvel nío foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb083ia.p".
                    else do:
                        message "Programa executˇvel nío foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb083ia.p"
                               view-as alert-box error buttons ok.
                        return.
                    end.
                end.
                else
                    run prgint/utb/utb083ia.p /*prg_det_plano_ccusto*/.
            end /* if */.
            assign frame f_adp_01_cta_grp_clien:sensitive = yes.
        end.
        else if  v_num_order = 2
             then do: /* ccusto*/
            find first ccusto no-lock 
                 where ccusto.cod_empresa      = INPUT FRAME {&FRAME-NAME} es-relat-cat.cod_empresa
                   and ccusto.cod_plano_ccusto = input frame {&frame-name} es-relat-cat.cod_plan_ccusto
                   and ccusto.cod_ccusto       = input frame {&frame-name} es-relat-cat.cod_ccusto
                 use-index ccusto_id no-error.
            if  avail ccusto
            then do:
                assign v_rec_ccusto = recid(ccusto)
                       v_rec_plano_ccusto = recid(ccusto).
                if  search("prgint/utb/utb066ia.r") = ? and search("prgint/utb/utb066ia.p") = ? then do:
                    if  v_cod_dwb_user begins 'es_' then
                        return "Programa executˇvel nío foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb066ia.p".
                    else do:
                        message "Programa executˇvel nío foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb066ia.p"
                               view-as alert-box error buttons ok.
                        return.
                    end.
                end.
                else
                    run prgint/utb/utb066ia.p /*prg_det_ccusto*/.
            end.
            assign frame f_adp_01_cta_grp_clien:sensitive = yes.
        end.
        assign v_num_order = 0.
    &else
        if  v_num_order = 1
        then do: /* plano_ccusto*/
            
            run prgint/utb/utb083ka.p /*prg_sea_plano_ccusto*/.

            if  v_rec_plano_ccusto <> ?
            then do:
                find plano_ccusto where recid(plano_ccusto) = v_rec_plano_ccusto no-lock no-error.
                assign es-relat-cat.cod_plano_ccusto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(plano_ccusto.cod_plano_ccusto)
                       des-plan-cc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(plano_ccusto.des_tit_ctbl).
            end.
        end.
        else if  v_num_order = 2 then do: /* ccusto*/
            run prgint/utb/utb066ka.p /*prg_sea_ccusto*/.
            if  v_rec_ccusto <> ? then do:

                find ems5.ccusto where recid(ccusto) = v_rec_ccusto no-lock no-error.
                ASSIGN es-relat-cat.cod_ccusto:screen-value in frame {&FRAME-NAME} = string(ccusto.cod_ccusto)
                       des-cc:screen-value in frame {&FRAME-NAME} = string(ccusto.des_tit_ctbl).
            end.
        end.
        assign v_num_order = 0.
    &endif
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cat-code" "es-relat-cat" "cat-code"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-relat-cat"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

