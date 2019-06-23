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
&Scoped-define EXTERNAL-TABLES es-natoper-rec
&Scoped-define FIRST-EXTERNAL-TABLE es-natoper-rec


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-natoper-rec.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-natoper-rec.nat-operacao-est ~
es-natoper-rec.nat-operacao-int es-natoper-rec.nat-operacao-rateio ~
es-natoper-rec.nat-operacao-rateio-int es-natoper-rec.exc-transf ~
es-natoper-rec.exc-especie-doc es-natoper-rec.l-op-terceiros ~
es-natoper-rec.exc-mercado es-natoper-rec.l-nota-propria ~
es-natoper-rec.red-cst-icms es-natoper-rec.cod-cfa-aplic ~
es-natoper-rec.excec-difal-cfop es-natoper-rec.cod-benef-icms-rec ~
es-natoper-rec.cod-benef-pis-cofins-rec es-natoper-rec.benef-cst-icms ~
es-natoper-rec.benef-cst-pis-cofins 
&Scoped-define ENABLED-TABLES es-natoper-rec
&Scoped-define FIRST-ENABLED-TABLE es-natoper-rec
&Scoped-Define ENABLED-OBJECTS rt-key rt-key-2 rt-key-3 rt-key-4 rt-key-5 ~
rt-key-7 RECT-1 rt-key-6 
&Scoped-Define DISPLAYED-FIELDS es-natoper-rec.nat-operacao-est ~
es-natoper-rec.nat-operacao-int es-natoper-rec.nat-operacao-rateio ~
es-natoper-rec.nat-operacao-rateio-int es-natoper-rec.exc-transf ~
es-natoper-rec.exc-especie-doc es-natoper-rec.l-op-terceiros ~
es-natoper-rec.exc-mercado es-natoper-rec.l-nota-propria ~
es-natoper-rec.red-cst-icms es-natoper-rec.cod-cfa-aplic ~
es-natoper-rec.excec-difal-cfop es-natoper-rec.cod-benef-icms-rec ~
es-natoper-rec.cod-benef-pis-cofins-rec es-natoper-rec.benef-cst-icms ~
es-natoper-rec.benef-cst-pis-cofins 
&Scoped-define DISPLAYED-TABLES es-natoper-rec
&Scoped-define FIRST-DISPLAYED-TABLE es-natoper-rec
&Scoped-Define DISPLAYED-OBJECTS desc-estadual desc-interestadual ~
desc-rateio desc-rateio-int fi-desc-benef-rec-icms fi-desc-benef-rec-pis 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE desc-estadual AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.14 BY .88 NO-UNDO.

DEFINE VARIABLE desc-interestadual AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.14 BY .88 NO-UNDO.

DEFINE VARIABLE desc-rateio AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.14 BY .88 NO-UNDO.

DEFINE VARIABLE desc-rateio-int AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-benef-rec-icms AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-benef-rec-pis AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.25.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.33.

DEFINE RECTANGLE rt-key-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.25.

DEFINE RECTANGLE rt-key-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.88.

DEFINE RECTANGLE rt-key-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.83.

DEFINE RECTANGLE rt-key-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.43 BY 2.83.

DEFINE RECTANGLE rt-key-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.58.

DEFINE RECTANGLE rt-key-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-natoper-rec.nat-operacao-est AT ROW 1.92 COL 9.43 COLON-ALIGNED WIDGET-ID 2
          LABEL "Estadual"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     desc-estadual AT ROW 1.92 COL 19.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     es-natoper-rec.nat-operacao-int AT ROW 2.92 COL 9.43 COLON-ALIGNED WIDGET-ID 4
          LABEL "Interestadual"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     desc-interestadual AT ROW 2.92 COL 19.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     es-natoper-rec.nat-operacao-rateio AT ROW 4.96 COL 9.43 COLON-ALIGNED WIDGET-ID 70
          LABEL "Estadual"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     desc-rateio AT ROW 4.96 COL 19.86 COLON-ALIGNED NO-LABEL WIDGET-ID 72
     es-natoper-rec.nat-operacao-rateio-int AT ROW 5.96 COL 9.43 COLON-ALIGNED WIDGET-ID 86
          LABEL "Interestadual"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     desc-rateio-int AT ROW 5.96 COL 19.86 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     es-natoper-rec.exc-transf AT ROW 7.88 COL 18 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Sim",Yes,
                     "N∆o",no
          DROP-DOWN-LIST
          SIZE 16 BY 1
     es-natoper-rec.exc-especie-doc AT ROW 7.88 COL 65.86 COLON-ALIGNED WIDGET-ID 16
          LABEL "Esp Doc Devoluá∆o"
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     es-natoper-rec.l-op-terceiros AT ROW 8 COL 37 WIDGET-ID 56
          LABEL "Op. de Terceiros"
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .83
     es-natoper-rec.exc-mercado AT ROW 8.88 COL 18 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Interno",1,
                     "Externo",2,
                     "Diversos",3
          DROP-DOWN-LIST
          SIZE 16 BY 1
     es-natoper-rec.l-nota-propria AT ROW 9 COL 37 WIDGET-ID 58
          LABEL "Nota Propria"
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es-natoper-rec.red-cst-icms AT ROW 10.88 COL 6.86 WIDGET-ID 68
          LABEL "ICMS"
          VIEW-AS FILL-IN 
          SIZE 63.57 BY .79
     es-natoper-rec.cod-cfa-aplic AT ROW 13.08 COL 9.29 COLON-ALIGNED WIDGET-ID 78
          LABEL "CFA Aplic."
          VIEW-AS FILL-IN 
          SIZE 63.57 BY .79
     es-natoper-rec.excec-difal-cfop AT ROW 15.71 COL 22 COLON-ALIGNED WIDGET-ID 42
          LABEL "CFOPs Interest. n∆o aplic†veis" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 51 BY .88
     es-natoper-rec.cod-benef-icms-rec AT ROW 17.83 COL 11 COLON-ALIGNED WIDGET-ID 38
          LABEL "Beneficio Rec"
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .88
     fi-desc-benef-rec-icms AT ROW 17.83 COL 14.43 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     es-natoper-rec.cod-benef-pis-cofins-rec AT ROW 17.83 COL 48 COLON-ALIGNED WIDGET-ID 40
          LABEL "Beneficio Rec"
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .88
     fi-desc-benef-rec-pis AT ROW 17.83 COL 51.43 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     es-natoper-rec.benef-cst-icms AT ROW 18.83 COL 11 COLON-ALIGNED WIDGET-ID 34
          LABEL "CST"
          VIEW-AS FILL-IN 
          SIZE 24 BY .88
     es-natoper-rec.benef-cst-pis-cofins AT ROW 18.83 COL 48 COLON-ALIGNED WIDGET-ID 36
          LABEL "CST"
          VIEW-AS FILL-IN 
          SIZE 25 BY .88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     "Benef°cios Recebimento PIS / COFINS" VIEW-AS TEXT
          SIZE 29 BY .54 AT ROW 17 COL 39 WIDGET-ID 50
     "Reduá∆o ICMS" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 10.08 COL 1.57 WIDGET-ID 62
     "Permitir utilizar naturezas com estas exceá‰es:" VIEW-AS TEXT
          SIZE 33.72 BY .54 AT ROW 7.33 COL 1.29 WIDGET-ID 14
     "Benef°cios Recebimento ICMS" VIEW-AS TEXT
          SIZE 21.57 BY .54 AT ROW 17 COL 1.43 WIDGET-ID 46
     "Difal ICMS" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 12.33 COL 2 WIDGET-ID 76
     "Natureza de Operaá∆o Placebo (Tempor†ria):" VIEW-AS TEXT
          SIZE 31.72 BY .54 AT ROW 1.04 COL 1.29 WIDGET-ID 10
     "Exceá‰es DIFAL ICMS" VIEW-AS TEXT
          SIZE 21.57 BY .54 AT ROW 14.83 COL 1.43 WIDGET-ID 30
     "Natureza de Operaá∆o Rateio (Tempor†ria):" VIEW-AS TEXT
          SIZE 31.72 BY .54 AT ROW 4.04 COL 1.29 WIDGET-ID 82
     rt-key AT ROW 1.63 COL 1
     rt-key-2 AT ROW 7.71 COL 1 WIDGET-ID 12
     rt-key-3 AT ROW 15.08 COL 1 WIDGET-ID 28
     rt-key-4 AT ROW 17.25 COL 1 WIDGET-ID 44
     rt-key-5 AT ROW 17.25 COL 38.57 WIDGET-ID 48
     rt-key-7 AT ROW 10.42 COL 1 WIDGET-ID 66
     RECT-1 AT ROW 12.5 COL 1 WIDGET-ID 74
     rt-key-6 AT ROW 4.63 COL 1 WIDGET-ID 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es-natoper-rec
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
         HEIGHT             = 19.17
         WIDTH              = 75.29.
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

/* SETTINGS FOR FILL-IN es-natoper-rec.benef-cst-icms IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.benef-cst-pis-cofins IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.cod-benef-icms-rec IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.cod-benef-pis-cofins-rec IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.cod-cfa-aplic IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN desc-estadual IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-interestadual IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-rateio IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-rateio-int IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.exc-especie-doc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.excec-difal-cfop IN FRAME f-main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi-desc-benef-rec-icms IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-benef-rec-pis IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX es-natoper-rec.l-nota-propria IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-natoper-rec.l-op-terceiros IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.nat-operacao-est IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.nat-operacao-int IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.nat-operacao-rateio IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.nat-operacao-rateio-int IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-natoper-rec.red-cst-icms IN FRAME f-main
   ALIGN-L EXP-LABEL                                                    */
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

&Scoped-define SELF-NAME es-natoper-rec.cod-benef-icms-rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-icms-rec V-table-Win
ON F5 OF es-natoper-rec.cod-benef-icms-rec IN FRAME f-main /* Beneficio Rec */
DO:
  {include/zoomvar.i &prog-zoom=esp\ymof0103-p01.w
                     &campo=es-natoper-rec.cod-benef-icms-rec
                     &campozoom=cod-beneficio}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-icms-rec V-table-Win
ON LEAVE OF es-natoper-rec.cod-benef-icms-rec IN FRAME f-main /* Beneficio Rec */
DO:
    FIND FIRST es-beneficio NO-LOCK
        WHERE es-beneficio.cod-beneficio = INTEGER (SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    IF AVAILABLE es-beneficio THEN
        ASSIGN fi-desc-benef-rec-icms:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-beneficio.desc-beneficio.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-icms-rec V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.cod-benef-icms-rec IN FRAME f-main /* Beneficio Rec */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-natoper-rec.cod-benef-pis-cofins-rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-pis-cofins-rec V-table-Win
ON F5 OF es-natoper-rec.cod-benef-pis-cofins-rec IN FRAME f-main /* Beneficio Rec */
DO:
    {include/zoomvar.i &prog-zoom=esp\ymof0103-p01.w
                       &campo=es-natoper-rec.cod-benef-pis-cofins-rec
                       &campozoom=cod-beneficio}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-pis-cofins-rec V-table-Win
ON LEAVE OF es-natoper-rec.cod-benef-pis-cofins-rec IN FRAME f-main /* Beneficio Rec */
DO:
    FIND FIRST es-beneficio NO-LOCK
        WHERE es-beneficio.cod-beneficio = INTEGER (SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
    IF AVAILABLE es-beneficio THEN
        ASSIGN fi-desc-benef-rec-pis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-beneficio.desc-beneficio.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.cod-benef-pis-cofins-rec V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.cod-benef-pis-cofins-rec IN FRAME f-main /* Beneficio Rec */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-natoper-rec.nat-operacao-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-est V-table-Win
ON F5 OF es-natoper-rec.nat-operacao-est IN FRAME f-main /* Estadual */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=es-natoper-rec.nat-operacao-est
                       &campozoom=nat-operacao
                       &campo2=desc-estadual
                       &campozoom2=denominacao }
    wait-for close of wh-pesquisa.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-est V-table-Win
ON LEAVE OF es-natoper-rec.nat-operacao-est IN FRAME f-main /* Estadual */
DO:
    {include/leave.i &tabela=natur-oper 
                     &atributo-ref=denominacao
                     &variavel-ref=desc-estadual
                     &where="natur-oper.nat-operacao = input frame {&frame-name} es-natoper-rec.nat-operacao-est"}        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-est V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.nat-operacao-est IN FRAME f-main /* Estadual */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-natoper-rec.nat-operacao-int
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-int V-table-Win
ON F5 OF es-natoper-rec.nat-operacao-int IN FRAME f-main /* Interestadual */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=es-natoper-rec.nat-operacao-int
                       &campozoom=nat-operacao
                       &campo2=desc-interestadual
                       &campozoom2=denominacao }
    wait-for close of wh-pesquisa.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-int V-table-Win
ON LEAVE OF es-natoper-rec.nat-operacao-int IN FRAME f-main /* Interestadual */
DO:
    {include/leave.i &tabela=natur-oper 
                     &atributo-ref=denominacao
                     &variavel-ref=desc-interestadual
                     &where="natur-oper.nat-operacao = input frame {&frame-name} es-natoper-rec.nat-operacao-int"}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-int V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.nat-operacao-int IN FRAME f-main /* Interestadual */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-natoper-rec.nat-operacao-rateio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio V-table-Win
ON F5 OF es-natoper-rec.nat-operacao-rateio IN FRAME f-main /* Estadual */
DO:
  
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=es-natoper-rec.nat-operacao-rateio
                       &campozoom=nat-operacao
                       &campo2=desc-rateio
                       &campozoom2=denominacao }
    wait-for close of wh-pesquisa.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio V-table-Win
ON LEAVE OF es-natoper-rec.nat-operacao-rateio IN FRAME f-main /* Estadual */
DO:
  
    {include/leave.i &tabela=natur-oper 
                     &atributo-ref=denominacao
                     &variavel-ref=desc-rateio
                     &where="natur-oper.nat-operacao = input frame {&frame-name} es-natoper-rec.nat-operacao-rateio"}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.nat-operacao-rateio IN FRAME f-main /* Estadual */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-natoper-rec.nat-operacao-rateio-int
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio-int V-table-Win
ON F5 OF es-natoper-rec.nat-operacao-rateio-int IN FRAME f-main /* Interestadual */
DO:
  
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=es-natoper-rec.nat-operacao-rateio-int
                       &campozoom=nat-operacao
                       &campo2=desc-rateio-int
                       &campozoom2=denominacao }
    wait-for close of wh-pesquisa.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio-int V-table-Win
ON LEAVE OF es-natoper-rec.nat-operacao-rateio-int IN FRAME f-main /* Interestadual */
DO:
  
    {include/leave.i &tabela=natur-oper 
                     &atributo-ref=denominacao
                     &variavel-ref=desc-rateio-int
                     &where="natur-oper.nat-operacao = input frame {&frame-name} es-natoper-rec.nat-operacao-rateio-int"}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-natoper-rec.nat-operacao-rateio-int V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-natoper-rec.nat-operacao-rateio-int IN FRAME f-main /* Interestadual */
DO:
   APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  es-natoper-rec.nat-operacao-est:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  es-natoper-rec.nat-operacao-int:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  es-natoper-rec.nat-operacao-rateio:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  es-natoper-rec.nat-operacao-rateio-int:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "es-natoper-rec"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-natoper-rec"}

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
/*     {include/i-valid.i} */
    if  not frame {&frame-name}:validate() then
        return 'ADM-ERROR':U.
    
    FOR FIRST natur-oper FIELDS(nat-operacao nat-ativa)
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-est NO-LOCK: END.
    IF NOT AVAIL natur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o Estadual~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-est)).
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-est.
        RETURN 'ADM-ERROR':U.
    END.
    IF NOT natur-oper.nat-ativa THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Natureza de operaá∆o estadual Inativa~~" + 
                                 "Natureza de operaá∆o estadual deve estar ativa, selecione outra.").
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-est.
        RETURN 'ADM-ERROR':U.
    END.

    FOR FIRST natur-oper FIELDS(nat-operacao nat-ativa)
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-int NO-LOCK: END.
    IF NOT AVAIL natur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o Interestadual~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-int)).
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-int.
        RETURN 'ADM-ERROR':U.
    END.
    IF NOT natur-oper.nat-ativa THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Natureza de operaá∆o interestadual Inativa~~" + 
                                 "Natureza de operaá∆o interestadual deve estar ativa, selecione outra.").
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-est.
        RETURN 'ADM-ERROR':U.
    END.


    FOR FIRST natur-oper FIELDS(nat-operacao nat-ativa)
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-rateio NO-LOCK: END.
    IF NOT AVAIL natur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o Rateio Estadual~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-rateio)).
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-rateio.
        RETURN 'ADM-ERROR':U.
    END.


    FOR FIRST natur-oper FIELDS(nat-operacao nat-ativa)
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-rateio-int NO-LOCK: END.
    IF NOT AVAIL natur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o Rateio Interestadual~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-natoper-rec.nat-operacao-rateio-int)).
        APPLY "ENTRY" TO es-natoper-rec.nat-operacao-rateio-int.
        RETURN 'ADM-ERROR':U.
    END.

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN es-natoper-rec.ep-codigo = i-ep-codigo-usuario.

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
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
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
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "LEAVE" TO es-natoper-rec.nat-operacao-est IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-natoper-rec.nat-operacao-int IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-natoper-rec.cod-benef-icms-rec IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-natoper-rec.cod-benef-pis-cofins-rec IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-natoper-rec.nat-operacao-rateio IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-natoper-rec.nat-operacao-rateio-int IN FRAME {&FRAME-NAME}.

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
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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
  {src/adm/template/snd-list.i "es-natoper-rec"}

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

