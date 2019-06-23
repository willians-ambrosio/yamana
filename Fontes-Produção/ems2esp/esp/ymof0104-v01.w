&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
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


DEF BUFFER b-es-ben-estab FOR es-ben-estab.


DEFINE VARIABLE l-icms-pri AS LOGICAL  INIT NO   NO-UNDO.
DEFINE VARIABLE l-pis-cofins AS LOGICAL INIT NO  NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES es-ben-estab
&Scoped-define FIRST-EXTERNAL-TABLE es-ben-estab


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-ben-estab.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-ben-estab.icms-prior ~
es-ben-estab.icms-ben-icms-est es-ben-estab.icms-ben-icms-inter ~
es-ben-estab.icms-ben-icms-imp es-ben-estab.icms-ben-icms-est-perc-redu ~
es-ben-estab.icms-ben-icms-inter-perc-redu ~
es-ben-estab.icms-ben-icms-imp-perc-redu ~
es-ben-estab.icms-ben-icms-est-red-bc ~
es-ben-estab.icms-ben-icms-inter-red-bc ~
es-ben-estab.icms-ben-icms-imp-red-bc ~
es-ben-estab.icms-ben-icms-est-nao-incide ~
es-ben-estab.icms-ben-icms-inter-nao-incide ~
es-ben-estab.icms-ben-icms-imp-nao-incide ~
es-ben-estab.pis-cofins-prioridade ~
es-ben-estab.pis-cofins-ben-pis-cof-compr-nac ~
es-ben-estab.pis-cofins-ben-pis-cof-imp es-ben-estab.compras-comprador-icms ~
es-ben-estab.compras-fornecedor-icms ~
es-ben-estab.compras-comprador-pis-confis ~
es-ben-estab.compras-fornecedor-pis-cofins ~
es-ben-estab.dt-ini-vald ~
    es-ben-estab.dt-fim-vald
&Scoped-define ENABLED-TABLES es-ben-estab
&Scoped-define FIRST-ENABLED-TABLE es-ben-estab
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold RECT-1 RECT-2 RECT-3 RECT-4 ~
RECT-5 RECT-6 RECT-7 f-desc-estab f-desc-benef 
&Scoped-Define DISPLAYED-FIELDS es-ben-estab.cod-estabel ~
es-ben-estab.cod-beneficio es-ben-estab.dt-ini-vald ~
es-ben-estab.dt-fim-vald es-ben-estab.icms-prior ~
es-ben-estab.icms-ben-icms-est es-ben-estab.icms-ben-icms-inter ~
es-ben-estab.icms-ben-icms-imp es-ben-estab.icms-ben-icms-est-perc-redu ~
es-ben-estab.icms-ben-icms-inter-perc-redu ~
es-ben-estab.icms-ben-icms-imp-perc-redu ~
es-ben-estab.icms-ben-icms-est-red-bc ~
es-ben-estab.icms-ben-icms-inter-red-bc ~
es-ben-estab.icms-ben-icms-imp-red-bc ~
es-ben-estab.icms-ben-icms-est-nao-incide ~
es-ben-estab.icms-ben-icms-inter-nao-incide ~
es-ben-estab.icms-ben-icms-imp-nao-incide ~
es-ben-estab.pis-cofins-prioridade ~
es-ben-estab.pis-cofins-ben-pis-cof-compr-nac ~
es-ben-estab.pis-cofins-ben-pis-cof-imp es-ben-estab.compras-comprador-icms ~
es-ben-estab.compras-fornecedor-icms ~
es-ben-estab.compras-comprador-pis-confis ~
es-ben-estab.compras-fornecedor-pis-cofins 
&Scoped-define DISPLAYED-TABLES es-ben-estab
&Scoped-define FIRST-DISPLAYED-TABLE es-ben-estab
&Scoped-Define DISPLAYED-OBJECTS f-desc-estab f-desc-benef 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-ben-estab.cod-estabel ~
es-ben-estab.cod-beneficio ~

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
DEFINE VARIABLE f-desc-benef AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .88 NO-UNDO.

DEFINE VARIABLE f-desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 6.54.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 4.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 4.29.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 4.29.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 3.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 4.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 2.79.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-ben-estab.cod-estabel AT ROW 1.54 COL 20.29 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     f-desc-estab AT ROW 1.54 COL 29.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     es-ben-estab.cod-beneficio AT ROW 2.5 COL 20.43 COLON-ALIGNED WIDGET-ID 2
          LABEL "Benef°cio/ Incentivo"
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     f-desc-benef AT ROW 2.5 COL 29.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     es-ben-estab.dt-ini-vald AT ROW 3.88 COL 17 COLON-ALIGNED WIDGET-ID 12
          LABEL "Validade"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es-ben-estab.dt-fim-vald AT ROW 3.88 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es-ben-estab.icms-prior AT ROW 5.63 COL 32 COLON-ALIGNED WIDGET-ID 20
          LABEL "Prioridade do Beneficio/ Incentivo" FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .88
     es-ben-estab.icms-ben-icms-est AT ROW 6.75 COL 5.72 WIDGET-ID 28
          LABEL "Ben.ICMS Est"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .83
     es-ben-estab.icms-ben-icms-inter AT ROW 6.75 COL 34 WIDGET-ID 40
          LABEL "Ben.ICMS Inter."
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .83
     es-ben-estab.icms-ben-icms-imp AT ROW 6.75 COL 62 WIDGET-ID 48
          LABEL "Ben.ICMS Imp."
          VIEW-AS TOGGLE-BOX
          SIZE 13 BY .83
     es-ben-estab.icms-ben-icms-est-perc-redu AT ROW 7.67 COL 12.29 COLON-ALIGNED WIDGET-ID 30
          LABEL "% Redu."
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-inter-perc-redu AT ROW 7.67 COL 40 COLON-ALIGNED WIDGET-ID 42
          LABEL "% Redu"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-imp-perc-redu AT ROW 7.67 COL 69.43 COLON-ALIGNED WIDGET-ID 50
          LABEL "% Redu"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-est-red-bc AT ROW 8.75 COL 12 COLON-ALIGNED WIDGET-ID 32
          LABEL "Redu. BC"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-inter-red-bc AT ROW 8.75 COL 40.14 COLON-ALIGNED WIDGET-ID 44
          LABEL "Red. BC"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-imp-red-bc AT ROW 8.75 COL 69.43 COLON-ALIGNED WIDGET-ID 52
          LABEL "Red.BC"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-ben-estab.icms-ben-icms-est-nao-incide AT ROW 9.83 COL 5.72 WIDGET-ID 34
          LABEL "N∆o incide"
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .83
     es-ben-estab.icms-ben-icms-inter-nao-incide AT ROW 9.83 COL 34 WIDGET-ID 46
          LABEL "N∆o-incide"
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es-ben-estab.icms-ben-icms-imp-nao-incide AT ROW 9.83 COL 62 WIDGET-ID 54
          LABEL "N∆o-incide"
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es-ben-estab.pis-cofins-prioridade AT ROW 12.71 COL 32.57 COLON-ALIGNED WIDGET-ID 60
          LABEL "Prioridade do Beneficio/ Incentivo"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .88
     es-ben-estab.pis-cofins-ben-pis-cof-compr-nac AT ROW 14 COL 4 WIDGET-ID 62
          LABEL "Benef. Pis/Cof compra Nacional"
          VIEW-AS TOGGLE-BOX
          SIZE 32 BY .83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     es-ben-estab.pis-cofins-ben-pis-cof-imp AT ROW 14 COL 45 WIDGET-ID 64
          LABEL "Benef. Pis/Cof Imp."
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .83
     es-ben-estab.compras-comprador-icms AT ROW 17 COL 23.86 COLON-ALIGNED WIDGET-ID 74
          LABEL "Comprador(ICMS)"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es-ben-estab.compras-fornecedor-icms AT ROW 17.04 COL 57 COLON-ALIGNED WIDGET-ID 78
          LABEL "Fornecedor(ICMS)"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es-ben-estab.compras-comprador-pis-confis AT ROW 18 COL 23.72 COLON-ALIGNED WIDGET-ID 76
          LABEL "Comprador(Pis/Cof)"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es-ben-estab.compras-fornecedor-pis-cofins AT ROW 18 COL 57 COLON-ALIGNED WIDGET-ID 80
          LABEL "Fonecedor(Pis/Cofis)"
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     "ICMS" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 5.13 COL 4 WIDGET-ID 18
     "COMPRAS" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 15.5 COL 4 WIDGET-ID 68
     "Orientaá‰es" VIEW-AS TEXT
          SIZE 8.29 BY .67 AT ROW 16.42 COL 10.72 WIDGET-ID 72
     "PIS/COFINS" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 11.96 COL 4 WIDGET-ID 58
     "atÇ" VIEW-AS TEXT
          SIZE 3.29 BY .67 AT ROW 4 COL 32.72 WIDGET-ID 14
     rt-key AT ROW 1.25 COL 1
     rt-mold AT ROW 3.63 COL 1
     RECT-1 AT ROW 5.38 COL 1.29 WIDGET-ID 16
     RECT-2 AT ROW 7 COL 3.72 WIDGET-ID 22
     RECT-3 AT ROW 7 COL 33 WIDGET-ID 36
     RECT-4 AT ROW 7 COL 61 WIDGET-ID 38
     RECT-5 AT ROW 12.13 COL 2 WIDGET-ID 56
     RECT-6 AT ROW 15.75 COL 2 WIDGET-ID 66
     RECT-7 AT ROW 16.71 COL 5 WIDGET-ID 70
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: es-ben-estab
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
         HEIGHT             = 19.21
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

/* SETTINGS FOR FILL-IN es-ben-estab.cod-beneficio IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN es-ben-estab.cod-estabel IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-ben-estab.compras-comprador-icms IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.compras-comprador-pis-confis IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.compras-fornecedor-icms IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.compras-fornecedor-pis-cofins IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.dt-fim-vald IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-ben-estab.dt-ini-vald IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
ASSIGN 
       f-desc-benef:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       f-desc-estab:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-est IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-est-nao-incide IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-est-perc-redu IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-est-red-bc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-imp IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-imp-nao-incide IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-imp-perc-redu IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-imp-red-bc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-inter IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.icms-ben-icms-inter-nao-incide IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-inter-perc-redu IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-ben-icms-inter-red-bc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.icms-prior IN FRAME f-main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.pis-cofins-ben-pis-cof-compr-nac IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-ben-estab.pis-cofins-ben-pis-cof-imp IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-ben-estab.pis-cofins-prioridade IN FRAME f-main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME es-ben-estab.cod-beneficio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.cod-beneficio V-table-Win
ON LEAVE OF es-ben-estab.cod-beneficio IN FRAME f-main /* Benef°cio/ Incentivo */
DO:
  FIND es-beneficio WHERE es-beneficio.cod-beneficio = INPUT FRAME f-main es-ben-estab.cod-beneficio NO-LOCK  NO-ERROR.
  IF NOT AVAIL es-beneficio THEN DO:

     MESSAGE "Beneficio n∆o cadastrado!!"
              VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.

  END.




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.cod-beneficio V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.cod-beneficio IN FRAME f-main /* Benef°cio/ Incentivo */
DO:
  {include/zoomvar.i &prog-zoom=esp\ymof0103-p01.w
                         &campo=es-ben-estab.cod-beneficio
                         &campozoom=cod-beneficio
                         &campo2=f-desc-benef
                         &campozoom2=desc-beneficio
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.cod-estabel V-table-Win
ON LEAVE OF es-ben-estab.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   FIND estabelec WHERE estabelec.cod-estabel =  es-ben-estab.cod-estabel:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL estabelec THEN DO:
        f-desc-estab:SCREEN-VALUE = estabelec.nome.
   END.
   ELSE DO:
       IF  es-ben-estab.cod-estabel:SCREEN-VALUE = "" THEN RETURN.
       MESSAGE "Estabelecimento n∆o cadastrado!!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   {include/zoomvar.i &prog-zoom=adzoom\z01ad107.r
                         &campo=es-ben-estab.cod-estabel
                         &campozoom=cod-estabel
                         &campo2=f-desc-estab
                         &campozoom2=nome
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.compras-comprador-icms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.compras-comprador-icms V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.compras-comprador-icms IN FRAME f-main /* Comprador(ICMS) */
DO:
   {include/zoomvar.i &prog-zoom=esp\ymof0105-p01.w
                         &campo=es-ben-estab.compras-comprador-icms
                         &campozoom=cod-mensagem
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.compras-comprador-pis-confis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.compras-comprador-pis-confis V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.compras-comprador-pis-confis IN FRAME f-main /* Comprador(Pis/Cof) */
DO:
   {include/zoomvar.i &prog-zoom=esp\ymof0105-p01.w
                         &campo=es-ben-estab.compras-comprador-pis-confis
                         &campozoom=cod-mensagem
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.compras-fornecedor-icms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.compras-fornecedor-icms V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.compras-fornecedor-icms IN FRAME f-main /* Fornecedor(ICMS) */
DO:
   {include/zoomvar.i &prog-zoom=esp\ymof0105-p01.w
                         &campo=es-ben-estab.compras-fornecedor-icms
                         &campozoom=cod-mensagem
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.compras-fornecedor-pis-cofins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.compras-fornecedor-pis-cofins V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-ben-estab.compras-fornecedor-pis-cofins IN FRAME f-main /* Fonecedor(Pis/Cofis) */
DO:
   {include/zoomvar.i &prog-zoom=esp\ymof0105-p01.w
                         &campo=es-ben-estab.compras-fornecedor-pis-cofins
                         &campozoom=cod-mensagem
                          }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.icms-ben-icms-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.icms-ben-icms-est V-table-Win
ON VALUE-CHANGED OF es-ben-estab.icms-ben-icms-est IN FRAME f-main /* Ben.ICMS Est */
DO:
  IF es-ben-estab.icms-ben-icms-est:CHECKED THEN DO:
      
          es-ben-estab.icms-ben-icms-est-perc-redu:READ-ONLY = FALSE .
          es-ben-estab.icms-ben-icms-est-nao-incide:SENSITIVE = TRUE . 
          es-ben-estab.icms-ben-icms-est-red-bc:READ-ONLY = FALSE .       
  END.
  ELSE DO:
      es-ben-estab.icms-ben-icms-est-perc-redu:READ-ONLY = TRUE .  
      es-ben-estab.icms-ben-icms-est-nao-incide:SENSITIVE = FALSE .
      es-ben-estab.icms-ben-icms-est-red-bc:READ-ONLY = TRUE  .     
  END                                                                             .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.icms-ben-icms-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.icms-ben-icms-imp V-table-Win
ON VALUE-CHANGED OF es-ben-estab.icms-ben-icms-imp IN FRAME f-main /* Ben.ICMS Imp. */
DO:
 
     IF  es-ben-estab.icms-ben-icms-imp:CHECKED THEN DO:                     
                                                                                   
             es-ben-estab.icms-ben-icms-imp-perc-redu:READ-ONLY = FALSE .    
             es-ben-estab.icms-ben-icms-imp-nao-incide:SENSITIVE = TRUE.     
             es-ben-estab.icms-ben-icms-imp-red-bc:READ-ONLY = FALSE .       
     END.                                                                          
     ELSE DO:                                                                      
          es-ben-estab.icms-ben-icms-imp-perc-redu:READ-ONLY = TRUE .        
          es-ben-estab.icms-ben-icms-imp-nao-incide:SENSITIVE = FALSE.       
          es-ben-estab.icms-ben-icms-imp-red-bc:READ-ONLY = TRUE .           
     END.                                                                          





END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.icms-ben-icms-inter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.icms-ben-icms-inter V-table-Win
ON VALUE-CHANGED OF es-ben-estab.icms-ben-icms-inter IN FRAME f-main /* Ben.ICMS Inter. */
DO:
                                                                      
    IF  es-ben-estab.icms-ben-icms-inter:CHECKED THEN DO:                          
                                                                                         
              es-ben-estab.icms-ben-icms-inter-perc-redu:READ-ONLY = FALSE .       
             es-ben-estab.icms-ben-icms-inter-nao-incide:SENSITIVE = FALSE.        
               es-ben-estab.icms-ben-icms-inter-red-bc:READ-ONLY = FALSE .         
    END.                                                                                 
    ELSE DO:                                                                             
        es-ben-estab.icms-ben-icms-inter-perc-redu:READ-ONLY = TRUE .              
        es-ben-estab.icms-ben-icms-inter-nao-incide:SENSITIVE = FALSE .            
        es-ben-estab.icms-ben-icms-inter-red-bc:READ-ONLY = TRUE .                 
    END                                                                             .    
    
    
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.icms-prior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.icms-prior V-table-Win
ON VALUE-CHANGED OF es-ben-estab.icms-prior IN FRAME f-main /* Prioridade do Beneficio/ Incentivo */
DO:
  
    ASSIGN l-icms-pri = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-ben-estab.pis-cofins-prioridade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-ben-estab.pis-cofins-prioridade V-table-Win
ON VALUE-CHANGED OF es-ben-estab.pis-cofins-prioridade IN FRAME f-main /* Prioridade do Beneficio/ Incentivo */
DO:
ASSIGN  l-pis-cofins = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF   
        
    es-ben-estab.cod-estabel:LOAD-MOUSE-POINTER('image/lupa.cur').
    es-ben-estab.cod-beneficio:LOAD-MOUSE-POINTER('image/lupa.cur').

    es-ben-estab.compras-comprador-icms:LOAD-MOUSE-POINTER('image/lupa.cur'). 
    es-ben-estab.compras-comprador-pis-confis:LOAD-MOUSE-POINTER('image/lupa.cur'). 
    es-ben-estab.compras-fornecedor-icms:LOAD-MOUSE-POINTER('image/lupa.cur'). 
    es-ben-estab.compras-fornecedor-pis-cofins:LOAD-MOUSE-POINTER('image/lupa.cur').





  
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
  {src/adm/template/row-list.i "es-ben-estab"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-ben-estab"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

 es-ben-estab.dt-fim-vald:SCREEN-VALUE IN FRAME f-main = STRING(TODAY).
 es-ben-estab.dt-ini-vald:SCREEN-VALUE IN FRAME f-main = STRING(TODAY).




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  FIND estabelec WHERE estabelec.cod-estabel  =  es-ben-estab.cod-estabel:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO:
   run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Estabelecimento n∆o cadastrado!!!" ).

        return 'ADM-ERROR':U.
  END.


IF adm-new-record OR l-pis-cofins OR l-icms-pri THEN DO:



 IF l-pis-cofins OR (l-pis-cofins = NO AND adm-new-record = YES)  THEN DO:


   IF es-ben-estab.pis-cofins-ben-pis-cof-compr-nac:CHECKED OR es-ben-estab.pis-cofins-ben-pis-cof-imp:CHECKED THEN DO:
   

     FIND b-es-ben-estab WHERE b-es-ben-estab.pis-cofins-prioridade = INPUT FRAME f-main es-ben-estab.pis-cofins-prioridade
                          AND     b-es-ben-estab.dt-fim-vald        > INPUT FRAME f-main es-ben-estab.dt-ini-vald  
                          AND    b-es-ben-estab.dt-ini-vald         < INPUT FRAME f-main es-ben-estab.dt-fim-vald
                         AND   b-es-ben-estab.cod-estabel           = INPUT FRAME f-main es-ben-estab.cod-estabel NO-LOCK NO-ERROR.
     IF AVAIL b-es-ben-estab THEN DO:
         run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Prioridade para PIS/COFINS j† existe para esse estabelecimento" ).
        
        return 'ADM-ERROR':U.
     END.
   END.
 END.


 IF l-icms-pri  OR l-icms-pri = NO AND adm-new-record = YES THEN DO:

    IF es-ben-estab.icms-ben-icms-est:CHECKED OR 
       es-ben-estab.icms-ben-icms-inter:CHECKED OR 
       es-ben-estab.icms-ben-icms-imp:CHECKED  THEN DO:
    
  
        FIND b-es-ben-estab WHERE  b-es-ben-estab.icms-prior   = INPUT FRAME f-main es-ben-estab.icms-prior
                            AND     b-es-ben-estab.dt-fim-vald > INPUT FRAME f-main es-ben-estab.dt-ini-vald
                            AND    b-es-ben-estab.dt-ini-vald  < INPUT FRAME f-main es-ben-estab.dt-fim-vald
                            AND    b-es-ben-estab.cod-estabel  = INPUT FRAME f-main es-ben-estab.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL b-es-ben-estab THEN DO:
             run utp/ut-msgs.p (input "show",
                              input 17006,
                              input "Prioridade para ICMS j† existe para esse estabelecimento" ).
            return 'ADM-ERROR':U.
        END.
    END.
 END.

l-icms-pri = NO.
l-pis-cofins = NO.


END.












  FIND es-beneficio WHERE es-beneficio.cod-beneficio = INPUT FRAME f-main es-ben-estab.cod-beneficio NO-LOCK  NO-ERROR.
  IF NOT AVAIL es-beneficio THEN DO:
    run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Beneficio n∆o Cadastrado!!!" ).
      
     return 'ADM-ERROR':U.
  END.


  IF date(es-ben-estab.dt-ini-vald:SCREEN-VALUE) >= date(es-ben-estab.dt-fim-vald:SCREEN-VALUE)  THEN DO:

      run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Data final da validade deve ser maior que a inicial!!!" ).
      return 'ADM-ERROR':U.
  END.


  IF ADM-NEW-RECORD THEN DO:
      FIND es-ben-estab WHERE es-ben-estab.cod-beneficio       = INPUT FRAME f-main es-ben-estab.cod-beneficio
                        AND   es-ben-estab.dt-fim-vald         > INPUT FRAME f-main es-ben-estab.dt-ini-vald
                        AND   es-ben-estab.dt-ini-vald   < INPUT FRAME f-main es-ben-estab.dt-fim-vald
                        AND   es-ben-estab.cod-estabel         = INPUT FRAME f-main es-ben-estab.cod-estabel NO-LOCK NO-ERROR.
      IF AVAIL es-ben-estab THEN DO:

           run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Beneficio ja cadastrado para este estabelecimento e periodo!!!" ).
           
          return 'ADM-ERROR':U.   
      END.
 END.


IF INPUT FRAME f-main es-ben-estab.compras-comprador-icm <> 0  THEN DO:
    FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = INPUT FRAME f-main es-ben-estab.compras-comprador-icms
                            NO-LOCK NO-ERROR.
    IF NOT AVAIL es-mensagem-ben  THEN DO:
        run utp/ut-msgs.p (input "show",                                                 
                   input 17006,                                                        
                   input "Mensagem n∆o cadastrada para <COMPRADOR ICMS>!!!" ).       
        return 'ADM-ERROR':U.
    END.
END.

IF INPUT FRAME f-main es-ben-estab.compras-comprador-pis-confis <> 0  THEN DO:
    FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = INPUT FRAME f-main es-ben-estab.compras-comprador-pis-confis
                            NO-LOCK NO-ERROR.
    IF NOT AVAIL es-mensagem-ben  THEN DO:
        run utp/ut-msgs.p (input "show",                                                 
                   input 17006,                                                        
                   input "Mensagem n∆o cadastrada para <COMPRADOR Pis/Cofins>!!!" ).       
        return 'ADM-ERROR':U.
    END.
END.

IF INPUT FRAME f-main es-ben-estab.compras-fornecedor-icms  <> 0  THEN DO:
    FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = INPUT FRAME f-main es-ben-estab.compras-fornecedor-icms
                            NO-LOCK NO-ERROR.
    IF NOT AVAIL es-mensagem-ben  THEN DO:
        run utp/ut-msgs.p (input "show",                                                 
                   input 17006,                                                        
                   input "Mensagem n∆o cadastrada para <Forncedor ICMS>!!!" ).       
        return 'ADM-ERROR':U.
    END.
END.
   
IF INPUT FRAME f-main es-ben-estab.compras-fornecedor-pis-cofins  <> 0  THEN DO:
    FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = INPUT FRAME f-main es-ben-estab.compras-fornecedor-pis-cofins
                            NO-LOCK NO-ERROR.
    IF NOT AVAIL es-mensagem-ben  THEN DO:
        run utp/ut-msgs.p (input "show",                                                 
                   input 17006,                                                        
                   input "Mensagem n∆o cadastrada para <Forncedor Pis/Cofins>!!!" ).       
        return 'ADM-ERROR':U.
    END.
END.
   
IF es-ben-estab.icms-ben-icms-est:CHECKED IN FRAME f-main THEN DO:                     
                                                                                     
        IF INPUT FRAME f-main  es-ben-estab.icms-ben-icms-est-perc-redu  = 0
           AND es-ben-estab.icms-ben-icms-est-red-bc:INPUT-VALUE = 0
           AND es-ben-estab.icms-ben-icms-est-nao-incide:CHECKED = FALSE THEN DO: 

          run utp/ut-msgs.p (input "show",                                                 
                   input 17006,                                                        
                   input "Beneficio de ICMS Externo selecionado mas n∆o parametrizado!!! " ).       
           return 'ADM-ERROR':U.
    END.
                      
END.                            
   
 IF  es-ben-estab.icms-ben-icms-inter:CHECKED THEN DO:                              
                                                                                          
     IF INPUT FRAME f-main es-ben-estab.icms-ben-icms-inter-perc-redu = 0
         AND    es-ben-estab.icms-ben-icms-inter-red-bc:INPUT-VALUE = 0    
         AND    es-ben-estab.icms-ben-icms-inter-nao-incide:CHECKED = FALSE THEN DO: 
             run utp/ut-msgs.p (input "show",                                                           
                      input 17006,                                                                      
                      input "Beneficio de ICMS Interno selecionado mas n∆o parametrizado!!! " ).        
              return 'ADM-ERROR':U.
     END.
                    
 END.                                                                                     


 IF  es-ben-estab.icms-ben-icms-imp:CHECKED THEN DO:                       
                                                                                 
    IF INPUT FRAME f-main  es-ben-estab.icms-ben-icms-imp-perc-redu = 0
       AND    es-ben-estab.icms-ben-icms-imp-red-bc:INPUT-VALUE = 0 
       AND   es-ben-estab.icms-ben-icms-imp-nao-incide:CHECKED = FALSE THEN DO: 
           run utp/ut-msgs.p (input "show",                                                       
                    input 17006,                                                                  
                    input "Beneficio de ICMS Importaá∆o selecionado mas n∆o parametrizado!!! " ).    
            return 'ADM-ERROR':U.
    END.


 END.                                                                            






   
   
   
   
   
   






    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
     
      
         DISABLE es-ben-estab.icms-ben-icms-est-perc-redu   WITH FRAME f-main.
         DISABLE  es-ben-estab.icms-ben-icms-est-nao-incide WITH FRAME f-main.
         DISABLE es-ben-estab.icms-ben-icms-est-red-bc      WITH FRAME f-main.
         DISABLE  es-ben-estab.icms-ben-icms-inter-perc-redu   WITH FRAME f-main.  
         DISABLE  es-ben-estab.icms-ben-icms-inter-nao-incide WITH FRAME f-main.   
         DISABLE  es-ben-estab.icms-ben-icms-inter-red-bc      WITH FRAME f-main. 
          DISABLE es-ben-estab.icms-ben-icms-imp-perc-redu  WITH FRAME f-main.   
          DISABLE es-ben-estab.icms-ben-icms-imp-nao-incide WITH FRAME f-main.   
          DISABLE es-ben-estab.icms-ben-icms-imp-red-bc     WITH FRAME f-main.   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .


 FIND estabelec WHERE estabelec.cod-estabel =  es-ben-estab.cod-estabel:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR.
   IF AVAIL estabelec THEN DO:
        f-desc-estab:SCREEN-VALUE = estabelec.nome.
   END.


 FIND es-beneficio WHERE es-beneficio.cod-beneficio = INPUT FRAME f-main es-ben-estab.cod-beneficio NO-LOCK NO-ERROR.
 IF AVAIL es-beneficio  THEN ASSIGN f-desc-benef:SCREEN-VALUE = es-beneficio.desc-beneficio. 












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
    
 if adm-new-record = NO THEN DO:
        IF es-ben-estab.icms-ben-icms-est:CHECKED IN FRAME f-main THEN DO:                     
                                                                                     
                es-ben-estab.icms-ben-icms-est-perc-redu:READ-ONLY = FALSE .   
                es-ben-estab.icms-ben-icms-est-nao-incide:SENSITIVE = TRUE .   
                es-ben-estab.icms-ben-icms-est-red-bc:READ-ONLY = FALSE .      
        END.                                                                         
        ELSE DO:                                                                     
            es-ben-estab.icms-ben-icms-est-perc-redu:READ-ONLY = TRUE .        
            es-ben-estab.icms-ben-icms-est-nao-incide:SENSITIVE = FALSE .      
            es-ben-estab.icms-ben-icms-est-red-bc:READ-ONLY = TRUE  .          
        END.

        IF  es-ben-estab.icms-ben-icms-inter:CHECKED THEN DO:                         
                                                                                            
                  es-ben-estab.icms-ben-icms-inter-perc-redu:READ-ONLY = FALSE .
                 es-ben-estab.icms-ben-icms-inter-nao-incide:SENSITIVE = TRUE.   
                   es-ben-estab.icms-ben-icms-inter-red-bc:READ-ONLY = FALSE .     
        END.                                                                                
        ELSE DO:                                                                            
            es-ben-estab.icms-ben-icms-inter-perc-redu:READ-ONLY = TRUE .   
            es-ben-estab.icms-ben-icms-inter-nao-incide:SENSITIVE = FALSE .    
            es-ben-estab.icms-ben-icms-inter-red-bc:READ-ONLY = TRUE .     
        END                                                                             .   
        IF  es-ben-estab.icms-ben-icms-imp:CHECKED THEN DO:                              
                                                                                               
                es-ben-estab.icms-ben-icms-imp-perc-redu:READ-ONLY = FALSE .   
                es-ben-estab.icms-ben-icms-imp-nao-incide:SENSITIVE = TRUE.    
                es-ben-estab.icms-ben-icms-imp-red-bc:READ-ONLY = FALSE .     
        END.                                                                                   
        ELSE DO:                                                                               
             es-ben-estab.icms-ben-icms-imp-perc-redu:READ-ONLY = TRUE .         
             es-ben-estab.icms-ben-icms-imp-nao-incide:SENSITIVE = FALSE.          
             es-ben-estab.icms-ben-icms-imp-red-bc:READ-ONLY = TRUE .            
        END.                                                                                   





 END.
 ELSE DO:
      es-ben-estab.icms-ben-icms-est-perc-redu:READ-ONLY = TRUE .     
      es-ben-estab.icms-ben-icms-est-nao-incide:SENSITIVE = FALSE .   
      es-ben-estab.icms-ben-icms-est-red-bc:READ-ONLY = TRUE  .       
      es-ben-estab.icms-ben-icms-imp-perc-redu:READ-ONLY = TRUE .        
      es-ben-estab.icms-ben-icms-imp-nao-incide:SENSITIVE = FALSE.       
      es-ben-estab.icms-ben-icms-imp-red-bc:READ-ONLY = TRUE .           
      es-ben-estab.icms-ben-icms-inter-perc-redu:READ-ONLY = TRUE .      
      es-ben-estab.icms-ben-icms-inter-nao-incide:SENSITIVE = FALSE .    
      es-ben-estab.icms-ben-icms-inter-red-bc:READ-ONLY = TRUE .         


 END.
 





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
  {src/adm/template/snd-list.i "es-ben-estab"}

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

