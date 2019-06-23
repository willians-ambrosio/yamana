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
{include/i-prgvrs.i YMRE0101B-V01 11.5.11.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMRE0101B-V01 MUT}
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
&Scoped-define EXTERNAL-TABLES es-rec-atrib-natoper
&Scoped-define FIRST-EXTERNAL-TABLE es-rec-atrib-natoper


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-rec-atrib-natoper.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-rec-atrib-natoper.cod-estabel ~
es-rec-atrib-natoper.estab-todos es-rec-atrib-natoper.classe ~
es-rec-atrib-natoper.classe-todos es-rec-atrib-natoper.operacao ~
es-rec-atrib-natoper.cod-beneficio-icms ~
es-rec-atrib-natoper.nenhum-ben-icms ~
es-rec-atrib-natoper.cod-beneficio-piscof ~
es-rec-atrib-natoper.nenhum-ben-piscof ~
es-rec-atrib-natoper.cod-model-nf-eletro es-rec-atrib-natoper.model-todos ~
es-rec-atrib-natoper.cod-cfop-saida es-rec-atrib-natoper.nenhum-cfop ~
es-rec-atrib-natoper.l-rateio es-rec-atrib-natoper.nat-operacao ~
es-rec-atrib-natoper.observacoes 
&Scoped-define ENABLED-TABLES es-rec-atrib-natoper
&Scoped-define FIRST-ENABLED-TABLE es-rec-atrib-natoper
&Scoped-Define ENABLED-OBJECTS rt-mold1 rt-mold rt-key rs-dif-aliq 
&Scoped-Define DISPLAYED-FIELDS es-rec-atrib-natoper.identificador ~
es-rec-atrib-natoper.cod-estabel es-rec-atrib-natoper.estab-todos ~
es-rec-atrib-natoper.classe es-rec-atrib-natoper.classe-todos ~
es-rec-atrib-natoper.operacao es-rec-atrib-natoper.cod-beneficio-icms ~
es-rec-atrib-natoper.nenhum-ben-icms ~
es-rec-atrib-natoper.cod-beneficio-piscof ~
es-rec-atrib-natoper.nenhum-ben-piscof ~
es-rec-atrib-natoper.cod-model-nf-eletro es-rec-atrib-natoper.model-todos ~
es-rec-atrib-natoper.cod-cfop-saida es-rec-atrib-natoper.nenhum-cfop ~
es-rec-atrib-natoper.dif-aliq-icm es-rec-atrib-natoper.l-rateio ~
es-rec-atrib-natoper.nat-operacao es-rec-atrib-natoper.observacoes 
&Scoped-define DISPLAYED-TABLES es-rec-atrib-natoper
&Scoped-define FIRST-DISPLAYED-TABLE es-rec-atrib-natoper
&Scoped-Define DISPLAYED-OBJECTS desc-estab desc-ben-icms desc-ben-piscof ~
rs-dif-aliq desc-nat-oper 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS es-rec-atrib-natoper.dif-aliq-icm 

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
DEFINE VARIABLE desc-ben-icms AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 NO-UNDO.

DEFINE VARIABLE desc-ben-piscof AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 NO-UNDO.

DEFINE VARIABLE desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 NO-UNDO.

DEFINE VARIABLE desc-nat-oper AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .88 NO-UNDO.

DEFINE VARIABLE rs-dif-aliq AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "NÆo", no,
"Sim", yes
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80.57 BY 8.25.

DEFINE RECTANGLE rt-mold1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80.57 BY 4.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-rec-atrib-natoper.identificador AT ROW 1.17 COL 16.57 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     es-rec-atrib-natoper.cod-estabel AT ROW 2.54 COL 6.57 WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     desc-estab AT ROW 2.54 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     es-rec-atrib-natoper.estab-todos AT ROW 2.54 COL 72 WIDGET-ID 38
          LABEL "Todos"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     es-rec-atrib-natoper.classe AT ROW 3.54 COL 16.57 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 52.43 BY .88
     es-rec-atrib-natoper.classe-todos AT ROW 3.54 COL 72 WIDGET-ID 42
          LABEL "Todos"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     es-rec-atrib-natoper.operacao AT ROW 4.54 COL 18.57 NO-LABEL WIDGET-ID 6
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Estadual", "E":U,
"Interestadual", "I":U
          SIZE 26.57 BY .88
     es-rec-atrib-natoper.cod-beneficio-icms AT ROW 5.54 COL 7.28 WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     desc-ben-icms AT ROW 5.54 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     es-rec-atrib-natoper.nenhum-ben-icms AT ROW 5.54 COL 72 WIDGET-ID 44
          LABEL "Nenhum"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     es-rec-atrib-natoper.cod-beneficio-piscof AT ROW 6.54 COL 2.28 WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     desc-ben-piscof AT ROW 6.54 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     es-rec-atrib-natoper.nenhum-ben-piscof AT ROW 6.54 COL 72 WIDGET-ID 46
          LABEL "Nenhum"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     es-rec-atrib-natoper.cod-model-nf-eletro AT ROW 7.54 COL 5 WIDGET-ID 16
          LABEL "Modelo NF-e/CT-e"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es-rec-atrib-natoper.model-todos AT ROW 7.54 COL 72 WIDGET-ID 48
          LABEL "Todos"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     es-rec-atrib-natoper.cod-cfop-saida AT ROW 8.54 COL 9.14 WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 52.43 BY .88
     es-rec-atrib-natoper.nenhum-cfop AT ROW 8.54 COL 72 WIDGET-ID 50
          LABEL "Nenhum"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .88
     rs-dif-aliq AT ROW 9.5 COL 18.72 NO-LABEL WIDGET-ID 60
     es-rec-atrib-natoper.dif-aliq-icm AT ROW 9.5 COL 45 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     es-rec-atrib-natoper.l-rateio AT ROW 9.5 COL 63 COLON-ALIGNED WIDGET-ID 68
          LABEL "Rateio"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     es-rec-atrib-natoper.nat-operacao AT ROW 10.92 COL 1.57 WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     desc-nat-oper AT ROW 10.92 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     es-rec-atrib-natoper.observacoes AT ROW 11.92 COL 18.57 NO-LABEL WIDGET-ID 22
          VIEW-AS EDITOR MAX-CHARS 400 SCROLLBAR-VERTICAL
          SIZE 62.43 BY 3
     "Tipo Opera‡Æo:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 4.63 COL 7.72 WIDGET-ID 26
     "Observa‡äes:" VIEW-AS TEXT
          SIZE 9.72 BY .54 AT ROW 11.88 COL 8.72 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     "ICMS Dif Al¡quotas" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 9.63 COL 4.57 WIDGET-ID 64
     rt-mold1 AT ROW 10.75 COL 1
     rt-mold AT ROW 2.38 COL 1 WIDGET-ID 52
     rt-key AT ROW 1 COL 1 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es-rec-atrib-natoper
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
         HEIGHT             = 14.88
         WIDTH              = 80.57.
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

/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.classe-todos IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.cod-beneficio-icms IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.cod-beneficio-piscof IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.cod-cfop-saida IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.cod-estabel IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.cod-model-nf-eletro IN FRAME f-main
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN desc-ben-icms IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-ben-piscof IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-nat-oper IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.dif-aliq-icm IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.estab-todos IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.identificador IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       es-rec-atrib-natoper.identificador:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.l-rateio IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.model-todos IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-rec-atrib-natoper.nat-operacao IN FRAME f-main
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.nenhum-ben-icms IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.nenhum-ben-piscof IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX es-rec-atrib-natoper.nenhum-cfop IN FRAME f-main
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

&Scoped-define SELF-NAME es-rec-atrib-natoper.classe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.classe V-table-Win
ON F5 OF es-rec-atrib-natoper.classe IN FRAME f-main /* CFA */
DO:
/*     {include/zoomvar.i &prog-zoom="esp/ymof0103b-p01.w"   */
/*                        &campo=es-rec-atrib-natoper.classe */
/*                        &campozoom=classe                  */
/*                        &campo2=desc-classe                */
/*                        &campozoom2=descricao }            */
/*     wait-for close of wh-pesquisa.                        */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.classe V-table-Win
ON LEAVE OF es-rec-atrib-natoper.classe IN FRAME f-main /* CFA */
DO:
/*     {include/leave.i &tabela=es-cfa                                                                  */
/*                      &atributo-ref=descricao                                                         */
/*                      &variavel-ref=desc-classe                                                       */
/*                      &where="es-cfa.classe = input frame {&frame-name} es-rec-atrib-natoper.classe"} */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.classe V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-rec-atrib-natoper.classe IN FRAME f-main /* CFA */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.classe-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.classe-todos V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.classe-todos IN FRAME f-main /* Todos */
DO:
    ASSIGN es-rec-atrib-natoper.classe:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.classe:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.cod-beneficio-icms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-icms V-table-Win
ON F5 OF es-rec-atrib-natoper.cod-beneficio-icms IN FRAME f-main /* Beneficio ICMS */
DO:
    {include/zoomvar.i &prog-zoom="esp/ymof0103-p01.w"
                       &campo=es-rec-atrib-natoper.cod-beneficio-icms
                       &campozoom=cod-beneficio
                       &campo2=desc-ben-icms
                       &campozoom2=desc-beneficio }
    wait-for close of wh-pesquisa.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-icms V-table-Win
ON LEAVE OF es-rec-atrib-natoper.cod-beneficio-icms IN FRAME f-main /* Beneficio ICMS */
DO:
    {include/leave.i &tabela=es-beneficio 
                     &atributo-ref=desc-beneficio
                     &variavel-ref=desc-ben-icms
                     &where="es-beneficio.cod-beneficio = input frame {&frame-name} es-rec-atrib-natoper.cod-beneficio-icms"}              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-icms V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-rec-atrib-natoper.cod-beneficio-icms IN FRAME f-main /* Beneficio ICMS */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.cod-beneficio-piscof
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-piscof V-table-Win
ON F5 OF es-rec-atrib-natoper.cod-beneficio-piscof IN FRAME f-main /* Beneficio PIS/COFINS */
DO:
    {include/zoomvar.i &prog-zoom="esp/ymof0103-p01.w"
                       &campo=es-rec-atrib-natoper.cod-beneficio-piscof
                       &campozoom=cod-beneficio
                       &campo2=desc-ben-piscof
                       &campozoom2=desc-beneficio }
    wait-for close of wh-pesquisa.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-piscof V-table-Win
ON LEAVE OF es-rec-atrib-natoper.cod-beneficio-piscof IN FRAME f-main /* Beneficio PIS/COFINS */
DO:
    {include/leave.i &tabela=es-beneficio 
                     &atributo-ref=desc-beneficio
                     &variavel-ref=desc-ben-piscof
                     &where="es-beneficio.cod-beneficio = input frame {&frame-name} es-rec-atrib-natoper.cod-beneficio-piscof"}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-beneficio-piscof V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-rec-atrib-natoper.cod-beneficio-piscof IN FRAME f-main /* Beneficio PIS/COFINS */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-estabel V-table-Win
ON F5 OF es-rec-atrib-natoper.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    {include/zoomvar.i &prog-zoom="adzoom/z02ad107.w"
                       &campo=es-rec-atrib-natoper.cod-estabel
                       &campozoom=cod-estabel
                       &campo2=desc-estab
                       &campozoom2=nome }
    wait-for close of wh-pesquisa.             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-estabel V-table-Win
ON LEAVE OF es-rec-atrib-natoper.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    {include/leave.i &tabela=estabelec 
                     &atributo-ref=nome
                     &variavel-ref=desc-estab
                     &where="estabelec.cod-estabel = input frame {&frame-name} es-rec-atrib-natoper.cod-estabel 
                         AND estabelec.ep-codigo   = i-ep-codigo-usuario "}              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-rec-atrib-natoper.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.estab-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.estab-todos V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.estab-todos IN FRAME f-main /* Todos */
DO:
    ASSIGN es-rec-atrib-natoper.cod-estabel:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.cod-estabel:SCREEN-VALUE = ""
               desc-estab                      :SCREEN-VALUE = "Todos".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.model-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.model-todos V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.model-todos IN FRAME f-main /* Todos */
DO:
    ASSIGN es-rec-atrib-natoper.cod-model-nf-eletro:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.cod-model-nf-eletro:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nat-operacao V-table-Win
ON F5 OF es-rec-atrib-natoper.nat-operacao IN FRAME f-main /* Nat Opera‡Æo AtribuÂ¡da */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in245.w"
                       &campo=es-rec-atrib-natoper.nat-operacao
                       &campozoom=nat-operacao
                       &campo2=desc-nat-oper
                       &campozoom2=denominacao }
    wait-for close of wh-pesquisa.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nat-operacao V-table-Win
ON LEAVE OF es-rec-atrib-natoper.nat-operacao IN FRAME f-main /* Nat Opera‡Æo AtribuÂ¡da */
DO:
    {include/leave.i &tabela=natur-oper 
                     &atributo-ref=denominacao
                     &variavel-ref=desc-nat-oper
                     &where="natur-oper.nat-operacao = input frame {&frame-name} es-rec-atrib-natoper.nat-operacao"}          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nat-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-rec-atrib-natoper.nat-operacao IN FRAME f-main /* Nat Opera‡Æo AtribuÂ¡da */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.nenhum-ben-icms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nenhum-ben-icms V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.nenhum-ben-icms IN FRAME f-main /* Nenhum */
DO:
    ASSIGN es-rec-atrib-natoper.cod-beneficio-icms:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.cod-beneficio-icms:SCREEN-VALUE = ""
               desc-ben-icms                          :SCREEN-VALUE = "Nenhum".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.nenhum-ben-piscof
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nenhum-ben-piscof V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.nenhum-ben-piscof IN FRAME f-main /* Nenhum */
DO:
    ASSIGN es-rec-atrib-natoper.cod-beneficio-piscof:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.cod-beneficio-piscof:SCREEN-VALUE = ""
               desc-ben-piscof                          :SCREEN-VALUE = "Nenhum". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-rec-atrib-natoper.nenhum-cfop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-rec-atrib-natoper.nenhum-cfop V-table-Win
ON VALUE-CHANGED OF es-rec-atrib-natoper.nenhum-cfop IN FRAME f-main /* Nenhum */
DO:
    ASSIGN es-rec-atrib-natoper.cod-cfop-saida:SENSITIVE = NOT SELF:CHECKED.
    IF SELF:CHECKED THEN
        ASSIGN es-rec-atrib-natoper.cod-cfop-saida:SCREEN-VALUE = "".   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-dif-aliq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-dif-aliq V-table-Win
ON VALUE-CHANGED OF rs-dif-aliq IN FRAME f-main
DO:
  IF LOGICAL(SELF:SCREEN-VALUE) THEN
      ASSIGN es-rec-atrib-natop.dif-aliq-icm:SENSITIVE = TRUE.
  ELSE
      ASSIGN es-rec-atrib-natop.dif-aliq-icm:SENSITIVE    = NO
             es-rec-atrib-natop.dif-aliq-icm:SCREEN-VALUE = STRING(0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  es-rec-atrib-natoper.cod-estabel:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
/*   es-rec-atrib-natoper.classe:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}. */
  es-rec-atrib-natoper.cod-beneficio-icms:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  es-rec-atrib-natoper.cod-beneficio-piscof:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  es-rec-atrib-natoper.nat-operacao:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
   
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
  {src/adm/template/row-list.i "es-rec-atrib-natoper"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-rec-atrib-natoper"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    DO WITH FRAME {&FRAME-NAME}:

        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.estab-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.classe-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-ben-icms.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-ben-piscof.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.model-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-cfop.
        
    END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bes-rec-atrib-natoper FOR es-rec-atrib-natoper. 
    DEFINE VARIABLE i-cfa AS INTEGER     NO-UNDO.
    /* Code placed here will execute PRIOR to standard behavior. */
/*     {include/i-valid.i} */
    if  not frame {&frame-name}:validate() then
        return 'ADM-ERROR':U.
    
    IF NOT es-rec-atrib-natoper.estab-todos:CHECKED THEN DO:
        FOR FIRST estabelec 
            WHERE estabelec.ep-codigo  = i-ep-codigo-usuario
              AND estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-estabel NO-LOCK: END.
        IF NOT AVAIL estabelec THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 2, 
                               input "Estabelecimento~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-estabel)).
            APPLY "ENTRY" TO es-rec-atrib-natoper.cod-estabel.
            RETURN 'ADM-ERROR':U.
        END.
    END.
    IF NOT es-rec-atrib-natoper.classe-todos:CHECKED THEN DO:
        IF TRIM(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe) = "" THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 17006, 
                               input "CFA inv lido~~" + 
                                     "Quando a op‡Æo TODOS nÆo estiver selecionada ‚ necess rio informar ao menos um CFA, ou mais de um utilizando ; como separador.").
            APPLY "ENTRY" TO es-rec-atrib-natoper.classe.
            RETURN 'ADM-ERROR':U.
        END.
        DO i-cfa = 1 TO NUM-ENTRIES(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe,";"):
            FOR FIRST es-cfa 
                WHERE es-cfa.classe = ENTRY(i-cfa,INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe,";") NO-LOCK: END.
            IF NOT AVAIL es-cfa THEN DO:
                run utp/ut-msgs.p (input "show":U, 
                                   input 2, 
                                   input "Classe~~" + QUOTER(ENTRY(i-cfa,INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe,";"))).
                APPLY "ENTRY" TO es-rec-atrib-natoper.classe.
                RETURN 'ADM-ERROR':U.
            END.
        END.
    END.

    IF NOT es-rec-atrib-natoper.nenhum-ben-icms:CHECKED THEN DO:
        FOR FIRST es-beneficio 
            WHERE es-beneficio.cod-beneficio = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-icms NO-LOCK: END.
        IF NOT AVAIL es-beneficio THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 2, 
                               input "Benef¡cio ICMS~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-icms)).
            APPLY "ENTRY" TO es-rec-atrib-natoper.cod-beneficio-icms.
            RETURN 'ADM-ERROR':U.
        END.
    END.

    IF NOT es-rec-atrib-natoper.nenhum-ben-piscof:CHECKED THEN DO:
        FOR FIRST es-beneficio 
            WHERE es-beneficio.cod-beneficio = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-piscof NO-LOCK: END.
        IF NOT AVAIL es-beneficio THEN DO:
            run utp/ut-msgs.p (input "show":U, 
                               input 2, 
                               input "Benef¡cio PIS/COFINS~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-piscof)).
            APPLY "ENTRY" TO es-rec-atrib-natoper.cod-beneficio-piscof.
            RETURN 'ADM-ERROR':U.
        END.
    END.

    IF NOT es-rec-atrib-natoper.model-todos:CHECKED                      AND
       TRIM(es-rec-atrib-natoper.cod-model-nf-eletro:SCREEN-VALUE) = ""  THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Modelo NF-e/CT-e inv lido~~" + 
                                 "Quando a op‡Æo TODOS nÆo estiver selecionada ‚ necess rio informar um valor para o campo 'Modelo NF-e/CT-e'.").
        APPLY "ENTRY" TO es-rec-atrib-natoper.cod-model-nf-eletro.
        RETURN 'ADM-ERROR':U.
    END.

    IF NOT es-rec-atrib-natoper.nenhum-cfop:CHECKED                 AND
       TRIM(es-rec-atrib-natoper.cod-cfop-saida:SCREEN-VALUE) = ""  THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "CFOP Sa¡da inv lido~~" + 
                                 "Quando a op‡Æo NENHUM nÆo estiver selecionada ‚ necess rio informar ao menos um CFOP, ou mais de um utilizando ; como separador.").
        APPLY "ENTRY" TO es-rec-atrib-natoper.cod-cfop-saida.
        RETURN 'ADM-ERROR':U.
    END.

    FOR FIRST natur-oper FIELDS(nat-operacao nat-ativa)
        WHERE natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.nat-operacao NO-LOCK: END.
    IF NOT AVAIL natur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de opera‡Æo~~" + QUOTER(INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.nat-operacao)).
        APPLY "ENTRY" TO es-rec-atrib-natoper.nat-operacao.
        RETURN 'ADM-ERROR':U.
    END.
    IF NOT natur-oper.nat-ativa THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Natureza de opera‡Æo Inativa~~" + 
                                 "Natureza de opera‡Æo deve estar ativa, selecione outra.").
        APPLY "ENTRY" TO es-rec-atrib-natoper.nat-operacao.
        RETURN 'ADM-ERROR':U.
    END.
    IF INPUT FRAME {&FRAME-NAME} rs-dif-aliq AND
       INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.dif-aliq-icm = 0 THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Diferencial de Al¡quota inv lido~~" + 
                                 "Se o parƒmetro estiver 'Sim', um valor maior que zero deve ser informado.").
        APPLY "ENTRY" TO es-rec-atrib-natoper.nat-operacao.
        RETURN 'ADM-ERROR':U.
    END.
    
    FOR FIRST bes-rec-atrib-natoper FIELDS(identificador)                       
        WHERE bes-rec-atrib-natoper.ep-codigo             = i-ep-codigo-usuario
          AND bes-rec-atrib-natoper.cod-estabel           = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-estabel         
          AND bes-rec-atrib-natoper.classe                = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe              
          AND bes-rec-atrib-natoper.operacao              = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.operacao            
          AND bes-rec-atrib-natoper.cod-beneficio-icms    = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-icms  
          AND bes-rec-atrib-natoper.cod-beneficio-piscof  = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-piscof
          AND bes-rec-atrib-natoper.cod-model-nf-eletro   = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-model-nf-eletro 
          AND bes-rec-atrib-natoper.cod-cfop-saida        = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-cfop-saida
          AND bes-rec-atrib-natoper.dif-aliq-icm          = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.dif-aliq-icm
          AND bes-rec-atrib-natoper.identificador        <> INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.identificador  
          AND bes-rec-atrib-natoper.l-rateio              = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.l-rateio NO-LOCK: END.
    IF AVAIL bes-rec-atrib-natoper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Regra de Atribui‡Æo j  exitente~~" + 
                                 "J  existe uma regra de atribui‡Æo da Natureza de Opera‡Æo no recebimento, identificador " + QUOTER(bes-rec-atrib-natoper.identificador) + ", com estes dados, utilize outras informa‡äes.").

        RETURN 'ADM-ERROR':U.
    END.
/*     IF (adm-new-record AND                                                                                                                                 */
/*         CAN-FIND (FIRST bes-rec-atrib-natoper                                                                                                              */
/*                  WHERE bes-rec-atrib-natoper.ep-codigo             = i-ep-codigo-usuario                                                                   */
/*                    AND bes-rec-atrib-natoper.cod-estabel           = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-estabel                            */
/*                    AND bes-rec-atrib-natoper.classe                = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe                                 */
/*                    AND bes-rec-atrib-natoper.operacao              = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.operacao                               */
/*                    AND bes-rec-atrib-natoper.cod-beneficio-icms    = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-icms                     */
/*                    AND bes-rec-atrib-natoper.cod-beneficio-piscof  = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-piscof                   */
/*                    AND bes-rec-atrib-natoper.cod-model-nf-eletro   = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-model-nf-eletro                    */
/*                    AND bes-rec-atrib-natoper.cod-cfop-saida        = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-cfop-saida  NO-LOCK)) OR           */
/*        (NOT adm-new-record AND                                                                                                                             */
/*         CAN-FIND (FIRST bes-rec-atrib-natoper                                                                                                              */
/*                  WHERE bes-rec-atrib-natoper.ep-codigo             = i-ep-codigo-usuario                                                                   */
/*                    AND bes-rec-atrib-natoper.cod-estabel           = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-estabel                            */
/*                    AND bes-rec-atrib-natoper.classe                = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.classe                                 */
/*                    AND bes-rec-atrib-natoper.operacao              = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.operacao                               */
/*                    AND bes-rec-atrib-natoper.cod-beneficio-icms    = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-icms                     */
/*                    AND bes-rec-atrib-natoper.cod-beneficio-piscof  = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-beneficio-piscof                   */
/*                    AND bes-rec-atrib-natoper.cod-model-nf-eletro   = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-model-nf-eletro                    */
/*                    AND bes-rec-atrib-natoper.cod-cfop-saida        = INPUT FRAME {&FRAME-NAME} es-rec-atrib-natoper.cod-cfop-saida                         */
/*                    AND ROWID(bes-rec-atrib-natoper)               <> ROWID(es-rec-atrib-natoper) NO-LOCK))                                                 */
/*         THEN DO:                                                                                                                                           */
/*         run utp/ut-msgs.p (input "show":U,                                                                                                                 */
/*                            input 17006,                                                                                                                    */
/*                            input "Regra de Atribui‡Æo j  exitente~~" +                                                                                     */
/*                                  "J  existe uma regra de atribui‡Æo da Natureza de Opera‡Æo no recebimento com estes dados, utilize outras informa‡äes."). */
/*                                                                                                                                                            */
/*         RETURN 'ADM-ERROR':U.                                                                                                                              */
/*     END.                                                                                                                                                   */

    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN es-rec-atrib-natoper.identificador:SCREEN-VALUE = "0".
    END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bes-rec FOR es-rec-atrib-natoper.
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    assign es-rec-atrib-natoper.ep-codigo = i-ep-codigo-usuario.
    FIND LAST bes-rec USE-INDEX identificador 
        WHERE bes-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF AVAIL bes-rec THEN
        ASSIGN es-rec-atrib-natoper.identificador = bes-rec.identificador + 1.
    ELSE
        ASSIGN es-rec-atrib-natoper.identificador = 1.
    

    
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

    DO WITH FRAME {&FRAME-NAME}:

        APPLY "LEAVE" TO es-rec-atrib-natoper.cod-estabel.
        APPLY "LEAVE" TO es-rec-atrib-natoper.classe.
        APPLY "LEAVE" TO es-rec-atrib-natoper.cod-beneficio-icms.
        APPLY "LEAVE" TO es-rec-atrib-natoper.cod-beneficio-piscof.
        APPLY "LEAVE" TO es-rec-atrib-natoper.nat-operacao.

        IF AVAIL es-rec-atrib-natop AND es-rec-atrib-natop.dif-aliq-icm > 0 THEN
            ASSIGN rs-dif-aliq:SCREEN-VALUE = "Yes".
        ELSE
            ASSIGN rs-dif-aliq:SCREEN-VALUE = "No".

    END.
    

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
    
    DO WITH FRAME {&FRAME-NAME}:

        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.estab-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.classe-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-ben-icms.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-ben-piscof.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.model-todos.
        APPLY "VALUE-CHANGED" TO es-rec-atrib-natoper.nenhum-cfop.
        APPLY "VALUE-CHANGED" TO rs-dif-aliq.


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
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
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
  {src/adm/template/snd-list.i "es-rec-atrib-natoper"}

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

