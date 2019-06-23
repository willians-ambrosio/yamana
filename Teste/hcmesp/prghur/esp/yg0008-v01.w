&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          hresp            PROGRESS
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

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

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




def var i_cdn_empresa  like funcionario.cdn_empresa.
def var i_cdn_estab    like funcionario.cdn_estab.

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
&Scoped-define EXTERNAL-TABLES es_categ_ptoelet
&Scoped-define FIRST-EXTERNAL-TABLE es_categ_ptoelet


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_categ_ptoelet.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_categ_ptoelet.cdn_efp_horas_in_itinere ~
es_categ_ptoelet.qtd_domingo es_categ_ptoelet.qtd_segunda ~
es_categ_ptoelet.qtd_terca es_categ_ptoelet.qtd_quarta ~
es_categ_ptoelet.qtd_quinta es_categ_ptoelet.qtd_sexta ~
es_categ_ptoelet.qtd_sabado es_categ_ptoelet.calc_pto_turno_dif 
&Scoped-define ENABLED-TABLES es_categ_ptoelet
&Scoped-define FIRST-ENABLED-TABLE es_categ_ptoelet
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold rt-mold-2 
&Scoped-Define DISPLAYED-FIELDS es_categ_ptoelet.cdn_empresa ~
es_categ_ptoelet.cdn_estab es_categ_ptoelet.cdn_clas_func ~
es_categ_ptoelet.cdn_categ_sal es_categ_ptoelet.cdn_efp_horas_in_itinere ~
es_categ_ptoelet.qtd_domingo es_categ_ptoelet.qtd_segunda ~
es_categ_ptoelet.qtd_terca es_categ_ptoelet.qtd_quarta ~
es_categ_ptoelet.qtd_quinta es_categ_ptoelet.qtd_sexta ~
es_categ_ptoelet.qtd_sabado es_categ_ptoelet.calc_pto_turno_dif 
&Scoped-define DISPLAYED-TABLES es_categ_ptoelet
&Scoped-define FIRST-DISPLAYED-TABLE es_categ_ptoelet
&Scoped-Define DISPLAYED-OBJECTS fi-empresa fi-estab fi-classe fi-categ ~
fi-evento 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_categ_ptoelet.cdn_empresa ~
es_categ_ptoelet.cdn_estab es_categ_ptoelet.cdn_clas_func ~
es_categ_ptoelet.cdn_categ_sal 

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
DEFINE VARIABLE fi-categ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-classe AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-evento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46.14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.29.

DEFINE RECTANGLE rt-mold-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_categ_ptoelet.cdn_empresa AT ROW 1.17 COL 25 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-empresa AT ROW 1.17 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     es_categ_ptoelet.cdn_estab AT ROW 2.17 COL 25 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-estab AT ROW 2.17 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     es_categ_ptoelet.cdn_clas_func AT ROW 3.17 COL 25 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-classe AT ROW 3.17 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     es_categ_ptoelet.cdn_categ_sal AT ROW 4.17 COL 25 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-categ AT ROW 4.17 COL 35.29 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     es_categ_ptoelet.cdn_efp_horas_in_itinere AT ROW 5.67 COL 25 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     fi-evento AT ROW 5.67 COL 32.86 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     es_categ_ptoelet.qtd_domingo AT ROW 7.17 COL 25 COLON-ALIGNED WIDGET-ID 12
          LABEL "Domingo"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_segunda AT ROW 8.17 COL 25 COLON-ALIGNED WIDGET-ID 20
          LABEL "Segunda-Feira"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_terca AT ROW 9.17 COL 25 COLON-ALIGNED WIDGET-ID 24
          LABEL "Ter‡a-Feira"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_quarta AT ROW 10.17 COL 25 COLON-ALIGNED WIDGET-ID 14
          LABEL "Quarta-Feira"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_quinta AT ROW 7.17 COL 58 COLON-ALIGNED WIDGET-ID 16
          LABEL "Quinta-Feira"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_sexta AT ROW 8.17 COL 58 COLON-ALIGNED WIDGET-ID 22
          LABEL "Sexta-Feira"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.qtd_sabado AT ROW 9.17 COL 58 COLON-ALIGNED WIDGET-ID 18
          LABEL "S bado"
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     es_categ_ptoelet.calc_pto_turno_dif AT ROW 10.13 COL 50.86 WIDGET-ID 46
          VIEW-AS TOGGLE-BOX
          SIZE 35 BY 1
     "Horas Desconsideradas" VIEW-AS TEXT
          SIZE 18.29 BY .67 AT ROW 6.79 COL 1.72 WIDGET-ID 42
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.46 COL 1
     rt-mold-2 AT ROW 7 COL 1 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: hresp.es_categ_ptoelet
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
         HEIGHT             = 10.38
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN es_categ_ptoelet.cdn_categ_sal IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.cdn_clas_func IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.cdn_empresa IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.cdn_estab IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-categ IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-classe IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-evento IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_domingo IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_quarta IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_quinta IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_sabado IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_segunda IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_sexta IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es_categ_ptoelet.qtd_terca IN FRAME f-main
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

&Scoped-define SELF-NAME es_categ_ptoelet.cdn_categ_sal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_categ_sal V-table-Win
ON F5 OF es_categ_ptoelet.cdn_categ_sal IN FRAME f-main /* Categoria Salarial */
DO:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py029.w"
                      &campo= es_categ_ptoelet.cdn_categ_sal
                      &campozoom=cdn_categ_sal}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_categ_sal V-table-Win
ON LEAVE OF es_categ_ptoelet.cdn_categ_sal IN FRAME f-main /* Categoria Salarial */
DO:
   ASSIGN i_cdn_empresa = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa
          i_cdn_estab   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab.


   {include/leave.i &tabela=categ_sal
                    &atributo-ref=des_categ_sal
                    &variavel-ref=fi-categ
                    &where="categ_sal.cdn_empresa   = i_cdn_empresa and 
                            categ_sal.cdn_estab     = i_cdn_estab   and
                            categ_sal.cdn_categ_sal = input frame {&frame-name} es_categ_ptoelet.cdn_categ_sal"}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_categ_sal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_categ_ptoelet.cdn_categ_sal IN FRAME f-main /* Categoria Salarial */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_categ_ptoelet.cdn_clas_func
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_clas_func V-table-Win
ON F5 OF es_categ_ptoelet.cdn_clas_func IN FRAME f-main /* Classe Funcion rio */
DO:
   {include/zoomvar.i &prog-zoom="object/sotm/zoom/z01tm037.w"
                      &campo= es_categ_ptoelet.cdn_clas_func
                      &campozoom=cdn_clas_func
                      &campo2= fi-classe
                      &campozoom2=des_clas_func
                      &campo3= es_categ_ptoelet.cdn_categ_sal
                      &campozoom3=cs-codigo                      
                      &campo4= fi-categ
                      &campozoom4=descricao       
                      &campo5= es_categ_ptoelet.cdn_estab
                      &campozoom5=cdn_estab                             
       }  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_clas_func V-table-Win
ON LEAVE OF es_categ_ptoelet.cdn_clas_func IN FRAME f-main /* Classe Funcion rio */
DO:
  {include/leave.i &tabela=clas_func
                   &atributo-ref=des_clas_func
                   &variavel-ref=fi-classe
                   &where="clas_func.cdn_clas_func = input frame {&frame-name} es_categ_ptoelet.cdn_clas_func"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_clas_func V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_categ_ptoelet.cdn_clas_func IN FRAME f-main /* Classe Funcion rio */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_categ_ptoelet.cdn_efp_horas_in_itinere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_efp_horas_in_itinere V-table-Win
ON ENTRY OF es_categ_ptoelet.cdn_efp_horas_in_itinere IN FRAME f-main /* Evento Horas In Intinere */
DO:
  APPLY "leave" TO es_categ_ptoelet.cdn_estab IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_efp_horas_in_itinere V-table-Win
ON F5 OF es_categ_ptoelet.cdn_efp_horas_in_itinere IN FRAME f-main /* Evento Horas In Intinere */
DO:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py067.w"
                      &campo=es_categ_ptoelet.cdn_efp_horas_in_itinere
                      &campozoom=cdn_event_fp
                      &campo2=fi-evento
                      &campozoom2=des_event_fp} 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_efp_horas_in_itinere V-table-Win
ON LEAVE OF es_categ_ptoelet.cdn_efp_horas_in_itinere IN FRAME f-main /* Evento Horas In Intinere */
DO:
   {include/leave.i &tabela=event_fp
                    &atributo-ref=des_event_fp
                    &variavel-ref=fi-evento
                    &where="event_fp.cdn_event_fp = input frame {&frame-name} es_categ_ptoelet.cdn_efp_horas_in_itinere"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_efp_horas_in_itinere V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_categ_ptoelet.cdn_efp_horas_in_itinere IN FRAME f-main /* Evento Horas In Intinere */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_categ_ptoelet.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_empresa V-table-Win
ON F5 OF es_categ_ptoelet.cdn_empresa IN FRAME f-main /* Empresa */
DO:
   {include/zoomvar.i &prog-zoom=object/sopy/zoom/z04py197.w
                      
                      &campo=es_categ_ptoelet.cdn_empresa
                      &campozoom=ep-codigo
                      &campo2=fi-empresa
                      &campozoom2=razao-social}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_empresa V-table-Win
ON LEAVE OF es_categ_ptoelet.cdn_empresa IN FRAME f-main /* Empresa */
DO:
   {include/leave.i &tabela=mgcad.empresa
                    &atributo-ref=razao-social
                    &variavel-ref=fi-empresa
                    &where="empresa.ep-codigo = input frame {&frame-name} es_categ_ptoelet.cdn_empresa"}

   ASSIGN i_cdn_empresa = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa
          i_cdn_estab   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_categ_ptoelet.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_categ_ptoelet.cdn_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_estab V-table-Win
ON F5 OF es_categ_ptoelet.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
      {include/zoomvar.i &prog-zoom  = "object\sopy\zoom\z01py060.w"
                         &campo      = es_categ_ptoelet.cdn_estab
                         &campozoom  = cdn_estab
                         &campo2     = fi-estab
                         &campozoom2 = nom_pessoa_jurid}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_estab V-table-Win
ON LEAVE OF es_categ_ptoelet.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  {include/leave.i &tabela=rh_estab
                   &atributo-ref=nom_pessoa_jurid
                   &variavel-ref=fi-estab
                   &where="rh_estab.cdn_empresa = v_cdn_empres_usuar and 
                           rh_estab.cdn_estab = input frame {&frame-name} es_categ_ptoelet.cdn_estab"}
    
    
    ASSIGN i_cdn_empresa = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa
           i_cdn_estab   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_categ_ptoelet.cdn_estab V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_categ_ptoelet.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  APPLY "f5" TO SELF.
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
    es_categ_ptoelet.cdn_empresa:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
    es_categ_ptoelet.cdn_estab:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
    es_categ_ptoelet.cdn_clas_func:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
    es_categ_ptoelet.cdn_categ_sal:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
    es_categ_ptoelet.cdn_efp_horas_in_itinere:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "es_categ_ptoelet"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_categ_ptoelet"}

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
    
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN es_categ_ptoelet.cdn_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_cdn_empres_usuar.

    APPLY "leave" TO es_categ_ptoelet.cdn_empresa IN FRAME {&FRAME-NAME}.

    es_categ_ptoelet.cdn_empresa:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

        

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
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate.

    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    APPLY "leave" TO es_categ_ptoelet.cdn_empresa   IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es_categ_ptoelet.cdn_estab     IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es_categ_ptoelet.cdn_clas_func IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es_categ_ptoelet.cdn_categ_sal IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO es_categ_ptoelet.cdn_efp_horas_in_itinere IN FRAME {&FRAME-NAME}.




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


   IF adm-new-record THEN DO:
      IF NOT CAN-FIND(FIRST mgcad.empresa
                      WHERE empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hcm empresa 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.

      IF NOT CAN-FIND(FIRST rh_estab
                      WHERE rh_estab.cdn_estab = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hcm rh_estab 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.


      IF NOT CAN-FIND(FIRST clas_func
                      WHERE clas_func.cdn_clas_func = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_clas_func
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hcm clas_func 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.

      IF NOT CAN-FIND(FIRST categ_sal
                      WHERE categ_sal.cdn_empresa   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa   AND  
                            categ_sal.cdn_estab     = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab     AND 
                            categ_sal.cdn_categ_sal = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_categ_sal
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hcm categ_sal 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.

      IF NOT CAN-FIND(FIRST categ_ptoelet
                      WHERE categ_ptoelet.cdn_empresa   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa   AND  
                            categ_ptoelet.cdn_estab     = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab     AND 
                            categ_ptoelet.cdn_clas_func = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_clas_func AND
                            categ_ptoelet.cdn_categ_sal = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_categ_sal
                      NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hcm categ_ptoelet 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.

      IF CAN-FIND(FIRST es_categ_ptoelet
                  WHERE es_categ_ptoelet.cdn_empresa   = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_empresa   AND  
                        es_categ_ptoelet.cdn_estab     = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_estab     AND 
                        es_categ_ptoelet.cdn_clas_func = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_clas_func AND
                        es_categ_ptoelet.cdn_categ_sal = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_categ_sal
                   NO-LOCK) THEN DO:
         {include/i-vldprg.i}                                            
         {utp/ut-table.i hresp es_categ_ptoelet 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 1, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.                                           
      END.
   END.

   IF NOT CAN-FIND(FIRST event_fp
                   WHERE event_fp.cdn_event_fp = INPUT FRAME {&FRAME-NAME} es_categ_ptoelet.cdn_efp_horas_in_itinere
                   NO-LOCK) THEN DO:
      {include/i-vldprg.i}                                            
      {utp/ut-table.i hcm event_fp 1}
      RUN utp/ut-msgs.p (INPUT "show":U, INPUT 2, INPUT RETURN-VALUE).
      RETURN 'ADM-ERROR':U.                                           

   END.
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
  {src/adm/template/snd-list.i "es_categ_ptoelet"}

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

