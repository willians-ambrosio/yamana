&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cademp       PROGRESS
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
{utp/ut-glob.i}
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
DEFINE VARIABLE c_file AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_file_2 AS CHARACTER   NO-UNDO.

DEF BUFFER bf-contrato-for FOR contrato-for.

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
&Scoped-define EXTERNAL-TABLES contrato-for
&Scoped-define FIRST-EXTERNAL-TABLE contrato-for


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contrato-for.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS contrato-for.nr-contrato 
&Scoped-define DISPLAYED-TABLES contrato-for
&Scoped-define FIRST-DISPLAYED-TABLE contrato-for
&Scoped-Define DISPLAYED-OBJECTS cb_tp_gar d_perc1 d_perc2 dt_termo ~
d_garant dt_pagto tl-processo ed_termo ed_contrato 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS cb_tp_gar d_perc1 d_perc2 dt_termo ~
dt_pagto 
&Scoped-define ADM-MODIFY-FIELDS cb_tp_gar d_perc1 d_perc2 dt_termo ~
dt_pagto 

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
DEFINE BUTTON bt-view 
     IMAGE-UP FILE "adeicon/browse-u.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-view-2 
     IMAGE-UP FILE "adeicon/browse-u.bmp":U
     LABEL "bt view 2" 
     SIZE 4 BY 1.

DEFINE VARIABLE cb_tp_gar AS CHARACTER FORMAT "X(20)":U INITIAL "0" 
     LABEL "Tipo Garantia" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "","0",
                     "Cau‡Æo","1",
                     "Seguro","2",
                     "Carta Fian‡a","3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ed_contrato AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 66 BY 2 NO-UNDO.

DEFINE VARIABLE ed_termo AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 66 BY 2 NO-UNDO.

DEFINE VARIABLE dt_pagto AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Pagto Reten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_termo AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Termo Encerramento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d_garant AS DECIMAL FORMAT "->,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor da Garantia" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d_perc1 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "% Reten‡Æo 1§ Parcela" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE d_perc2 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "% Reten‡Æo demais parcelas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 11.

DEFINE VARIABLE tl-processo AS LOGICAL INITIAL no 
     LABEL "Processo Judicial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cb_tp_gar AT ROW 1.25 COL 19.14 COLON-ALIGNED WIDGET-ID 10
     d_perc1 AT ROW 1.25 COL 57 COLON-ALIGNED WIDGET-ID 24
     d_perc2 AT ROW 2.25 COL 57 COLON-ALIGNED WIDGET-ID 26
     dt_termo AT ROW 2.29 COL 19.14 COLON-ALIGNED WIDGET-ID 12
     d_garant AT ROW 3.25 COL 57 COLON-ALIGNED WIDGET-ID 44
     dt_pagto AT ROW 3.33 COL 19 COLON-ALIGNED WIDGET-ID 14
     tl-processo AT ROW 4.5 COL 21 WIDGET-ID 54
     contrato-for.nr-contrato AT ROW 4.58 COL 60 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 7.57 BY .79
     ed_termo AT ROW 6.29 COL 8 NO-LABEL WIDGET-ID 16
     bt-view AT ROW 7.33 COL 74 WIDGET-ID 28
     ed_contrato AT ROW 9.25 COL 8 NO-LABEL WIDGET-ID 50
     bt-view-2 AT ROW 10.29 COL 74 WIDGET-ID 46
     "Arquivo contrato:" VIEW-AS TEXT
          SIZE 22.86 BY .67 AT ROW 8.5 COL 8.14 WIDGET-ID 52
     "Arquivo termo de encerramento:" VIEW-AS TEXT
          SIZE 22.86 BY .67 AT ROW 5.5 COL 9 WIDGET-ID 18
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems2cademp.contrato-for
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
         HEIGHT             = 11.08
         WIDTH              = 78.14.
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

/* SETTINGS FOR BUTTON bt-view IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-view-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb_tp_gar IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN dt_pagto IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN dt_termo IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN d_garant IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d_perc1 IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN d_perc2 IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR EDITOR ed_contrato IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       ed_contrato:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR EDITOR ed_termo IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       ed_termo:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN contrato-for.nr-contrato IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       contrato-for.nr-contrato:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tl-processo IN FRAME f-main
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

&Scoped-define SELF-NAME bt-view
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-view V-table-Win
ON CHOOSE OF bt-view IN FRAME f-main /* Button 11 */
DO:
  
    IF ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN
    RUN prgfin\apl\apya599.p (INPUT ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-view-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-view-2 V-table-Win
ON CHOOSE OF bt-view-2 IN FRAME f-main /* bt view 2 */
DO:

    IF ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN
    RUN prgfin\apl\apya599.p (INPUT ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_tp_gar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_tp_gar V-table-Win
ON VALUE-CHANGED OF cb_tp_gar IN FRAME f-main /* Tipo Garantia */
DO:
END.


/* ,0,            */
/* Cau‡Æo,1,      */
/* Seguro,2,      */
/* Carta Fian‡a,3 */

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
  {src/adm/template/row-list.i "contrato-for"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contrato-for"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-record V-table-Win 
PROCEDURE local-display-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER row-param-controle-for AS ROWID. 


    FIND FIRST bf-contrato-for WHERE ROWID(bf-contrato-for) = row-param-controle-for NO-LOCK NO-ERROR.
    IF AVAIL bf-contrato-for THEN DO:

        FIND FIRST ext-contrato-for OF bf-contrato-for NO-LOCK NO-ERROR.
        IF AVAIL ext-contrato-for THEN
            ASSIGN cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.ind_tip_gar)
                   d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.perc_1)
                   d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.perc_2)
                   dt_pagto:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.dt_pg_ret)
                   dt_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.dt_termo_enc)
                   ed_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ext-contrato-for.arq_termo
                   d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.val_garantia)
                   tl-processo:CHECKED IN FRAME {&FRAME-NAME}      = (IF ext-contrato-for.ind-status = 9 THEN YES ELSE NO)
                   ed_contrato:SCREEN-VALUE  IN FRAME {&FRAME-NAME}= ext-contrato-for.arq_contrato.
    END.
    ELSE ASSIGN cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "0"
                d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = "0.00"
                d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = "0.00"
                dt_pagto:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ?
                dt_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ?
                ed_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ""
                d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = "0.00"
                tl-processo:CHECKED IN FRAME {&FRAME-NAME}      =  NO
                ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    IF ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:
    ENABLE bt-view
        WITH FRAME {&FRAME-NAME}.
    END.
    IF ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:
        ENABLE bt-view-2
            WITH FRAME {&FRAME-NAME}.
    END.


    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-record':U ) .


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
   
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

/*   RUN local-display-record. */



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
  {src/adm/template/snd-list.i "contrato-for"}

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

