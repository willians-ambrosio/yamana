&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cademp       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/****************************************************************************************** 
**         Programa: escn0201a-v01.P 
**        Autor: Daniela Campos
**       Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: Aba Garantia de Contrato chamado pelo programa upc\cn0201a-upc02.P
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor                   Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{include/i-prgvrs.i ESCN0201A-V01 12.01.99.999}
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

DEFINE VARIABLE i-gar AS INTEGER     NO-UNDO.

DEF BUFFER bf-contrato-for FOR contrato-for.

{utp/utapi019.i}

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
&Scoped-Define ENABLED-FIELDS contrato-for.nr-contrato 
&Scoped-define ENABLED-TABLES contrato-for
&Scoped-define FIRST-ENABLED-TABLE contrato-for
&Scoped-Define ENABLED-OBJECTS rt-mold 
&Scoped-Define DISPLAYED-FIELDS contrato-for.nr-contrato 
&Scoped-define DISPLAYED-TABLES contrato-for
&Scoped-define FIRST-DISPLAYED-TABLE contrato-for
&Scoped-Define DISPLAYED-OBJECTS cb_tp_gar d_perc1 d_perc2 dt_termo ~
d_garant dt_pagto ed_contrato ed_termo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS d_perc1 d_perc2 dt_termo dt_pagto 
&Scoped-define ADM-MODIFY-FIELDS cb_tp_gar d_perc1 d_perc2 dt_termo ~
dt_pagto 
&Scoped-define List-4 d_perc1 d_perc2 d_garant 

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

DEFINE BUTTON bt_file 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 10" 
     SIZE 4 BY .88.

DEFINE BUTTON bt_file-2 
     IMAGE-UP FILE "image/file.png":U
     LABEL "bt_file 2" 
     SIZE 4 BY .88.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cb_tp_gar AT ROW 1.25 COL 19.14 COLON-ALIGNED WIDGET-ID 10
     d_perc1 AT ROW 1.25 COL 57 COLON-ALIGNED WIDGET-ID 24
     d_perc2 AT ROW 2.25 COL 57 COLON-ALIGNED WIDGET-ID 26
     dt_termo AT ROW 2.29 COL 19.14 COLON-ALIGNED WIDGET-ID 12
     d_garant AT ROW 3.25 COL 57 COLON-ALIGNED WIDGET-ID 44
     dt_pagto AT ROW 3.33 COL 19.14 COLON-ALIGNED WIDGET-ID 14
     contrato-for.nr-contrato AT ROW 4.25 COL 63 COLON-ALIGNED WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 7.57 BY .79
     ed_contrato AT ROW 6.25 COL 8 NO-LABEL WIDGET-ID 50
     bt_file-2 AT ROW 6.25 COL 74 WIDGET-ID 48
     bt-view-2 AT ROW 7.21 COL 74 WIDGET-ID 46
     ed_termo AT ROW 9.38 COL 8 NO-LABEL WIDGET-ID 16
     bt_file AT ROW 9.38 COL 74 WIDGET-ID 22
     bt-view AT ROW 10.38 COL 74 WIDGET-ID 28
     "Arquivo contrato:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 5.5 COL 9 WIDGET-ID 52
     "Arquivo termo de encerramento:" VIEW-AS TEXT
          SIZE 22.86 BY .67 AT ROW 8.5 COL 8 WIDGET-ID 18
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
/* SETTINGS FOR BUTTON bt_file IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt_file-2 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb_tp_gar IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN dt_pagto IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN dt_termo IN FRAME f-main
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR FILL-IN d_garant IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN d_perc1 IN FRAME f-main
   NO-ENABLE 2 3 4                                                      */
/* SETTINGS FOR FILL-IN d_perc2 IN FRAME f-main
   NO-ENABLE 2 3 4                                                      */
/* SETTINGS FOR EDITOR ed_contrato IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       ed_contrato:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR EDITOR ed_termo IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       ed_termo:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       contrato-for.nr-contrato:HIDDEN IN FRAME f-main           = TRUE.

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


&Scoped-define SELF-NAME bt_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_file V-table-Win
ON CHOOSE OF bt_file IN FRAME f-main /* Button 10 */
DO:
  
  c_file = ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    
  SYSTEM-DIALOG GET-FILE c_file
       TITLE "Termo de Encerramento".

  ASSIGN ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c_file.

  RUN pi_habi_bts_view.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_file-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_file-2 V-table-Win
ON CHOOSE OF bt_file-2 IN FRAME f-main /* bt_file 2 */
DO:
  
  c_file_2 = ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    
  SYSTEM-DIALOG GET-FILE c_file_2
       TITLE "Contrato".

  ASSIGN ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c_file_2.

  RUN pi_habi_bts_view.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_tp_gar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_tp_gar V-table-Win
ON VALUE-CHANGED OF cb_tp_gar IN FRAME f-main /* Tipo Garantia */
DO:
  ASSIGN i-gar = INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  CASE INPUT FRAME {&FRAME-NAME} cb_tp_gar:
      
      WHEN "1" THEN DO:
          DISABLE d_garant
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_perc1 d_perc2 dt_termo dt_pagto bt_file bt_file-2
                 WITH FRAME {&FRAME-NAME}.                
         ASSIGN d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0,00"
                d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for THEN STRING(ext-contrato-for.perc_1) ELSE "").
                d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for THEN STRING(ext-contrato-for.perc_2) ELSE "").

      END.
      WHEN "2" THEN DO:
          DISABLE d_perc1 d_perc2 
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_garant dt_termo dt_pagto bt_file bt_file-2
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar = i-gar THEN STRING(ext-contrato-for.val_garantia) ELSE "").
      END.
      WHEN "3" THEN DO:
          DISABLE d_perc1 d_perc2 
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_garant dt_termo dt_pagto bt_file bt_file-2
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar = i-gar THEN STRING(ext-contrato-for.val_garantia) ELSE "").
      END.
      OTHERWISE DO:
          DISABLE d_perc1 d_perc2 dt_termo dt_pagto bt_file bt_file-2 d_garant
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN 
        d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        dt_pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".     
      END.
  END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dt_termo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt_termo V-table-Win
ON LEAVE OF dt_termo IN FRAME f-main /* Dt Termo Encerramento */
DO:
  IF INPUT FRAME {&frame-name} dt_termo  <> ? THEN
      ASSIGN dt_pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&frame-name} dt_termo + 30).
  ELSE ASSIGN dt_pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF AVAIL ext-contrato-for THEN RELEASE ext-contrato-for NO-ERROR.

            ASSIGN cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "0"
                   d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = "0.00"
                   d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = "0.00"
                   dt_pagto:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ?
                   dt_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ?
                   ed_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ""
                   d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = "0.00"
                   ed_contrato:SCREEN-VALUE  IN FRAME {&FRAME-NAME}= "".

    enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.

    APPLY "VALUE-CHANGE" TO cb_tp_gar. 
    
    RUN pi_habi_bts_view.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
         return 'ADM-ERROR':U.

                                                                 
     /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .


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
            
        IF adm-new-record OR 
          (AVAIL ext-contrato-for AND ext-contrato-for.pendente = YES OR 
           NOT AVAIL ext-contrato-for)
            THEN ENABLE {&ADM-MODIFY-FIELDS} with frame {&frame-name}.

       IF AVAIL ext-contrato-for AND 
                ext-contrato-for.ind-status > 1 THEN
          DISABLE {&ADM-MODIFY-FIELDS} with frame {&frame-name}.

    &endif
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-assign V-table-Win 
PROCEDURE pi-assign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER r-row-contrato AS ROWID       NO-UNDO.
      
   
      

    FIND bf-contrato-for NO-LOCK WHERE
         ROWID(bf-contrato-for) = r-row-contrato NO-ERROR.

    IF AVAIL bf-contrato-for AND INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 THEN DO:

       FIND ext-contrato-for WHERE
             ext-contrato-for.nr-contrato = bf-contrato-for.nr-contrato NO-ERROR.
       IF NOT AVAIL ext-contrato-for THEN DO:
            CREATE ext-contrato-for.
            ASSIGN ext-contrato-for.nr-contrato = bf-contrato-for.nr-contrato.
       END.

        ASSIGN ext-contrato-for.arq_termo       = ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
               ext-contrato-for.arq_contrato    = ed_contrato:SCREEN-VALUE IN FRAME {&FRAME-NAME}
               ext-contrato-for.dt_pg_ret       = INPUT FRAME {&FRAME-NAME} dt_pagto
               ext-contrato-for.dt_termo_enc    = INPUT FRAME {&FRAME-NAME} dt_termo
               ext-contrato-for.ind_tip_gar     = INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME})
               ext-contrato-for.ind-status      = 1  /* 1-Vigente, 2-Termo Enc, 3-Reten‡Æo Liberada, 4-Processo Jud */
               ext-contrato-for.perc_1          = INPUT FRAME {&FRAME-NAME} d_perc1
               ext-contrato-for.perc_2          = INPUT FRAME {&FRAME-NAME} d_perc2
               ext-contrato-for.val_garantia    = DEC(INPUT FRAME {&FRAME-NAME} d_garant)
               ext-contrato-for.pendente        = YES /* At‚ que o contrato seja encerrado e liberado pela c‚lula, ficar  pendente */
               .
        IF INPUT FRAME {&FRAME-NAME} dt_termo <> ? THEN DO:

            ASSIGN ext-contrato-for.usuar_liber  = c-seg-usuario
                   ext-contrato-for.dt_liber     = TODAY.

            RUN pi-send-email. /* Envia e-mail para a c‚lula de contratos comunicando do encerramento do contrato */
        END.
    END.
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display V-table-Win 
PROCEDURE pi-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER r-row-contrato AS ROWID       NO-UNDO.

    FIND bf-contrato-for NO-LOCK WHERE
         ROWID(bf-contrato-for) = r-row-contrato NO-ERROR.

    ASSIGN  d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ""
            ed_contrato:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = "".

    IF AVAIL bf-contrato-for THEN DO:

        FIND FIRST ext-contrato-for OF contrato-for NO-LOCK NO-ERROR.
        IF AVAIL ext-contrato-for THEN
            ASSIGN cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.ind_tip_gar)
                   d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.perc_1)
                   d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.perc_2)
                   dt_pagto:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.dt_pg_ret)
                   dt_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.dt_termo_enc)
                   ed_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = ext-contrato-for.arq_termo
                   d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(ext-contrato-for.val_garantia)
                   ed_contrato:SCREEN-VALUE  IN FRAME {&FRAME-NAME}= ext-contrato-for.arq_contrato.

         APPLY "VALUE-CHANGED" TO cb_tp_gar.

         IF bf-contrato-for.ind-sit-contrat = 2 /* emitido */ 
            THEN DISABLE {&list-4}  with frame {&frame-name}.
         IF AVAIL ext-contrato-for AND NOT ext-contrato-for.pendente THEN
             DISABLE {&list-4}  {&ADM-MODIFY-FIELDS} with frame {&frame-name}.


    END.

    RUN pi_habi_bts_view.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-passa-handle V-table-Win 
PROCEDURE pi-passa-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER c-handle AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER h-handle AS HANDLE      NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-send-email V-table-Win 
PROCEDURE pi-send-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
    FIND FIRST param_email NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2.

    FOR EACH es_param_emp NO-LOCK 
        WHERE es_param_emp.cod_empresa    = v_cod_empres_usuar
        AND   es_param_emp.cod_prog_dtsul = "CN0201"
        AND   es_param_emp.cod_referencia BEGINS "email_contrato":
         
         /* Verifica a existencia da planilha em excel gerada pela procedure pi-report-excel*/
         /* Ser  enviado pelo anexo para o usuario */
             
         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.

         CREATE tt-envio2.
         ASSIGN tt-envio2.versao-integracao = 1
                tt-envio2.exchange          = param_email.log_servid_exchange
                tt-envio2.servidor          = param_email.cod_servid_e_mail
                tt-envio2.porta             = param_email.num_porta
                tt-envio2.destino           = es_param_emp.cod_parametro
                tt-envio2.assunto           = "Comunicado de encerramento de contrato"
                tt-envio2.remetente         = "encerramento_contrato@yamana.com"
                tt-envio2.copia             = ""
                tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) +
                                              "O contrato nr " + STRING(ext-contrato-for.nr-contrato) + " foi encerrado em " + 
                                               STRING(ext-contrato-for.dt_termo_enc,"99/99/9999") + "." + CHR(13) + 
                                              "Segue em anexo o contrato e termo de encerramento." + "." + CHR(13) + CHR(13) + 
                                              "Atenciosamente," + CHR(13) + CHR(13) + "Suprimentos Yamana"    
                tt-envio2.importancia       = 1
                tt-envio2.log-enviada       = NO 
                tt-envio2.log-lida          = NO 
                tt-envio2.acomp             = NO.
/*                 tt-envio2.arq-anexo         = ext-contrato-for.arq_termo + "," + ext-contrato-for.arq_contrato. */
         CREATE tt-mensagem.
         ASSIGN tt-mensagem.seq-mensagem = 1
                tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13) +                                           
                                           "O contrato nr " + STRING(ext-contrato-for.nr-contrato) + " foi encerrado em " +   
                                            STRING(ext-contrato-for.dt_termo_enc,"99/99/9999") + "." + CHR(13) +                
                                           "Segue em anexo o contrato e termo de encerramento." + "." + CHR(13) + CHR(13) +   
                                           "Atenciosamente," + CHR(13) + CHR(13) + "Suprimentos Yamana".                    
                                           
         RUN utp/utapi019.p PERSISTENT SET h-utapi019.

         RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,
                                       OUTPUT TABLE tt-erros).
         FOR EACH tt-erros:

             MESSAGE  "Falha ao enviar e-mail" SKIP
                      "Cod. Erro: " tt-erros.cod-erro  SKIP
                      tt-erros.desc-erro
                      tt-erros.desc-arq 
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
    
         DELETE PROCEDURE h-utapi019.     
    END.
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
   
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

IF INPUT FRAME {&FRAME-NAME} cb_tp_gar <> "" THEN DO:

    IF INPUT FRAME {&FRAME-NAME} dt_termo <> ? THEN DO:
    
        IF INPUT FRAME {&FRAME-NAME} dt_pagto = ? THEN DO:
    
            MESSAGE "Data para pagamento da reten‡Æo deve ser informada!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
            APPLY "ENTRY" TO dt_pagto.
            RETURN "adm-error".
        END.
    
        IF SEARCH(ed_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = ? THEN DO:
    
            MESSAGE "NÆo encontrado arquivo DO termo de encerramento!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "adm-error".
        END.
    END.

    IF INPUT FRAME {&FRAME-NAME} cb_tp_gar = "1" THEN DO:
        IF INPUT FRAME {&frame-name} d_perc1 = 0 AND INPUT FRAME {&frame-name} d_perc2 = 0 THEN DO:
        
            MESSAGE "Percentual de reten‡Æo dever  ser informado!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
            APPLY "ENTRY" TO d_perc1.
        
            RETURN "adm-error".
        END.
    END.
    IF INPUT FRAME {&FRAME-NAME} cb_tp_gar = "2" OR INPUT FRAME {&FRAME-NAME} cb_tp_gar = "3" THEN DO:
        IF INPUT FRAME {&frame-name} d_garant = 0 THEN DO:
        
            MESSAGE "Valor da Garantia dever  ser informado!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
            APPLY "ENTRY" TO d_garant.
        
            RETURN "adm-error".
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_habi_bts_view V-table-Win 
PROCEDURE pi_habi_bts_view :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    IF ed_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME} <> "" THEN DO:
        ENABLE bt-view 
               WITH FRAME {&FRAME-NAME}.
    END.
    IF ed_contrato:SCREEN-VALUE  IN FRAME {&FRAME-NAME} <> "" THEN DO:
        ENABLE bt-view-2 
               WITH FRAME {&FRAME-NAME}.
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

