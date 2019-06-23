&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

{include/i-prgvrs.i van002-w01 1.00.00.000}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE NEW GLOBAL SHARED VARIABLE c-cod_cta_corren     as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE c-tip_trans_cx       as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE c-cod_finalid_econ   as char no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE i-psdid              as int  no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE i-cdn_parcei_edi     as int  no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE l-aux-salvar         as INT  no-undo.

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_tip_trans_cx AS RECID NO-UNDO.

DEF BUFFER b-cta_corren FOR cta_corren.

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 RECT-2 c_tip_trans_cx ~
bt-zoom c_cod_finalid_econ i_psdid i_cdn_parcei_edi Btn_OK Btn_Cancel ~
Btn_excluir Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS f_cod_cta_corren c_tip_trans_cx ~
c_cod_finalid_econ i_psdid i_cdn_parcei_edi 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-zoom 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "" 
     SIZE 3 BY .88.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 12 BY 1.13.

DEFINE BUTTON Btn_excluir AUTO-END-KEY 
     LABEL "Excluir" 
     SIZE 12 BY 1.13.

DEFINE BUTTON Btn_Help 
     LABEL "Ajuda" 
     SIZE 12 BY 1.13.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 12 BY 1.13.

DEFINE VARIABLE c_cod_finalid_econ AS CHARACTER FORMAT "x(10)" 
     LABEL "Cod finalidade econ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c_tip_trans_cx AS CHARACTER FORMAT "x(8)" 
     LABEL "Tipo Trans Cx" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE f_cod_cta_corren AS CHARACTER FORMAT "x(30)" 
     LABEL "Conta Corrente" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88 NO-UNDO.

DEFINE VARIABLE i_cdn_parcei_edi AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Parceiro EDI" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE i_psdid AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "PSDID" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.72 BY 2.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.72 BY 6.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 62 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     f_cod_cta_corren AT ROW 1.63 COL 16 COLON-ALIGNED WIDGET-ID 8
     c_tip_trans_cx AT ROW 4.42 COL 23.29 COLON-ALIGNED WIDGET-ID 10
     bt-zoom AT ROW 4.42 COL 34.57 WIDGET-ID 18
     c_cod_finalid_econ AT ROW 5.42 COL 23.29 COLON-ALIGNED WIDGET-ID 12
     i_psdid AT ROW 6.42 COL 23.29 COLON-ALIGNED WIDGET-ID 14
     i_cdn_parcei_edi AT ROW 7.42 COL 23.29 COLON-ALIGNED WIDGET-ID 16
     Btn_OK AT ROW 10.71 COL 2.72
     Btn_Cancel AT ROW 10.71 COL 15.72
     Btn_excluir AT ROW 10.71 COL 28.72 WIDGET-ID 22
     Btn_Help AT ROW 10.71 COL 51.14
     rt-button AT ROW 10.54 COL 1.86 WIDGET-ID 2
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 4
     RECT-2 AT ROW 3.42 COL 1 WIDGET-ID 6
     SPACE(0.27) SKIP(1.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 7 
         TITLE "PSDID"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN f_cod_cta_corren IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* PSDID */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-zoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom gDialog
ON CHOOSE OF bt-zoom IN FRAME gDialog
DO:
  APPLY "f5" TO self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-zoom gDialog
ON F5 OF bt-zoom IN FRAME gDialog
DO:

   run prgfin/cmg/cmg003kb.p.
   if  v_rec_tip_trans_cx <> ?
    then do:
        find tip_trans_cx where recid(tip_trans_cx) = v_rec_tip_trans_cx no-lock no-error.
        assign c_tip_trans_cx:screen-value in frame {&FRAME-NAME} =
               string(tip_trans_cx.cod_tip_trans_cx).

    end /* if */.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel gDialog
ON CHOOSE OF Btn_Cancel IN FRAME gDialog /* Cancelar */
DO:
  
    ASSIGN c-tip_trans_cx       = ""
           c-cod_finalid_econ   = ""
           i-psdid              = 0
           i-cdn_parcei_edi     = 0
           l-aux-salvar         = 2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_excluir gDialog
ON CHOOSE OF Btn_excluir IN FRAME gDialog /* Excluir */
DO:
  
  IF CAN-FIND(FIRST es_param_van_dir
              WHERE es_param_van_dir.psdid = i-psdid) THEN DO:
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro possui relacionamento!~~Favor verificar o Cadastro de parƒmetros da Van.").

     RETURN NO-APPLY.
  END.

   ASSIGN c-tip_trans_cx       = ""
          c-cod_finalid_econ   = ""
          i-psdid              = 0
          i-cdn_parcei_edi     = 0
          l-aux-salvar         = 3.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help gDialog
ON CHOOSE OF Btn_Help IN FRAME gDialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
 {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
  IF INPUT FRAME {&FRAME-NAME} f_cod_cta_corren = "" THEN DO:

/*      MESSAGE "Conta corrente deve ser preenchida!" */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.        */
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro inv lido!~~Conta corrente deve ser preenchida!").
     RETURN NO-APPLY.

  END.
  ELSE IF NOT CAN-FIND(FIRST tip_trans_cx
              WHERE tip_trans_cx.cod_tip_trans_cx = INPUT FRAME {&FRAME-NAME} c_tip_trans_cx) OR 
     INPUT FRAME {&FRAME-NAME} c_tip_trans_cx = "" THEN DO:

/*      MESSAGE "Tipo de Transa‡Æo inv lida!"  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro inv lido!~~Tipo de Transa‡Æo inv lida!").

     RETURN NO-APPLY.
    
  END.
  ELSE IF INPUT FRAME {&FRAME-NAME} c_cod_finalid_econ = "" THEN DO:

/*      MESSAGE "Finalidade Econ“mica deve ser preenchida!" */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.              */
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro inv lido!~~Finalidade Econ“mica deve ser preenchida!").
     RETURN NO-APPLY.

  END.
  ELSE IF INPUT FRAME {&FRAME-NAME} i_psdid = 0 THEN DO:

/*      MESSAGE "PSDID deve ser preenchido!"   */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro inv lido!~~PSDID deve ser preenchido!").
     RETURN NO-APPLY.

  END.
  ELSE IF INPUT FRAME {&FRAME-NAME} i_cdn_parcei_edi = 0 THEN DO:

/*      MESSAGE "Parceiro EDI deve ser preenchido!" */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Registro inv lido!~~Parceiro EDI deve ser preenchido!").
     RETURN NO-APPLY.

  END.
   
 ASSIGN  c-tip_trans_cx      = INPUT FRAME {&FRAME-NAME} c_tip_trans_cx
         c-cod_finalid_econ  = INPUT FRAME {&FRAME-NAME} c_cod_finalid_econ
         i-psdid             = INPUT FRAME {&FRAME-NAME} i_psdid
         i-cdn_parcei_edi    = INPUT FRAME {&FRAME-NAME} i_cdn_parcei_edi.


 l-aux-salvar = 1.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableObject gDialog 
PROCEDURE enableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN f_cod_cta_corren:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = c-cod_cta_corren   
         c_tip_trans_cx:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = c-tip_trans_cx     
         c_cod_finalid_econ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-cod_finalid_econ 
         i_psdid:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = string(i-psdid)            
         i_cdn_parcei_edi:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(i-cdn_parcei_edi).
                                                                             

  IF i-psdid <> 0 THEN DO:
     i_psdid:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY f_cod_cta_corren c_tip_trans_cx c_cod_finalid_econ i_psdid 
          i_cdn_parcei_edi 
      WITH FRAME gDialog.
  ENABLE rt-button RECT-1 RECT-2 c_tip_trans_cx bt-zoom c_cod_finalid_econ 
         i_psdid i_cdn_parcei_edi Btn_OK Btn_Cancel Btn_excluir Btn_Help 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

