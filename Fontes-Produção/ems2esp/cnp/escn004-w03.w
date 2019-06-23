&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCN004-W03 12.99.99.999}

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{cnp\escn004.i}

DEFINE INPUT  PARAMETER ipr-row-contr   AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER ipi-nr-contrato AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER ipl-altera      AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opl-ok          AS LOGICAL     NO-UNDO.

DEF BUFFER bf-ext-contrato-for FOR ext-contrato-for.


    DEFINE VARIABLE i-gar AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-33 cb_tp_gar d_perc1 dt_termo ~
d_perc2 dt_pagto d_garant tl-processo bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS cb_tp_gar d_perc1 dt_termo d_perc2 ~
dt_pagto d_garant tl-processo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb_tp_gar d_perc1 dt_termo d_perc2 dt_pagto d_garant 
&Scoped-define List-2 cb_tp_gar d_perc1 dt_termo d_perc2 dt_pagto 
&Scoped-define List-3 cb_tp_gar d_perc1 dt_termo d_perc2 dt_pagto 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE cb_tp_gar AS CHARACTER FORMAT "X(20)":U INITIAL "0" 
     LABEL "Tipo Garantia" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "","0",
                     "Cau‡Æo","1",
                     "Seguro","2",
                     "Carta Fian‡a","3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.86 BY 5.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 72 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tl-processo AS LOGICAL INITIAL no 
     LABEL "Processo Judicial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     cb_tp_gar AT ROW 2 COL 17 COLON-ALIGNED WIDGET-ID 10
     d_perc1 AT ROW 2 COL 55 COLON-ALIGNED WIDGET-ID 24
     dt_termo AT ROW 3 COL 17 COLON-ALIGNED WIDGET-ID 12
     d_perc2 AT ROW 3 COL 55 COLON-ALIGNED WIDGET-ID 26
     dt_pagto AT ROW 4 COL 17 COLON-ALIGNED WIDGET-ID 14
     d_garant AT ROW 4 COL 55 COLON-ALIGNED WIDGET-ID 44
     tl-processo AT ROW 5.04 COL 19 WIDGET-ID 54
     bt-ok AT ROW 6.75 COL 2
     bt-cancela AT ROW 6.75 COL 13
     bt-ajuda AT ROW 6.75 COL 62
     rt-buttom AT ROW 6.5 COL 1
     RECT-33 AT ROW 1.13 COL 1.14 WIDGET-ID 56
     SPACE(0.13) SKIP(1.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb_tp_gar IN FRAME D-Dialog
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN dt_pagto IN FRAME D-Dialog
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN dt_termo IN FRAME D-Dialog
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN d_garant IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN d_perc1 IN FRAME D-Dialog
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN d_perc2 IN FRAME D-Dialog
   1 2 3                                                                */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <insert SmartDialog title> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  ASSIGN opl-ok = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
    RUN pi-salva.
    opl-ok = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_tp_gar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_tp_gar D-Dialog
ON VALUE-CHANGED OF cb_tp_gar IN FRAME D-Dialog /* Tipo Garantia */
DO:
   ASSIGN i-gar = INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

  CASE INPUT FRAME {&FRAME-NAME} cb_tp_gar:
      
      WHEN "1" THEN DO:
          DISABLE d_garant
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_perc1 d_perc2 dt_termo dt_pagto 
                 WITH FRAME {&FRAME-NAME}.                
         ASSIGN d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0,00"
                d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for THEN STRING(ext-contrato-for.perc_1) ELSE "").
                d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for THEN STRING(ext-contrato-for.perc_2) ELSE "").

      END.
      WHEN "2" THEN DO:
          DISABLE d_perc1 d_perc2 
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_garant dt_termo dt_pagto
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar = i-gar THEN STRING(ext-contrato-for.val_garantia) ELSE "").
      END.
      WHEN "3" THEN DO:
          DISABLE d_perc1 d_perc2 
                 WITH FRAME {&FRAME-NAME}.
          ENABLE d_garant dt_termo dt_pagto
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = (IF AVAIL ext-contrato-for AND ext-contrato-for.ind_tip_gar = i-gar THEN STRING(ext-contrato-for.val_garantia) ELSE "").
      END.
      OTHERWISE DO:
          DISABLE d_perc1 d_perc2 dt_termo dt_pagto d_garant
                 WITH FRAME {&FRAME-NAME}.
          ASSIGN 
        d_perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        d_perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        d_garant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
        dt_pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
        
      END.
  END CASE.



END.


/* ,0,            */
/* Cau‡Æo,1,      */
/* Seguro,2,      */
/* Carta Fian‡a,3 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY cb_tp_gar d_perc1 dt_termo d_perc2 dt_pagto d_garant tl-processo 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-33 cb_tp_gar d_perc1 dt_termo d_perc2 dt_pagto d_garant 
         tl-processo bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESCN004-W03" "12.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND FIRST bf-ext-contrato-for NO-LOCK 
      WHERE ROWID(bf-ext-contrato-for) = ipr-row-contr NO-ERROR.
  IF AVAIL bf-ext-contrato-for THEN DO:
           ASSIGN cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.ind_tip_gar)
                   d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.perc_1)
                   d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.perc_2)
                   dt_pagto:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.dt_pg_ret)
                   dt_termo:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.dt_termo_enc)
                   d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}   = STRING(bf-ext-contrato-for.val_garantia)
                   tl-processo:CHECKED IN FRAME {&FRAME-NAME}      = (IF bf-ext-contrato-for.ind-status = 4 THEN YES ELSE NO).

        IF NOT ipl-altera THEN 
            DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  END.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salva D-Dialog 
PROCEDURE pi-salva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST bf-ext-contrato-for 
          WHERE ROWID(bf-ext-contrato-for) = ipr-row-contr NO-ERROR.
    IF NOT AVAIL bf-ext-contrato-for THEN DO:  

        CREATE bf-ext-contrato-for.
        ASSIGN bf-ext-contrato-for.nr-contrato = ipi-nr-contrato.
    END.
    
    ASSIGN bf-ext-contrato-for.ind_tip_gar   = INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           bf-ext-contrato-for.perc_1        = DEC(d_perc1:SCREEN-VALUE   IN FRAME {&FRAME-NAME})
           bf-ext-contrato-for.perc_2        = DEC(d_perc2:SCREEN-VALUE   IN FRAME {&FRAME-NAME})
           bf-ext-contrato-for.dt_pg_ret     = DATE(dt_pagto:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           bf-ext-contrato-for.dt_termo_enc  = DATE(dt_termo:SCREEN-VALUE IN FRAME {&FRAME-NAME})
           bf-ext-contrato-for.val_garantia  = DEC(d_garant:SCREEN-VALUE  IN FRAME {&FRAME-NAME}).

    IF tl-processo:CHECKED IN FRAME {&FRAME-NAME} THEN
        ASSIGN bf-ext-contrato-for.ind-status = 4.

    /* 1-Vigente, 2-Termo Enc, 3-Reten‡Æo Liberada, 4-Processo Jud */

    RELEASE bf-ext-contrato-for.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

