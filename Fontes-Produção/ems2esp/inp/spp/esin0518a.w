&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIN0518A 2.00.00.000}   /*** 010000 ***/

def buffer empresa for ems2cadme.empresa.
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER v_log_planilha     AS LOGICAL    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER v_cod_arq_planilha AS CHARACTER format "x(100)" NO-UNDO.
DEFINE OUTPUT       PARAMETER p-confirma         AS LOGICAL    NO-UNDO.

/* Local Variable Definitions ---                                       */
def var c-arquivo-n AS CHAR FORMAT "x(100)" no-undo.
def var i-aux       as int no-undo.
def var i-total     as int no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS l-planilha c-arquivo bt-arquivo ~
bt-ok bt-cancela bt-ajuda RECT-13 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS l-planilha c-arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-arquivo AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 4.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 58 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE l-planilha AS LOGICAL INITIAL yes 
     LABEL "Gerar Arquivo Planilha" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     l-planilha AT ROW 1.75 COL 6
     c-arquivo AT ROW 3 COL 4 COLON-ALIGNED NO-LABEL
     bt-arquivo AT ROW 3 COL 51 HELP
          "Escolha do nome do arquivo"
     bt-ok AT ROW 5.5 COL 2
     bt-cancela AT ROW 5.5 COL 13
     bt-ajuda AT ROW 5.5 COL 48
     RECT-13 AT ROW 1 COL 1
     rt-buttom AT ROW 5.25 COL 1
     SPACE(0.00) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert SmartDialog title>"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

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


&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo D-Dialog
ON CHOOSE OF bt-arquivo IN FRAME D-Dialog
DO:
  
    def var c-arq-conv  as char no-undo.
    DEF VAR iLegenda    AS INT  NO-UNDO.
    DEF VAR iSoma       AS INT  NO-UNDO.
    DEF VAR l-ok AS LOGICAL.

    assign c-arq-conv = replace(input frame {&FRAME-NAME} c-arquivo, "/", "\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.csv" "*.csv",
               "*.*" "*.*"
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "csv"
       INITIAL-DIR "spool" 
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign c-arquivo = replace(c-arq-conv, "\", "/"). 
        ASSIGN c-arquivo-n = c-arquivo
               iLegenda    = LENGTH(c-arquivo)
               iSoma       = 0.
        
        DO WHILE iLegenda > 0:
           IF  SUBSTR(c-arquivo,iLegenda,1) = "/" THEN DO:
               ASSIGN c-arquivo-n = SUBSTR(c-arquivo, (iLegenda + 1), iSoma).
               ASSIGN i-aux       =  INDEX(c-arquivo-n, ".").
               ASSIGN c-arquivo   = SUBSTR(c-arquivo, 1, iLegenda) +
                                    SUBSTR(c-arquivo-n, 1, i-aux - 1) + ".csv".
               LEAVE.
           END.
           ASSIGN iSoma    = iSoma    + 1
                  iLegenda = iLegenda - 1.
        END.

        display c-arquivo with frame {&FRAME-NAME}.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  ASSIGN p-confirma = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
      
    ASSIGN INPUT FRAME {&FRAME-NAME} c-arquivo
    .

    IF  INPUT FRAME {&FRAME-NAME} l-planilha = YES THEN DO:

        run pi_filename_validation (Input c-arquivo) /*pi_filename_validation*/.
        if  return-value = "NOK" then do:
            run utp/ut-msgs.p (input "show",
                               input 4714,
                               input "").
            APPLY 'entry' TO c-arquivo IN FRAME {&FRAME-NAME}.
            return no-apply.
        END.
    
        if index(c-arquivo,'~\') <> 0 then
            assign file-info:file-name = substring(c-arquivo ,1,r-index(c-arquivo,'~\') - 1).
        else
           assign file-info:file-name = substring(c-arquivo,1,r-index(c-arquivo,'~/') - 1).
        if  (file-info:file-type = ?) and (index(c-arquivo,'~\') <> 0 or 
           index(c-arquivo,'/') <> 0 or index(c-arquivo ,':') <> 0) then do:
            run utp/ut-msgs.p (input "show",
                               input 4715,
                               input STRING(c-arquivo)).
            APPLY 'entry' TO c-arquivo IN FRAME {&FRAME-NAME}.
            return no-apply.
        end.

        ASSIGN v_cod_arq_planilha = input frame {&FRAME-NAME} c-arquivo
        .

    END.

    ASSIGN v_log_planilha = input frame {&FRAME-NAME} l-planilha.

    ASSIGN p-confirma = yes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-planilha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-planilha D-Dialog
ON VALUE-CHANGED OF l-planilha IN FRAME D-Dialog /* Gerar Arquivo Planilha */
DO:
  
    IF  INPUT FRAME {&FRAME-NAME} l-planilha = YES THEN DO:
        ASSIGN bt-arquivo:SENSITIVE     IN FRAME {&FRAME-NAME} = YES
               c-arquivo:SENSITIVE      IN FRAME {&FRAME-NAME} = YES 
               .

        IF  INPUT FRAME {&FRAME-NAME} c-arquivo = "" THEN
            ASSIGN c-arquivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v_cod_arq_planilha.

    END.
    ELSE
        ASSIGN bt-arquivo:SENSITIVE        IN FRAME {&FRAME-NAME} = NO
               c-arquivo:SENSITIVE         IN FRAME {&FRAME-NAME} = NO 
               c-arquivo:SCREEN-VALUE      IN FRAME {&FRAME-NAME} = ""
               .

END.

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
  DISPLAY l-planilha c-arquivo 
      WITH FRAME D-Dialog.
  ENABLE l-planilha c-arquivo bt-arquivo bt-ok bt-cancela 
         bt-ajuda RECT-13 rt-buttom 
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

{utp/ut9000.i "AP0777I" "2.00.00.000"}
    
  ASSIGN c-arquivo      = v_cod_arq_planilha
         l-planilha     = v_log_planilha.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    

  APPLY 'value-changed' TO l-planilha IN FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_filename_validation D-Dialog 
PROCEDURE pi_filename_validation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_filename
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_1                          as character       no-undo. /*local*/
    def var v_cod_2                          as character       no-undo. /*local*/
    def var v_num_1                          as integer         no-undo. /*local*/
    def var v_num_2                          as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    if  p_cod_filename = "" or p_cod_filename = "."
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    assign v_cod_1 = replace(p_cod_filename, "~\", "/").

    1_block:
    repeat v_num_1 = 1 to length(v_cod_1):
        if  index('abcdefghijklmnopqrstuvwxyz0123456789-_:/.', substring(v_cod_1, v_num_1, 1)) = 0
        then do:
            return "NOK" /*l_nok*/ .
        end /* if */.
    end /* repeat 1_block */.

    if  num-entries(v_cod_1, ":") > 2
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ":") = 2 and length(entry(1,v_cod_1,":")) > 1
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ".") > 3
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ".") = 3 and length(entry(3,v_cod_1,".")) > 3
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  index(v_cod_1, "~/~/") > 0
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  index(entry(num-entries(v_cod_1, "/"),v_cod_1, "/"),".") = 0
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.
    else do:
        if  entry(1,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = ""
        or  entry(2,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = ""
        then do:
           return "NOK" /*l_nok*/ .
        end /* if */.
    end /* else */.

/*
    assign v_num_1 = 1.
    2_block:
    repeat v_num_2 = 1 to length(v_cod_1):
        if  index(":" + "/" + ".", substring(v_cod_1, v_num_2, 1)) > 0
        then do:
            assign v_cod_2 = substring(v_cod_1, v_num_1, v_num_2 - v_num_1)
                   v_num_1 = v_num_2 + 1.
            if  length(v_cod_2) > 8
            then do:
                return "NOK" /*l_nok*/ .
            end /* if */.
        end /* if */.
    end /* repeat 2_block */.
    assign v_cod_2 = substring(v_cod_1, v_num_1).
    if  length(v_cod_2) > 8
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.
*/

    return "OK" /*l_ok*/ .

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

