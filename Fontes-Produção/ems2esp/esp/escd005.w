&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: 
** Descricao.............: 
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 25/04/2013
*****************************************************************************/

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE h-acomp     AS HANDLE      NO-UNDO.
DEFINE VARIABLE cLinha      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArquivo    AS CHARACTER   NO-UNDO.

def new global shared var i-ep-codigo-usuario     like mguni.empresa.ep-codigo        no-undo.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

DEFINE TEMP-TABLE tt-emitente LIKE emitente.

DEFINE TEMP-TABLE tt-erros
    FIELD cgc       LIKE emitente.cgc
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(80)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf RECT-8 ~
tg-imp-fornec fi-arq-imp-fornec bt_fechar bt_cancelar Btn_Help bt_executar 
&Scoped-Define DISPLAYED-OBJECTS tg-imp-fornec fi-arq-imp-fornec 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-busca-arq 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Arquivo" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Arquivo".

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_executar 
     IMAGE-UP FILE "image/im-run.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Executar" 
     SIZE 4 BY 1.13 TOOLTIP "Executar".

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-arq-imp-fornec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .

DEFINE VARIABLE tg-imp-fornec AS LOGICAL INITIAL no 
     LABEL "Importa Dados Fornecedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 3.79 COL 61.57 WIDGET-ID 154
     tg-imp-fornec AT ROW 2.75 COL 4 WIDGET-ID 140
     fi-arq-imp-fornec AT ROW 3.92 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     bt_fechar AT ROW 15.71 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 15.71 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 15.71 COL 70.43 WIDGET-ID 24
     bt_executar AT ROW 1.17 COL 1.86 WIDGET-ID 76
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 15.5 COL 1 WIDGET-ID 2
     RECT-8 AT ROW 3.17 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 1
         CANCEL-BUTTON bt_cancelar WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ".: Importaá∆o de Dados :."
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f_main
   FRAME-NAME                                                           */
ASSIGN 
       bt-busca-arq:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

ASSIGN 
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Importaá∆o de Dados :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Importaá∆o de Dados :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-arq C-Win
ON CHOOSE OF bt-busca-arq IN FRAME f_main /* Busca Arquivo */
DO:
    system-dialog GET-FILE v_cod_dwb_file
        title "Importar" /*l_importar*/ 
        filters '*.csv'  '*.csv'
        save-as
        create-test-file.
    assign fi-arq-imp-fornec:screen-value in frame {&FRAME-NAME} = v_cod_dwb_file.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME f_main /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancelar C-Win
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Cancelar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_executar C-Win
ON CHOOSE OF bt_executar IN FRAME f_main /* Executar */
DO:
    DEFINE VARIABLE lok AS LOGICAL     NO-UNDO.

    IF SEARCH (INPUT FRAME {&FRAME-NAME} fi-arq-imp-fornec) = ? THEN DO:
        MESSAGE "Informe um arquivo v†lido"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        UNDO, RETURN.
    END.

    ASSIGN lok = NO.
    MESSAGE "Confirma importaá∆o?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lok.
    IF lok = NO THEN
        UNDO, RETURN.

    do on error undo, return no-apply:
        run pi_executar.
    end.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    {include/i_dbinst.i}
    {include/i_dbtype.i}
    {include/i_fcldef.i}

    {include/i_fclwin.i c-win}
    {include/i_fclfrm.i f_main }

  RUN enable_UI.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY tg-imp-fornec fi-arq-imp-fornec 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf RECT-8 tg-imp-fornec fi-arq-imp-fornec 
         bt_fechar bt_cancelar Btn_Help bt_executar 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-fornec C-Win 
PROCEDURE pi-imp-fornec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE c-cgc AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE tt-emitente.
    EMPTY TEMP-TABLE tt-erros.

    ASSIGN iCont = 0.

    INPUT FROM VALUE(SEARCH(INPUT FRAME {&FRAME-NAME} fi-arq-imp-fornec)) NO-CONVERT NO-ECHO.
    REPEAT:
        IMPORT UNFORMAT cLinha.

        IF cLinha BEGINS "CNPJ" THEN NEXT.

        ASSIGN iCont = iCont + 1.

        RUN pi-acompanhar IN h-acomp (INPUT "Item: " + STRING (iCont)).

        ASSIGN c-cgc = ENTRY(1,cLinha,";").
        IF LENGTH (c-cgc) = 13 THEN ASSIGN c-cgc = "0" + c-cgc.
        IF LENGTH (c-cgc) = 12 THEN ASSIGN c-cgc = "00" + c-cgc.
        IF LENGTH (c-cgc) = 11 THEN ASSIGN c-cgc = "000" + c-cgc.
        IF LENGTH (c-cgc) = 10 THEN ASSIGN c-cgc = "0" + c-cgc.

        FIND FIRST tt-emitente
            WHERE tt-emitente.cgc = c-cgc NO-ERROR.
        IF AVAILABLE tt-emitente THEN DO:
            MESSAGE "Emitente < " + c-cgc + " > duplicado no arquivo!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        FIND emitente NO-LOCK
            WHERE emitente.cgc = c-cgc NO-ERROR.

        IF AVAILABLE emitente THEN DO:
            CREATE tt-emitente.
            ASSIGN tt-emitente.cod-emitente     = emitente.cod-emitente
                   tt-emitente.nome-abrev       = emitente.nome-abrev
                   tt-emitente.cgc              = c-cgc
                   tt-emitente.nome-emit        = ENTRY(2,cLinha,";")
                   tt-emitente.endereco         = ENTRY(3,cLinha,";") + ", " + ENTRY(4,cLinha,";")
                   tt-emitente.endereco2        = ENTRY(5,cLinha,";")
                   tt-emitente.cep              = ENTRY(6,cLinha,";")
                   tt-emitente.ins-estadual     = ENTRY(7,cLinha,";")
                   .
            IF tt-emitente.ins-estadual = "" THEN
                ASSIGN tt-emitente.ins-estadual = "ISENTO".
        END.
        ELSE DO:
            CREATE tt-erros.
            ASSIGN tt-erros.cgc       = c-cgc
                   tt-erros.cod-erro  = 1
                   tt-erros.desc-erro = "CGC n∆o encontrado ou duplicado.".
        END.
    END.
    INPUT CLOSE.

    ASSIGN iCont    = 0
           cArquivo = SESSION:TEMP-DIRECTORY + "ESCD005.txt".

    OUTPUT TO VALUE (cArquivo) NO-CONVERT.

    PUT UNFORMAT "                                  LOG DE IMPORTAÄ«O DE DADOS DO FORNECEDOR " SKIP.
    PUT UNFORMAT "                                  ======================================== " SKIP(2).

    FOR EACH tt-erros:
        DISP tt-erros.cgc
             tt-erros.cod-erro
             tt-erros.desc-erro
             WITH WIDTH 333.
    END.
    PUT SKIP(2).

    PUT UNFORMAT 
        "Nome Fornecedor"     AT 15
        "Endereco"            AT 96
        "CEP"                 AT 196
        "Inscr. Estadual"     AT 211
        .
    PUT SKIP.
    PUT UNFORMAT 
        "              "
        "================================================================================ "
        "=================================================================================================== "
        "============== "
        "===============".
    PUT SKIP.

    FOR EACH tt-emitente:
        FIND emitente
            WHERE emitente.cod-emitente = tt-emitente.cod-emitente NO-ERROR.
        ASSIGN iCont = iCont + 1.

        PUT UNFORMAT 
            "Codigo: "              AT 1
            emitente.cod-emitente   AT 9
            "Nome Abrev: "          AT 20
            emitente.nome-abrev     AT 32
            "CGC: "                 AT 50
            tt-emitente.cgc         AT 55
            .
        PUT SKIP.

        PUT UNFORMAT "Dados ATUAIS: "
            emitente.nome-emit    AT 15
            emitente.endereco     AT 96
            emitente.cep          AT 196
            emitente.ins-estadual AT 211
            .
        PUT SKIP.

        PUT UNFORMAT "Dados NOVOS: "
            tt-emitente.nome-emit    AT 15  
            tt-emitente.endereco     AT 96  
            tt-emitente.cep          AT 196 
            tt-emitente.ins-estadual AT 211
            .
        PUT SKIP(1).

        IF INPUT FRAME {&FRAME-NAME} tg-imp-fornec = YES THEN DO:
            IF AVAILABLE emitente THEN DO:
                ASSIGN emitente.nome-emit    = tt-emitente.nome-emit   
                       emitente.endereco     = tt-emitente.endereco    
                       emitente.endereco2    = tt-emitente.endereco2
                       emitente.cep          = IF TRIM (tt-emitente.cep) <> "" THEN tt-emitente.cep ELSE emitente.cep
                       emitente.ins-estadual = IF TRIM (tt-emitente.ins-estadual) <> "" THEN tt-emitente.ins-estadual ELSE emitente.ins-estadual
                       .
            END.
        END.

    END.
    OUTPUT CLOSE.

    OS-COMMAND NO-WAIT VALUE (cArquivo) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_executar C-Win 
PROCEDURE pi_executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Inicializando").

do on error undo, return error on stop  undo, return error:

    RUN pi-acompanhar IN h-acomp (INPUT "Processando Itens...").

    RUN pi-imp-fornec.

end.

run pi-finalizar in h-acomp. 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

