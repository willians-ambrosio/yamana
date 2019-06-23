&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/****************************************************************************************** 
**         Programa: rep001-v02.w
**            Autor: Felipe Vieira
**       Fornecedor: DKP
**       Data: 25/09/2018
** Change/Chamado: 
**    Objetivo: Cadastro de aprovadores (viewer pai 2)
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**   
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include/i-prgvrs.i REP001-V02 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i REP001-V02 MUT}
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
&Scoped-define EXTERNAL-TABLES esp_aprovador
&Scoped-define FIRST-EXTERNAL-TABLE esp_aprovador


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR esp_aprovador.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS esp_aprovador.aprov_contas_pagar ~
esp_aprovador.data_vigen_ini esp_aprovador.aprov_caixa_bancos ~
esp_aprovador.data_vigen_fim esp_aprovador.aprov_recebimento ~
esp_aprovador.nivel_aprovador esp_aprovador.email 
&Scoped-define ENABLED-TABLES esp_aprovador
&Scoped-define FIRST-ENABLED-TABLE esp_aprovador
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-23 
&Scoped-Define DISPLAYED-FIELDS esp_aprovador.aprov_contas_pagar ~
esp_aprovador.data_vigen_ini esp_aprovador.aprov_caixa_bancos ~
esp_aprovador.data_vigen_fim esp_aprovador.aprov_recebimento ~
esp_aprovador.nivel_aprovador esp_aprovador.email 
&Scoped-define DISPLAYED-TABLES esp_aprovador
&Scoped-define FIRST-DISPLAYED-TABLE esp_aprovador


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
cod_usuario|y|y|mgesp.esp_aprovador.cod_usuario
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod_usuario",
     Keys-Supplied = "cod_usuario"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 4.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     esp_aprovador.aprov_contas_pagar AT ROW 2.25 COL 45 WIDGET-ID 22
          VIEW-AS TOGGLE-BOX
          SIZE 23.57 BY .83
     esp_aprovador.data_vigen_ini AT ROW 2.58 COL 23.72 COLON-ALIGNED WIDGET-ID 16
          LABEL "Data Inicial da Vigˆncia"
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     esp_aprovador.aprov_caixa_bancos AT ROW 3.25 COL 45 WIDGET-ID 26
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .83
     esp_aprovador.data_vigen_fim AT ROW 4.04 COL 23.57 COLON-ALIGNED WIDGET-ID 10
          LABEL "Data Final da Vigˆncia"
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     esp_aprovador.aprov_recebimento AT ROW 4.25 COL 45 WIDGET-ID 24
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .83
     esp_aprovador.nivel_aprovador AT ROW 5.25 COL 53 COLON-ALIGNED WIDGET-ID 28
          LABEL "Nivel Aprovador"
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     esp_aprovador.email AT ROW 6.88 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 51 BY .88
     "M¢dulos para Aprova‡Æo" VIEW-AS TEXT
          SIZE 17.57 BY .67 AT ROW 1.25 COL 45 WIDGET-ID 20
     rt-mold AT ROW 1 COL 1
     RECT-23 AT ROW 1.5 COL 39 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.esp_aprovador
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
         HEIGHT             = 8.04
         WIDTH              = 83.14.
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

/* SETTINGS FOR FILL-IN esp_aprovador.data_vigen_fim IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN esp_aprovador.data_vigen_ini IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN esp_aprovador.nivel_aprovador IN FRAME f-main
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

&Scoped-define SELF-NAME esp_aprovador.aprov_caixa_bancos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp_aprovador.aprov_caixa_bancos V-table-Win
ON VALUE-CHANGED OF esp_aprovador.aprov_caixa_bancos IN FRAME f-main /* Caixa e Bancos */
DO:
    IF esp_aprovador.aprov_contas_pagar:CHECKED OR 
        esp_aprovador.aprov_caixa_bancos:CHECKED THEN
        ASSIGN esp_aprovador.nivel_aprovador:SENSITIVE = YES.
    ELSE
        ASSIGN esp_aprovador.nivel_aprovador:SENSITIVE = NO
               esp_aprovador.nivel_aprovador:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME esp_aprovador.aprov_contas_pagar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp_aprovador.aprov_contas_pagar V-table-Win
ON VALUE-CHANGED OF esp_aprovador.aprov_contas_pagar IN FRAME f-main /* Contas a pagar */
DO:
    IF esp_aprovador.aprov_contas_pagar:CHECKED OR 
        esp_aprovador.aprov_caixa_bancos:CHECKED THEN
        ASSIGN esp_aprovador.nivel_aprovador:SENSITIVE = YES.
    ELSE
        ASSIGN esp_aprovador.nivel_aprovador:SENSITIVE = NO
               esp_aprovador.nivel_aprovador:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME esp_aprovador.email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL esp_aprovador.email V-table-Win
ON LEAVE OF esp_aprovador.email IN FRAME f-main /* E-mail */
DO:
/*    {include/LEAVE.i &tabela=email                                                        */
/*                     &atributo-ref=email-name                                             */
/*                     &variavel-ref=c-email-name                                           */
/*                     &WHERE="email.email = input frame {&frame-name} esp_aprovador.email" */
/*        }                                                                                 */
/* END.                                                                                     */
/*                                                                                          */
/*                                                                                          */
/*  ON F5 OF esp_aprovador.email IN FRAME {&FRAME-NAME}                                     */
/*   OR mouse-select-dblclick OF esp_aprovador.email IN FRAME {&FRAME-NAME}                 */
/*   DO:                                                                                    */
/*                                                                                          */
/*                                                                                          */
/*     {include/zoomvar.i &prog-zoom=CadastroComplexo\w-pesqui-Zoomemail.w                  */
/*                        &campo=esp_aprovador.email                                        */
/*                        &campozoom=email                                                  */
/*                                                                                          */
/*     }                                                                                    */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */ 
 &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cod_usuario':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = esp_aprovador
           &WHERE = "WHERE esp_aprovador.cod_usuario eq key-value"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "esp_aprovador"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "esp_aprovador"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* IF AVAIL esp_aprovador THEN                                               */
/* DO:                                                                       */
/*     FIND email WHERE email.email = esp_aprovador.email NO-LOCK NO-ERROR.  */
/*     ASSIGN c-email-name = IF AVAIL email THEN email.email-name ELSE "":U. */
/* END.                                                                      */

RUN dispatch IN THIS-PROCEDURE (INPUT 'display-fields':U).


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
    
/*     IF adm-new-record THEN DO:                                                             */
/*                                                                                            */
/*          esp_aprovador.data_vigen_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY). */
/*                                                                                            */
/*     END.                                                                                   */


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

IF INPUT FRAME {&FRAME-NAME} esp_aprovador.data_vigen_fim < INPUT FRAME {&FRAME-NAME} esp_aprovador.data_vigen_ini THEN DO:

    {include/i-vldprg.i}                                             
    run utp/ut-msgs.p (input "show":U, input 27100, input "Data final menor que a data inicial~~A data final nao pode ser menor que a data inicial!"). 
    return 'ADM-ERROR':U.     

END.
ELSE IF (STRING(INPUT FRAME {&FRAME-NAME} esp_aprovador.data_vigen_fim)      = "")
     OR (STRING(INPUT FRAME {&FRAME-NAME} esp_aprovador.data_vigen_ini)      = "")
     OR (STRING(INPUT FRAME {&FRAME-NAME} esp_aprovador.email)               = "")
     OR (INPUT FRAME {&FRAME-NAME} esp_aprovador.aprov_contas_pagar = FALSE
     AND INPUT FRAME {&FRAME-NAME} esp_aprovador.aprov_recebimento  = FALSE)
     THEN DO:
    {include/i-vldprg.i}
    run utp/ut-msgs.p (input "show":U, input 27100, input "Todos os campos devem ser preenchidos~~NÆo pode haver campo sem preechimento!").
    return 'ADM-ERROR':U.
END.

IF (INPUT FRAME {&FRAME-NAME} esp_aprovador.aprov_contas_pagar OR
   INPUT FRAME {&FRAME-NAME} esp_aprovador.aprov_contas_pagar)  AND 
   INPUT FRAME {&FRAME-NAME} esp_aprovador.nivel_aprovador = 0  THEN
DO:
    RUN utp/ut-msgs.p ("show",17006,"Nivel do Aprovador Inv lido~~Quando marcar os modulos Contas a Pagar e/ou Caixa e Bancos, o n¡vel do aprovador deve ser informado 1 ou 2").
    APPLY "ENTRY" TO esp_aprovador.nivel_aprovador IN  FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
END.

IF INPUT FRAME {&FRAME-NAME} esp_aprovador.nivel_aprovador > 2  THEN
DO:
    RUN utp/ut-msgs.p ("show",17006,"Nivel do Aprovador Inv lido~~N¡vel do aprovador deve ser informado 1 ou 2").
    APPLY "ENTRY" TO esp_aprovador.nivel_aprovador IN  FRAME {&FRAME-NAME}.
    RETURN 'ADM-ERROR':U.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod_usuario" "esp_aprovador" "cod_usuario"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "esp_aprovador"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

