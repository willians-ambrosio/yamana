&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/****************************************************************************************** 
** 	   Programa: rep001-v04.w
**   	      Autor: Felipe Vieira
**   	 Fornecedor: DKP
**    	 Data: 25/09/2018
** Change/Chamado: 
**    Objetivo: Cadastro de aprovadores (inclui/modifica estabelecimento)
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**   
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include/i-prgvrs.i REP001-V03 1.00.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i REP001-V03 REP}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/*:T vari veis de uso local */
def var v-row-table  as rowid no-undo.

/*:T fim das variaveis utilizadas no estilo */

DEFINE TEMP-TABLE tt-estab
  FIELD cod_estab  AS CHAR      
  FIELD nom_pessoa AS CHAR.

DEFINE TEMP-TABLE tt-estab_usuar
  FIELD cod-estabel AS CHAR
  FIELD nom_pessoa  AS CHAR
  FIELD cod-usuar   AS CHAR.

DEFINE VARIABLE c-cod-usuar AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME b-estabelecimento

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES esp_aprovador
&Scoped-define FIRST-EXTERNAL-TABLE esp_aprovador


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR esp_aprovador.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-estab tt-estab_usuar

/* Definitions for BROWSE b-estabelecimento                             */
&Scoped-define FIELDS-IN-QUERY-b-estabelecimento tt-estab.cod_estab tt-estab.nom_pessoa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-estabelecimento   
&Scoped-define SELF-NAME b-estabelecimento
&Scoped-define QUERY-STRING-b-estabelecimento FOR EACH tt-estab NO-LOCK
&Scoped-define OPEN-QUERY-b-estabelecimento OPEN QUERY {&SELF-NAME} FOR EACH tt-estab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-b-estabelecimento tt-estab
&Scoped-define FIRST-TABLE-IN-QUERY-b-estabelecimento tt-estab


/* Definitions for BROWSE b-UsuEstb                                     */
&Scoped-define FIELDS-IN-QUERY-b-UsuEstb tt-estab_usuar.cod-estabel tt-estab_usuar.nom_pessoa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-UsuEstb   
&Scoped-define SELF-NAME b-UsuEstb
&Scoped-define QUERY-STRING-b-UsuEstb FOR EACH tt-estab_usuar
&Scoped-define OPEN-QUERY-b-UsuEstb OPEN QUERY {&SELF-NAME} FOR EACH tt-estab_usuar.
&Scoped-define TABLES-IN-QUERY-b-UsuEstb tt-estab_usuar
&Scoped-define FIRST-TABLE-IN-QUERY-b-UsuEstb tt-estab_usuar


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-b-estabelecimento}~
    ~{&OPEN-QUERY-b-UsuEstb}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-estabelecimento b-UsuEstb BUTTON-1 ~
BUTTON-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BUTTON-1 BUTTON-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod_usuario||y|mgesp.esp_estab_aprov.cod_usuario
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod_usuario"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "adeicon/next-au.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon/next-ai.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "adeicon/prev-au.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon/prev-ai.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1.13.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b-estabelecimento FOR 
      tt-estab SCROLLING.

DEFINE QUERY b-UsuEstb FOR 
      tt-estab_usuar SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b-estabelecimento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-estabelecimento B-table-Win _FREEFORM
  QUERY b-estabelecimento DISPLAY
      tt-estab.cod_estab  FORMAT "X(5)"  COLUMN-LABEL "Estab."
tt-estab.nom_pessoa FORMAT "X(40)"  COLUMN-LABEL "Nome"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 9
         TITLE "Estabelecimentos dispon¡veis" ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.

DEFINE BROWSE b-UsuEstb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-UsuEstb B-table-Win _FREEFORM
  QUERY b-UsuEstb DISPLAY
      tt-estab_usuar.cod-estabel FORMAT "X(5)"  COLUMN-LABEL "Estab." 
tt-estab_usuar.nom_pessoa  FORMAT "X(40)"  COLUMN-LABEL "Nome"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 9
         TITLE "Estabelecimentos do Aprovador" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     b-estabelecimento AT ROW 1.25 COL 1.72 WIDGET-ID 200
     b-UsuEstb AT ROW 1.25 COL 45.14 WIDGET-ID 300
     BUTTON-1 AT ROW 2.75 COL 40.57 WIDGET-ID 2
     BUTTON-2 AT ROW 7.29 COL 40.43 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: mgesp.esp_aprovador
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.25
         WIDTH              = 82.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB b-estabelecimento 1 F-Main */
/* BROWSE-TAB b-UsuEstb b-estabelecimento F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-estabelecimento
/* Query rebuild information for BROWSE b-estabelecimento
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-estab NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-estabelecimento */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-UsuEstb
/* Query rebuild information for BROWSE b-UsuEstb
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-estab_usuar.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-UsuEstb */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 B-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
    IF b-estabelecimento:FETCH-SELECTED-ROW(1) THEN DO: /*Verifica se foi selecionado um campos do browse*/

      IF AVAIL(tt-estab) THEN DO: /*Verifica se existe um registro*/
/*           CREATE esp_estab_aprov. /*Cria um registro na tabela temporaria*/ */
/*           ASSIGN esp_estab_aprov.cod_usuario  = c-cod-usuar                 */
/*                  esp_estab_aprov.cod-estabel  = tt-estab.cod_estab.         */

          CREATE tt-estab_usuar.
          ASSIGN tt-estab_usuar.cod-estabel   = tt-estab.cod_estab     
                 tt-estab_usuar.cod-usuar     = c-cod-usuar    
                 tt-estab_usuar.nom_pessoa    = tt-estab.nom_pessoa. 
            

          b-estabelecimento:DELETE-SELECTED-ROWS().
          /*
          RUN AtualizaUsuEst.
           */
          {&OPEN-QUERY-B-UsuEstb}

      END.

    END.

 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 B-table-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  
    IF B-UsuEstb:FETCH-SELECTED-ROW(1) THEN DO: /*Verifica se foi selecionado um campos do browse*/
        
      IF AVAIL(tt-estab_usuar) THEN DO: /*Verifica se existe um registro*/
 
/*         FIND FIRST esp_estab_aprov EXCLUSIVE-LOCK                                                         */
/*             WHERE esp_estab_aprov.cod_usuario = c-cod-usuar                                               */
/*               AND esp_estab_aprov.cod-estabel = tt-estab_usuar.cod-estabel NO-ERROR.                      */
/*                                                                                                               */
/*         IF AVAIL(esp_estab_aprov) THEN DELETE esp_estab_aprov. /*deleta o registro na tabela especi*/ */

        DELETE tt-estab_usuar.
        
       
          B-UsuEstb:DELETE-SELECTED-ROWS().
          RUN AtualizaEstab. 
          {&OPEN-QUERY-b-estabelecimento}
      END.

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b-estabelecimento
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AtualizaEstab B-table-Win 
PROCEDURE AtualizaEstab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-estab.
FOR EACH estabelecimento NO-LOCK
       WHERE NOT CAN-FIND(tt-estab_usuar WHERE tt-estab_usuar.cod-estabel         = estabelecimento.cod_estab 
                                           AND tt-estab_usuar.cod-usuar           = c-cod-usuar               
                          ):   

    CREATE tt-estab.
    ASSIGN tt-estab.cod_estab    =  estabelecimento.cod_estab   
           tt-estab.nom_pessoa   =  estabelecimento.nom_pessoa  .                            


END.

{&OPEN-QUERY-b-estabelecimento}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AtualizaUsuEst B-table-Win 
PROCEDURE AtualizaUsuEst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-estab_usuar.

FOR EACH esp_estab_aprov
     WHERE esp_estab_aprov.cod_usuario = c-cod-usuar NO-LOCK:

    FIND estabelecimento WHERE estabelecimento.cod_estab = esp_estab_aprov.cod-estabel NO-LOCK NO-ERROR.

    CREATE tt-estab_usuar.
    ASSIGN tt-estab_usuar.nom_pessoa   = estabelecimento.nom_pessoa    
           tt-estab_usuar.cod-estabel  = esp_estab_aprov.cod-estabel 
           tt-estab_usuar.cod-usuar    = c-cod-usuar.                            

END.

 {&OPEN-QUERY-b-UsuEstb}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hab-Des B-table-Win 
PROCEDURE Hab-Des :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER l-habl      AS LOG.
DEF INPUT PARAMETER c-aprovador AS CHAR.

/* c-cod-usuar = c-aprovador.                                    */
/*                                                               */
/* RUN AtualizaUsuEst.                                           */
/* RUN AtualizaEstab.                                            */
/*                                                               */
/* IF l-habl THEN DO:                                            */
/*                                                               */
/*    b-estabelecimento:SENSITIVE IN FRAME {&FRAME-NAME}= TRUE.  */
/*    b-UsuEstb:SENSITIVE IN FRAME {&FRAME-NAME}= TRUE.          */
/*    button-1:SENSITIVE IN FRAME {&FRAME-NAME}= TRUE.           */
/*    button-2:SENSITIVE IN FRAME {&FRAME-NAME}= TRUE.           */
/*                                                               */
/*                                                               */
/* END.                                                          */
/* ELSE DO:                                                      */
/*                                                               */
/*    b-estabelecimento:SENSITIVE IN FRAME {&FRAME-NAME}= FALSE. */
/*    b-UsuEstb:SENSITIVE IN FRAME {&FRAME-NAME}= FALSE.         */
/*    button-1:SENSITIVE IN FRAME {&FRAME-NAME}= FALSE.          */
/*    button-2:SENSITIVE IN FRAME {&FRAME-NAME}= FALSE.          */
/*                                                               */
/*                                                               */
/* END.                                                          */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN adm-open-query-cases.

/*
c-cod-usuar = esp_aprovador.cod_usuario.
RUN AtualizaUsuEst.
RUN AtualizaEstab. 

{&OPEN-QUERY-b-UsuEstb}
{&OPEN-QUERY-b-estabelecimento}

*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent B-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.

    FIND esp_aprovador NO-LOCK 
         WHERE ROWID(esp_aprovador) = v-row-parent.

    c-cod-usuar = esp_aprovador.cod_usuario.
    RUN AtualizaUsuEst.
    RUN AtualizaEstab. 
     
    {&OPEN-QUERY-b-UsuEstb}
    {&OPEN-QUERY-b-estabelecimento}



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-salva-estabel B-table-Win 
PROCEDURE Pi-salva-estabel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER salva AS CHAR.

FOR EACH esp_estab_aprov EXCLUSIVE-LOCK WHERE esp_estab_aprov.cod_usuario = c-cod-usuar:

    IF NOT CAN-FIND(FIRST tt-estab_usuar WHERE tt-estab_usuar.cod-estabel      = esp_estab_aprov.cod-estabel
                                           AND tt-estab_usuar.cod-usuar        = c-cod-usuar                   
                    ) THEN DO: 
          DELETE  esp_estab_aprov.
    END.

END.


FOR EACH tt-estab_usuar: 
                     
    IF NOT CAN-FIND(FIRST esp_estab_aprov WHERE esp_estab_aprov.cod-estabel = tt-estab_usuar.cod-estabel
                                                AND esp_estab_aprov.cod_usuario = c-cod-usuar) THEN DO:

        CREATE esp_estab_aprov.
        ASSIGN esp_estab_aprov.cod-estabel = tt-estab_usuar.cod-estabel
               esp_estab_aprov.cod_usuario = c-cod-usuar.
        
    END.

END.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod_usuario" "esp_estab_aprov" "cod_usuario"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
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
  {src/adm/template/snd-list.i "tt-estab_usuar"}
  {src/adm/template/snd-list.i "tt-estab"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

