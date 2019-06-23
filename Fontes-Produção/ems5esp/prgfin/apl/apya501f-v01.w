&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
          mgesp            PROGRESS
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

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

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

DEFINE VARIABLE l-ok AS LOGICAL NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE v_rec_banco AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_produt_financ AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_operac_financ AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-implanta AS LOG.
DEF BUFFER banco FOR ems5.banco.

DEF BUFFER bf-contrat_apf FOR contrat_apf.

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
&Scoped-define EXTERNAL-TABLES es_operac_financ
&Scoped-define FIRST-EXTERNAL-TABLE es_operac_financ


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_operac_financ.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_operac_financ.cod_livre_1 
&Scoped-define ENABLED-TABLES es_operac_financ
&Scoped-define FIRST-ENABLED-TABLE es_operac_financ
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold rt-mold-2 bt-arquivo-entrada 
&Scoped-Define DISPLAYED-FIELDS es_operac_financ.cod_contrat_apf ~
es_operac_financ.cod_banco es_operac_financ.cod_produt_financ ~
es_operac_financ.cod_operac_financ es_operac_financ.cod_livre_1 
&Scoped-define DISPLAYED-TABLES es_operac_financ
&Scoped-define FIRST-DISPLAYED-TABLE es_operac_financ
&Scoped-Define DISPLAYED-OBJECTS fi-banco fi-produt_financ c_arquivo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_operac_financ.cod_banco ~
es_operac_financ.cod_produt_financ es_operac_financ.cod_operac_financ 
&Scoped-define ADM-ASSIGN-FIELDS es_operac_financ.cod_contrat_apf ~
es_operac_financ.cod_banco es_operac_financ.cod_produt_financ ~
es_operac_financ.cod_operac_financ es_operac_financ.cod_livre_1 
&Scoped-define ADM-MODIFY-FIELDS es_operac_financ.cod_livre_1 

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
DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image/file.png":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c_arquivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .88 NO-UNDO.

DEFINE VARIABLE fi-banco AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-produt_financ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.33.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_operac_financ.cod_contrat_apf AT ROW 1.21 COL 20 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     es_operac_financ.cod_banco AT ROW 2.25 COL 20 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.14 BY .88
     fi-banco AT ROW 2.25 COL 33.43 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     es_operac_financ.cod_produt_financ AT ROW 3.25 COL 20 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-produt_financ AT ROW 3.25 COL 32.43 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     es_operac_financ.cod_operac_financ AT ROW 4.25 COL 20 COLON-ALIGNED WIDGET-ID 2
          LABEL "Operaá∆o Financeira"
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .88
     es_operac_financ.cod_livre_1 AT ROW 5.67 COL 9.71 WIDGET-ID 50
          LABEL "Obervaá‰es"
          VIEW-AS FILL-IN 
          SIZE 63 BY .88
     bt-arquivo-entrada AT ROW 7.13 COL 85.29 HELP
          "Escolha do nome do arquivo" WIDGET-ID 34
     c_arquivo AT ROW 7.17 COL 20 COLON-ALIGNED WIDGET-ID 58
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.5 COL 1
     rt-mold-2 AT ROW 7 COL 1 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es_operac_financ
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
         HEIGHT             = 7.29
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN es_operac_financ.cod_banco IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN es_operac_financ.cod_contrat_apf IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN es_operac_financ.cod_livre_1 IN FRAME f-main
   ALIGN-L 2 3 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN es_operac_financ.cod_operac_financ IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN es_operac_financ.cod_produt_financ IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN c_arquivo IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       c_arquivo:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN fi-banco IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-produt_financ IN FRAME f-main
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

&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada V-table-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-main
DO:
     SYSTEM-DIALOG GET-FILE c_arquivo
         INITIAL-DIR SESSION:TEMP-DIRECTORY
         RETURN-TO-START-DIR 
     TITLE "Arquivo de Contrato Operaá∆o Financeira".

     ASSIGN c_arquivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c_arquivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_operac_financ.cod_banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_banco V-table-Win
ON ENTRY OF es_operac_financ.cod_banco IN FRAME f-main /* Banco */
DO:
  FIND bf-contrat_apf WHERE
       ROWID(bf-contrat_apf) = v-row-parent NO-LOCK NO-ERROR.
    IF AVAIL bf-contrat_apf THEN
       ASSIGN es_operac_financ.cod_contrat_apf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = bf-contrat_apf.cod_contrat_apf.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_banco V-table-Win
ON F5 OF es_operac_financ.cod_banco IN FRAME f-main /* Banco */
DO:

    RUN prgint/utb/utb098ka.p.

    IF v_rec_banco <> ? THEN DO:

       FIND banco NO-LOCK 
             WHERE RECID(banco) = v_rec_banco NO-ERROR.

       ASSIGN es_operac_financ.cod_banco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = banco.cod_banco WHEN AVAIL banco.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_banco V-table-Win
ON LEAVE OF es_operac_financ.cod_banco IN FRAME f-main /* Banco */
DO:
    {include/leave.i &tabela=ems5.banco
               &atributo-ref=nom_banco
               &variavel-ref=fi-banco
               &where="ems5.banco.cod_banco = input frame {&frame-name} es_operac_financ.cod_banco"} 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_banco V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_operac_financ.cod_banco IN FRAME f-main /* Banco */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_operac_financ.cod_operac_financ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_operac_financ V-table-Win
ON F5 OF es_operac_financ.cod_operac_financ IN FRAME f-main /* Operaá∆o Financeira */
DO:
    RUN prgfin/apl/apl007ka.p.

    IF v_rec_operac_financ <> ? THEN DO:

       FIND operac_financ NO-LOCK 
             WHERE RECID(operac_financ) = v_rec_operac_financ NO-ERROR.

       ASSIGN es_operac_financ.cod_operac_financ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = operac_financ.cod_operac_financ WHEN AVAIL operac_financ
              es_operac_financ.cod_banco:SCREEN-VALUE IN FRAME {&FRAME-NAME} = operac_financ.cod_banco WHEN AVAIL operac_financ
              es_operac_financ.cod_produt_financ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = operac_financ.cod_produt_financ WHEN AVAIL operac_financ.

       APPLY "leave" TO es_operac_financ.cod_operac_financ.
       APPLY "leave" TO es_operac_financ.cod_banco.
       APPLY "leave" TO es_operac_financ.cod_produt_financ.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_operac_financ V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_operac_financ.cod_operac_financ IN FRAME f-main /* Operaá∆o Financeira */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_operac_financ.cod_produt_financ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_produt_financ V-table-Win
ON F5 OF es_operac_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
    RUN prgfin/apl/apl005ka.p.

    IF v_rec_produt_financ <> ? THEN DO:

       FIND produt_financ NO-LOCK 
             WHERE RECID(produt_financ) = v_rec_produt_financ NO-ERROR.

       ASSIGN es_operac_financ.cod_produt_financ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = produt_financ.cod_produt_financ WHEN AVAIL produt_financ.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_produt_financ V-table-Win
ON LEAVE OF es_operac_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
DO:
   {include/leave.i &tabela=ems5.produt_financ
               &atributo-ref=des_produt_financ
               &variavel-ref=fi-produt_financ
               &where="ems5.produt_financ.cod_produt_financ = input frame {&frame-name} es_operac_financ.cod_produt_financ"} 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_operac_financ.cod_produt_financ V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_operac_financ.cod_produt_financ IN FRAME f-main /* Produto Financeiro */
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
  es_operac_financ.cod_banco:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME} .
  es_operac_financ.cod_produt_financ:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  es_operac_financ.cod_operac_financ:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

  ASSIGN l-implanta = NO.

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
  {src/adm/template/row-list.i "es_operac_financ"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_operac_financ"}

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
  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

   RUN pi-validate.

   RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
   IF RETURN-VALUE = 'ADM-ERROR':U THEN 
        RETURN 'ADM-ERROR':U.
 
    IF AVAIL es_operac_financ THEN
        ASSIGN es_operac_financ.arquivo        = c_arquivo
               es_operac_financ.log_atualizado = yes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */



    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    find contrat_apf  where rowid(contrat_apf) = v-row-parent no-lock no-error.
    if available contrat_apf  then do:
        assign es_operac_financ.cod_contrat_apf = contrat_apf.cod_contrat_apf.
    end.
    
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
    
    IF AVAILABLE(es_operac_financ) THEN DO:
       APPLY "leave" TO es_operac_financ.cod_banco         IN FRAME {&FRAME-NAME}.
       APPLY "leave" TO es_operac_financ.cod_produt_financ IN FRAME {&FRAME-NAME}.

       ASSIGN c_arquivo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es_operac_financ.arquivo
              c_arquivo = es_operac_financ.arquivo.
        
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

    FIND es_operac_financ WHERE
         ROWID(es_operac_financ) = v-row-parent NO-ERROR.
    IF AVAILABLE(es_operac_financ) THEN DO:
       APPLY "leave" TO es_operac_financ.cod_banco         IN FRAME {&FRAME-NAME}.
       APPLY "leave" TO es_operac_financ.cod_produt_financ IN FRAME {&FRAME-NAME}.

       ASSIGN es_operac_financ.cod_contrat_apf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es_operac_financ.cod_contrat_apf.        
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    
    IF adm-new-record THEN DO:

       IF NOT CAN-FIND(FIRST ems5.banco
                       WHERE banco.cod_banco = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_banco) THEN DO:
         
          {utp/ut-table.i ems5 banco 1}
          run utp/ut-msgs.p (input "show":U, input 2, input return-value).
          RETURN 'ADM-ERROR':U.
       END.

       IF NOT CAN-FIND(FIRST produt_financ
                       WHERE produt_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_produt_financ) THEN DO:
         
          {utp/ut-table.i ems5 produt_financ 1}
          run utp/ut-msgs.p (input "show":U, input 2, input return-value).
          return 'ADM-ERROR':U.
       END.

       FIND FIRST operac_financ
                   WHERE operac_financ.cod_banco         = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_banco          AND
                         operac_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_produt_financ  AND
                         operac_financ.cod_operac_financ = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_operac_financ NO-LOCK NO-ERROR.
       IF NOT AVAIL operac_financ THEN DO:
             {utp/ut-table.i ems5 operac_financ 1}
             RUN utp/ut-msgs.p (input "show":U, input 1, input return-value).
             RETURN 'ADM-ERROR':U.
       END.
       ELSE DO:

           /* incluir validaá∆o do status da operaá∆o - ativa e encerrada */
           IF operac_financ.ind_sit_operac_financ_apl <> "Ativa" AND operac_financ.ind_sit_operac_financ_apl <> "Encerrada" THEN DO:

               MESSAGE "Operaá∆o financeira n∆o pode ser inclu°da no contrato, " SKIP
                       "pois encontra-se em digitaá∆o ou foi estornada."
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
               RETURN 'ADM-ERROR':U.
           END.

           /* Valida datas do Contrato */
           /* Data da operaá∆o maior que a data do contrato */
           IF (operac_financ.dat_vencto_operac_financ > bf-contrat_apf.dat_fim_valid AND  
               operac_financ.dat_operac_financ < bf-contrat_apf.dat_inic_valid) OR 
               /* Data da operaá∆o comeáa depois do in°cio do contrato e termina depois */
              (operac_financ.dat_vencto_operac_financ > bf-contrat_apf.dat_fim_valid AND
               operac_financ.dat_operac_financ > bf-contrat_apf.dat_inic_valid)       OR
               /* Operaá∆o inicia antes do in°cio do contrato */
              (operac_financ.dat_vencto_operac_financ <= bf-contrat_apf.dat_fim_valid AND
               operac_financ.dat_operac_financ < bf-contrat_apf.dat_inic_valid)      
           THEN DO:
    
             MESSAGE "Operaá∆o financeira com data de vencimento e/ou operaá∆o" skip
                     "fora da data do contrato." skip
                     "Data Operaá∆o Financeira: " operac_financ.dat_operac_financ SKIP 
                     "Data de vencimento: " operac_financ.dat_vencto_operac_financ skip
                     "Data inicial contrato:" bf-contrat_apf.dat_inic_valid SKIP
                     "Data final contrato: " bf-contrat_apf.dat_fim_valid
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             RETURN 'ADM-ERROR':U.
           END.  
       END.

      IF CAN-FIND(FIRST es_operac_financ
                  WHERE es_operac_financ.cod_contrat_apf   = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_contrat_apf    AND
                        es_operac_financ.cod_banco         = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_banco          AND
                        es_operac_financ.cod_produt_financ = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_produt_financ  AND
                        es_operac_financ.cod_operac_financ = INPUT FRAME {&FRAME-NAME} es_operac_financ.cod_operac_financ) THEN DO:
         {utp/ut-table.i mgesp es_operac_financ 1}
         RUN utp/ut-msgs.p (INPUT "show":U, INPUT 1, INPUT RETURN-VALUE).
         RETURN 'ADM-ERROR':U.
      END.
    END.      
    
    IF SEARCH(c_arquivo) = ? THEN DO:
        MESSAGE "Arquivo n∆o encontrado ou n∆o informado!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

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
  {src/adm/template/snd-list.i "es_operac_financ"}

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

