&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/****************************************************************************************** 
**         Programa: apya501-v01.p
**        Autor: 
**       Fornecedor: DKP
**         Data: 
** Change/Chamado: 
**      Objetivo: Viewer do cadastro de contrato pai - apya501.w
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
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



DEFINE NEW GLOBAL SHARED VARIABLE h-viewer AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR  v_rec_indic_econ AS RECID format ">>>>>>9":U initial ? no-undo.

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
&Scoped-define EXTERNAL-TABLES contrat_apf
&Scoped-define FIRST-EXTERNAL-TABLE contrat_apf


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contrat_apf.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contrat_apf.des_contrat_apf ~
contrat_apf.cod_admdra_apf 
&Scoped-define ENABLED-TABLES contrat_apf
&Scoped-define FIRST-ENABLED-TABLE contrat_apf
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold rt-key-2 BUTTON-1 
&Scoped-Define DISPLAYED-FIELDS contrat_apf.cod_empresa ~
contrat_apf.cod_contrat_apf contrat_apf.des_contrat_apf ~
contrat_apf.cod_admdra_apf contrat_apf.cod_indic_econ ~
contrat_apf.dat_inic_valid contrat_apf.dat_fim_valid ~
contrat_apf.val_lim_cr_contrat_total contrat_apf.val_lim_cr_contrat_apf ~
contrat_apf.val_aditivos contrat_apf.val_operac_financ 
&Scoped-define DISPLAYED-TABLES contrat_apf
&Scoped-define FIRST-DISPLAYED-TABLE contrat_apf
&Scoped-Define DISPLAYED-OBJECTS fi-empresa fi-adm fi-moeda Fi-dt-adic ~
fi-saldo c-arquivo-entrada 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS contrat_apf.cod_empresa ~
contrat_apf.cod_contrat_apf contrat_apf.cod_indic_econ ~
contrat_apf.dat_inic_valid contrat_apf.dat_fim_valid ~
contrat_apf.val_lim_cr_contrat_total contrat_apf.val_lim_cr_contrat_apf ~
c-arquivo-entrada 
&Scoped-define ADM-ASSIGN-FIELDS contrat_apf.cod_indic_econ ~
contrat_apf.dat_inic_valid contrat_apf.dat_fim_valid Fi-dt-adic ~
contrat_apf.val_lim_cr_contrat_total contrat_apf.val_lim_cr_contrat_apf 
&Scoped-define ADM-MODIFY-FIELDS c-arquivo-entrada 

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
     IMAGE-INSENSITIVE FILE "image/file.png":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "prgfin/image/contrato.png":U
     LABEL "Button 1" 
     SIZE 4.57 BY 1.25 TOOLTIP "Contrato".

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 61.86 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi-adm AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .88 NO-UNDO.

DEFINE VARIABLE Fi-dt-adic AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Adiá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-moeda AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .88 NO-UNDO.

DEFINE VARIABLE fi-saldo AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo (b + c - d)" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.25.

DEFINE RECTANGLE rt-key-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.38.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 9.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     contrat_apf.cod_empresa AT ROW 1.17 COL 20 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     fi-empresa AT ROW 1.17 COL 27.43 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     BUTTON-1 AT ROW 1.25 COL 83 WIDGET-ID 40
     contrat_apf.cod_contrat_apf AT ROW 2.17 COL 20 COLON-ALIGNED WIDGET-ID 2
          LABEL "Contrato"
          VIEW-AS FILL-IN 
          SIZE 40 BY .88
     contrat_apf.des_contrat_apf AT ROW 3.67 COL 20 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     contrat_apf.cod_admdra_apf AT ROW 4.67 COL 20 COLON-ALIGNED WIDGET-ID 4
          LABEL "Administradora"
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     fi-adm AT ROW 4.67 COL 41.43 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     contrat_apf.cod_indic_econ AT ROW 5.67 COL 20 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-moeda AT ROW 5.67 COL 32.43 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     contrat_apf.dat_inic_valid AT ROW 6.67 COL 20 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     contrat_apf.dat_fim_valid AT ROW 6.67 COL 60 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     Fi-dt-adic AT ROW 7.54 COL 60 COLON-ALIGNED WIDGET-ID 54
     contrat_apf.val_lim_cr_contrat_total AT ROW 7.75 COL 20 COLON-ALIGNED WIDGET-ID 42
          LABEL "Valor do Contrato (a)"
          VIEW-AS FILL-IN 
          SIZE 22 BY .88
     contrat_apf.val_lim_cr_contrat_apf AT ROW 8.75 COL 20 COLON-ALIGNED WIDGET-ID 10
          LABEL "Saldo Inicial (b)"
          VIEW-AS FILL-IN 
          SIZE 21.72 BY .88
     contrat_apf.val_aditivos AT ROW 9.75 COL 20 COLON-ALIGNED WIDGET-ID 50
          LABEL "Aditivos (c)"
          VIEW-AS FILL-IN 
          SIZE 21.72 BY .88
     contrat_apf.val_operac_financ AT ROW 10.67 COL 20 COLON-ALIGNED WIDGET-ID 52
          LABEL "Operaá‰es (d)"
          VIEW-AS FILL-IN 
          SIZE 21.72 BY .75
     fi-saldo AT ROW 11.67 COL 20 COLON-ALIGNED WIDGET-ID 48
     bt-arquivo-entrada AT ROW 13.13 COL 84.72 HELP
          "Escolha do nome do arquivo" WIDGET-ID 34
     c-arquivo-entrada AT ROW 13.17 COL 22 NO-LABEL WIDGET-ID 36
     "Arquivo:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 13.25 COL 13.57 WIDGET-ID 38
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
     rt-key-2 AT ROW 12.92 COL 1 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.contrat_apf
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
         HEIGHT             = 13.42
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

/* SETTINGS FOR BUTTON bt-arquivo-entrada IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo-entrada IN FRAME f-main
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN contrat_apf.cod_admdra_apf IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN contrat_apf.cod_contrat_apf IN FRAME f-main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN contrat_apf.cod_empresa IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN contrat_apf.cod_indic_econ IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN contrat_apf.dat_fim_valid IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN contrat_apf.dat_inic_valid IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN fi-adm IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-dt-adic IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-moeda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-saldo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contrat_apf.val_aditivos IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN contrat_apf.val_lim_cr_contrat_apf IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN contrat_apf.val_lim_cr_contrat_total IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN contrat_apf.val_operac_financ IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
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
    {include/i-imarq.i c-arquivo-entrada {&FRAME-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 V-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME f-main /* Button 1 */
DO:
    IF AVAIL contrat_apf THEN
        RUN prgfin/apl/apya599.p (INPUT contrat_apf.arquivo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contrat_apf.cod_admdra_apf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_admdra_apf V-table-Win
ON F5 OF contrat_apf.cod_admdra_apf IN FRAME f-main /* Administradora */
DO:
    {include/zoomvar.i &prog-zoom="prgfin/apl/apya502-z01.w"
                     &campo=contrat_apf.cod_admdra_apf
                     &campozoom=cod_admdra_apf
                     &campo2=fi-adm 
                     &campozoom2=nom_razao_social}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_admdra_apf V-table-Win
ON LEAVE OF contrat_apf.cod_admdra_apf IN FRAME f-main /* Administradora */
DO:
   {include/leave.i &tabela=admdra_apf
               &atributo-ref=nom_razao_social
               &variavel-ref=fi-adm
               &where="admdra_apf.cod_admdra_apf = input frame {&frame-name} contrat_apf.cod_admdra_apf"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_admdra_apf V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contrat_apf.cod_admdra_apf IN FRAME f-main /* Administradora */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contrat_apf.cod_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_empresa V-table-Win
ON F5 OF contrat_apf.cod_empresa IN FRAME f-main /* Empresa */
DO:
   {include/zoomvar.i &prog-zoom="prgfin/apl/apya501-z02.w"
                     &campo=contrat_apf.cod_empresa
                     &campozoom=cod_empresa
                     &campo2=fi-empresa
                     &campozoom2=nom_razao_social}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_empresa V-table-Win
ON LEAVE OF contrat_apf.cod_empresa IN FRAME f-main /* Empresa */
DO:
   {include/leave.i &tabela=ems5.empresa
               &atributo-ref=nom_razao_social
               &variavel-ref=fi-empresa
               &where="ems5.empresa.cod_empresa = input frame {&frame-name} contrat_apf.cod_empresa"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF contrat_apf.cod_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contrat_apf.cod_indic_econ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_indic_econ V-table-Win
ON F5 OF contrat_apf.cod_indic_econ IN FRAME f-main /* Moeda */
DO:
    run prgint/utb/utb013ka.p.
    if  v_rec_indic_econ <> ?
    then do:

         find indic_econ where recid(indic_econ) = v_rec_indic_econ no-lock no-error.
         if AVAIL indic_econ THEN 
            ASSIGN contrat_apf.cod_indic_econ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = indic_econ.cod_indic_econ
                   fi-moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = indic_econ.des_indic_econ.  

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_indic_econ V-table-Win
ON LEAVE OF contrat_apf.cod_indic_econ IN FRAME f-main /* Moeda */
DO:
   {include/leave.i &tabela=indic_econ
               &atributo-ref=des_indic_econ
               &variavel-ref=fi-moeda
               &where="indic_econ.cod_indic_econ = input frame {&frame-name} contrat_apf.cod_indic_econ"}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.cod_indic_econ V-table-Win
ON LEFT-MOUSE-DBLCLICK OF contrat_apf.cod_indic_econ IN FRAME f-main /* Moeda */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contrat_apf.val_lim_cr_contrat_total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contrat_apf.val_lim_cr_contrat_total V-table-Win
ON LEAVE OF contrat_apf.val_lim_cr_contrat_total IN FRAME f-main /* Valor do Contrato (a) */
DO:
  ASSIGN contrat_apf.val_lim_cr_contrat_apf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(contrat_apf.val_lim_cr_contrat_total:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
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
contrat_apf.cod_empresa:LOAD-MOUSE-POINTER("image/lupa.cur").
contrat_apf.cod_admdra_apf:LOAD-MOUSE-POINTER("image/lupa.cur").
contrat_apf.cod_indic_econ:LOAD-MOUSE-POINTER("image/lupa.cur").

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
  {src/adm/template/row-list.i "contrat_apf"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contrat_apf"}

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
    c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    
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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
        RUN pi-validate.

   ASSIGN c-arquivo-entrada = c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
   ASSIGN contrat_apf.arquivo = c-arquivo-entrada.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF CAN-FIND(FIRST aditivo_contrat_apf  OF contrat_apf) THEN DO:

    MESSAGE "Contrato possui aditivos e n∆o pode ser exclu°do!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN 'NOK'.
END.

IF CAN-FIND(FIRST es_operac_financ WHERE 
                   es_operac_financ.cod_contrat_apf = contrat_apf.cod_contrat_apf) THEN DO:

    MESSAGE "Contrato m∆e possui Operaá∆o Financeira e n∆o poder† ser exclu°do!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RETURN "NOK".
END.
                                                             

RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
    
    /* Code placed here will execute AFTER standard behavior.    */
    APPLY "leave" TO contrat_apf.cod_empresa    IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO contrat_apf.cod_admdra_apf IN FRAME {&FRAME-NAME}.
    APPLY "leave" TO contrat_apf.cod_indic_econ IN FRAME {&FRAME-NAME}.

    IF AVAILABLE(contrat_apf) THEN DO:
       ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = contrat_apf.arquivo.

       RUN pi-atualiza-total (2).


       ASSIGN Fi-dt-adic:SCREEN-VALUE IN frame {&FRAME-NAME} = ?.

       FOR LAST aditivo_contrat_apf OF contrat_apf NO-LOCK USE-INDEX idx1:
         
           ASSIGN Fi-dt-adic:SCREEN-VALUE IN frame {&FRAME-NAME} = string(aditivo_contrat_apf.dat_fim_valid).
       END.

       ASSIGN button-1:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    END.
    ELSE ASSIGN button-1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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
    bt-arquivo-entrada:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    &endif
    
    ASSIGN h-viewer = THIS-PROCEDURE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-total V-table-Win 
PROCEDURE pi-atualiza-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-acao AS INTEGER NO-UNDO.

    IF AVAILABLE(contrat_apf) THEN DO:
       RUN prgfin/apl/apya598.p (INPUT ROWID(contrat_apf)).

       ASSIGN fi-saldo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING((contrat_apf.val_lim_cr_contrat_apf + contrat_apf.val_aditivo) - contrat_apf.val_operac_financ).
    END.

    IF ip-acao = 1 THEN
       RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  
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
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */


   IF adm-new-record THEN DO:


       IF NOT CAN-FIND(FIRST ems5.empresa
                       WHERE empresa.cod_empresa = INPUT FRAME {&FRAME-NAME} contrat_apf.cod_empresa
                       NO-LOCK) THEN DO:
          {include/i-vldprg.i}                                            
          {utp/ut-table.i ems5 empresa 1}
          run utp/ut-msgs.p (input "show":U, input 2, input return-value).
          return 'ADM-ERROR':U.                                           

       END.

       IF INPUT FRAME {&FRAME-NAME} contrat_apf.cod_contrat_apf = "" THEN DO:
          {include/i-vldprg.i}                                            
          {utp/ut-field.i mgesp contrat_apf cod_contrat_apf 1}             
          run utp/ut-msgs.p (input "show":U, input 164, input return-value).  
          return 'ADM-ERROR':U.                                           
       END.

       IF CAN-FIND(FIRST contrat_apf
                   WHERE contrat_apf.cod_empresa      = INPUT FRAME {&FRAME-NAME} contrat_apf.cod_empresa  AND
                         contrat_apf.cod_contrat_apf  = INPUT FRAME {&FRAME-NAME} contrat_apf.cod_contrat_apf
                   NO-LOCK) THEN DO:
          {include/i-vldprg.i}                                            
          {utp/ut-table.i mgesp contrat_apf 1}
          run utp/ut-msgs.p (input "show":U, input 1, input return-value).
          return 'ADM-ERROR':U.                                           

       END.
   END.

   IF INPUT FRAME {&FRAME-NAME} contrat_apf.des_contrat_apf = "" THEN DO:
      {include/i-vldprg.i}                                            
      {utp/ut-field.i mgesp contrat_apf des_contrat_apf 1}             
      run utp/ut-msgs.p (input "show":U, input 164, input return-value).  
      return 'ADM-ERROR':U.                                           
   END.

   IF NOT CAN-FIND(FIRST admdra_apf
                   WHERE admdra_apf.cod_admdra_apf = INPUT FRAME {&FRAME-NAME} contrat_apf.cod_admdra_apf
                   NO-LOCK) THEN DO:
      {include/i-vldprg.i}                                            
      {utp/ut-table.i mgesp admdra_apf 1}
      run utp/ut-msgs.p (input "show":U, input 2, input return-value).
      return 'ADM-ERROR':U.                                           

   END.

   IF NOT CAN-FIND(FIRST indic_econ
                   WHERE indic_econ.cod_indic_econ = INPUT FRAME {&FRAME-NAME} contrat_apf.cod_indic_econ
                   NO-LOCK) THEN DO:
      {include/i-vldprg.i}                                            
      {utp/ut-table.i ems5 indic_econ 1}
      run utp/ut-msgs.p (input "show":U, input 2, input return-value).
      return 'ADM-ERROR':U.                                           

   END.

   IF INPUT FRAME {&FRAME-NAME} contrat_apf.dat_inic_valid > INPUT FRAME {&FRAME-NAME} contrat_apf.dat_fim_valid THEN DO:
      {include/i-vldprg.i}                                            
      run utp/ut-msgs.p (input "show":U, input 142, input "Validade~~Validade").   
      return 'ADM-ERROR':U.                                           
   END.

   IF INPUT FRAME {&FRAME-NAME} c-arquivo-entrada = "" THEN DO:
      {include/i-vldprg.i}                                            
      run utp/ut-msgs.p (input "show":U, input 164, input "Arquivo").  
      return 'ADM-ERROR':U.                                           
   END.


    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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
  {src/adm/template/snd-list.i "contrat_apf"}

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

