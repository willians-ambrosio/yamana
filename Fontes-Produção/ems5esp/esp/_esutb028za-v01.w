&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*
{include/i-prgvrs.i V14IN287 2.00.00.047}  /*** 010047 ***/



&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i v14in287 MUT}
&ENDIF
*/

/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */


def new global shared var v_rec_estabelecimento
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def new global shared var v_rec_mapa_distrib_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.


DEFINE BUFFER bf-mapa_distrib_ccusto FOR mapa_distrib_ccusto.

DEFINE BUFFER bf-item_lista_ccusto FOR item_lista_ccusto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-20 bt-busca-estab-orig ~
fi-cod_estab-orig bt-busca-mapa-de fi-cod_mapa_distrib_ccusto-de ~
bt-busca-mapa-para fi-cod_mapa_distrib_ccusto-para bt-busca-empr-dest ~
fi-cod_empresa-dest bt-busca-estab-dest fi-cod_estab-dest tg-ccusto 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_estab-orig fi-desc-estab-orig ~
fi-cod_mapa_distrib_ccusto-de fi-desc-mapa-de ~
fi-cod_mapa_distrib_ccusto-para fi-desc-mapa-para fi-cod_empresa-dest ~
fi-desc-empr-dest fi-cod_estab-dest fi-desc-estab-dest tg-ccusto 

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
DEFINE BUTTON bt-busca-empr-dest 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Dest" 
     SIZE 3.72 BY .92.

DEFINE BUTTON bt-busca-estab-dest 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Dest" 
     SIZE 3.72 BY .92.

DEFINE BUTTON bt-busca-estab-orig 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Orig" 
     SIZE 3.72 BY .92.

DEFINE BUTTON bt-busca-mapa-de 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Orig" 
     SIZE 3.72 BY .92.

DEFINE BUTTON bt-busca-mapa-para 
     IMAGE-UP FILE "image/im-zoo.bmp":U
     LABEL "Busca Estab Orig" 
     SIZE 3.72 BY .92.

DEFINE VARIABLE fi-cod_empresa-dest AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa Destino" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_estab-dest AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento Destino" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_estab-orig AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento Origem" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_mapa_distrib_ccusto-de AS CHARACTER FORMAT "x(8)" 
     LABEL "Mapa Dist CCusto DE" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod_mapa_distrib_ccusto-para AS CHARACTER FORMAT "x(8)" 
     LABEL "Mapa Dist CCusto ATê" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-empr-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estab-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estab-orig AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-mapa-de AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-mapa-para AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.14 BY 4.46.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.86 BY 10.46.

DEFINE VARIABLE tg-ccusto AS LOGICAL INITIAL no 
     LABEL "Copia Centros de Custo" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bt-busca-estab-orig AT ROW 2.21 COL 36.29 WIDGET-ID 8
     fi-cod_estab-orig AT ROW 2.25 COL 27 COLON-ALIGNED HELP
          "C¢digo Estabelecimento" WIDGET-ID 4
     fi-desc-estab-orig AT ROW 2.25 COL 38.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     bt-busca-mapa-de AT ROW 3.21 COL 39.29 WIDGET-ID 16
     fi-cod_mapa_distrib_ccusto-de AT ROW 3.25 COL 27 COLON-ALIGNED HELP
          "C¢digo Mapa Distribuiá∆o Centro Custo" WIDGET-ID 12
     fi-desc-mapa-de AT ROW 3.25 COL 41.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     bt-busca-mapa-para AT ROW 4.21 COL 39.29 WIDGET-ID 20
     fi-cod_mapa_distrib_ccusto-para AT ROW 4.25 COL 27 COLON-ALIGNED HELP
          "C¢digo Mapa Distribuiá∆o Centro Custo" WIDGET-ID 22
     fi-desc-mapa-para AT ROW 4.25 COL 41.29 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     bt-busca-empr-dest AT ROW 6.08 COL 36.29 WIDGET-ID 26
     fi-cod_empresa-dest AT ROW 6.13 COL 27 COLON-ALIGNED HELP
          "C¢digo Empresa" WIDGET-ID 28
     fi-desc-empr-dest AT ROW 6.13 COL 38.14 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     bt-busca-estab-dest AT ROW 7.13 COL 36.29 WIDGET-ID 32
     fi-cod_estab-dest AT ROW 7.17 COL 27 COLON-ALIGNED HELP
          "C¢digo Estabelecimento" WIDGET-ID 34
     fi-desc-estab-dest AT ROW 7.17 COL 38.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     tg-ccusto AT ROW 8.25 COL 29 WIDGET-ID 38
     RECT-19 AT ROW 1.04 COL 1.86 WIDGET-ID 2
     RECT-20 AT ROW 5.5 COL 2 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         HEIGHT             = 15.29
         WIDTH              = 85.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
    /*
{utp/ut-glob.i}
*/

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

/* SETTINGS FOR FILL-IN fi-desc-empr-dest IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-estab-dest IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-estab-orig IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-mapa-de IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-mapa-para IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-busca-empr-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-empr-dest V-table-Win
ON CHOOSE OF bt-busca-empr-dest IN FRAME f-main /* Busca Estab Dest */
DO:

    run prgint/utb/utb069ka.p /*prg_sea_empresa*/.

    if  v_rec_empresa <> ?
    then do:
        find ems5.empresa where recid(empresa) = v_rec_empresa no-lock no-error.
        IF fi-cod_empresa-dest:screen-value in frame {&FRAME-NAME} <> string(empresa.cod_empresa)THEN
            ASSIGN fi-cod_estab-dest:screen-value in frame {&FRAME-NAME} = "".

        assign fi-cod_empresa-dest:screen-value in frame {&FRAME-NAME} = string(empresa.cod_empresa).

        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-estab-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-estab-dest V-table-Win
ON CHOOSE OF bt-busca-estab-dest IN FRAME f-main /* Busca Estab Dest */
DO:
    run esp/esutb071na.p (Input INPUT FRAME {&FRAME-NAME} fi-cod_empresa-dest).

    if  v_rec_estabelecimento <> ? then do:
        find estabelecimento where recid(estabelecimento) = v_rec_estabelecimento no-lock no-error.
        assign fi-cod_estab-dest:screen-value in frame {&FRAME-NAME} = string(estabelecimento.cod_estab).

        assign fi-desc-estab-dest:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-estab-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-estab-orig V-table-Win
ON CHOOSE OF bt-busca-estab-orig IN FRAME f-main /* Busca Estab Orig */
DO:
    run prgint/utb/utb071na.p (Input v_cod_empres_usuar).

    if  v_rec_estabelecimento <> ? then do:
        find estabelecimento where recid(estabelecimento) = v_rec_estabelecimento no-lock no-error.
        assign fi-cod_estab-orig:screen-value in frame {&FRAME-NAME} = string(estabelecimento.cod_estab).

        assign fi-desc-estab-orig:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-mapa-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-mapa-de V-table-Win
ON CHOOSE OF bt-busca-mapa-de IN FRAME f-main /* Busca Estab Orig */
DO:

    run prgint/utb/utb028nb.p (Input input frame {&FRAME-NAME} fi-cod_estab-orig).

    if  v_rec_mapa_distrib_ccusto <> ?
    then do:
        find mapa_distrib_ccusto where recid(mapa_distrib_ccusto) = v_rec_mapa_distrib_ccusto no-lock no-error.
        assign fi-cod_mapa_distrib_ccusto-de:screen-value in frame {&FRAME-NAME} = string(mapa_distrib_ccusto.cod_mapa_distrib_ccusto).

        assign fi-desc-mapa-de:screen-value in frame {&FRAME-NAME} = mapa_distrib_ccusto.des_mapa_distrib_ccusto.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-mapa-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-mapa-para V-table-Win
ON CHOOSE OF bt-busca-mapa-para IN FRAME f-main /* Busca Estab Orig */
DO:

    run prgint/utb/utb028nb.p (Input input frame {&FRAME-NAME} fi-cod_estab-orig).

    if  v_rec_mapa_distrib_ccusto <> ?
    then do:
        find mapa_distrib_ccusto where recid(mapa_distrib_ccusto) = v_rec_mapa_distrib_ccusto no-lock no-error.
        assign fi-cod_mapa_distrib_ccusto-para:screen-value in frame {&FRAME-NAME} = string(mapa_distrib_ccusto.cod_mapa_distrib_ccusto).

        assign fi-desc-mapa-para:screen-value in frame {&FRAME-NAME} = mapa_distrib_ccusto.des_mapa_distrib_ccusto.

    end /* if */.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_empresa-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_empresa-dest V-table-Win
ON LEAVE OF fi-cod_empresa-dest IN FRAME f-main /* Empresa Destino */
DO:
    find FIRST ems5.empresa
        where empresa.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
        no-lock no-error.
    IF AVAILABLE ems5.empresa THEN DO:
        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = empresa.nom_razao_social.
    END.
    ELSE DO:
        assign fi-desc-empr-dest:screen-value in frame {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_estab-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_estab-dest V-table-Win
ON LEAVE OF fi-cod_estab-dest IN FRAME f-main /* Estabelecimento Destino */
DO:
    find estabelecimento 
        where estabelecimento.cod_empresa = fi-cod_empresa-dest:screen-value in frame {&FRAME-NAME}
        AND   estabelecimento.cod_estab   = fi-cod_estab-dest:screen-value in frame {&FRAME-NAME}
        no-lock no-error.
    IF AVAILABLE estabelecimento THEN DO:
        assign fi-desc-estab-dest:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.
    END.
    ELSE DO:
        assign fi-desc-estab-dest:screen-value in frame {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_estab-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_estab-orig V-table-Win
ON LEAVE OF fi-cod_estab-orig IN FRAME f-main /* Estabelecimento Origem */
DO:
    find estabelecimento 
        where estabelecimento.cod_estab = fi-cod_estab-orig:screen-value in frame {&FRAME-NAME}
        no-lock no-error.
    IF AVAILABLE estabelecimento THEN DO:
        assign fi-desc-estab-orig:screen-value in frame {&FRAME-NAME} = estabelecimento.nom_pessoa.
    END.
    ELSE DO:
        assign fi-desc-estab-orig:screen-value in frame {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_mapa_distrib_ccusto-de
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_mapa_distrib_ccusto-de V-table-Win
ON LEAVE OF fi-cod_mapa_distrib_ccusto-de IN FRAME f-main /* Mapa Dist CCusto DE */
DO:
    
    find mapa_distrib_ccusto 
        where mapa_distrib_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-orig
        AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = INPUT FRAME {&FRAME-NAME} fi-cod_mapa_distrib_ccusto-de
        no-lock no-error.

    IF AVAILABLE mapa_distrib_ccusto THEN DO:
        assign fi-desc-mapa-de:screen-value in frame {&FRAME-NAME} = mapa_distrib_ccusto.des_mapa_distrib_ccusto.
    END.
    ELSE DO:
        assign fi-desc-mapa-de:screen-value in frame {&FRAME-NAME} = "".
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_mapa_distrib_ccusto-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_mapa_distrib_ccusto-para V-table-Win
ON LEAVE OF fi-cod_mapa_distrib_ccusto-para IN FRAME f-main /* Mapa Dist CCusto ATê */
DO:
    find mapa_distrib_ccusto 
        where mapa_distrib_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-orig
        AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = INPUT FRAME {&FRAME-NAME} fi-cod_mapa_distrib_ccusto-para
        no-lock no-error.
    IF AVAILABLE mapa_distrib_ccusto THEN DO:
        assign fi-desc-mapa-para:screen-value in frame {&FRAME-NAME} = mapa_distrib_ccusto.des_mapa_distrib_ccusto.
    END.
    ELSE DO:
        assign fi-desc-mapa-para:screen-value in frame {&FRAME-NAME} = "".
    END.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/i-valid.i}
  /* Ponha na pi-validate todas as validaá‰es */
  /* N∆o gravar nada no registro antes do dispatch do assign-record e 
  nem na PI-validate. */  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  /* Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run get-attribute ('adm-new-record').
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

                                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava V-table-Win 
PROCEDURE pi-grava :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    find FIRST ems5.empresa
        where empresa.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
        no-lock no-error.
    IF NOT AVAILABLE ems5.empresa THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, input "Empresa inv†lida!~~Empresa < " + INPUT frame {&FRAME-NAME} fi-cod_empresa-dest + " > n∆o encontrada!").
        APPLY "ENTRY" TO fi-cod_empresa-dest.
        return error.
    END.

    find estabelecimento 
        where estabelecimento.cod_empresa = fi-cod_empresa-dest:screen-value in frame {&FRAME-NAME}
        AND   estabelecimento.cod_estab   = fi-cod_estab-dest:screen-value in frame {&FRAME-NAME}
        no-lock no-error.
    IF NOT AVAILABLE estabelecimento THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, input "Estabelecimento inv†lido!~~Estabelecimento < " + INPUT frame {&FRAME-NAME} fi-cod_estab-dest + " > n∆o encontrado para a empresa < " + fi-cod_empresa-dest:screen-value in frame {&FRAME-NAME} + " > !").
        APPLY "ENTRY" TO fi-cod_estab-dest.
        return error.
    END.

    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "esutb028za.txt").

    FOR EACH bf-mapa_distrib_ccusto NO-LOCK
        WHERE bf-mapa_distrib_ccusto.cod_estab                = INPUT FRAME {&FRAME-NAME} fi-cod_estab-orig
        AND   bf-mapa_distrib_ccusto.cod_mapa_distrib_ccusto >= INPUT FRAME {&FRAME-NAME} fi-cod_mapa_distrib_ccusto-de
        AND   bf-mapa_distrib_ccusto.cod_mapa_distrib_ccusto <= INPUT FRAME {&FRAME-NAME} fi-cod_mapa_distrib_ccusto-para
        AND   bf-mapa_distrib_ccusto.dat_inic_valid          <= TODAY
        AND   bf-mapa_distrib_ccusto.dat_fim_valid           >= TODAY
        :

        FIND FIRST mapa_distrib_ccusto NO-LOCK
            WHERE mapa_distrib_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-dest
            AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = bf-mapa_distrib_ccusto.cod_mapa_distrib_ccusto
            NO-ERROR.
        IF NOT AVAILABLE mapa_distrib_ccusto THEN DO:
            CREATE mapa_distrib_ccusto.
            BUFFER-COPY bf-mapa_distrib_ccusto EXCEPT cod_estab cod_empresa TO mapa_distrib_ccusto.
            ASSIGN mapa_distrib_ccusto.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab-dest
                   mapa_distrib_ccusto.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
                   .

            DISP 
                mapa_distrib_ccusto.cod_empresa 
                mapa_distrib_ccusto.cod_estab 
                mapa_distrib_ccusto.cod_plano_ccusto 
                mapa_distrib_ccusto.cod_mapa_distrib_ccusto 
                mapa_distrib_ccusto.des_mapa_distrib_ccusto 
                mapa_distrib_ccusto.dat_inic_valid 
                mapa_distrib_ccusto.dat_fim_valid 
                mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto 
                WITH WIDTH 333.

            IF LOGICAL (INPUT FRAME {&FRAME-NAME} tg-ccusto) THEN DO:
                FOR EACH bf-item_lista_ccusto OF bf-mapa_distrib_ccusto NO-LOCK:
                    CREATE item_lista_ccusto.
                    BUFFER-COPY bf-item_lista_ccusto EXCEPT cod_estab cod_empresa TO item_lista_ccusto.
                    ASSIGN item_lista_ccusto.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab-dest
                           item_lista_ccusto.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
                           .
                    DISP 
                        item_lista_ccusto.cod_empresa 
                        item_lista_ccusto.cod_estab 
                        item_lista_ccusto.cod_plano_ccusto 
                        item_lista_ccusto.cod_mapa_distrib_ccusto 
                        item_lista_ccusto.cod_ccusto 
                        WITH WIDTH 333.
                END.
            END.
        END.
        ELSE DO:
            IF LOGICAL (INPUT FRAME {&FRAME-NAME} tg-ccusto) THEN DO:
                FOR EACH bf-item_lista_ccusto OF bf-mapa_distrib_ccusto NO-LOCK:

                    FIND FIRST item_lista_ccusto NO-LOCK
                        WHERE item_lista_ccusto.cod_empresa             = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
                        AND   item_lista_ccusto.cod_estab               = INPUT FRAME {&FRAME-NAME} fi-cod_estab-dest
                        AND   item_lista_ccusto.cod_mapa_distrib_ccusto = bf-item_lista_ccusto.cod_mapa_distrib_ccusto
                        AND   item_lista_ccusto.cod_plano_ccusto        = bf-item_lista_ccusto.cod_plano_ccusto       
                        AND   item_lista_ccusto.cod_ccusto              = bf-item_lista_ccusto.cod_ccusto             
                        NO-ERROR.
                    IF NOT AVAILABLE item_lista_ccusto THEN DO:
                        CREATE item_lista_ccusto.
                        BUFFER-COPY bf-item_lista_ccusto EXCEPT cod_estab cod_empresa TO item_lista_ccusto.
                        ASSIGN item_lista_ccusto.cod_estab   = INPUT FRAME {&FRAME-NAME} fi-cod_estab-dest
                               item_lista_ccusto.cod_empresa = INPUT frame {&FRAME-NAME} fi-cod_empresa-dest
                               .
                        DISP 
                            item_lista_ccusto.cod_empresa 
                            item_lista_ccusto.cod_estab 
                            item_lista_ccusto.cod_plano_ccusto 
                            item_lista_ccusto.cod_mapa_distrib_ccusto 
                            item_lista_ccusto.cod_ccusto 
                            WITH WIDTH 333.
                    END.
                END.
            END.
        END.
    END.

    run utp/ut-msgs.p (input "show":U, input 15825, input "Registros gerados com sucesso!").

    OS-COMMAND NO-WAIT VALUE(SESSION:TEMP-DIRECTORY + "esutb028za.txt").



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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

