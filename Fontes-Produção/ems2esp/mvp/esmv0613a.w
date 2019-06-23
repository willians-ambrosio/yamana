&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0613A 2.00.00.001}  /*** 010004 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0613A
&GLOBAL-DEFINE Version        2.00.00.001

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE Folder         NO

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 ~
                              fi-cc-fim fi-cc-ini fi-empresa-fim fi-empresa-ini ~
                              fi-equipto-fim fi-equipto-ini fi-estab-fim ~
                              fi-estab-ini fi-evento-fim fi-evento-ini fi-grupo-fim ~
                              fi-grupo-ini fi-modelo-fim fi-modelo-ini fi-oficina-fim ~
                              fi-oficina-ini fi-plan-fim fi-plan-ini fi-setor-fim fi-setor-ini ~
                              fi-tag-ini fi-tag-fim fi-sub-sist-ini fi-sub-sist-fim

{mvp/esmv0613.i} /** Defini‡Æo da ttSelecao **/

/* Parameters Definitions ---                                           */
define input-output parameter table for ttSelecao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 ~
IMAGE-15 IMAGE-16 IMAGE-19 IMAGE-20 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 ~
IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-5 IMAGE-6 ~
IMAGE-7 IMAGE-8 IMAGE-9 rtToolBar fi-empresa-ini fi-empresa-fim ~
fi-equipto-ini fi-equipto-fim fi-grupo-ini fi-grupo-fim fi-modelo-ini ~
fi-modelo-fim fi-estab-ini fi-estab-fim fi-tag-ini fi-tag-fim fi-cc-ini ~
fi-cc-fim fi-plan-ini fi-plan-fim fi-setor-ini fi-setor-fim fi-oficina-ini ~
fi-oficina-fim fi-evento-ini fi-evento-fim fi-sub-sist-ini fi-sub-sist-fim ~
btOK btCancel btHelp2 
&Scoped-Define DISPLAYED-OBJECTS fi-empresa-ini fi-empresa-fim ~
fi-equipto-ini fi-equipto-fim fi-grupo-ini fi-grupo-fim fi-modelo-ini ~
fi-modelo-fim fi-estab-ini fi-estab-fim fi-tag-ini fi-tag-fim fi-cc-ini ~
fi-cc-fim fi-plan-ini fi-plan-fim fi-setor-ini fi-setor-fim fi-oficina-ini ~
fi-oficina-fim fi-evento-ini fi-evento-fim fi-sub-sist-ini fi-sub-sist-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-evento-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-evento-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Evento" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-grupo-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-grupo-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Grupo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-modelo-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-modelo-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Modelo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-oficina-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-oficina-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Oficina":R9 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-plan-fim AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-plan-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Planejador":R12 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-setor-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-setor-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Setor Oficina":R15 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sub-sist-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-sub-sist-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Sub-Sistema":R14 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88.

DEFINE VARIABLE fi-tag-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tag-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Tag":R9 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 65 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     fi-empresa-ini AT ROW 1.33 COL 16.86 COLON-ALIGNED
     fi-empresa-fim AT ROW 1.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-equipto-ini AT ROW 2.33 COL 16.86 COLON-ALIGNED
     fi-equipto-fim AT ROW 2.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-grupo-ini AT ROW 3.33 COL 16.86 COLON-ALIGNED
     fi-grupo-fim AT ROW 3.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-modelo-ini AT ROW 4.33 COL 16.86 COLON-ALIGNED
     fi-modelo-fim AT ROW 4.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-estab-ini AT ROW 5.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo do estabelecimento."
     fi-estab-fim AT ROW 5.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo do estabelecimento." NO-LABEL
     fi-tag-ini AT ROW 6.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo da TAG"
     fi-tag-fim AT ROW 6.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo da TAG" NO-LABEL
     fi-cc-ini AT ROW 7.33 COL 16.86 COLON-ALIGNED
     fi-cc-fim AT ROW 7.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-plan-ini AT ROW 8.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo do planejador"
     fi-plan-fim AT ROW 8.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo do planejador" NO-LABEL
     fi-setor-ini AT ROW 9.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo do Setor da Oficina"
     fi-setor-fim AT ROW 9.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo do Setor da Oficina" NO-LABEL
     fi-oficina-ini AT ROW 10.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo da Oficina"
     fi-oficina-fim AT ROW 10.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo da Oficina" NO-LABEL
     fi-evento-ini AT ROW 11.33 COL 16.86 COLON-ALIGNED
     fi-evento-fim AT ROW 11.33 COL 43.57 COLON-ALIGNED NO-LABEL
     fi-sub-sist-ini AT ROW 12.33 COL 16.86 COLON-ALIGNED HELP
          "C¢digo Sub-Sistema"
     fi-sub-sist-fim AT ROW 12.33 COL 43.57 COLON-ALIGNED HELP
          "C¢digo Sub-Sistema" NO-LABEL
     btOK AT ROW 13.83 COL 2
     btCancel AT ROW 13.83 COL 13
     btHelp2 AT ROW 13.83 COL 55
     IMAGE-10 AT ROW 4.33 COL 41.86
     IMAGE-11 AT ROW 5.33 COL 41.86
     IMAGE-12 AT ROW 5.33 COL 35.86
     IMAGE-13 AT ROW 7.33 COL 41.86
     IMAGE-14 AT ROW 7.33 COL 35.86
     IMAGE-15 AT ROW 8.33 COL 35.86
     IMAGE-16 AT ROW 8.33 COL 41.86
     IMAGE-19 AT ROW 10.33 COL 41.86
     IMAGE-20 AT ROW 10.33 COL 35.86
     IMAGE-21 AT ROW 6.33 COL 41.86
     IMAGE-22 AT ROW 6.33 COL 35.86
     IMAGE-23 AT ROW 9.33 COL 41.86
     IMAGE-24 AT ROW 9.33 COL 35.86
     IMAGE-29 AT ROW 1.33 COL 42.14
     IMAGE-30 AT ROW 1.33 COL 36.14
     IMAGE-31 AT ROW 11.33 COL 41.86
     IMAGE-32 AT ROW 11.33 COL 35.86
     IMAGE-33 AT ROW 12.33 COL 35.86
     IMAGE-34 AT ROW 12.33 COL 41.86
     IMAGE-5 AT ROW 2.33 COL 35.86
     IMAGE-6 AT ROW 2.33 COL 41.86
     IMAGE-7 AT ROW 3.38 COL 35.86
     IMAGE-8 AT ROW 3.38 COL 41.86
     IMAGE-9 AT ROW 4.33 COL 35.86
     rtToolBar AT ROW 13.63 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 65 BY 14.04
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 14.04
         WIDTH              = 65
         MAX-HEIGHT         = 14.92
         MAX-WIDTH          = 65
         VIRTUAL-HEIGHT     = 14.92
         VIRTUAL-WIDTH      = 65
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    run piGrava in this-procedure.  
    apply "CLOSE":U to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{utp/ut-liter.i "Empresa"}
assign fi-empresa-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Equipamento"}
assign fi-equipto-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Grupo"}
assign fi-grupo-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Modelo"}
assign fi-modelo-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Estabelecimento"}
assign fi-estab-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Centro Custo"}
assign fi-cc-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Planejador"}
assign fi-plan-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Setor Oficina"}
assign fi-setor-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Oficina"}
assign fi-oficina-ini:label in frame fPage0 = trim(return-value).
{utp/ut-liter.i "Evento"}
assign fi-evento-ini:label in frame fPage0 = trim(return-value).


do with frame fPage0:
    find first ttSelecao no-lock no-error.
    if avail ttSelecao then do:
        assign fi-empresa-ini:screen-value  = string(ttSelecao.empresa-ini)
               fi-empresa-fim:screen-value  = string(ttSelecao.empresa-fim)
               fi-equipto-ini:screen-value  = ttSelecao.equipto-ini
               fi-equipto-fim:screen-value  = ttSelecao.equipto-fim
               fi-grupo-ini:screen-value    = ttSelecao.grupo-ini  
               fi-grupo-fim:screen-value    = ttSelecao.grupo-fim  
               fi-modelo-ini:screen-value   = ttSelecao.modelo-ini 
               fi-modelo-fim:screen-value   = ttSelecao.modelo-fim 
               fi-estab-ini:screen-value    = ttSelecao.estab-ini  
               fi-estab-fim:screen-value    = ttSelecao.estab-fim  
               fi-cc-ini:screen-value       = ttSelecao.centro-ini 
               fi-cc-fim:screen-value       = ttSelecao.centro-fim 
               fi-plan-ini:screen-value     = ttSelecao.planej-ini 
               fi-plan-fim:screen-value     = ttSelecao.planej-fim 
               fi-setor-ini:screen-value    = ttSelecao.setor-ini  
               fi-setor-fim:screen-value    = ttSelecao.setor-fim  
               fi-oficina-ini:screen-value  = ttSelecao.oficina-ini
               fi-oficina-fim:screen-value  = ttSelecao.oficina-fim
               fi-evento-ini:screen-value   = ttSelecao.evento-ini 
               fi-evento-fim:screen-value   = ttSelecao.evento-fim
               fi-tag-ini:screen-value      = ttSelecao.tag-ini
               fi-tag-fim:screen-value      = ttSelecao.tag-fim
               fi-sub-sist-ini:screen-value = ttSelecao.sub-sist-ini
               fi-sub-sist-fim:screen-value = ttSelecao.sub-sist-fim.
    end.
end.
           
RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava wWindow 
PROCEDURE piGrava :
/*------------------------------------------------------------------------------
  Purpose:     piGrava
  Parameters:  <none>
  Notes:       Grava os dados de tela na temp-table 
------------------------------------------------------------------------------*/
find first ttSelecao exclusive-lock no-error.
if avail ttSelecao then do:
    assign ttSelecao.empresa-ini  =  fi-empresa-ini:screen-value  in frame fPage0    
           ttSelecao.empresa-fim  =  fi-empresa-fim:screen-value  in frame fPage0    
           ttSelecao.equipto-ini  =  fi-equipto-ini:screen-value  in frame fPage0    
           ttSelecao.equipto-fim  =  fi-equipto-fim:screen-value  in frame fPage0    
           ttSelecao.grupo-ini    =  fi-grupo-ini:screen-value    in frame fPage0      
           ttSelecao.grupo-fim    =  fi-grupo-fim:screen-value    in frame fPage0      
           ttSelecao.modelo-ini   =  fi-modelo-ini:screen-value   in frame fPage0     
           ttSelecao.modelo-fim   =  fi-modelo-fim:screen-value   in frame fPage0     
           ttSelecao.estab-ini    =  fi-estab-ini:screen-value    in frame fPage0      
           ttSelecao.estab-fim    =  fi-estab-fim:screen-value    in frame fPage0      
           ttSelecao.centro-ini   =  fi-cc-ini:screen-value       in frame fPage0        
           ttSelecao.centro-fim   =  fi-cc-fim:screen-value       in frame fPage0        
           ttSelecao.planej-ini   =  fi-plan-ini:screen-value     in frame fPage0     
           ttSelecao.planej-fim   =  fi-plan-fim:screen-value     in frame fPage0     
           ttSelecao.setor-ini    =  fi-setor-ini:screen-value    in frame fPage0     
           ttSelecao.setor-fim    =  fi-setor-fim:screen-value    in frame fPage0     
           ttSelecao.oficina-ini  =  fi-oficina-ini:screen-value  in frame fPage0        
           ttSelecao.oficina-fim  =  fi-oficina-fim:screen-value  in frame fPage0        
           ttSelecao.evento-ini   =  fi-evento-ini:screen-value   in frame fPage0
           ttSelecao.evento-fim   =  fi-evento-fim:screen-value   in frame fPage0
           ttSelecao.tag-ini      =  fi-tag-ini:screen-value      in frame fPage0
           ttSelecao.tag-fim      =  fi-tag-fim:screen-value      in frame fPage0
           ttSelecao.sub-sist-ini =  fi-sub-sist-ini:screen-value in frame fPage0
           ttSelecao.sub-sist-fim =  fi-sub-sist-fim:screen-value in frame fPage0.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

