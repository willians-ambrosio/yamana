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
{include/i-prgvrs.i ESMV0603A 2.03.00.003}  /*** 010003 ***/
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
&GLOBAL-DEFINE Program        ESMV0603A
&GLOBAL-DEFINE Version        2.03.00.003

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE Folder         NO

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2 fi-estab-ini fi-estab-fim fi-empresa-ini ~
                              fi-empresa-fim fi-equipto-ini fi-equipto-fim fi-grp-evento-ini fi-grp-evento-fim ~
                              fi-grupo-ini fi-grupo-fim fi-modelo-ini fi-modelo-fim fi-evento-ini ~
                              fi-evento-fim fi-ccusto-ini fi-ccusto-fim fi-sistema-ini fi-sistema-fim fi-sub-sist-ini ~
                              fi-sub-sist-fim fi-periodo-ini fi-periodo-fim fi-tag-ini fi-tag-fim fiCausaIni fiCausaFim fiSintomaIni fiSintomaFim

{mvp/esmv0603.i} /** Defini‡Æo da ttSelecao **/

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
IMAGE-25 IMAGE-26 IMAGE-29 IMAGE-30 IMAGE-5 IMAGE-51 IMAGE-52 IMAGE-6 ~
IMAGE-7 IMAGE-8 IMAGE-9 RECT-23 rtToolBar IMAGE-57 IMAGE-58 IMAGE-55 ~
IMAGE-59 fi-periodo-ini fi-periodo-fim fi-empresa-ini fi-empresa-fim ~
fi-equipto-ini fi-equipto-fim fi-grupo-ini fi-grupo-fim fi-modelo-ini ~
fi-modelo-fim fi-estab-ini fi-estab-fim fi-grp-evento-ini fi-grp-evento-fim ~
fi-evento-ini fi-evento-fim fi-ccusto-ini fi-ccusto-fim fi-sistema-ini ~
fi-sistema-fim fi-sub-sist-ini fi-sub-sist-fim fi-tag-ini fi-tag-fim ~
fiCausaini fiCausaFim fiSintomaIni fiSintomaFim btOK btCancel btHelp2 
&Scoped-Define DISPLAYED-OBJECTS fi-periodo-ini fi-periodo-fim ~
fi-empresa-ini fi-empresa-fim fi-equipto-ini fi-equipto-fim fi-grupo-ini ~
fi-grupo-fim fi-modelo-ini fi-modelo-fim fi-estab-ini fi-estab-fim ~
fi-grp-evento-ini fi-grp-evento-fim fi-evento-ini fi-evento-fim ~
fi-ccusto-ini fi-ccusto-fim fi-sistema-ini fi-sistema-fim fi-sub-sist-ini ~
fi-sub-sist-fim fi-tag-ini fi-tag-fim fiCausaini fiCausaFim fiSintomaIni ~
fiSintomaFim 

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

DEFINE VARIABLE fi-ccusto-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ccusto-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS INTEGER FORMAT ">>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-evento-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-evento-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Evento" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-grp-evento-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-grp-evento-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Grupo Evento" 
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

DEFINE VARIABLE fi-periodo-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1800 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sistema-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sistema-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Sistema" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sub-sist-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-sub-sist-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Sub Sistema":R11 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tag-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tag-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiCausaFim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiCausaini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Causa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiSintomaFim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiSintomaIni AS CHARACTER FORMAT "X(8)":U 
     LABEL "Sintoma" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-55
     FILENAME "image/im-las":U
     SIZE 5.43 BY .88.

DEFINE IMAGE IMAGE-57
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-58
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image/im-las":U
     SIZE 5.43 BY .88.

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

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 15.25.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     fi-periodo-ini AT ROW 1.33 COL 13 COLON-ALIGNED
     fi-periodo-fim AT ROW 1.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-empresa-ini AT ROW 2.33 COL 13 COLON-ALIGNED
     fi-empresa-fim AT ROW 2.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-equipto-ini AT ROW 3.33 COL 13 COLON-ALIGNED
     fi-equipto-fim AT ROW 3.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-grupo-ini AT ROW 4.33 COL 13 COLON-ALIGNED
     fi-grupo-fim AT ROW 4.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-modelo-ini AT ROW 5.33 COL 13 COLON-ALIGNED
     fi-modelo-fim AT ROW 5.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-estab-ini AT ROW 6.33 COL 13 COLON-ALIGNED HELP
          "C¢digo do estabelecimento."
     fi-estab-fim AT ROW 6.33 COL 39.14 COLON-ALIGNED HELP
          "C¢digo do estabelecimento." NO-LABEL
     fi-grp-evento-ini AT ROW 7.33 COL 13 COLON-ALIGNED
     fi-grp-evento-fim AT ROW 7.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-evento-ini AT ROW 8.33 COL 13 COLON-ALIGNED
     fi-evento-fim AT ROW 8.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-ccusto-ini AT ROW 9.33 COL 13 COLON-ALIGNED
     fi-ccusto-fim AT ROW 9.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-sistema-ini AT ROW 10.33 COL 13 COLON-ALIGNED
     fi-sistema-fim AT ROW 10.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fi-sub-sist-ini AT ROW 11.33 COL 13 COLON-ALIGNED HELP
          "C¢digo da Atividade"
     fi-sub-sist-fim AT ROW 11.33 COL 39.14 COLON-ALIGNED HELP
          "C¢digo da Atividade" NO-LABEL
     fi-tag-ini AT ROW 12.33 COL 13 COLON-ALIGNED
     fi-tag-fim AT ROW 12.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fiCausaini AT ROW 13.33 COL 13 COLON-ALIGNED
     fiCausaFim AT ROW 13.33 COL 39.14 COLON-ALIGNED NO-LABEL
     fiSintomaIni AT ROW 14.33 COL 13 COLON-ALIGNED
     fiSintomaFim AT ROW 14.33 COL 39.14 COLON-ALIGNED NO-LABEL
     btOK AT ROW 16.5 COL 2
     btCancel AT ROW 16.5 COL 13
     btHelp2 AT ROW 16.5 COL 53
     IMAGE-10 AT ROW 4.33 COL 37.43
     IMAGE-11 AT ROW 5.33 COL 37.43
     IMAGE-12 AT ROW 5.33 COL 31.43
     IMAGE-13 AT ROW 7.33 COL 37.43
     IMAGE-14 AT ROW 7.33 COL 31.43
     IMAGE-15 AT ROW 8.33 COL 31.43
     IMAGE-16 AT ROW 8.33 COL 37.43
     IMAGE-19 AT ROW 9.33 COL 37.43
     IMAGE-20 AT ROW 9.33 COL 31.43
     IMAGE-21 AT ROW 6.33 COL 37.43
     IMAGE-22 AT ROW 6.33 COL 31.43
     IMAGE-23 AT ROW 10.33 COL 37.43
     IMAGE-24 AT ROW 10.33 COL 31.43
     IMAGE-25 AT ROW 11.33 COL 37.43
     IMAGE-26 AT ROW 11.33 COL 31.43
     IMAGE-29 AT ROW 1.33 COL 37.72
     IMAGE-30 AT ROW 1.33 COL 31.72
     IMAGE-5 AT ROW 2.33 COL 31.43
     IMAGE-51 AT ROW 12.33 COL 31.43
     IMAGE-52 AT ROW 12.33 COL 37.43
     IMAGE-6 AT ROW 2.33 COL 37.43
     IMAGE-7 AT ROW 3.33 COL 31.43
     IMAGE-8 AT ROW 3.33 COL 37.43
     IMAGE-9 AT ROW 4.33 COL 31.43
     RECT-23 AT ROW 1 COL 1
     rtToolBar AT ROW 16.25 COL 1
     IMAGE-57 AT ROW 13.33 COL 31.43
     IMAGE-58 AT ROW 14.33 COL 31.43
     IMAGE-55 AT ROW 13.33 COL 37.43
     IMAGE-59 AT ROW 14.33 COL 37.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.72 BY 16.79
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 16.79
         WIDTH              = 63
         MAX-HEIGHT         = 16.79
         MAX-WIDTH          = 65.29
         VIRTUAL-HEIGHT     = 16.79
         VIRTUAL-WIDTH      = 65.29
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


ASSIGN     fi-periodo-ini     = TODAY - 30
           fi-periodo-fim     = today.


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
do with frame fPage0:
    find first ttSelecao no-lock no-error.
    if avail ttSelecao then do:
        assign fi-periodo-ini:screen-value      = string(ttSelecao.periodo-ini)
               fi-periodo-fim:screen-value      = string(ttSelecao.periodo-fim)
               fi-empresa-ini:screen-value      = string(ttSelecao.empresa-ini)
               fi-empresa-fim:screen-value      = string(ttSelecao.empresa-fim)
               fi-equipto-ini:screen-value      = ttSelecao.equipto-ini    
               fi-equipto-fim:screen-value      = ttSelecao.equipto-fim    
               fi-grupo-ini:screen-value        = ttSelecao.grupo-ini      
               fi-grupo-fim:screen-value        = ttSelecao.grupo-fim      
               fi-modelo-ini:screen-value       = ttSelecao.modelo-ini     
               fi-modelo-fim:screen-value       = ttSelecao.modelo-fim     
               fi-estab-ini:screen-value        = ttSelecao.estab-ini      
               fi-estab-fim:screen-value        = ttSelecao.estab-fim      
               fi-grp-evento-ini:screen-value   = ttSelecao.grp-evento-ini        
               fi-grp-evento-fim:screen-value   = ttSelecao.grp-evento-fim        
               fi-evento-ini:screen-value       = ttSelecao.evento-ini     
               fi-evento-fim:screen-value       = ttSelecao.evento-fim     
               fi-ccusto-ini:screen-value       = ttSelecao.ccusto-ini     
               fi-ccusto-fim:screen-value       = ttSelecao.ccusto-fim     
               fi-sistema-ini:screen-value      = ttSelecao.sistema-ini
               fi-sistema-fim:screen-value      = ttSelecao.sistema-fim
               fi-sub-sist-ini:screen-value     = ttSelecao.sub-sist-ini  
               fi-sub-sist-fim:screen-value     = ttSelecao.sub-sist-fim
               fi-tag-ini:screen-value          = ttSelecao.tag-ini
               fi-tag-fim:screen-value          = ttSelecao.tag-fim
               fiSintomaIni:screen-value        = ttSelecao.sintoma-ini 
               fiSintomaFim:screen-value        = ttSelecao.sintoma-fim 
               fiCausaIni:screen-value          = ttSelecao.causa-ini
               fiCausaFim:screen-value          = ttSelecao.causa-fim
            
            .
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
    assign ttSelecao.periodo-ini        =  date(fi-periodo-ini:screen-value in frame fPage0)    
           ttSelecao.periodo-fim        =  date(fi-periodo-fim:screen-value in frame fPage0)
           ttSelecao.empresa-ini        =  fi-empresa-ini:screen-value in frame fPage0
           ttSelecao.empresa-fim        =  fi-empresa-fim:screen-value in frame fPage0
           ttSelecao.equipto-ini        =  fi-equipto-ini:screen-value in frame fPage0    
           ttSelecao.equipto-fim        =  fi-equipto-fim:screen-value in frame fPage0    
           ttSelecao.grupo-ini          =  fi-grupo-ini:screen-value in frame fPage0      
           ttSelecao.grupo-fim          =  fi-grupo-fim:screen-value in frame fPage0      
           ttSelecao.modelo-ini         =  fi-modelo-ini:screen-value in frame fPage0     
           ttSelecao.modelo-fim         =  fi-modelo-fim:screen-value in frame fPage0     
           ttSelecao.estab-ini          =  fi-estab-ini:screen-value in frame fPage0      
           ttSelecao.estab-fim          =  fi-estab-fim:screen-value in frame fPage0      
           ttSelecao.grp-evento-ini     =  fi-grp-evento-ini:screen-value in frame fPage0        
           ttSelecao.grp-evento-fim     =  fi-grp-evento-fim:screen-value in frame fPage0        
           ttSelecao.evento-ini         =  fi-evento-ini:screen-value in frame fPage0     
           ttSelecao.evento-fim         =  fi-evento-fim:screen-value in frame fPage0     
           ttSelecao.ccusto-ini         =  fi-ccusto-ini:screen-value in frame fPage0     
           ttSelecao.ccusto-fim         =  fi-ccusto-fim:screen-value in frame fPage0     
           ttSelecao.sistema-ini        =  fi-sistema-ini:screen-value in frame fPage0        
           ttSelecao.sistema-fim        =  fi-sistema-fim:screen-value in frame fPage0        
           ttSelecao.sub-sist-ini       =  fi-sub-sist-ini:screen-value in frame fPage0
           ttSelecao.sub-sist-fim       =  fi-sub-sist-fim:screen-value in frame fPage0
           ttSelecao.tag-ini            =  fi-tag-ini:screen-value in frame fPage0
           ttSelecao.tag-fim            =  fi-tag-fim:screen-value in frame fPage0
           ttSelecao.causa-ini          =  fiCausaIni:screen-value in frame fpage0 
           ttSelecao.causa-fim          =  fiCausaFim:screen-value in frame fpage0 
           ttSelecao.sintoma-ini        =  fiSintomaIni:screen-value in frame fpage0
           ttSelecao.sintoma-fim        =  fiSintomaFim:screen-value in frame fpage0
        
        
        .
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

