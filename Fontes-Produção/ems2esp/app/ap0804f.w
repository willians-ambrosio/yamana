&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME ap0804f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ap0804f 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804F 2.00.00.044}  /*** 010044 ***/


/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters: 
      <none>

  Output Parameters: 
      <none>

  Version: 1.00.00

  History: 

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
&glob version 1.00.00

def buffer b-tit-ap for tit-ap.

def new global shared var gr-emitente   as rowid   no-undo.
def new global shared var rw-tit        as rowid   no-undo.
def new global shared var i-emp-selecao as char    no-undo.

def var rw-emitente   as rowid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-empresa bt-pendente 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR ap0804f AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V  para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi_Empresa     LABEL "&Empresa"       ACCELERATOR "CTRL-E"
       MENU-ITEM mi-pendente    LABEL "Relacionamentos &Pendentes" ACCELERATOR "CTRL-R"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_ap0804f-b07 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_ap0804f-b08 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b01ad133 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b03ad293 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b04ad180 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b05ad180 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b26ad180 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b32ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q15ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v08ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v10ad180 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v10ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v16ad260 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v21ad260 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-empresa 
     IMAGE-UP FILE "image~\emsinm":U
     LABEL "Empresa" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-pendente 
     IMAGE-UP FILE "image~\im-notah.bmp":U
     LABEL "Relacionamentos Pendentes" 
     SIZE 4 BY 1.25 TOOLTIP "Relacionamentos Pendentes".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 88.14 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-empresa AT ROW 1.33 COL 26.29 HELP
          "Seleciona a empresa dos t¡tulos"
     bt-pendente AT ROW 1.33 COL 35 HELP
          "Consulta dos relacionamentos pendentes"
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW ap0804f ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Detalhes Documento"
         HEIGHT             = 16.83
         WIDTH              = 90.29
         MAX-HEIGHT         = 22.38
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.38
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB ap0804f 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-concom.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW ap0804f
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
                                                                        */
ASSIGN 
       FRAME f-cad:RESIZABLE        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ap0804f)
THEN ap0804f:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ap0804f
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804f ap0804f
ON END-ERROR OF ap0804f /* Consulta Detalhes Documento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap0804f ap0804f
ON WINDOW-CLOSE OF ap0804f /* Consulta Detalhes Documento */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-empresa ap0804f
ON CHOOSE OF bt-empresa IN FRAME f-cad /* Empresa */
DO:

       run cdp/cd0726.w (input-output i-emp-selecao).

       find first b-tit-ap no-lock
            where b-tit-ap.ep-codigo = i-emp-selecao no-error.

       if  not avail b-tit-ap then do:
           run utp/ut-msgs.p (input "show",
                              input 8499,
                              input string(i-emp-selecao)).
       end.
       else do:
           run pi-reposiciona in h_q15ad260 (input rowid(b-tit-ap)).
       end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pendente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pendente ap0804f
ON CHOOSE OF bt-pendente IN FRAME f-cad /* Relacionamentos Pendentes */
DO:

    run pi-retorna-docto in h_v21ad260 (output rw-tit).
    
    if  rw-tit <> ? then
        run app/ap0804fe.w.
    else
        run utp/ut-msgs.p (input "show",
                            input 7146,
                            input "").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior ap0804f
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo ap0804f
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas ap0804f
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo ap0804f
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir ap0804f
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pendente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pendente ap0804f
ON CHOOSE OF MENU-ITEM mi-pendente /* Relacionamentos Pendentes */
DO:
  APPLY "choose" TO bt-pendente IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa ap0804f
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro ap0804f
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo ap0804f
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair ap0804f
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre ap0804f
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo ap0804f
ON CHOOSE OF MENU-ITEM mi-ultimo /* éltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para ap0804f
ON CHOOSE OF MENU-ITEM mi-va-para /* V  para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi_Empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi_Empresa ap0804f
ON CHOOSE OF MENU-ITEM mi_Empresa /* Empresa */
DO:
  APPLY "CHOOSE" TO bt-empresa IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ap0804f 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects ap0804f  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 73.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v21ad260 ).
       RUN set-position IN h_v21ad260 ( 2.79 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.08 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Det.|Movto|Hist.|FASB|Banco|Retenc|CMCAC|ND/NC|Impto' + ',
                     FOLDER-TAB-TYPE = 3':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 5.92 , 1.14 ) NO-ERROR.
       RUN set-size IN h_folder ( 11.75 , 89.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adqry/q15ad260.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Key-Name = ,
                     ProgPesquisa = adzoom/z15ad260.w,
                     ProgVaPara = adgo/g10ad260.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q15ad260 ).
       RUN set-position IN h_q15ad260 ( 1.29 , 54.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 8.29 ) */

       /* Links to SmartViewer h_v21ad260. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_v21ad260 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q15ad260. */
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q15ad260 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q15ad260 ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-v02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v08ad260 ).
       RUN set-position IN h_v08ad260 ( 7.17 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.08 , 87.57 ) */

       /* Links to SmartViewer h_v08ad260. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_v08ad260 ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b26ad180 ).
       RUN set-position IN h_b26ad180 ( 7.83 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 9.42 , 84.86 ) */

       /* Links to SmartBrowser h_b26ad180. */
       RUN add-link IN adm-broker-hdl ( h_v21ad260 , 'Record':U , h_b26ad180 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-v03.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v10ad260 ).
       RUN set-position IN h_v10ad260 ( 7.96 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.33 , 84.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b01ad133 ).
       RUN set-position IN h_b01ad133 ( 9.54 , 2.72 ) NO-ERROR.
       /* Size in UIB:  ( 7.67 , 85.43 ) */

       /* Links to SmartViewer h_v10ad260. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_v10ad260 ).

       /* Links to BrowserCadastro2 h_b01ad133. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_b01ad133 ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-v04.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v10ad180 ).
       RUN set-position IN h_v10ad180 ( 7.67 , 2.43 ) NO-ERROR.
       /* Size in UIB:  ( 2.17 , 86.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b03.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b04ad180 ).
       RUN set-position IN h_b04ad180 ( 10.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 6.67 , 87.43 ) */

       /* Links to SmartViewer h_v10ad180. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_v10ad180 ).

       /* Links to SmartBrowser h_b04ad180. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_b04ad180 ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-v05.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v16ad260 ).
       RUN set-position IN h_v16ad260 ( 7.83 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 9.25 , 87.29 ) */

       /* Links to SmartViewer h_v16ad260. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_v16ad260 ).

    END. /* Page 5 */

    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b04.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b32ad260 ).
       RUN set-position IN h_b32ad260 ( 7.21 , 3.29 ) NO-ERROR.
       /* Size in UIB:  ( 5.29 , 86.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b08.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_ap0804f-b08 ).
       RUN set-position IN h_ap0804f-b08 ( 12.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.08 , 86.00 ) */

       /* Links to SmartBrowser h_b32ad260. */
       RUN add-link IN adm-broker-hdl ( h_v21ad260 , 'Record':U , h_b32ad260 ).

       /* Links to BrowserCadastro2 h_ap0804f-b08. */
       RUN add-link IN adm-broker-hdl ( h_v21ad260 , 'Record':U , h_ap0804f-b08 ).

    END. /* Page 6 */

    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b05.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b05ad180 ).
       RUN set-position IN h_b05ad180 ( 8.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 8.67 , 86.86 ) */

       /* Links to SmartBrowser h_b05ad180. */
       RUN add-link IN adm-broker-hdl ( h_q15ad260 , 'Record':U , h_b05ad180 ).

    END. /* Page 7 */

    WHEN 8 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b07.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_ap0804f-b07 ).
       RUN set-position IN h_ap0804f-b07 ( 7.75 , 2.29 ) NO-ERROR.
       /* Size in UIB:  ( 9.42 , 86.43 ) */

       /* Links to SmartBrowser h_ap0804f-b07. */
       RUN add-link IN adm-broker-hdl ( h_v21ad260 , 'Record':U , h_ap0804f-b07 ).

    END. /* Page 8 */

    WHEN 9 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'app/ap0804f-b06.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b03ad293 ).
       RUN set-position IN h_b03ad293 ( 7.38 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.96 , 87.14 ) */

       /* Links to SmartBrowser h_b03ad293. */
       RUN add-link IN adm-broker-hdl ( h_v21ad260 , 'Record':U , h_b03ad293 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 9 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available ap0804f  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ap0804f  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ap0804f)
  THEN DELETE WIDGET ap0804f.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ap0804f  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-empresa bt-pendente 
      WITH FRAME f-cad IN WINDOW ap0804f.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW ap0804f.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy ap0804f 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit ap0804f 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize ap0804f 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  {include/win-size.i}
  run pi-before-initialize.

{utp/ut9000.i "AP0804F" "2.00.00.044"}

  assign i-emp-selecao = i-ep-codigo-usuario.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  if rw-tit <> ? then do:
     run pi-reposiciona-query in h_q15ad260 (input rw-tit).              
  end.   

  if  i-pais-impto-usuario = 1 then do:
      RUN init-pages ("9").
      RUN dispatch IN h_b03ad293 ('destroy':U).
      RUN delete-folder-page IN h_folder (INPUT 9).
  end.  
  else do: 
      RUN init-pages ("7").
      RUN dispatch IN h_b05ad180 ('destroy':U).
      RUN delete-folder-page IN h_folder (INPUT 7).
  end.

  run pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_ems_xref ap0804f 
PROCEDURE pi_ems_xref :
/*------------------------------------------------------------------------------
  Purpose:     Refer¼ncias XRef
  Parameters:  <none> 
  Notes:       N’o altere/elimine o c½digo abaixo.
------------------------------------------------------------------------------*/
RUN adzoom/z15ad260.w.
RUN adgo/g10ad260.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RTB_xref_generator ap0804f 
PROCEDURE RTB_xref_generator :
/* -----------------------------------------------------------
Purpose:    Generate RTB xrefs for SMARTOBJECTS.
Parameters: <none>
Notes:      This code is generated by the UIB.  DO NOT modify it.
            It is included for Roundtable Xref generation. Without
            it, Xrefs for SMARTOBJECTS could not be maintained by
            RTB.  It will in no way affect the operation of this
            program as it never gets executed.
-------------------------------------------------------------*/
  RUN "panel/p-navega.w *RTB-SmObj* ".
  RUN "panel/p-exihel.w *RTB-SmObj* ".
  RUN "adm/objects/folder.w *RTB-SmObj* ".
  RUN "adqry/q15ad260.w *RTB-SmObj* ".
  RUN "app/ap0804f-v02.w *RTB-SmObj* ".
  RUN "app/ap0804f-b01.w *RTB-SmObj* ".
  RUN "app/ap0804f-v03.w *RTB-SmObj* ".
  RUN "app/ap0804f-b02.w *RTB-SmObj* ".
  RUN "app/ap0804f-v04.w *RTB-SmObj* ".
  RUN "app/ap0804f-b03.w *RTB-SmObj* ".
  RUN "app/ap0804f-v05.w *RTB-SmObj* ".
  RUN "app/ap0804f-b04.w *RTB-SmObj* ".
  RUN "app/ap0804f-b08.w *RTB-SmObj* ".
  RUN "app/ap0804f-b05.w *RTB-SmObj* ".
  RUN "app/ap0804f-b07.w *RTB-SmObj* ".
  RUN "app/ap0804f-b06.w *RTB-SmObj* ".
  RUN "app/ap0804f-v01.w *RTB-SmObj* ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records ap0804f  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed ap0804f 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

