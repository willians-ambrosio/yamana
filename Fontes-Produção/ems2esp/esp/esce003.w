&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCE003 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


{cep\ceapi001.i}
{cdp\cd0666.i}



DEF VAR l-erro AS LOG.

DEF VAR c-linha   AS CHAR.
DEF VAR arq       AS CHAR FORMAT "x(45)" LABEL "Arquivo".
DEF VAR okpressed AS LOG.
DEF VAR procname  AS CHAR.
DEF VAR i         AS i.

DEF TEMP-TABLE tt-dev
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD cod-refer   LIKE movto-estoq.cod-refer
    FIELD cod-estabel LIKE saldo-estoq.cod-estabel
    FIELD cod-depos   LIKE saldo-estoq.cod-depos
    FIELD lote        LIKE movto-estoq.lote
    FIELD cod-localiz LIKE movto-estoq.cod-localiz
    FIELD tipo-trans  LIKE movto-estoq.tipo-trans
    FIELD un          LIKE movto-estoq.un
    FIELD nro-docto   LIKE movto-estoq.nro-docto
    FIELD cod-emitente LIKE movto-estoq.cod-emitente
    FIELD nat-oper     LIKE movto-estoq.nat-operacao
    FIELD conta-contab LIKE movto-estoq.conta-contab
    FIELD ct-codigo    LIKE movto-estoq.ct-codigo
    FIELD sc-codigo    LIKE movto-estoq.sc-codigo

    FIELD quantidade  LIKE movto-estoq.quantidade
    FIELD valor-mat   AS DEC EXTENT 3 
    FIELD valor-mob   AS DEC EXTENT 3
    FIELD valor-ggf   AS DEC EXTENT 3.

FIND FIRST param-estoq NO-LOCK NO-ERROR.

DEF TEMP-TABLE tt-erro-final LIKE tt-erro.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-2 RECT-3 RECT-4 c-serie ~
d-data i-executar c-descricao bt-executar 
&Scoped-Define DISPLAYED-OBJECTS c-serie d-data i-executar c-descricao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat½rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE c-descricao AS CHARACTER FORMAT "X(40)":U 
     LABEL "Descri‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(5)":U INITIAL "ACT" 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE d-data AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-executar AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Listar", 1,
"Zerar Saldos", 2
     SIZE 12 BY 1.5
     FGCOLOR 12  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.43 BY 4.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.43 BY 1.92.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 1.88.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     c-serie AT ROW 3.75 COL 18.43 COLON-ALIGNED WIDGET-ID 40
     d-data AT ROW 4.75 COL 18.43 COLON-ALIGNED WIDGET-ID 42
     i-executar AT ROW 5 COL 63 NO-LABEL WIDGET-ID 58
     c-descricao AT ROW 5.75 COL 18.43 COLON-ALIGNED WIDGET-ID 48
     bt-executar AT ROW 13.38 COL 20.29 WIDGET-ID 2
     "Transa‡Æo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3 COL 2 WIDGET-ID 52
          FGCOLOR 9 
     "Aten‡Æo ! Dever  ser informada a data do ultimo dia do mˆs e ap¢s descalcular o m‚dio" VIEW-AS TEXT
          SIZE 68 BY .54 AT ROW 9 COL 6 WIDGET-ID 82
     rt-button AT ROW 1 COL 1
     RECT-2 AT ROW 3.25 COL 1 WIDGET-ID 50
     RECT-3 AT ROW 13 COL 1 WIDGET-ID 56
     RECT-4 AT ROW 4.75 COL 61 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.21
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Gera Movimentacao de Estoque para Acerto de Valores"
         HEIGHT             = 15
         WIDTH              = 79.57
         MAX-HEIGHT         = 20.04
         MAX-WIDTH          = 97.57
         VIRTUAL-HEIGHT     = 20.04
         VIRTUAL-WIDTH      = 97.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Gera Movimentacao de Estoque para Acerto de Valores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Gera Movimentacao de Estoque para Acerto de Valores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Executar */
DO:
  RUN pi-validar.

  FOR EACH tt-dev: DELETE tt-dev. END.

  IF l-erro = NO THEN DO:
      
     RUN pi-montar-tt.
    
     IF INPUT FRAME {&FRAME-NAME} i-executar = 1 THEN DO: /* Listar */
         OUTPUT TO VALUE(session:TEMP-DIRECTORY + "td.txt").
         FOR EACH tt-dev:
             DISP tt-dev WITH WIDTH 300 STREAM-IO.
             DISP 
                  tt-dev.valor-mat[1] COLUMN-LABEL 'Vl.Mat Real' 
                  tt-dev.valor-mat[2] COLUMN-LABEL 'Vl.Mat Dolar'
                  tt-dev.valor-mat[3] COLUMN-LABEL 'Vl.Mat 3'

                 tt-dev.valor-mob[1] COLUMN-LABEL 'Vl.Mob Real' 
                 tt-dev.valor-mob[2] COLUMN-LABEL 'Vl.Mob Dolar'
                 tt-dev.valor-mob[3] COLUMN-LABEL 'Vl.Mob 3'

                 tt-dev.valor-ggf[1] COLUMN-LABEL 'Vl.GGF Real' 
                 tt-dev.valor-ggf[2] COLUMN-LABEL 'Vl.GGF Dolar'
                 tt-dev.valor-ggf[3] COLUMN-LABEL 'Vl.GGF 3'.

         END.
         OUTPUT CLOSE.
         DOS SILENT VALUE(session:TEMP-DIRECTORY + "td.txt").
     END. /* IF i-executar = 1 THEN DO: */

     ELSE DO:

         /*
         FIND FIRST tt-dev WHERE tt-dev.observacao <> '' NO-LOCK NO-ERROR.
         IF AVAIL tt-dev THEN 
             MESSAGE 'EXISTEM PROBLEMAS NA RELACAO DOS MOVIMENTOS!' SKIP
                     'Gere a lista dos movimentos e veja em Observacoes.'
                 VIEW-AS ALERT-BOX WARNING BUTTONS OK TITLE 'ATENCAO'.
           */
         MESSAGE 'Deseja realizar o acerto dos valores  ?'
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-zerar AS LOG.

         IF l-zerar THEN RUN pi-acerta-saldo.  

     END.
   
  END. /* IF l-erro = NO THEN DO: */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat½rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 63.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-serie:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY c-serie d-data i-executar c-descricao 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-2 RECT-3 RECT-4 c-serie d-data i-executar c-descricao 
         bt-executar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESCE003" "2.06.00.000"}

 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN d-data:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).


  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acerta-saldo w-livre 
PROCEDURE pi-acerta-saldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-erro-final. DELETE tt-erro-final. END.

FOR EACH tt-dev .

    FIND FIRST ITEM WHERE ITEM.it-codigo = tt-dev.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT.

    FOR EACH tt-movto. DELETE tt-movto. END.

    
    CREATE tt-MOVTO.   /* Tira do deposito do sujeito CD */
    assign tt-movto.cod-versao-integracao = 1
           tt-MOVTO.cod-depos             = tt-dev.cod-depos
           tt-MOVTO.cod-estabel           = tt-dev.cod-estabel
           tt-MOVTO.ct-codigo             = tt-dev.ct-codigo
           tt-MOVTO.sc-codigo             = tt-dev.sc-codigo
           tt-MOVTO.dt-trans              = INPUT FRAME {&FRAME-NAME} d-data
           tt-MOVTO.esp-docto             = 6 /* "DIV" */
           tt-MOVTO.it-codigo             = tt-dev.it-codigo
           tt-MOVTO.quantidade            = tt-dev.quantidade
           tt-MOVTO.serie-docto           = INPUT FRAME {&FRAME-NAME} c-serie
           tt-MOVTO.tipo-trans            = IF tt-dev.tipo-trans = 1 THEN 2 ELSE 1  /* True = + | False = - */
           tt-MOVTO.UN                    = tt-dev.un
           tt-MOVTO.nro-docto             = tt-dev.nro-docto
           tt-movto.cod-usu-ult-alter     = "SUPER"
           tt-movto.usuario               = "SUPER"
           tt-movto.conta-contabil        = tt-dev.conta-contab
/*           tt-movto.conta-db              = '9999999999'*/
           tt-movto.tipo-val              = 1
           tt-MOVTO.dt-vali-lote          = 12/31/9999
           
           tt-MOVTO.descricao             = INPUT FRAME {&FRAME-NAME} c-descricao
           tt-MOVTO.cod-refer             = tt-dev.cod-refer
           tt-MOVTO.lote                  = tt-dev.lote
           tt-MOVTO.cod-localiz           = tt-dev.cod-localiz

           tt-movto.valor-mat-m[1]        = tt-dev.valor-mat[1]
           tt-movto.valor-mat-m[2]        = tt-dev.valor-mat[2]
           tt-movto.valor-mat-m[3]        = tt-dev.valor-mat[3]
           
           tt-movto.valor-mob-m[1]        = tt-dev.valor-mob[1]
           tt-movto.valor-mob-m[2]        = tt-dev.valor-mob[2]
           tt-movto.valor-mob-m[3]        = tt-dev.valor-mob[3]

           tt-movto.valor-ggf-m[1]        = tt-dev.valor-ggf[1]
           tt-movto.valor-ggf-m[2]        = tt-dev.valor-ggf[2]
           tt-movto.valor-ggf-m[3]        = tt-dev.valor-ggf[3]

           tt-movto.cod-prog-orig         = 'ESCE003'.
                      
           run cep\ceapi001.r (input-output table tt-movto, input-output 
               table tt-erro, input yes).
               
           for each tt-erro:
              create tt-erro-final.
              buffer-copy tt-erro to tt-erro-final. 
              
              delete tt-erro.
           end.
END.


  FIND FIRST tt-erro-final NO-ERROR.
  IF   AVAIL tt-erro-final THEN DO:
      OUTPUT TO VALUE(session:TEMP-DIRECTORY + "erro.txt").
      for each tt-erro-final NO-LOCK:
         display tt-erro-final WITH width 320 STREAM-IO.
      end.
      OUTPUT CLOSE.
      DOS SILENT VALUE(session:TEMP-DIRECTORY + "erro.txt").
  END.

  MESSAGE 'Concluido.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-montar-tt w-livre 
PROCEDURE pi-montar-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

SYSTEM-DIALOG GET-FILE procname
        TITLE      "Escolha o arquivo"
        FILTERS    "Arquivos TXT" "*.txt", "Arquivos PRN" "*.prn", "Todos os Tipos (*.*)" "*.*"
        INITIAL-DIR SESSION:TEMP-DIRECTORY
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.

    IF OKpressed = TRUE THEN
    ASSIGN   arq = procname.

    IF OKpressed = NO THEN QUIT.

    MESSAGE "Deseja importar ?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO TITLE " "
            UPDATE l-importa AS LOG.

    IF l-importa = NO THEN QUIT.

    IF SEARCH(arq) = ? THEN QUIT.

    input from value(arq).
 
/*Inicio da Importa»’o*/ 

REPEAT:
      import unformatted c-linha.
      if SUBSTRING(c-linha,1,1) = " "   then next.
      

      CREATE tt-dev.
      ASSIGN tt-dev.it-codigo   = entry(1,c-linha,';')   
             tt-dev.cod-refer   = entry(2,c-linha,';')   
             tt-dev.cod-estabel = entry(3,c-linha,';')   
             tt-dev.cod-depos   = entry(4,c-linha,';')   
             tt-dev.lote        = entry(5,c-linha,';')   
             tt-dev.COD-localiz = entry(6,c-linha,';')   
             tt-dev.tipo-trans  = int(entry(7,c-linha,';'))
             tt-dev.un          =     ENTRY(8,c-linha,';')
             tt-dev.nro-docto   =     ENTRY(9,c-linha,';')
             tt-dev.cod-emit    = INT(ENTRY(10,c-linha,';'))
             tt-dev.nat-oper    =     ENTRY(11,c-linha,';')
             tt-dev.conta-contab = ENTRY(12,c-linha,';')
             tt-dev.ct-codigo   = substr(tt-dev.conta-contab,1,8)
             tt-dev.sc-codigo   = substr(tt-dev.conta-contab,11,6)

             tt-dev.quantidade  = int(entry(13,c-linha,';'))


             tt-dev.valor-mat[1] = DEC(ENTRY(14,c-linha,';'))
             tt-dev.valor-mat[2] = DEC(ENTRY(15,c-linha,';'))
             tt-dev.valor-mat[3] = DEC(ENTRY(16,c-linha,';'))
             tt-dev.valor-mob[1] = DEC(ENTRY(17,c-linha,';'))
             tt-dev.valor-mob[2] = DEC(ENTRY(18,c-linha,';'))
             tt-dev.valor-mob[3] = DEC(ENTRY(19,c-linha,';'))
             tt-dev.valor-ggf[1] = DEC(ENTRY(20,c-linha,';'))
             tt-dev.valor-ggf[2] = DEC(ENTRY(21,c-linha,';'))
             tt-dev.valor-ggf[3] = DEC(ENTRY(22,c-linha,';')).
END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validar w-livre 
PROCEDURE pi-validar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

l-erro = NO.
/* VALIDAR SELECAO ***********************************************************************/


/*
/* VALIDAR TRANSACAO ***********************************************************************/
IF INPUT FRAME {&FRAME-NAME} c-docto = '' THEN DO:
    MESSAGE 'Documento deve ser informado!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-docto IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.
  */
IF INPUT FRAME {&FRAME-NAME} c-serie = "" THEN
DO:
    MESSAGE 'Serie deve ser informada!'
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO c-serie IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.
IF param-estoq.mensal-ate >= INPUT FRAME {&FRAME-NAME} d-data THEN DO:
    MESSAGE 'Data informada menor que data do œltimo per­odo calculado!' SKIP
        'Medio calculado at² ' STRING(param-estoq.mensal-ate,'99/99/9999')
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY 'entry' TO d-data IN FRAME {&FRAME-NAME}.
    l-erro = YES.
    RETURN.
END.   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
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

