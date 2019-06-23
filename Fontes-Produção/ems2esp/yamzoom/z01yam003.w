&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wZoom

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-especialid-func2 NO-UNDO LIKE mmv-especialid-func
       field r-rowid as rowid.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wZoom 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i Z01YAM003 2.00.00.000}  /*** 010009 ***/
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
&GLOBAL-DEFINE Program      Z01YAM003
&GLOBAL-DEFINE Version      2.00.00.000

&GLOBAL-DEFINE Folder       YES
&GLOBAL-DEFINE InitialPage  1

&GLOBAL-DEFINE FolderLabels Especial,Descri��o

&GLOBAL-DEFINE Range        No
&GLOBAL-DEFINE FieldNames   mgesp.mmv-especialid-func.cod-especialid, mgmnt.mmv-especialid-func.descricao

&GLOBAL-DEFINE ttTable1     tt-mmv-especialid-func
&GLOBAL-DEFINE hDBOTable1   hDBOEspecialidFunc
&GLOBAL-DEFINE DBOTable1    mmv-especialid-func

&GLOBAL-DEFINE ttTable2     tt-mmv-especialid-func2
&GLOBAL-DEFINE hDBOTable2   hDBOEspecialidFunc2
&GLOBAL-DEFINE DBOTable2    mmv-especialid-func

&GLOBAL-DEFINE page1Browse  brTable1    
&GLOBAL-DEFINE page2Browse  brTable2    

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable1} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTable2} AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Zoom
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mmv-especialid-func ~
tt-mmv-especialid-func2

/* Definitions for BROWSE brTable1                                      */
&Scoped-define FIELDS-IN-QUERY-brTable1 ~
tt-mmv-especialid-func.cod-especialid tt-mmv-especialid-func.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1 
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-mmv-especialid-func NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY brTable1 FOR EACH tt-mmv-especialid-func NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable1 tt-mmv-especialid-func
&Scoped-define FIRST-TABLE-IN-QUERY-brTable1 tt-mmv-especialid-func


/* Definitions for BROWSE brTable2                                      */
&Scoped-define FIELDS-IN-QUERY-brTable2 tt-mmv-especialid-func2.descricao ~
tt-mmv-especialid-func2.cod-especialid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable2 
&Scoped-define QUERY-STRING-brTable2 FOR EACH tt-mmv-especialid-func2 NO-LOCK ~
    BY tt-mmv-especialid-func2.descricao INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable2 OPEN QUERY brTable2 FOR EACH tt-mmv-especialid-func2 NO-LOCK ~
    BY tt-mmv-especialid-func2.descricao INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable2 tt-mmv-especialid-func2
&Scoped-define FIRST-TABLE-IN-QUERY-brTable2 tt-mmv-especialid-func2


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brTable1}

/* Definitions for FRAME fPage2                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage2 ~
    ~{&OPEN-QUERY-brTable2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wZoom AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btCheck1 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "" 
     SIZE 5.14 BY 1.

DEFINE BUTTON btImplant1 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-codigo-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-codigo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Especialidade":R22 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE BUTTON btCheck2 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "" 
     SIZE 5.14 BY 1.

DEFINE BUTTON btImplant2 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-descricao-fim AS CHARACTER FORMAT "x(30)" INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE c-descricao-ini AS CHARACTER FORMAT "x(30)" 
     LABEL "Descri��o":R11 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTable1 FOR 
      tt-mmv-especialid-func SCROLLING.

DEFINE QUERY brTable2 FOR 
      tt-mmv-especialid-func2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 wZoom _STRUCTURED
  QUERY brTable1 NO-LOCK DISPLAY
      tt-mmv-especialid-func.cod-especialid FORMAT "X(12)":U
      tt-mmv-especialid-func.descricao FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 10.5
         FONT 2.

DEFINE BROWSE brTable2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable2 wZoom _STRUCTURED
  QUERY brTable2 NO-LOCK DISPLAY
      tt-mmv-especialid-func2.descricao FORMAT "X(40)":U
      tt-mmv-especialid-func2.cod-especialid FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 10.5
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.71 COL 2
     btCancel AT ROW 16.71 COL 13
     btHelp AT ROW 16.71 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.98
         FONT 1.

DEFINE FRAME fPage2
     c-descricao-ini AT ROW 1.21 COL 9.29 COLON-ALIGNED
     c-descricao-fim AT ROW 1.21 COL 45.72 COLON-ALIGNED NO-LABEL
     btCheck2 AT ROW 1.21 COL 78.57
     brTable2 AT ROW 2.33 COL 2
     btImplant2 AT ROW 12.83 COL 2
     IMAGE-3 AT ROW 1.21 COL 40.14
     IMAGE-4 AT ROW 1.21 COL 44.57
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.45
         SIZE 84.43 BY 13.13
         FONT 1.

DEFINE FRAME fPage1
     btCheck1 AT ROW 1.08 COL 78.29
     c-codigo-ini AT ROW 1.17 COL 18.86 COLON-ALIGNED
     c-codigo-fim AT ROW 1.21 COL 49.29 COLON-ALIGNED NO-LABEL
     brTable1 AT ROW 2.33 COL 2
     btImplant1 AT ROW 12.83 COL 2
     IMAGE-1 AT ROW 1.17 COL 30.57
     IMAGE-2 AT ROW 1.17 COL 47.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 2.45
         SIZE 84.43 BY 13.13
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Zoom
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-mmv-especialid-func T "?" NO-UNDO mgesp mmv-especialid-func
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-especialid-func2 T "?" NO-UNDO mgesp mmv-especialid-func
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wZoom ASSIGN
         HIDDEN             = YES
         TITLE              = "Pesquisa Especialidade"
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wZoom 
/* ************************* Included-Libraries *********************** */

{zoom/zoom.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wZoom
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable1 c-codigo-fim fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* BROWSE-TAB brTable2 btCheck2 fPage2 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wZoom)
THEN wZoom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable1
/* Query rebuild information for BROWSE brTable1
     _TblList          = "Temp-Tables.tt-mmv-especialid-func"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tt-mmv-especialid-func.cod-especialid
     _FldNameList[2]   = Temp-Tables.tt-mmv-especialid-func.descricao
     _Query            is OPENED
*/  /* BROWSE brTable1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable2
/* Query rebuild information for BROWSE brTable2
     _TblList          = "Temp-Tables.tt-mmv-especialid-func2"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-mmv-especialid-func2.descricao|yes"
     _FldNameList[1]   = Temp-Tables.tt-mmv-especialid-func2.descricao
     _FldNameList[2]   = Temp-Tables.tt-mmv-especialid-func2.cod-especialid
     _Query            is OPENED
*/  /* BROWSE brTable2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON END-ERROR OF wZoom /* Pesquisa Especialidade */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON WINDOW-CLOSE OF wZoom /* Pesquisa Especialidade */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wZoom
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btCheck1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCheck1 wZoom
ON CHOOSE OF btCheck1 IN FRAME fPage1
DO:
    RUN setConstraints IN THIS-PROCEDURE (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btCheck2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCheck2 wZoom
ON CHOOSE OF btCheck2 IN FRAME fPage2
DO:
  RUN setConstraints IN THIS-PROCEDURE (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wZoom
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btImplant1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant1 wZoom
ON CHOOSE OF btImplant1 IN FRAME fPage1 /* Implantar */
DO:
    {zoom/Implant.i &ProgramImplant="mip/mi0103"
                    &PageNumber="1"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btImplant2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant2 wZoom
ON CHOOSE OF btImplant2 IN FRAME fPage2 /* Implantar */
DO:
    {zoom/Implant.i &ProgramImplant="mip/mi0103"
                    &PageNumber="2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wZoom
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN returnValues IN THIS-PROCEDURE.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME c-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-codigo-ini wZoom
ON ANY-KEY OF c-codigo-ini IN FRAME fPage1 /* Especialidade */
DO:
      {Zoom\AnyKey.i &Variavel=c-codigo-ini &pageNumber=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME c-descricao-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-descricao-ini wZoom
ON ANY-KEY OF c-descricao-ini IN FRAME fPage2 /* Descri��o */
DO:
       {Zoom\AnyKey.i &Variavel=c-descricao-ini &pageNumber=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wZoom 


/* ***************************  Main Block  *************************** */

/*--- L�gica para inicializa��o do programam ---*/
{Zoom/MainBlock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterEnableFields wZoom 
PROCEDURE afterEnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
enable c-codigo-ini
       c-codigo-fim
       btCheck1 with frame fPage1.
       
enable c-descricao-ini
       c-descricao-fim
       btCheck2 with frame fPage2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wZoom 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
enable c-codigo-ini
       c-codigo-fim
       btCheck1 with frame fPage1.
       
enable c-descricao-ini
       c-descricao-fim
       btCheck2 with frame fPage2.

assign c-codigo-fim:screen-value    in frame fPage1 = "ZZZZZZZZ"
       c-descricao-fim:screen-value in frame fPage2 = "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ".

APPLY "ENTRY" TO c-codigo-ini IN FRAME fpage1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wZoom 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Verifica se o DBO j� est� inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable1}) THEN DO:
        {btb/btb008za.i1 yambo/ydm004.p}
        {btb/btb008za.i2 yambo/ydm004.p '' {&hDBOTable1}} 
    END.
    
    RUN setConstraintEspecialidade IN {&hDBOTable1} (input "",
                                                     input "ZZZZZZZZ").
    
    /*--- Verifica se o DBO j� est� inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable2}) THEN DO:
        {btb/btb008za.i1 yambo/ydm004.p}
        {btb/btb008za.i2 yambo/ydm004.p '' {&hDBOTable2}} 
    END.
    
    RUN setConstraintDescricao IN {&hDBOTable2} (input "",
                                                  input "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ").
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries wZoom 
PROCEDURE openQueries :
/*------------------------------------------------------------------------------
  Purpose:     Atualiza browsers
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    {zoom/OpenQueries.i &Query="Especialidade"
                        &PageNumber="1"}
    
    {zoom/OpenQueries.i &Query="Descricao"
                        &PageNumber="2"}
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage1 wZoom 
PROCEDURE returnFieldsPage1 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p�gina 1
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.
    
    IF AVAILABLE {&ttTable1} THEN DO:
        CASE pcField:
            WHEN "cod-especialid":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.cod-especialid).
            WHEN "descricao":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.descricao).      
        END CASE.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage2 wZoom 
PROCEDURE returnFieldsPage2 :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p�gina 2
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.
    
    IF AVAILABLE {&ttTable2} THEN DO:
        CASE pcField:
            WHEN "cod-especialid":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.cod-especialid).
            WHEN "descricao":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.descricao).
        END CASE.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConstraints wZoom 
PROCEDURE setConstraints :
/*------------------------------------------------------------------------------
  Purpose:     Seta constraints e atualiza o browse, conforme n�mero da p�gina
               passado como par�metro
  Parameters:  recebe n�mero da p�gina
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta constraints conforme n�mero da p�gina ---*/
    CASE pPageNumber:
        WHEN 1 THEN
            /*--- Seta Constraints para o DBO Table1 ---*/
            RUN setConstraintEspecialidade IN {&hDBOTable1} (INPUT input frame fPage1 c-codigo-ini,
                                                             INPUT input frame fPage1 c-codigo-fim).
        
        WHEN 2 THEN
            /*--- Seta Constraints para o DBO Table2 ---*/
            RUN setConstraintDescricao IN {&hDBOTable2} (INPUT input frame fPage2 c-descricao-ini,
                                                         INPUT input frame fPage2 c-descricao-fim).
    END CASE.
    
    /*--- Seta vari�vel iConstraintPageNumber com o n�mero da p�gina atual 
          Esta vari�vel � utilizada no m�todo openQueries ---*/
    ASSIGN iConstraintPageNumber = pPageNumber.
    
    /*--- Atualiza browse ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

