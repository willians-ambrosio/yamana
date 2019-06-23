&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
  Altera‡Æo: 20/05/2014 - L‚a Campos - 
             Find do emitente passa a ser pelo nome-abrev e nÆo mais
             pelo cgc (Devido ao cadastro de inativo com cnpj duplicado)
  
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
{dsc\ra\include\buffers.i}
{dsc\ra\include\variaveis-nfe-receb.i}


/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER p_rw_item                  AS ROWID                            NO-UNDO.
DEFINE INPUT  PARAMETER p_it_codigo                AS CHAR FORMAT "x(016)"             NO-UNDO.
DEFINE INPUT  PARAMETER p_natur_oper               AS CHAR FORMAT "x(008)"             NO-UNDO.
DEFINE INPUT  PARAMETER p_pre-unit-xml             AS DEC                              NO-UNDO.
DEFINE OUTPUT PARAMETER p_rw_doc_nota              AS ROWID                            NO-UNDO.  

/* Local Variable Definitions ---                                       */

DEF BUFFER bf-nfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
DEF BUFFER bf-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
     
DEFINE TEMP-TABLE tt-nfe-ref NO-UNDO
    field nr-nota-fis     like it-nota-fisc.nr-nota-fis
    field serie           like it-nota-fisc.serie       
    field nr-seq-fat      like it-nota-fisc.nr-seq-fat  
    field dt-emis-nota    like it-nota-fisc.dt-emis-nota
    FIELD cod-estabel     LIKE nota-fiscal.cod-estabel
    FIELD it-codigo       LIKE it-nota-fisc.it-codigo
    field qt-faturada     AS DEC
    field qt-devolvida    AS DEC
    field qt-saldo        AS DEC
    field vl-preuni       AS DEC
    field qt-vl-aprox     AS DEC.

DEF VAR i-soma AS DEC.
def var de-qtd-dev as dec.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-it-nota-fisc

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-nfe-ref

/* Definitions for BROWSE br-it-nota-fisc                               */
&Scoped-define FIELDS-IN-QUERY-br-it-nota-fisc tt-nfe-ref.nr-nota-fis tt-nfe-ref.serie tt-nfe-ref.it-codigo tt-nfe-ref.nr-seq-fat tt-nfe-ref.dt-emis-nota tt-nfe-ref.qt-faturada tt-nfe-ref.qt-devolvida tt-nfe-ref.qt-saldo tt-nfe-ref.vl-preuni   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-nota-fisc   
&Scoped-define SELF-NAME br-it-nota-fisc
&Scoped-define QUERY-STRING-br-it-nota-fisc for each tt-nfe-ref NO-LOCK            BY tt-nfe-ref.qt-vl-aprox DESCENDING            indexed-reposition
&Scoped-define OPEN-QUERY-br-it-nota-fisc OPEN QUERY {&SELF-NAME}        for each tt-nfe-ref NO-LOCK            BY tt-nfe-ref.qt-vl-aprox DESCENDING            indexed-reposition.
&Scoped-define TABLES-IN-QUERY-br-it-nota-fisc tt-nfe-ref
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-nota-fisc tt-nfe-ref


/* Definitions for DIALOG-BOX D-Dialog                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-tp-busca fi-c-it-codigo-ini ~
fi-c-it-codigo-fim fi-nro-docto-nota-ini fi-nro-docto-nota-fim ~
fi-serie-docto-nota-ini fi-serie-docto-nota-fim fi-seq-docto-nota-ini ~
fi-seq-docto-nota-fim bt-confirma br-it-nota-fisc btn-ok btn-cancelar ~
fi-d-periodo-ini fi-d-periodo-fim RECT-43 RECT-41 IMAGE-22 IMAGE-25 ~
IMAGE-26 IMAGE-27 RECT-34 IMAGE-18 RECT-36 IMAGE-36 
&Scoped-Define DISPLAYED-OBJECTS rs-tp-busca fi-c-cod-estabel ~
fi-c-nome-estab fi-i-cod-emitente fi-c-nome-emit fi-c-it-codigo-ini ~
fi-c-it-codigo-fim fi-nro-docto-nota-ini fi-nro-docto-nota-fim ~
fi-serie-docto-nota-ini fi-serie-docto-nota-fim fi-seq-docto-nota-ini ~
fi-seq-docto-nota-fim fi-d-periodo-ini fi-d-periodo-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON btn-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "dsc\ra\img\cancelar.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.13 TOOLTIP "Cancelar".

DEFINE BUTTON btn-ok AUTO-END-KEY 
     IMAGE-UP FILE "dsc\ra\img\salvar.bmp":U
     LABEL "Ok" 
     SIZE 10 BY 1.13 TOOLTIP "Confirmar".

DEFINE VARIABLE fi-c-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelec" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-nome-emit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 59.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-c-nome-estab AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 64.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-d-periodo-fim AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-d-periodo-ini AS DATE FORMAT "99/99/99":U 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-i-cod-emitente AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-nota-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-nota-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nr. Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq-docto-nota-fim AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-seq-docto-nota-ini AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Seq Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-nota-fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-nota-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Serie Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-18
     FILENAME "dsc\ra\img\dsc.bmp":U
     SIZE 4.86 BY 1.42.

DEFINE IMAGE IMAGE-22
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "dsc\ra\img\im-faixa.bmp":U
     SIZE 5.86 BY .88.

DEFINE VARIABLE rs-tp-busca AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Nota Referenciada", 1,
"Todas", 2
     SIZE 17 BY 1.5 NO-UNDO.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 85 BY 1.92
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 8.5.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 5.67.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-it-nota-fisc FOR 
      tt-nfe-ref SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-it-nota-fisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-it-nota-fisc D-Dialog _FREEFORM
  QUERY br-it-nota-fisc NO-LOCK DISPLAY
      tt-nfe-ref.nr-nota-fis              COLUMN-LABEL "Nota Fiscal"
      tt-nfe-ref.serie                    COLUMN-LABEL "Serie"
      tt-nfe-ref.it-codigo                COLUMN-LABEL "Item"
      tt-nfe-ref.nr-seq-fat               COLUMN-LABEL "Seq"
      tt-nfe-ref.dt-emis-nota             COLUMN-LABEL "EmissÆo"
      tt-nfe-ref.qt-faturada              COLUMN-LABEL "Quantidade"
      tt-nfe-ref.qt-devolvida             COLUMN-LABEL "Qtde Devolvida"
      tt-nfe-ref.qt-saldo                 COLUMN-LABEL "Saldo"
      tt-nfe-ref.vl-preuni                COLUMN-LABEL "Pr.Unit rio"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 83.43 BY 8.04
         FONT 1
         TITLE "Nota Referenciada".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-tp-busca AT ROW 5.75 COL 67 NO-LABEL WIDGET-ID 264
     fi-c-cod-estabel AT ROW 1.42 COL 10.57 COLON-ALIGNED WIDGET-ID 224
     fi-c-nome-estab AT ROW 1.42 COL 16.14 COLON-ALIGNED NO-LABEL WIDGET-ID 226
     fi-i-cod-emitente AT ROW 2.42 COL 11 COLON-ALIGNED WIDGET-ID 200
     fi-c-nome-emit AT ROW 2.42 COL 21.14 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     fi-c-it-codigo-ini AT ROW 5 COL 21.14 COLON-ALIGNED WIDGET-ID 254
     fi-c-it-codigo-fim AT ROW 5 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 252
     fi-nro-docto-nota-ini AT ROW 6 COL 21.14 COLON-ALIGNED WIDGET-ID 240
     fi-nro-docto-nota-fim AT ROW 6 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 244
     fi-serie-docto-nota-ini AT ROW 7 COL 31.86 COLON-ALIGNED WIDGET-ID 242
     fi-serie-docto-nota-fim AT ROW 7 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 246
     fi-seq-docto-nota-ini AT ROW 8 COL 31.86 COLON-ALIGNED WIDGET-ID 206
     fi-seq-docto-nota-fim AT ROW 8 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 248
     bt-confirma AT ROW 7.5 COL 56 WIDGET-ID 182
     br-it-nota-fisc AT ROW 9.88 COL 2.14 WIDGET-ID 300
     btn-ok AT ROW 18.21 COL 22.14 WIDGET-ID 152
     btn-cancelar AT ROW 18.21 COL 55.57 WIDGET-ID 150
     fi-d-periodo-ini AT ROW 4.08 COL 21.14 COLON-ALIGNED WIDGET-ID 258
     fi-d-periodo-fim AT ROW 4.08 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     "Tipo de Busca" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 4.75 COL 67 WIDGET-ID 262
          FGCOLOR 9 FONT 0
     RECT-43 AT ROW 1.21 COL 1.57 WIDGET-ID 218
     RECT-41 AT ROW 3.5 COL 1.57 WIDGET-ID 190
     IMAGE-22 AT ROW 7 COL 41 WIDGET-ID 216
     IMAGE-25 AT ROW 6 COL 41 WIDGET-ID 220
     IMAGE-26 AT ROW 8 COL 41 WIDGET-ID 228
     IMAGE-27 AT ROW 5 COL 41 WIDGET-ID 250
     RECT-34 AT ROW 17.75 COL 1.57 WIDGET-ID 156
     IMAGE-18 AT ROW 18 COL 80.86 WIDGET-ID 154
     RECT-36 AT ROW 9.21 COL 1.57 WIDGET-ID 158
     IMAGE-36 AT ROW 4.08 COL 41 WIDGET-ID 260
     SPACE(39.85) SKIP(15.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "esnfe200z19 - Pesquisa Documento/Nota Referenciada" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-it-nota-fisc bt-confirma D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-c-cod-estabel IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-nome-emit IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-c-nome-estab IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-i-cod-emitente IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-nota-fisc
/* Query rebuild information for BROWSE br-it-nota-fisc
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
       for each tt-nfe-ref NO-LOCK
           BY tt-nfe-ref.qt-vl-aprox DESCENDING
           indexed-reposition.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-it-nota-fisc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON MOUSE-SELECT-DBLCLICK OF FRAME D-Dialog /* esnfe200z19 - Pesquisa Documento/Nota Referenciada */
DO:
  APPLY "CHOOSE" TO btn-ok IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* esnfe200z19 - Pesquisa Documento/Nota Referenciada */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-it-nota-fisc
&Scoped-define SELF-NAME br-it-nota-fisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-it-nota-fisc D-Dialog
ON MOUSE-SELECT-DBLCLICK OF br-it-nota-fisc IN FRAME D-Dialog /* Nota Referenciada */
DO:
    APPLY "CHOOSE" TO btn-ok IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma D-Dialog
ON CHOOSE OF bt-confirma IN FRAME D-Dialog /* Button 1 */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        IF rs-tp-busca:SCREEN-VALUE = "1" THEN DO:
            FIND FIRST nfe-it-nota-fisc-rec 
                WHERE rowid(nfe-it-nota-fisc-rec) =  p_rw_item NO-LOCK NO-ERROR.
            IF AVAIL nfe-it-nota-fisc-rec THEN DO:
                FOR EACH nfe-ref-nota-fisc-rec
                    WHERE nfe-ref-nota-fisc-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe NO-LOCK:
                    FIND FIRST nota-fiscal
                        WHERE nota-fiscal.nr-nota-fis = SUBSTR(nfe-ref-nota-fisc-rec.nfe-ref,28,7)
                          AND nota-fiscal.serie       = STRING(INT(SUBSTR(nfe-ref-nota-fisc-rec.nfe-ref,23,3)))
                          AND nota-fiscal.cod-estabel = INPUT fi-c-cod-estabel NO-LOCK NO-ERROR.
                    
                    IF AVAIL nota-fiscal THEN DO:
                        for each it-nota-fisc NO-LOCK 
                            WHERE it-nota-fisc.cod-estabel  = nota-fiscal.cod-estabel
                              AND it-nota-fisc.serie        = nota-fiscal.serie
                              AND it-nota-fisc.nr-nota-fis  = nota-fiscal.nr-nota-fis
                              AND it-nota-fisc.nr-seq-fat  >= int(fi-seq-docto-nota-ini)
                              AND it-nota-fisc.nr-seq-fat  <= int(fi-seq-docto-nota-fim)
                              AND it-nota-fisc.it-codigo   >= INPUT fi-c-it-codigo-ini
                              AND it-nota-fisc.it-codigo   <= INPUT fi-c-it-codigo-fim:
                            IF (it-nota-fisc.qt-faturada[1] < it-nota-fisc.qt-devolvida[1]) THEN NEXT.
                            
                            CREATE tt-nfe-ref.
                            ASSIGN tt-nfe-ref.nr-nota-fis   = it-nota-fisc.nr-nota-fis
                                   tt-nfe-ref.serie         = it-nota-fisc.serie
                                   tt-nfe-ref.nr-seq-fat    = it-nota-fisc.nr-seq-fat   
                                   tt-nfe-ref.it-codigo     = it-nota-fisc.it-codigo
                                   tt-nfe-ref.cod-estabel   = nota-fiscal.cod-estabel
                                   tt-nfe-ref.dt-emis-nota  = it-nota-fisc.dt-emis-nota 
                                   tt-nfe-ref.qt-faturada   = it-nota-fisc.qt-faturada[1]  
                                   tt-nfe-ref.qt-devolvida  = it-nota-fisc.qt-devolvida[1] 
                                   tt-nfe-ref.qt-saldo      = (it-nota-fisc.qt-faturada[1] - it-nota-fisc.qt-devolvida[1])     
                                   tt-nfe-ref.vl-preuni     = it-nota-fisc.vl-preuni    
                                   tt-nfe-ref.qt-vl-aprox   = dec(p_pre-unit-xml) - dec(it-nota-fisc.vl-preuni).
                            IF tt-nfe-ref.qt-vl-aprox < 0 THEN 
                                ASSIGN tt-nfe-ref.qt-vl-aprox = tt-nfe-ref.qt-vl-aprox * -1.  /*transformar numero negativo em positivo*/
                        END.
                    END.
                END. 
            END.
        END.
        ELSE DO:
            FOR EACH nota-fiscal NO-LOCK
                WHERE nota-fiscal.cod-estabel   = INPUT fi-c-cod-estabel
                  AND nota-fiscal.serie        >= fi-serie-docto-nota-ini
                  AND nota-fiscal.serie        <= fi-serie-docto-nota-fim
                  and nota-fiscal.nr-nota-fis  >= INPUT fi-nro-docto-nota-ini
                  and nota-fiscal.nr-nota-fis  <= INPUT fi-nro-docto-nota-fim
                  AND nota-fiscal.nome-ab-cli   = emitente.nome-abrev
                  AND nota-fiscal.dt-emis-nota >= INPUT fi-d-periodo-ini
                  AND nota-fiscal.dt-emis-nota <= INPUT fi-d-periodo-fim,
                EACH it-nota-fisc OF nota-fiscal NO-LOCK
                WHERE it-nota-fisc.it-codigo   >= INPUT fi-c-it-codigo-ini
                  and it-nota-fisc.it-codigo   <= INPUT fi-c-it-codigo-fim:

                ASSIGN i-soma     = 0
                       de-qtd-dev = 0.
                
                FOR EACH nfe-relac-doc-nota-rec
                    WHERE nfe-relac-doc-nota-rec.nr-docto-nota = nota-fiscal.nr-nota-fis
                     AND nfe-relac-doc-nota-rec.it-codigo      = it-nota-fisc.it-codigo NO-LOCK:
                
                    ASSIGN i-soma = i-soma + nfe-relac-doc-nota-rec.quantidade.
                END.
                
                for each devol-cli use-index ch-nfs
                    where devol-cli.cod-estabel  = it-nota-fisc.cod-estabel 
                      and devol-cli.serie        = it-nota-fisc.serie 
                      and devol-cli.nr-nota-fis  = it-nota-fisc.nr-nota-fis 
                      and devol-cli.nr-sequencia = it-nota-fisc.nr-seq-fat 
                      and devol-cli.it-codigo    = it-nota-fisc.it-codigo no-lock:
                    assign de-qtd-dev = de-qtd-dev + devol-cli.qt-devolvida.
                end.  

                IF (it-nota-fisc.qt-faturada[1] - (it-nota-fisc.qt-devolvida[1] + i-soma + de-qtd-dev)) < it-nota-fisc.qt-faturada[1] /*se saldo for menor q qtd faturada*/ THEN NEXT.

                CREATE tt-nfe-ref.
                ASSIGN tt-nfe-ref.nr-nota-fis   = it-nota-fisc.nr-nota-fis
                       tt-nfe-ref.serie         = it-nota-fisc.serie
                       tt-nfe-ref.nr-seq-fat    = it-nota-fisc.nr-seq-fat
                       tt-nfe-ref.it-codigo     = it-nota-fisc.it-codigo
                       tt-nfe-ref.cod-estabel   = nota-fiscal.cod-estabel
                       tt-nfe-ref.dt-emis-nota  = it-nota-fisc.dt-emis-nota 
                       tt-nfe-ref.qt-faturada   = it-nota-fisc.qt-faturada[1]  
                       tt-nfe-ref.qt-devolvida  = it-nota-fisc.qt-devolvida[1] + i-soma + de-qtd-dev  
                       tt-nfe-ref.qt-saldo      = (it-nota-fisc.qt-faturada[1] - (it-nota-fisc.qt-devolvida[1] + i-soma + de-qtd-dev))     
                       tt-nfe-ref.vl-preuni     = it-nota-fisc.vl-preuni    
                       tt-nfe-ref.qt-vl-aprox   = dec(p_pre-unit-xml) - dec(it-nota-fisc.vl-preuni).
                IF tt-nfe-ref.qt-vl-aprox < 0 THEN
                    ASSIGN tt-nfe-ref.qt-vl-aprox = tt-nfe-ref.qt-vl-aprox * -1.  /*transformar numero negativo em positivo*/
            END.
        END.
        {&open-query-br-it-nota-fisc}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancelar D-Dialog
ON CHOOSE OF btn-cancelar IN FRAME D-Dialog /* Cancelar */
DO:
    ASSIGN p_rw_doc_nota = ?.
              
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok D-Dialog
ON CHOOSE OF btn-ok IN FRAME D-Dialog /* Ok */
DO:
    DO WITH FRAME {&FRAME-NAME} :
        /* --- Devolucao de Clientes --- */
        FIND FIRST it-nota-fisc 
            WHERE it-nota-fisc.cod-estabel  = tt-nfe-ref.cod-estabel
              AND it-nota-fisc.serie        = tt-nfe-ref.serie
              and it-nota-fisc.nr-nota-fis  = tt-nfe-ref.nr-nota-fis
              and it-nota-fisc.nr-seq-fat   = tt-nfe-ref.nr-seq-fat
              and it-nota-fisc.it-codigo    = tt-nfe-ref.it-codigo NO-LOCK NO-ERROR.

        IF AVAIL it-nota-fisc THEN 
            ASSIGN p_rw_doc_nota = ROWID(it-nota-fisc).
        ELSE 
            ASSIGN p_rw_doc_nota = ?.
            
        IF p_rw_doc_nota <> ? THEN DO:
            APPLY "GO" TO FRAME {&FRAME-NAME}.
            APPLY "CLOSE":U TO THIS-PROCEDURE.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-c-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-c-cod-estabel D-Dialog
ON LEAVE OF fi-c-cod-estabel IN FRAME D-Dialog /* Estabelec */
DO:
    /*DO WITH FRAME {&FRAME-NAME} :
        FIND FIRST estabelec WHERE estabelec.cod-estabel = INPUT fi-c-cod-estabel
                             NO-LOCK NO-ERROR.
        ASSIGN fi-c-nome-estab:SCREEN-VALUE = IF AVAIL estabelec THEN estabelec.nome
                                                                 ELSE "".
    END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tp-busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-busca D-Dialog
ON VALUE-CHANGED OF rs-tp-busca IN FRAME D-Dialog
DO:
  IF INPUT FRAME {&FRAME-NAME} rs-tp-busca = 1 THEN
      ASSIGN fi-d-periodo-ini:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             fi-d-periodo-fim:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  ELSE
      ASSIGN fi-d-periodo-ini:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             fi-d-periodo-fim:SENSITIVE IN FRAME {&FRAME-NAME} = YES.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY rs-tp-busca fi-c-cod-estabel fi-c-nome-estab fi-i-cod-emitente 
          fi-c-nome-emit fi-c-it-codigo-ini fi-c-it-codigo-fim 
          fi-nro-docto-nota-ini fi-nro-docto-nota-fim fi-serie-docto-nota-ini 
          fi-serie-docto-nota-fim fi-seq-docto-nota-ini fi-seq-docto-nota-fim 
          fi-d-periodo-ini fi-d-periodo-fim 
      WITH FRAME D-Dialog.
  ENABLE rs-tp-busca fi-c-it-codigo-ini fi-c-it-codigo-fim 
         fi-nro-docto-nota-ini fi-nro-docto-nota-fim fi-serie-docto-nota-ini 
         fi-serie-docto-nota-fim fi-seq-docto-nota-ini fi-seq-docto-nota-fim 
         bt-confirma br-it-nota-fisc btn-ok btn-cancelar fi-d-periodo-ini 
         fi-d-periodo-fim RECT-43 RECT-41 IMAGE-22 IMAGE-25 IMAGE-26 IMAGE-27 
         RECT-34 IMAGE-18 RECT-36 IMAGE-36 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
  
    FIND FIRST bf-nfe-it-nota-fisc-rec WHERE ROWID(bf-nfe-it-nota-fisc-rec) = p_rw_item
                                       NO-LOCK NO-ERROR.
    FIND FIRST bf-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.
    IF AVAIL bf-nfe-nota-fiscal-rec THEN DO:
        /* --- Retona o Tipo de Nota de acordo com a Natureza --- */
        RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
        RUN pi_valida_tipo_nota IN h_bonfe001 (INPUT  p_natur_oper,
                                               OUTPUT i-tipo-nfe,
                                               OUTPUT i-tp-oper-terc).
        DELETE PROCEDURE h_bonfe001.

        FIND FIRST estabelec WHERE estabelec.cod-estabel = bf-nfe-nota-fiscal-rec.cod-estabel
                             NO-LOCK NO-ERROR.
        IF AVAIL estabelec THEN
            ASSIGN fi-c-cod-estabel = estabelec.cod-estabel
                   fi-c-nome-estab  = estabelec.nome.

        FIND emitente WHERE 
             emitente.nome-abrev = bf-nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN
            ASSIGN fi-i-cod-emitente = emitente.cod-emitente
                   fi-c-nome-emit    = emitente.nome-emit.
            
        FIND FIRST ITEM WHERE ITEM.it-codigo = p_it_codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
            IF TRIM(p_it_codigo) = "" THEN
                ASSIGN fi-c-it-codigo-ini = ""
                       fi-c-it-codigo-fim = "ZZZZZZZZZZZZZZZZ".
            ELSE
                ASSIGN fi-c-it-codigo-ini = ITEM.it-codigo
                       fi-c-it-codigo-fim = ITEM.it-codigo.
        END.
    END.

    ASSIGN fi-d-periodo-ini = TODAY - 90        
           fi-d-periodo-fim = TODAY.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ).

    /*DO WITH FRAME {&FRAME-NAME}:
        /* --- Devolucao de Clientes --- */
        IF i-tipo-nfe = 2 THEN DO:
            ASSIGN fi-nro-docto-nota-ini:LABEL   = "Nr. Nota Fiscal"
                   fi-serie-docto-nota-ini:LABEL = "Serie Nota Fiscal"
                   fi-seq-docto-nota-ini:LABEL   = "Seq Nota Fiscal".

            /*ASSIGN br-saldo-terc:HIDDEN = YES
                   fi-serie-docto-nota-ini:SENSITIVE = NO
                   fi-serie-docto-nota-fim:SENSITIVE = NO
                   fi-seq-docto-nota-ini:SENSITIVE = NO
                   fi-seq-docto-nota-fim:SENSITIVE = NO.*/
                    
        END.
        
        /* --- Retorno Terceiros --- */
        /*IF i-tipo-nfe = 3 THEN DO:
            ASSIGN fi-nro-docto-nota-ini:LABEL     = "Nr. Documento"
                   fi-serie-docto-nota-ini:LABEL   = "Serie Documento"
                   fi-seq-docto-nota-ini:LABEL     = "Seq Documento".

            ASSIGN br-it-nota-fisc:HIDDEN = YES.
        END. */
    END.*/
    
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-nfe-ref"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

