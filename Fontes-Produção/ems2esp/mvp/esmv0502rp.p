&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (2008)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*****************************************************************************
**
**       PROGRAMA: ESMV0502rp.p
**
**       DATA....: Fevereiro de 2008
**
**       AUTOR...: Ricardo Sutil - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Espec¡fico para empresa Yamana (Relat¢rio por Status da 
**                 ordem de manuten‡Æo)
**
*****************************************************************************/
{include/i-prgvrs.i ESMV0502RP 2.06.00.0000}  /*** 010000 ***/
/*****************************************************************************
**
**       PROGRAMA: ESMV0502RP.p
**
**       DATA....: Fevereiro de 2007
**
**       AUTOR...: Ricardo Sutil - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Fonte Espec¡fico Yamana (Relat¢rio Status Ordem De 
**                 Manuten‡Æo)
**
*****************************************************************************/
                                             /** Defini‡Æo das temp-tables **/
                                             /** Parƒmetros para Sele‡Æo   **/
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD i-ord-ini                 LIKE mmv-ord-manut.nr-ord-produ
    FIELD i-ord-fim                 LIKE mmv-ord-manut.nr-ord-produ
    FIELD c-oficina-ini             LIKE mmv-ord-manut.cod-ofici
    FIELD c-oficina-fim             LIKE mmv-ord-manut.cod-ofici
    FIELD d-data-fim                LIKE mmv-ord-manut.dat-abert
    FIELD d-data-ini                LIKE mmv-ord-manut.dat-abert
    FIELD i-empresa-fim             LIKE mmv-ord-manut.ep-codigo
    FIELD i-empresa-ini             LIKE mmv-ord-manut.ep-codigo
    FIELD c-equipamento-ini         LIKE mmv-ord-manut.cod-eqpto
    FIELD c-equipamento-fim         LIKE mmv-ord-manut.cod-eqpto
    FIELD c-componente-ini          LIKE mmv-ord-manut.cod-compon
    FIELD c-componente-fim          LIKE mmv-ord-manut.cod-compon
    FIELD lNormal                   AS LOGICAL 
    FIELD lAguardandoMaterial       AS LOGICAL
    FIELD lAguardandoMOB            AS LOGICAL
    FIELD lServExt                  AS LOGICAL
    FIELD lAguardandoEquipamento    AS LOGICAL
    FIELD lAguardandoProgramacao    AS LOGICAL 
    FIELD lAguardandoRecurso        AS LOGICAL
    FIELD lOrdemEquipamento         AS LOGICAL
    FIELD lOrdemComponente          AS LOGICAL
    FIELD lOrdemCCusto              AS LOGICAL
    FIELD destino                   AS INTEGER
    FIELD arquivo                   AS CHAR FORMAT "x(35)":U
    FIELD usuario                   AS CHAR FORMAT "x(12)":U
    FIELD data-exec                 AS DATE
    FIELD hora-exec                 AS INTEGER
    FIELD classifica                AS INTEGER
    FIELD desc-classifica           AS CHAR FORMAT "x(40)":U
    FIELD modelo                    AS CHAR FORMAT "x(35)":U
    field l-habilitaRtf             AS LOG.
                                               
                                                /** Sele‡Æo para ImpressÆo **/
/** Carrega a sele‡Æo para mostrar conforme a classifica‡Æo do usu rio:    **/
                                                /** Ordem ou Status        **/
DEFINE TEMP-TABLE tt-selecao
    FIELD nr-ord-produ     LIKE mmv-ord-manut.nr-ord-produ 
    FIELD cod-generica     AS CHAR FORMAT "x(36)"
    FIELD cd-tipo          LIKE mmv-ord-manut.cd-tipo 
    FIELD cod-ofici        LIKE mmv-ord-manut.cod-ofici 
    FIELD cod-plandor      LIKE mmv-ord-manut.cod-plandor 
    FIELD dat-abert        LIKE mmv-ord-manut.dat-abert 
    FIELD HRA-ABERT        LIKE mmv-ord-manut.hra-abert 
    FIELD hra-criacao      LIKE mmv-ord-manut.hra-criacao 
    FIELD dat-criacao      LIKE mmv-ord-manut.dat-criacao 
    FIELD idi-tip-ord      LIKE mmv-ord-manut.idi-tip-ord
    FIELD dat-prev-term    LIKE mmv-ord-manut.dat-prev-term  
    FIELD dat-term         LIKE mmv-ord-manut.dat-term
    FIELD hra-prev-term    LIKE mmv-ord-manut.hra-prev-term
    FIELD hra-term         LIKE mmv-ord-manut.hra-term
    FIELD data-sit         LIKE mmv-ord-status.data-sit 
    FIELD hora-sit         LIKE mmv-ord-status.hora-sit                                                       
    FIELD idi-status-ord   LIKE mmv-ord-status.idi-status-ord
    FIELD ct-ordem         LIKE mmv-ord-manut.ct-ordem 
    FIELD cc-ordem         LIKE mmv-ord-manut.cc-ordem
    FIELD ep-codigo-ordem  LIKE mmv-ord-manut.ep-codigo-ordem
    FIELD ep-codigo        LIKE mmv-ord-manut.ep-codigo
    FIELD ep-codigo-despes LIKE mmv-ord-manut.ep-codigo-despes 
    FIELD ct-codigo        LIKE mmv-ord-manut.ct-codigo
    FIELD cc-codigo        LIKE mmv-ord-manut.cc-codigo
    FIELD cod-usuar        LIKE mmv-ord-manut.cod-usuar
    FIELD cod-estabel      LIKE mmv-ord-manut.cod-estabel
    FIELD dat-entr         LIKE mmv-ord-manut.dat-entr
    FIELD hra-entr         LIKE mmv-ord-manut.hra-entr
    FIELD estado           LIKE mmv-ord-manut.estado
    FIELD cod-sub-sist     LIKE mmv-ord-manut.cod-sub-sist
    FIELD val-hodom-horim  LIKE mmv-ord-manut.val-hodom-horim
    FIELD cod-compon       LIKE mmv-ord-manut.cod-compon
    INDEX por-ordem        IS UNIQUE PRIMARY  nr-ord-produ 
    INDEX por-status       IS UNIQUE idi-status-ord nr-ord-produ.


/** Defini‡Æo de Vari veis **/
DEFINE VARIABLE c-liter-sel  AS CHARACTER FORMAT "x(09)" NO-UNDO.
DEFINE VARIABLE c-liter-cla  AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE c-liter-par  AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE c-liter-imp  AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE c-destino    AS CHARACTER FORMAT "x(09)" NO-UNDO.

/** Layout Relat¢rio **/
DEFINE VARIABLE c-cod-usuar           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ccod-ofici            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE Cdes-ofici            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-Status             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-status              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-data               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-empresa            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-pagina             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-hr-movto           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-titulo             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-documento          AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE i-num-ord             AS INTEGER    NO-UNDO.  
DEFINE VARIABLE cl-abertura           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE d-data-abert          AS DATE       NO-UNDO.  
DEFINE VARIABLE c-hora-abert          AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-tp-om              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-tp-om               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-prev-term          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-data-prev           AS DATE       NO-UNDO.  
DEFINE VARIABLE c-hora-prev           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-estado             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE c-estado              AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-termino            AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE d-data-term           AS DATE       NO-UNDO.  
DEFINE VARIABLE c-hora-term           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-conta              AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE iep-cod-ordem         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-conta               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cc-custo              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-criacao            AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE d-dat-criacao         AS DATE       NO-UNDO.  
DEFINE VARIABLE c-hr-cricao           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-conta2             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE iep-conta-desp        AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE c-cont-desp           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cc-cond-desp          AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-usuar              AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-estabel1           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-estabel         AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-oficina            AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE i-cod-emp             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE c-cod-generica        AS CHARACTER  NO-UNDO. /** Receber  o cod do equipamento e empresa, caso seja componente recebera apenas o cod-compon   **/ 
DEFINE VARIABLE c-generica            AS CHARACTER  NO-UNDO. /** Essa Vari vel conforme o tipo de Manuten‡Æo receber  um modelo de componente ou equipamento  **/
DEFINE VARIABLE cl-val-generica       AS CHARACTER  NO-UNDO. /** A varivel assumir  dois nomes e sendo Sub-Sistema ou Contador, conforme o tipo de Manuten‡Æo **/
DEFINE VARIABLE c-val-generica        AS CHARACTER  NO-UNDO. /** Vari vel char para receber qualquer tipo de valor conforme o tipo de manuten‡Æo              **/
DEFINE VARIABLE c-un                  AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-tp-manut           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE i-tp-manut            AS INTEGER    NO-UNDO.  
DEFINE VARIABLE c-descricao           AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-planej             AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE c-plane               AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE c-nome                AS CHARACTER  NO-UNDO.  
DEFINE VARIABLE cl-entrada            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-data-entrada        AS DATE       NO-UNDO.
DEFINE VARIABLE c-hora-entrada        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cl-generica           AS CHARACTER  NO-UNDO. /** Mudar  o label conforme a manuten‡Æo **/
DEFINE VARIABLE cl-data-status        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-plano               AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-hora-status         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-data-status         AS DATE       NO-UNDO.
DEFINE VARIABLE contador              AS INTEGER    NO-UNDO.

/** Tradu‡Æo das vari veis **/
{utp/ut-liter.i Usu rio * R }
assign cl-usuar = trim(return-value).
{utp/ut-liter.i Estabel * R }
ASSIGN cl-estabel1 = TRIM(RETURN-VALUE).
{utp/ut-liter.i Conta_Ordem * R }
assign  cl-conta = trim(return-value).
{utp/ut-liter.i Conta_Despesa * R }
assign  cl-conta2 = trim(return-value).   
{utp/ut-liter.i Cria‡Æo * R }
assign  cl-criacao = trim(return-value).   
{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
{utp/ut-liter.i PµGINA * R }
assign  cl-pagina = trim(return-value).
{utp/ut-liter.i Listagem_Ordem_Por_Status * R }
assign  cl-titulo = trim(return-value).
{utp/ut-liter.i Ordem_de_Manuten‡Æo * R }
assign  cl-documento = trim(return-value).
{utp/ut-liter.i Abertura * R }
assign  cl-abertura = trim(return-value).
{utp/ut-liter.i Entrada * R }
assign  cl-entrada = trim(return-value).
{utp/ut-liter.i Tipo_OM * R }
assign  cl-tp-om = trim(return-value).
{utp/ut-liter.i Prev.T‚rmino * R }
assign  cl-prev-term = trim(return-value).
{utp/ut-liter.i T‚rmino * R }
assign  cl-termino = trim(return-value).
{utp/ut-liter.i Estado * R }
assign  cl-estado = trim(return-value).
{utp/ut-liter.i Conta_Ordem * R }
assign cl-conta = trim(return-value).
{utp/ut-liter.i Equipamento * R }
assign cl-generica = trim(return-value).
{utp/ut-liter.i Oficina * R }
assign cl-oficina = trim(return-value).
{utp/ut-liter.i Contador * R }
assign cl-val-generica = trim(return-value).
{utp/ut-liter.i Tipo_Manuten‡Æo * R }
ASSIGN cl-tp-manut  = trim(return-value).
{utp/ut-liter.i Planejador * R }
assign cl-planej  = trim(return-value).
{utp/ut-liter.i HORA * R }
ASSIGN cl-hr-movto   = trim(return-value).
{utp/ut-liter.i Data_da_Situa‡Æo * R }
ASSIGN cl-data-status   = trim(return-value).
{utp/ut-liter.i "Status" * R }
ASSIGN cl-status   = trim(return-value).

/** Form para impressÆo **/    
FORM HEADER 
     "+"                                        AT  1 
     FILL("-":U, 78)    format "x(78)":U        AT  2  
     "+"                                        AT 80 SKIP 
     "|"                                        AT  1 
     cl-data            format "x(04)"          AT  2
     ":"                                        AT  6  
     today format "99/99/9999":U                AT  8 
     cl-empresa         format "x(41)"          AT 24
     cl-pagina          format "x(06)"          AT 65 
     ":"                                        AT 71 
     PAGE-NUMBER format ">>>>>9":U              AT 73   
     "|"                                        AT 80 skip
     "|"                                        AT  1
     cl-hr-movto        format "x(04)"          AT  2 
     ":"                                        AT  6 
     string(TIME,"HH:MM")                       AT  8
     "ESMV0502"                                 AT 18
     "-"                                        AT 27
     cl-titulo          format "x(40)"          AT 29 
     "|"                                        AT 80 skip
     "|"                                        AT  1
     "|"                                        AT 80 SKIP
     "|"                                        AT  1 
     cl-documento       format "x(19)"          AT  2     
     ":"                                        AT 21 
     i-num-ord        format "->,>>>,>>>,>>9"   AT 22
     "|"                                        AT 80 SKIP

     "|"                                        AT  1 
     cl-generica       format "x(12)"           AT  9
     ":"                                        AT 21
     c-cod-generica      format "x(18)"         AT 23
     "-"                                        AT 45
     c-generica   format "x(30)"                AT 47
     "|"                                        AT 80 SKIP

     "|"                                        AT  1 
     cl-status      format "x(6)"               AT  5
     ":"                                        AT 11
     c-status       FORMAT "x(27)"              AT 13
     cl-data-status format "x(16)"              AT 43 /*Para Status */
     ":"                                        AT 59 
     d-data-status     format "99/99/9999":U    AT 61
     c-hora-status     format "99:99":u         AT 74
     
     "|"                                        AT 80 SKIP
     
     "|"                                        AT  1
     cl-abertura        format "x(08)"          AT 51
     ":"                                        AT 59 
     d-data-abert      format "99/99/9999":U    AT 61
     c-hora-abert      format "99:99":u         AT 74
     "|"                                        AT 80 SKIP

     "|"                                        AT  1
     cl-entrada     format "x(07)"              AT 52
     ":"                                        AT 59 
     d-data-entrada format "99/99/9999":U       AT 61
     c-hora-entrada format "99:99":u            AT 74
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-tp-om     format "x(07)"                AT 10  
     ":"                                        AT 17
     c-tp-om      format "x(15)"                AT 19
     cl-prev-term format "x(12)"                AT 47
     ":"                                        AT 59
      d-data-prev  format "99/99/9999":U        AT 61
      c-hora-prev  format "99:99":u             AT 74
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-estado      format "x(06)"              AT 11
     ":"                                        AT 17
     c-estado       format "x(16)"              AT 19
     cl-termino     format "x(07)"              AT 52
     ":"                                        AT 59
     d-data-term    format "99/99/9999":U       AT 61
     c-hora-term    format "99:99":u            AT 74
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-conta       format "x(11)"              AT  6
     ":"                                        AT 17
     iep-cod-ordem  format ">>9"                AT 18
     "-"                                        AT 22
     c-conta        format "x(08)"              AT 23
     "-"                                        AT 32
     cc-custo                                   AT 34
     cl-criacao     format "x(07)"              AT 52
     ":"                                        AT 59
     d-dat-criacao  format "99/99/9999":U       AT 61
     c-hr-cricao    format "99:99":u            AT 74
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-conta2      format "X(13)"              AT  4
     ":"                                        AT 17
     iep-conta-desp format ">>9"                AT 18
     "-"                                        AT 22
     c-cont-desp    format "x(08)"              AT 24
     "-"                                        AT 34
     cc-cond-desp   format "x(08)"              AT 36
     cl-usuar       format "x(07)"              AT 52
     ":"                                        AT 59
     c-cod-usuar    format "x(12)"              AT 61 
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-estabel1    format "x(7)"               AT 52
     ":"                                        AT 59
     c-cod-estabel  format "x(3)"               AT 61
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-oficina       format "x(7)"             AT 10
     ":"                                        AT 17
     ccod-ofici      format "x(08)"             at 19 no-label
     "-"                                        AT 38
    Cdes-ofici      format "x(30)"              at 40 
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
     cl-val-generica  format "x(10)"            AT  7
     ":"                                        AT 17
     c-val-generica format "x(45)"              AT 20
      /* c-un                                       AT 30 */
     "|"                                        AT 80 SKIP
     "|"                                        AT  1
      cl-tp-manut format "x(15)"                AT  2
      ":"                                       AT 17
      i-tp-manut   format ">>,>>9"              AT 19
      "-"                                       AT 38
      c-descricao   format "x(30)"              AT 40
      "|"                                       AT 80 SKIP
      "|"                                       AT  1
      cl-planej format "x(10)"                  AT  7
      ":"                                       AT 17
      c-plane                                   AT 19
      "-"                                       AT 38
      c-nome         format "x(30)"             AT 40
      "|"                                       AT 80 SKIP
      "+"                                        AT  1 
      FILL("-":U, 78)    format "x(78)":U        AT  2  
      "+"                                        AT 80
      with no-box width 132 NO-LABELS frame fFrameA stream-io.

/** Form de Parƒmetros **/
form   /*SELE€ÇO*/
       SKIP(1)
       c-liter-sel                      AT 09                       NO-LABEL
       SKIP(1)
       tt-param.i-ord-ini               COLON 30  "|<  >|" AT 50 
       tt-param.i-ord-fim               NO-LABEL                 
       tt-param.c-oficina-ini           COLON 30  "|<  >|" AT 50 
       tt-param.c-oficina-fim           NO-LABEL
       tt-param.d-data-ini              COLON 30  "|<  >|" AT 50 
       tt-param.d-data-fim              NO-LABEL                
       tt-param.i-empresa-ini           COLON 30  "|<  >|" AT 50 
       tt-param.i-empresa-fim           NO-LABEL                
       tt-param.c-equipamento-ini       COLON 30  "|<  >|" AT 50 
       tt-param.c-equipamento-fim       NO-LABEL                
       tt-param.c-componente-ini        COLON 30  "|<  >|" AT 50 
       tt-param.c-componente-fim        NO-LABEL                
       /*CLASSIFICA€ÇO*/
       skip(1)
       c-liter-cla              AT 09    NO-LABEL
       skip(1)
       tt-param.classifica      COLON 30 " - " tt-param.desc-classifica NO-LABEL
       /*PAR¶METRO*/
       skip(1)
       c-liter-par              AT 09    NO-LABEL
       skip(1)
       "Status":T SKIP 
       tt-param.lNormal                     COLON 50
       tt-param.lAguardandoMaterial         COLON 50
       tt-param.lAguardandoMOB              COLON 50
       tt-param.lServExt                    COLON 50
       tt-param.lAguardandoEquipamento      COLON 50
       tt-param.lAguardandoProgramacao      COLON 50
       tt-param.lAguardandoRecurso          COLON 50
       tt-param.lOrdemEquipamento           COLON 50
       tt-param.lOrdemComponente            COLON 50 
       tt-param.lOrdemCCusto                COLON 50 
       /*IMPRESSÇO*/
       skip(1)
       c-liter-imp              AT 09    no-label
       SKIP(1)
       c-destino                COLON 30 " - " tt-param.arquivo NO-LABEL
       tt-param.usuario         COLON 30
       SKIP(1)
       WITH WIDTH 132 SIDE-LABELS FRAME f-param-definidos STREAM-IO.
/** Form pula linhas para cada trˆs ordens apresentadas **/
FORM HEADER
       SKIP(1)
       SKIP(1)
       SKIP(1)
       SKIP(1)
    WITH WIDTH 132 SIDE-LABELS FRAME f-pula-linha STREAM-IO.

                                               /** Literais Form Parƒmetros **/
{utp/ut-liter.i Usu rio * l}
ASSIGN tt-param.usuario:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Destino * l}
ASSIGN c-destino:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i SELE€ÇO * R}
ASSIGN c-liter-sel = TRIM(RETURN-VALUE).                                                              

{utp/ut-liter.i CLASSIFICA€ÇO * R}                                                                    
ASSIGN c-liter-cla = TRIM(RETURN-VALUE).                                                              
                                                                                                      
{utp/ut-liter.i PAR¶METROS * R}                                                                       
ASSIGN c-liter-par = TRIM(RETURN-VALUE).                                                              
                                                                                                      
{utp/ut-liter.i IMPRESSÇO * R}                                                                        
ASSIGN c-liter-imp = TRIM(RETURN-VALUE).                                                              
                                        
{utp/ut-liter.i "Ordem" * R}
ASSIGN tt-param.i-ord-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Oficina" * R}
ASSIGN tt-param.c-oficina-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Data Abertura" * R}
ASSIGN tt-param.d-data-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Empresa" * R}
ASSIGN tt-param.i-empresa-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Equipamento" * R}
ASSIGN tt-param.c-equipamento-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Componente" * R}
ASSIGN tt-param.c-componente-ini:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i "Normal" * R}
ASSIGN tt-param.lNormal:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Material * R}
ASSIGN tt-param.lAguardandoMaterial:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_MÆo-de-Obra * R}
ASSIGN tt-param.lAguardandoMOB:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Servi‡o_Externo * R}
ASSIGN tt-param.lServExt:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Equipamento * R}
ASSIGN tt-param.lAguardandoEquipamento:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Programa‡Æo * R}
ASSIGN tt-param.lAguardandoProgramacao:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Recurso * R}
ASSIGN tt-param.lAguardandoRecurso:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Ordem_Equipamento * R}
ASSIGN tt-param.lOrdemEquipamento:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Ordem_Componente * R}
ASSIGN tt-param.lOrdemComponente:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Aguardando_Ordem_Centro_Custo * R}
ASSIGN tt-param.lOrdemCCusto:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Tipo_Classifica‡Æo * R}
ASSIGN tt-param.classifica:LABEL IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lNormal:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE). 

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lAguardandoMaterial:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lAguardandoMOB:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lServExt:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lAguardandoEquipamento:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lAguardandoProgramacao:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lAguardandoRecurso:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lOrdemEquipamento:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lOrdemComponente:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).

{utp/ut-liter.i Sim/NÆo * r}
ASSIGN tt-param.lOrdemCCusto:FORMAT IN FRAME f-param-definidos = TRIM(RETURN-VALUE).
/*** Fim de tradu‡Æo ***/

/**** Dados da Window para o RP ****/    
DEFINE  TEMP-TABLE tt-raw-digita
   FIELD raw-digita AS RAW.
/****************************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                                                /** Defini‡Æo de Vari veis **/
define variable h-acomp as handle no-undo.
{include/i-rpvar.i}
/****************************************************************************/
                                                     /** Cabe‡alho e Forms **/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.
&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.63
         WIDTH              = 32.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/** Parametriza padräes de cabe‡alho e rodap‚ a serem exibidos **/
/* run piInicial in this-procedure. */
       
/** Imprime cabe‡alho e abre o output para arquivo **/

{include/i-rpcab.i}    
{include/i-rpout.i}

/** Procedure para inicializar c lculos e impressÆo **/
run piPrincipal in this-procedure.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

return "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-displayParametros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayParametros Procedure 
PROCEDURE displayParametros :
/*------------------------------------------------------------------------------
  Purpose:     displayParametros
  Parameters:  <none>
  Notes:       Mostra os parƒmetros selecionados
------------------------------------------------------------------------------*/
/** Cria a p gina dos parƒmetros **/
page.

/******************************************************************
  Espa‡o reservado para buscar informa‡äes a serem impressas nos 
  parƒmetros (Opcional)
******************************************************************/

/** L¢gica opcional do programador **/

/******************************************************************
  Final do espa‡o reservado
******************************************************************/

DISPLAY /*SELE€ÇO*/ 
        c-liter-sel       
        tt-param.i-ord-ini        
        tt-param.i-ord-fim        
        tt-param.c-oficina-ini    
        tt-param.c-oficina-fim    
        tt-param.d-data-ini       
        tt-param.d-data-fim       
        tt-param.i-empresa-ini    
        tt-param.i-empresa-fim    
        tt-param.c-equipamento-ini
        tt-param.c-equipamento-fim
        tt-param.c-componente-ini 
        tt-param.c-componente-fim 
        /*CLASSIFICA€ÇO*/
        c-liter-cla          
        tt-param.classifica  tt-param.desc-classifica
        /*PAR¶METRO*/
        c-liter-par          
        tt-param.lNormal                
        tt-param.lAguardandoMaterial    
        tt-param.lAguardandoMOB         
        tt-param.lServExt               
        tt-param.lAguardandoEquipamento 
        tt-param.lAguardandoProgramacao 
        tt-param.lAguardandoRecurso     
        tt-param.lOrdemEquipamento      
        tt-param.lOrdemComponente       
        tt-param.lOrdemCCusto           
        /*IMPRESSÇO*/
        c-liter-imp          
        c-destino            
        tt-param.arquivo
        tt-param.usuario     
    WITH FRAME f-param-definidos.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/*****************************************************************************/
/************************Defini»’o de Variÿveis de Layout*************/
&IF DEFINED(EXCLUDE-piInicial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piInicial Procedure 
PROCEDURE piInicial :
/*------------------------------------------------------------------------------
  Purpose:     piInicial
  Parameters:  <none>
  Notes:       Define os valores que serÆo mostrados o cabe‡alho e rodap‚
------------------------------------------------------------------------------*/
/** Busca os parƒmetros criados na interface gr fica **/
/* find first tt-param.  */

assign c-programa = "ESMV0502"
       c-versao   = "2.06"
       c-revisao  = "000"
       /** Define o destino do arquivo a ser gerado **/
       c-destino  = {varinc/var00002.i 04 integer(tt-param.destino)}.

/** Busca empresa padrÆo **/
find first param-global no-lock no-error.
if  available param-global then
    assign c-empresa = param-global.grupo.

/** Guarda valores para imprimir t¡tulos **/
{utp/ut-liter.i "ESMV0502 - Listagem de Ordem por Status" *}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i "Manuten‡Æo Mecƒnica" *}
assign c-sistema = trim(return-value).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piPrincipal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPrincipal PROCEDURE 
PROCEDURE piPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     piPrincipal
  Parameters:  <none>
  Notes:       Corpo princiapl da aplica‡Æo
------------------------------------------------------------------------------*/
/** Mostra frames de cabe‡alho e rodap‚ padräes da Datasul **/
    /*
view frame f-cabec. 
view frame f-rodape. */

/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/

/** L¢gica opcional do programador **/

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

FOR EACH  mmv-ord-manut
    WHERE mmv-ord-manut.nr-ord-produ     >= tt-param.i-ord-ini
    AND   mmv-ord-manut.nr-ord-produ     <= tt-param.i-ord-fim
    AND   mmv-ord-manut.cod-ofici        >= tt-param.c-oficina-ini
    AND   mmv-ord-manut.cod-ofici        <= tt-param.c-oficina-fim 
    AND   mmv-ord-manut.dat-abert        >= tt-param.d-data-ini
    AND   mmv-ord-manut.dat-abert        <= tt-param.d-data-fim 
    AND   mmv-ord-manut.ep-codigo        >= tt-param.i-empresa-ini
    AND   mmv-ord-manut.ep-codigo        <= tt-param.i-empresa-fim
    AND   mmv-ord-manut.cod-eqpto        >= tt-param.c-equipamento-ini
    AND   mmv-ord-manut.cod-eqpto        <= tt-param.c-equipamento-fim
    AND   mmv-ord-manut.cod-compon       >= tt-param.c-componente-ini
    AND   mmv-ord-manut.cod-compon       <= tt-param.c-componente-fim  
    AND   ((mmv-ord-manut.idi-tip-ord = 1 AND tt-param.lOrdemEquipamento) 
        OR (mmv-ord-manut.idi-tip-ord = 2 AND tt-param.lOrdemComponente)
        OR (mmv-ord-manut.idi-tip-ord = 3 AND tt-param.lOrdemCCusto))  NO-LOCK:
        /** Procura tabela espec¡fica definindo o status da Ordem**/
        FOR FIRST mmv-ord-status
            WHERE mmv-ord-status.nr-ord-produ = mmv-ord-manut.nr-ord-produ NO-LOCK: 
                IF (tt-param.lNormal AND mmv-ord-status.idi-status-ord = 1)
                    OR (tt-param.lAguardandoMaterial AND mmv-ord-status.idi-status-ord = 2)
                    OR (tt-param.lAguardandoMOB AND mmv-ord-status.idi-status-ord = 3)
                    OR (tt-param.lServExt AND mmv-ord-status.idi-status-ord = 4)
                    OR (tt-param.lAguardandoEquipamento AND mmv-ord-status.idi-status-ord = 5)
                    OR (tt-param.lAguardandoProgramacao AND mmv-ord-status.idi-status-ord = 6)
                    OR (tt-param.lAguardandoRecurso AND mmv-ord-status.idi-status-ord = 7) THEN DO: 
         
         /** Temp-Temp para mostrar as informa‡äes que irÆo para a tela **/
         CREATE tt-selecao.
         ASSIGN tt-selecao.nr-ord-produ     = mmv-ord-manut.nr-ord-produ   
                tt-selecao.cod-generica     = IF mmv-ord-manut.idi-tip-ord = 1 THEN mmv-ord-manut.cod-eqpto 
                                              ELSE IF mmv-ord-manut.idi-tip-ord = 2 THEN mmv-ord-manut.cod-compon
                                              ELSE ""
                tt-selecao.cd-tipo          = mmv-ord-manut.cd-tipo   
                tt-selecao.cod-ofici        = mmv-ord-manut.cod-ofici     
                tt-selecao.cod-plandor      = mmv-ord-manut.cod-plandor
                tt-selecao.dat-abert        = mmv-ord-manut.dat-abert   
                tt-selecao.hra-abert        = mmv-ord-manut.hra-abert
                tt-selecao.hra-criacao      = mmv-ord-manut.hra-criacao
                tt-selecao.dat-criacao      = mmv-ord-manut.dat-criacao
                tt-selecao.dat-prev-term    = mmv-ord-manut.dat-prev-term
                tt-selecao.dat-term         = mmv-ord-manut.dat-term
                tt-selecao.hra-prev-term    = mmv-ord-manut.hra-prev-term
                tt-selecao.hra-term         = mmv-ord-manut.hra-term
                tt-selecao.data-sit         = mmv-ord-status.data-sit
                tt-selecao.hora-sit         = mmv-ord-status.hora-sit    
                tt-selecao.idi-status-ord   = mmv-ord-status.idi-status-ord
                tt-selecao.ct-ordem         = mmv-ord-manut.ct-ordem  
                tt-selecao.cc-ordem         = mmv-ord-manut.cc-ordem  
                tt-selecao.ep-codigo-ordem  = mmv-ord-manut.ep-codigo-ordem
                tt-selecao.ep-codigo-despes = mmv-ord-manut.ep-codigo-despes
                tt-selecao.ct-codigo        = mmv-ord-manut.ct-codigo
                tt-selecao.cc-codigo        = mmv-ord-manut.cc-codigo
                tt-selecao.cod-usuar        = mmv-ord-manut.cod-usuar
                tt-selecao.cod-estabel      = mmv-ord-manut.cod-estabel
                tt-selecao.dat-entr         = mmv-ord-manut.dat-entr
                tt-selecao.hra-entr         = mmv-ord-manut.hra-entr 
                tt-selecao.estado           = mmv-ord-manut.estado
                tt-selecao.idi-tip-ord      = mmv-ord-manut.idi-tip-ord
                tt-selecao.ep-codigo        = mmv-ord-manut.ep-codigo
                tt-selecao.val-hodom-horim  = mmv-ord-manut.val-hodom-horim
                tt-selecao.cod-sub-sist     = mmv-ord-manut.cod-sub-sist
                tt-selecao.cod-compon       = mmv-ord-manut.cod-compon
             .
                END. 
         
        END. 
END.

IF tt-param.classifica = 1 THEN DO: /** Classifica‡Æo por Ordem, mostra normalmente a sele‡Æo **/
    FOR EACH tt-selecao NO-LOCK:

        ASSIGN c-status = {ydminc/i00ydm001.i 4 tt-selecao.idi-status-ord}
               d-data-status = tt-selecao.data-sit
               c-hora-status = tt-selecao.hora-sit. 
    
        FOR FIRST mmv-ofici FIELDS (cod-ofici des-ofici) 
            WHERE mmv-ofici.cod-ofici = tt-selecao.cod-ofici NO-LOCK:
            ASSIGN cCod-Ofici = mmv-ofici.cod-ofici
                   cDes-Ofici = mmv-ofici.des-ofici.
        END.
        /** Busca a descriÆo da Oficina **/
        FOR FIRST mmv-ofici FIELDS (cod-ofici des-ofici) 
            WHERE mmv-ofici.cod-ofici = tt-selecao.cod-ofici NO-LOCK:
            ASSIGN cCod-Ofici = mmv-ofici.cod-ofici
                   cDes-Ofici = mmv-ofici.des-ofici.
        END.
        /** Busca o nome do planajador **/
        FOR FIRST mmv-plandor FIELDS (cod-plandor nom-plandor)
            WHERE mmv-plandor.cod-plandor = tt-selecao.cod-plandor NO-LOCK:
           ASSIGN c-nome = mmv-plandor.nom-plandor.
        END.
        /** Busca para o tipo de manuten‡Æo **/
        FOR FIRST tipo-manut FIELDS (cd-tipo descricao)
            WHERE tipo-manut.cd-tipo = tt-selecao.cd-tipo NO-LOCK:
           ASSIGN c-descricao = tipo-manut.descricao.
        END.
        /** Pelo C¢digo do equipto busca o modelo e sua descri‡Æo **/
        /*
        FOR FIRST mab-eqpto FIELDS (cod-eqpto)
            WHERE mab-eqpto.cod-eqpto = tt-selecao.cod-generica NO-LOCK:
            FOR FIRST mab-model FIELDS (cod-model des-model)
                WHERE mab-model.cod-model = tt-selecao.cod-generica NO-LOCK:
               ASSIGN c-generica = mab-model.des-model.
            END.
        END.
        */
        CASE tt-selecao.idi-tip-ord:
            /** Equipamentos **/
            WHEN 1 THEN DO:
                {utp/ut-liter.i Contador * R}
                ASSIGN cl-val-generica = TRIM(RETURN-VALUE).
                FOR FIRST mab-eqpto 
                    WHERE mab-eqpto.ep-codigo = tt-selecao.ep-codigo
                    AND   mab-eqpto.cod-eqpto = tt-selecao.cod-generica NO-LOCK:
                END.
                IF AVAIL mab-eqpto THEN DO:
                    FOR FIRST mab-eqpto FIELDS (cod-eqpto cod-model)
                    WHERE mab-eqpto.cod-eqpto = tt-selecao.cod-generica NO-LOCK:
                        FOR FIRST mab-model FIELDS (cod-model des-model un)     
                            WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
                           ASSIGN c-generica = mab-model.des-model.
                        END.
                    END. 
                    /* ASSIGN c-generica = mab-eqpto.cod-model. */
                END.
                /** Altera o cod-generica para apresentar a empresa na impressÆo, caso seja equipamento o tipo da OM **/
                ASSIGN tt-selecao.cod-generica     = STRING(tt-selecao.ep-codigo) + " - " + tt-selecao.cod-generica
                       c-val-generica = STRING(tt-selecao.val-hodom-horim) + " " + mab-model.un.
            END.
            WHEN 2 THEN DO:
                {utp/ut-liter.i Componente * R}
                ASSIGN cl-generica = TRIM(RETURN-VALUE).
                {utp/ut-liter.i SubSistema * R}
                ASSIGN cl-val-generica = TRIM(RETURN-VALUE).
                FOR FIRST mab-sub-sist FIELDS (cod-sub-sist des-sub-sist idi-compon)
                        WHERE mab-sub-sist.cod-sub-sist = tt-selecao.cod-sub-sist NO-LOCK:
                    END. 
                    IF AVAIL mab-sub-sist THEN DO:
                        ASSIGN c-val-generica = tt-selecao.cod-sub-sist + " - " + mab-sub-sist.des-sub-sist.
                IF NOT AVAIL mco-compon THEN DO:
                   FOR FIRST mco-compon
                       WHERE mco-compon.cod-compon = tt-selecao.cod-compon NO-LOCK:
                   END. 
                END.
                IF AVAIL mco-compon THEN DO:
                    ASSIGN c-generica               = mco-compon.des-model
                           tt-selecao.cod-generica  = mco-compon.cod-compon. 
                    END.
                END. 
            END.

            WHEN 3 THEN DO:
                {utp/ut-liter.i Centro-Custo * R }
                ASSIGN cl-generica = TRIM(RETURN-VALUE).
                FOR FIRST centro-custo
                    WHERE centro-custo.cc-codigo = tt-selecao.cc-codigo NO-LOCK:
                END.
                IF AVAIL centro-custo THEN DO:
                    ASSIGN c-generica = centro-custo.descricao.
                END.
                /** Altera o cod-generica para apresentar a empresa na impressÆo, caso seja equipamento o tipo da OM **/
                ASSIGN tt-selecao.cod-generica     = tt-selecao.cc-codigo.
            END.
        END.
            
    
            /** Atualiza variaveis para impressÆp de dados **/
        ASSIGN d-data-abert      = tt-selecao.dat-abert
               c-hora-abert      = IF tt-selecao.hra-abert <> "00:00:00":U 
                                   THEN SUBSTRING(tt-selecao.hra-abert,1,5)  
                                   ELSE ""         /** Hora + Minuto **/
               d-data-entrada    = tt-selecao.dat-entr 
               c-hora-entrada    = IF tt-selecao.hra-entr <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-entr,1,5)
                                   ELSE ""         /** Hora + Minuto **/ 
               d-data-prev       = tt-selecao.dat-prev     /** Ano **/  
               c-hora-prev       = IF tt-selecao.hra-prev <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-prev,1,5) 
                                   ELSE ""         /** Hora + Minuto **/    
               d-data-term       = tt-selecao.dat-term
               c-hora-term       = IF tt-selecao.hra-term <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-term,1,5) 
                                   ELSE ""         /** Hora + Minuto **/ 
               c-tp-om           = {frinc/i00fr072.i 4 tt-selecao.idi-tip-ord} 
               c-estado          = {ininc/i01in271.i 4 tt-selecao.estado}
               c-conta           = tt-selecao.ct-ordem
               cc-custo          = tt-selecao.cc-ordem
               iep-cod-ordem     = tt-selecao.ep-codigo-ordem
               c-cod-generica    = tt-selecao.cod-generica
               i-tp-manut        = tt-selecao.cd-tipo
               c-plane           = tt-selecao.cod-plandor
               i-num-ord         = tt-selecao.nr-ord-produ
               d-dat-criacao     = tt-selecao.dat-criacao 
               c-hr-cricao       = tt-selecao.hra-criacao
               iep-conta-desp    = tt-selecao.ep-codigo-despes
               c-cont-desp       = tt-selecao.ct-codigo
               cc-cond-desp      = tt-selecao.cc-codigo
               c-cod-usuar       = tt-selecao.cod-usuar
               c-cod-estabel     = tt-selecao.cod-estabel.
                                    
               /** Pular linhas a cada trˆs ordens mostradas em tela **/
               IF contador > 2 THEN DO:
                    DISPLAY WITH FRAME f-pula-linha.
                    PAGE.
                    ASSIGN contador = 0.
                END.
                   /** Display na Frame padrÆo **/
                   DISPLAY WITH FRAME fFrameA.
                      DOWN WITH FRAME fFrameA. 
                      ASSIGN contador = contador + 1.
    END.
END. 

ELSE IF tt-param.classifica = 2 THEN DO: /** Classifica‡Æo por Status **/
    FOR EACH tt-selecao BY idi-status-ord:

        ASSIGN c-status = {ydminc/i00ydm001.i 4 tt-selecao.idi-status-ord}
               d-data-status = tt-selecao.data-sit
               c-hora-status = tt-selecao.hora-sit. 
    
        FOR FIRST mmv-ofici FIELDS (cod-ofici des-ofici) 
            WHERE mmv-ofici.cod-ofici = tt-selecao.cod-ofici NO-LOCK:
            ASSIGN cCod-Ofici = mmv-ofici.cod-ofici
                   cDes-Ofici = mmv-ofici.des-ofici.
        END.
        /** Busca a descriÆo da Oficina **/
        FOR FIRST mmv-ofici FIELDS (cod-ofici des-ofici) 
            WHERE mmv-ofici.cod-ofici = tt-selecao.cod-ofici NO-LOCK:
            ASSIGN cCod-Ofici = mmv-ofici.cod-ofici
                   cDes-Ofici = mmv-ofici.des-ofici.
        END.
        /** Busca o nome do planajador **/
        FOR FIRST mmv-plandor FIELDS (cod-plandor nom-plandor)
            WHERE mmv-plandor.cod-plandor = tt-selecao.cod-plandor NO-LOCK:
           ASSIGN c-nome = mmv-plandor.nom-plandor.
        END.
        /** Busca para o tipo de manuten‡Æo **/
        FOR FIRST tipo-manut FIELDS (cd-tipo descricao)
            WHERE tipo-manut.cd-tipo = tt-selecao.cd-tipo NO-LOCK:
           ASSIGN c-descricao = tipo-manut.descricao.
        END.
        /** Pelo C¢digo do equipto busca o modelo e sua descri‡Æo **/
        /*
        FOR FIRST mab-eqpto FIELDS (cod-eqpto)
            WHERE mab-eqpto.cod-eqpto = tt-selecao.cod-eqpto NO-LOCK:
            FOR FIRST mab-model FIELDS (cod-model des-model)
                WHERE mab-model.cod-model = tt-selecao.cod-eqpto NO-LOCK:
               ASSIGN c-model = mab-model.des-model.
            END.
        END.
        */
        CASE tt-selecao.idi-tip-ord:
            /** Equipamentos **/
            WHEN 1 THEN DO:
                {utp/ut-liter.i Contador * R}
                ASSIGN cl-val-generica = TRIM(RETURN-VALUE).
                FOR FIRST mab-eqpto 
                    WHERE mab-eqpto.ep-codigo = tt-selecao.ep-codigo
                    AND   mab-eqpto.cod-eqpto = tt-selecao.cod-generica NO-LOCK:
                END.
                IF AVAIL mab-eqpto THEN DO:
                    FOR FIRST mab-eqpto FIELDS (cod-eqpto cod-model)
                    WHERE mab-eqpto.cod-eqpto = tt-selecao.cod-generica NO-LOCK:
                        FOR FIRST mab-model FIELDS (cod-model des-model un)     
                            WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
                           ASSIGN c-generica = mab-model.des-model.
                        END.
                    END. 
                    /* ASSIGN c-generica = mab-eqpto.cod-model. */
                END.
                /** Altera o cod-generica para apresentar a empresa na impressÆo, caso seja equipamento o tipo da OM **/
                ASSIGN tt-selecao.cod-generica     = STRING(tt-selecao.ep-codigo) + " - " + tt-selecao.cod-generica
                       c-val-generica = STRING(tt-selecao.val-hodom-horim) + " " + mab-model.un.
            END.
            WHEN 2 THEN DO:
                {utp/ut-liter.i Componente * R}
                ASSIGN cl-generica = TRIM(RETURN-VALUE).
                {utp/ut-liter.i SubSistema * R}
                ASSIGN cl-val-generica = TRIM(RETURN-VALUE).
                FOR FIRST mab-sub-sist FIELDS (cod-sub-sist des-sub-sist idi-compon)
                        WHERE mab-sub-sist.cod-sub-sist = tt-selecao.cod-sub-sist NO-LOCK:
                    END. 
                    IF AVAIL mab-sub-sist THEN DO:
                        ASSIGN c-val-generica = tt-selecao.cod-sub-sist + " - " + mab-sub-sist.des-sub-sist.
                IF NOT AVAIL mco-compon THEN DO:
                   FOR FIRST mco-compon
                       WHERE mco-compon.cod-compon = tt-selecao.cod-compon NO-LOCK:
                   END. 
                END.
                IF AVAIL mco-compon THEN DO:
                    ASSIGN c-generica               = mco-compon.des-model
                           tt-selecao.cod-generica  = mco-compon.cod-compon. 
                    END.
                END. 
            END.

            WHEN 3 THEN DO:
                {utp/ut-liter.i Centro-Custo * R }
                ASSIGN cl-generica = TRIM(RETURN-VALUE).
                FOR FIRST centro-custo
                    WHERE centro-custo.cc-codigo = tt-selecao.cc-codigo NO-LOCK:
                END.
                IF AVAIL centro-custo THEN DO:
                    ASSIGN c-generica = centro-custo.descricao.
                END.
                /** Altera o cod-generica para apresentar a empresa na impressÆo, caso seja equipamento o tipo da OM **/
                ASSIGN tt-selecao.cod-generica     = tt-selecao.cc-codigo.
            END.
        END.
    
            /** Atualiza variaveis para impressÆo de dados **/
        ASSIGN d-data-abert      = tt-selecao.dat-abert
               c-hora-abert      = IF tt-selecao.hra-abert <> "00:00:00":U 
                                   THEN SUBSTRING(tt-selecao.hra-abert,1,5)  
                                   ELSE ""         /** Hora + Minuto **/
               d-data-entrada    = tt-selecao.dat-entr 
               c-hora-entrada    = IF tt-selecao.hra-entr <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-entr,1,5)
                                   ELSE ""         /** Hora + Minuto **/ 
               d-data-prev       = tt-selecao.dat-prev     /** Ano **/  
               c-hora-prev       = IF tt-selecao.hra-prev <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-prev,1,5) 
                                   ELSE ""         /** Hora + Minuto **/    
               d-data-term       = tt-selecao.dat-term
               c-hora-term       = IF tt-selecao.hra-term <> "" 
                                   THEN SUBSTRING(tt-selecao.hra-term,1,5) 
                                   ELSE ""         /** Hora + Minuto **/ 
               c-tp-om           = {frinc/i00fr072.i 4 tt-selecao.idi-tip-ord} 
               c-estado          = {ininc/i01in271.i 4 tt-selecao.estado}
               c-conta           = tt-selecao.ct-ordem
               cc-custo          = tt-selecao.cc-ordem
               iep-cod-ordem     = tt-selecao.ep-codigo-ordem
               c-cod-generica    = tt-selecao.cod-generica
               i-tp-manut        = tt-selecao.cd-tipo
               c-plane           = tt-selecao.cod-plandor
               i-num-ord         = tt-selecao.nr-ord-produ
               d-dat-criacao     = tt-selecao.dat-criacao 
               c-hr-cricao       = tt-selecao.hra-criacao
               iep-conta-desp    = tt-selecao.ep-codigo-despes
               c-cont-desp       = tt-selecao.ct-codigo
               cc-cond-desp      = tt-selecao.cc-codigo
               c-cod-usuar       = tt-selecao.cod-usuar
               c-cod-estabel     = tt-selecao.cod-estabel.
                /** Pular linhas a cada trˆs ordens mostradas em tela **/                          
                IF contador > 2 THEN DO:
                    DISPLAY WITH FRAME f-pula-linha.
                    PAGE.
                    ASSIGN contador = 0.
                END.
                   /** Display na Frame padrÆo **/
                   DISPLAY WITH FRAME fFrameA.
                      DOWN WITH FRAME fFrameA. 
                      ASSIGN contador = contador + 1.
    END.
END.
 
    
/** Mostra parƒmetros selecionados **/
run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

