/*-------------------------------------------------------------------------------
Programa: esp-esmv0616b.p
Objetivo: Gerenciador de Indicadores
Autor:    Joao B. C. Bisneto - DSC
Cliente:  Yamana Gold
-------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcel       AS COM-HANDLE.
DEFINE VARIABLE chWorkbook    AS COM-HANDLE.
DEFINE VARIABLE chWorksheet   AS COM-HANDLE.
/*-------------------------------------------------------------------------------*/
DEFINE NEW GLOBAL SHARED VARIABLE i-ep-codigo-usuario LIKE mguni.empresa.ep-codigo NO-UNDO.
/*-------------------------------------------------------------------------------*/
DEFINE VARIABLE ch-Excel      AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ch-Planilha   AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE ch-Arquivo    AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE h-acomp       AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-utils       as handle      NO-UNDO.
DEFINE VARIABLE dataAux       AS DATE        NO-UNDO.
DEFINE variable i-lin         as integer     no-undo.
DEFINE variable i-lin-ini     as integer     no-undo.
DEFINE variable i-lin-fim     as integer     no-undo.
define variable i-conta-col   as integer     no-undo.
/*-------------------------------------------------------------------------------*/
DEFINE VARIABLE c-alfabeto    AS CHAR 
  init "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,as" 
  NO-UNDO.
/*-------------------------------------------------------------------------------*/
DEF TEMP-TABLE tt-disp-dia
  NO-UNDO
  FIELD cd-equipto LIKE equipto.cd-equipto
  FIELD data       LIKE data-turno-calen.data
  FIELD disponib   AS DEC
  INDEX idx01 AS PRIMARY UNIQUE cd-equipto data.
DEF TEMP-TABLE tt-totais-eqpto
  LIKE tt-disp-dia.
DEF TEMP-TABLE tt-tag-manutencao 
  NO-UNDO 
  LIKE tag
  FIELD tt-disponibilidade AS DEC.
DEF TEMP-TABLE tt-eqpto NO-UNDO
  FIELD cod-eqpto  LIKE mab-eqpto.cod-eqpto
  FIELD cd-tag     LIKE mab-eqpto.cd-tag
  FIELD descricao  LIKE mab-model.des-model
  FIELD dispo-acum AS DECIMAL FORMAT ">>9.99"
  INDEX id IS PRIMARY UNIQUE cod-eqpto.
DEF TEMP-TABLE tt-equipamento 
  NO-UNDO 
  LIKE equipto
  FIELD tt-pmpl            AS DEC
  FIELD tt-mttr            AS DEC
  FIELD tt-mtbf            AS DEC
  FIELD tt-disponibilidade AS DEC.
DEF TEMP-TABLE tt-pai NO-UNDO
  FIELD codigo     LIKE mab-eqpto.cd-tag
  FIELD descricao  LIKE mab-model.des-model
  FIELD dispo-acum AS DECIMAL FORMAT ">>9.99"
  INDEX id IS PRIMARY UNIQUE codigo.
/*-------------------------------------------------------------------------------*/
{mvp\esp-esmv0616.i1}
/*-------------------------------------------------------------------------------*/
DEFINE BUFFER btt-equipamento    FOR tt-equipamento.
DEFINE BUFFER btt-tag-manutencao FOR tt-tag-manutencao.
/*-------------------------------------------------------------------------------*/
def input parameter table for ttSelecao.
def input parameter table for tt-equipamento.
def input parameter table for tt-tag-manutencao.
def input parameter table for tt-disp-dia.
DEF INPUT PARAM c-cd-equipto LIKE tt-equipamento.cd-equipto NO-UNDO.
/*-------------------------------------------------------------------------------*/
FIND FIRST ttSelecao NO-LOCK NO-ERROR.
IF NOT AVAIL ttSelecao THEN RETURN "NOK":U.
/*-------------------------------------------------------------------------------*/
RUN piExcel.
/*-------------------------------------------------------------------------------*/
PROCEDURE piExcel :
    DEF VAR l-ok        AS LOG        NO-UNDO. 
    DEF VAR c-range     AS CHAR       NO-UNDO.
    DEF VAR i-alf       AS INTEGER    NO-UNDO.
    DEF VAR i-alf2      AS INTEGER    NO-UNDO.
    DEF VAR i-colu      AS INTEGER    NO-UNDO.
    DEF VAR lin-Inicial AS INTEGER    NO-UNDO.
    DEF VAR lin-Final   AS INTEGER    NO-UNDO.
    DEF VAR cDescEqpto  AS CHARACTER  NO-UNDO.
    DEF VAR cDescTagMod AS CHARACTER  NO-UNDO.

    run utp/ut-utils.p persistent set h-utils.
    /** inicia tela de acompanhamento **/
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Exporta‡Æo").
    run pi-acompanhar  in h-acomp (input "Carregando..").

    repeat i-alf = 1 TO 26:
        repeat i-alf2 = 1 TO 26:
            assign c-alfabeto = c-alfabeto + "," + entry(i-alf,c-alfabeto,",") + entry(i-alf2,c-alfabeto,",").
        end.
    end.

    assign chExcel     = ?
           chWorkbook  = ?
           chWorksheet = ?.

    create "Excel.Application" chExcel.

    chWorkbook  = chExcel:Workbooks:open(search("mvp/esmv0616.xls")).
    chWorksheet = chWorkbook:Sheets:item(1).
    chWorksheet:Activate().

    chWorkSheet:Cells(1,3):value  = "Dia".
    chWorkSheet:Cells(41,3):value = "Dia".
 
    chWorkSheet:Cells(2,3):value  = "Disponibilidade Di ria".
    chWorkSheet:Cells(42,3):value = "Disponibilidade Di ria".                 
  
    chWorkSheet:Cells(1,4):value  = "ACUM.".
    chWorkSheet:Cells(41,4):value = "ACUM.".
    
    /** Disponibilidade Acumuluda **/
    FOR FIRST tt-equipamento
      NO-LOCK
      WHERE tt-equipamento.cd-equipto = c-cd-equipto,
      FIRST tt-tag-manutencao
      NO-LOCK
      WHERE tt-tag-manutencao.cd-tag = tt-equipamento.cd-tag:
        ASSIGN
          chWorkSheet:Cells(2,4):VALUE = tt-tag-manutencao.tt-disponibilidade
          cDescTagMod = tt-tag-manutencao.descricao.
        RUN piFormato2 (INPUT 2, INPUT 4, INPUT 10 /* bfttDados.dispo */ ).
      END.
    FOR FIRST tt-equipamento
      WHERE tt-equipamento.cd-equipto = c-cd-equipto:
        ASSIGN
          chWorkSheet:Cells(3,4):value = tt-equipamento.tt-disponibilidade
          cDescEqpto = tt-equipamento.descricao.
        RUN piFormato2 in this-procedure (input 3, input 4, input tt-equipamento.tt-disponibilidade).
      END.    
    /** Imprime cabe‡alho das Data **/
    assign i-colu = 4.
    repeat dataAux = ttSelecao.periodo-ini to ttSelecao.periodo-fim:
        assign i-colu = i-colu + 1
               chWorkSheet:Cells(1,i-colu):value  = dataAux
               chWorkSheet:Cells(41,i-colu):value = dataAux.
    end.
    
    /** Pinta Linhas das Datas **/
    assign chWorksheet:Range(entry(3,c-alfabeto,",") + "1" + ":" + entry(i-colu,c-alfabeto,",") + "1"):Interior:ColorIndex = 48
           chWorksheet:Range(entry(3,c-alfabeto,",") + "1" + ":" + entry(i-colu,c-alfabeto,",") + "1"):Font:ColorIndex = 1.

    assign chWorksheet:Range(entry(3,c-alfabeto,",") + "41" + ":" + entry(i-colu,c-alfabeto,",") + "41"):Interior:ColorIndex = 48
           chWorksheet:Range(entry(3,c-alfabeto,",") + "41" + ":" + entry(i-colu,c-alfabeto,",") + "41"):Font:ColorIndex = 1.
    
    /** Imprime TAG que ‚ informado no Grafico **/
    assign chWorkSheet:Cells(2,1):value = 1 /** Dimensao 1 **/
           chWorkSheet:Cells(2,2):value = tt-tag-manutencao.cd-tag + " - " + cDescTagMod
           i-colu = 4.

    FOR EACH tt-totais-eqpto: DELETE tt-totais-eqpto. END.
    FOR EACH btt-equipamento
      WHERE btt-equipamento.cd-tag = tt-tag-manutencao.cd-tag:
      FOR EACH tt-disp-dia
        NO-LOCK
        WHERE tt-disp-dia.cd-equipto = btt-equipamento.cd-equipto
        BY tt-disp-dia.data:
        FIND tt-totais-eqpto
          WHERE tt-totais-eqpto.data = tt-disp-dia.data
          NO-ERROR.
        IF NOT AVAIL tt-totais-eqpto THEN
          DO:
            CREATE tt-totais-eqpto.
            ASSIGN
              tt-totais-eqpto.cd-equipto = ""
              tt-totais-eqpto.data       = tt-disp-dia.data.             
          END.
        ASSIGN tt-totais-eqpto.disponib = tt-totais-eqpto.disponib + tt-disp-dia.disponib.
      END.
    END.
    ASSIGN i-conta-col = 4.
    FOR EACH tt-totais-eqpto
      NO-LOCK
      /* WHERE tt-totais-eqpto.cd-equipto = tt-equipamento.cd-equipto */
      BY tt-totais-eqpto.data:
        assign
          i-conta-col = i-conta-col + 1
          chWorkSheet:Cells(2,i-conta-col):value = tt-totais-eqpto.disponib.
        run piFormato2 in this-procedure (input 2, input i-conta-col, input tt-totais-eqpto.disponib).
    END.
    
    /** Imprime Eqpto que ‚ informado no Grafico **/
    assign chWorkSheet:Cells(3,1):value = 2 /** Dimensao 2 **/
           chWorkSheet:Cells(3,2):value = tt-equipamento.cd-equipto + " - " + cDescEqpto.
           i-colu = 4.

    ASSIGN i-conta-col = 4.
    FOR EACH tt-disp-dia
      NO-LOCK
      WHERE tt-disp-dia.cd-equipto = tt-equipamento.cd-equipto
      BY tt-disp-dia.data:
        assign
          i-conta-col = i-conta-col + 1
          chWorkSheet:Cells(3,i-conta-col):value = tt-disp-dia.disponib.
        run piFormato2 in this-procedure (input 3, input i-conta-col, input tt-disp-dia.disponib).
    END.
    
   /** Imprime todas as visoes mostrandos suas disponibilidades diaria **/
    assign i-lin       = 41
           i-conta-col = 4.

    FOR EACH btt-tag-manutencao 
      BY btt-tag-manutencao.cd-tag:
        ASSIGN 
          i-lin       = i-lin + 1
          i-conta-col = 4
          lin-Inicial = i-lin + 1
          chWorkSheet:Cells(i-lin,1):value = 1
          chWorkSheet:Cells(i-lin,2):value = btt-tag-manutencao.cd-tag + " @-@ " + btt-tag-manutencao.descricao
          chWorkSheet:Cells(i-lin,4):value = btt-tag-manutencao.tt-disponibilidade.
        run piFormato2 in this-procedure (input i-lin, input 4, input btt-tag-manutencao.tt-disponibilidade).
        /** Imprime Disponibilidade do Tag dia a dia **/

        RUN pi-imprime-totais.

        /** Imprime os Eqptos do TAG **/
        ASSIGN
          i-lin-ini = i-lin + 1.
          i-lin-fim = 0.
        for each tt-equipamento
            where tt-equipamento.cd-tag  = btt-tag-manutencao.cd-tag:
            assign i-lin = i-lin + 1
                   i-lin-fim   = i-lin
                   i-conta-col = 4
                   chWorkSheet:Cells(i-lin,1):value = 2
                   chWorkSheet:Cells(i-lin,2):value = tt-equipamento.cd-equipto + " - " + tt-equipamento.descricao
                   chWorkSheet:Cells(i-lin,4):value = tt-equipamento.tt-disponibilidade.
            run piFormato2 in this-procedure (input i-lin, input 4, input tt-equipamento.tt-disponibilidade).
            /** Imprime Disponibilidade do Eqpto dia a dia **/

            run pi-acompanhar in h-acomp (input "Equipamento: " + tt-equipamento.cd-equipto).
            ASSIGN i-conta-col = 4.
            FOR EACH tt-disp-dia 
              NO-LOCK
              WHERE tt-disp-dia.cd-equipto = tt-equipamento.cd-equipto
              BY tt-disp-dia.data:
              assign 
                i-conta-col = i-conta-col + 1
                chWorkSheet:Cells(i-lin,i-conta-col):value = tt-disp-dia.disponib.
                run piFormato2 in this-procedure (input i-lin, input i-conta-col, input tt-disp-dia.disponib).
              IF i-conta-col > 31 THEN LEAVE.
            END.
        end.
        assign l-ok = chWorksheet:Range("A" + TRIM(STRING(i-lin-ini))  + ":A" + TRIM(STRING(i-lin-fim)) ):group(1) no-error.
        assign l-ok = chWorksheet:OutLine:ShowLevels(1,0).
      END.

    /** Acerta tamanho das colunas **/
    if c-range = "" then
        assign c-range = entry(1,c-alfabeto,",") + ":" + entry(256,c-alfabeto,",").
    chWorksheet:columns(c-range):EntireColumn:AutoFit no-error.

    /** Ocultando linhas de grade **/
    chExcel:ActiveWindow:DisplayGridlines = False.

    /** Posiciona na primeira celula **/
    chWorksheet:Range("A1"):Select().

    /*chWorksheet:Save.        */
    chExcel:VISIBLE = TRUE.  
    /*chExcel:WindowState = 3. */

    /** encerra tela de acompanhamento **/
    run pi-finalizar in h-acomp. 

    /* --- Release all the com handles --- */
    if chWorksheet <> ? then
        release object chWorksheet.

    if chWorkbook <> ? then
        release object chWorkbook.

    if chExcel <> ? then
        release object chExcel.

    session:set-wait-state ("").
    
RETURN "OK":U.

END PROCEDURE.
PROCEDURE piFormato :
    define input param pLin  as integer no-undo.
    define input param pCol  as integer no-undo.
    define input param pDisp as decimal no-undo.
    
    if pDisp <> 100 then do:
        chWorkSheet:Range(entry(pCol,c-alfabeto) + string(pLin)):select.
        chExcel:Selection:NumberFormat = "#,##0.00".
    end.
    
    return "OK":U.

END PROCEDURE.
PROCEDURE piFormato2 :
    define input param pLin  as integer no-undo.
    define input param pCol  as integer no-undo.
    define input param pDisp as decimal no-undo.
    
    if pDisp <> 100 then do:
        chWorkSheet:Range(entry(pCol,c-alfabeto) + string(pLin)):select.
        chExcel:Selection:NumberFormat = "###0,00".
    end.
    
    return "OK":U.

END PROCEDURE.

PROCEDURE pi-imprime-totais:
  DEF VAR i-col-tot AS INT NO-UNDO.
  FOR EACH tt-totais-eqpto: DELETE tt-totais-eqpto. END.
  FOR EACH btt-equipamento
    WHERE btt-equipamento.cd-tag = btt-tag-manutencao.cd-tag:
    FOR EACH tt-disp-dia
      NO-LOCK
      WHERE tt-disp-dia.cd-equipto = btt-equipamento.cd-equipto
      BY tt-disp-dia.data:
      FIND tt-totais-eqpto
        WHERE tt-totais-eqpto.data = tt-disp-dia.data
        NO-ERROR.
      IF NOT AVAIL tt-totais-eqpto THEN
        DO:
          CREATE tt-totais-eqpto.
          ASSIGN
            tt-totais-eqpto.cd-equipto = ""
            tt-totais-eqpto.data       = tt-disp-dia.data.             
        END.
      ASSIGN tt-totais-eqpto.disponib = tt-totais-eqpto.disponib + tt-disp-dia.disponib.
    END.
  END.
  ASSIGN i-col-tot = 4.
  FOR EACH tt-totais-eqpto
    NO-LOCK
    BY tt-totais-eqpto.data:
      assign
        i-col-tot = i-col-tot + 1
        chWorkSheet:Cells(i-lin,i-col-tot):value = tt-totais-eqpto.disponib.
      run piFormato2 in this-procedure (input i-lin, input i-col-tot, input tt-totais-eqpto.disponib).
  END.
END PROCEDURE.
