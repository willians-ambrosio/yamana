DEFINE INPUT PARAMETER ip-arquivo      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER v-num-reg-lidos AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER ip-opcao AS INTEGER.
DEFINE INPUT PARAMETER ip-destino AS INTEGER.

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.
def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
def new shared var v-cod-tipo-grafico-dv203 as character format "x(30)" label "Tipo Grÿfico" view-as combo-box list-items "<<Nenhum>>","Colunas Agrupadas","Colunas Agrupadas 3D","Colunas 3D","Barras  Agrupadas","Barras  Agrupadas 3D","Linhas","Linhas  Com Marcadores","Linhas  3D","Pizza","Pizza   Explodida","Pizza   3D","Pizza   Explodida 3D","Colunas Cilindricas Agrupadas","Barras  Cilindricas Agrupadas","Colunas Cilindricas 3D","Colunas CËnicas Agrupadas","Barras  CËnicas Agrupadas","Colunas CËnicas 3D","Colunas Piramidais Agrupadas","Barras  Piramidais Agrupadas","Colunas Piramidais 3D" .
def new global shared var v_cod_arq_gerdoc  as char no-undo.

RUN pi-cria-tabela-dinamica-excel.

FUNCTION translate RETURNS CHARACTER (str AS CHAR):
    RETURN str.
END FUNCTION.

Procedure pi-cria-tabela-dinamica-excel:
    

    DEF VAR v-ch-Excel         AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Workbook      AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotCache    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotTable    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Publish       AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-1             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-2             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-3             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-4             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-5             AS COM-HANDLE NO-UNDO.
    DEF VAR v-titulo           as char format 'X(60)'.
    DEF VAR i-cont             as int.
    DEF VAR l-error            as log.

    assign v-titulo = "Relat½rio de Arquivos nos Itens".

    assign v-num-reg-lidos = v-num-reg-lidos + 1. /* Incrementa em 1 para linha de labels*/

    IF NOT VALID-HANDLE(v-ch-Excel)
    then do:
        CREATE "Excel.Application " v-ch-Excel CONNECT NO-ERROR.
        IF ERROR-STATUS:ERROR THEN CREATE "Excel.Application" v-ch-Excel.
    end.

    v-ch-Excel:DisplayAlerts = FALSE.
    v-ch-Excel:VISIBLE = YES.
    if i-num-ped-exec-rpw = 0
        then v-ch-Excel:Workbooks:Open(replace(ip-arquivo,"/","\")).
        else v-ch-Excel:Workbooks:Open(replace(c-dir-spool-servid-exec,"/","\") + "\" + ip-arquivo).
    v-ch-Excel:Sheets(1):NAME = translate("Dados").
    v-ch-Workbook = v-ch-Excel:ActiveWorkbook.


    v-ch-Excel:COLUMNS("K:L"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,00".

    v-ch-Excel:COLUMNS("B:B"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "@".

    v-ch-Excel:Selection:Replace("@@@@@","'").

    v-ch-Excel:COLUMNS("M:M"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
    v-ch-Excel:COLUMNS("R:S"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
    v-ch-Excel:COLUMNS("Z:Z"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
    v-ch-Excel:COLUMNS("AE:AE"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,00".
    v-ch-Excel:COLUMNS("A:AE"):EntireColumn:AUTOFIT().


    v-ch-Excel:COLUMNS("C:C"):SELECT.
    v-ch-Excel:Selection:ColumnWidth = 90.
    v-ch-Excel:Selection:VerticalAlignment = -4160.

                                                                   /*A1:G*/
    v-ch-Excel:ActiveSheet:PivotTableWizard(1,translate("Dados") + "!A1:AE" + string(v-num-reg-lidos),"", v-titulo).
    v-ch-Workbook:ActiveSheet:Cells(1, 1):SELECT.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):ColumnGrand = FALSE.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):RowGrand = FALSE.

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):ORIENTATION = 3 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo Fam­lia na tabela din³mica, pois o mesmo provocou um estouro no limite de pÿginas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Fam­lia selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Fam­lia")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo Item na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Item selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo Libera»’o na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Libera»’o selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera»’o")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo Un na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Un selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo C½digo Complementar na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo C½digo Complementar selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C½digo Complementar")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "N’o foi poss­vel inserir o campo Arq PDF/IIMG Item na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Arq PDF/IIMG Item selecionado para exibi»’o." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri»’o")):ORIENTATION = 4 no-error.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
           MESSAGE "N’o foi poss­vel inserir o campo Descri»’o na tabela din³mica, pois o mesmo provocou um estouro no limite de dados da tabela din³mica do Excel." skip
                   "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Descri»’o selecionado para exibi»’o." view-as alert-box information.
         END.
      END.
    END.


    IF index(v-ch-Excel:Version, "9.") = 0 THEN /* Tem Office >= XP */ 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contar de "   + translate("Descri»’o") + ""):NAME = translate("Conta Descri»’o") no-error. 
ELSE /* Tem Office 2000 */
    /*v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contagem de " + translate("Descri»’o") + ""):NAME = translate("Conta Descri»’o") no-error. */
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Count of " + translate("Descri»’o") + ""):NAME = translate("Conta Descri»’o") no-error. 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Count of "    + translate("Descri»’o") + ""):NAME = translate("Conta Descri»’o") no-error.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Conta Descri»’o")):FUNCTION = 1 no-error.

    v-ch-Workbook:ActiveSheet:NAME = translate("Cenÿrio").
      
    if index(v-ch-Excel:Version, "8.") = 0 and /* Tem Office 2000 */
       integer(v-cod-tipo-grafico-dv203) <> 0 /* Nenhum */
    then do:
        v-ch-Workbook:Charts:ADD().
        v-ch-Workbook:ActiveChart:ChartType = integer(v-cod-tipo-grafico-dv203).
        v-ch-Workbook:ActiveSheet:NAME = translate("Grÿfico").
    end.

    if ip-opcao = 2 /* Microsoft Internet Explorer */
    then do:
        if i-num-ped-exec-rpw <> 0
        then do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cenÿrio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,ip-arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,ip-arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
        end.
        else do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cenÿrio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,ENTRY(1,ip-arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo ,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,ENTRY(1,ip-arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
            if  search(ENTRY(1,ip-arquivo,".") + ".html") <> ?
            then do:
                file-info:file-name = search(ENTRY(1,ip-arquivo,".") + ".html").
                run pi-gera-marcadagua-html(input file-info:file-name).
                if  ip-destino = 3
                then
                    run OpenDocument (input file-info:full-pathname) no-error.
                else message translate("Arquivos gerados com sucesso.") view-as alert-box information.
            end.
        end.
    end.
    if session:set-wait-state("") then.

    if ip-opcao = 1 /* Microsoft Excel */
    then do:
        if i-num-ped-exec-rpw = 0
        then do:
            v-ch-excel:ActiveWorkbook:SaveAs(replace(ip-arquivo,".csv", ".csv"),1,"","",no,no,no) no-error.
            v-ch-excel:quit().
            if  ip-destino = 3
            then do:
                file-info:file-name = search(replace(ip-arquivo,".csv", ".csv")).
                run OpenDocument (input file-info:full-pathname) no-error.
            end.
            else message translate("Arquivos gerados com sucesso.") view-as alert-box information.
        end.
        else
            v-ch-excel:ActiveWorkbook:SaveAs(replace(c-dir-spool-servid-exec,"/","\") + "\" , replace(ip-arquivo,".csv", ".csv"),1,"","",no,no,no) no-error.
    end.

    ASSIGN v_cod_arq_gerdoc = ( IF c-dir-spool-servid-exec <> ""
                                THEN (REPLACE(c-dir-spool-servid-exec,"/","\") + "\" + replace(ip-arquivo,".csv":U,".csv":U))
                                ELSE replace(ip-arquivo,".csv",".csv":U)).


    RELEASE OBJECT v-ch-1 no-error.
    RELEASE OBJECT v-ch-2 no-error.
    RELEASE OBJECT v-ch-3 no-error.
    RELEASE OBJECT v-ch-4 no-error.
    RELEASE OBJECT v-ch-5 no-error.
    RELEASE OBJECT v-ch-Publish  no-error.
    RELEASE OBJECT v-ch-Workbook no-error.
    if ip-opcao = 2 or /* Microsoft Internet Explorer */
       i-num-ped-exec-rpw   <> 0
    then v-ch-excel:quit().
    RELEASE OBJECT v-ch-Excel no-error.

End procedure.

Procedure OpenDocument:
    def input param c-doc as char no-undo.
    def var c-exec        as char no-undo.
    def var h-Inst        as int  no-undo.
    def var c-arq         as char no-undo.

    run ConverteparaNomeDos (input-output c-doc).

    assign c-exec = fill("x",255).

    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if  h-inst >= 0 and h-inst <= 32
    then do:
        message translate("N’o foi encontrada associa»’o do arquivo (.htm)") skip translate("com nenhum software de visualiza»’o. Deseja associar agora?") view-as alert-box question buttons yes-no title translate("Associar arquivo") UPDATE l-associar AS LOGICAL.
        if  l-associar = yes
        then
            run ShellExecuteA (input 0,
                               input "open",
                               input "rundll32.exe",
                               input "shell32.dll,OpenAs_RunDLL "+ c-doc,
                               input "",
                               input 1,
                               output h-inst).

        else
            return.
    end.

    assign c-arq = "'" + string(c-exec) + " " + string(c-doc).
    assign c-arq = replace(c-arq,"'","").

    run WinExec (input c-arq,
                 input 1,
                 output h-inst).

    if  h-inst < 0 or
        h-inst > 32
    then
        return "OK".
    else
    return "NOK".

END PROCEDURE.

PROCEDURE ConverteparaNomeDos:
    def input-output param c-Nome as char no-undo.
    def var iLen   as int  init 255 no-undo.
    def var pShort as memptr.

    repeat:
        set-size(pShort) = iLen.
        run GetShortPathNameA (c-Nome,
                               get-pointer-value(pShort),
                               get-size(pShort),
                               output iLen).
        if get-size(pShort) >= iLen then leave.
        set-size(pShort) = 0.
    end.
    c-Nome = get-string(pShort,1).
END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "shell32.dll":
    define input parameter lpFile as char.
    define input parameter lpDirectory as char.
    define input-output parameter lpResult as char.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":
    define input parameter hwnd as long.
    define input parameter lpOperation as char.
    define input parameter lpFile as char.
    define input parameter lpParameters as char.
    define input parameter lpDirectory as char.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE GetShortPathNameA EXTERNAL "KERNEL32":
    DEF INPUT  PARAM lpszLongPath  AS CHAR NO-UNDO.
    DEF INPUT  PARAM lpszShortPath AS LONG NO-UNDO.
    DEF INPUT  PARAM cchBuffer     AS LONG NO-UNDO.
    DEF RETURN PARAM lenBuffer     AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "KERNEL32":
    define input parameter lpszCmdLine as char.
    define input parameter fuCmdShow as LONG.
    define return parameter nTask as LONG.
END PROCEDURE.
