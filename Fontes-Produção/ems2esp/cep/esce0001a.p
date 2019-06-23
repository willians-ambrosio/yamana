DEFINE TEMP-TABLE tt_requisicao 
   FIELD nr-requisicao  LIKE requisicao.nr-requisicao           
   FIELD nome-abrev     LIKE requisicao.nome-abrev                
   FIELD dt-requisicao  LIKE requisicao.dt-requisicao           
   FIELD nome-usuar     LIKE usuar-mater.nome-usuar             
   FIELD loc-entrega    LIKE requisicao.loc-entrega             
   FIELD estado         LIKE requisicao.estado             
   FIELD cod-estabel    LIKE requisicao.cod-estabel
   INDEX idx-requisicao AS PRIMARY UNIQUE nr-requisicao
   INDEX idx-requisitante nome-abrev
                          nr-requisicao.

DEFINE TEMP-TABLE tt_it-requisicao
   FIELD sequencia      LIKE it-requisicao.sequencia 
   FIELD it-codigo      LIKE it-requisicao.it-codigo   
   FIELD qt-a-atender   LIKE it-requisicao.qt-a-atender
   FIELD preco-unit     LIKE it-requisicao.preco-unit  
   FIELD dt-entrega     LIKE it-requisicao.dt-entrega  
   FIELD cod-depos      LIKE it-requisicao.cod-depos
   FIELD cod-localiz    LIKE it-requisicao.cod-localiz 
   FIELD sc-codigo      LIKE it-requisicao.sc-codigo   
   FIELD ct-codigo      LIKE it-requisicao.ct-codigo
   FIELD conta-contabil LIKE it-requisicao.conta-contabil
   FIELD un             LIKE ITEM.un                   
   FIELD descricao      AS CHAR FORMAT "x(36)"           
   FIELD qtidade-atu    LIKE saldo-estoq.qtidade-atu
   FIELD nr-requisicao  LIKE requisicao.nr-requisicao
   INDEX idx-it-requisicao AS PRIMARY UNIQUE nr-requisicao
                                             sequencia
                                             it-codigo
                                             cod-localiz
                                             cod-depos
                                             conta-contabil.

DEF TEMP-TABLE tt-cabec
  FIELD nr-requisicao       LIKE tt_requisicao.nr-requisicao
  FIELD dt-requisicao       LIKE tt_requisicao.dt-requisicao
  FIELD estado              LIKE tt_requisicao.estado     
  FIELD loc-entrega         LIKE tt_requisicao.loc-entrega
  FIELD nome-abrev          LIKE tt_requisicao.nome-abrev  
  FIELD nome-usuar          LIKE tt_requisicao.nome-usuar.
DEF TEMP-TABLE tt-corpo 
  FIELD sequencia           LIKE tt_it-requisicao.sequencia         
  FIELD it-codigo           LIKE tt_it-requisicao.it-codigo         
  FIELD un                  LIKE tt_it-requisicao.un                
  FIELD qt-a-atender        LIKE tt_it-requisicao.qt-a-atender      
  FIELD preco-unit          LIKE tt_it-requisicao.preco-unit        
  FIELD dt-entrega          LIKE tt_it-requisicao.dt-entrega        
  FIELD descricao           LIKE tt_it-requisicao.descricao         
  FIELD conta-contabil      LIKE tt_it-requisicao.conta-contabil
  FIELD cod-depos           LIKE tt_it-requisicao.cod-depos     
  FIELD cod-localiz         LIKE tt_it-requisicao.cod-localiz   
  FIELD qtidade-atu         LIKE tt_it-requisicao.qtidade-atu.   

DEF INPUT PARAM TABLE FOR tt-cabec.
DEF INPUT PARAM TABLE FOR tt-corpo.

def var AppWord             as com-handle         no-undo.
def var c-arq-new as char                         no-undo.
def var c-arq-pdf as char                         no-undo.
DEF VAR c-valor   AS CHAR   FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR i-cont    AS DEC                          NO-UNDO.
DEFINE VARIABLE ch-estado AS CHARACTER EXTENT 2 INIT ["Aprovada","NÆo Aprovada"] NO-UNDO.
create "Word.Application" AppWord no-error.

IF ERROR-STATUS:ERROR THEN RETURN.

AppWord:VISIBLE = false.
AppWord:DisplayAlerts = False.

AppWord:Documents:open(search("cep\esce0001.dot")) NO-ERROR.
assign c-arq-new = session:temp-directory + "esce0001-" + string(today, "999999") + string(time) + ".doc".
AppWord:ActiveDocument:saveas(c-arq-new).
/*-------------------------------------------------------------------------------------------*/
AppWord:ActiveWindow:ActivePane:View:SeekView = 1.
AppWord:selection:moveUp(5,2).
AppWord:ActiveWindow:ActivePane:View:SeekView = 0.
/*-------------------------------------------------------------------------------------------*/
AppWord:selection:EndKey(6).
FIND FIRST tt-cabec NO-LOCK NO-ERROR.
AppWord:selection:TypeText("N£mero: ").
AppWord:selection:TypeText(STRING(tt-cabec.nr-requisicao)).

c-valor = FILL(" ",10).
AppWord:selection:TypeText(STRING(c-valor)).
AppWord:selection:TypeText("Data de EmissÆo: ").
AppWord:selection:TypeText(STRING(tt-cabec.dt-requisicao)).

c-valor = FILL(" ",10).
AppWord:selection:TypeText(STRING(c-valor)).
AppWord:selection:TypeText("Estado: ").
AppWord:selection:TypeText(STRING(ch-estado[tt-cabec.estado])).

AppWord:selection:TypeText(CHR(10)).
AppWord:selection:TypeText(CHR(10)).

AppWord:selection:TypeText("Local de Entrega: ").
AppWord:selection:TypeText(STRING(tt-cabec.loc-entrega)).

AppWord:selection:TypeText(CHR(10)).
AppWord:selection:TypeText(CHR(10)).

AppWord:selection:TypeText("Requisitante: ").
AppWord:selection:TypeText(STRING(tt-cabec.nome-abrev + " - " + tt-cabec.nome-usuar)).
                                                             
AppWord:selection:TypeText(CHR(10)).
AppWord:selection:TypeText(CHR(10)).
AppWord:selection:TypeText(CHR(10)).

AppWord:selection:TypeText("Seq Item Supr. Descri‡Æo                        Un Qtde a Atender Vlr. a Atender Dt.Entrega Dep Localiz.             Conta Contabil       Qtde em Estoque").
AppWord:selection:TypeText(CHR(10)).
AppWord:selection:TypeText("--- ---------- -------------------------------- -- -------------- -------------- ---------- --- -------------------- -------------------- ---------------").
AppWord:selection:TypeText(CHR(10)).
AppWord:selection:EndKey(6).
ASSIGN i-cont = 0.
FOR EACH tt-corpo
  NO-LOCK
  WHERE tt-corpo.sequencia > 0:
  c-valor = STRING(tt-corpo.sequencia,">>9").
  AppWord:selection:TypeText(STRING(c-valor,"X(05)")).
  AppWord:selection:TypeText(STRING(tt-corpo.it-codigo,"x(10)")).
  AppWord:selection:TypeText(STRING(tt-corpo.descricao,"x(33)")).
  AppWord:selection:TypeText(STRING(tt-corpo.un,"x(03)")).
  c-valor = STRING(tt-corpo.qt-a-atender,">,>>>,>>9.9999").
  AppWord:selection:TypeText(STRING(c-valor,"x(15)")).
  c-valor = STRING(tt-corpo.preco-unit,">,>>>,>>9.9999").
  AppWord:selection:TypeText(STRING(c-valor,"x(14)")).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  IF tt-corpo.dt-entrega <> ? THEN
  c-valor = STRING(tt-corpo.dt-entrega,"99/99/9999").
  AppWord:SELECTION:TypeText(STRING(c-valor)).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  c-valor = STRING(tt-corpo.cod-depos,"x(03)").
  AppWord:selection:TypeText(STRING(c-valor)).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  AppWord:selection:TypeText(STRING(tt-corpo.cod-localiz,"x(20)")).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  AppWord:selection:TypeText(STRING(tt-corpo.conta-contabil,"x(20)")).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  c-valor = FILL(" ",1).
  AppWord:selection:TypeText(STRING(c-valor)).
  c-valor = STRING(tt-corpo.qtidade-atu,">>>,>>>,>>9.99").
  AppWord:selection:TypeText(STRING(c-valor,"x(14)")).
  AppWord:selection:TypeText(CHR(10)).
END.
/*-------------------------------------------------------------------------------------------*/
AppWord:selection:homekey(6).
AppWord:selection:moveDown(5, 4).
AppWord:selection:EndKey(6, 1).
AppWord:selection:ParagraphFormat:Alignment = 3. 
AppWord:ActiveDocument:save.

assign c-arq-pdf = session:temp-directory + string(today, "999999") + string(time) + "escc0001.pdf".

AppWord:ActiveDocument:ExportAsFixedFormat(c-arq-pdf, 17, true,,,,,,,,,,,).

AppWord:ActiveDocument:Close(FALSE).
AppWord:Quit().

IF  VALID-HANDLE(AppWord) THEN
    RELEASE OBJECT AppWord NO-ERROR.

OS-DELETE VALUE(c-arq-new).


