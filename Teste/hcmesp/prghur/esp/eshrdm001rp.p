/***************************************************************************************
** Programa : eshrdm001rp.p
** Objetivo : Relat¢rio da Tabela func_desligto
** Autor    : Joao B. C. Bisneto
** Empresa  : DSC-Praxis
** Data     : 17/03/2016
***************************************************************************************/

{include/i-rpvar.i}

def new shared var c-dest-e-mail as char format "x(400)" no-undo.
def var h-acomp                  as handle               no-undo.
/* def var c-det-cabec              as char format "x(23)"  no-undo. */
def var c-prg-obj                as char                 no-undo.
def var c-prg-vrs                as char                 no-undo.

{prghur\esp\eshrdm001.i}

def temp-table tt-raw-digita
    field raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer raw-digita TO tt-digita.
end.

create tt-param.
raw-transfer raw-param to tt-param.

DEF VAR c-ender-arq AS CHAR NO-UNDO.
DEF VAR c-titulo    AS CHAR NO-UNDO.
def var i-linha     AS INT INIT 4 no-undo.
DEF VAR l-achou     AS LOG        NO-UNDO.

{include/i-rpout.i}
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input string(c-titulo-relat)).
/*----Logica Principal*/
find first param-global no-lock no-error.
RUN pi-processa.
/*----Fim Logica Principal*/
{include/i-rpclo.i}
run pi-finalizar in h-acomp.

return 'ok'.

/*----procedures internas---------------------*/
PROCEDURE pi-processa:
  RUN pi-carrega-dados.
  RUN pi-imprime.
END PROCEDURE.
PROCEDURE pi-carrega-dados:
  /*pi-carrega-dados*/
  FIND FIRST tt-param NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-param THEN RETURN "Error".
  FOR EACH tt-func-desligto: DELETE tt-func-desligto. END.
  FOR EACH func_desligto
    NO-LOCK
    WHERE func_desligto.cdn_empresa      >= tt-param.ini-empresa     
    AND   func_desligto.cdn_empresa      <= tt-param.fim-empresa     
    AND   func_desligto.cdn_estab        >= tt-param.ini-cdn-estab   
    AND   func_desligto.cdn_estab        <= tt-param.fim-cdn-estab   
    AND   func_desligto.cdn_funcionario  >= tt-param.ini-cdn-func    
    AND   func_desligto.cdn_funcionario  <= tt-param.fim-cdn-func:
      CREATE tt-func-desligto.
      BUFFER-COPY func_desligto TO tt-func-desligto.
      ASSIGN tt-func-desligto.l-considera = NO.
    END. /* FOR EACH func_desligto */
  FOR EACH tt-func-desligto
    WHERE tt-func-desligto.usuar_alter >= tt-param.ini-usuar-alter
    AND   tt-func-desligto.usuar_alter <= tt-param.fim-usuar-alter:
      ASSIGN tt-func-desligto.l-considera = YES.
    END. /* FOR EACH tt-func-desligto */
  FOR EACH tt-func-desligto:
    IF tt-func-desligto.l-considera = NO THEN NEXT.
    IF 
      tt-param.l-cta-duplic               = YES AND 
      tt-func-desligto.tem-avaliacao      = NO  THEN
      ASSIGN tt-func-desligto.l-considera = NO.
    IF 
      tt-param.l-sem-avaliac                = YES AND 
      tt-func-desligto.tem-avaliacao        = NO  THEN
      ASSIGN tt-func-desligto.tem-avaliacao = NO.
    END.
END PROCEDURE.
PROCEDURE pi-imprime:
  /* "Base de dados;Tabela;Atributo;Programa;Programa;Data Atualiza‡Æo;Hora Atualiza‡Æo;Evento;". */
  PUT UNFORMATTED
    "Empresa;Estab;Funcion rio;Data Desligamento;Existe Cta Duplic;Usuar Alter;Tem Avalia‡Æo;" SKIP.
  FOR EACH tt-func-desligto
    WHERE  tt-func-desligto.l-considera = YES:
      PUT UNFORMATTED
        tt-func-desligto.cdn_empresa      ";"   
        tt-func-desligto.cdn_estab        ";"
        tt-func-desligto.cdn_funcionario  ";"
        tt-func-desligto.dat_desligto     ";"
        tt-func-desligto.existe-cta       ";"
        tt-func-desligto.usuar_alter      ";"
        tt-func-desligto.tem-avaliacao    ";"
        SKIP.
    END. /* FOR EACH tt-func-desligto */
END PROCEDURE.
/*----fim procedures internas-----------------*/
