/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Modulo.................: EYM - Especificos Yamana                                        |
|  Programa...............: bodi135cancel0-u01.P                                            |
|  Sub Programa...........:                                                                 |
|  Descricao..............: EPC NA BO DE CANCELAMENTO DA NF                                 |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 08|08|2008 | Wellington Ap       | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao : Programa ira gera o Layout de cancelamento da Nofa-Fiscal Eletronica   |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |

\  --------------------------------------------------------------------------------------- */
 {include/i-prgvrs.i bodi135cancel-U01 2.06.00.000} 

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO TEMP-TBLE                                      *| 
\* --------------------------------------------------------------------------------------- */                      
{include/i-epc200.i  bodi135cancel}
{include/i-epc200.i2 bodi135cancel}

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO PARAMETROS                                     *| 
\* --------------------------------------------------------------------------------------- */                      
DEFINE INPUT PARAM  p-ind-event  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE  FOR tt-epc.
DEFINE INPUT-OUTPUT PARAM l-erro AS LOGICAL.

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO VARIAVEIS GLOBAIS                              *| 
\* --------------------------------------------------------------------------------------- */                      

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO VARIAVEIS LOCAIS                               *| 
\* --------------------------------------------------------------------------------------- */                      
DEFINE VARIABLE r-nota-fiscal                                 AS ROWID              NO-UNDO.
DEFINE VARIABLE h-bodi135cancel                               AS HANDLE             NO-UNDO.
DEFINE VARIABLE c-lista-prog                                  AS CHAR               NO-UNDO.
DEFINE VARIABLE i-cont                                        AS INTEGER            NO-UNDO.
DEFINE VARIABLE c-chave-origem                                AS CHARACTER          NO-UNDO.
/* --------------------------------------------------------------------------------------- *|
|*                                    DEFINI€ÇOÃBUFFER                                     *| 
\* --------------------------------------------------------------------------------------- */                      
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

/* --------------------------------------------------------------------------------------- *|
|*                                     INICIO PROCESSO                                     *| 
\* --------------------------------------------------------------------------------------- */                      

/* RUN pi-msg.     */

/* ---> VALIDADOR DE PROGRAMA CHAMADOR <--- */
ASSIGN i-cont = 1.

REPEAT:
  IF PROGRAM-NAME(i-cont) = ? THEN LEAVE.
  ASSIGN
    c-lista-prog = c-lista-prog + (IF c-lista-prog <> "" THEN "," ELSE "") + PROGRAM-NAME(i-cont)
    i-cont = i-cont + 1.
END.

/* ---> PROCESSAMENTO DE INFORMA€OES  <--- */
/* IF c-lista-prog MATCHES "*ft2200*" THEN DO: */
  
  FOR EACH tt-epc
    WHERE tt-epc.cod-event = "Fim_CancelaNotaFiscal"
    AND   tt-epc.cod-parameter = "notafiscal-rowid":
      
    FIND FIRST nota-fiscal NO-LOCK
      WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
    IF AVAIL nota-fiscal THEN DO:

      IF nota-fiscal.dt-cancela = ? THEN NEXT.
      
      RUN pi-layout-nfe-canc.
      
    END.
  END. 
/* END. */

PROCEDURE pi-layout-nfe-canc:
    
  {esp\variaveis-calc.i}

  FIND FIRST ext-diretorios-nfe NO-LOCK NO-ERROR.
  if  not avail ext-diretorios-nfe then do:
      run utp/ut-msgs.p(input "show", input 17006, input "Tabela Ext-Diretorios-NFE não parametrizada.").
      return.
  end.

  ASSIGN c-diretorio = REPLACE(ext-diretorios-nfe.dir-entrada-canc,"/","\")
         c-arquivo   = STRING(nota-fiscal.cod-estabel) + "_" +     /* ---> Estabelecimento  <--- */
                       STRING(nota-fiscal.serie)       + "_" +     /* ---> Serie  <--- */
                       STRING(nota-fiscal.nr-nota-fis, "9999999"). /* ---> Num. Nota Fiscal <--- */

  FIND FIRST estabelec        OF nota-fiscal NO-LOCK NO-ERROR.
  FIND FIRST natur-oper       OF nota-fiscal NO-LOCK NO-ERROR.

  /* ---> Grava CNPJ do Emitente da Nota <--- */
  ASSIGN  c-cnpj-emit = estabelec.cgc .

  /* ---> Valida Tipo da Nota 0 - Entrada / 1 - Saida <--- */
  CASE natur-oper.tipo:
    WHEN 1 THEN
      ASSIGN c-origem = '0'.
    WHEN 2 THEN
      ASSIGN c-origem = '1'.
    WHEN 3 THEN
      ASSIGN c-origem = '1'.
  END CASE.

  /* ----------> Cabecalho <---------- */
  hXML:CREATE-NODE(hRoot, "infNFe", "ELEMENT":U).
  hXML:APPEND-CHILD(hRoot).
  hRoot:SET-ATTRIBUTE ("versao", "1.01").

  ASSIGN 
    c-dtemi = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999") + "-" + 
              STRING(MONTH(nota-fiscal.dt-emis-nota),"99")  + "-" + 
              STRING(DAY(nota-fiscal.dt-emis-nota),"99")
    c-chave-origem = STRING(nota-fiscal.cod-estabel) + "_" + 
                     STRING(nota-fiscal.serie) + "_" + 
                     STRING(nota-fiscal.nr-nota-fis, "9999999"). 
    
  ASSIGN 
    c-lista = "CNPJ|mod|serie|nNF|dEmi|tpNF|chaveOrigem"
    c-valor = c-cnpj-emit                                + '|' +
              "55"                                       + '|' + 
              nota-fiscal.serie                          + '|' + 
              STRING(INT(nota-fiscal.nr-nota-fis))       + '|' +
              c-dtemi                                    + '|' +
              c-origem                                   + '|' +
              c-chave-origem                             + '|'   
    i-cont  = 1.

    REPEAT i-cont = 1 to 7:
      hXML:CREATE-NODE(hRecordAux, ENTRY(i-cont,c-lista,'|'), "ELEMENT":U).
      hRoot:APPEND-CHILD(hRecordAux).

      hXML:CREATE-NODE(hRecordAuxValue, "infNFe":U, "Text":U).
      hRecordAux:APPEND-CHILD(hRecordAuxValue).
      hRecordAuxValue:NODE-VALUE = ENTRY(i-cont,c-valor,'|').
    END.

    /* ---> SALVA ARQUIVO COMO XML <--- */
    hXML:SAVE("FILE":U, c-diretorio + c-arquivo + "_CAN.xml.txt").
END PROCEDURE.

/* ---> MSG DE ACOMPANHAMENTO DE PONTOS EPC <--- */
PROCEDURE pi-msg:
    for each tt-epc:
        message tt-epc.cod-event     skip
                tt-epc.cod-parameter skip
                tt-epc.val-parameter
            view-as alert-box info buttons ok.
    end.
END PROCEDURE.
