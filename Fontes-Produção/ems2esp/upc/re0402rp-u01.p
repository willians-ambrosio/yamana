/*-----------------------------------------------------------------------------------
    PROGRAMA : re0402rp-u01.P
    OBJETIVO : UPC de chamada para o programa re0402rp
    AUTOR    : Wellington Aparecido (DSC)
    DATA     : 04/09/2008
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i re0402rp-u00 2.06.00.000}
{include/i-epc200.i re0402rp} 

/* ************************************************************************************
                                     PARAMETROS
************************************************************************************ */
DEFINE INPUT PARAM c-ponto AS  CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/
def var c-cabec1     as char format "X(132)".
def var c-cabec2     as char format "X(132)".          
def var c-textolabel as char no-undo.
def var l-erro-nota  as log initial no no-undo.
assign c-cabec1 = "".
DEFINE VARIABLE c-lista-prog                                  AS CHAR               NO-UNDO.
DEFINE VARIABLE i-cont                                        AS INTEGER            NO-UNDO.
DEFINE VARIABLE c-chave-origem                                AS CHARACTER          NO-UNDO.


        
{utp/ut-field.i mgind docum-est serie-docto 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,5,"character") + " ".

{utp/ut-field.i mgind docum-est nro-docto 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,16,"character") + " ".

{utp/ut-field.i mgind docum-est cod-emitente 1}    
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,8,"character") + "   ".

{utp/ut-field.i mgind docum-est nat-operacao 1}
assign c-textolabel = trim(return-value) + "                              ".
assign c-cabec1 = c-cabec1 + substring(c-textolabel,1,8,"character") + "  ".

{utp/ut-liter.i Erro * R}        
assign c-cabec1 = c-cabec1 + trim(return-value) + "    ".
{utp/ut-liter.i Mensagem * R}        
assign c-cabec1 = c-cabec1 + trim(return-value).
assign c-cabec2 = "----- ---------------- ---------  --------  ------  --------------------------------------------------------------------------------".


/* RUN pi-msg. */

IF c-ponto = "Fim-Atualizacao" THEN DO:
  /* ---> L½gica <--- */
  FIND FIRST tt-epc
    WHERE    tt-epc.cod-event     = c-ponto
    AND      tt-epc.cod-parameter = "docum-est-rowid"  NO-ERROR.

  IF AVAILABLE tt-epc THEN DO:
    FOR FIRST docum-est NO-LOCK
      WHERE ROWID(docum-est) = TO-ROWID(tt-epc.val-parameter):


      FIND FIRST natur-oper OF docum-est NO-LOCK NO-ERROR.
      /* ---> GERAR XML DE NOTAS DE RECEBIMENTO QUE 
                 TEM NOTA FISCAL NO FATURAMENTO     <--- */
      IF AVAIL natur-oper AND natur-oper.imp-nota THEN DO:

        FIND FIRST nota-fiscal NO-LOCK
          WHERE nota-fiscal.cod-estabel = docum-est.cod-estabel
          AND   nota-fiscal.serie       = docum-est.serie
          AND   nota-fiscal.nr-nota-fis = docum-est.nro-docto NO-ERROR.
        IF AVAIL nota-fiscal THEN DO:

          IF nota-fiscal.dt-cancela = ? THEN NEXT.
            RUN pi-layout-nfe-canc.

        END.
      END.
    END.
  END.
END.

PROCEDURE pi-layout-nfe-canc:
    
  {esp\variaveis-calc.i}

  FIND FIRST ext-diretorios-nfe NO-LOCK NO-ERROR.
  if  not avail ext-diretorios-nfe then do:
      run utp/ut-msgs.p(input "show", input 17006, input "Tabela Ext-Diretorios-NFE não parametrizada.").
      return.
  end.

  ASSIGN 
    c-diretorio = REPLACE(ext-diretorios-nfe.dir-entrada-canc,"/","\")
    c-arquivo   = STRING(nota-fiscal.cod-estabel) + "_" + /* ---> Estabelecimento  <--- */
                  STRING(nota-fiscal.serie)       + "_" + /* ---> Serie  <--- */
                  STRING(nota-fiscal.nr-nota-fis, "9999999").        /* ---> Num. Nota Fiscal <--- */

  FIND FIRST estabelec        OF nota-fiscal NO-LOCK NO-ERROR.
  FIND FIRST natur-oper       OF nota-fiscal NO-LOCK NO-ERROR.

  /* ---> Grava CNPJ do Emitente da Nota <--- */
  ASSIGN  c-cnpj-emit = estabelec.cgc .

  /* ---> Valida Tipo da Nota 0 - Entrada / 1 - Saida <--- */
  CASE natur-oper.mercado:
    WHEN 1 THEN ASSIGN c-origem = '0'.
    WHEN 2 THEN ASSIGN c-origem = '1'.
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


RETURN "OK":U.




PROCEDURE pi-msg:

  FOR EACH tt-epc:
    MESSAGE 
      tt-epc.cod-event SKIP
      tt-epc.cod-parameter SKIP
      tt-epc.val-parameter
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE. 
