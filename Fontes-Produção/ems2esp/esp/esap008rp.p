/************************************************************************
* Programa..: ESAP008rp.P 
* Descri‡Æo.: 
* AUTOR.....: Bruno Bertulli (DSC)
* DATA......: 07/08/2013
************************************************************************/

/* include de controle de vers’o */
{include/i-prgvrs.i ESAP008RP 2.06.00.001}

/* prýprocessador para ativar ou n’o a saðda para RTF */
&GLOBAL-DEFINE RTF NO

/* prýprocessador para setar o tamanho da p˜gina */
&SCOPED-DEFINE pagesize 0   
/* defini¯Êo das temp-tables para recebimento de parümetros */

define temp-table tt-param no-undo
    field destino          as integer
    field c-destino        as char 
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field ep-codigo-ini    like his-d-ap.ep-codigo   
    field ep-codigo-fim    like his-d-ap.ep-codigo   
    field cod-estabel-ini  like his-d-ap.cod-estabel 
    field cod-estabel-fim  like his-d-ap.cod-estabel 
    field cod-esp-ini      like his-d-ap.cod-esp     
    field cod-esp-fim      like his-d-ap.cod-esp     
    field serie-ini        like his-d-ap.serie       
    field serie-fim        like his-d-ap.serie       
    field nr-docto-ini     like his-d-ap.nr-docto    
    field nr-docto-fim     like his-d-ap.nr-docto    
    field parcela-ini      like his-d-ap.parcela     
    field parcela-fim      like his-d-ap.parcela     
    field cod-fornec-ini   like his-d-ap.cod-fornec  
    field cod-fornec-fim   like his-d-ap.cod-fornec  
    field dt-his-doc-ini   like his-d-ap.dt-his-doc 
    field dt-his-doc-fim   like his-d-ap.dt-his-doc 
    FIELD l-exibe-log-sis  AS   LOGICAL 
    field sel-usuario      as char format "x(12)"
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parümetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

DEFINE VARIABLE chExcelApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkItem          AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE c-historico         AS CHARACTER   NO-UNDO.

/* ===> Main Block <=== */

FIND FIRST tt-param NO-ERROR.
    
OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.

PUT UNFORMAT
    "Empresa"
    ";Estabelec"
    ";Esp‚cie"
    ";S‚rie"
    ";Documento"
    ";Parcela"
    ";Fornecedor"
    ";Nome Fornec"
    ";Data Hist¢rico"
    ";Seq Hist¢rico"
    ";Hor rio"
    ";Usu rio"
    ";Hist¢rico".
PUT SKIP.

FOR EACH his-d-ap NO-LOCK
    WHERE his-d-ap.ep-codigo    = tt-param.ep-codigo-ini
    AND   his-d-ap.cod-estabel >= tt-param.cod-estabel-ini
    AND   his-d-ap.cod-estabel <= tt-param.cod-estabel-fim
    AND   his-d-ap.cod-esp     >= tt-param.cod-esp-ini
    AND   his-d-ap.cod-esp     <= tt-param.cod-esp-fim
    AND   his-d-ap.serie       >= tt-param.serie-ini
    AND   his-d-ap.serie       <= tt-param.serie-fim
    AND   his-d-ap.nr-docto    >= tt-param.nr-docto-ini
    AND   his-d-ap.nr-docto    <= tt-param.nr-docto-fim
    AND   his-d-ap.parcela     >= tt-param.parcela-ini
    AND   his-d-ap.parcela     <= tt-param.parcela-fim
    AND   his-d-ap.cod-fornec  >= tt-param.cod-fornec-ini
    AND   his-d-ap.cod-fornec  <= tt-param.cod-fornec-fim
    AND   his-d-ap.dt-his-doc  >= tt-param.dt-his-doc-ini
    AND   his-d-ap.dt-his-doc  <= tt-param.dt-his-doc-fim
        BREAK BY his-d-ap.dt-his-doc
              BY his-d-ap.horario:

    IF NOT tt-param.l-exibe-log-sis AND his-d-ap.sequencia > 99999 THEN NEXT.

    ASSIGN c-historico = his-d-ap.historico
           c-historico = replace(c-historico,CHR(10)," ")
           c-historico = replace(c-historico,CHR(11)," ")
           c-historico = replace(c-historico,CHR(12)," ")
           c-historico = replace(c-historico,CHR(13)," ")
           c-historico = replace(c-historico,";"," ")
           c-historico = substring(c-historico,1,500).

    IF tt-param.sel-usuario <> "" THEN DO:
        IF INDEX(c-historico, tt-param.sel-usuario) = 0 THEN NEXT.

        FIND FIRST usuar_mestre NO-LOCK
            WHERE usuar_mestre.cod_usuario = tt-param.sel-usuario NO-ERROR.
    END.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = his-d-ap.cod-fornec NO-ERROR.

    PUT UNFORMAT
        his-d-ap.ep-codigo  
        ";" his-d-ap.cod-estabel
        ";" his-d-ap.cod-esp    
        ";" his-d-ap.serie      
        ";" his-d-ap.nr-docto   
        ";" his-d-ap.parcela    
        ";" his-d-ap.cod-fornec 
        ";" (IF AVAILABLE emitente THEN emitente.nome-emit ELSE "")
        ";" his-d-ap.dt-his-doc 
        ";" his-d-ap.sequencia
        ";" his-d-ap.horario FORMAT "99:99:99"
        ";" (IF AVAILABLE usuar_mestre THEN tt-param.sel-usuario + "-" + usuar_mestre.nom_usuario ELSE tt-param.sel-usuario)
        ";" c-historico.
    PUT SKIP.
END.

c-historico = "".
FOR EACH es-his-tit-ap NO-LOCK
    WHERE es-his-tit-ap.ep-codigo     = tt-param.ep-codigo-ini
    AND   es-his-tit-ap.cod-estabel  >= tt-param.cod-estabel-ini
    AND   es-his-tit-ap.cod-estabel  <= tt-param.cod-estabel-fim
    AND   es-his-tit-ap.cod-esp      >= tt-param.cod-esp-ini
    AND   es-his-tit-ap.cod-esp      <= tt-param.cod-esp-fim
    AND   es-his-tit-ap.serie        >= tt-param.serie-ini
    AND   es-his-tit-ap.serie        <= tt-param.serie-fim
    AND   es-his-tit-ap.nr-docto     >= tt-param.nr-docto-ini
    AND   es-his-tit-ap.nr-docto     <= tt-param.nr-docto-fim
    AND   es-his-tit-ap.parcela      >= tt-param.parcela-ini
    AND   es-his-tit-ap.parcela      <= tt-param.parcela-fim
    AND   es-his-tit-ap.cod-fornec   >= tt-param.cod-fornec-ini
    AND   es-his-tit-ap.cod-fornec   <= tt-param.cod-fornec-fim
    AND   es-his-tit-ap.dt-transacao >= tt-param.dt-his-doc-ini
    AND   es-his-tit-ap.dt-transacao <= tt-param.dt-his-doc-fim:

    ASSIGN c-historico = es-his-tit-ap.hist-alterado
           c-historico = replace(c-historico,CHR(10)," ")
           c-historico = replace(c-historico,CHR(11)," ")
           c-historico = replace(c-historico,CHR(12)," ")
           c-historico = replace(c-historico,CHR(13)," ")
           c-historico = replace(c-historico,";"," ")
           c-historico = substring(c-historico,1,500).

    IF tt-param.sel-usuario <> "" AND es-his-tit-ap.usuario <> tt-param.sel-usuario THEN NEXT.

    FIND FIRST usuar_mestre NO-LOCK
        WHERE usuar_mestre.cod_usuario = es-his-tit-ap.usuario NO-ERROR.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = his-d-ap.cod-fornec NO-ERROR.

    PUT UNFORMAT
            es-his-tit-ap.ep-codigo  
        ";" es-his-tit-ap.cod-estabel
        ";" es-his-tit-ap.cod-esp    
        ";" es-his-tit-ap.serie      
        ";" es-his-tit-ap.nr-docto   
        ";" es-his-tit-ap.parcela    
        ";" es-his-tit-ap.cod-fornec 
        ";" (IF AVAILABLE emitente THEN emitente.nome-emit ELSE "")
        ";" es-his-tit-ap.dt-transacao 
        ";" 
        ";" STRING (es-his-tit-ap.hr-transacao,"HH:MM:SS")
        ";" (IF AVAILABLE usuar_mestre THEN es-his-tit-ap.usuario + "-" + usuar_mestre.nom_usuario ELSE es-his-tit-ap.usuario)
        ";" c-historico.
    PUT SKIP.
END.

OUTPUT CLOSE.

RUN pi-geraexcel (INPUT tt-param.arquivo).

/* ===> Procedures <=== */

PROCEDURE Pi-GeraExcel:
    DEFINE INPUT PARAM p-arquivo AS CHARACTER   NO-UNDO.

    CREATE "Excel.Application" chExcelApplication.
    chExcelApplication:Visible = true.
    chExcelApplication:Workbooks:OpenText(p-arquivo, , , , , , TRUE ).
    chWorkSheet = chExcelApplication:Sheets:Item(1).
/*     chWorkSheet:rows("1:1"):Select().        */
/*     chExcelApplication:selection:INSERT(1).  */
                                                           
    /* Altera a largura da coluna de acordo com o tamanho do seu conteudo */
    chExcelApplication:Cells:Select.
    chExcelApplication:Cells:EntireColumn:AutoFit.

    chWorkSheet:Range("A2"):Select(). 

/* *******************************/                                                     

    RELEASE OBJECT chExcelApplication.
/*     RELEASE OBJECT chWorkbook. */
    RELEASE OBJECT chWorksheet.
END PROCEDURE.

