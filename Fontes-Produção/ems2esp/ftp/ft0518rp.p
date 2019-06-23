/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i ESFT0518RP 2.00.00.999}  /*** 010000 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ft0518rp MFT}
&ENDIF

/*****************************************************************************
**       Programa: FT0518rp.p
**       Data....: 14/03/07
**       Autor...: DATASUL S.A.
**       Objetivo: Emissor DANFE - NF-e
**       Vers∆o..: 1.00.000 - super
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

/*define variable c-prog-gerado as character no-undo initial "FT0518rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.*/

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param-aux
    field destino              as integer
    field destino-bloq         as integer
    field arquivo              as char
    field arquivo-bloq         as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field cod-layout           as character
    field des-layout           as character
    field log-impr-dados       as logical  
    field v_num_tip_aces_usuar as integer
&IF "{&mguni_version}" >= "2.071" &THEN
    field ep-codigo            LIKE ems2cadme.empresa.ep-codigo
&ELSE
    field ep-codigo            LIKE ems2cadme.empresa.ep-codigo
&ENDIF
    field c-cod-estabel        like nota-fiscal.cod-estabel
    field c-serie              like nota-fiscal.serie
    field c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis
    field de-cdd-embarq-ini    like nota-fiscal.cdd-embarq
    field de-cdd-embarq-fim    like nota-fiscal.cdd-embarq
    field da-dt-saida          like nota-fiscal.dt-saida
    field c-hr-saida           like nota-fiscal.hr-confirma
    field banco                as integer
    field cod-febraban         as integer      
    field cod-portador         as integer      
    field prox-bloq            as char         
    field c-instrucao          as char extent 5
    field imprime-bloq         as logical
    field rs-imprime           as INTEGER
    FIELD impressora-so        AS CHAR
    FIELD impressora-so-bloq   AS CHAR
    FIELD nr-copias            AS INTEGER.

/****************** Parametros **********************/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.



DEF TEMP-TABLE ttArquivo NO-UNDO
      FIELD sequencia AS INT
      FIELD nomeArquivo AS CHAR
      INDEX idx1 sequencia.

define shared temp-table tt-notas-impressas
    field r-nota as rowid.


{bcp/bcapi004.i}
{cdp/cd0666.i}
{cdp/cdcfgdis.i}

/*************************
*   definicao de buffer
*************************/
def buffer b-nota-fiscal for nota-fiscal.
                                                              
{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */ 

{ftp/ft0518rp.i1 "NEW"} /* Definiá∆o temp-table ttCaracteres como NEW SHARED */
{ftp/ft0518rp.i2}       /* Criaá∆o registros temp-table ttCaracteres e ttColunasDANFE */

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

{utp/ut-glob.i}

/****  Variaveis Compartilhadas  ****/
DEFINE NEW SHARED VAR r-nota       AS ROWID.
DEFINE NEW SHARED VAR c-hr-saida   AS CHAR    FORMAT "xx:xx:xx" INIT "000000".
DEFINE NEW SHARED VAR l-dt         AS LOGICAL FORMAT "Sim/Nao"  INIT NO.
                       

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.
def var c-arquivo-continua   as char   no-undo.
DEF VAR lSemWord             AS LOG    NO-UNDO.

DEF VAR da-dt-saida          AS DATE   NO-UNDO.
DEF VAR c-cod-layout         AS CHAR   NO-UNDO.
DEF VAR l-mais-itens         AS LOG    NO-UNDO INIT NO.
&IF "{&mguni_version}" >= "2.071" &THEN
def var c-cod-estabel      like nota-fiscal.cod-estabel format "x(05)"       initial "" no-undo.
&ELSE
def var c-cod-estabel      like nota-fiscal.cod-estabel format "X(3)"       initial "" no-undo.
&ENDIF
def var c-serie            like nota-fiscal.serie       format "x(5)"       initial "" no-undo.
def var r-ped-venda     as rowid.
def var r-pre-fat       as rowid.
def var r-emitente      as rowid.
def var r-estabel       as rowid.
def var r-docum-est     as rowid.
DEF VAR r-ser-estab     AS ROWID.
def var r-natur-oper   as rowid.
def var l-tipo-nota     as logical format "Entrada/Saida" no-undo.
/*def var de-conv        as decimal format ">>>>9.99".
def var de-conv-pis    as decimal format ">>>>9.99".
def var de-conv-cofins as decimal format ">>>>9.99".
def var de-conv-total  as decimal format ">>>>9.99".*/


def var i-sit-nota-ini    as integer.
def var i-sit-nota-fim    as integer.

define stream arq-erro.
def var c-arquivo as char format "X(40)".

create tt-param-aux.
raw-transfer raw-param to tt-param-aux.

{include/i-rpvar.i}

assign c-programa     = "FT0518rp":U
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Emissor DANFE - NF-e"
       c-sistema      = "mft".


{varinc/var00002.i}

run utp/ut-acomp.p persistent set h-acomp.

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa  = mguni.empresa.razao-social.
else
    assign c-empresa = "".

/* run setConstant in h-FunctionLibrary ("cCompany", c-empresa).                */
/* run setConstant in h-FunctionLibrary ("cReportTitle", c-titulo-relat).       */
/* run setConstant in h-FunctionLibrary ("cSystem", c-sistema).                 */
/* run setConstant in h-FunctionLibrary ("cProgram", c-programa).               */
/* run setConstant in h-FunctionLibrary ("cVersion", c-versao).                 */
/* run setConstant in h-FunctionLibrary ("cRevision", c-revisao).               */
/* run setConstant in h-FunctionLibrary ("cCurrentUser", tt-param-aux.usuario). */
/* run setConstant in h-FunctionLibrary ("cActualDate", STRING(TODAY)).         */
/* run setConstant in h-FunctionLibrary ("cActualHour", STRING(TIME)).          */

/**** EXECUCAO RELATORIO GRAFICO ****/
case tt-param-aux.destino:
    when 1 then assign v-cod-destino-impres = "Impressora".
    when 2 then assign v-cod-destino-impres = "Arquivo".
    otherwise   assign v-cod-destino-impres = "Terminal".
end case.

/*run printForm in h-FunctionLibrary.*/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

for each tt-notas-impressas:
    delete tt-notas-impressas.
end.

if tt-param-aux.data-exec <> ? then 
   assign l-dt = yes.
else 
   assign l-dt = no.

assign c-cod-estabel       = tt-param-aux.c-cod-estabel
       c-serie             = tt-param-aux.c-serie
       da-dt-saida     = tt-param-aux.da-dt-saida
       c-hr-saida    = string(tt-param-aux.c-hr-saida,"99:99:99")
       c-cod-layout = tt-param-aux.cod-layout.

if   tt-param-aux.rs-imprime = 1 then
     assign i-sit-nota-ini = 1
            i-sit-nota-fim = 1.
else assign i-sit-nota-ini = 2
            i-sit-nota-fim = 7.

for each nota-fiscal no-lock
   where nota-fiscal.cod-estabel  = c-cod-estabel      
     and nota-fiscal.serie        = c-serie
     and nota-fiscal.nr-nota-fis >= c-nr-nota-fis-ini
     and nota-fiscal.nr-nota-fis <= c-nr-nota-fis-fim
     and nota-fiscal.cdd-embarq >= de-cdd-embarq-ini 
     and nota-fiscal.cdd-embarq <= de-cdd-embarq-fim 
     and   nota-fiscal.ind-sit-nota >= i-sit-nota-ini
     and   nota-fiscal.ind-sit-nota <= i-sit-nota-fim  
     break by nota-fiscal.cod-estabel
           by nota-fiscal.serie
           by nota-fiscal.nr-nota-fis:

    &IF DEFINED (bf_dis_nfe) &THEN                                                                       
        &IF "{&bf_dis_versao_ems}" >= "2.07" &THEN                                                       
            
            IF (nota-fiscal.idi-forma-emis-nf-eletro  = 1        /* Tipo de Emiss∆o   = Normal                  */
           AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* Situaá∆o da nota <> Uso Autorizado          */ 
            OR (nota-fiscal.idi-forma-emis-nf-eletro  = 4        /* Tipo de Emiss∆o   = Contingencia DPEC       */
           AND  nota-fiscal.idi-sit-nf-eletro        <> 15       /* Situaá∆o da nota <> DPEC recebido pelo SCE  */
           AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* Situaá∆o da nota <> Uso Autorizado          */ 
            OR (nota-fiscal.idi-forma-emis-nf-eletro <> 1        /* Tipo de Emiss∆o  <> Normal                  */
           AND  nota-fiscal.idi-forma-emis-nf-eletro <> 4        /* Tipo de Emiss∆o  <> Contingencia DPEC       */
           AND  nota-fiscal.idi-sit-nf-eletro         = 5 ) THEN /* Situaá∆o da nota  = Documento Rejeitado */
               NEXT.
        &ELSE
            IF  &if '{&bf_dis_versao_ems}' >= '2.07':U
                &then STRING(nota-fiscal.idi-forma-emis-nf-eletro) = 0
                &else SUBSTR(nota-fiscal.char-2,65,2) = '' &endif THEN 
                NEXT.

            IF &if '{&bf_dis_versao_ems}' >= '2.07':U
               &then STRING(nota-fiscal.idi-forma-emis-nf-eletro)
               &else SUBSTR(nota-fiscal.char-2,65,2) &endif = '1' THEN DO: /*Tp Emis 1 = Normal*/
            
                FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                IF  NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro <> 3 THEN /*Sit 3 = Uso Autorizado*/
                    NEXT.
            END.
            IF &if '{&bf_dis_versao_ems}' >= '2.07':U
               &then STRING(nota-fiscal.idi-forma-emis-nf-eletro)
               &else SUBSTR(nota-fiscal.char-2,65,2) &endif = '4' THEN DO: /*Tp Emis 4 = Contingància DPEC*/
            
                FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                IF  NOT AVAIL sit-nf-eletro OR (sit-nf-eletro.idi-sit-nf-eletro <> 15 AND sit-nf-eletro.idi-sit-nf-eletro <> 3) THEN /*Sit 15 = DPEC recebido pelo SCE*/
                    NEXT.
            END.
        &ENDIF
    &ENDIF


    run pi-acompanhar in h-acomp("Gerando DANFE para nota " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    
    if  first-of(nota-fiscal.nr-nota-fis) then do:
        assign v-cont-registro = 0.
    end.

    if  first-of(nota-fiscal.nr-nota-fis) then do:
        run pi-imprime-nota.
    end.

/*    IF l-mais-itens THEN
        run printForm in h-FunctionLibrary2.
    
    if  last-of(nota-fiscal.nr-nota-fis) then do:
        run writePage   in h-FunctionLibrary (input yes).
        IF l-mais-itens THEN
            run writePage   in h-FunctionLibrary2 (input yes).
    end.
    else do:
        if  v-cont-registro = -1 then do: 
            assign v-cont-registro = 0.
            run writePage in h-FunctionLibrary (input no).
            IF l-mais-itens THEN
                run writePage   in h-FunctionLibrary2 (input NO).
        end.
    end.*/
end.

run pi-acompanhar in h-acomp("Gerando Arquivo Final").

IF CAN-FIND (FIRST ser-estab                                 
             WHERE ser-estab.cod-estabel = c-cod-estabel      
               AND ser-estab.serie       = c-serie
               AND &IF "{&bf_dis_versao_ems}":U >= "2.08":U &THEN    
                       ser-estab.log-word-danfe                      
                   &ElSE                                             
                       substring(ser-estab.char-1,70,1) = "S":U   
                   &ENDIF) THEN ASSIGN lSemWord = YES. 

IF NOT lSemWord THEN
   RUN piJuntaArquivos.

run pi-acompanhar in h-acomp("Abrindo Documento DANFE").

/*IF tt-param-aux.destino = 1 OR tt-param-aux.destino = 4 THEN DO:
    RUN setPrintable IN h-FunctionLibrary.
    IF l-mais-itens THEN
        RUN setPrintable IN h-FunctionLibrary2.
END.                                      

run saveXML in h-FunctionLibrary(input tt-param-aux.arquivo).
ASSIGN c-arquivo-continua = session:temp-directory + "FT0518aa-cont.pdf".
IF l-mais-itens THEN
    run saveXML in h-FunctionLibrary2(input c-arquivo-continua).
*/
assign v-des-retorno = "OK":U.

if v-des-retorno <> "OK" then do:
    if i-num-ped-exec-rpw <> 0 then
        return v-des-retorno.
    else
        message v-des-retorno view-as alert-box error buttons ok.
end.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

IF  lSemWord THEN DO:
    IF  tt-param-aux.destino = 1 
    OR  tt-param-aux.destino = 3 THEN DO:
        FOR EACH ttArquivo:
            RUN OpenDocument(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).
        END.
    END.
END.
ELSE DO:
    /* Tati
    IF  tt-param-aux.destino = 3 THEN DO:
        RUN OpenDocument(tt-param-aux.arquivo). ---->>> Ç aqui que est† abrindo o arquivo word
        IF l-mais-itens THEN
            RUN OpenDocument(c-arquivo-continua).
    END.
    */
END.

return 'OK'.

/* Procedure para impressao da nota fiscal */
procedure pi-imprime-nota:
    /* data de saida da nota fiscal */
    assign r-nota = rowid(nota-fiscal).

    if  l-dt = YES AND string(da-dt-saida) <> "" AND da-dt-saida <> ? then do:
        find first b-nota-fiscal
             where rowid(b-nota-fiscal) = rowid(nota-fiscal) EXCLUSIVE-LOCK no-error.
        assign b-nota-fiscal.dt-saida = da-dt-saida.
    end.
    
    find first ped-venda
         where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
           and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
         no-lock no-error.
    
    find first estabelec
         where estabelec.cod-estabel = nota-fiscal.cod-estabel
         no-lock no-error.

    find first emitente
         where emitente.nome-abrev = nota-fiscal.nome-ab-cli
         no-lock no-error.

    find first pre-fatur use-index ch-embarque
         where pre-fatur.cdd-embarq   = nota-fiscal.cdd-embarq
         and   pre-fatur.nome-abrev   = nota-fiscal.nome-ab-cli
         and   pre-fatur.nr-pedcli    = nota-fiscal.nr-pedcli
         and   pre-fatur.nr-resumo    = nota-fiscal.nr-resumo
         no-lock no-error.

    find first natur-oper
         where natur-oper.nat-operacao = nota-fiscal.nat-operacao
         no-lock no-error.

    FIND FIRST ser-estab NO-LOCK
        WHERE  ser-estab.cod-estabel = c-cod-estabel
          AND  ser-estab.serie       = c-serie NO-ERROR.

    assign r-estabel    = rowid(estabelec)
           r-ped-venda  = rowid(ped-venda)
           r-emitente   = rowid(emitente)
           r-natur-oper = rowid(natur-oper)
           r-ser-estab  = ROWID(ser-estab)
           r-pre-fat    = if  avail pre-fatur then
                              rowid(pre-fatur)
                          else ?
           l-tipo-nota  = no.

    /*assign de-conv        = 1
           de-conv-pis    = 1
           de-conv-cofins = 1
           de-conv-total  = 1.

    find first cidade-zf where cidade-zf.cidade = nota-fiscal.cidade
                         and   cidade-zf.estado = nota-fiscal.estado
                         no-lock no-error.
    if  avail cidade-zf 
    and dec(substr(natur-oper.char-2,66,5)) > 0 then
        assign de-conv = (100 - dec(substr(natur-oper.char-2,66,5))) / 100.
        /* valor para tratamento de ZFM */

    &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN

        if  avail cidade-zf 
        and natur-oper.val-perc-desc-pis-zfm > 0 then
            assign de-conv-pis = (100 - natur-oper.val-perc-desc-pis-zfm) / 100.
    
        if  avail cidade-zf 
        and natur-oper.val-perc-desc-cofins-zfm > 0 then
            assign de-conv-cofins = (100 - natur-oper.val-perc-desc-cofins-zfm) / 100.
    
        if  avail cidade-zf 
        and (dec(substr(natur-oper.char-2,66,5)) > 0 OR 
             natur-oper.val-perc-desc-pis-zfm    > 0 OR
             natur-oper.val-perc-desc-cofins-zfm > 0) THEN
            assign de-conv-total = (100 - (dec(substr(natur-oper.char-2,66,5)) +
                                           natur-oper.val-perc-desc-pis-zfm    +
                                           natur-oper.val-perc-desc-cofins-zfm)) / 100.
    &ELSE

        if  avail cidade-zf 
        and dec(substr(natur-oper.char-1,110,8)) > 0 then
            assign de-conv-pis = (100 - dec(substr(natur-oper.char-1,110,8))) / 100.
    
        if  avail cidade-zf 
        and dec(substr(natur-oper.char-1,118,8)) > 0 then
            assign de-conv-cofins = (100 - dec(substr(natur-oper.char-1,118,8))) / 100.
    
        if  avail cidade-zf 
        and (dec(substr(natur-oper.char-2,66,5)) > 0 OR 
             dec(substr(natur-oper.char-1,110,8)) > 0 OR
             dec(substr(natur-oper.char-1,118,8)) > 0) THEN
            assign de-conv-total = (100 - (dec(substr(natur-oper.char-2,66,5))  + 
                                           dec(substr(natur-oper.char-1,110,8)) + 
                                           dec(substr(natur-oper.char-1,118,8)))) / 100.
    &ENDIF
    */

    run ftp/ft0518f.p (INPUT-OUTPUT TABLE ttArquivo).

    /* muda o status da nota-fiscal */
    run ftp/ft0503a.p .

    create tt-notas-impressas.
    assign tt-notas-impressas.r-nota = rowid(nota-fiscal).

    /* GERA ETIQUETAS PARA O MODULO DE COLETA DE DADOS */

    if  avail param-global
    and param-global.modulo-cl
    and ( nota-fiscal.ind-tip-nota = 2) /* tipo de nota-fiscal Manual */
    then do:
        create tt-prog-bc.
        assign tt-prog-bc.cod-prog-dtsul        = "ft0513"
               tt-prog-bc.cod-versao-integracao = 1
               tt-prog-bc.usuario               = tt-param-aux.usuario
               tt-prog-bc.opcao                 = 1.

        run bcp/bcapi004.p (input-output table tt-prog-bc,
                            input-output table tt-erro).

        find first tt-prog-bc no-error.

        assign  c-arquivo = tt-prog-bc.nome-dir-etiq + "/" + c-arquivo.

        if  return-value = "OK" then do:

            {utp/ut-liter.i Gerando_Etiquetas  MRE R}
            run pi-acompanhar in h-acomp (input return-value).

            erro:
            do  on stop     undo erro,leave erro
                on quit     undo erro,leave erro
                on error    undo erro,leave erro
                on endkey   undo erro,leave erro:

                run value(tt-prog-bc.prog-criacao)(input tt-prog-bc.cd-trans,
                                                   input rowid(nota-fiscal),
                                                   input-output table tt-erro) no-error.

                if  ERROR-STATUS:ERROR 
                or  (    error-status:get-number(1) <> 138
                     and error-status:num-messages  <> 0)
                then do:
                    output stream arq-erro to value(c-arquivo) append.

                    {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas_-_Progress MRE R}
                    put stream arq-erro "***" return-value skip.
                    {utp/ut-liter.i Programa * R}
                    put stream arq-erro error-status:get-message(1) skip.
                    put stream arq-erro return-value ": " tt-prog-bc.prog-criacao skip.
                    put stream arq-erro nota-fiscal.serie                           at 1.
                    put stream arq-erro nota-fiscal.nr-nota-fis                     at 7.
                    put stream arq-erro nota-fiscal.cod-estabel                     at 24.

                    output stream arq-erro close.
                end.

                if  return-value = "NOK" then do:
                    find first tt-erro no-error.
                    if  avail tt-erro
                    then do:
                        output stream arq-erro to value(c-arquivo) append.

                        {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
                        put stream arq-erro "***" return-value skip.
                        for each tt-erro:
                            put stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                        end.
                        put stream arq-erro skip.
                        output stream arq-erro close.
                    end.
                end.
            end.
        end.
        else do:
            /**** caso tenha integraá∆o com o coleta e ocorreu erros ***/
            find first tt-erro no-error.
            if  avail tt-erro then do:
                output stream arq-erro to value(c-arquivo) append.

                {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
                put stream arq-erro "***" return-value skip.
                for each tt-erro:
                    put  stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                end.
                put stream arq-erro skip.
                output stream arq-erro close.
            end.
        end.
    end.
    /*************************************************/
end procedure.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE piJuntaArquivos:
    DEF VAR lEntrou     AS LOG NO-UNDO.
    DEF VAR ch-app-word AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE i-numero-copia AS INTEGER     NO-UNDO.

    /**PDF**/
    DEFINE VARIABLE viReturnCode AS INTEGER NO-UNDO.
    DEFINE VARIABLE c-caracter   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i-tamanho    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-arquivopdf AS CHARACTER   NO-UNDO. 

    DEFINE VARIABLE c-impressora-padrao AS CHARACTER  NO-UNDO.

    assign c-arquivopdf = tt-param-aux.arquivo.
    do while c-caracter <> ".":
        assign i-tamanho  = length(c-arquivopdf)
               c-caracter = substring(c-arquivopdf, i-tamanho, 1).
        if c-caracter <> "." then
            assign c-arquivopdf = substring(c-arquivopdf,1,i-tamanho - 1).
    end.
    assign c-arquivopdf = c-arquivopdf + "pdf".

    CREATE 'Word.Application':U ch-app-word.                         /* Cria uma aplicaá∆o WORD */
    ch-app-word:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
    ch-app-word:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */

    FOR EACH ttArquivo:
        run pi-acompanhar in h-acomp("Imprimindo Nota Fiscal " + ENTRY(3,ttArquivo.nomeArquivo,"-") ).
        
        DO i-numero-copia = 1 TO tt-param-aux.nr-copias:
        
            IF  ttArquivo.Sequencia = 1 AND 
                i-numero-copia      = 1 THEN
                ch-app-word:Documents:ADD(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).        /* Inclui arquivo */
            ELSE DO:
                ch-app-word:SELECTION:EndKey(6).                         /* Posiciona cursor no final do arquivo */
                ch-app-word:SELECTION:InsertBreak(7).                    /* Qubra pagina antes de inserir arquivo */
                ch-app-word:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).  /* Insere arquivo no documento aberto */
            END.

        END.

        ASSIGN lEntrou = YES.
    END.
    
    IF NOT lEntrou THEN
       ch-app-word:Documents:ADD().                                 /* Inclui arquivo */

    /*ch-app-word:ActiveDocument:SaveAs(tt-param-aux.arquivo).         /* Salva o arquivo aberto no WORD com o nome final do arquivo */*/

    /*SALVA PDF*/
    ch-app-word:ActiveDocument:SaveAs(c-arquivopdf, 17, YES, "", YES, "", NO, NO, NO, NO, NO).  /*Salva pdf*/
    
    

    IF SEARCH(c-arquivopdf) <> ? THEN
        FILE-INFO:FILE-NAME = c-arquivopdf.

    /*MOSTRA pdf*/
    RUN ShellExecuteA(INPUT 0,
                      INPUT "open":U,
                      INPUT FILE-INFO:FULL-PATHNAME,
                      INPUT "":U,
                      INPUT "":U,
                      INPUT 1,   /* Se for zero n'o abre o arquivo */
                      OUTPUT viReturnCode).
    /**/    

    IF  tt-param-aux.destino = 1 THEN DO: /* Impressora */
        
        IF INDEX (tt-param-aux.impressora-so," in session") > 0 THEN
            tt-param-aux.impressora-so = tt-param-aux.impressora-so + " on " + SESSION:PRINTER-PORT.
        /* Guarda a impressora padr∆o do windows antes da geraá∆o do DANFE */
        ASSIGN c-impressora-padrao = SESSION:PRINTER-NAME.
        /* Seleciona a impressora para impress∆o */
        ch-app-word:ActivePrinter = tt-param-aux.impressora-so.
        /* Imprime o documento na impressora selecionada */
        ch-app-word:printout(0). /* 0 : N∆o mostra erros nem advertencias */
        /* Volta a impressora padr∆o do windows */
        ch-app-word:ActivePrinter = c-impressora-padrao.    
    END.

    ch-app-word:VISIBLE = no.
    ch-app-word:ActiveDocument:CLOSE.                                /* Fecha o arquivo do WORD */
    ch-app-word:QUIT().                                              /* Fechar o WORD */
    RELEASE OBJECT ch-app-word.                                      /* Elimina o endereáo utilizado para o WORD na m†quina */

    FOR EACH ttArquivo:
        OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo) NO-ERROR.
    END.

END.

/* fim do programa */
