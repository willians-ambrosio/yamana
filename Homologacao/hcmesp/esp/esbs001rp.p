/*********************************************************************************************************
**
**       Programa: ESBS001RP.P
**
**       Data....: 08.01.2013
**
**       Autor...: Ricardo Stebulaitis
**
**       Objetivo: Gera»’o de Extra»’o de Dados de Beneficios de Previdencia Privada para o Bradesco
**
*********************************************************************************************************/

def temp-table tt-raw-digita
        field raw-digita        as raw.

define temp-table tt-param no-undo
    field destino             as integer
    field arquivo             as char format "x(35)"
    field usuario             as char format "x(12)"
    field data-exec           as date
    field hora-exec           as INTEGER
    FIELD cdn-empresa-ini     LIKE benefic_func.cdn_empresa
    FIELD cdn-empresa-fim     LIKE benefic_func.cdn_empresa
    FIELD cdn-estab-ini       LIKE benefic_func.cdn_estab
    FIELD cdn-estab-fim       LIKE benefic_func.cdn_estab
    FIELD cdn-funcionario-ini LIKE benefic_func.cdn_funcionario
    FIELD cdn-funcionario-fim LIKE benefic_func.cdn_funcionario.


DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEFINE TEMP-TABLE tt-erro LIKE RowErrors
    FIELD linha AS INTEGER.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEFINE VARIABLE h-bodi163 AS HANDLE.

DEFINE VARIABLE p-row  AS ROWID       NO-UNDO.

DEFINE VARIABLE c-arquivo-saida   AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE c-arquivo-busca   AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE c-arquivo-con     AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE c-arquivo-excel   AS CHARACTER FORMAT "x(200)" NO-UNDO.

/*------------------------------------------------------\
| Defini»’o das variaveis de Excel.                     |
\------------------------------------------------------*/
DEFINE VARIABLE ch_excel_application AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ch_excel_book        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ch_excel_sheet       AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ch_excel_sheet2      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE l_erro               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE i-coluna             AS INT.
DEFINE VARIABLE i-linha              AS CHAR.
DEFINE VARIABLE numero-linha         AS INT.

DEFINE VARIABLE l_sobrepor_arquivo   AS LOGICAL FORMAT "SIM/NAO"  NO-UNDO INITIAL YES.
DEFINE VAR c_arquivo_destino    AS CHARACTER FORMAT "X(60)"  NO-UNDO.

DEFINE VARIABLE i-filho-menor-21 AS INT.

DEFINE VARIABLE i-liberado AS LOGICAL INITIAL YES.

ASSIGN c_arquivo_destino = "c:\temp\esbs001" + REPLACE(STRING(TODAY),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".xls".

/* recebimento de parümetros */
{utp/ut-glob.i}
                               
/* include de controle de versÊo */
{include/buffers_RH.i}

{include/i-prgvrs.i ******* 2.04.00.001}

create tt-param.
raw-transfer raw-param to tt-param.

/* defini»’o de variaveis DATASUL WA */
def var h-acomp as handle no-undo. 

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
  
run pi-inicializar in h-acomp (input RETURN-VALUE).

/*LOGICA DO PROGRAMA */  

RUN pi_valida.
IF i-liberado = YES THEN DO:
    RUN pi_abre_planilha.
    RUN pi_escreve_planilha.
    RUN pi_salva_planilha.
END.

PROCEDURE pi_valida:
    FOR EACH rh_estab
        WHERE rh_estab.cdn_empresa >= tt-param.cdn-empresa-ini
        AND   rh_estab.cdn_empresa <= tt-param.cdn-empresa-fim
        AND   rh_estab.cdn_estab   >= tt-param.cdn-estab-ini
        AND   rh_estab.cdn_estab   <= tt-param.cdn-estab-fim NO-LOCK.
        
        FIND FIRST estab_benef_prev_brad
            WHERE estab_benef_prev_brad.cdn_empresa = rh_estab.cdn_empresa
            AND   estab_benef_prev_brad.cdn_estab   = rh_estab.cdn_estab NO-LOCK NO-ERROR.
        IF NOT AVAIL estab_benef_prev_brad THEN DO:
            MESSAGE "A Empresa " + STRING(rh_estab.cdn_empresa) + " Estabelecimento " + STRING(rh_estab.cdn_estab) + " nÆo possui Parƒmetro Cadastrado! Favor cadastrar atrav‚s da tela ""ESBS002"" !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN i-liberado = NO.
        END.
    END.
END PROCEDURE.

PROCEDURE pi_abre_planilha.
    CREATE "EXCEL.APPLICATION" ch_excel_application.
    ASSIGN ch_excel_book  = ch_excel_application:WORKBOOKS:ADD().
           ch_excel_application:sheets:ADD.
           ch_excel_application:sheets:ADD.
           ch_excel_sheet = ch_excel_application:SHEETS:ITEM(1).
           ch_excel_sheet:name = "EMP PGBL".

END PROCEDURE.

PROCEDURE pi_escreve_planilha:

    /*Cria Arquivo Previdencia Empresa*/

    ASSIGN ch_excel_sheet:RANGE("A1"):VALUE = 'CONTRATO'.
    ASSIGN ch_excel_sheet:RANGE("B1"):VALUE = 'CLIENTE'.
    ASSIGN ch_excel_sheet:RANGE("C1"):VALUE = 'EMPRESA'.
    ASSIGN ch_excel_sheet:RANGE("D1"):VALUE = 'CPF'.
    ASSIGN ch_excel_sheet:RANGE("E1"):VALUE = 'MATRICULA'.
    ASSIGN ch_excel_sheet:RANGE("F1"):VALUE = 'NOME'.
    ASSIGN ch_excel_sheet:RANGE("G1"):VALUE = 'BENEFEMP'.
    ASSIGN ch_excel_sheet:RANGE("H1"):VALUE = 'CONTR1EM'.
    ASSIGN ch_excel_sheet:RANGE("I1"):VALUE = 'BENEFFUN'.
    ASSIGN ch_excel_sheet:RANGE("J1"):VALUE = 'CONTR1FUN'.
    ASSIGN ch_excel_sheet:RANGE("K1"):VALUE = 'PROPOSTA'.
    ASSIGN ch_excel_sheet:RANGE("L1"):VALUE = 'SEXO'.
    ASSIGN ch_excel_sheet:RANGE("M1"):VALUE = 'DTNASC'.
    ASSIGN ch_excel_sheet:RANGE("N1"):VALUE = 'DTADMIS'.
    ASSIGN ch_excel_sheet:RANGE("O1"):VALUE = 'DTATC'.
    ASSIGN ch_excel_sheet:RANGE("P1"):VALUE = 'DTINICIO'.
    ASSIGN ch_excel_sheet:RANGE("Q1"):VALUE = 'ESTCIV'.
    ASSIGN ch_excel_sheet:RANGE("R1"):VALUE = 'NFIL'.
    ASSIGN ch_excel_sheet:RANGE("S1"):VALUE = 'ENDERECO'.
    ASSIGN ch_excel_sheet:RANGE("T1"):VALUE = 'NUMERO'.
    ASSIGN ch_excel_sheet:RANGE("U1"):VALUE = 'COMPLEMENT'.
    ASSIGN ch_excel_sheet:RANGE("V1"):VALUE = 'BAIRRO'.
    ASSIGN ch_excel_sheet:RANGE("W1"):VALUE = 'MUNICIPIO'.
    ASSIGN ch_excel_sheet:RANGE("X1"):VALUE = 'ESTADO'.
    ASSIGN ch_excel_sheet:RANGE("Y1"):VALUE = 'CEP'.
    ASSIGN ch_excel_sheet:RANGE("Z1"):VALUE = 'SALARIO'.
    ASSIGN ch_excel_sheet:RANGE("AA1"):VALUE = 'VLBENEF'.
    ASSIGN ch_excel_sheet:RANGE("AB1"):VALUE = 'TARIFA'.
    ASSIGN ch_excel_sheet:RANGE("AC1"):VALUE = 'TPARQ'.
    ASSIGN ch_excel_sheet:RANGE("AD1"):VALUE = 'IMPAUT'.


    ASSIGN numero-linha = 2.

    FOR EACH funcionario NO-LOCK
        WHERE funcionario.dat_desligto_func = ?
        AND   funcionario.cdn_empresa     >= tt-param.cdn-empresa-ini
        AND   funcionario.cdn_empresa     <= tt-param.cdn-empresa-fim
        AND   funcionario.cdn_estab       >= tt-param.cdn-estab-ini
        AND   funcionario.cdn_estab       <= tt-param.cdn-estab-fim
        AND   funcionario.cdn_funcionario >= tt-param.cdn-funcionario-ini
        AND   funcionario.cdn_funcionario <= tt-param.cdn-funcionario-fim,
        FIRST rh_pessoa_fisic OF funcionario NO-LOCK,
        FIRST benefic_func OF funcionario
            WHERE benefic_func.cdn_beneficio >= 310
            AND   benefic_func.cdn_beneficio <= 380 NO-LOCK,
        FIRST beneficio OF benefic_func NO-LOCK,
        FIRST movto_integr_benefic_fp OF benefic_func NO-LOCK.

        RUN pi-acompanhar IN h-acomp ("Funcionario..: " + STRING(funcionario.cdn_funcionario) + ' - ' + rh_pessoa_fisic.nom_pessoa_fisic).

        FIND FIRST estab_benef_prev_brad
            WHERE estab_benef_prev_brad.cdn_empresa = funcionario.cdn_empresa
            AND   estab_benef_prev_brad.cdn_estab   = funcionario.cdn_estab NO-LOCK NO-ERROR.

        ASSIGN ch_excel_sheet:RANGE("A" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_contrato /*Verificar*/.
        ASSIGN ch_excel_sheet:RANGE("B" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_cliente /*Verificar*/.
        FIND FIRST rh_estab
            WHERE rh_estab.cdn_estab = funcionario.cdn_estab NO-LOCK NO-ERROR.
        ASSIGN ch_excel_sheet:RANGE("C" + STRING(numero-linha)):VALUE = UPPER(rh_estab.nom_pessoa_jurid).
        ASSIGN ch_excel_sheet:RANGE("D" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_id_feder.
        ASSIGN ch_excel_sheet:RANGE("E" + STRING(numero-linha)):VALUE = funcionario.cdn_funcionario.
        ASSIGN ch_excel_sheet:RANGE("F" + STRING(numero-linha)):VALUE = UPPER(rh_pessoa_fisic.nom_pessoa_fisic).
        ASSIGN ch_excel_sheet:RANGE("G" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("H" + STRING(numero-linha)):VALUE = movto_integr_benefic_fp.val_rat_benefic_fp[2].
        ASSIGN ch_excel_sheet:RANGE("I" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("J" + STRING(numero-linha)):VALUE = '0' /*Verificar*/.
        ASSIGN ch_excel_sheet:RANGE("K" + STRING(numero-linha)):VALUE = ''/*Deixar em Branco*/.
        ASSIGN ch_excel_sheet:RANGE("L" + STRING(numero-linha)):VALUE = IF rh_pessoa_fisic.idi_sexo = 1 THEN 'M' ELSE 'F'.
        ASSIGN ch_excel_sheet:RANGE("M" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.dat_nascimento.
        ASSIGN ch_excel_sheet:RANGE("N" + STRING(numero-linha)):VALUE = funcionario.dat_admis_func.
        ASSIGN ch_excel_sheet:RANGE("O" + STRING(numero-linha)):VALUE = '01/' + (IF STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1) = '13' THEN '01' ELSE STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1)) + '/' + STRING(INTEGER(YEAR(rh_pessoa_fisic.dat_nascimento)) + 60).
        ASSIGN ch_excel_sheet:RANGE("P" + STRING(numero-linha)):VALUE = '01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY)) /*Verificar*/.
        CASE rh_pessoa_fisic.idi_estado_civil:
            WHEN 1 THEN /*Casado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '2' /* Casado */.
            WHEN 2 THEN /*Solteiro*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '1' /* Solteiro */.
            WHEN 3 THEN /*Desquitado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 4 THEN /*Divorciado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 5 THEN /*Viuvo*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '3' /* Viuvo */.
            WHEN 6 THEN /*Separado Judicialmente*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 7 THEN /*Outros*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '0' /* Outros */.
            WHEN 8 THEN /*Uni’o Estavel*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '5' /* Amigado / M<atrial */.
        END CASE.
        ASSIGN i-filho-menor-21 = 0.
        FOR EACH depend_func OF funcionario
            WHERE depend_func.idi_grau_depen_func = 1
            AND   (INT(TODAY - depend_func.dat_nascimento) / 365) < 21 NO-LOCK.
            ASSIGN i-filho-menor-21 = i-filho-menor-21 + 1.
        END.
        ASSIGN ch_excel_sheet:RANGE("R" + STRING(numero-linha)):VALUE = i-filho-menor-21.
        ASSIGN ch_excel_sheet:RANGE("S" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_ender_rh.
        ASSIGN ch_excel_sheet:RANGE("T" + STRING(numero-linha)):VALUE = INT(SUBSTRING(rh_pessoa_fisic.cod_livre_1,66,8)).
        ASSIGN ch_excel_sheet:RANGE("U" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_pto_refer.
        ASSIGN ch_excel_sheet:RANGE("V" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_bairro.
        ASSIGN ch_excel_sheet:RANGE("W" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_cidad.
        ASSIGN ch_excel_sheet:RANGE("X" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_unid_federac_rh.
        ASSIGN ch_excel_sheet:RANGE("Y" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_cep.
        ASSIGN ch_excel_sheet:RANGE("Z" + STRING(numero-linha)):VALUE = funcionario.val_salario_atual.
        ASSIGN ch_excel_sheet:RANGE("AA" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AB" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AC" + STRING(numero-linha)):VALUE = IF MONTH(funcionario.dat_admis_func) = MONTH(TODAY) AND YEAR(funcionario.dat_admis_func) = YEAR(TODAY) THEN 'IM' ELSE 'DM'.
        ASSIGN ch_excel_sheet:RANGE("AD" + STRING(numero-linha)):VALUE = 'S'.

        ASSIGN numero-linha = numero-linha + 1.

    END.
    
    /*Mudando de Aba - PGBL Funcionario*/

    ch_excel_sheet = ch_excel_application:SHEETS:ITEM(2).
    ch_excel_sheet:name = "FUNC PGBL".
  
    ASSIGN ch_excel_sheet:RANGE("A1"):VALUE = 'CONTRATO'.
    ASSIGN ch_excel_sheet:RANGE("B1"):VALUE = 'CLIENTE'.
    ASSIGN ch_excel_sheet:RANGE("C1"):VALUE = 'EMPRESA'.
    ASSIGN ch_excel_sheet:RANGE("D1"):VALUE = 'CPF'.
    ASSIGN ch_excel_sheet:RANGE("E1"):VALUE = 'MATRICULA'.
    ASSIGN ch_excel_sheet:RANGE("F1"):VALUE = 'NOME'.
    ASSIGN ch_excel_sheet:RANGE("G1"):VALUE = 'BENEFEMP'.
    ASSIGN ch_excel_sheet:RANGE("H1"):VALUE = 'CONTR1EM'.
    ASSIGN ch_excel_sheet:RANGE("I1"):VALUE = 'BENEFFUN'.
    ASSIGN ch_excel_sheet:RANGE("J1"):VALUE = 'CONTR1FUN'.
    ASSIGN ch_excel_sheet:RANGE("K1"):VALUE = 'PROPOSTA'.
    ASSIGN ch_excel_sheet:RANGE("L1"):VALUE = 'SEXO'.
    ASSIGN ch_excel_sheet:RANGE("M1"):VALUE = 'DTNASC'.
    ASSIGN ch_excel_sheet:RANGE("N1"):VALUE = 'DTADMIS'.
    ASSIGN ch_excel_sheet:RANGE("O1"):VALUE = 'DTATC'.
    ASSIGN ch_excel_sheet:RANGE("P1"):VALUE = 'DTINICIO'.
    ASSIGN ch_excel_sheet:RANGE("Q1"):VALUE = 'ESTCIV'.
    ASSIGN ch_excel_sheet:RANGE("R1"):VALUE = 'NFIL'.
    ASSIGN ch_excel_sheet:RANGE("S1"):VALUE = 'ENDERECO'.
    ASSIGN ch_excel_sheet:RANGE("T1"):VALUE = 'NUMERO'.
    ASSIGN ch_excel_sheet:RANGE("U1"):VALUE = 'COMPLEMENT'.
    ASSIGN ch_excel_sheet:RANGE("V1"):VALUE = 'BAIRRO'.
    ASSIGN ch_excel_sheet:RANGE("W1"):VALUE = 'MUNICIPIO'.
    ASSIGN ch_excel_sheet:RANGE("X1"):VALUE = 'ESTADO'.
    ASSIGN ch_excel_sheet:RANGE("Y1"):VALUE = 'CEP'.
    ASSIGN ch_excel_sheet:RANGE("Z1"):VALUE = 'SALARIO'.
    ASSIGN ch_excel_sheet:RANGE("AA1"):VALUE = 'VLBENEF'.
    ASSIGN ch_excel_sheet:RANGE("AB1"):VALUE = 'TARIFA'.
    ASSIGN ch_excel_sheet:RANGE("AC1"):VALUE = 'TPARQ'.
    ASSIGN ch_excel_sheet:RANGE("AD1"):VALUE = 'IMPAUT'.


    ASSIGN numero-linha = 2.

    FOR EACH funcionario NO-LOCK
        WHERE funcionario.dat_desligto_func = ?
        AND   funcionario.cdn_empresa     >= tt-param.cdn-empresa-ini
        AND   funcionario.cdn_empresa     <= tt-param.cdn-empresa-fim
        AND   funcionario.cdn_estab       >= tt-param.cdn-estab-ini
        AND   funcionario.cdn_estab       <= tt-param.cdn-estab-fim
        AND   funcionario.cdn_funcionario >= tt-param.cdn-funcionario-ini
        AND   funcionario.cdn_funcionario <= tt-param.cdn-funcionario-fim,
        FIRST rh_pessoa_fisic OF funcionario NO-LOCK,
        FIRST benefic_func OF funcionario
            WHERE benefic_func.cdn_beneficio >= 401
            AND   benefic_func.cdn_beneficio <= 408 NO-LOCK,
        FIRST beneficio OF benefic_func NO-LOCK,
        FIRST movto_integr_benefic_fp OF benefic_func NO-LOCK.

        RUN pi-acompanhar IN h-acomp ("Funcionario..: " + STRING(funcionario.cdn_funcionario) + ' - ' + rh_pessoa_fisic.nom_pessoa_fisic).

        FIND FIRST estab_benef_prev_brad
            WHERE estab_benef_prev_brad.cdn_empresa = funcionario.cdn_empresa
            AND   estab_benef_prev_brad.cdn_estab   = funcionario.cdn_estab NO-LOCK NO-ERROR.

        ASSIGN ch_excel_sheet:RANGE("A" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_contrato /*Verificar*/.
        ASSIGN ch_excel_sheet:RANGE("B" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_cliente /*Verificar*/.
        FIND FIRST rh_estab
            WHERE rh_estab.cdn_estab = funcionario.cdn_estab NO-LOCK NO-ERROR.
        ASSIGN ch_excel_sheet:RANGE("C" + STRING(numero-linha)):VALUE = UPPER(rh_estab.nom_pessoa_jurid).
        ASSIGN ch_excel_sheet:RANGE("D" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_id_feder.
        ASSIGN ch_excel_sheet:RANGE("E" + STRING(numero-linha)):VALUE = funcionario.cdn_funcionario.
        ASSIGN ch_excel_sheet:RANGE("F" + STRING(numero-linha)):VALUE = UPPER(rh_pessoa_fisic.nom_pessoa_fisic).
        ASSIGN ch_excel_sheet:RANGE("G" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("H" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("I" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("J" + STRING(numero-linha)):VALUE = movto_integr_benefic_fp.val_rat_benefic_fp[1].
        ASSIGN ch_excel_sheet:RANGE("K" + STRING(numero-linha)):VALUE = ''/*Deixar em Branco*/.
        ASSIGN ch_excel_sheet:RANGE("L" + STRING(numero-linha)):VALUE = IF rh_pessoa_fisic.idi_sexo = 1 THEN 'M' ELSE 'F'.
        ASSIGN ch_excel_sheet:RANGE("M" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.dat_nascimento.
        ASSIGN ch_excel_sheet:RANGE("N" + STRING(numero-linha)):VALUE = funcionario.dat_admis_func.
        ASSIGN ch_excel_sheet:RANGE("O" + STRING(numero-linha)):VALUE = '01/' + (IF STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1) = '13' THEN '01' ELSE STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1)) + '/' + STRING(INTEGER(YEAR(rh_pessoa_fisic.dat_nascimento)) + 60).
        ASSIGN ch_excel_sheet:RANGE("P" + STRING(numero-linha)):VALUE = '01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY)) /*Verificar*/.
        CASE rh_pessoa_fisic.idi_estado_civil:
            WHEN 1 THEN /*Casado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '2' /* Casado */.
            WHEN 2 THEN /*Solteiro*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '1' /* Solteiro */.
            WHEN 3 THEN /*Desquitado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 4 THEN /*Divorciado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 5 THEN /*Viuvo*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '3' /* Viuvo */.
            WHEN 6 THEN /*Separado Judicialmente*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 7 THEN /*Outros*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '0' /* Outros */.
            WHEN 8 THEN /*Uni’o Estavel*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '5' /* Amigado / M<atrial */.
        END CASE.
        ASSIGN i-filho-menor-21 = 0.
        FOR EACH depend_func OF funcionario
            WHERE depend_func.idi_grau_depen_func = 1
            AND   (INT(TODAY - depend_func.dat_nascimento) / 365) < 21 NO-LOCK.
            ASSIGN i-filho-menor-21 = i-filho-menor-21 + 1.
        END.
        ASSIGN ch_excel_sheet:RANGE("R" + STRING(numero-linha)):VALUE = i-filho-menor-21.
        ASSIGN ch_excel_sheet:RANGE("S" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_ender_rh.
        ASSIGN ch_excel_sheet:RANGE("T" + STRING(numero-linha)):VALUE = INT(SUBSTRING(rh_pessoa_fisic.cod_livre_1,66,8)).
        ASSIGN ch_excel_sheet:RANGE("U" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_pto_refer.
        ASSIGN ch_excel_sheet:RANGE("V" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_bairro.
        ASSIGN ch_excel_sheet:RANGE("W" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_cidad.
        ASSIGN ch_excel_sheet:RANGE("X" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_unid_federac_rh.
        ASSIGN ch_excel_sheet:RANGE("Y" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_cep.
        ASSIGN ch_excel_sheet:RANGE("Z" + STRING(numero-linha)):VALUE = funcionario.val_salario_atual.
        ASSIGN ch_excel_sheet:RANGE("AA" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AB" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AC" + STRING(numero-linha)):VALUE = IF MONTH(funcionario.dat_admis_func) = MONTH(TODAY) AND YEAR(funcionario.dat_admis_func) = YEAR(TODAY) THEN 'IM' ELSE 'DM'.
        ASSIGN ch_excel_sheet:RANGE("AD" + STRING(numero-linha)):VALUE = 'S'.

        ASSIGN numero-linha = numero-linha + 1.

    END.
    

    /*Mudando de Aba - VGBL Funcionario*/
    ch_excel_sheet = ch_excel_application:SHEETS:ITEM(3).
    ch_excel_sheet:name = "FUNC VGBL".
    
   ASSIGN ch_excel_sheet:RANGE("A1"):VALUE = 'CONTRATO'.
    ASSIGN ch_excel_sheet:RANGE("B1"):VALUE = 'CLIENTE'.
    ASSIGN ch_excel_sheet:RANGE("C1"):VALUE = 'EMPRESA'.
    ASSIGN ch_excel_sheet:RANGE("D1"):VALUE = 'CPF'.
    ASSIGN ch_excel_sheet:RANGE("E1"):VALUE = 'MATRICULA'.
    ASSIGN ch_excel_sheet:RANGE("F1"):VALUE = 'NOME'.
    ASSIGN ch_excel_sheet:RANGE("G1"):VALUE = 'BENEFEMP'.
    ASSIGN ch_excel_sheet:RANGE("H1"):VALUE = 'CONTR1EM'.
    ASSIGN ch_excel_sheet:RANGE("I1"):VALUE = 'BENEFFUN'.
    ASSIGN ch_excel_sheet:RANGE("J1"):VALUE = 'CONTR1FUN'.
    ASSIGN ch_excel_sheet:RANGE("K1"):VALUE = 'PROPOSTA'.
    ASSIGN ch_excel_sheet:RANGE("L1"):VALUE = 'SEXO'.
    ASSIGN ch_excel_sheet:RANGE("M1"):VALUE = 'DTNASC'.
    ASSIGN ch_excel_sheet:RANGE("N1"):VALUE = 'DTADMIS'.
    ASSIGN ch_excel_sheet:RANGE("O1"):VALUE = 'DTATC'.
    ASSIGN ch_excel_sheet:RANGE("P1"):VALUE = 'DTINICIO'.
    ASSIGN ch_excel_sheet:RANGE("Q1"):VALUE = 'ESTCIV'.
    ASSIGN ch_excel_sheet:RANGE("R1"):VALUE = 'NFIL'.
    ASSIGN ch_excel_sheet:RANGE("S1"):VALUE = 'ENDERECO'.
    ASSIGN ch_excel_sheet:RANGE("T1"):VALUE = 'NUMERO'.
    ASSIGN ch_excel_sheet:RANGE("U1"):VALUE = 'COMPLEMENT'.
    ASSIGN ch_excel_sheet:RANGE("V1"):VALUE = 'BAIRRO'.
    ASSIGN ch_excel_sheet:RANGE("W1"):VALUE = 'MUNICIPIO'.
    ASSIGN ch_excel_sheet:RANGE("X1"):VALUE = 'ESTADO'.
    ASSIGN ch_excel_sheet:RANGE("Y1"):VALUE = 'CEP'.
    ASSIGN ch_excel_sheet:RANGE("Z1"):VALUE = 'SALARIO'.
    ASSIGN ch_excel_sheet:RANGE("AA1"):VALUE = 'VLBENEF'.
    ASSIGN ch_excel_sheet:RANGE("AB1"):VALUE = 'TARIFA'.
    ASSIGN ch_excel_sheet:RANGE("AC1"):VALUE = 'TPARQ'.
    ASSIGN ch_excel_sheet:RANGE("AD1"):VALUE = 'IMPAUT'.


    ASSIGN numero-linha = 2.

    FOR EACH funcionario NO-LOCK
        WHERE funcionario.dat_desligto_func = ?
        AND   funcionario.cdn_empresa     >= tt-param.cdn-empresa-ini
        AND   funcionario.cdn_empresa     <= tt-param.cdn-empresa-fim
        AND   funcionario.cdn_estab       >= tt-param.cdn-estab-ini
        AND   funcionario.cdn_estab       <= tt-param.cdn-estab-fim
        AND   funcionario.cdn_funcionario >= tt-param.cdn-funcionario-ini
        AND   funcionario.cdn_funcionario <= tt-param.cdn-funcionario-fim,
        FIRST rh_pessoa_fisic OF funcionario NO-LOCK,
        FIRST benefic_func OF funcionario
            WHERE benefic_func.cdn_beneficio >= 409
            AND   benefic_func.cdn_beneficio <= 416 NO-LOCK,
        FIRST beneficio OF benefic_func NO-LOCK,
        FIRST movto_integr_benefic_fp OF benefic_func NO-LOCK.

        RUN pi-acompanhar IN h-acomp ("Funcionario..: " + STRING(funcionario.cdn_funcionario) + ' - ' + rh_pessoa_fisic.nom_pessoa_fisic).

        FIND FIRST estab_benef_prev_brad
            WHERE estab_benef_prev_brad.cdn_empresa = funcionario.cdn_empresa
            AND   estab_benef_prev_brad.cdn_estab   = funcionario.cdn_estab NO-LOCK NO-ERROR.

        ASSIGN ch_excel_sheet:RANGE("A" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_contrato /*Verificar*/.
        ASSIGN ch_excel_sheet:RANGE("B" + STRING(numero-linha)):VALUE = estab_benef_prev_brad.nr_cliente /*Verificar*/.
        FIND FIRST rh_estab
            WHERE rh_estab.cdn_estab = funcionario.cdn_estab NO-LOCK NO-ERROR.
        ASSIGN ch_excel_sheet:RANGE("C" + STRING(numero-linha)):VALUE = UPPER(rh_estab.nom_pessoa_jurid).
        ASSIGN ch_excel_sheet:RANGE("D" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_id_feder.
        ASSIGN ch_excel_sheet:RANGE("E" + STRING(numero-linha)):VALUE = funcionario.cdn_funcionario.
        ASSIGN ch_excel_sheet:RANGE("F" + STRING(numero-linha)):VALUE = UPPER(rh_pessoa_fisic.nom_pessoa_fisic).
        ASSIGN ch_excel_sheet:RANGE("G" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("H" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("I" + STRING(numero-linha)):VALUE = beneficio.des_abrev_benefic.
        ASSIGN ch_excel_sheet:RANGE("J" + STRING(numero-linha)):VALUE = movto_integr_benefic_fp.val_rat_benefic_fp[1].
        ASSIGN ch_excel_sheet:RANGE("K" + STRING(numero-linha)):VALUE = ''/*Deixar em Branco*/.
        ASSIGN ch_excel_sheet:RANGE("L" + STRING(numero-linha)):VALUE = IF rh_pessoa_fisic.idi_sexo = 1 THEN 'M' ELSE 'F'.
        ASSIGN ch_excel_sheet:RANGE("M" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.dat_nascimento.
        ASSIGN ch_excel_sheet:RANGE("N" + STRING(numero-linha)):VALUE = funcionario.dat_admis_func.
        ASSIGN ch_excel_sheet:RANGE("O" + STRING(numero-linha)):VALUE = '01/' + (IF STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1) = '13' THEN '01' ELSE STRING(INTEGER(MONTH(rh_pessoa_fisic.dat_nascimento)) + 1)) + '/' + STRING(INTEGER(YEAR(rh_pessoa_fisic.dat_nascimento)) + 60).
        ASSIGN ch_excel_sheet:RANGE("P" + STRING(numero-linha)):VALUE = '01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY)) /*Verificar*/.
        CASE rh_pessoa_fisic.idi_estado_civil:
            WHEN 1 THEN /*Casado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '2' /* Casado */.
            WHEN 2 THEN /*Solteiro*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '1' /* Solteiro */.
            WHEN 3 THEN /*Desquitado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 4 THEN /*Divorciado*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 5 THEN /*Viuvo*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '3' /* Viuvo */.
            WHEN 6 THEN /*Separado Judicialmente*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '4' /* Desquitado / Divorciado */.
            WHEN 7 THEN /*Outros*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '0' /* Outros */.
            WHEN 8 THEN /*Uni’o Estavel*/
                ASSIGN ch_excel_sheet:RANGE("Q" + STRING(numero-linha)):VALUE = '5' /* Amigado / M<atrial */.
        END CASE.
        ASSIGN i-filho-menor-21 = 0.
        FOR EACH depend_func OF funcionario
            WHERE depend_func.idi_grau_depen_func = 1
            AND   (INT(TODAY - depend_func.dat_nascimento) / 365) < 21 NO-LOCK.
            ASSIGN i-filho-menor-21 = i-filho-menor-21 + 1.
        END.
        ASSIGN ch_excel_sheet:RANGE("R" + STRING(numero-linha)):VALUE = i-filho-menor-21.
        ASSIGN ch_excel_sheet:RANGE("S" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_ender_rh.
        ASSIGN ch_excel_sheet:RANGE("T" + STRING(numero-linha)):VALUE = INT(SUBSTRING(rh_pessoa_fisic.cod_livre_1,66,8)).
        ASSIGN ch_excel_sheet:RANGE("U" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_pto_refer.
        ASSIGN ch_excel_sheet:RANGE("V" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_bairro.
        ASSIGN ch_excel_sheet:RANGE("W" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.nom_cidad.
        ASSIGN ch_excel_sheet:RANGE("X" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_unid_federac_rh.
        ASSIGN ch_excel_sheet:RANGE("Y" + STRING(numero-linha)):VALUE = rh_pessoa_fisic.cod_cep.
        ASSIGN ch_excel_sheet:RANGE("Z" + STRING(numero-linha)):VALUE = funcionario.val_salario_atual.
        ASSIGN ch_excel_sheet:RANGE("AA" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AB" + STRING(numero-linha)):VALUE = '0'.
        ASSIGN ch_excel_sheet:RANGE("AC" + STRING(numero-linha)):VALUE = IF MONTH(funcionario.dat_admis_func) = MONTH(TODAY) AND YEAR(funcionario.dat_admis_func) = YEAR(TODAY) THEN 'IM' ELSE 'DM'.
        ASSIGN ch_excel_sheet:RANGE("AD" + STRING(numero-linha)):VALUE = 'S'.

        ASSIGN numero-linha = numero-linha + 1.

    END.
    
    RETURN.
     
END PROCEDURE.

PROCEDURE pi_salva_planilha:

    IF l_sobrepor_arquivo THEN DO:
        ASSIGN FILE-INFO:FILE-NAME = c_arquivo_destino.
        IF FILE-INFO:PATHNAME <> ? THEN DO:
            OS-DELETE VALUE(c_arquivo_destino).
        END.
    END.

    ch_excel_sheet:SAVEAS(c_arquivo_destino).
    ch_excel_application:WORKBOOKS:CLOSE().

    RELEASE OBJECT ch_excel_application.
    RELEASE OBJECT ch_excel_book.
    RELEASE OBJECT ch_excel_sheet.
    
    CREATE "EXCEL.APPLICATION" ch_excel_application.
        ch_excel_application:VISIBLE = TRUE.
        ch_excel_book = ch_excel_application:WORKBOOKS:OPEN(c_arquivo_destino).
        RELEASE OBJECT ch_excel_application.

    RETURN.

END PROCEDURE.

/* FIM DO BLOCO */


run pi-finalizar in h-acomp.

RETURN "OK":U. 

