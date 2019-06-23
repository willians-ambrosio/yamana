/*****************************************************************************
** Programa..............: 
** Descricao.............: 
** Versao................: 
** Procedimento..........: 
** Nome Externo..........: 
** Data Geracao..........: 
** Criado por............: Gustavo S. B. Carvalho
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.001":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=0":U.
/*************************************  *************************************/

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_param_program_formul no-undo
    field tta_cdn_segment_edi              as Integer format ">>>>>9" initial 0 label "Segmento" column-label "Segmento"
    field tta_cdn_element_edi              as Integer format ">>>>>9" initial 0 label "Elemento" column-label "Elemento"
    field tta_des_label_utiliz_formul_edi  as character format "x(10)" label "Label Utiliz Formula" column-label "Label Utiliz Formula"
    field ttv_des_contdo                   as character format "x(100)" label "Conteudo" column-label "Conteudo"
    index tt_param_program_formul_id       is primary
          tta_cdn_segment_edi              ascending
          tta_cdn_element_edi              ascending
    .


/********************** Temporary Table Definition End **********************/

/************************ Parameter Definition Begin ************************/

def Input param p_cdn_mapa_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param p_cdn_segment_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param p_cdn_element_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param table 
    for tt_param_program_formul.


/************************* Parameter Definition End *************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu rio"
    column-label "Usu rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usu rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa¡s Empresa Usu rio"
    column-label "Pa¡s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu rio Corrente"
    column-label "Usu rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.

DEF VAR c-retorno AS CHARACTER FORM "X(1000)" NO-UNDO.

/************************** Variable Definition End *************************/


/************************** Function Definition Begin ***********************/

FUNCTION fc_valor RETURNS CHARACTER (INPUT segmento AS INT, elemento AS INT):

    FIND FIRST tt_param_program_formul NO-LOCK
            WHERE tt_param_program_formul.tta_cdn_segment_edi = segmento
              AND tt_param_program_formul.tta_cdn_element_edi = elemento
        NO-ERROR.

    IF AVAIL tt_param_program_formul THEN
    DO:
        IF tt_param_program_formul.tta_des_label_utiliz_formul_edi BEGINS "Vlr" THEN
            RETURN REPLACE(tt_param_program_formul.ttv_des_contdo,",00","").
        ELSE IF tt_param_program_formul.tta_des_label_utiliz_formul_edi BEGINS "Dt" THEN
            RETURN STRING(YEAR(DATE(tt_param_program_formul.ttv_des_contdo)),"9999") + STRING(MONTH(DATE(tt_param_program_formul.ttv_des_contdo)),"99") + STRING(DAY(DATE(tt_param_program_formul.ttv_des_contdo)),"99").
        ELSE
            RETURN tt_param_program_formul.ttv_des_contdo.
    END.
    ELSE
        RETURN "Erro no elemento".
END.

/************************** Function Definition End *************************/

/****************************** Main Code Begin *****************************/
/*
OUTPUT TO VALUE("V:\Temp\campos.txt") APPEND.
for each tt_param_program_formul:
    EXPORT DELIMITER ";" 
        tt_param_program_formul.tta_cdn_segment_edi
        tt_param_program_formul.tta_cdn_element_edi
        tt_param_program_formul.tta_des_label_utiliz_formul_edi
        tt_param_program_formul.ttv_des_contdo.
end.
OUTPUT CLOSE.
*/

ASSIGN c-retorno = fc_valor(300,5153).

IF p_cdn_mapa_edi = 21 AND p_cdn_segment_edi = 289 THEN
CASE p_cdn_element_edi:

    /* Agencia */
    WHEN 4678 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,172,4).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,171,4).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,135,4).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Dig. Agencia */
    WHEN 3941 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,176,1).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,175,1).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,139,1).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Conta Corrente */
    WHEN 4679 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,181,7).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,180,7).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,144,7).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Dig. Conta Corrente */
    WHEN 3904 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,188,1).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,187,1).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,151,1).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Forma de Pagamento */
    WHEN 3728 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "".
            WHEN "005" THEN /* GPS */
                RETURN "".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Tipo Pagamento */
    WHEN 3729 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "".
            WHEN "005" THEN /* GPS */
                RETURN "".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Nosso numero */
    WHEN 3743 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "".
            WHEN "005" THEN /* GPS */
                RETURN "".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Seu Numero */
    WHEN 3928 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,952,16).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,952,16).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,952,16).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Data Pagamento */
    WHEN 3709 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,342,2) + SUBSTR(c-retorno,340,2) + SUBSTR(c-retorno,336,4).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,342,2) + SUBSTR(c-retorno,340,2) + SUBSTR(c-retorno,336,4).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,288,2) + SUBSTR(c-retorno,286,2) + SUBSTR(c-retorno,282,4).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor do Titulo */
    WHEN 4436 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,198,15).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,197,15).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,290,15).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* data Efetiva */
    WHEN 2705 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,195,2) + SUBSTR(c-retorno,193,2) + SUBSTR(c-retorno,189,4).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,194,2) + SUBSTR(c-retorno,192,2) + SUBSTR(c-retorno,188,4).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,118,2) + SUBSTR(c-retorno,116,2) + SUBSTR(c-retorno,112,4).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Efetivo */
    WHEN 4439 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,243,15).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,242,15).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,305,15).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Abatimento */
    WHEN 4425 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "0".
            WHEN "005" THEN /* GPS */
                RETURN "0".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "0".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Desconto */
    WHEN 4423 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "0".
            WHEN "005" THEN /* GPS */
                RETURN "0".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,350,15).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Multa */
    WHEN 4426 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,228,15).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,227,15).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,320,15).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Juros */
    WHEN 4422 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,213,15).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,212,15).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,335,15).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Valor Correcao */
    WHEN 4437 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "0".
            WHEN "005" THEN /* GPS */
                RETURN "0".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "0".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Mensagem 1 */
    WHEN 3945 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,900,02).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,900,02).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,900,02).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Mensagem 2 */
    WHEN 3946 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,902,02).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,902,02).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,902,02).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.
    
    /* Mensagem 3 */
    WHEN 3947 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,904,02).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,904,02).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,904,02).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.
    
    /* Mensagem 4 */
    WHEN 3948 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,906,02).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,906,02).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,906,02).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.
    
    /* Mensagem 5 */
    WHEN 3949 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN SUBSTR(c-retorno,908,02).
            WHEN "005" THEN /* GPS */
                RETURN SUBSTR(c-retorno,908,02).
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,908,02).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.
    
    /* Codigo de Barras */
    WHEN 2807 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "".
            WHEN "005" THEN /* GPS */
                RETURN "".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN SUBSTR(c-retorno,68,44).
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

    /* Tipo de Movimento */
    WHEN 3808 THEN
    DO:
        CASE SUBSTR(c-retorno,38,3):
            WHEN "001" THEN /* DARF */
                RETURN "".
            WHEN "005" THEN /* GPS */
                RETURN "".
            WHEN "006" THEN /* Codigo de Barras */
                RETURN "".
            OTHERWISE
                RETURN ?.
        END CASE.
    END.

END CASE.

