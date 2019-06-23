/*****************************************************************************
** Programa..............: Mapa de envio Bradesco 1000 posi‡äes
** Descricao.............: Envio de DARF, GPS e pagamentos com c¢digo de barras
** Versao................: 1.0
** Procedimento..........: 
** Nome Externo..........: 
** Data Geracao..........: 02/01/2012
** Criado por............: Gustavo S. B. Carvalho - Totvs Paulistana
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
          tta_cdn_element_edi              ascending.


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

DEF VAR c-retorno       AS CHAR FORM "X(1000)" NO-UNDO.
DEF VAR c-inf-empresa   AS CHAR                NO-UNDO.
DEF VAR c-classif-impto AS CHAR                NO-UNDO.
DEF VAR c-forn-impto    AS CHAR                NO-UNDO.
DEF VAR c-cnpj-impto    AS CHAR                NO-UNDO.
DEF VAR c-ende-impto    AS CHAR                NO-UNDO.
DEF VAR c-cep-impto     AS CHAR                NO-UNDO.

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
        RETURN "?".
END.

/************************** Function Definition End *************************/


/****************************** Main Code Begin *****************************/
/*
OUTPUT TO VALUE("V:\Temp\campos.txt") APPEND.
for each tt_param_program_formul:
    EXPORT DELIMITER ";" 
        tt_param_program_formul.tta_cdn_segment_edi
        t_param_program_formul.tta_cdn_element_edi
        t_param_program_formul.tta_des_label_utiliz_formul_edi
        t_param_program_formul.ttv_des_contdo.
end.
OUTPUT CLOSE.
*/
find first tt_param_program_formul no-lock
    WHERE tt_param_program_formul.tta_cdn_segment_edi = 289 
      AND tt_param_program_formul.tta_cdn_element_edi = 3928
    no-error.

IF AVAIL tt_param_program_formul THEN
    assign c-inf-empresa = tt_param_program_formul.ttv_des_contdo.
ELSE
    assign c-inf-empresa = "".

find first item_bord_ap no-lock
    where item_bord_ap.cod_estab_bord = entry(1,c-inf-empresa,';')
      and item_bord_ap.num_id_item_bord_ap = int(entry(2,c-inf-empresa,';')) 
    no-error.

FIND tit_ap NO-LOCK OF ITEM_bord_ap NO-ERROR.

if avail item_bord_ap then
  find first compl_impto_retid_ap NO-LOCK OF tit_ap
/*       where compl_impto_retid_ap.num_id_tit_ap = int(tit_Ap.num_id_tit_ap)
         and compl_impto_retid_ap.cod_estab     = item_bord_ap.cod_estab removido em 05/01/2012 - ainda nao testado */
    no-error.

IF AVAIL compl_impto_retid_ap THEN
DO:
    ASSIGN c-classif-impto = string(compl_impto_retid_ap.cod_classif_impto,'9999'). /* C«digo Receita */

    find first ems5.fornecedor no-lock
        WHERE fornecedor.cod_empresa    = compl_impto_retid_ap.cod_empresa 
          AND fornecedor.cdn_fornecedor = compl_impto_retid_ap.cdn_fornecedor 
        no-error.

    ASSIGN c-forn-impto = fornecedor.nom_pessoa
           c-cnpj-impto = fornecedor.cod_id_feder.

    FIND pessoa_jurid NO-LOCK 
        WHERE pessoa_jurid.num_pessoa_jurid = fornecedor.num_pessoa 
        NO-ERROR.

    IF AVAIL pessoa_jurid THEN
        ASSIGN c-ende-impto = pessoa_jurid.nom_endereco
               c-cep-impto  = pessoa_jurid.cod_cep.
    ELSE
    DO:
        FIND pessoa_fisic NO-LOCK
            WHERE pessoa_fisic.num_pessoa_fisic = fornecedor.num_pessoa 
            NO-ERROR.

        IF AVAIL pessoa_fisic THEN
            ASSIGN c-ende-impto = pessoa_fisic.nom_endereco
                   c-cep-impto  = pessoa_fisic.cod_cep.
    END.
END.
ELSE
    ASSIGN c-classif-impto = "".

/**************************************************************************************/
/* Manter o mesmo numero de remessa entre os portadores com o mesmo convenio **********/
/**************************************************************************************/
DISABLE TRIGGERS FOR LOAD OF portad_edi.
FOR EACH portad_edi EXCLUSIVE-LOCK:
    IF INDEX(portad_edi.des_tip_var_portad_edi,fc_valor(288,3712)) > 0 THEN
        ASSIGN portad_edi.num_prox_remes_msg_edi = DEC(fc_valor(288,3898)) + 1.
END.
/* Fim da rotina de numero de remessa *************************************************/

CASE INT(fc_valor(289,4838)):

    WHEN 16 THEN /* DARF */
        ASSIGN c-retorno = string(1,"9") +                                      	/* IDENTIFICADOR DO TIPO DE REGISTRO                        001 01  N   FIXO "1" */
                           string(DEC(fc_valor(288,3712)),"99999999") +             /* CODIGO DE COMUNICACAO (IDENTIFICADOR DA EMPRESA NO BANCO 002 08  N   SERA FORNECIDO PELO BRADESCO */
                           string(DEC(fc_valor(288,4643)),"999999999999999") +    	/* CNPJ/CPF - BASE DA EMPRESA ADMINISTRADORA                010 09  N   NUMERO BASE DO CNPJ OU CPF DA EMPRESA ADMINISTRADORA */
                           string(DEC(fc_valor(288,3898)),"99999") +                /* NUMERO DO LOTE/REMESSA                                   025 05  N   NUMERO SEQUENCIAL CRESCENTE ( DE 1 EM 1) POR EMPRESA */
                           string(DEC(fc_valor(288,3914)),"99999999") +             /* DATA DE GRAVACAO DO ARQUIVO                              030 08  N   DATA NO FORMATO (AAAAMMDD) */
                           string(1,"999") +                           				/* IDENTIFICADOR DO TRIBUTO                                 038 03  N   001 - DARF */
                           string(0,"99999999999999999999999999") +             	/* IDENTIFICADOR DO DOCUMENTO                               041 26  N   NUMERO DE CONTROLE GERADO PELO BRADESCO CONFORME INDICADOR DO TIPO DE MOVIMENTO QUANDO (I) ENTAO MOVER ZEROS, QUANDO (A) OU (E) ENTAO MOVER DADO RECEBIDO ARQUIVO RETORNO  */
                           string('I',"X") +                                    	/* INDICADOR DO TIPO DE MOVIMENTO                           067 01  A   I - INCLUSAO E - EXCLUSAO A - ALTERACAO */ 
                           string(INT(fc_valor(288,3710)),"9") +                    /* TIPO DE INSCRICAO DO CONTRIBUINTE                        068 01  N   1 - CPF 2 - CNPJ */
                           string(DEC(fc_valor(288,4643)),"999999999999999") +      /* CNPJ/CPF - BASE DO CONTRIBUINTE                          069 09  N   NUMERO BASE DO CNPJ OU CPF DO CONTRIBUINTE */
                           string(fc_valor(288,16),"X(40)") +                       /* NOME DO CONTRIBUINTE                                     084 40  A   RAZAO SOCIAL ( NOME DO CONTRIBUINTE) */
                           string(fc_valor(288,3905),"X(40)") +           			/* ENDERE°O DO CONTRIBUINTE                                 124 40  A   ENDERE°O DO CONTRIBUINTE */
                           string(DEC(fc_valor(288,3908)),"99999999") + 			/* CEP DO CONTRIBUINTE                                      164 05  N   CEP DO CONTRIBUINTE */
                           string(DEC(fc_valor(288,3901)),"9999") +					/* AGENCIA DA EMPRESA ADMINISTRADORA                        172 04  N   CODIGO DA AGENCIA DE DEBITO (EMPRESA ADMINISTRADORA) */
                           string(fc_valor(288,3902),"9") + 						/* DIGITO DA AGENCIA                                        176 01  N   DIGITO DA AGENCIA DE DEBITO (EMPRESA ADMINISTRADORA) */
                           string(0,"9999") + 		      			                /* RAZAO DA CONTA DA EMPRESA ADMINISTRADORA                 177 04  N   RAZAO DA CONTA PARA DEBITO (EMPRESA ADMINISTRADORA) */
                           string(DEC(fc_valor(288,3903)),"9999999") +				/* CONTA DA EMPRESA ADMINISTRADORA                          181 07  N   CONTA DE DEBITO (EMPRESA ADMINISTRADORA) */
                           string(DEC(replace(fc_valor(288,3904),"P","0")),"9") + 	/* DIGITO DA CONTA                                          188 01  N   DIGITO DA CONTA DE DEBITO (EMPRESA ADMINISTRADORA) */
                           string(DEC(fc_valor(289,3709)),"99999999") + 			/* DATA PARA EFETIVACAO DO DEBITO                           189 08  N   DATA NO FORMATO (AAAAMMDD) */
                           string('S',"X") + 										/* INDICADOR PARA AUTORIZACAO DE DEBITO                     197 01  A   DEVERA SER IGUAL A 'S' OU 'N' O DEBITO SERA EFETUADO, NA DATA DE EFETIVACAO INDICADA, QUANDO O CONTEUDO FOR IGUAL A 'S' */
                           string(DEC(fc_valor(289,4421)),"999999999999999") + 		/* VALOR PRINCIPAL                                          198 15  N   VALOR PRINCIPAL (COM DUAS CASAS DECIMAIS) */
                           string(DEC(fc_valor(289,4422)),"999999999999999") + 		/* VALOR DOS JUROS/ENCARGOS                                 213 15  N   VALOR DOS JUROS/ENCARGOS (COM DUAS CASAS DECIMAIS) */
                           string(DEC(fc_valor(289,4426)),"999999999999999") + 		/* VALOR DA MULTA                                           228 15  N   VALOR DA MULTA (COM DUAS CASAS DECIMAIS) */
                           string(DEC(fc_valor(289,4436)),"999999999999999") + 	    /* VALOR TOTAL                                              243 15  N   SOMATORIA DOS CAMPOS VALOR PRINCIPAL, VALOR DA MULTA E VALOR DOS JUROS/ENCARGOS (COM DUAS CASAS DECIMAIS) */
                           string('',"X(72)") + 									/* RESERVA                                                  258 72  A   BRANCOS */
                           string(0 ,"99") + 										/* QUOTA                                                    330 02  N   BRANCOS */
                           string(SUBSTR(fc_valor(289,3704),1,4),"9999") + 		    /* ANO EXERCICIO                                            332 04  N   BRANCOS */
                           string(fc_valor(289,3606),"99999999") + 					/* DATA DE VENCIMENTO                                       336 08  N   DATA NO FORMATO (AAAAMMDD) */
                           string(c-classif-impto,"9999") + 						/* CODIGO DA RECEITA                                        344 04  N   CODIGO DA RECEITA DO DARF */
        				   string(2,"9") + 									  		/* TIPO DE DARF                                             348 01  N   1 - SIMPLES 2 - PRETO EUROPA */
                           string(fc_valor(289,3704),"99999999") +  				/* PERIODO DE APURACAO                                      349 08  N   DATA DO PERIODO DE APURACAO NO FORMATO AAAAMMDD */
                           string(0,"9999") + 										/* PERCENTUAL                                               357 04  N   PERCENTUAL (COM DUAS CASAS DECIMAIS) */
                           string(0,"99999999999999999") + 							/* REFERENCIA                                               361 17  N   NUMERO DA REFERENCIA */
                           string(0,"999999999999999") + 							/* VALOR DA RECEITA BRUTA                                   378 15  N   VALOR DA RECEITA BRUTA (COM DUAS CASAS DECIMAIS) */
                           string('',"XX") + 										/* RESERVA                                                  393 02  A   BRANCOS */
                           string('' ,"X") + 										/* MODALIDADE                                               395 01  N   FIXO ZERO */
                           string('',"X(378)") + 									/* RESERVA                                                  396 378 A   BRANCOS */ 
                           string('',"X(064)") + 									/* AUTENTICACAO DIGITAL                                     774 64  A   BRANCOS */                                                                                                                                                 
                           string('',"X(006)") + 									/* VERSAO DO SOFTWARE OBB UTILIZADA NA MONTAGEM DO ARQUIVO  838 06  A   BRANCOS */
                           string('',"X(006)") + 									/* VERSAO DO SOFTWARE - CONTROLE DO BANCO                   844 06  A   BRANCOS */
                           string('',"X(040)") + 									/* MENSAGEM 1                                               850 40  A   BRANCOS */
                           string('',"X(002)") + 									/* QUANTIDADE DE VIAS                                       890 02  A   BRANCOS */
                           string('',"X(008)") + 									/* MENSAGEM 2                                               892 08  A   BRANCOS */
                           string('',"X(010)") + 									/* CODIGO RETORNO                                           900 10  A   BRANCOS (TERA CONTEUDO SOMENTE QUANDO TRATAR-SE DE ARQUIVO RETORNO) */
                           string('',"X") + 					    				/* AUTORIZACAO PAGAMENTO                                    910 01  N   FIXO ZERO */
                           string('',"X(041)") + 									/* RESERVA                                                  911 41  A   BRANCOS */
                           string(fc_valor(289,3928),"X(016)") + 	                /* CONTROLE DA EMPRESA                                      952 16  A   CONTEUDO DE UTILIZACAO DA EMPRESA */
                           string('',"X(027)") + 									/* AUTENTICACAO                                             968 27  A   BRANCOS (PODERA TER CONTEUDO QUANDO TRATAR-SE DE RETORNO DE PAGAMENTO) */
                           string(DEC(fc_valor(289,2018)),"999999")                 /* SEQUENCIAL */.

    WHEN 17 THEN /* GPS */
        ASSIGN c-retorno = string(1,"9") +                                      	/* IDENTIFICADOR DO TIPO DE REGISTRO                        001 01  N   FIXO "1" */
                           string(DEC(fc_valor(288,3712)),"99999999") +             /* CODIGO DE COMUNICACAO (IDENTIFICADOR DA EMPRESA NO BANCO 002 08  N   SERA FORNECIDO PELO BRADESCO */
                           string(DEC(fc_valor(288,4643)),"999999999999999") +    	/* CNPJ/CPF - BASE DA EMPRESA ADMINISTRADORA                010 09  N   NUMERO BASE DO CNPJ OU CPF DA EMPRESA ADMINISTRADORA */
                           string(DEC(fc_valor(288,3898)),"99999") +                /* NUMERO DO LOTE/REMESSA                                   025 05  N   NUMERO SEQUENCIAL CRESCENTE ( DE 1 EM 1) POR EMPRESA */
                           string(DEC(fc_valor(288,3914)),"99999999") +             /* DATA DE GRAVACAO DO ARQUIVO                              030 08  N   DATA NO FORMATO (AAAAMMDD) */
                           string(5,"999") +                           				/* IDENTIFICADOR DO TRIBUTO                                 038 03  N   005 - GPS */
                           string(0,"99999999999999999999999999") +             	/* IDENTIFICADOR DO DOCUMENTO                               041 26  N   NUMERO DE CONTROLE GERADO PELO BRADESCO CONFORME INDICADOR DO TIPO DE MOVIMENTO QUANDO (I) ENTAO MOVER ZEROS, QUANDO (A) OU (E) ENTAO MOVER DADO RECEBIDO ARQUIVO RETORNO  */
                           string('I',"X") +                                    	/* INDICADOR DO TIPO DE MOVIMENTO                           067 01  A   I - INCLUSAO E - EXCLUSAO A - ALTERACAO */ 
                           string(c-forn-impto,"X(40)") +                           /* NOME DO CONTRIBUINTE */
                           string(c-ende-impto,"X(40)") +                           /* ENDERECO DO CONTRIBUINTE */
                           string(c-cep-impto,"X(08)") +                            /* CEP DO CONTRIBUINTE */
                           string('',"X(15)") +                      								
                           string(DEC(fc_valor(288,3901)),"9999") +					/* AGENCIA DA EMPRESA ADMINISTRADORA */
                           string(fc_valor(288,3902),"9") + 						/* DIGITO DA AGENCIA */
                           string(0,"9999") + 		      			                /* RAZAO DA CONTA DA EMPRESA ADMINISTRADORA */
                           string(DEC(fc_valor(288,3903)),"9999999") +				/* CONTA DA EMPRESA ADMINISTRADORA */
                           string(DEC(replace(fc_valor(288,3904),"P","0")),"9") + 	/* DIGITO DA CONTA */
                           string(fc_valor(289,3709),"99999999") + 
                           string('S',"X") + 														
                           string(DEC(fc_valor(289,4421)),"999999999999999") + 		/* VALOR PRINCIPAL */
                           string(DEC(fc_valor(289,4422)),"999999999999999") + 		/* VALOR DOS JUROS/ENCARGOS */
                           string(DEC(fc_valor(289,4426)),"999999999999999") + 		/* VALOR DA MULTA */
                           string(DEC(fc_valor(289,4436)),"999999999999999") + 	    /* VALOR TOTAL */
                           string(c-classif-impto,"9999") +   						/* CODIGO DA RECEITA */
                           IF INT(fc_valor(288,3710)) = 1 THEN "002" ELSE "001" +   /* TIPO DE INSCRICAO DO CONTRIBUINTE */
                           string(DEC(fc_valor(288,4643)),"99999999999999") +
                           string('',"X(58)") +
                           string(fc_valor(289,3709),"99999999") +
                           string('',"X(3)") +
                           SUBSTR(fc_valor(289,3704),1,4) + 
                           SUBSTR(fc_valor(289,3704),5,2) +
                           string('',"X(599)") +
                           string(fc_valor(289,3928),"X(016)") + 	                /* CONTROLE DA EMPRESA                                      952 16  A   CONTEUDO DE UTILIZACAO DA EMPRESA */
                           string('',"X(027)") + 									/* AUTENTICACAO                                             968 27  A   BRANCOS (PODERA TER CONTEUDO QUANDO TRATAR-SE DE RETORNO DE PAGAMENTO) */
                           string(DEC(fc_valor(289,2018)),"999999")                 /* SEQUENCIAL */.

    OTHERWISE /* codigo de barras */
        ASSIGN c-retorno = string(1,"9") +                                      	/* IDENTIFICADOR DO TIPO DE REGISTRO                        001 01  N   FIXO "1" */
                           string(DEC(fc_valor(288,3712)),"99999999") +             /* CODIGO DE COMUNICACAO (IDENTIFICADOR DA EMPRESA NO BANCO 002 08  N   SERA FORNECIDO PELO BRADESCO */
                           string(DEC(fc_valor(288,4643)),"999999999999999") +    	/* CNPJ/CPF - BASE DA EMPRESA ADMINISTRADORA                010 09  N   NUMERO BASE DO CNPJ OU CPF DA EMPRESA ADMINISTRADORA */
                           string(DEC(fc_valor(288,3898)),"99999") +                /* NUMERO DO LOTE/REMESSA                                   025 05  N   NUMERO SEQUENCIAL CRESCENTE ( DE 1 EM 1) POR EMPRESA */
                           string(DEC(fc_valor(288,3914)),"99999999") +             /* DATA DE GRAVACAO DO ARQUIVO                              030 08  N   DATA NO FORMATO (AAAAMMDD) */
                           string(006,"999") +                                      /* IDENTIFICADOR DO TRIBUTO                                 038 03  N   006 - CODIGO DE BARRAS */
                           string(0,"99999999999999999999999999") +
                           string('I',"X") +
                           string(fc_valor(289,2807),"X(44)") +
                           string(fc_valor(289,3709),"99999999") +
                           string(DEC(fc_valor(289,4436)),"999999999999999") + 	    /* VALOR TOTAL */
                           string(DEC(fc_valor(288,3901)),"9999") +					/* AGENCIA DA EMPRESA ADMINISTRADORA */
                           string(fc_valor(288,3902),"9") + 						/* DIGITO DA AGENCIA */
                           string(0,"9999") + 		      			                /* RAZAO DA CONTA DA EMPRESA ADMINISTRADORA */
                           string(DEC(fc_valor(288,3903)),"9999999") +				/* CONTA DA EMPRESA ADMINISTRADORA */
                           string(DEC(replace(fc_valor(288,3904),"P","0")),"9") + 	/* DIGITO DA CONTA */
                           string('S',"X") +
                           string(0,"999") + 
                           string('',"X(80)") +
                           string(0,"999999") +
                           string('',"X(40)") +
                           string(FILL("0",87),"X(87)") +
                           string('',"X(87)") +
                           string(0,"999999999") +
                           string('',"X(115)") +
                           string(FILL("0",41),"X(41)") +
                           string('',"X(11)") +
                           string(0,"999") +
                           string('',"X(317)") +                      								
                           string(fc_valor(289,3928),"X(016)") + 	                /* CONTROLE DA EMPRESA                                      952 16  A   CONTEUDO DE UTILIZACAO DA EMPRESA */
                           string('',"X(027)") + 									/* AUTENTICACAO                                             968 27  A   BRANCOS (PODERA TER CONTEUDO QUANDO TRATAR-SE DE RETORNO DE PAGAMENTO) */
                           string(DEC(fc_valor(289,2018)),"999999")                 /* SEQUENCIAL */.
END CASE.

return c-retorno.

/******************************* Main Code End ******************************/
