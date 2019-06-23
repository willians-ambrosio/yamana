/****************************************************************************
** Programa: ESHR888RP.i
** Objetivo: Exporta‡Æo de Funciomnarios
** Autor   : Joao B. C. Bisneto
** Data    : Janeiro/2016.
** Versao  : 2.10.00.000 - Inicial
*****************************************************************************/
define temp-table tt-param
  field destino         as integer
  field arq-destino     as char
  field arq-entrada     as char
  field usuario         as char
  field data-exec       as date
  field hora-exec       as integer
  FIELD rs-todos        AS INT
  FIELD emp-ini         AS INT
  FIELD emp-fim         AS INT
  FIELD est-ini         AS INT
  FIELD est-fim         AS INT
  FIELD matr-ini        AS INT
  FIELD matr-fim        AS INT
  FIELD dt-ini          AS DATE
  FIELD dt-fim          AS DATE
  FIELD l-cabecalho     AS LOG.
Define Temp-Table tt-raw-digita Field raw-digita As Raw.
define temp-table tt-export
  field cod_id_feder         LIKE rh_pessoa_fisic.cod_id_feder                         /* CPF */  
  field cod_id_feder_gestor  LIKE rh_pessoa_fisic.cod_id_feder                         /* CPF */  
  FIELD cdn_funcionario      LIKE funcionario.cdn_funcionario                                                                       
  field cod_rh_ccusto        LIKE funcionario.cod_rh_ccusto                                                                         
  field CDN_EMPRESA          LIKE FUNCIONARIO.CDN_EMPRESA                                                                           
  field frequencia           as character 
  FIELD cdn_categ_sal        AS CHARACTER   
  FIELD dat_admis_func       LIKE funcionario.dat_admis_func                                                                        
  FIELD dat_desligto_func    LIKE funcionario.dat_desligto_func                                                                     
  FIELD cod_pais_localid     LIKE funcionario.cod_pais_localid                                                                      
  FIELD nome                 AS CHARACTER   
  FIELD sobre-nome           AS CHARACTER   
  FIELD nom_ender_rh         LIKE rh_pessoa_fisic.nom_ender_rh                                                                      
  FIELD nom_cidad_rh         LIKE rh_pessoa_fisic.nom_cidad_rh  
  FIELD num_ender_rh         AS CHAR 
  FIELD cod_unid_federec_rh  LIKE rh_pessoa_fisic.cod_unid_federac_rh                                                               
  FIELD cod_cep_rh           LIKE rh_pessoa_fisic.cod_cep_rh                                                                        
  FIELD idi_cor_cutis        AS CHARACTER   
  FIELD idi_estado_civil     AS CHARACTER   
  FIELD dat_nascimento       LIKE rh_pessoa_fisic.dat_nascimento                                                                    
  FIELD idi_sexo             AS CHARACTER 
  FIELD cdn_cargo_basic      LIKE funcionario.cdn_cargo_basic                                                                       
  FIELD des_cargo_basic      LIKE cargo_basic.des_cargo_basic                                                                       
  FIELD val_salario_atual    LIKE funcionario.val_salario_atual                                                                     
  FIELD cod_pais_nasc        LIKE rh_pessoa_fisic.cod_pais_nasc                                                                     
  FIELD telefone             AS CHARACTER   
  FIELD c-STATUS             AS CHARACTER
  FIELD union-code           LIKE funcionario.cdn_sindicato
  FIELD cdn_estab            LIKE funcionario.cdn_estab.


DEFINE TEMP-TABLE tt-dados
  FIELD CDN_EMPRESA          LIKE funcionario.CDN_EMPRESA      COLUMN-LABEL "Empresa"
  FIELD cdn_estab            LIKE funcionario.cdn_estab        COLUMN-LABEL "Estabelecimento"
  FIELD cdn_funcionario      LIKE funcionario.cdn_funcionario  COLUMN-LABEL "Matr.Func."
  FIELD nom_pessoa_fisic     LIKE funcionario.nom_pessoa_fisic COLUMN-LABEL "Nome"
  FIELD cod_id_feder         LIKE rh_pessoa_fisic.cod_id_feder COLUMN-LABEL "CPF"
  FIELD dat_admis_func       LIKE funcionario.dat_admis_func   COLUMN-LABEL "Dt Admiss".
