/****************************************************************************
** Programa: ESHR888RP.P
** Objetivo: Exporta‡Æo de Funciomnarios
** Autor   : Joao B. C. Bisneto
** Data    : Janeiro/2016.
** Versao  : 2.10.00.000 - Inicial
*****************************************************************************/
/* MESSAGE "eshr999rp.p"                  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
{utp/ut-glob.i}
/*--------------------------------------------------------------------------*/
Define buffer empresa for mguni.empresa.
DEFINE BUFFER bf-rh_pessoa_fisic    FOR rh_pessoa_fisic.
DEFINE BUFFER bf-funcionario        FOR funcionario.

/*--------------------------------------------------------------------------*/
DEF STREAM s-cliente.
/*--------------------------------------------------------------------------*/
{esp\eshr999rp.i}
DEF TEMP-TABLE tt-sitafastfunc
  FIELD dat_inic_sit_afast LIKE sit_afast_func.dat_inic_sit_afast
  FIELD dat_term_sit_afast LIKE sit_afast_func.dat_term_sit_afast
  INDEX idx01 AS PRIMARY dat_term_sit_afast.
/*--------------------------------------------------------------------------*/
Define Input Parameter raw-param As Raw No-Undo.
Define Input Parameter Table For tt-raw-digita.
/*--------------------------------------------------------------------------*/
Define Variable h-acomp         As Handle                   No-Undo.
DEFINE VARIABLE l-afastado      AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE l-alt-sal-cargo AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE i-vez           AS INTEGER                  NO-UNDO.
DEFINE VARIABLE c-nome          AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE c-sobre-nome    AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER                  NO-UNDO.
DEFINE VARIABLE c-letra         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-cor-pele      AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-estado-civil  AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-id-feder-gestor AS CHAR                 NO-UNDO.

/*--------------------------------------------------------------------------*/
Create tt-param. 
Raw-Transfer raw-param To tt-param.
Find First tt-param No-Lock No-Error.
/*--------------------------------------------------------------------------*/
Run utp/ut-acomp.p persistent set h-acomp.
Run pi-inicializar In h-acomp (Input "ImpressÆo").
Run pi-seta-tipo   In h-acomp (Input 6).
/*--------------------------------------------------------------------------*/
RUN pi-limpa-tt.
Run pi-gera-dados.
/*--------------------------------------------------------------------------*/
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.
/*--------------------------------------------------------------------------*/
PROCEDURE pi-gera-dados.
  FOR EACH tt-dados:  DELETE tt-dados.  END.
  FOR EACH tt-export: DELETE tt-export. END.
  blk_func:
  FOR EACH funcionario NO-LOCK
    WHERE  funcionario.cdn_empresa      >= STRING(tt-param.emp-ini)
    AND    funcionario.cdn_empresa      <= STRING(tt-param.emp-fim)  
    AND    funcionario.cdn_estab        >= STRING(tt-param.est-ini)  
    AND    funcionario.cdn_estab        <= STRING(tt-param.est-fim)  
    AND    funcionario.cdn_funcionario  >= tt-param.matr-ini 
    AND    funcionario.cdn_funcionario  <= tt-param.matr-fim  ,
    FIRST  RH_PESSOA_FISIC OF FUNCIONARIO NO-LOCK,
    FIRST  RH_CCUSTO       OF FUNCIONARIO NO-LOCK, 
    FIRST  RH_ESTAB        OF FUNCIONARIO NO-LOCK,
    FIRST  UNID_LOTAC      OF FUNCIONARIO NO-LOCK,
    FIRST  cargo_basic     OF funcionario NO-LOCK:
      /*----------------------------------------------------------------------*/
      RUN pi-acompanhar 
        IN h-acomp (INPUT "Exportando funcionarios: " + 
        STRING(funcionario.cdn_funcionario)).
      /*----------------------------------------------------------------------*/
      /* Funcionario admitido no periodo */
      /*
      MESSAGE 
          "dat_admis_func    "  funcionario.dat_admis_func      SKIP 
          "dat_desligto_func "  funcionario.dat_desligto_func   SKIP 
          "cdn_estab         "  funcionario.cdn_estab           SKIP
          "cdn_empresa       "  funcionario.cdn_empresa         SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */    
      IF funcionario.dat_desligto_func  < tt-param.dt-ini THEN
           NEXT blk_func.

      /*05/09/16 - Natanael - Procura ultimo Gestor associado ao Funcionario*/
      FIND LAST es_HistGestor 
          WHERE es_HistGestor.cdn_empresa       = funcionario.cdn_empresa
            AND es_HistGestor.cdn_estab         = funcionario.cdn_estab
            AND es_HistGestor.cdn_funcionario   = funcionario.cdn_funcionario
          NO-LOCK NO-ERROR.
      IF AVAIL es_HistGestor THEN DO:
          FIND es_gestor
              WHERE es_gestor.cdn_gestor = es_HistGestor.cdn_gestor NO-LOCK NO-ERROR.
    
          IF AVAIL es_gestor AND es_gestor.origem = 1 /*Interno*/ THEN DO:
              FIND bf-funcionario
                  WHERE bf-funcionario.cdn_empresa     = es_gestor.cdn_empresa
                    AND bf-funcionario.cdn_estab       = es_gestor.cdn_estab
                    AND bf-funcionario.cdn_funcionario = es_gestor.cdn_funcionario
                  NO-LOCK NO-ERROR.
        
              FIND bf-rh_pessoa_fisic
                  WHERE bf-rh_pessoa_fisic.num_pessoa_fisic = bf-funcionario.num_pessoa_fisic
                  NO-LOCK NO-ERROR.
         
              ASSIGN c-id-feder-gestor = bf-rh_pessoa_fisic.cod_id_feder.
          END.
          ELSE 
              ASSIGN c-id-feder-gestor = STRING(es_gestor.cdn_gestor,"99999999999").
      END.
      ELSE
          ASSIGN c-id-feder-gestor = "".

      /*** Fim Nataanel ***/

      /*----------------------------------------------------------------------*/
      RUN pi-afastado.
      RUN pi-alt-sal-cargo.
      RUN pi-nome-sobre-nome.
      RUN pi-cor-pele.
      RUN pi-estado-civil.
      RUN pi-tt-export.
      RUN pi-status-func.
      RUN pi-tt-dados.
      /*----------------------------------------------------------------------*/
    END. /* FOR EACH funcionario NO-LOCK */
  /*---------------------------------------------------------------*/
  RUN esp/eshr999a.p(
    INPUT TABLE tt-dados,
    INPUT TABLE tt-export,
    INPUT TABLE tt-param).
  /*---------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-afastado:
  DEF VAR dt-data AS DATE NO-UNDO.
  ASSIGN l-afastado = NO.
  FOR EACH tt-sitafastfunc: DELETE tt-sitafastfunc. END.
  blk_sitafastfunc:
  FOR EACH sit_afast_func
    NO-LOCK
    WHERE sit_afast_func.cdn_empresa              = funcionario.cdn_empresa    
    AND   sit_afast_func.cdn_estab                = funcionario.cdn_estab      
    AND   sit_afast_func.cdn_funcionario          = funcionario.cdn_funcionario:
    IF sit_afast_func.dat_inic_sit_afast > tt-param.dt-fim THEN
      NEXT blk_sitafastfunc.
    IF sit_afast_func.dat_term_sit_afast < tt-param.dt-fim THEN
      NEXT blk_sitafastfunc.
    FIND sit_afast
      NO-LOCK 
      WHERE sit_afast.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func
      NO-ERROR.
    IF AVAIL sit_afast AND sit_afast.idi_signif_sit <> 2 THEN
      NEXT blk_sitafastfunc.
    CREATE tt-sitafastfunc.
    ASSIGN 
      tt-sitafastfunc.dat_inic_sit_afast = sit_afast_func.dat_inic_sit_afast
      tt-sitafastfunc.dat_term_sit_afast = sit_afast_func.dat_term_sit_afast.
  END. /* FOR EACH sit_afast_func */
  FOR LAST tt-sitafastfunc
    WHERE tt-sitafastfunc.dat_term_sit_afast >= tt-param.dt-fim:
    ASSIGN l-afastado = YES.
  END. /* FOR LAST tt-sitafastfunc */ 
  /*----------------------------------------------------------------------
  Comentado Prada e Saturnino (Bisneto)
  Ficou acertado a regra do bloco acima
  FOR EACH sit_afast_func
    NO-LOCK
    WHERE sit_afast_func.cdn_sit_afast_func >= 0
    AND   sit_afast_func.cdn_sit_afast_func <= 99 
    AND   sit_afast_func.cdn_empresa         = funcionario.cdn_empresa    
    AND   sit_afast_func.cdn_estab           = funcionario.cdn_estab      
    AND   sit_afast_func.cdn_funcionario     = funcionario.cdn_funcionario
    AND   sit_afast_func.dat_inic_sit_afast >= tt-param.dt-ini 
    AND   sit_afast_func.dat_inic_sit_afast <= tt-param.dt-fim
    BREAK BY :
      IF 
        sit_afast_func.cdn_sit_afast_func = 5 OR
        sit_afast_func.cdn_sit_afast_func = 6 OR
        sit_afast_func.cdn_sit_afast_func = 8 OR
        sit_afast_func.cdn_sit_afast_func = 10 OR
        sit_afast_func.cdn_sit_afast_func = 12 OR
        sit_afast_func.cdn_sit_afast_func = 13 OR
        sit_afast_func.cdn_sit_afast_func = 14 OR
        sit_afast_func.cdn_sit_afast_func = 17 OR
        sit_afast_func.cdn_sit_afast_func = 18 OR
        sit_afast_func.cdn_sit_afast_func = 20 OR
        sit_afast_func.cdn_sit_afast_func = 25 OR
        sit_afast_func.cdn_sit_afast_func = 26 OR
        sit_afast_func.cdn_sit_afast_func = 27 OR
        sit_afast_func.cdn_sit_afast_func = 28 OR
        sit_afast_func.cdn_sit_afast_func = 29 OR
        sit_afast_func.cdn_sit_afast_func = 30 OR
        sit_afast_func.cdn_sit_afast_func = 35 OR
        sit_afast_func.cdn_sit_afast_func = 36 OR
        sit_afast_func.cdn_sit_afast_func = 37 OR
        sit_afast_func.cdn_sit_afast_func = 38 OR
        sit_afast_func.cdn_sit_afast_func = 39 OR
        sit_afast_func.cdn_sit_afast_func = 40 OR
        sit_afast_func.cdn_sit_afast_func = 41 OR
        sit_afast_func.cdn_sit_afast_func = 42 OR
        sit_afast_func.cdn_sit_afast_func = 44 OR
        sit_afast_func.cdn_sit_afast_func = 45 OR
        sit_afast_func.cdn_sit_afast_func = 46 OR
        sit_afast_func.cdn_sit_afast_func = 47 OR
        sit_afast_func.cdn_sit_afast_func = 55 OR
        sit_afast_func.cdn_sit_afast_func = 59 OR
        sit_afast_func.cdn_sit_afast_func = 61 OR
        sit_afast_func.cdn_sit_afast_func = 63 OR
        sit_afast_func.cdn_sit_afast_func = 65 OR
        sit_afast_func.cdn_sit_afast_func = 67 OR
        sit_afast_func.cdn_sit_afast_func = 68 OR
        sit_afast_func.cdn_sit_afast_func = 70 OR
        sit_afast_func.cdn_sit_afast_func = 71 OR
        sit_afast_func.cdn_sit_afast_func = 73 OR
        sit_afast_func.cdn_sit_afast_func = 75 OR
        sit_afast_func.cdn_sit_afast_func = 80 OR
        sit_afast_func.cdn_sit_afast_func = 81 OR
        sit_afast_func.cdn_sit_afast_func = 82 OR
        sit_afast_func.cdn_sit_afast_func = 83 OR
        sit_afast_func.cdn_sit_afast_func = 84 OR
        sit_afast_func.cdn_sit_afast_func = 85 OR
        sit_afast_func.cdn_sit_afast_func = 86 OR
        sit_afast_func.cdn_sit_afast_func = 90 OR
        sit_afast_func.cdn_sit_afast_func = 91 OR
        sit_afast_func.cdn_sit_afast_func = 92 OR
        sit_afast_func.cdn_sit_afast_func = 93 OR
        sit_afast_func.cdn_sit_afast_func = 94 OR
        sit_afast_func.cdn_sit_afast_func = 96 THEN
          ASSIGN l-afastado = YES.
      
    END. /* FOR EACH sit_afast_func */
  ----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-alt-sal-cargo:
  /*----------------------------------------------------------------------*/
  ASSIGN l-alt-sal-cargo = NO.
  /*----------------------------------------------------------------------*/
  FOR EACH histor_sal_func NO-LOCK 
    WHERE histor_sal_func.cdn_empresa      = funcionario.cdn_empresa
    AND   histor_sal_func.cdn_estab        = funcionario.cdn_estab
    AND   histor_sal_func.cdn_funcionario  = funcionario.cdn_funcionario
    AND   histor_sal_func.dat_liber_sal   >= tt-param.dt-ini
    AND   histor_sal_func.dat_liber_sal   <= tt-param.dt-fim.
      /*----------------------------------------------------------------------*/
      ASSIGN l-alt-sal-cargo = YES.
      /*----------------------------------------------------------------------*/
    END. /* FOR EACH histor_sal_func NO-LOCK */
  /*----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-nome-sobre-nome:
  ASSIGN 
    i-vez        = 1 
    c-nome       = ""
    c-sobre-nome = "".
  /*----------------------------------------------------------------------*/
  DO i-cont = 1 TO 60.
    ASSIGN c-letra = "".
    ASSIGN c-letra = SUBSTRING(RH_PESSOA_FISIC.nom_pessoa_fisic,i-cont,1).
    IF i-vez = 1 THEN 
      DO:
        ASSIGN c-nome = c-nome + c-letra.
        IF c-letra = "" THEN
        ASSIGN i-vez = 2.
      END.
    IF i-vez = 2 THEN
      ASSIGN c-sobre-nome = c-sobre-nome + c-letra.
  END.
  /*----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-cor-pele:
  /*----------------------------------------------------------------------*/
  ASSIGN c-cor-pele = "".
  IF rh_pessoa_fisic.idi_cor_cutis = 1 THEN   /* Branca */
    ASSIGN c-cor-pele = "01" .             
  ELSE
  IF rh_pessoa_fisic.idi_cor_cutis = 2 THEN   /* Negra */
    ASSIGN c-cor-pele = "02" .
  ELSE        
  IF rh_pessoa_fisic.idi_cor_cutis = 3 THEN   /* Parda */
    ASSIGN c-cor-pele = "03" .
  ELSE
  IF rh_pessoa_fisic.idi_cor_cutis = 4 THEN   /* Amarela */
    ASSIGN c-cor-pele = "04" .
  ELSE
  IF rh_pessoa_fisic.idi_cor_cutis = 5 THEN   /* Outros */
    ASSIGN c-cor-pele = "05" .
  ELSE
  IF rh_pessoa_fisic.idi_cor_cutis = 6 THEN   /* Indigena */
    ASSIGN c-cor-pele = "06" .
  /*----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-estado-civil:
  /*----------------------------------------------------------------------*/
  ASSIGN c-estado-civil = "" .
  IF rh_pessoa_fisic.idi_estado_civil = 1 THEN    /* casado */
      ASSIGN c-estado-civil = "1" .
  ELSE
  IF rh_pessoa_fisic.idi_estado_civil = 2 THEN    /* solteiro */
      ASSIGN c-estado-civil = "2" .
  ELSE                                
  IF rh_pessoa_fisic.idi_estado_civil = 3 THEN    /* desquitado */
      ASSIGN c-estado-civil = "3" .
  ELSE
  IF rh_pessoa_fisic.idi_estado_civil = 4 THEN    /* divorciado */
      ASSIGN c-estado-civil = "4" .
  ELSE
  IF rh_pessoa_fisic.idi_estado_civil = 5 THEN    /* viuvo */
      ASSIGN c-estado-civil = "5" .
  ELSE
  IF rh_pessoa_fisic.idi_estado_civil = 6 THEN    /* separado */
      ASSIGN c-estado-civil = "6" .
  ELSE
  IF rh_pessoa_fisic.idi_estado_civil = 7 THEN    /* outros */
      ASSIGN c-estado-civil = "7" .
  /*----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-tt-export:
  DEF VAR c-tp-contratacao AS CHAR EXTENT 15 FORMAT "X(20)"
    INIT ["Funcion rio", "Estagi rio", "Aposentado", "Empregador", "Prazo Determinado",
          "Tempo Parcial", "Aprendiz", "Reservado", "Reservado", "Tempor rio", "Cooperado",
          "Contratado", "S¢cio Quotista", "Estagi rio Contratado", "Terceiros - Ponto"]
      NO-UNDO.
  /*----------------------------------------------------------------------*/
  create tt-export.
  Assign 
    tt-export.cod_id_feder          = rh_pessoa_fisic.cod_id_feder /* CPF */
    tt-export.cdn_funcionario       = funcionario.cdn_funcionario
    tt-export.cod_rh_ccusto         = funcionario.cod_rh_ccusto
    tt-export.CDN_EMPRESA           = FUNCIONARIO.CDN_EMPRESA  
    tt-export.frequencia            = "M"
    tt-export.cdn_categ_sal         = c-tp-contratacao[funcionario.idi_tip_func] /* IF funcionario.cdn_categ_sal = 1 THEN "M" ELSE "H" */
    tt-export.dat_admis_func        = funcionario.dat_admis_func
    tt-export.dat_desligto_func     = funcionario.dat_desligto_func     
    tt-export.cod_pais_localid      = funcionario.cod_pais_localid
    tt-export.nome                  = c-nome
    tt-export.sobre-nome            = c-sobre-nome         
    tt-export.nom_ender_rh          = rh_pessoa_fisic.nom_ender_rh
    tt-export.nom_cidad_rh          = rh_pessoa_fisic.nom_cidad_rh
    tt-export.cod_unid_federec_rh   = rh_pessoa_fisic.cod_unid_federac_rh
    tt-export.cod_cep_rh            = rh_pessoa_fisic.cod_cep_rh                                                                             
    tt-export.idi_cor_cutis         = c-cor-pele  
    tt-export.idi_estado_civil      = c-estado-civil 
    tt-export.dat_nascimento        = rh_pessoa_fisic.dat_nascimento
    tt-export.idi_sexo              = IF rh_pessoa_fisic.idi_sexo = 1 THEN "M" ELSE "F"
    tt-export.cdn_cargo_basic       = funcionario.cdn_cargo_basic  
    tt-export.des_cargo_basic       = cargo_basic.des_cargo_basic
    tt-export.val_salario_atual     = funcionario.val_salario_atual    
    tt-export.cod_pais_nasc         = rh_pessoa_fisic.cod_pais_nasc 
    tt-export.telefone              = string(rh_pessoa_fisic.num_telefone,"999999999")
    tt-export.union-code            = funcionario.cdn_sindicato
    tt-export.cdn_estab             = funcionario.cdn_estab
    /*05/09/16 - Natanael Silva */
    tt-export.cod_id_feder_gestor   = c-id-feder-gestor
    tt-export.num_ender_rh          = SUBSTRING(rh_pessoa_fisic.cod_livre_1,66,8).
    /*** Fim Natanael ***/
                                                                                    
  /*----------------------------------------------------------------------*/
END PROCEDURE.
/*----------------------------------------------------------------------
PROCEDURE pi-tt-dados:
  FIND FIRST tt-dados
    NO-LOCK
    WHERE tt-dados.CDN_EMPRESA      = funcionario.CDN_EMPRESA                       
    AND   tt-dados.cdn_estab        = funcionario.cdn_estab                         
    AND   tt-dados.cdn_funcionario  = funcionario.cdn_funcionario                   
    AND   tt-dados.nom_pessoa_fisic = funcionario.nom_pessoa_fisic  
    AND   tt-dados.cod_id_feder     = rh_pessoa_fisic.cod_id_feder /* CPF */
    AND   tt-dados.dat_admis_func   = funcionario.dat_admis_func
    NO-ERROR.
  IF NOT AVAIL tt-dados THEN
    DO:
      CREATE tt-dados.
      ASSIGN 
        tt-dados.CDN_EMPRESA      = funcionario.CDN_EMPRESA     
        tt-dados.cdn_estab        = funcionario.cdn_estab       
        tt-dados.cdn_funcionario  = funcionario.cdn_funcionario 
        tt-dados.nom_pessoa_fisic = funcionario.nom_pessoa_fisic
        tt-dados.cod_id_feder     = rh_pessoa_fisic.cod_id_feder
        tt-dados.dat_admis_func   = funcionario.dat_admis_func.
    END.
END PROCEDURE.
----------------------------------------------------------------------*/
PROCEDURE pi-tt-dados:
  FIND FIRST tt-dados
    NO-LOCK
    WHERE /* tt-dados.CDN_EMPRESA      = funcionario.CDN_EMPRESA                       
    AND   tt-dados.cdn_estab        = funcionario.cdn_estab                         
    AND   tt-dados.cdn_funcionario  = funcionario.cdn_funcionario                   
    AND   tt-dados.nom_pessoa_fisic = funcionario.nom_pessoa_fisic 
    AND   */  tt-dados.cod_id_feder     = rh_pessoa_fisic.cod_id_feder /* CPF */
    /* AND   tt-dados.dat_admis_func   = funcionario.dat_admis_func */ 
    NO-ERROR.
  IF NOT AVAIL tt-dados THEN
    DO:
      CREATE tt-dados.
      ASSIGN 
        /* tt-dados.CDN_EMPRESA      = funcionario.CDN_EMPRESA     
        tt-dados.cdn_estab        = funcionario.cdn_estab       
        tt-dados.cdn_funcionario  = funcionario.cdn_funcionario 
        tt-dados.nom_pessoa_fisic = funcionario.nom_pessoa_fisic */
        tt-dados.cod_id_feder     = rh_pessoa_fisic.cod_id_feder
        tt-dados.dat_admis_func   = funcionario.dat_admis_func.
    END.
  ELSE
    DO:
      IF funcionario.dat_admis_func > tt-dados.dat_admis_func THEN
        DO:
          ASSIGN tt-dados.dat_admis_func = funcionario.dat_admis_func.
        END.
    END.
END PROCEDURE.
PROCEDURE pi-status-func:
  ASSIGN tt-export.c-STATUS = "F".
  IF l-afastado = NO THEN 
    DO:
      IF funcionario.dat_desligto_func <> ? THEN
        DO:
          IF funcionario.dat_desligto_func <= tt-param.dt-fim THEN
            DO:
              ASSIGN tt-export.c-STATUS = "T".
            END.
          ELSE
            ASSIGN tt-export.c-STATUS = "1".
        END.
      ELSE
        ASSIGN tt-export.c-STATUS = "1".
    END.
END PROCEDURE.
/*----------------------------------------------------------------------*/
PROCEDURE pi-limpa-tt:
  FOR EACH tt-export: DELETE tt-export. END.
  FOR EACH tt-dados:  DELETE tt-dados.  END.
END PROCEDURE.
