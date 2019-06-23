/****************************************************************************
** Programa: eshr888a.p
** Objetivo: Exporta‡Æo de Funciomnarios
** Autor   : Joao B. C. Bisneto
** Data    : Janeiro/2016.
** Versao  : 2.10.00.000 - Inicial
*****************************************************************************/
/* MESSAGE "eshr999a.p"                   */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
{utp/ut-glob.i}
/*--------------------------------------------------------------------------*/
Define buffer empresa for mguni.empresa.
/*--------------------------------------------------------------------------*/
{esp\ESHR999RP.i}
/*--------------------------------------------------------------------------*/
Define Input Parameter Table For tt-dados.
Define Input Parameter Table For tt-export.
Define Input Parameter Table For tt-param.
/*--------------------------------------------------------------------------*/
Define Variable h-acomp As Handle No-Undo.
/*--------------------------------------------------------------------------*/
Run utp/ut-acomp.p persistent set h-acomp.
Run pi-inicializar In h-acomp (Input "ImpressÆo").
Run pi-seta-tipo   In h-acomp (Input 6).
FIND FIRST tt-param NO-LOCK NO-ERROR.
/*--------------------------------------------------------------------------*/
/* OUTPUT TO c:\temp\bizzzz.txt.                            */
/* FOR EACH tt-dados                                        */
/*   NO-LOCK                                                */
/*   /* WHERE tt-dados.cod_id_feder = "16405188" */         */
/*   BY  tt-dados.cod_id_feder                              */
/*   BY  tt-dados.dat_admis_func:                           */
/*     DISP tt-dados.cod_id_feder  tt-dados.dat_admis_func. */
/*   END.                                                   */
/* OUTPUT CLOSE.                                            */
OUTPUT TO VALUE(tt-param.arq-entrada) NO-CONVERT.  
Find First tt-param No-Lock No-Error.
IF AVAIL tt-param THEN 
  DO:
    IF tt-param.l-cabecalho = YES THEN
      DO:
        RUN pi-imprime-cabec.
      END. /* IF tt-param.l-cabecalho = YES THEN */
    RUN pi-imprime-corpo.
  END. /* IF AVAIL tt-param THEN  */
OUTPUT CLOSE.
RUN pi-finalizar IN h-acomp.
/*--------------------------------------------------------------------------*/
RETURN "OK":U.
/*--------------------------------------------------------------------------*/
PROCEDURE pi-imprime-cabec:
  PUT 
    'Employee Tax ID'                     ";"
    'Additional Employee NO'              ";"
    'Home Business Unit'                  ";"
    'Company-Home - home business unit'   ";"
    /*'Establishment Home - business unit home' ";"*/ 
    'Pay Frequency'                       ";"
    'Employee Pay Status'                 ";"
    'Original Employment-Date'            ";"
    'Pay Starts-Date'                     ";"
    'Date Started'                        ";"
    'Date - Terminated'                   ";"
    'Country Code'                        ";"
    'Address Number'                      ";"
    'Alpha Name'                          ";"
    'Tax ID'                              ";"
    'Search Type'                         ";"
    'Business Unit'                       ";"
    'Mailing Name'                        ";"
    'Address Line 1'                      ";"
    'Address Line 2'                      ";"
    'City'                                ";"
    'State'                               ";"
    'Postal Code'                         ";"
    'Country'                             ";"
    'County'                              ";"
    'Payables'                            ";"
    'AR/AP Netting Indicator'             ";"
    'Category code 28'                    ";"
    'Emplooyee Y/N'                       ";"
    'Name - employee'                     ";"
    'Additional Name'                     ";"
    'Ethic Code'                          ";"
    'Marital Status'                      ";"
    'Date of Birth'                       ";"
    'Language'                            ";"
    'Gender'                              ";"
    'Supervisor'                          ";"
    'Home Business Unit'                  ";"
    'Position ID'                         ";"
    'Job Type'                            ";"
    'Job step'                            ";"
    'Job Title'                           ";"
    'Employment Status'                   ";"
    'Locality'                            ";"
    'Confidential payoll'                 ";"
    'Pay Class'                           ";"
    'Pay Grade'                           ";"
    'Salary'                              ";"
    'ANNUAL'                              ";"
    'Hourly Rate'                         ";"
    'Std Hrs/Day'                         ";"
    'Std Hrs/Year'                        ";"
    'Std Days/Year'                       ";"
    'Overtime Exempt'                     ";"
    'FTE'                                 ";"
    'Labor Distribution Method'           ";"
    'Employee Classification Status'      ";"
    'Record Type'                         ";"
    'Labor Distribution Multiplier'       ";"
    '1st Nationality-Citizenship'         ";"
    'Telefono de Contacto'                ";"
    'Caracteristica - Tel de contacto'    ";"
    'Union Code;'                          ";" SKIP.
END PROCEDURE.
/*-------------------------------------------------------------------
PROCEDURE pi-imprime-corpo:
  FOR EACH tt-dados
    NO-LOCK
    BREAK 
    BY  tt-dados.cod_id_feder
    BY  tt-dados.dat_admis_func:
      IF LAST-OF(tt-dados.dat_admis_func) THEN
        DO:
          /*----------------------------------------------------*/
          FOR EACH tt-export
            NO-LOCK
            WHERE tt-export.cod_id_feder   = tt-dados.cod_id_feder
            AND   tt-export.dat_admis_func = tt-dados.dat_admis_func:
              RUN pi-acompanhar IN h-acomp (
                INPUT "Ger.Arquivo Func: " + 
                STRING(tt-export.cdn_funcionario)).
              RUN pi-corpo-imp.
            END. /* FOR EACH tt-export */
          /*----------------------------------------------------*/
        END.
    END. /* FOR EACH tt-dados */
END PROCEDURE.
-------------------------------------------------------------------*/
PROCEDURE pi-imprime-corpo:

  FOR EACH tt-dados
    NO-LOCK
    /* BREAK */
    BY  tt-dados.cod_id_feder
    BY  tt-dados.dat_admis_func:
      /* IF LAST-OF(tt-dados.dat_admis_func) THEN */
      DO:
        /*----------------------------------------------------*/
        FOR EACH tt-export
          NO-LOCK
          WHERE tt-export.cod_id_feder   = tt-dados.cod_id_feder
          AND   tt-export.dat_admis_func = tt-dados.dat_admis_func
          BY tt-export.dat_desligto_func DESCENDING:
            RUN pi-acompanhar IN h-acomp (
              INPUT "Ger.Arquivo Func: " + 
              STRING(tt-export.cdn_funcionario)).
            RUN pi-corpo-imp.
            LEAVE.
          END.
        /*
        FOR EACH tt-export
          NO-LOCK
          WHERE tt-export.cod_id_feder   = tt-dados.cod_id_feder
          AND   tt-export.dat_admis_func = tt-dados.dat_admis_func:
            RUN pi-acompanhar IN h-acomp (
              INPUT "Ger.Arquivo Func: " + 
              STRING(tt-export.cdn_funcionario)).
            RUN pi-corpo-imp.
          END. /* FOR EACH tt-export */
        */  
        /*----------------------------------------------------*/
      END.
    END. /* FOR EACH tt-dados */
END PROCEDURE.
PROCEDURE pi-corpo-imp:

    DEF VAR v_cod_rh_ccusto LIKE rh_ccusto.cod_rh_ccusto NO-UNDO.

    IF LENGTH(tt-export.cod_rh_ccusto) = 8 THEN
        ASSIGN v_cod_rh_ccusto = tt-export.cdn_estab + SUBSTRING(tt-export.cod_rh_ccusto,03,6).
    ELSE
        ASSIGN v_cod_rh_ccusto = tt-export.cdn_estab + tt-export.cod_rh_ccusto.

  PUT  
    tt-export.cod_id_feder                                     /* FORMAT "x(20)"         */  ";" 
    tt-export.cdn_funcionario                                  /* FORMAT ">>>>>>>9"      */  ";" 
    v_cod_rh_ccusto                                               FORMAT "x(12)"             ";" 
    tt-export.CDN_EMPRESA                                      /* FORMAT ">>>>9"         */  ";"
    /*tt-export.cdn_estab                                          ";"*/
    tt-export.frequencia                                       /* FORMAT "x(1)"          */  ";"
    tt-export.cdn_categ_sal                                       FORMAT "x(30)"             ";"
    tt-export.dat_admis_func                                   /* FORMAT "99/99/9999"    */  ";"
    tt-export.dat_admis_func                                   /* FORMAT "99/99/9999"    */  ";"
    tt-export.dat_admis_func                                   /* FORMAT "99/99/9999"    */  ";"
    tt-export.dat_desligto_func                                /* FORMAT "99/99/9999"    */  ";"
    tt-export.cod_pais_localid                                 /* FORMAT "x(3)"          */  ";"
                                                               /* FORMAT "x(8)"          */  ";"
    TRIM(tt-export.sobre-nome) + "," + TRIM(tt-export.nome)       FORMAT "x(40)"             ";"
    tt-export.cod_id_feder                                     /* FORMAT "x(20)"         */  ";"
    "E"                                                        /* FORMAT "x(1)"          */  ";"
    v_cod_rh_ccusto                                               FORMAT "x(12)"             ";"
    TRIM(tt-export.sobre-nome) + "," + TRIM(tt-export.nome)       FORMAT "x(40)"             ";"
    tt-export.nom_ender_rh                                     /* FORMAT "x(40)"         */  ";"
    tt-export.num_ender_rh   /*NLS*/                           /* FORMAT "x(40)"         */  ";"
    tt-export.nom_cidad_rh                                     /* FORMAT "x(25)"         */  ";"
    tt-export.cod_unid_federec_rh                              /* FORMAT "x(2)"          */  ";"
    tt-export.cod_cep_rh                                       /* FORMAT "x(12)"         */  ";"
    tt-export.cod_cep_rh                                       /* FORMAT "x(3)"          */  ";"
    tt-export.cod_pais_localid                                 /* FORMAT "x(25)"         */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(2)"          */  ";"
                                                               /* FORMAT "x(3)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(50)"         */  ";"
                                                               /* FORMAT "x(20)"         */  ";"
    tt-export.idi_cor_cutis                                    /* FORMAT "x(2)"          */  ";"
    tt-export.idi_estado_civil                                 /* FORMAT "x(1)"          */  ";"
    tt-export.dat_nascimento                                   /* FORMAT "99/99/9999"    */  ";"
    "P"                                                        /* FORMAT "x(1)"          */  ";"
    tt-export.idi_sexo                                         /* FORMAT "x(1)"          */  ";"
    tt-export.cod_id_feder_gestor  /*NLS*/                     /* FORMAT "x(8)"          */  ";"
    v_cod_rh_ccusto                                               FORMAT "x(12)"             ";"
    tt-export.cdn_cargo_basic                                  /* FORMAT "x(8)"          */  ";"
    tt-export.cdn_cargo_basic                                  /* FORMAT ">>>>>9"        */  ";"
                                                               /* FORMAT "x(4)"          */  ";"
    tt-export.des_cargo_basic                                  /* FORMAT "x(30)"         */  ";"
    tt-export.c-STATUS                                         /* FORMAT "x(1)"          */  ";"
    tt-export.cod_pais_localid                                 /* FORMAT "x(8)"          */  ";"
    "N"                                                        /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
    tt-export.val_salario_atual                                /* FORMAT ">>>>,>>9.99"   */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(9)"          */  ";"
                                                               /* FORMAT "x(5)"          */  ";"
                                                               /* FORMAT "x(7)"          */  ";"
                                                               /* FORMAT "x(5)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(9)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(1)"          */  ";"
                                                               /* FORMAT "x(5)"          */  ";"
    tt-export.cod_pais_nasc                                    /* FORMAT "x(3)"          */  ";"
    tt-export.telefone                                            FORMAT "x(20)"             ";"
                                                              /* FORMAT "x(12)"         */   ";"
    STRING(tt-export.union-code)      +                                                               ";" SKIP.
END PROCEDURE.
                                                            
