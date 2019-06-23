DEF TEMP-TABLE tt-cross-reference LIKE conta-contab
    FIELD cod_unid_organ        LIKE trad_org_ext.cod_unid_organ
    FIELD des_tit_ctbl          LIKE cta_ctbl.des_tit_ctbl
    FIELD des_ccusto            LIKE ems5.ccusto.des_tit_ctbl
    FIELD lancto-contas-pagar   AS LOG INIT NO
    FIELD lancto-contas-receber AS LOG INIT NO
    FIELD lancto-estoque        AS LOG INIT NO
    FIELD lancto-faturamento    AS LOG INIT NO
    FIELD lancto-frotas         AS LOG INIT NO
    FIELD observacao            AS CHAR.
