/******************************************************************************
**
**       Programa: ESAPI002
**
**       Data....: 10/06/2009
**
**       Autor...: JULIANA K. OLIVEIRA
**
**       Objetivo: Integracao MI - Frotas x Investimentos
**
**       Versao..: 1.00.000 - Juliana K. Oliveira
**
*******************************************************************************/
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD da-ini           AS DATE FORMAT "99/99/9999"
    FIELD da-fim           AS DATE FORMAT "99/99/9999"
    FIELD l-requisicoes    AS LOGICAL
    FIELD l-ordens         AS LOGICAL.

/** VARIAVEIS **/
DEFINE BUFFER bf-controle-inv-frotas FOR controle-inv-frotas.

DEFINE VARIABLE de-vl-material    LIKE ordem-man.vl-material              NO-UNDO.
DEFINE VARIABLE de-vl-mat-tot     LIKE de-vl-material                     NO-UNDO.
DEFINE VARIABLE de-vl-mob         LIKE ordem-man.vl-mob                   NO-UNDO.
DEFINE VARIABLE de-vl-mob-tot     LIKE de-vl-mob                          NO-UNDO.
DEFINE VARIABLE de-vl-servico     LIKE ordem-man.vl-servico               NO-UNDO.
DEFINE VARIABLE de-vl-ser-tot     LIKE de-vl-servico                      NO-UNDO.
DEFINE VARIABLE de-vl-ggf         LIKE de-vl-mob                          NO-UNDO.
DEFINE VARIABLE de-vl-ggf-tot     LIKE de-vl-ggf                          NO-UNDO.
DEFINE VARIABLE de-quantidade     LIKE movto-mat.quantidade               NO-UNDO.
DEFINE VARIABLE c-ano-mes         LIKE ordem-man.ano-mes                  NO-UNDO.
DEFINE VARIABLE de-vl-mat-conv    LIKE movto-apr.vl-mat                   NO-UNDO.
DEFINE VARIABLE de-vl-mob-conv    LIKE movto-apr.vl-mob                   NO-UNDO.
DEFINE VARIABLE de-vl-mat-est     LIKE movto-apr.vl-mat                   NO-UNDO.
DEFINE VARIABLE de-vl-mob-est     LIKE movto-apr.vl-mob                   NO-UNDO.
DEFINE VARIABLE de-horas          AS   DECIMAL                            NO-UNDO.
DEFINE VARIABLE de-vl-ggf-base    AS   DECIMAL                            NO-UNDO.
DEFINE VARIABLE de-vl-mob-base    AS   DECIMAL                            NO-UNDO.
DEFINE VARIABLE de-vl-ser-base    AS   DECIMAL                            NO-UNDO.
DEFINE VARIABLE de-vl-mat-base    AS   DECIMAL                            NO-UNDO.
DEFINE VARIABLE h-acomp           AS HANDLE                               NO-UNDO.

DEFINE VARIABLE c-mensagem        AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-ord          AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-selec        AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-par          AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-impr         AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-dest         AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-usuar        AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-requis       AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-ordens       AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-par-requis   AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-par-ordens   AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-periodo      AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-destino         AS CHAR                                 NO-UNDO.
DEFINE VARIABLE c-lb-producao     AS CHAR FORMAT "x(30)"                  NO-UNDO.
DEFINE VARIABLE c-lb-ord-prod     AS CHAR FORMAT "x(11)"                  NO-UNDO.
DEFINE VARIABLE c-lb-emissao      AS CHAR FORMAT "x(10)"                  NO-UNDO.
DEFINE VARIABLE c-lb-vl-mat       AS CHAR FORMAT "x(15)"                  NO-UNDO.
DEFINE VARIABLE c-lb-vl-ser       AS CHAR FORMAT "x(15)"                  NO-UNDO.
DEFINE VARIABLE c-lb-vl-mob       AS CHAR FORMAT "x(15)"                  NO-UNDO.
DEFINE VARIABLE c-lb-vl-ggf       AS CHAR FORMAT "x(15)"                  NO-UNDO.
DEFINE VARIABLE c-lb-it-cod       AS CHAR FORMAT "x(16)"                  NO-UNDO.
DEFINE VARIABLE c-lb-quant        AS CHAR FORMAT "x(13)"                  NO-UNDO.
DEFINE VARIABLE c-lb-horas        AS CHAR FORMAT "x(12)"                  NO-UNDO.
/***************/
