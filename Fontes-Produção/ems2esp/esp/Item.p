/*******************************************************************************
Extracao de Estabelecimentos
Kraft Consulting
30/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-desc-tipo-contr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-desc-indservmat AS CHARACTER   NO-UNDO.


FOR EACH item NO-LOCK USE-INDEX codigo:

    ASSIGN c-arquivo = "".

    FIND FIRST es_item
        WHERE es_item.codigo    = ITEM.it-codigo NO-ERROR.

    IF NOT AVAIL es_item THEN DO:
        Create es_item.
        Assign es_item.codigo       =  string(ITEM.it-codigo).   /*chave primaria e unica*/
    END.


    IF ITEM.tipo-contr = 1 THEN 
        ASSIGN c-arquivo = "Normal".
    ELSE IF ITEM.tipo-contr = 2 THEN 
        ASSIGN c-arquivo = "Tranferencia".
    ELSE IF ITEM.tipo-contr = 3 THEN 
        ASSIGN c-arquivo = "Debito GGF".

    CASE ITEM.tipo-contr:
        WHEN 1 THEN
            c-desc-tipo-contr = "Fisico".
        WHEN 2 THEN
            c-desc-tipo-contr = "Total".
        WHEN 3 THEN
            c-desc-tipo-contr = "Consignado".
        WHEN 4 THEN
            c-desc-tipo-contr = "D‚bito Direto".
        WHEN 5 THEN
            c-desc-tipo-contr = "NÆo Definido".
    END CASE.

   IF ITEM.ind-serv-mat = 1 THEN
       c-desc-indservmat = "Servi‡o".
   ELSE
       c-desc-indservmat = "Material".

    ASSIGN es_item.descricao    =  SUBSTRING(ITEM.desc-item,1,61)
           es_item.idUnidade    =  STRING(item.un)          
           es_item.idFamilia    =  STRING(item.fm-codigo)   
           es_item.TipoControle =  c-arquivo
           es_item.TipoCompra   =  STRING({ininc/i09in122.i 04 item.tipo-requis})
           es_item.indservmat   =  item.ind-serv-mat
           es_item.tipocontr    =  item.tipo-contr
           es_item.codigocomplementar = ITEM.codigo-refer
           es_item.informcomplementar = ITEM.inform-compl
           es_item.desc_indservmat   =   c-desc-indservmat
           es_item.desc_tipocontr    =   c-desc-tipo-contr
                       .
    VALIDATE es_item.

END.




