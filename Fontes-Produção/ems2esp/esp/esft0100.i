/* --------------------------------------------------------------------------------------- 
                                                                                        
   Sistema................: TOTVS                                                       
   Modulo.................:                                                  
                                                                                           
   Programa...............: esft0100                                 
   Sub Programa...........:                                                                
                                                                                            
   Descricao..............: Integra‡Æo TOTVS x MES ordens de Produ‡Æo
   programa para consultar: 
                             
   Entidade Desenvolvedora: DSC PRAXIS
   
   tabelas usadas.........: 
   importante.............:  
                                                                                           
   Historico Programa -------------------------------------------------------------------+ 
   | Data       | Autor               | Descricao                                        | 
   +----------- +---------------------+--------------------------------------------------+ 
   | 21/02/2017 | Marcos A.Souza      | Desenvolvimento do Programa                      | 
   +------------+---------------------+--------------------------------------------------+ */
   
 define temp-table tt-param no-undo
     field destino          as integer
     field arquivo          as char format "x(35)"
     field usuario          as char format "x(12)"
     field data-exec        as date
     field hora-exec        as integer
     field classifica       as integer
     field desc-classifica  as char format "x(40)"
     field modelo-rtf       as char format "x(35)"
     field l-habilitaRtf    as LOG     
     FIELD dispositivo      AS CHARACTER FORMAT 'x(256)'
     FIELD cod-estabel-ini  LIKE es_it_nota_fisc.cod_estabel          
     FIELD cod-estabel-fim  LIKE es_it_nota_fisc.cod_estabel          
     FIELD ep-codigo        LIKE es_it_nota_fisc.ep_codigo            
     FIELD dt-emis-nota-ini LIKE es_it_nota_fisc.dt_emis_nota         
     FIELD dt-emis-nota-fim LIKE es_it_nota_fisc.dt_emis_nota         
     FIELD it-codigo-ini    LIKE es_it_nota_fisc.it_codigo            
     FIELD it-codigo-fim    LIKE es_it_nota_fisc.it_codigo
     FIELD l-rpw            AS LOGICAL
     FIELD mesCorent        AS LOGICAL.

 define temp-table tt-digita no-undo
     field lote-gl          as character format "x(40)" LABEL "Lote GL"
     field lote             as character format "x(40)" LABEL "Lote Fornec"
     index id lote-gl lote.

 define buffer b-tt-digita for tt-digita.

