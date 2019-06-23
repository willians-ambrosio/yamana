
/******************************************************************************
***
*** esp/esap0749rp.i3 - Form's
***
******************************************************************************/

form 
     tt-mov-ap.dat_transacao       format "99/99/9999"             column-label "Dt-Trans"                 
     tt-mov-ap.cod_estab           format "x(5)"                   column-label "Estab"
     i-cod-gr-forn                 format "x(4)"                   column-label "Grp Forn"
     c-descricao                   format "x(40)"                  column-label "Descri‡Æo"
     tt-mov-ap.cdn_fornecedor      format ">>>>>>>>9"              column-label "Fornec"
     tt-mov-ap.nom_abrev           format "X(15)"                  column-label "Nom-Abrev"            
     tt-mov-ap.cod_espec_docto     format "x(3)"                   column-label "Esp"
     tt-mov-ap.cod_ser_docto       format "x(3)"                   column-label "Ser"           
     tt-mov-ap.cod_tit_ap          format "x(10)"                  column-label "Docto"           
     tt-mov-ap.cod_parcela         format "x(02)"      
     tt-mov-ap.dat_emis_docto      format "99/99/9999"             column-label "Dt-Emis"
     tt-mov-ap.dat_vencto_tit_ap   format "99/99/9999"             column-label "Dt-Venc"           
     tt-mov-ap.cod_refer           format "x(10)"                  column-label "Refer"           
     tt-mov-ap.ind_trans_ap        format "x(26)"                     column-label "Tr"
     tt-mov-ap.dat_gerac_movto     format "99/99/9999"             column-label "Dt-Maq"
     tt-mov-ap.ind_origin_tit_ap   format "x(3)"                     column-label "Or"           
     tt-mov-ap.val_movto_ap        format "->>>,>>>,>>9.99"     column-label "Vl-Movto"
        with width 600 no-box page-top stream-io frame f-cabec-1.

form 
     tt-mov-ap.dat_transacao       format "99/99/9999"             column-label "Dt-Trans"                 
     tt-mov-ap.cod_estab           format "x(5)"                   column-label "Estab"
     i-cod-gr-forn                 format "x(4)"                   column-label "Grp"
     c-descricao                   format "x(40)"                  column-label "Descri‡Æo"
     tt-mov-ap.cdn_fornecedor      format ">>>>>>>>9"              column-label "Fornec"     
     tt-mov-ap.nom_abrev           format "X(15)"                  column-label "Nom-Abrev"            
     tt-mov-ap.cod_espec_docto     format "x(3)"                   column-label "Esp"
     tt-mov-ap.cod_ser_docto       format "x(3)"                   column-label "Ser"           
     tt-mov-ap.cod_tit_ap          format "x(10)"                  column-label "Docto"           
     tt-mov-ap.cod_parcela         format "x(02)"                  column-label "Pr"
     tt-mov-ap.dat_emis_docto      format "99/99/9999"             column-label "Dt-Emis"
     tt-mov-ap.dat_vencto_tit_ap   format "99/99/9999"             column-label "Dt-Venc"           
     tt-mov-ap.cod_refer           format "x(10)"                  column-label "Refer"           
     tt-mov-ap.ind_trans_ap        format "x(26)"                     column-label "Tr"
     tt-mov-ap.dat_gerac_movto     format "99/99/9999"             column-label "Dt-Maq"          
     tt-mov-ap.ind_origin_tit_ap   format "x(3)"                     column-label "Or"           
     tt-mov-ap.val_movto_ap        format "->>>>>>,>>>,>>9.99"     column-label "Vl-Movto"
     de-vl-liquido                 format "->>>>>>>>,>>9.99"       column-label "Vl Liquido"
        with width 600 no-box page-top stream-io frame f-cabec-2.

form 
     tt-mov-ap.dat_transacao      format "99/99/9999"             /*at 003*/
     tt-mov-ap.cod_estab          format "x(5)"                   /*at 016*/
     i-cod-gr-forn                format "x(4)"
     c-descricao                  format "x(40)"
     tt-mov-ap.cdn_fornecedor     format ">>>>>>>>9"              /*at 020*/
     tt-mov-ap.nom_abrev          format "X(15)"                  /*at 030*/
     tt-mov-ap.cod_espec_docto    format "x(3)"                     /*at 045*/
     tt-mov-ap.cod_ser_docto      format "x(3)"                     /*at 077     */
     tt-mov-ap.cod_tit_ap         format "x(10)"                    /*at 083     */
     tt-mov-ap.cod_parcela        format "x(02)"                    /*at 100     */
     tt-mov-ap.dat_emis_docto     format "99/99/9999"               /*at 103     */
     tt-mov-ap.dat_vencto_tit_ap  format "99/99/9999"               /*at 114     */
     tt-mov-ap.cod_refer          format "x(10)"                    /*at 125     */
     tt-mov-ap.ind_trans_ap       format "x(26)"                    /*   at 136  */
     tt-mov-ap.dat_gerac_movto    format "99/99/9999"               /* at 139    */
     tt-mov-ap.ind_origin_tit_ap  format "x(3)"                     /*  at 150   */
     tt-mov-ap.val_movto_ap       format "->>>>>>,>>>,>>9.99"       /*at 153     */
        with down width 300 no-box no-label stream-io frame f-det-1.
        
form 
     tt-mov-ap.dat_transacao      format "99/99/9999"             /*at 003*/
     tt-mov-ap.cod_estab          format "x(5)"                   /*at 016*/
     i-cod-gr-forn                format "x(4)"
     c-descricao                  format "x(40)"
     tt-mov-ap.cdn_fornecedor     format ">>>>>>>>9"              /*at 020*/
     tt-mov-ap.nom_abrev          format "X(15)"                  /*at 030*/
     tt-mov-ap.cod_espec_docto    format "x(3)"                     /*at 045*/
     tt-mov-ap.cod_ser_docto      format "x(3)"                     /*at 077        */
     tt-mov-ap.cod_tit_ap         format "x(10)"                    /*at 083        */
     tt-mov-ap.cod_parcela        format "x(02)"                    /*at 100        */
     tt-mov-ap.dat_emis_docto     format "99/99/9999"               /*at 103        */  
     tt-mov-ap.dat_vencto_tit_ap  format "99/99/9999"               /*at 114        */
     tt-mov-ap.cod_refer          format "x(10)"                    /*at 125        */
     tt-mov-ap.ind_trans_ap       format "x(26)"                    /*   at 136     */
     tt-mov-ap.dat_gerac_movto   format "99/99/9999"                /*at 139        */
     tt-mov-ap.ind_origin_tit_ap  format "x(3)"                     /*  at 150      */
     tt-mov-ap.val_movto_ap       format "->>>>>>,>>>,>>9.99"       /*at 153        */
     de-vl-liquido                format "->>>>>>>>>,>>9.99"        /*at 177        */
        with down width 300 no-box no-label stream-io frame f-det-2.   

form    
     c-par                     no-label
     skip
     tt-param.l-tit-subs   format "Sim/NÆo" colon 20
     skip
     tt-param.l-nota-db-cr format "Sim/NÆo" colon 20
     skip
     tt-param.c-origem     format "x(30)"   colon 20
     skip(2)
     c-sel                      no-label
     tt-param.cod-estabel-ini               colon 20 
     "|< >|"                                at 44 
     tt-param.cod-estabel-fim   no-label
     tt-param.cod-esp-ini                   colon 20 
     "|< >|"                                at 44 
     tt-param.cod-esp-fim       no-label
     tt-param.cod-emit-ini                  colon 20 
     "|< >|"                                at 44 
     tt-param.cod-emit-fim      no-label
     tt-param.dt-trans-ini                  colon 20 
     "|< >|"                                at 44 
     tt-param.dt-trans-fim      no-label
     tt-param.dt-venci-ini                  colon 20 
     "|< >|"                                at 44 
     tt-param.dt-venci-fim      no-label
     skip(2)
     tt-param.dt-maq-ini                  colon 20 
     "|< >|"                                at 44 
     tt-param.dt-maq-fim      no-label
     skip(2)

     c-cla                      no-label
     skip
     c-cla-2                    no-label 
     skip(2)
     c-imp                      no-label
     c-destino                              colon 20  
     tt-param.arquivo                       colon 20   
     tt-param.usuario                       colon 20                                            
        with stream-io no-box width 256 side-labels frame f-imp-par.
                
&if "{&mgadm_version}" < "2.06b" &then
  IF l-selec-ref THEN do:
    form    
         c-par                     no-label
         skip
         tt-param.l-tit-subs   format "Sim/NÆo" colon 20
         skip
         tt-param.l-nota-db-cr format "Sim/NÆo" colon 20
         skip
         tt-param.c-origem     format "x(30)"   colon 20
         skip(2)
         c-sel                      no-label
         tt-param.cod-estabel-ini               colon 20 
         "|< >|"                                at 44 
         tt-param.cod-estabel-fim   no-label
         tt-param.cod-esp-ini                   colon 20 
         "|< >|"                                at 44 
         tt-param.cod-esp-fim       no-label
         tt-param.cod-emit-ini                  colon 20 
         "|< >|"                                at 44 
         tt-param.cod-emit-fim      no-label
         tt-param.dt-trans-ini                  colon 20 
         "|< >|"                                at 44 
         tt-param.dt-trans-fim      no-label
         tt-param.dt-venci-ini                  colon 20 
         "|< >|"                                at 44 
         tt-param.dt-venci-fim      no-label
         tt-param.referencia-ini                colon 20
         "|< >|"                                at 44 
         tt-param.referencia-fim    no-label
         skip(2)
         tt-param.dt-maq-ini                  colon 20 
        "|< >|"                                at 44 
        tt-param.dt-maq-fim      no-label
         c-cla                      no-label
         skip
         c-cla-2                    no-label 
         skip(2)
         c-imp                      no-label
         c-destino                              colon 20  
         tt-param.arquivo                       colon 20   
         tt-param.usuario                       colon 20                                            
            with stream-io no-box width 256 side-labels frame f-imp-par-2.  
  end.
&else 
  form    
       c-par                     no-label
       skip
       tt-param.l-tit-subs   format "Sim/NÆo" colon 20
       skip
       tt-param.l-nota-db-cr format "Sim/NÆo" colon 20
       skip
       tt-param.c-origem     format "x(30)"   colon 20
       skip(2)
       c-sel                      no-label
       tt-param.cod-estabel-ini               colon 20 
       "|< >|"                                at 44 
       tt-param.cod-estabel-fim   no-label
       tt-param.cod-esp-ini                   colon 20 
       "|< >|"                                at 44 
       tt-param.cod-esp-fim       no-label
       tt-param.cod-emit-ini                  colon 20 
       "|< >|"                                at 44 
       tt-param.cod-emit-fim      no-label
       tt-param.dt-trans-ini                  colon 20 
       "|< >|"                                at 44 
       tt-param.dt-trans-fim      no-label
       tt-param.dt-venci-ini                  colon 20 
       "|< >|"                                at 44 
       tt-param.dt-venci-fim      no-label
       tt-param.referencia-ini                colon 20
       "|< >|"                                at 44 
       tt-param.referencia-fim    no-label
       skip(2)
       c-cla                      no-label
       skip
       tt-param.dt-maq-ini                  colon 20 
     "|< >|"                                at 44 
     tt-param.dt-maq-fim      no-label
       c-cla-2                    no-label 
       skip(2)
       c-imp                      no-label
       c-destino                              colon 20  
       tt-param.arquivo                       colon 20   
       tt-param.usuario                       colon 20                                            
          with stream-io no-box width 256 side-labels frame f-imp-par-2.  
&endif 
         
run utp/ut-trfrrp.p (input frame f-cabec-1:handle).
run utp/ut-trfrrp.p (input frame f-cabec-2:handle).
run utp/ut-trfrrp.p (input frame f-det-1:handle).
run utp/ut-trfrrp.p (input frame f-det-2:handle).
run utp/ut-trfrrp.p (input frame f-imp-par:handle).
                      

