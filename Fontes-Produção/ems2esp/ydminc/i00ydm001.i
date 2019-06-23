/**************************************************************************
** i00ydm001.i  - campo: idi-status-ord  ( tabela: mmv-ord-status )
**************************************************************************/

{include/i-lgcode.i}

&IF "{&LANGUAGE-CODE}" = "POR" &THEN
&glob val1 Aguardando Planejamento
&glob val2 Aguardando Material
&glob val3 Aguardando MÆo-de-Obra
&glob val4 Aguardando Serv. Ext.
&glob val5 Aguardando DF Equipamento
&glob val6 Aguardando Programa‡Æo
&glob val7 Aguardando Ferramenta
&glob val8 Aguardando Especifica‡Æo Solicitante
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ESP" &THEN
&glob val1 Aguardando Planificaci¢n
&glob val2 Aguardando Material
&glob val3 Aguardando Mano de Obra
&glob val4 Aguardando Servicio Externo
&glob val5 Aguardando Equipo
&glob val6 Aguardando Programacion
&glob val7 Aguardando Hierramenta
&glob val8 Aguardando Especificaci¢n Solicitante
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ING" &THEN

&ENDIF

{include/ind01-10.i {1} {2} {3}}
/* Fim */
