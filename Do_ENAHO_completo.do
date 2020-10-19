clear 
global base19 cd "D:\Bases\ENAHO\2019"
global trab19 cd "D:\Bases\ENAHO\2019\Trabajadas"

************
* Bases individuales:
*Modulo 3:
global mod3 "p300a p301a p301d p302 p302x p303 p308b p308c p313 p3151- p3156 p316_1- p316c10 p314a p314b_1 p314b_2 p314b_3 p314b_4 p314b_7 p314b1_1 p314b1_2 p314b1_6 p314b1_7 p314b1_8 p314d factor07 factora07"
*Modulo 4:
global mod4 "p401 p401h1- p401h6 p4021-p40314 p407f1- p407h2 p4091-p40911 p414n_01- p414n_16 p414_01- p414_16 p4191- p4198 p420a"
*Modulo 5:
global mod5 "ocu500 p501-p504 p511a p5151- p51511 p517 p518 p519 p520 p530a p530b p535 p536 p558d2_1 p558d2_2 p506 p507 p510 p5111- p51112 p512a p512b p513t p513a1 p513a2 p514 p523- p524e2 p558a1- p558a5 p558c- p558d1 p558e1- p558f p558h1_1- p558h1_6 p559_01- p559_03 p560t_01- p560a1_10 ocupinf- fac500a"
* Modulo del negocio propio (enaho04-2019-1-preg-1-a-13)
global negocio "e2 e4a1- e5"

*********************

* Bases nivel de hogar:
* Modulo 1:
global mod1 "p25_1- p25_5 p101- p104b2 p110- p110c3 p111a- p112a p1141 - p1145 t111a- factor07"
* Modulo Sumaria:
global sumaria "percepho- totmieho estrsocial- pobreza gashog2d"
* Modulo programas sociales (700)
global progsoc "p700i p701_01- p701_09 p710_01- ticuest01"
* Modulo equipamiento del hogar (612) --> p612n==7 | p612n==12
global equipos "p612n p612 p612a"
* Modulo Consumo de implementos de limpieza (606d) --> keep if p606n==1
global limpieza "p606n p606d"

**************************
********************************************
* Trabajo bases de equipamiento y limpieza *
********************************************

$base19
use "enaho01-2019-612.dta",clear
keep if p612n==7 | p612n==12
gen computadora=(p612n==7 & p612==1)
gen refri=(p612n==12 & p612==1)
collapse (sum) computadora refri,by(conglome vivienda hogar)
$trab19
save "equi.dta",replace

$base19
use "enaho01-2019-606d.dta",clear
keep if p606n==1
gen jabon=(p606n==1 & p606d==1)
collapse (sum) jabon,by(conglome vivienda hogar)
$trab19
save "jabon.dta",replace

**************************
* Merge bases de hogar  *
**************************
$base19
use "enaho01-2019-100.dta",clear
keep conglome- tipenc $mod1
merge 1:1 conglome vivienda hogar using "sumaria-2019.dta",keepusing($sumaria) gen(merge1_sum)
keep if merge1_sum==3
merge 1:1 conglome vivienda hogar using "enaho01-2019-700.dta",keepusing($progsoc) gen(merge1_sum_ps)
$trab19
merge 1:1 conglome vivienda hogar using "equi.dta",gen(merge1_sum_ps_equi)
merge 1:1 conglome vivienda hogar using "jabon.dta",gen(merge1_sum_ps_equi_jab)
save "hogar2019.dta",replace

sum computadora refri jabon
**********

********************************
* Merge de bases individuales*
********************************
$base19
use "enaho04-2019-1-preg-1-a-13.dta",clear
keep if activida=="1"
$trab19
save "enaho04-2019-1-preg-1-a-13_v2.dta",replace

$base19
use "enaho01-2019-200.dta",clear
drop if p203==0
keep conglome - estrato p203 p203a p203b p207 p208a p208b p210 p211a facpob07
merge 1:1 conglome vivienda hogar codperso using "enaho01a-2019-300.dta",keepusing($mod3) gen(merge2_3)
merge 1:1 conglome vivienda hogar codperso using "enaho01a-2019-400.dta",keepusing($mod4) gen(merge2_3_4)
merge 1:1 conglome vivienda hogar codperso using "enaho01a-2019-500.dta",keepusing($mod5) gen(merge2_3_4_5)
$trab19
merge 1:1 conglome vivienda hogar codperso using "enaho04-2019-1-preg-1-a-13_v2.dta",keepusing($negocio) gen(merge2_3_4_5_neg)
save "indiv2019.dta",replace

****************************************
****************************************
****************************************

**********************************
* Trabajo con base de individuos *
**********************************

$trab19
use "indiv2019.dta",clear

* Conservar solo a la PEA:
*keep if ocu500>=1 & ocu500<=3
* grupos:
gen grupo=1 if ocu500==1 & ocupinf==2
replace grupo=2 if ocu500==1 & ocupinf==1
replace grupo=3 if ocu500==2 | ocu500==3
label define grupo 1 "Ocupado formal" 2 "Ocupado informal" 3 "Desempleado"
label values grupo grupo

*Grupo de edad:
gen grupo_edad=1 if p208a>=14 & p208a<=20
replace grupo_edad=2 if p208a>=21 & p208a<=30
replace grupo_edad=3 if p208a>=31 & p208a<=40
replace grupo_edad=4 if p208a>=41 & p208a<=50
replace grupo_edad=5 if p208a>=51 & p208a<=60
replace grupo_edad=6 if p208a>=61 & p208a<=70
replace grupo_edad=7 if p208a>=70 & p208a<=200
label define grupo_edad 1 "De 14 a 20" 2 "De 21 a 30" 3 "De 31 a 40" 4 "De 41 a 50" 5 "De 51 a 60" 6 "De 61 a 70" 7 "De 71 a más"
label values grupo_edad grupo_edad

* Sexo:
gen hombre=1 if p207==1
replace hombre=0 if p207==2
label define hombre 0 "Mujer" 1 "Hombre"
label values hombre hombre

* Nivel de estudios:
gen nivel_estudios=1 if p301a==1 | p301a==2
replace nivel_estudios=2 if p301a==3
replace nivel_estudios=3 if p301a==4
replace nivel_estudios=4 if p301a==5
replace nivel_estudios=5 if p301a==6
replace nivel_estudios=6 if p301a==7 | p301a==9
replace nivel_estudios=7 if p301a==8 | p301a==10 | p301a==11
label define nivel_estudios 1 "Sin nivel/inicial" 2 "Primaria incompleta" 3 "Primaria completa" 4 "Secundaria incompleta" 5 "Secundaria completa" /*
*/ 6 "Superior incompleta" 7 "Superior completa"
label values nivel_estudios nivel_estudios

* Sector economico:
gen sector_econ=1 if p506>=100 & p506<=1000
replace sector_econ=2 if p506>=1010 & p506<4100
replace sector_econ=3 if (p506>=4510 & p506<=4799) | (p506>=4911 & p506<=5320) | (p506>=5811 & p506<=6399) | (p506>=5510 & p506<=5630)
replace sector_econ=4 if p506!=. & sector_econ==.
label define sector_econ 1 "Agro y pesca" 2 "Manufactura" 3 "Comercio, restaurantes, transportes y comunicaciones" /*
*/ 4 "Otros sectores"
label values sector_econ sector_econ

* Actividad principal de negocio como ambulante o en la calle:
gen ambulante_improv=(e2==1 | e2==4 | e2==5 | e2==6) if e2!=.
gen transportista=(e2==3) if e2!=.

* Funcion en su trabajo:
gen funcion=1 if p507==1 
replace funcion=2 if p507==2
replace funcion=3 if p507==3
replace funcion=4 if p507==4
replace funcion=5 if p507>=5 & p507!=.
label define funcion 1 "Empleador/patrono" 2 "Trabajador independiente" 3 "Empleado" 4 "Obrero" 5 "Trabajador familiar no remunerado/otro"
label values funcion funcion

* Trabaja en otro distrito:
gen trabaja_otro_dist=(p558d2_1==2) if p558d2_1!=.

* Mayores de 18 anios con acceso al sistema financiero:
gen acceso_financiero=(p558e6==0) if p558e6!=.

* Alimentacion fuera de casa:
gen desayuno_fuera=(p559_01==1) if p559_01!=.
gen almuerza_fuera=(p559_02==1) if p559_02!=.
gen cena_fuera=(p559_03==1) if p559_03!=.

* Uso de transporte publico (solo pea)
gen usa_tpublico=(p560t_01==1 | p560t_02==1 | p560t_03==1 | p560t_04==1 | p560t_05==1 | p560t_06==1 | p560t_07==1) if p560t_01!=.

* Lengua materna:
gen lengua_materna=1 if p300==4
replace lengua_materna=2 if p300==1 | p300==2 | p300==3 
replace lengua_materna=3 if p300>=6 & p300!=.
label define lengua_materna 1 "Castellano" 2 "Quechua/Aymara/otra nativa" 3 "Otra lengua"
label values lengua_materna lengua_materna

* Uso de internet
gen uso_internet=p314a==1
label define uso_internet 0 "No uso" 1 "Uso Internet"
label values uso_internet uso_internet

/*
* Tiene hijos (solo es posible conocerlo para el jefe del hogar)
gen hij=p203==3
bys conglome vivienda hogar: egen ind_hijos=sum(hij)

gen es_jefe=1 if p203==1
gen jefe_hijos=1 if es_jefe==1 & ind_hijos==1
replace jefe_hijos=0 if es_jefe==1 & ind_hijos==0
label define jefe_hijos 0 "Jefe sin hijos" 1 "Jefe con hijos",modify
label values jefe_hijos jefe_hijos
drop hij ind_hijos
*/

* Acceso a servicios de salud:
gen enfermo=1 if p4025==1
gen acceso_servsalud=0 if enfermo==1
forvalues x = 1/9{
replace acceso_servsalud=1 if p403`x'==1 
}
label define acceso_servsalud 0 "Enfermo sin acceso" 1 "Enfermo con acceso"
label values acceso_servsalud acceso_servsalud

drop  p414n_01- p414n_16

* Seguro de salud:
gen seguro=1 if p4191==2 & p4192==2 & p4193==2 & p4194==2 & p4195==2 & p4196==2 & p4197==2 & p4198==2
replace seguro=2 if p4192==1 | p4193==1 | p4196==1 | p4197==1 | p4198==1 
replace seguro=3 if p4191==1 | p4194==1 | p4195==1
label define seguro 1 "No tiene" 2 "Seguro privado/otro" 3 "Seguro público"
label values seguro seguro

* Limitaciones
gen limitacion=0 if p401h1!=.
replace limitacion=1 if p401h1==1 | p401h2==1 | p401h3==1 | p401h4==1 | p401h5==1 | p401h6==1
label define limitacion 0 "Sin limitacion" 1 "Alguna limitacion"
label values limitacion

***** Guardamos la base
$trab19
save "indiv2019_v2.dta",replace

*************************

**************************
* Trabajo bases de hogar *
**************************

$trab19
use "hogar2019.dta",clear
order computadora refri jabon,after (merge1_sum_ps_equi_jab)

* Pobreza
sum pobreza

* Servicios basicos:
gen agua_pub=(p110==1 | p110==2 | p110==3)
gen saneamiento_pub=(p111==1 | p111==2 | p111==3 | p111==4)
gen electricidad=(p1121==1)
gen telefonia=(p1141==1 | p1142==1)
gen internet=(p1144==1)
label define agua_pub 0 "Sin acceso" 1 "Con Agua por red pública/pilón"
label define saneamiento_pub 0 "Sin acceso" 1 "Red pública/tratamiento"
label define electricidad 0 "Sin acceso" 1 "Con Electricidad"
label define telefonia 0 "Sin acceso" 1 "Con telefono fijo/movil"
label define internet 0 "Sin acceso" 1 "Con internet"
foreach x of varlist agua_pub-internet{
label values `x' `x'
}


* Gasto mensual percapita
gen gasto_mes_pc=gashog2d/(12*mieperho)
label var gasto_mes_pc "Gasto mensual percapita"

* Acceso a programas sociales:
** Alimentarios:
gen prog_socialesA=1 if p701_09!=.
replace prog_socialesA=0 if p701_09==1

** No alimentarios:
gen prog_socialesNA=1 if p710_14!=.
replace prog_socialesNA=0 if p710_14==1

* Alguno.
gen prog_sociales=0 if p701_09!=.
replace prog_sociales=1 if prog_socialesA==1 | prog_socialesNA==1

label define prog_socialesA 0 "No p.sociales alimentarios" 1 "Sí p.sociales alimentarios"
label define prog_socialesNA 0 "No p.sociales no alimentarios" 1 "Sí p.sociales no alimentarios"
label define prog_sociales 0 "No p.sociales" 1 "Sí p.sociales"
label values prog_socialesA  prog_socialesA 
label values prog_socialesNA  prog_socialesNA 
label values prog_sociales  prog_sociales

** Hacinamiento:
gen mie_habtot=mieperho/p104
gen mie_habdormir=mieperho/p104a

* Vulnerabilidad ante la pobreza:
gen dif_pe=(gasto_mes_pc-linpe)/linpe
gen dif_pne=(gasto_mes_pc-linea)/linea
foreach x in 05 10 20{
gen b`x'_pe=(dif_pe>0 & dif_pe<=0.`x')
gen b`x'_pne=(dif_pne>0 & dif_pne<=0.`x')
label var b`x'_pe "Gasto solo se encuentra entre 0% y `x'% sobre la línea de pobreza extrema"
label var b`x'_pne "Gasto solo se encuentra entre 0% y `x'% sobre la línea de pobreza"
}

br b05_pe b10_pe b20_pe b05_pne b10_pne b20_pne dif_pe dif_pne dif_pne gasto_mes_pc linpe linea

$trab19
save "hogar2019_v2.dta",replace

******************

****************************************
* Obtenemos las tablas de interés ******
****************************************

global result cd "D:\Material academico\Covid\Propuesta_libro\Desarrollo\Resultados"

** Individual:
$trab19
use "indiv2019_v2.dta",clear

* Situacion laboral
tab grupo,gen(grupo)
rename grupo1 ocu_formal
rename grupo2 ocu_informal
rename grupo3 desempleado

* Grupo de edad:
tab grupo_edad,gen(grupo_edad)
rename grupo_edad1 de14_20
rename grupo_edad2 de21_30
rename grupo_edad3 de31_40
rename grupo_edad4 de41_50
rename grupo_edad5 de51_60
rename grupo_edad6 de61_70
rename grupo_edad7 de71_mas

* informalidad en adultos mayores de 50 anios
gen am_informal=((de51_60==1 | de61_70==1 | de71_mas==1) & ocu_informal==1) if ocu_informal!=.

* Fragilidad del empleo (temporal o sin contrato)
gen contrato_fragil=(p511a==2 | p511a==5 | p511a==6 | p511a==7) if ocu500==1

* Sexo
tab hombre,gen(sexo)
rename sexo1 mujer
rename sexo2 varon

* Sector economico:
tab sector_econ,gen(sector)
rename sector1 agro_pesca
rename sector2 manufactura
rename sector3 comercio_transp_rest
rename sector4 otro_sector

* Trabajo independiente en la calle: ambulantes, transportistas.
gen indep_ambulante=0 if agro_pesca!=.
replace indep_ambulante=1 if ambulante_improv==1
gen indep_transp=0 if agro_pesca!=.
replace indep_transp=1 if transportista==1

* Trabaja en otro distrito:
order trabaja_otro_dist,after(indep_transp)

* Acceso al sistema financiero (mayores de 18)
order acceso_financiero,after(trabaja_otro_dist)

* Comidas fuera y uso de transporte público:
order  desayuno_fuera almuerza_fuera cena_fuera usa_tpublico,after(acceso_financiero)

* Acceso a servicios de salud si estuvo enfermo:
order acceso_servsalud,after(usa_tpublico)

* Tenencia de seguro:
tab seguro,gen(seguro)
rename seguro1 seguro_notiene
rename seguro2 seguro_privado
rename seguro3 seguro_publico

***********
***********

* Variables adicionales
gen urbano=(estrato>=1 & estrato<=5)
gen am_urbanos=((de51_60==1 | de61_70==1 | de71_mas==1) & urbano==1)

order urbano am_urbanos,before(ocu_formal)

* Sistematizacion:
gen dpto= real(substr(ubigeo,1,2))
gen prov= real(substr(ubigeo,3,2))
replace dpto=15 if (dpto==7)
replace dpto=dpto+1 if dpto>=16
replace dpto=16 if dpto==15 & (prov==1)
replace dpto=16 if dpto==7
label define dpto 1"Amazonas" 2"Ancash" 3"Apurimac" 4"Arequipa" 5"Ayacucho" 6"Cajamarca" 8"Cusco" 9"Huancavelica" 10"Huanuco" 11"Ica" /*
*/12"Junin" 13"La_Libertad" 14"Lambayeque" 15"Lima Regiones" 16 "Lima_Metropolitana y Callao" 17 "Loreto" 18 "Madre_de_Dios" 19 "Moquegua" 20"Pasco" 21"Piura" 22"Puno" 23"San_Martin" /*
*/24"Tacna" 25"Tumbes" 26"Ucayali" 
lab val dpto dpto 

foreach x of varlist urbano-seguro_publico{
gen d`x'=`x'
replace d`x'=d`x'*100
}

* Colapso con el peso respectivo:
global varpob "urbano am_urbanos de14_20- de71_mas mujer varon acceso_servsalud- seguro_publico"
global varocu " contrato_fragil ocu_formal- desempleado am_informal agro_pesca- usa_tpublico"
global dvarpob "durbano dam_urbanos dde14_20- dde71_mas dmujer dvaron dacceso_servsalud- dseguro_publico"
global dvarocu "dcontrato_fragil docu_formal- ddesempleado dam_informal dagro_pesca- dusa_tpublico"

preserve
collapse (sum) $varocu (mean) $dvarocu [iw=fac500],by(dpto)
$result
save "varocu19.dta",replace
restore

collapse (sum) $varpob (mean) $dvarpob [iw=fac500],by(dpto)
merge 1:1 dpto using "varocu19.dta"
drop _merge
$result
save "sist_indiv2019.dta",replace

**********************************

** Hogar:
$trab19
use "hogar2019_v2.dta",clear
drop dif_pe dif_pne

* Nuevas variables de interes:

* piso de tierra:
gen piso_tierra=p103==6

* Acceso a agua todos los dias:
gen agua_siempre=p110c==1

* necesidades basicas insatisfechas
order nbi1- nbi5,after(agua_siempre)

* ratio de dependencia:
gen ratio_dep=percepho/mieperho

* estrato social:
tab estrsocial,gen(social)
rename social1 social_a
rename social2 social_b
rename social3 social_c
rename social4 social_d
rename social5 social_e
rename social6 social_rural

* pobreza
tab pobreza,gen(pobre)
rename pobre1 pobre_extremo
rename pobre2 pobre_noext
rename pobre3 no_pobre

********
order agua_siempre piso_tierra,after(agua_pub)
order ratio_dep mie_habtot mie_habdormir gasto_mes_pc,after(b20_pne)
order ratio_dep gasto_mes_pc mie_habtot mie_habdormir,after(no_pobre)

* Variables adicionales
gen urbano=(estrato>=1 & estrato<=5)
order urbano,before(computadora)

foreach x of varlist urbano- mie_habdormir{
gen d`x'=`x'
replace d`x'=d`x'*100
}

* Sistematizacion:
gen dpto= real(substr(ubigeo,1,2))
gen prov= real(substr(ubigeo,3,2))
replace dpto=15 if (dpto==7)
replace dpto=dpto+1 if dpto>=16
replace dpto=16 if dpto==15 & (prov==1)
replace dpto=16 if dpto==7
label define dpto 1"Amazonas" 2"Ancash" 3"Apurimac" 4"Arequipa" 5"Ayacucho" 6"Cajamarca" 8"Cusco" 9"Huancavelica" 10"Huanuco" 11"Ica" /*
*/12"Junin" 13"La_Libertad" 14"Lambayeque" 15"Lima Regiones" 16 "Lima_Metropolitana y Callao" 17 "Loreto" 18 "Madre_de_Dios" 19 "Moquegua" 20"Pasco" 21"Piura" 22"Puno" 23"San_Martin" /*
*/24"Tacna" 25"Tumbes" 26"Ucayali" 
lab val dpto dpto 

collapse (sum) urbano- no_pobre (mean) ratio_dep- dno_pobre [iw=factor07],by(dpto)
$result
save "sist_hogar2019.dta",replace
