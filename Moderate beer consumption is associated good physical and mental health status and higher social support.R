

library("foreign")
library("plyr")
library("dplyr")
library("car")
library("forcats")
library("reshape2")


#### Data Handling ####
#### ENSE 2017 ####
ense2017 <- read.spss("C:/Users/anton/Desktop/Estudios Cerveza-Epidem Manuel de Oya/MMR/Encuesta Salud España/2017 ENSE/ORIGINAL_ENSE17_Adulto.sav", to.data.frame = T, use.missings = F)
ense2017 <- ense2017[c(1,2,5,6,7,15,16,17,18,19,22,23,42:149,153:187,273:320,345,346,347:359,360:388,396,404,412,420,428,
                       436:447,451:456,465, 389:395, 397:403, 405:411, 413:419, 421:427, 429:435)]

names(ense2017)[1:2] <- c("Año", "CCAA")
names(ense2017)[3:4] <- c("Genero", "Edad")
names(ense2017)[267] <- c("IMC_grupo")
ense2017$IMC_grupo <- fct_expand(ense2017$IMC_grupo, c("NS/NC")) 
ense2017$IMC_grupo[ense2017$IMC_grupo == "9"] <- "NS/NC"
ense2017$IMC_grupo<- fct_drop(ense2017$IMC_grupo, only = c("9"))

names(ense2017)[13:16] <- c("Salud_percibida", "Enfermedad_cronica", "Grado_limitacion", "Tipo_problema_enf")
ense2017$Tipo_problema_enf <- fct_expand(ense2017$Tipo_problema_enf, c("ninguno")) 
ense2017$Tipo_problema_enf[ense2017$Grado_limitacion == "Nada limitado"] <- "ninguno"

names(ense2017)[271] <- c("Lugar_residencia")

names(ense2017)[156:157] <- c("Medicamentos_receta", "Medicamentos_no_receta")

names(ense2017)[207:218] <- c("Frec_AF_libre", "Frec_AF_vig", "Horas_vig",
                              "Minutos_vig", "Frec_AF_mod", "Horas_mod",
                              "Minutos_mod","Frec_AF_caminar", "Horas_caminar",
                              "Minutos_caminar","Horas_sed", "Minutos_sed")

names(ense2017)[219:220] <- c("Frec_fruta", "Cantidad_fruta")
names(ense2017)[226:227] <- c("Frec_verdura", "Cantidad_verdura")
names(ense2017)[231] <- c("Frec_dulces")
names(ense2017)[232] <- c("Frec_refrescos")
names(ense2017)[233] <- c("Frec_comida_rapida")
names(ense2017)[234] <- c("Frec_aperitivos")

names(ense2017)[240] <- c("Tabaco")

names(ense2017)[266] <- c("Clase_social_ocup")
names(ense2017)[12] <- c("Educacion")


names(ense2017)[246] <- c("Frec_alcohol")
ense2017$Frec_alcohol <- fct_expand(ense2017$Frec_alcohol, c("Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida")) 
ense2017$Frec_alcohol[ense2017$Frec_alcohol == "9"] <- "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"
ense2017$Frec_alcohol<- fct_drop(ense2017$Frec_alcohol, only = c("9"))

names(ense2017)[247] <- c("Consumo_cerveza")
ense2017$Consumo_cerveza[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2017$Frec_alcohol ==  "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2017$Frec_alcohol == "Menos de una vez al mes" | 
                             ense2017$Frec_alcohol == "Una vez al mes" | 
                             ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[272:278] <- c("Consumo_cerveza_lunes", "Consumo_cerveza_martes",
                              "Consumo_cerveza_mier", "Consumo_cerveza_jueves", "Consumo_cerveza_vier",
                              "Consumo_cerveza_sab","Consumo_cerveza_dom")
names(ense2017)[248] <- c("Consumo_vino")
ense2017$Consumo_vino[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                          ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                          ense2017$Frec_alcohol == "Menos de una vez al mes" | 
                          ense2017$Frec_alcohol == "Una vez al mes" | 
                          ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[279:285] <- c("Consumo_vino_lunes", "Consumo_vino_martes",
                              "Consumo_vino_mier", "Consumo_vino_jueves", "Consumo_vino_vier",
                              "Consumo_vino_sab","Consumo_vino_dom")
names(ense2017)[249] <- c("Consumo_vermut") 
ense2017$Consumo_vermut[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                            ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                            ense2017$Frec_alcohol == "Menos de una vez al mes" | 
                            ense2017$Frec_alcohol == "Una vez al mes" | 
                            ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[286:292] <- c("Consumo_vermut_lunes", "Consumo_vermut_martes",
                              "Consumo_vermut_mier", "Consumo_vermut_jueves", "Consumo_vermut_vier",
                              "Consumo_vermut_sab","Consumo_vermut_dom")
names(ense2017)[250] <- c("Consumo_licores")
ense2017$Consumo_licores[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2017$Frec_alcohol == "Menos de una vez al mes" | 
                             ense2017$Frec_alcohol == "Una vez al mes" | 
                             ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[293:299] <- c("Consumo_licores_lunes", "Consumo_licores_martes",
                              "Consumo_licores_mier", "Consumo_licores_jueves", "Consumo_licores_vier",
                              "Consumo_licores_sab","Consumo_licores_dom")
names(ense2017)[251] <- c("Consumo_destilada") 
ense2017$Consumo_destilada[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                               ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                               ense2017$Frec_alcohol == "Menos de una vez al mes" | 
                               ense2017$Frec_alcohol == "Una vez al mes" | 
                               ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[300:306] <- c("Consumo_destilada_lunes", "Consumo_destilada_martes",
                              "Consumo_destilada_mier", "Consumo_destilada_jueves", "Consumo_destilada_vier",
                              "Consumo_destilada_sab","Consumo_destilada_dom")
names(ense2017)[252] <- c("Consumo_locales")
ense2017$Consumo_locales[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2017$Frec_alcohol == "Menos de una vez al mes" |
                             ense2017$Frec_alcohol == "Una vez al mes" | 
                             ense2017$Frec_alcohol == "2-3 días en un mes"] <- "no"
names(ense2017)[307:313] <- c("Consumo_locales_lunes", "Consumo_locales_martes",
                              "Consumo_locales_mier", "Consumo_locales_jueves", "Consumo_locales_vier",
                              "Consumo_locales_sab","Consumo_locales_dom")
names(ense2017)[253] <- c("Consumo_alcohol_abusivo_dia")
ense2017$Consumo_alcohol_abusivo_dia[ense2017$Consumo_alcohol_abusivo_dia == "Menos de una vez al mes_duplicated_8"] <- "Menos de una vez al mes"
ense2017$Consumo_alcohol_abusivo_dia <- fct_drop(ense2017$Consumo_alcohol_abusivo_dia, only = c("Menos de una vez al mes_duplicated_8"))
ense2017$Consumo_alcohol_abusivo_dia <- fct_expand(ense2017$Consumo_alcohol_abusivo_dia, c("Ex_bebedor")) 
ense2017$Consumo_alcohol_abusivo_dia[ense2017$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"] <- "Nunca en toda mi vida"
ense2017$Consumo_alcohol_abusivo_dia[ense2017$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol"] <- "Ex_bebedor"

names(ense2017)[268] <- c("Consumo_alcohol_medio_semanal")

names(ense2017)[144:155] <- c("Mental_concentrarse", "Mental_preocupaciones", "Mental_util", 
                              "Mental_decisiones", "Mental_agobiado", "Mental_no_superacion", 
                              "Mental_disfrutar", "Mental_frente_problemas", "Mental_deprimido", 
                              "Mental_no_confianza", "Mental_no_valor", "Mental_feliz")

names(ense2017)[254:264] <- c("Social_visitas", "Social_ayuda_hogar", "Social_trabajo", 
                              "Social_personas", "Social_afecto", "Social_hablar_trabajo", 
                              "Social_hablar_personal", "Social_hablar_economico", "Social_salir_fuera", 
                              "Social_consejos", "Social_ayuda_enferm")


ense2017 <- ense2017[-c(which(is.na(ense2017$Año))), ]
ense2017$Año[ense2017$Año == 2017] <- "2017"
ense2017$ID <- seq(from=1,to=23089) 

ense2017 <- ense2017[c(1,2,3,4,267,13:16,271,156,157,207:218,219,226,231:234,240,266,12,246,247,
                       272:278,248:253,279:285,268,286:313,144:155,254:264,314)]


ense2017$Consumo_cerveza_lunes_cont <- ense2017$Consumo_cerveza_lunes
ense2017$Consumo_cerveza_martes_cont <- ense2017$Consumo_cerveza_martes
ense2017$Consumo_cerveza_mier_cont <- ense2017$Consumo_cerveza_mier
ense2017$Consumo_cerveza_jueves_cont <- ense2017$Consumo_cerveza_jueves
ense2017$Consumo_cerveza_vier_cont <- ense2017$Consumo_cerveza_vier
ense2017$Consumo_cerveza_sab_cont <- ense2017$Consumo_cerveza_sab
ense2017$Consumo_cerveza_dom_cont <- ense2017$Consumo_cerveza_dom

ense2017$Consumo_cerveza_lunes_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_martes_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_mier_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_jueves_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_vier_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_sab_cont[ense2017$Consumo_cerveza == "no"] <- 0
ense2017$Consumo_cerveza_dom_cont[ense2017$Consumo_cerveza == "no"] <- 0


ense2017$Consumo_vino_lunes_cont <- ense2017$Consumo_vino_lunes 
ense2017$Consumo_vino_martes_cont <- ense2017$Consumo_vino_martes
ense2017$Consumo_vino_mier_cont <- ense2017$Consumo_vino_mier
ense2017$Consumo_vino_jueves_cont <- ense2017$Consumo_vino_jueves 
ense2017$Consumo_vino_vier_cont <- ense2017$Consumo_vino_vier
ense2017$Consumo_vino_sab_cont <- ense2017$Consumo_vino_sab
ense2017$Consumo_vino_dom_cont <- ense2017$Consumo_vino_dom

ense2017$Consumo_vino_lunes_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_martes_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_mier_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_jueves_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_vier_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_sab_cont[ense2017$Consumo_vino == "no"] <- 0
ense2017$Consumo_vino_dom_cont[ense2017$Consumo_vino == "no"] <- 0


ense2017$Consumo_vermut_lunes_cont <- ense2017$Consumo_vermut_lunes
ense2017$Consumo_vermut_martes_cont <- ense2017$Consumo_vermut_martes
ense2017$Consumo_vermut_mier_cont <- ense2017$Consumo_vermut_mier
ense2017$Consumo_vermut_jueves_cont <- ense2017$Consumo_vermut_jueves
ense2017$Consumo_vermut_vier_cont <- ense2017$Consumo_vermut_vier
ense2017$Consumo_vermut_sab_cont <- ense2017$Consumo_vermut_sab
ense2017$Consumo_vermut_dom_cont <- ense2017$Consumo_vermut_dom

ense2017$Consumo_vermut_lunes_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_martes_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_mier_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_jueves_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_vier_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_sab_cont[ense2017$Consumo_vermut == "no"] <- 0
ense2017$Consumo_vermut_dom_cont[ense2017$Consumo_vermut == "no"] <- 0


ense2017$Consumo_licores_lunes_cont <- ense2017$Consumo_licores_lunes
ense2017$Consumo_licores_martes_cont <- ense2017$Consumo_licores_martes
ense2017$Consumo_licores_mier_cont <- ense2017$Consumo_licores_mier
ense2017$Consumo_licores_jueves_cont <- ense2017$Consumo_licores_jueves
ense2017$Consumo_licores_vier_cont <- ense2017$Consumo_licores_vier
ense2017$Consumo_licores_sab_cont <- ense2017$Consumo_licores_sab
ense2017$Consumo_licores_dom_cont <- ense2017$Consumo_licores_dom

ense2017$Consumo_licores_lunes_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_martes_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_mier_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_jueves_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_vier_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_sab_cont[ense2017$Consumo_licores == "no"] <- 0
ense2017$Consumo_licores_dom_cont[ense2017$Consumo_licores == "no"] <- 0


ense2017$Consumo_destilada_lunes_cont <- ense2017$Consumo_destilada_lunes
ense2017$Consumo_destilada_martes_cont <- ense2017$Consumo_destilada_martes
ense2017$Consumo_destilada_mier_cont <- ense2017$Consumo_destilada_mier
ense2017$Consumo_destilada_jueves_cont <- ense2017$Consumo_destilada_jueves
ense2017$Consumo_destilada_vier_cont <- ense2017$Consumo_destilada_vier
ense2017$Consumo_destilada_sab_cont <- ense2017$Consumo_destilada_sab
ense2017$Consumo_destilada_dom_cont <- ense2017$Consumo_destilada_dom

ense2017$Consumo_destilada_lunes_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_martes_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_mier_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_jueves_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_vier_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_sab_cont[ense2017$Consumo_destilada == "no"] <- 0
ense2017$Consumo_destilada_dom_cont[ense2017$Consumo_destilada == "no"] <- 0


ense2017$Consumo_locales_lunes_cont <- ense2017$Consumo_locales_lunes
ense2017$Consumo_locales_martes_cont <- ense2017$Consumo_locales_martes
ense2017$Consumo_locales_mier_cont <- ense2017$Consumo_locales_mier
ense2017$Consumo_locales_jueves_cont <- ense2017$Consumo_locales_jueves
ense2017$Consumo_locales_vier_cont <- ense2017$Consumo_locales_vier
ense2017$Consumo_locales_sab_cont <- ense2017$Consumo_locales_sab
ense2017$Consumo_locales_dom_cont <- ense2017$Consumo_locales_dom

ense2017$Consumo_locales_lunes_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_martes_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_mier_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_jueves_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_vier_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_sab_cont[ense2017$Consumo_locales == "no"] <- 0
ense2017$Consumo_locales_dom_cont[ense2017$Consumo_locales == "no"] <- 0

ense2017$Consumo_alcohol_medio_semanal_cont <- ense2017$Consumo_alcohol_medio_semanal


levels(ense2017$Mental_concentrarse)
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "mejor que lo habitual"] <- 0
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "igual que lo habitual"] <- 0
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "menos que lo habitual"] <- 1
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "mucho menos que lo habitual"] <- 1
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "ns"] <- NA
ense2017$Mental_concentrarse_cont[ense2017$Mental_concentrarse == "nc"] <- NA

levels(ense2017$Mental_preocupaciones)
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "No, en absoluto"] <- 0
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "no mas que lo habitual"] <- 0
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "Algo mas que lo habitual"] <- 1
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "mucho mas que lo habitual"] <- 1
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "ns"] <- NA
ense2017$Mental_preocupaciones_cont[ense2017$Mental_preocupaciones == "nc"] <- NA

levels(ense2017$Mental_util)
ense2017$Mental_util_cont[ense2017$Mental_util == "mas que lo habitual"] <- 0
ense2017$Mental_util_cont[ense2017$Mental_util == "igual que lo habitual"] <- 0
ense2017$Mental_util_cont[ense2017$Mental_util == "menos que lo habitual"] <- 1
ense2017$Mental_util_cont[ense2017$Mental_util == "mucho menos que lo habitual"] <- 1
ense2017$Mental_util_cont[ense2017$Mental_util == "ns"] <- NA
ense2017$Mental_util_cont[ense2017$Mental_util == "nc"] <- NA

levels(ense2017$Mental_decisiones)
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "mas que lo habitual"] <- 0
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "igual que lo habitual"] <- 0
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "menos que lo habitual"] <- 1
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "mucho menos que lo habitual"] <- 1
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "ns"] <- NA
ense2017$Mental_decisiones_cont[ense2017$Mental_decisiones == "nc"] <- NA

levels(ense2017$Mental_agobiado)
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "no, en absoluto"] <- 0
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "no mas que lo habitual"] <- 0
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "algo mas que lo habitual"] <- 1
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "mucho más que lo habitual"] <- 1
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "ns"] <- NA
ense2017$Mental_agobiado_cont[ense2017$Mental_agobiado == "nc"] <- NA

levels(ense2017$Mental_no_superacion)
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "no, en absoluto"] <- 0
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "no mas que lo habitual"] <- 0
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "algo mas que lo habitual"] <- 1
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "mucho mas que lo habitual"] <- 1
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "ns"] <- NA
ense2017$Mental_no_superacion_cont[ense2017$Mental_no_superacion == "nc"] <- NA

levels(ense2017$Mental_disfrutar)
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "mas que lo habitual"] <- 0
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "igual que lo habitual"] <- 0
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "menos que lo habitual"] <- 1
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "mucho menos que lo habitual"] <- 1
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "ns"] <- NA
ense2017$Mental_disfrutar_cont[ense2017$Mental_disfrutar == "nc"] <- NA

levels(ense2017$Mental_frente_problemas)
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "mas que lo habitual"] <- 0
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "igual que lo habitual"] <- 0
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "menos que lo habitual"] <- 1
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "mucho menos que lo habitual"] <- 1
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "ns"] <- NA
ense2017$Mental_frente_problemas_cont[ense2017$Mental_frente_problemas == "nc"] <- NA

levels(ense2017$Mental_deprimido)
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "no, en absoluto"] <- 0
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "no mas que lo habitual"] <- 0
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "algo mas que lo habitual"] <- 1
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "mucho mas que lo habitual"] <- 1
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "ns"] <- NA
ense2017$Mental_deprimido_cont[ense2017$Mental_deprimido == "nc"] <- NA

levels(ense2017$Mental_no_confianza)
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "no, en absoluto"] <- 0
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "no mas que lo habitual"] <- 0
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "algo mas que lo habitual"] <- 1
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "mucho mas que lo habitual"] <- 1
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "ns"] <- NA
ense2017$Mental_no_confianza_cont[ense2017$Mental_no_confianza == "nc"] <- NA

levels(ense2017$Mental_no_valor)
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "no, en absoluto"] <- 0
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "no mas que lo habitual"] <- 0
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "algo mas que lo habitual"] <- 1
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "mucho mas que lo habitual"] <- 1
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "ns"] <- NA
ense2017$Mental_no_valor_cont[ense2017$Mental_no_valor == "nc"] <- NA

levels(ense2017$Mental_feliz)
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "mas que lo habitual"] <- 0
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "igual que lo habitual"] <- 0
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "menos que lo habitual"] <- 1
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "mucho menos que lo habitual"] <- 1
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "ns"] <- NA
ense2017$Mental_feliz_cont[ense2017$Mental_feliz == "nc"] <- NA


ense2017$Salud_mental_general_cont <- ense2017$Mental_concentrarse_cont + ense2017$Mental_preocupaciones_cont + ense2017$Mental_util_cont +
    ense2017$Mental_decisiones_cont + ense2017$Mental_agobiado_cont + ense2017$Mental_no_superacion_cont +
    ense2017$Mental_disfrutar_cont + ense2017$Mental_no_confianza_cont + ense2017$Mental_no_valor_cont +
    ense2017$Mental_feliz_cont + ense2017$Mental_deprimido_cont + ense2017$Mental_frente_problemas_cont


levels(ense2017$Social_ayuda_hogar)
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "Menos de lo que deseo"] <- 2
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "Ni mucho ni poco"] <- 3
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "Casi como deseo"] <- 4
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "Tanto como deseo"] <- 5
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "ns"] <- NA
ense2017$Social_ayuda_hogar_cont[ense2017$Social_ayuda_hogar == "nc"] <- NA

levels(ense2017$Social_trabajo)
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "Menos de lo que deseo"] <- 2
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "Ni mucho ni poco"] <- 3
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "Casi como deseo"] <- 4
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "Tanto como deseo"] <- 5
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "ns"] <- NA
ense2017$Social_trabajo_cont[ense2017$Social_trabajo == "nc"] <- NA

levels(ense2017$Social_afecto)
ense2017$Social_afecto_cont[ense2017$Social_afecto == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_afecto_cont[ense2017$Social_afecto == "Menos de lo que deseo"] <- 2
ense2017$Social_afecto_cont[ense2017$Social_afecto == "Ni mucho ni poco"] <- 3
ense2017$Social_afecto_cont[ense2017$Social_afecto == "Casi como deseo"] <- 4
ense2017$Social_afecto_cont[ense2017$Social_afecto == "Tanto como deseo"] <- 5
ense2017$Social_afecto_cont[ense2017$Social_afecto == "ns"] <- NA
ense2017$Social_afecto_cont[ense2017$Social_afecto == "nc"] <- NA

levels(ense2017$Social_hablar_trabajo)
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "Menos de lo que deseo"] <- 2
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "Ni mucho ni poco"] <- 3
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "Casi como deseo"] <- 4
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "Tanto como deseo"] <- 5
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "ns"] <- NA
ense2017$Social_hablar_trabajo_cont[ense2017$Social_hablar_trabajo == "nc"] <- NA

levels(ense2017$Social_hablar_economico)
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "Menos de lo que deseo"] <- 2
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "Ni mucho ni poco"] <- 3
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "Casi como deseo"] <- 4
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "Tanto como deseo"] <- 5
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "ns"] <- NA
ense2017$Social_hablar_economico_cont[ense2017$Social_hablar_economico == "nc"] <- NA

levels(ense2017$Social_ayuda_enferm)
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "Menos de lo que deseo"] <- 2
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "Ni mucho ni poco"] <- 3
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "Casi como deseo"] <- 4
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "Tanto como deseo"] <- 5
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "ns"] <- NA
ense2017$Social_ayuda_enferm_cont[ense2017$Social_ayuda_enferm == "nc"] <- NA

levels(ense2017$Social_consejos)
ense2017$Social_consejos_cont[ense2017$Social_consejos == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_consejos_cont[ense2017$Social_consejos == "Menos de lo que deseo"] <- 2
ense2017$Social_consejos_cont[ense2017$Social_consejos == "Ni mucho ni poco"] <- 3
ense2017$Social_consejos_cont[ense2017$Social_consejos == "Casi como deseo"] <- 4
ense2017$Social_consejos_cont[ense2017$Social_consejos == "Tanto como deseo"] <- 5
ense2017$Social_consejos_cont[ense2017$Social_consejos == "ns"] <- NA
ense2017$Social_consejos_cont[ense2017$Social_consejos == "nc"] <- NA

levels(ense2017$Social_salir_fuera)
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "Menos de lo que deseo"] <- 2
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "Ni mucho ni poco"] <- 3
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "Casi como deseo"] <- 4
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "Tanto como deseo"] <- 5
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "ns"] <- NA
ense2017$Social_salir_fuera_cont[ense2017$Social_salir_fuera == "nc"] <- NA

levels(ense2017$Social_visitas)
ense2017$Social_visitas_cont[ense2017$Social_visitas == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_visitas_cont[ense2017$Social_visitas == "Menos de lo que deseo"] <- 2
ense2017$Social_visitas_cont[ense2017$Social_visitas == "Ni mucho ni poco"] <- 3
ense2017$Social_visitas_cont[ense2017$Social_visitas == "Casi como deseo"] <- 4
ense2017$Social_visitas_cont[ense2017$Social_visitas == "Tanto como deseo"] <- 5
ense2017$Social_visitas_cont[ense2017$Social_visitas == "ns"] <- NA
ense2017$Social_visitas_cont[ense2017$Social_visitas == "nc"] <- NA

levels(ense2017$Social_personas)
ense2017$Social_personas_cont[ense2017$Social_personas == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_personas_cont[ense2017$Social_personas == "Menos de lo que deseo"] <- 2
ense2017$Social_personas_cont[ense2017$Social_personas == "Ni mucho ni poco"] <- 3
ense2017$Social_personas_cont[ense2017$Social_personas == "Casi como deseo"] <- 4
ense2017$Social_personas_cont[ense2017$Social_personas == "Tanto como deseo"] <- 5
ense2017$Social_personas_cont[ense2017$Social_personas == "ns"] <- NA
ense2017$Social_personas_cont[ense2017$Social_personas == "nc"] <- NA

levels(ense2017$Social_hablar_personal)
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "Mucho menos de lo que deseo"] <- 1
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "Menos de lo que deseo"] <- 2
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "Ni mucho ni poco"] <- 3
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "Casi como deseo"] <- 4
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "Tanto como deseo"] <- 5
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "ns"] <- NA
ense2017$Social_hablar_personal_cont[ense2017$Social_hablar_personal == "nc"] <- NA


ense2017$Apoyo_social_general_cont <- ense2017$Social_ayuda_hogar_cont + ense2017$Social_trabajo_cont + ense2017$Social_afecto_cont +
    ense2017$Social_hablar_trabajo_cont + ense2017$Social_hablar_economico_cont + ense2017$Social_ayuda_enferm_cont +
    ense2017$Social_salir_fuera_cont + ense2017$Social_visitas_cont + ense2017$Social_personas_cont +
    ense2017$Social_consejos_cont + ense2017$Social_hablar_personal_cont


ense2017 <- ense2017[c(1:13,25:35,43,44,45,46,47,48,85:151,164,176)]



#### ENSE 2012 ####
ense2012 <- read.spss("C:/Users/anton/Desktop/Estudios Cerveza-Epidem Manuel de Oya/MMR/Encuesta Salud España/2012 ENSE/ORIGINAL_ENSE 2011_2.sav", to.data.frame = T, use.missings = T)
ense2012 <- ense2012[c(1,2,4,6,7,15,16,17,18,19,20,22,23,41:139,151:189, 198:209, 302:347,366:506,513:523,568:578, 663)]

names(ense2012)[1:2] <- c("CCAA", "Año")
names(ense2012)[4:5] <- c("Genero", "Edad")
names(ense2012)[364] <- c("IMC_grupo")

names(ense2012)[14:15] <- c("Salud_percibida", "Enfermedad_cronica")
names(ense2012)[123:124] <- c("Grado_limitacion", "Tipo_problema_enf")
ense2012$Tipo_problema_enf <- fct_expand(ense2012$Tipo_problema_enf, c("ninguno")) 
ense2012$Tipo_problema_enf[ense2012$Grado_limitacion == "Nada limitado"] <- "ninguno"

names(ense2012)[3] <- c("Lugar_residencia")
ense2012$Lugar_residencia <- as.factor(ense2012$Lugar_residencia)
ense2012$Lugar_residencia <- fct_expand(ense2012$Lugar_residencia, c("Municipios de más de 500.000 habitantes", "Municipio capital de provincia (excepto los anteriores)", 
                                                         "Municipios con más de 100.000 habitantes (excepto los anteriores)", "Municipios de 50.000 a 100.000 habitantes (excepto los anteriores)",
                                                         "Municipios de 20.000 a 50.000 habitantes (excepto los anteriores)", "Municipios de 10.000 a 20.000 habitantes",
                                                         "Municipios con menos de 10.000 habitantes")) 
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "0"] <- "Municipios de más de 500.000 habitantes"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "1"] <- "Municipio capital de provincia (excepto los anteriores)"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "2"] <- "Municipios con más de 100.000 habitantes (excepto los anteriores)"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "3"] <- "Municipios de 50.000 a 100.000 habitantes (excepto los anteriores)"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "4"] <- "Municipios de 20.000 a 50.000 habitantes (excepto los anteriores)"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "5"] <- "Municipios de 10.000 a 20.000 habitantes"
ense2012$Lugar_residencia[ense2012$Lugar_residencia == "6"] <- "Municipios con menos de 10.000 habitantes"
ense2012$Lugar_residencia<- fct_drop(ense2012$Lugar_residencia, only = c("0","1","2","3","4","5","6"))

names(ense2012)[164:165] <- c("Medicamentos_receta_hombre", "Medicamentos_receta_mujer")

names(ense2012)[310:321] <- c("Frec_AF_libre", "Frec_AF_vig", "Horas_vig",
                              "Minutos_vig", "Frec_AF_mod", "Horas_mod",
                              "Minutos_mod","Frec_AF_caminar", "Horas_caminar",
                              "Minutos_caminar","Horas_sed", "Minutos_sed")

names(ense2012)[330:331] <- c("Frec_fruta", "Cantidad_fruta")
ense2012$Frec_fruta <- fct_expand(ense2012$Frec_fruta, c("No sabe", "No contesta")) 
ense2012$Frec_fruta[ense2012$Frec_fruta == "8"] <- "No sabe"
ense2012$Frec_fruta[ense2012$Frec_fruta == "9"] <- "No contesta"
ense2012$Frec_fruta<- fct_drop(ense2012$Frec_fruta, only = c("8","9"))
names(ense2012)[337:338] <- c("Frec_verdura", "Cantidad_verdura")
ense2012$Frec_verdura <- fct_expand(ense2012$Frec_verdura, c("No sabe", "No contesta")) 
ense2012$Frec_verdura[ense2012$Frec_verdura == "8"] <- "No sabe"
ense2012$Frec_verdura[ense2012$Frec_verdura == "9"] <- "No contesta"
ense2012$Frec_verdura<- fct_drop(ense2012$Frec_verdura, only = c("8","9"))
names(ense2012)[342] <- c("Frec_dulces")
ense2012$Frec_dulces <- fct_expand(ense2012$Frec_dulces, c("No sabe", "No contesta")) 
ense2012$Frec_dulces[ense2012$Frec_dulces == "8"] <- "No sabe"
ense2012$Frec_dulces[ense2012$Frec_dulces == "9"] <- "No contesta"
ense2012$Frec_dulces<- fct_drop(ense2012$Frec_dulces, only = c("8","9"))
names(ense2012)[343] <- c("Frec_refrescos")
ense2012$Frec_refrescos <- fct_expand(ense2012$Frec_refrescos, c("No sabe", "No contesta")) 
ense2012$Frec_refrescos[ense2012$Frec_refrescos == "8"] <- "No sabe"
ense2012$Frec_refrescos[ense2012$Frec_refrescos == "9"] <- "No contesta"
ense2012$Frec_refrescos<- fct_drop(ense2012$Frec_refrescos, only = c("8","9"))
names(ense2012)[344] <- c("Frec_comida_rapida")
ense2012$Frec_comida_rapida <- fct_expand(ense2012$Frec_comida_rapida, c("No sabe", "No contesta")) 
ense2012$Frec_comida_rapida[ense2012$Frec_comida_rapida == "8"] <- "No sabe"
ense2012$Frec_comida_rapida[ense2012$Frec_comida_rapida == "9"] <- "No contesta"
ense2012$Frec_comida_rapida<- fct_drop(ense2012$Frec_comida_rapida, only = c("8","9"))
names(ense2012)[345] <- c("Frec_aperitivos")
ense2012$Frec_aperitivos <- fct_expand(ense2012$Frec_aperitivos, c("No sabe", "No contesta")) 
ense2012$Frec_aperitivos[ense2012$Frec_aperitivos == "8"] <- "No sabe"
ense2012$Frec_aperitivos[ense2012$Frec_aperitivos == "9"] <- "No contesta"
ense2012$Frec_aperitivos<- fct_drop(ense2012$Frec_aperitivos, only = c("8","9"))

names(ense2012)[213] <- c("Tabaco")

names(ense2012)[363] <- c("Clase_social_ocup")
names(ense2012)[373] <- c("Educacion")
ense2012$Educacion <- as.factor(ense2012$Educacion)
ense2012$Educacion <- fct_expand(ense2012$Educacion, c("No procede, es menor de 10 años", 
                                                       "No sabe leer o escribir", 
                                                        "Ha asistido menos de 5 años a la escuela (Educación Primaria incompleta)", 
                                                        "Fue 5 o más años a la escuela y no llegó al último curso de la enseñanza obligatoria (Educación Primaria completa)",
                                                        "Enseñanza Secundaria de Primera etapa (ESO, EGB, Bachillerato Elemental)", 
                                                        "Estudios de Bachillerato",
                                                        "Enseñanzas profesionales de grado medio o equivalentes",
                                                       "Enseñanzas profesionales de grado superior o equivalentes",
                                                       "Estudios universitarios o equivalentes",
                                                       "No sabe", "No contesta")) 
ense2012$Educacion[ense2012$Educacion == "1"] <- "No procede, es menor de 10 años"
ense2012$Educacion[ense2012$Educacion == "2"] <- "No sabe leer o escribir"
ense2012$Educacion[ense2012$Educacion == "3"] <- "Ha asistido menos de 5 años a la escuela (Educación Primaria incompleta)"
ense2012$Educacion[ense2012$Educacion == "4"] <- "Fue 5 o más años a la escuela y no llegó al último curso de la enseñanza obligatoria (Educación Primaria completa)"
ense2012$Educacion[ense2012$Educacion == "5"] <- "Enseñanza Secundaria de Primera etapa (ESO, EGB, Bachillerato Elemental)"
ense2012$Educacion[ense2012$Educacion == "6"] <- "Estudios de Bachillerato"
ense2012$Educacion[ense2012$Educacion == "7"] <- "Enseñanzas profesionales de grado medio o equivalentes"
ense2012$Educacion[ense2012$Educacion == "8"] <- "Enseñanzas profesionales de grado superior o equivalentes"
ense2012$Educacion[ense2012$Educacion == "9"] <- "Estudios universitarios o equivalentes"
ense2012$Educacion[ense2012$Educacion == "98"] <- "No sabe"
ense2012$Educacion[ense2012$Educacion == "99"] <- "No contesta"
ense2012$Educacion<- fct_drop(ense2012$Educacion, only = c("1","2","3","4","5","6","7","8","9","98","99"))

names(ense2012)[256] <- c("Frec_alcohol")
ense2012$Frec_alcohol <- fct_expand(ense2012$Frec_alcohol, c("Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida",
                                                             "No en los últimos 12 meses, he dejado de tomar alcohol"))
ense2012$Frec_alcohol[ense2012$T121 == "No" &  ense2012$T122 == "No"] <- "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"
ense2012$Frec_alcohol[ense2012$T121 == "No" &  ense2012$T122 == "Sí"] <- "No en los últimos 12 meses, he dejado de tomar alcohol"

names(ense2012)[257] <- c("Consumo_cerveza")
ense2012$Consumo_cerveza[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[258:264] <- c("Consumo_cerveza_lunes", "Consumo_cerveza_martes",
                              "Consumo_cerveza_mier", "Consumo_cerveza_jueves", "Consumo_cerveza_vier",
                              "Consumo_cerveza_sab","Consumo_cerveza_dom")
names(ense2012)[265] <- c("Consumo_vino")
ense2012$Consumo_vino[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                          ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                          ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[266:272] <- c("Consumo_vino_lunes", "Consumo_vino_martes",
                              "Consumo_vino_mier", "Consumo_vino_jueves", "Consumo_vino_vier",
                              "Consumo_vino_sab","Consumo_vino_dom")
names(ense2012)[273] <- c("Consumo_vermut") 
ense2012$Consumo_vermut[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                            ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                            ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[274:280] <- c("Consumo_vermut_lunes", "Consumo_vermut_martes",
                              "Consumo_vermut_mier", "Consumo_vermut_jueves", "Consumo_vermut_vier",
                              "Consumo_vermut_sab","Consumo_vermut_dom")
names(ense2012)[281] <- c("Consumo_licores")
ense2012$Consumo_licores[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[282:288] <- c("Consumo_licores_lunes", "Consumo_licores_martes",
                              "Consumo_licores_mier", "Consumo_licores_jueves", "Consumo_licores_vier",
                              "Consumo_licores_sab","Consumo_licores_dom")
names(ense2012)[289] <- c("Consumo_destilada") 
ense2012$Consumo_destilada[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                               ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                               ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[290:296] <- c("Consumo_destilada_lunes", "Consumo_destilada_martes",
                              "Consumo_destilada_mier", "Consumo_destilada_jueves", "Consumo_destilada_vier",
                              "Consumo_destilada_sab","Consumo_destilada_dom")
names(ense2012)[297] <- c("Consumo_locales")
ense2012$Consumo_locales[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida" | 
                             ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol" |
                             ense2012$Frec_alcohol == "Una vez al mes o menos"] <- "No"
names(ense2012)[298:304] <- c("Consumo_locales_lunes", "Consumo_locales_martes",
                              "Consumo_locales_mier", "Consumo_locales_jueves", "Consumo_locales_vier",
                              "Consumo_locales_sab","Consumo_locales_dom")
names(ense2012)[306] <- c("Consumo_alcohol_abusivo_dia_hombre")
names(ense2012)[307] <- c("Consumo_alcohol_abusivo_dia_mujer")

names(ense2012)[370] <- c("Consumo_alcohol_medio_semanal")

names(ense2012)[152:163] <- c("Mental_concentrarse", "Mental_preocupaciones", "Mental_util", 
                              "Mental_decisiones", "Mental_agobiado", "Mental_no_superacion", 
                              "Mental_disfrutar", "Mental_frente_problemas", "Mental_deprimido", 
                              "Mental_no_confianza", "Mental_no_valor", "Mental_feliz")
names(ense2012)[368] <- c("Salud_mental_general")

names(ense2012)[351:361] <- c("Social_visitas", "Social_ayuda_hogar", "Social_trabajo", 
                              "Social_personas", "Social_afecto", "Social_hablar_trabajo", 
                              "Social_hablar_personal", "Social_hablar_economico", "Social_salir_fuera", 
                              "Social_consejos", "Social_ayuda_enferm")
names(ense2012)[369] <- c("Apoyo_social_general")


ense2012 <- ense2012[-c(which(is.na(ense2012$Año))), ]
ense2012$Año[ense2012$Año == 2011] <- "2012"

ense2012$ID <- seq(from=45932,to=66938) # CONTINUE 2017 NUMBER OF CASES

ense2012 <- ense2012[c(1:5,364,14,15,123,124,164,165,310:321,330,337,342:345,213,363,373,256:304,306,307,370,
                       152:163,368,351:361,369,370,374)]


ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "0"] <- 0
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "1"] <- 1
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "2"] <- 2
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "3"] <- 3
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "4"] <- 4
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "5"] <- 5
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "6"] <- 6
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "7"] <- 7
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "8"] <- 8
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "10"] <- 10
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "12"] <- 12
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "NS"] <- NA
ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza_lunes == "NC"] <- NA

ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "0"] <- 0
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "1"] <- 1
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "2"] <- 2
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "3"] <- 3
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "4"] <- 4
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "5"] <- 5
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "6"] <- 6
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "7"] <- 7
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "8"] <- 8
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "10"] <- 10
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "12"] <- 12
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "NS"] <- NA
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza_martes == "NC"] <- NA

ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "0"] <- 0
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "1"] <- 1
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "2"] <- 2
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "3"] <- 3
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "4"] <- 4
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "5"] <- 5
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "6"] <- 6
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "7"] <- 7
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "8"] <- 8
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "10"] <- 10
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "12"] <- 12
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "NS"] <- NA
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza_mier == "NC"] <- NA

ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "0"] <- 0
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "1"] <- 1
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "2"] <- 2
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "3"] <- 3
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "4"] <- 4
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "5"] <- 5
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "6"] <- 6
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "7"] <- 7
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "8"] <- 8
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "10"] <- 10
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "12"] <- 12
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "NS"] <- NA
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza_jueves == "NC"] <- NA

ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "0"] <- 0
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "1"] <- 1
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "2"] <- 2
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "3"] <- 3
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "4"] <- 4
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "5"] <- 5
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "6"] <- 6
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "7"] <- 7
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "8"] <- 8
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "9"] <- 9
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "10"] <- 10
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "12"] <- 12
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "NS"] <- NA
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza_vier == "NC"] <- NA

ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "0"] <- 0
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "1"] <- 1
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "2"] <- 2
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "3"] <- 3
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "4"] <- 4
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "5"] <- 5
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "6"] <- 6
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "7"] <- 7
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "8"] <- 8
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "9"] <- 9
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "10"] <- 10
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "11"] <- 11
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "14"] <- 14
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "NS"] <- NA
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza_sab == "NC"] <- NA

ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "0"] <- 0
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "1"] <- 1
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "2"] <- 2
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "3"] <- 3
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "4"] <- 4
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "5"] <- 5
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "6"] <- 6
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "7"] <- 7
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "8"] <- 8
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "9"] <- 9
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "10"] <- 10
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "12"] <- 12
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "NS"] <- NA
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza_dom == "NC"] <- NA

ense2012$Consumo_cerveza_lunes_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_martes_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_mier_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_jueves_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_vier_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_sab_cont[ense2012$Consumo_cerveza == "No"] <- 0
ense2012$Consumo_cerveza_dom_cont[ense2012$Consumo_cerveza == "No"] <- 0


ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "0"] <- 0
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "1"] <- 1
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "2"] <- 2
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "3"] <- 3
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "4"] <- 4
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "5"] <- 5
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "6"] <- 6
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "7"] <- 7
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "8"] <- 8
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "10"] <- 10
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "12"] <- 12
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "NS"] <- NA
ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino_lunes == "NC"] <- NA

ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "0"] <- 0
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "1"] <- 1
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "2"] <- 2
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "3"] <- 3
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "4"] <- 4
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "5"] <- 5
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "6"] <- 6
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "7"] <- 7
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "8"] <- 8
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "10"] <- 10
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "12"] <- 12
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "NS"] <- NA
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino_martes == "NC"] <- NA

ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "0"] <- 0
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "1"] <- 1
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "2"] <- 2
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "3"] <- 3
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "4"] <- 4
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "5"] <- 5
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "6"] <- 6
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "7"] <- 7
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "8"] <- 8
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "10"] <- 10
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "12"] <- 12
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "NS"] <- NA
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino_mier == "NC"] <- NA

ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "0"] <- 0
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "1"] <- 1
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "2"] <- 2
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "3"] <- 3
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "4"] <- 4
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "5"] <- 5
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "6"] <- 6
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "7"] <- 7
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "8"] <- 8
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "10"] <- 10
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "12"] <- 12
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "NS"] <- NA
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino_jueves == "NC"] <- NA

ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "0"] <- 0
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "1"] <- 1
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "2"] <- 2
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "3"] <- 3
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "4"] <- 4
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "5"] <- 5
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "6"] <- 6
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "7"] <- 7
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "8"] <- 8
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "10"] <- 10
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "12"] <- 12
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "NS"] <- NA
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino_vier == "NC"] <- NA

ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "0"] <- 0
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "1"] <- 1
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "2"] <- 2
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "3"] <- 3
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "4"] <- 4
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "5"] <- 5
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "6"] <- 6
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "7"] <- 7
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "8"] <- 8
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "10"] <- 10
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "12"] <- 12
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "NS"] <- NA
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino_sab == "NC"] <- NA

ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "0"] <- 0
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "1"] <- 1
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "2"] <- 2
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "3"] <- 3
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "4"] <- 4
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "5"] <- 5
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "6"] <- 6
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "7"] <- 7
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "8"] <- 8
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "10"] <- 10
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "12"] <- 12
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "NS"] <- NA
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino_dom == "NC"] <- NA

ense2012$Consumo_vino_lunes_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_martes_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_mier_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_jueves_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_vier_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_sab_cont[ense2012$Consumo_vino == "No"] <- 0
ense2012$Consumo_vino_dom_cont[ense2012$Consumo_vino == "No"] <- 0


ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut_lunes == "0"] <- 0
ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut_lunes == "1"] <- 1
ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut_lunes == "4"] <- 4
ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut_lunes == "NS"] <- NA
ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut_lunes == "NC"] <- NA

ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "0"] <- 0
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "1"] <- 1
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "2"] <- 2
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "4"] <- 4
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "NS"] <- NA
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut_martes == "NC"] <- NA

ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "0"] <- 0
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "1"] <- 1
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "2"] <- 2
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "4"] <- 4
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "NS"] <- NA
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut_mier == "NC"] <- NA

ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "0"] <- 0
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "1"] <- 1
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "2"] <- 2
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "4"] <- 4
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "NS"] <- NA
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut_jueves == "NC"] <- NA

ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "0"] <- 0
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "1"] <- 1
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "2"] <- 2
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "3"] <- 3
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "4"] <- 4
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "NS"] <- NA
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut_vier == "NC"] <- NA

ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "0"] <- 0
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "1"] <- 1
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "2"] <- 2
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "3"] <- 3
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "4"] <- 4
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "8"] <- 8
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "NS"] <- NA
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut_sab == "NC"] <- NA

ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "0"] <- 0
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "1"] <- 1
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "2"] <- 2
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "4"] <- 4
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "NS"] <- NA
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut_dom == "NC"] <- NA

ense2012$Consumo_vermut_lunes_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_martes_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_mier_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_jueves_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_vier_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_sab_cont[ense2012$Consumo_vermut == "No"] <- 0
ense2012$Consumo_vermut_dom_cont[ense2012$Consumo_vermut == "No"] <- 0


ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores_lunes == "0"] <- 0
ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores_lunes == "1"] <- 1
ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores_lunes == "2"] <- 2
ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores_lunes == "NS"] <- NA
ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores_lunes == "NC"] <- NA

ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores_martes == "0"] <- 0
ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores_martes == "1"] <- 1
ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores_martes == "2"] <- 2
ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores_martes == "NS"] <- NA
ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores_martes == "NC"] <- NA

ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores_mier == "0"] <- 0
ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores_mier == "1"] <- 1
ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores_mier == "2"] <- 2
ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores_mier == "NS"] <- NA
ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores_mier == "NC"] <- NA

ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores_jueves == "0"] <- 0
ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores_jueves == "1"] <- 1
ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores_jueves == "2"] <- 2
ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores_jueves == "NS"] <- NA
ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores_jueves == "NC"] <- NA

ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "0"] <- 0
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "1"] <- 1
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "2"] <- 2
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "3"] <- 3
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "4"] <- 4
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "NS"] <- NA
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores_vier == "NC"] <- NA

ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "0"] <- 0
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "1"] <- 1
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "2"] <- 2
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "3"] <- 3
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "4"] <- 4
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "5"] <- 5
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "10"] <- 10
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "NS"] <- NA
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores_sab == "NC"] <- NA

ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "0"] <- 0
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "1"] <- 1
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "2"] <- 2
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "3"] <- 3
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "NS"] <- NA
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores_dom == "NC"] <- NA

ense2012$Consumo_licores_lunes_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_martes_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_mier_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_jueves_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_vier_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_sab_cont[ense2012$Consumo_licores == "No"] <- 0
ense2012$Consumo_licores_dom_cont[ense2012$Consumo_licores == "No"] <- 0


ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "0"] <- 0
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "1"] <- 1
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "2"] <- 2
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "3"] <- 3
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "6"] <- 6
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "NS"] <- NA
ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada_lunes == "NC"] <- NA

ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "0"] <- 0
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "1"] <- 1
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "2"] <- 2
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "3"] <- 3
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "4"] <- 4
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "6"] <- 6
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "10"] <- 10
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "NS"] <- NA
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada_martes == "NC"] <- NA

ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "0"] <- 0
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "1"] <- 1
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "2"] <- 2
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "3"] <- 3
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "4"] <- 4
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "6"] <- 6
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "NS"] <- NA
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada_mier == "NC"] <- NA

ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "0"] <- 0
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "1"] <- 1
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "2"] <- 2
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "3"] <- 3
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "4"] <- 4
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "5"] <- 5
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "6"] <- 6
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "NS"] <- NA
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada_jueves == "NC"] <- NA

ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "0"] <- 0
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "1"] <- 1
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "2"] <- 2
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "3"] <- 3
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "4"] <- 4
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "5"] <- 5
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "6"] <- 6
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "7"] <- 7
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "10"] <- 10
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "NS"] <- NA
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada_vier == "NC"] <- NA

ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "0"] <- 0
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "1"] <- 1
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "2"] <- 2
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "3"] <- 3
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "4"] <- 4
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "5"] <- 5
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "6"] <- 6
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "7"] <- 7
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "8"] <- 8
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "9"] <- 9
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "10"] <- 10
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "15"] <- 15
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "NS"] <- NA
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada_sab == "NC"] <- NA

ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "0"] <- 0
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "1"] <- 1
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "2"] <- 2
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "3"] <- 3
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "4"] <- 4
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "5"] <- 5
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "6"] <- 6
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "NS"] <- NA
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada_dom == "NC"] <- NA

ense2012$Consumo_destilada_lunes_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_martes_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_mier_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_jueves_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_vier_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_sab_cont[ense2012$Consumo_destilada == "No"] <- 0
ense2012$Consumo_destilada_dom_cont[ense2012$Consumo_destilada == "No"] <- 0


ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "0"] <- 0
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "1"] <- 1
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "2"] <- 2
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "3"] <- 3
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "4"] <- 4
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "5"] <- 5
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "6"] <- 6
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "NS"] <- NA
ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales_lunes == "NC"] <- NA

ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "0"] <- 0
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "1"] <- 1
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "2"] <- 2
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "3"] <- 3
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "4"] <- 4
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "5"] <- 5
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "6"] <- 6
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "NS"] <- NA
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales_martes == "NC"] <- NA

ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "0"] <- 0
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "1"] <- 1
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "2"] <- 2
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "3"] <- 3
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "4"] <- 4
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "5"] <- 5
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "6"] <- 6
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "NS"] <- NA
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales_mier == "NC"] <- NA

ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "0"] <- 0
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "1"] <- 1
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "2"] <- 2
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "3"] <- 3
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "4"] <- 4
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "5"] <- 5
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "6"] <- 6
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "NS"] <- NA
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales_jueves == "NC"] <- NA

ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "0"] <- 0
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "1"] <- 1
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "2"] <- 2
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "3"] <- 3
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "4"] <- 4
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "5"] <- 5
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "6"] <- 6
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "8"] <- 8
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "9"] <- 9
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "12"] <- 12
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "NS"] <- NA
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales_vier == "NC"] <- NA

ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "0"] <- 0
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "1"] <- 1
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "2"] <- 2
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "3"] <- 3
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "4"] <- 4
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "5"] <- 5
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "6"] <- 6
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "8"] <- 8
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "9"] <- 9
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "10"] <- 10
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "12"] <- 12
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "NS"] <- NA
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales_sab == "NC"] <- NA

ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "0"] <- 0
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "1"] <- 1
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "2"] <- 2
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "3"] <- 3
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "4"] <- 4
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "5"] <- 5
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "6"] <- 6
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "9"] <- 9
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "NS"] <- NA
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales_dom == "NC"] <- NA

ense2012$Consumo_locales_lunes_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_martes_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_mier_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_jueves_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_vier_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_sab_cont[ense2012$Consumo_locales == "No"] <- 0
ense2012$Consumo_locales_dom_cont[ense2012$Consumo_locales == "No"] <- 0


ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "0"] <- 0
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1000"] <- 1000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "10000"] <- 10000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "10286"] <- 10286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "10571"] <- 10571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "11000"] <-11000 
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1143"] <- 1143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "12000"] <- 12000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "12143"] <- 12143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "12286"] <- 12286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "12571"] <- 12571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1286"] <- 1286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "14000"] <-14000 
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1429"] <- 1429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "143"] <- 143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "14429"] <- 14429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1571"] <- 1571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1714"] <- 1714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "17143"] <-17143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "1857"] <-1857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2000"] <-2000 
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2143"] <- 2143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2286"] <- 2286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2429"] <- 2429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2571"] <- 2571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2714"] <- 2714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "2857"] <- 2857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "286"] <- 286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3000"] <- 3000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3143"] <- 3143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3286"] <-3286 
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3429"] <- 3429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3571"] <- 3571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3714"] <- 3714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "3857"] <- 3857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4000"] <- 4000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4143"] <- 4143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4286"] <- 4286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "429"] <- 429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4429"] <-4429 
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4571"] <- 4571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4714"] <- 4714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "4857"] <- 4857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5000"] <- 5000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5143"] <- 5143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5286"] <- 5286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5429"] <- 5429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5571"] <- 5571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "571"] <- 571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5714"] <- 5714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "5857"] <- 5857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6000"] <- 6000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6143"] <- 6143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6286"] <- 6286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6429"] <- 6429
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6571"] <- 6571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "6857"] <- 6857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "7000"] <- 7000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "714"] <- 714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "7143"] <- 7143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "7286"] <- 7286
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "7571"] <- 7571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "7857"] <- 7857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "8000"] <- 8000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "8143"] <- 8143
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "857"] <- 857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "8571"] <- 8571
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "8714"] <- 8714
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "8857"] <- 8857
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "9000"] <- 9000
ense2012$Consumo_alcohol_medio_semanal_cont[ense2012$Consumo_alcohol_medio_semanal == "NC"] <- NA

ense2012$Medicamentos_receta[ense2012$Medicamentos_receta_hombre == "Sí"] <- "Sí"
ense2012$Medicamentos_receta[ense2012$Medicamentos_receta_hombre == "No"] <- "No"
ense2012$Medicamentos_receta[ense2012$Medicamentos_receta_mujer == "Sí"] <- "Sí"
ense2012$Medicamentos_receta[ense2012$Medicamentos_receta_mujer == "No"] <- "No"

ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "Nunca"] <- "Nunca"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "Menos de una vez al mes"] <- "Menos de una vez al mes"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "Mensualmente"] <- "Mensualmente"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "Semanalmente"] <- "Semanalmente"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "A diario o casi a diario"] <- "A diario o casi a diario"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "NS"] <- "NS"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_hombre == "NC"] <- "NC"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "Nunca"] <- "Nunca"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "Menos de una vez al mes"] <- "Menos de una vez al mes"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "Mensualmente"] <- "Mensualmente"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "Semanalmente"] <- "Semanalmente"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "A diario o casi a diario"] <- "A diario o casi a diario"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "NS"] <- "NS"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Consumo_alcohol_abusivo_dia_mujer == "NC"] <- "NC"

ense2012$Consumo_alcohol_abusivo_dia <- fct_expand(ense2012$Consumo_alcohol_abusivo_dia, c("Ex_bebedor")) 
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"] <- "Nunca"
ense2012$Consumo_alcohol_abusivo_dia[ense2012$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol"] <- "Ex_bebedor"


levels(ense2012$Salud_mental_general)
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "0"] <- 0
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "1"] <- 1
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "2"] <- 2
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "3"] <- 3
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "4"] <- 4
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "5"] <- 5
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "6"] <- 6
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "7"] <- 7
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "8"] <- 8
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "9"] <- 9
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "10"] <- 10
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "11"] <- 11
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "12"] <- 12
ense2012$Salud_mental_general_cont[ense2012$Salud_mental_general == "NC"] <- NA

levels(ense2012$Apoyo_social_general)
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "11"] <- 11
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "12"] <- 12
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "13"] <- 13
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "14"] <- 14
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "15"] <- 15
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "16"] <- 16
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "17"] <- 17
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "18"] <- 18
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "19"] <- 19
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "20"] <- 20
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "21"] <- 21
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "22"] <- 22
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "23"] <- 23
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "24"] <- 24
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "25"] <- 25
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "26"] <- 26
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "27"] <- 27
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "28"] <- 28
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "29"] <- 29
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "30"] <- 30
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "31"] <- 31
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "32"] <- 32
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "33"] <- 33
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "34"] <- 34
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "35"] <- 35
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "36"] <- 36
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "37"] <- 37
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "38"] <- 38
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "39"] <- 39
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "40"] <- 40
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "41"] <- 41
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "42"] <- 42
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "43"] <- 43
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "44"] <- 44
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "45"] <- 45
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "46"] <- 46
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "47"] <- 47
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "48"] <- 48
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "49"] <- 49
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "50"] <- 50
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "51"] <- 51
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "52"] <- 52
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "53"] <- 53
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "54"] <- 54
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "55"] <- 55
ense2012$Apoyo_social_general_cont[ense2012$Apoyo_social_general == "NC"] <- NA


ense2012 <- ense2012[c(1:13,25:35,43,51,59,67,75,83,84,86:110,112:159)]



#### Union ENSE ####
Total_data <- bind_rows(list(ense2017, ense2012))
cols2 <- c(1,11); Total_data[cols2] <- lapply(Total_data[cols2], factor) 


levels(Total_data$CCAA)
Total_data$CCAA[Total_data$CCAA == "Andalucía"] <- "Andalucia"
Total_data$CCAA[Total_data$CCAA == "Aragón"] <- "Aragon"
Total_data$CCAA[Total_data$CCAA == "Asturias, Principado de"] <- "Asturias"
Total_data$CCAA[Total_data$CCAA == "Balears, Illes"] <- "Baleares"
Total_data$CCAA[Total_data$CCAA == "Castilla - La Mancha"] <- "Castilla la Mancha"
Total_data$CCAA[Total_data$CCAA == "Comunitat Valenciana"] <- "Com Valenciana"
Total_data$CCAA[Total_data$CCAA == "Madrid, Comunidad de"] <- "Madrid"
Total_data$CCAA[Total_data$CCAA == "Murcia, Región de"] <- "Murcia"
Total_data$CCAA[Total_data$CCAA == "Navarra, Comunidad Foral de"] <- "Navarra"
Total_data$CCAA[Total_data$CCAA == "Rioja, La"] <- "La Rioja"
Total_data$CCAA[Total_data$CCAA == "Asturias (Princ de)"] <- "Asturias"
Total_data$CCAA[Total_data$CCAA == "Baleares (Islas)"] <- "Baleares"
Total_data$CCAA[Total_data$CCAA == "Castilla La Mancha"] <- "Castilla la Mancha"
Total_data$CCAA[Total_data$CCAA == "Comunidad Valenciana"] <- "Com Valenciana"
Total_data$CCAA[Total_data$CCAA == "Madrid (Comunidad de)"] <- "Madrid"
Total_data$CCAA[Total_data$CCAA == "Murcia (Región de)"] <- "Murcia"
Total_data$CCAA[Total_data$CCAA == "Navarra (Comunidad Foral)"] <- "Navarra"
Total_data$CCAA<- fct_drop(Total_data$CCAA, only = c("Andalucía","Aragón","Asturias, Principado de","Balears, Illes",
                                                     "Castilla - La Mancha","Comunitat Valenciana","Madrid, Comunidad de",
                                                     "Murcia, Región de","Navarra, Comunidad Foral de","Rioja, La",
                                                     "Asturias (Princ de)","Baleares (Islas)","Castilla La Mancha",
                                                     "Comunidad Valenciana","Madrid (Comunidad de)","Murcia (Región de)",
                                                     "Navarra (Comunidad Foral)"))


levels(Total_data$IMC_grupo)
Total_data$IMC_grupo[Total_data$IMC_grupo == "peso insuficiente"] <- "Peso insuficiente"
Total_data$IMC_grupo[Total_data$IMC_grupo == "Peso infusiciente"] <- "Peso insuficiente"
Total_data$IMC_grupo[Total_data$IMC_grupo == "sobrepeso"] <- "Sobrepeso"
Total_data$IMC_grupo[Total_data$IMC_grupo == "obesidad"] <- "Obesidad"
Total_data$IMC_grupo[Total_data$IMC_grupo == "NC"] <- "NS/NC"
Total_data$IMC_grupo[Total_data$IMC_grupo == "No consta"] <- "NS/NC"
Total_data$IMC_grupo<- fct_drop(Total_data$IMC_grupo, only = c("peso insuficiente","sobrepeso","obesidad","NC",
                                                               "No consta","Peso infusiciente"))


levels(Total_data$Salud_percibida)
Total_data$Salud_percibida[Total_data$Salud_percibida == "muy malo"] <- "Muy malo"
Total_data$Salud_percibida[Total_data$Salud_percibida == "malo"] <- "Malo"
Total_data$Salud_percibida[Total_data$Salud_percibida == "regular"] <- "Regular"
Total_data$Salud_percibida[Total_data$Salud_percibida == "bueno"] <- "Bueno"
Total_data$Salud_percibida[Total_data$Salud_percibida == "Buneo"] <- "Bueno"
Total_data$Salud_percibida<- fct_drop(Total_data$Salud_percibida, only = c("muy malo","malo","regular","bueno","Buneo"))


levels(Total_data$Enfermedad_cronica)
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "si"] <- "Sí"
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "no"] <- "No"
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "ns"] <- "NS"
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "nc"] <- "NC"
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "No sabe"] <- "NS"
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "No conetsta"] <- "NC"
Total_data$Enfermedad_cronica<- fct_drop(Total_data$Enfermedad_cronica, only = c("si","no","ns","nc","No sabe", "No conetsta"))

levels(Total_data$Grado_limitacion)
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "gravemente limitado"] <- "Gravemente limitado"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "Gravemente limitado/a"] <- "Gravemente limitado"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "Limitado/a, pero no gravemente"] <- "Limitado pero no gravemente"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "Nada limitado/a"] <- "Nada limitado"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "No sabe"] <- "NS"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "No contesta"] <- "NC"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "ns"] <- "NS"
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "nc"] <- "NC"
Total_data$Grado_limitacion<- fct_drop(Total_data$Grado_limitacion, only = c("gravemente limitado", "Gravemente limitado/a","Limitado/a, pero no gravemente","Nada limitado/a","ns","nc","No sabe", "No contesta"))

levels(Total_data$Tipo_problema_enf)
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "fisico"] <- "Físico"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "mental"] <- "Mental"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "ambos"] <- "Ambos"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "No sabe"] <- "NS"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "No contesta"] <- "NC"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "ns"] <- "NS"
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "nc"] <- "NC"
Total_data$Tipo_problema_enf<- fct_drop(Total_data$Tipo_problema_enf, only = c("fisico", "mental","ambos","ns","nc","No sabe", "No contesta"))

levels(Total_data$Lugar_residencia)
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS DE MÁS DE 500.000 HABITANTES"] <- "Municipios de más de 500.000 habitantes"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS CAPITAL DE PROVINCIA (EXCEPTO LOS ANTERIORES)"] <- "Municipio capital de provincia (excepto los anteriores)"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS CON MÁS DE 100.000 HABITANTES (EXCEPTO LOS ANTERIORES)"] <- "Municipios con más de 100.000 habitantes (excepto los anteriores)"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS DE 50.001 A 100.000 HABITANTES (EXCEPTO LOS ANTERIORES)"] <- "Municipios de 50.000 a 100.000 habitantes (excepto los anteriores)"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS DE 20.001 A 50.000 HABITANTES"] <- "Municipios de 20.000 a 50.000 habitantes (excepto los anteriores)"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS DE 10.001 A 20.000 HABITANTES"] <- "Municipios de 10.000 a 20.000 habitantes"
Total_data$Lugar_residencia[Total_data$Lugar_residencia == "MUNICIPIOS HASTA 10.000 HABITANTES"] <- "Municipios con menos de 10.000 habitantes"
Total_data$Lugar_residencia<- fct_drop(Total_data$Lugar_residencia, only = c("MUNICIPIOS DE MÁS DE 500.000 HABITANTES", 
                                                                             "MUNICIPIOS CAPITAL DE PROVINCIA (EXCEPTO LOS ANTERIORES)",
                                                                             "MUNICIPIOS CON MÁS DE 100.000 HABITANTES (EXCEPTO LOS ANTERIORES)",
                                                                             "MUNICIPIOS DE 50.001 A 100.000 HABITANTES (EXCEPTO LOS ANTERIORES)",
                                                                             "MUNICIPIOS DE 20.001 A 50.000 HABITANTES",
                                                                             "MUNICIPIOS DE 10.001 A 20.000 HABITANTES", 
                                                                             "MUNICIPIOS HASTA 10.000 HABITANTES"))

levels(Total_data$Medicamentos_receta)
Total_data$Medicamentos_receta[Total_data$Medicamentos_receta == "no"] <- "No"
Total_data$Medicamentos_receta[Total_data$Medicamentos_receta == "si"] <- "Sí"
Total_data$Medicamentos_receta<- fct_drop(Total_data$Medicamentos_receta, only = c("si","no"))

levels(Total_data$Medicamentos_no_receta)
Total_data$Medicamentos_no_receta[Total_data$Medicamentos_no_receta == "no"] <- "No"
Total_data$Medicamentos_no_receta[Total_data$Medicamentos_no_receta == "si"] <- "Sí"
Total_data$Medicamentos_no_receta[Total_data$Medicamentos_no_receta == "ns"] <- "Ns/Nc"
Total_data$Medicamentos_no_receta[Total_data$Medicamentos_no_receta == "nc"] <- "Ns/Nc"
Total_data$Medicamentos_no_receta<- fct_drop(Total_data$Medicamentos_no_receta, only = c("si","no","ns","nc"))

levels(Total_data$Frec_AF_libre)
Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "No hago ejercicio. El TL lo ocupo de forma casi completamente sedentaria (leer, ver la televisión, ir al cine, etc.)"] <- "No hago ejercicio. El tiempo libre lo ocupo de forma casi completamente sedentaria"
Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "Hace entrenamiento deportivo o físico varias veces a la semana"] <- "Hago entrenamiento deportivo o físico varias veces a la semana"
Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "ns"] <- "NS"
Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "nc"] <- "NC"
Total_data$Frec_AF_libre<- fct_drop(Total_data$Frec_AF_libre, only = c("No hago ejercicio. El TL lo ocupo de forma casi completamente sedentaria (leer, ver la televisión, ir al cine, etc.)",
                                                                       "Hace entrenamiento deportivo o físico varias veces a la semana",
                                                                       "ns","nc"))

levels(Total_data$Frec_fruta)
Total_data$Frec_fruta[Total_data$Frec_fruta == "A diario"] <- "Una o más veces al día"
Total_data$Frec_fruta[Total_data$Frec_fruta == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_fruta[Total_data$Frec_fruta == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_fruta[Total_data$Frec_fruta == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_fruta[Total_data$Frec_fruta == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_fruta[Total_data$Frec_fruta == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_fruta[Total_data$Frec_fruta == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_fruta[Total_data$Frec_fruta == "ns"] <- "NS"
Total_data$Frec_fruta[Total_data$Frec_fruta == "nc"] <- "NC"
Total_data$Frec_fruta[Total_data$Frec_fruta == "No sabe"] <- "NS"
Total_data$Frec_fruta[Total_data$Frec_fruta == "No contesta"] <- "NC"
Total_data$Frec_fruta<- fct_drop(Total_data$Frec_fruta, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_verdura)
Total_data$Frec_verdura[Total_data$Frec_verdura == "A diario"] <- "Una o más veces al día"
Total_data$Frec_verdura[Total_data$Frec_verdura == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_verdura[Total_data$Frec_verdura == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_verdura[Total_data$Frec_verdura == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_verdura[Total_data$Frec_verdura == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_verdura[Total_data$Frec_verdura == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_verdura[Total_data$Frec_verdura == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_verdura[Total_data$Frec_verdura == "ns"] <- "NS"
Total_data$Frec_verdura[Total_data$Frec_verdura == "nc"] <- "NC"
Total_data$Frec_verdura[Total_data$Frec_verdura == "No sabe"] <- "NS"
Total_data$Frec_verdura[Total_data$Frec_verdura == "No contesta"] <- "NC"
Total_data$Frec_verdura<- fct_drop(Total_data$Frec_verdura, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_dulces)
Total_data$Frec_dulces[Total_data$Frec_dulces == "A diario"] <- "Una o más veces al día"
Total_data$Frec_dulces[Total_data$Frec_dulces == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_dulces[Total_data$Frec_dulces == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_dulces[Total_data$Frec_dulces == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_dulces[Total_data$Frec_dulces == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_dulces[Total_data$Frec_dulces == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_dulces[Total_data$Frec_dulces == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_dulces[Total_data$Frec_dulces == "ns"] <- "NS"
Total_data$Frec_dulces[Total_data$Frec_dulces == "nc"] <- "NC"
Total_data$Frec_dulces[Total_data$Frec_dulces == "No sabe"] <- "NS"
Total_data$Frec_dulces[Total_data$Frec_dulces == "No contesta"] <- "NC"
Total_data$Frec_dulces<- fct_drop(Total_data$Frec_dulces, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_refrescos)
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "A diario"] <- "Una o más veces al día"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "ns"] <- "NS"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "nc"] <- "NC"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "No sabe"] <- "NS"
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "No contesta"] <- "NC"
Total_data$Frec_refrescos<- fct_drop(Total_data$Frec_refrescos, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_comida_rapida)
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "A diario"] <- "Una o más veces al día"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "ns"] <- "NS"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "nc"] <- "NC"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "No sabe"] <- "NS"
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "No contesta"] <- "NC"
Total_data$Frec_comida_rapida<- fct_drop(Total_data$Frec_comida_rapida, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_aperitivos)
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "A diario"] <- "Una o más veces al día"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "Uno o más veces al día"] <- "Una o más veces al día"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "De 4 a 6 veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "Tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "tres veces a la semana"] <- "Tres o más vaces a la semana, pero no a diario"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "Nunca"] <- "Nunca o casi nunca"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "Menos de 1 vez a la semana"] <- "Menos de una vez a la semana"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "ns"] <- "NS"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "nc"] <- "NC"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "No sabe"] <- "NS"
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "No contesta"] <- "NC"
Total_data$Frec_aperitivos<- fct_drop(Total_data$Frec_aperitivos, only = c("A diario","Uno o más veces al día",
                                                                 "De 4 a 6 veces a la semana","Tres veces a la semana",
                                                                 "tres veces a la semana","Nunca","Menos de 1 vez a la semana",
                                                                 "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Frec_alcohol)
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "Menos de una vez al mes"] <- "Una vez al mes o menos"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "Una vez al mes"] <- "Una vez al mes o menos"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "Todos los días"] <- "A diario o casi a diario"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "De 1 a 2 veces a la semana"] <- "1-2 días por semana"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "De 2 a 3 veces al mes"] <- "2-3 días en un mes"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "3-4 días por semana"] <- "De 3 a 6 veces a la semana"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "4-4 días por semana"] <- "De 3 a 6 veces a la semana"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "5-6 días por semana"] <- "De 3 a 6 veces a la semana"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "ns"] <- "NS"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "nc"] <- "NC"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "No sabe"] <- "NS"
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "No contesta"] <- "NC"
Total_data$Frec_alcohol<- fct_drop(Total_data$Frec_alcohol, only = c("Menos de una vez al mes","Una vez al mes","Todos los días",
                                                                           "De 1 a 2 veces a la semana","De 2 a 3 veces al mes",
                                                                           "3-4 días por semana","4-4 días por semana","5-6 días por semana",
                                                                           "ns","nc", "No sabe", "No contesta"))

levels(Total_data$Tabaco)
Total_data$Tabaco <- fct_expand(Total_data$Tabaco, c("Fumador", "Exfumador", "Nunca")) 
Total_data$Tabaco[Total_data$Tabaco == "Sí fuma, diariamente"] <- "Fumador"
Total_data$Tabaco[Total_data$Tabaco == "Sí fuma, pero no diariamente"] <- "Fumador"
Total_data$Tabaco[Total_data$Tabaco == "Sí, fumo a diario"] <- "Fumador"
Total_data$Tabaco[Total_data$Tabaco == "Sí fumo, pero no a diario"] <- "Fumador"
Total_data$Tabaco[Total_data$Tabaco == "No fumo actualmente, pero he fumado antes"] <- "Exfumador"
Total_data$Tabaco[Total_data$Tabaco == "No fuma actualmente, pero ha fumado antes"] <- "Exfumador"
Total_data$Tabaco[Total_data$Tabaco == "No fumo ni he fumado nunca de manera habitual"] <- "Nunca"
Total_data$Tabaco[Total_data$Tabaco == "No fuma ni ha fumado nunca de manera habitual"] <- "Nunca"
Total_data$Tabaco[Total_data$Tabaco == "ns"] <- "NS"
Total_data$Tabaco[Total_data$Tabaco == "nc"] <- "NC"
Total_data$Tabaco<- fct_drop(Total_data$Tabaco, only = c("Sí fuma, diariamente","Sí fuma, pero no diariamente","Sí, fumo a diario",
                                                         "Sí fumo, pero no a diario","No fumo actualmente, pero he fumado antes",
                                                         "No fuma actualmente, pero ha fumado antes","No fumo ni he fumado nunca de manera habitual",
                                                         "No fuma ni ha fumado nunca de manera habitual",
                                                         "ns","nc"))


levels(Total_data$Clase_social_ocup)
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Directores de establecimientos de 10 o más asalariados/as y profesionales asociados/as a  universitarias"] <- "Clase Social I"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Directores de establecimientos de menos de 10 asalariados y profesionales asociados/as a diplomaturas universitarias"] <- "Clase Social II"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Ocupaciones intermedias y trabajadores/as por cuenta propia"] <- "Clase Social III"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Supervisores/as y trabajadores/as en ocupaciones técnicas"] <- "Clase Social IV"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Trabajadores/as cualificados/as del sector primario y otros/as"] <- "Clase Social V"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "Trabajadores/as no cualificados/as"] <- "Clase Social VI"
Total_data$Clase_social_ocup[Total_data$Clase_social_ocup == "ns nc"] <- "NS/NC"
Total_data$Clase_social_ocup<- fct_drop(Total_data$Clase_social_ocup, only = c("Directores de establecimientos de 10 o más asalariados/as y profesionales asociados/as a  universitarias", 
                                                                             "Directores de establecimientos de menos de 10 asalariados y profesionales asociados/as a diplomaturas universitarias",
                                                                             "Ocupaciones intermedias y trabajadores/as por cuenta propia",
                                                                             "Supervisores/as y trabajadores/as en ocupaciones técnicas",
                                                                             "Trabajadores/as cualificados/as del sector primario y otros/as",
                                                                             "Trabajadores/as no cualificados/as", 
                                                                             "ns nc"))

levels(Total_data$Educacion)
Total_data$Educacion[Total_data$Educacion == "Ha asistido menos de 5 años a la escuela (Educación Primaria incompleta)"] <- "Educación Primaria incompleta (Ha asistido menos de 5 años a la escuela)"
Total_data$Educacion[Total_data$Educacion == "Fue 5 o más años a la escuela y no llegó al último curso de la enseñanza obligatoria (Educación Primaria completa)"] <- "Educación Primaria completa"
Total_data$Educacion[Total_data$Educacion == "Enseñanza Secundaria de Primera etapa (ESO, EGB, Bachillerato Elemental)"] <- "Primera etapa de Enseñanza Secundaria, con o sin título (2º ESO aprobado, EGB, Bachillerato Elemental)"
Total_data$Educacion<- fct_drop(Total_data$Educacion, only = c("Ha asistido menos de 5 años a la escuela (Educación Primaria incompleta)", 
                                                                               "Fue 5 o más años a la escuela y no llegó al último curso de la enseñanza obligatoria (Educación Primaria completa)",
                                                                               "Enseñanza Secundaria de Primera etapa (ESO, EGB, Bachillerato Elemental)"))

levels(Total_data$Consumo_cerveza)
Total_data$Consumo_cerveza[Total_data$Consumo_cerveza == "no"] <- "No"
Total_data$Consumo_cerveza[Total_data$Consumo_cerveza == "si"] <- "Sí"
Total_data$Consumo_cerveza[Total_data$Consumo_cerveza == "ns"] <- "NS"
Total_data$Consumo_cerveza[Total_data$Consumo_cerveza == "nc"] <- "NC"
Total_data$Consumo_cerveza<- fct_drop(Total_data$Consumo_cerveza, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_vermut)
Total_data$Consumo_vermut[Total_data$Consumo_vermut == "no"] <- "No"
Total_data$Consumo_vermut[Total_data$Consumo_vermut == "si"] <- "Sí"
Total_data$Consumo_vermut[Total_data$Consumo_vermut == "ns"] <- "NS"
Total_data$Consumo_vermut[Total_data$Consumo_vermut == "nc"] <- "NC"
Total_data$Consumo_vermut<- fct_drop(Total_data$Consumo_vermut, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_destilada)
Total_data$Consumo_destilada[Total_data$Consumo_destilada == "no"] <- "No"
Total_data$Consumo_destilada[Total_data$Consumo_destilada == "si"] <- "Sí"
Total_data$Consumo_destilada[Total_data$Consumo_destilada == "ns"] <- "NS"
Total_data$Consumo_destilada[Total_data$Consumo_destilada == "nc"] <- "NC"
Total_data$Consumo_destilada<- fct_drop(Total_data$Consumo_destilada, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_vino)
Total_data$Consumo_vino[Total_data$Consumo_vino == "no"] <- "No"
Total_data$Consumo_vino[Total_data$Consumo_vino == "si"] <- "Sí"
Total_data$Consumo_vino[Total_data$Consumo_vino == "ns"] <- "NS"
Total_data$Consumo_vino[Total_data$Consumo_vino == "nc"] <- "NC"
Total_data$Consumo_vino<- fct_drop(Total_data$Consumo_vino, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_locales)
Total_data$Consumo_locales[Total_data$Consumo_locales == "no"] <- "No"
Total_data$Consumo_locales[Total_data$Consumo_locales == "si"] <- "Sí"
Total_data$Consumo_locales[Total_data$Consumo_locales == "ns"] <- "NS"
Total_data$Consumo_locales[Total_data$Consumo_locales == "nc"] <- "NC"
Total_data$Consumo_locales<- fct_drop(Total_data$Consumo_locales, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_licores)
Total_data$Consumo_licores[Total_data$Consumo_licores == "no"] <- "No"
Total_data$Consumo_licores[Total_data$Consumo_licores == "si"] <- "Sí"
Total_data$Consumo_licores[Total_data$Consumo_licores == "ns"] <- "NS"
Total_data$Consumo_licores[Total_data$Consumo_licores == "nc"] <- "NC"
Total_data$Consumo_licores<- fct_drop(Total_data$Consumo_licores, only = c("no","si","ns","nc"))

levels(Total_data$Consumo_alcohol_abusivo_dia)
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "Nunca"] <- "Nunca en toda mi vida"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "Nunca en toda la vida"] <- "Nunca en toda mi vida"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "De 5 a 6 días por semana"] <- "Semanalmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "De 3 a 4 días por semana"] <- "Semanalmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "De 1 a 2 días por semana"] <- "Semanalmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "De 2 a 3 días en un mes"] <- "Mensualmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "De 2 a 3 días por semana"] <- "Mensualmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "Una vez al mes"] <- "Semanalmente"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "No sabe"] <- "NS"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "No contesta"] <- "NC"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "ns"] <- "NS"
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "nc"] <- "NC"
Total_data$Consumo_alcohol_abusivo_dia<- fct_drop(Total_data$Consumo_alcohol_abusivo_dia, only = c("Nunca en toda la vida","Nunca",
                                                                               "De 3 a 4 días por semana","De 5 a 6 días por semana",
                                                                               "De 2 a 3 días en un mes","De 1 a 2 días por semana",
                                                                               "Una vez al mes", "De 2 a 3 días por semana",
                                                                               "ns", "nc", "No sabe", "No contesta"))

levels(Total_data$Mental_concentrarse)
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "mejor que lo habitual"] <- "Mejor que lo habitual"
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "igual que lo habitual"] <- "Igual que lo habitual"
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "menos que lo habitual"] <- "Menos que lo habitual"
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "mucho menos que lo habitual"] <- "Mucho menos que lo habitual"
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "ns"] <- "NS"
Total_data$Mental_concentrarse[Total_data$Mental_concentrarse == "nc"] <- "NC"
Total_data$Mental_concentrarse<- fct_drop(Total_data$Mental_concentrarse, only = c("mejor que lo habitual",
                                                                                    "igual que lo habitual",
                                                                                    "menos que lo habitual",
                                                                                    "mucho menos que lo habitual", 
                                                                                    "ns", "nc"))

levels(Total_data$Mental_preocupaciones)
Total_data$Mental_preocupaciones[Total_data$Mental_preocupaciones == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_preocupaciones[Total_data$Mental_preocupaciones == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_preocupaciones[Total_data$Mental_preocupaciones == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_preocupaciones[Total_data$Mental_preocupaciones == "ns"] <- "NS"
Total_data$Mental_preocupaciones[Total_data$Mental_preocupaciones == "nc"] <- "NC"
Total_data$Mental_preocupaciones<- fct_drop(Total_data$Mental_preocupaciones, only = c("no mas que lo habitual",
                                                                                   "algo mas que lo habitual",
                                                                                   "mucho mas que lo habitual",
                                                                                   "ns", "nc"))

levels(Total_data$Mental_util)
Total_data$Mental_util[Total_data$Mental_util == "mucho menos que lo habitual"] <- "Mucho menos que lo habitual"
Total_data$Mental_util[Total_data$Mental_util == "menos que lo habitual"] <- "Menos útil que lo habitual"
Total_data$Mental_util[Total_data$Mental_util == "igual que lo habitual"] <- "Igual que lo habitual"
Total_data$Mental_util[Total_data$Mental_util == "mas que lo habitual"] <- "Más útil que lo habitual"
Total_data$Mental_util[Total_data$Mental_util == "ns"] <- "NS"
Total_data$Mental_util[Total_data$Mental_util == "nc"] <- "NC"
Total_data$Mental_util<- fct_drop(Total_data$Mental_util, only = c("mas que lo habitual",
                                                                                   "igual que lo habitual",
                                                                                   "menos que lo habitual",
                                                                                   "mucho menos que lo habitual", 
                                                                                   "ns", "nc"))

levels(Total_data$Mental_decisiones)
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "Más útil que lo habitual"] <- "mas que lo habitual"
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "Igual que lo habitual"] <- "igual que lo habitual"
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "Menos útil que lo habitual"] <- "menos que lo habitual"
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "Mucho menos que lo habitual"] <- "mucho menos que lo habitual"
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "ns"] <- "NS"
Total_data$Mental_decisiones[Total_data$Mental_decisiones == "nc"] <- "NC"
Total_data$Mental_decisiones<- fct_drop(Total_data$Mental_decisiones, only = c("Más útil que lo habitual",
                                                                   "Igual que lo habitual",
                                                                   "Menos útil que lo habitual",
                                                                   "Mucho menos que lo habitual", 
                                                                   "ns", "nc"))

levels(Total_data$Mental_agobiado)
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "no, en absoluto"] <- "No, en absoluto"
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "ns"] <- "NS"
Total_data$Mental_agobiado[Total_data$Mental_agobiado == "nc"] <- "NC"
Total_data$Mental_agobiado<- fct_drop(Total_data$Mental_agobiado, only = c("no, en absoluto",
                                                                               "no mas que lo habitual",
                                                                               "algo mas que lo habitual",
                                                                               "mucho mas que lo habitual", 
                                                                               "ns", "nc"))

levels(Total_data$Mental_no_superacion)
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "no, en absoluto"] <- "No, en absoluto"
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "ns"] <- "NS"
Total_data$Mental_no_superacion[Total_data$Mental_no_superacion == "nc"] <- "NC"
Total_data$Mental_no_superacion<- fct_drop(Total_data$Mental_no_superacion, only = c("no, en absoluto",
                                                                           "no mas que lo habitual",
                                                                           "algo mas que lo habitual",
                                                                           "mucho mas que lo habitual", 
                                                                           "ns", "nc"))

levels(Total_data$Mental_disfrutar)
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "Más útil que lo habitual"] <- "mas que lo habitual"
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "Igual que lo habitual"] <- "igual que lo habitual"
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "Menos útil que lo habitual"] <- "menos que lo habitual"
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "Mucho menos que lo habitual"] <- "mucho menos que lo habitual"
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "ns"] <- "NS"
Total_data$Mental_disfrutar[Total_data$Mental_disfrutar == "nc"] <- "NC"
Total_data$Mental_disfrutar<- fct_drop(Total_data$Mental_disfrutar, only = c("Más útil que lo habitual",
                                                                               "Igual que lo habitual",
                                                                               "Menos útil que lo habitual",
                                                                               "Mucho menos que lo habitual", 
                                                                               "ns", "nc"))

levels(Total_data$Mental_frente_problemas)
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "Más útil que lo habitual"] <- "mas que lo habitual"
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "Igual que lo habitual"] <- "igual que lo habitual"
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "Menos útil que lo habitual"] <- "menos que lo habitual"
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "Mucho menos que lo habitual"] <- "mucho menos que lo habitual"
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "ns"] <- "NS"
Total_data$Mental_frente_problemas[Total_data$Mental_frente_problemas == "nc"] <- "NC"
Total_data$Mental_frente_problemas<- fct_drop(Total_data$Mental_frente_problemas, only = c("Más útil que lo habitual",
                                                                               "Igual que lo habitual",
                                                                               "Menos útil que lo habitual",
                                                                               "Mucho menos que lo habitual", 
                                                                               "ns", "nc"))

levels(Total_data$Mental_deprimido)
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "no, en absoluto"] <- "No, en absoluto"
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "ns"] <- "NS"
Total_data$Mental_deprimido[Total_data$Mental_deprimido == "nc"] <- "NC"
Total_data$Mental_deprimido<- fct_drop(Total_data$Mental_deprimido, only = c("no, en absoluto",
                                                                           "no mas que lo habitual",
                                                                           "algo mas que lo habitual",
                                                                           "mucho mas que lo habitual", 
                                                                           "ns", "nc"))

levels(Total_data$Mental_no_confianza)
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "no, en absoluto"] <- "No, en absoluto"
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "ns"] <- "NS"
Total_data$Mental_no_confianza[Total_data$Mental_no_confianza == "nc"] <- "NC"
Total_data$Mental_no_confianza<- fct_drop(Total_data$Mental_no_confianza, only = c("no, en absoluto",
                                                                           "no mas que lo habitual",
                                                                           "algo mas que lo habitual",
                                                                           "mucho mas que lo habitual", 
                                                                           "ns", "nc"))

levels(Total_data$Mental_no_valor)
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "no, en absoluto"] <- "No, en absoluto"
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "no mas que lo habitual"] <- "No más que lo habitual"
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "algo mas que lo habitual"] <- "Algo más que lo habitual"
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "mucho mas que lo habitual"] <- "Mucho más que lo habitual"
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "ns"] <- "NS"
Total_data$Mental_no_valor[Total_data$Mental_no_valor == "nc"] <- "NC"
Total_data$Mental_no_valor<- fct_drop(Total_data$Mental_no_valor, only = c("no, en absoluto",
                                                                           "no mas que lo habitual",
                                                                           "algo mas que lo habitual",
                                                                           "mucho mas que lo habitual", 
                                                                           "ns", "nc"))

levels(Total_data$Mental_feliz)
Total_data$Mental_feliz[Total_data$Mental_feliz == "Más útil que lo habitual"] <- "mas que lo habitual"
Total_data$Mental_feliz[Total_data$Mental_feliz == "Igual que lo habitual"] <- "igual que lo habitual"
Total_data$Mental_feliz[Total_data$Mental_feliz == "Menos útil que lo habitual"] <- "menos que lo habitual"
Total_data$Mental_feliz[Total_data$Mental_feliz == "Mucho menos que lo habitual"] <- "mucho menos que lo habitual"
Total_data$Mental_feliz[Total_data$Mental_feliz == "ns"] <- "NS"
Total_data$Mental_feliz[Total_data$Mental_feliz == "nc"] <- "NC"
Total_data$Mental_feliz<- fct_drop(Total_data$Mental_feliz, only = c("Más útil que lo habitual",
                                                                               "Igual que lo habitual",
                                                                               "Menos útil que lo habitual",
                                                                               "Mucho menos que lo habitual", 
                                                                               "ns", "nc"))


levels(Total_data$Social_visitas)
Total_data$Social_visitas[Total_data$Social_visitas == "ns"] <- "NS"
Total_data$Social_visitas[Total_data$Social_visitas == "nc"] <- "NC"
Total_data$Social_visitas<- fct_drop(Total_data$Social_visitas, only = c("ns","nc"))

levels(Total_data$Social_ayuda_hogar)
Total_data$Social_ayuda_hogar[Total_data$Social_ayuda_hogar == "ns"] <- "NS"
Total_data$Social_ayuda_hogar[Total_data$Social_ayuda_hogar == "nc"] <- "NC"
Total_data$Social_ayuda_hogar<- fct_drop(Total_data$Social_ayuda_hogar, only = c("ns","nc"))

levels(Total_data$Social_trabajo)
Total_data$Social_trabajo[Total_data$Social_trabajo == "ns"] <- "NS"
Total_data$Social_trabajo[Total_data$Social_trabajo == "nc"] <- "NC"
Total_data$Social_trabajo<- fct_drop(Total_data$Social_trabajo, only = c("ns","nc"))

levels(Total_data$Social_personas)
Total_data$Social_personas[Total_data$Social_personas == "ns"] <- "NS"
Total_data$Social_personas[Total_data$Social_personas == "nc"] <- "NC"
Total_data$Social_personas<- fct_drop(Total_data$Social_personas, only = c("ns","nc"))

levels(Total_data$Social_afecto)
Total_data$Social_afecto[Total_data$Social_afecto == "ns"] <- "NS"
Total_data$Social_afecto[Total_data$Social_afecto == "nc"] <- "NC"
Total_data$Social_afecto<- fct_drop(Total_data$Social_afecto, only = c("ns","nc"))

levels(Total_data$Social_hablar_trabajo)
Total_data$Social_hablar_trabajo[Total_data$Social_hablar_trabajo == "ns"] <- "NS"
Total_data$Social_hablar_trabajo[Total_data$Social_hablar_trabajo == "nc"] <- "NC"
Total_data$Social_hablar_trabajo<- fct_drop(Total_data$Social_hablar_trabajo, only = c("ns","nc"))

levels(Total_data$Social_hablar_personal)
Total_data$Social_hablar_personal[Total_data$Social_hablar_personal == "ns"] <- "NS"
Total_data$Social_hablar_personal[Total_data$Social_hablar_personal == "nc"] <- "NC"
Total_data$Social_hablar_personal<- fct_drop(Total_data$Social_hablar_personal, only = c("ns","nc"))

levels(Total_data$Social_hablar_economico)
Total_data$Social_hablar_economico[Total_data$Social_hablar_economico == "ns"] <- "NS"
Total_data$Social_hablar_economico[Total_data$Social_hablar_economico == "nc"] <- "NC"
Total_data$Social_hablar_economico<- fct_drop(Total_data$Social_hablar_economico, only = c("ns","nc"))

levels(Total_data$Social_salir_fuera)
Total_data$Social_salir_fuera[Total_data$Social_salir_fuera == "ns"] <- "NS"
Total_data$Social_salir_fuera[Total_data$Social_salir_fuera == "nc"] <- "NC"
Total_data$Social_salir_fuera<- fct_drop(Total_data$Social_salir_fuera, only = c("ns","nc"))

levels(Total_data$Social_consejos)
Total_data$Social_consejos[Total_data$Social_consejos == "ns"] <- "NS"
Total_data$Social_consejos[Total_data$Social_consejos == "nc"] <- "NC"
Total_data$Social_consejos<- fct_drop(Total_data$Social_consejos, only = c("ns","nc"))

levels(Total_data$Social_ayuda_enferm)
Total_data$Social_ayuda_enferm[Total_data$Social_ayuda_enferm == "ns"] <- "NS"
Total_data$Social_ayuda_enferm[Total_data$Social_ayuda_enferm == "nc"] <- "NC"
Total_data$Social_ayuda_enferm<- fct_drop(Total_data$Social_ayuda_enferm, only = c("ns","nc"))

Total_data <- Total_data[-c(100:105)]



#### Starting analysis - Creating new variables and defining NAs ####
Total_data$Edad_3grupos [ Total_data$Edad >= 15 & Total_data$Edad < 35 ] <- "18-34"  
Total_data$Edad_3grupos [ Total_data$Edad >= 35 & Total_data$Edad < 65 ] <- "35-64"  
Total_data$Edad_3grupos [ Total_data$Edad >= 65] <- "65+"  
Total_data$Edad_3grupos <- as.factor(Total_data$Edad_3grupos)

Total_data$Salud_percibida_2cat [ Total_data$Salud_percibida == "Muy bueno" | Total_data$Salud_percibida == "Bueno" ] <- "Bueno"  
Total_data$Salud_percibida_2cat [ Total_data$Salud_percibida == "Regular" | Total_data$Salud_percibida == "Malo" | 
                                  Total_data$Salud_percibida == "Muy malo"] <- "Malo"
Total_data$Salud_percibida_2cat <- as.factor(Total_data$Salud_percibida_2cat)

Total_data$Clase_social_3cat [ Total_data$Clase_social_ocup == "Clase Social I" | Total_data$Clase_social_ocup == "Clase Social II" ] <- "Alta"  
Total_data$Clase_social_3cat [ Total_data$Clase_social_ocup == "Clase Social III" ] <- "Media"  
Total_data$Clase_social_3cat [ Total_data$Clase_social_ocup == "Clase Social IV" | Total_data$Clase_social_ocup == "Clase Social V" |Total_data$Clase_social_ocup == "Clase Social VI"] <- "Baja"  
Total_data$Clase_social_3cat [ Total_data$Clase_social_ocup == "NS/NC" | Total_data$Clase_social_ocup == "No consta" ] <- NA  
Total_data$Clase_social_3cat <- as.factor(Total_data$Clase_social_3cat)


Total_data$Educacion_3cat [ Total_data$Educacion == "No sabe leer o escribir" |
                                Total_data$Educacion == "Educación Primaria incompleta (Ha asistido menos de 5 años a la escuela)" |
                                Total_data$Educacion == "Educación Primaria completa"
                                ] <- "Primarios o sin estudios"  
Total_data$Educacion_3cat [ Total_data$Educacion == "Primera etapa de Enseñanza Secundaria, con o sin título (2º ESO aprobado, EGB, Bachillerato Elemental)" | 
                                Total_data$Educacion == "Estudios de Bachillerato" |
                                Total_data$Educacion == "Enseñanzas profesionales de grado medio o equivalentes" | 
                                Total_data$Educacion == "Enseñanzas profesionales de grado superior o equivalentes"] <- "Secundarios"  
Total_data$Educacion_3cat [ Total_data$Educacion == "Estudios universitarios o equivalentes"] <- "Universitarios"  
Total_data$Educacion_3cat [ Total_data$Educacion == "No sabe"] <- NA  
Total_data$Educacion_3cat [ Total_data$Educacion == "No contesta"] <- NA  
Total_data$Educacion_3cat [ Total_data$Educacion == "No procede, es menor de 10 años"] <- NA  
Total_data$Educacion_3cat <- as.factor(Total_data$Educacion_3cat)


Total_data$Lugar_residencia_3cat [ Total_data$Lugar_residencia == "Municipios de más de 500.000 habitantes" ] <- "Metropolitana"  
Total_data$Lugar_residencia_3cat [ Total_data$Lugar_residencia == "Municipio capital de provincia (excepto los anteriores)" | 
                                   Total_data$Lugar_residencia == "Municipios con más de 100.000 habitantes (excepto los anteriores)" |
                                   Total_data$Lugar_residencia == "Municipios de 50.000 a 100.000 habitantes (excepto los anteriores)" | 
                                       Total_data$Lugar_residencia == "Municipios de 20.000 a 50.000 habitantes (excepto los anteriores)" | 
                                       Total_data$Lugar_residencia == "Municipios de 10.000 a 20.000 habitantes" ] <- "Suburbe"  
Total_data$Lugar_residencia_3cat [ Total_data$Lugar_residencia == "Municipios con menos de 10.000 habitantes"] <- "Rural"  
Total_data$Lugar_residencia_3cat <- as.factor(Total_data$Lugar_residencia_3cat)






Total_data$IMC_grupo[Total_data$IMC_grupo == "NS/NC"] <-NA
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "NS"] <-NA
Total_data$Enfermedad_cronica[Total_data$Enfermedad_cronica == "NC"] <-NA
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "NS"] <-NA
Total_data$Grado_limitacion[Total_data$Grado_limitacion == "NC"] <-NA
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "NS"] <-NA
Total_data$Tipo_problema_enf[Total_data$Tipo_problema_enf == "NC"] <-NA

Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "NS"] <-NA
Total_data$Frec_AF_libre[Total_data$Frec_AF_libre == "NC"] <-NA
Total_data$Frec_verdura[Total_data$Frec_verdura == "NS"] <-NA
Total_data$Frec_verdura[Total_data$Frec_verdura == "NC"] <-NA
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "NS"] <-NA
Total_data$Frec_aperitivos[Total_data$Frec_aperitivos == "NC"] <-NA
Total_data$Frec_dulces[Total_data$Frec_dulces == "NS"] <-NA
Total_data$Frec_dulces[Total_data$Frec_dulces == "NC"] <-NA
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "NS"] <-NA
Total_data$Frec_comida_rapida[Total_data$Frec_comida_rapida == "NC"] <-NA
Total_data$Frec_fruta[Total_data$Frec_fruta == "NS"] <-NA
Total_data$Frec_fruta[Total_data$Frec_fruta == "NC"] <-NA
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "NS"] <-NA
Total_data$Frec_refrescos[Total_data$Frec_refrescos == "NC"] <-NA
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "NS"] <-NA
Total_data$Frec_alcohol[Total_data$Frec_alcohol == "NC"] <-NA

Total_data$Tabaco[Total_data$Tabaco == "NS"] <-NA
Total_data$Tabaco[Total_data$Tabaco == "NC"] <-NA

Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "NS"] <- NA
Total_data$Consumo_alcohol_abusivo_dia[Total_data$Consumo_alcohol_abusivo_dia == "NC"] <- NA


Total_data$Consumo_cerveza_sem <- Total_data$Consumo_cerveza_lunes_cont + 
    Total_data$Consumo_cerveza_martes_cont + Total_data$Consumo_cerveza_mier_cont + Total_data$Consumo_cerveza_jueves_cont + 
    Total_data$Consumo_cerveza_vier_cont + Total_data$Consumo_cerveza_sab_cont + Total_data$Consumo_cerveza_dom_cont 

Total_data$Consumo_vino_sem <- Total_data$Consumo_vino_lunes_cont + 
    Total_data$Consumo_vino_martes_cont + Total_data$Consumo_vino_mier_cont + Total_data$Consumo_vino_jueves_cont + 
    Total_data$Consumo_vino_vier_cont + Total_data$Consumo_vino_sab_cont + Total_data$Consumo_vino_dom_cont 

Total_data$Consumo_vermut_sem <- Total_data$Consumo_vermut_lunes_cont + 
    Total_data$Consumo_vermut_martes_cont + Total_data$Consumo_vermut_mier_cont + Total_data$Consumo_vermut_jueves_cont + 
    Total_data$Consumo_vermut_vier_cont + Total_data$Consumo_vermut_sab_cont + Total_data$Consumo_vermut_dom_cont 

Total_data$Consumo_licores_sem <- Total_data$Consumo_licores_lunes_cont + 
    Total_data$Consumo_licores_martes_cont + Total_data$Consumo_licores_mier_cont + Total_data$Consumo_licores_jueves_cont + 
    Total_data$Consumo_licores_vier_cont + Total_data$Consumo_licores_sab_cont + Total_data$Consumo_licores_dom_cont 

Total_data$Consumo_destilada_sem <- Total_data$Consumo_destilada_lunes_cont + 
    Total_data$Consumo_destilada_martes_cont + Total_data$Consumo_destilada_mier_cont + Total_data$Consumo_destilada_jueves_cont + 
    Total_data$Consumo_destilada_vier_cont + Total_data$Consumo_destilada_sab_cont + Total_data$Consumo_destilada_dom_cont 

Total_data$Consumo_locales_sem <- Total_data$Consumo_locales_lunes_cont + 
    Total_data$Consumo_locales_martes_cont + Total_data$Consumo_locales_mier_cont + Total_data$Consumo_locales_jueves_cont + 
    Total_data$Consumo_locales_vier_cont + Total_data$Consumo_locales_sab_cont + Total_data$Consumo_locales_dom_cont 



Total_data$Consumo_total_sem_unidades <- Total_data$Consumo_cerveza_sem + 
    Total_data$Consumo_vino_sem + Total_data$Consumo_vermut_sem + Total_data$Consumo_licores_sem + 
    Total_data$Consumo_destilada_sem + Total_data$Consumo_locales_sem

Total_data$Consumo_total_dia_unidades <- Total_data$Consumo_total_sem_unidades/7


Total_data$Consumo_total_sem_gramos_alc <- Total_data$Consumo_cerveza_sem*10 + 
    Total_data$Consumo_vino_sem*10 + Total_data$Consumo_vermut_sem*20 + Total_data$Consumo_licores_sem*20 + 
    Total_data$Consumo_destilada_sem*20 + Total_data$Consumo_locales_sem*10 

Total_data$Consumo_total_dia_gramos_alc <- Total_data$Consumo_total_sem_gramos_alc/7


Total_data$Consumo_perc_cerveza <- ((Total_data$Consumo_cerveza_sem*10)/Total_data$Consumo_total_sem_gramos_alc)*100
Total_data$Consumo_perc_vino <- ((Total_data$Consumo_vino_sem*10)/Total_data$Consumo_total_sem_gramos_alc)*100
Total_data$Consumo_perc_vermut <- ((Total_data$Consumo_vermut_sem*20)/Total_data$Consumo_total_sem_gramos_alc)*100
Total_data$Consumo_perc_licores <- ((Total_data$Consumo_licores_sem*20)/Total_data$Consumo_total_sem_gramos_alc)*100
Total_data$Consumo_perc_destilada <- ((Total_data$Consumo_destilada_sem*20)/Total_data$Consumo_total_sem_gramos_alc)*100
Total_data$Consumo_perc_locales <- ((Total_data$Consumo_locales_sem*10)/Total_data$Consumo_total_sem_gramos_alc)*100


Total_data$Alcohol_preferido [ Total_data$Consumo_perc_cerveza > 50 ] <- "Cerveza"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_vino > 50 ] <- "Vino"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_vermut > 50 ] <- "Vermut"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_licores > 50 ] <- "Licores"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_destilada > 50 ] <- "Destilada"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_locales > 50 ] <- "Locales"  
Total_data$Alcohol_preferido [ Total_data$Consumo_perc_cerveza <= 50 & 
                                   Total_data$Consumo_perc_vino <= 50 & 
                                   Total_data$Consumo_perc_vermut <= 50 & 
                                   Total_data$Consumo_perc_licores <= 50 & 
                                   Total_data$Consumo_perc_destilada <= 50 & 
                                   Total_data$Consumo_perc_locales <= 50 ] <- "Mixto"  
Total_data$Alcohol_preferido <- as.factor(Total_data$Alcohol_preferido)



Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_cerveza > 50 ] <- "Cerveza"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_vino > 50 ] <- "Vino"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_vermut > 50 ] <- "Vermut"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_licores > 50 ] <- "Licores"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_destilada > 50 ] <- "Destilada"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_locales > 50 ] <- "Locales"  
Total_data$Alcohol_preferido2 [ Total_data$Consumo_perc_cerveza <= 50 & 
                                   Total_data$Consumo_perc_vino <= 50 & 
                                   Total_data$Consumo_perc_vermut <= 50 & 
                                   Total_data$Consumo_perc_licores <= 50 & 
                                   Total_data$Consumo_perc_destilada <= 50 & 
                                   Total_data$Consumo_perc_locales <= 50 ] <- "Mixto"  
Total_data$Alcohol_preferido2 [ Total_data$Frec_alcohol == "Una vez al mes o menos" | 
                              Total_data$Frec_alcohol == "2-3 días en un mes"] <- "Ocasional"
Total_data$Alcohol_preferido2 [ Total_data$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol"] <- "Exbebedor"
Total_data$Alcohol_preferido2 [ Total_data$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"] <- "Abstemio"
Total_data$Alcohol_preferido2 <- as.factor(Total_data$Alcohol_preferido2)



Total_data$Alcohol_intensidad [ Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Moderado"
Total_data$Alcohol_intensidad [ Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Moderado"
Total_data$Alcohol_intensidad [ Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Excesivo"
Total_data$Alcohol_intensidad [ Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Excesivo"
Total_data$Alcohol_intensidad [ Total_data$Frec_alcohol == "Una vez al mes o menos" | 
                              Total_data$Frec_alcohol == "2-3 días en un mes"] <- "Ocasional"
Total_data$Alcohol_intensidad [ Total_data$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol"] <- "Exbebedor"
Total_data$Alcohol_intensidad [ Total_data$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"] <- "Abstemio"
Total_data$Alcohol_intensidad <- as.factor(Total_data$Alcohol_intensidad)




Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Cerveza" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Cerveza moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Cerveza" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Cerveza moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vino" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Vino moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vino" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Vino moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vermut" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Vermut moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vermut" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Vermut moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Licores" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Licores moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Licores" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Licores moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Destilada" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Destilada moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Destilada" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Destilada moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Locales" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Locales moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Locales" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Locales moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Mixto" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc <= 12 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Mixto moderado"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Mixto" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc <= 24 & Total_data$Consumo_total_dia_gramos_alc > 0] <- "Mixto moderado"


Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Cerveza" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Cerveza excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Cerveza" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Cerveza excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vino" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Vino excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vino" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Vino excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vermut" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Vermut excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Vermut" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Vermut excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Licores" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Licores excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Licores" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Licores excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Destilada" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Destilada excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Destilada" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Destilada excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Locales" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Locales excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Locales" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Locales excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Mixto" & Total_data$Genero == "Mujer" &
                              Total_data$Consumo_total_dia_gramos_alc > 12] <- "Mixto excesivo"
Total_data$Alcohol_5cat [ Total_data$Alcohol_preferido == "Mixto" & Total_data$Genero == "Hombre" &
                              Total_data$Consumo_total_dia_gramos_alc > 24] <- "Mixto excesivo"

Total_data$Alcohol_5cat [ Total_data$Frec_alcohol == "Una vez al mes o menos" | 
                              Total_data$Frec_alcohol == "2-3 días en un mes"] <- "Ocasional"
Total_data$Alcohol_5cat [ Total_data$Frec_alcohol == "No en los últimos 12 meses, he dejado de tomar alcohol"] <- "Exbebedor"
Total_data$Alcohol_5cat [ Total_data$Frec_alcohol == "Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"] <- "Abstemio"

Total_data$Alcohol_5cat <- as.factor(Total_data$Alcohol_5cat)




Total_data$Alcohol_beer <- Total_data$Alcohol_5cat
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Destilada excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Destilada moderado"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Licores excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Licores moderado"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Locales excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Locales moderado"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Mixto excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Mixto moderado"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Vermut excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Vermut moderado"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Vino excesivo"] <-NA
Total_data$Alcohol_beer[Total_data$Alcohol_beer == "Vino moderado"] <-NA

Total_data$Alcohol_beer<- fct_drop(Total_data$Alcohol_beer, only = c("Destilada excesivo","Destilada moderado","Licores excesivo",
                                                                     "Licores moderado","Locales excesivo","Locales moderado",
                                                                     "Mixto excesivo", "Mixto moderado","Vermut excesivo",
                                                                     "Vermut moderado","Vino excesivo","Vino moderado"))



Total_data$Salud_mental_general_3cat [ Total_data$Salud_mental_general_cont >= 0 & Total_data$Salud_mental_general_cont < 4 ] <- "Salud_mental_mejor"  
Total_data$Salud_mental_general_3cat [ Total_data$Salud_mental_general_cont >= 4 & Total_data$Salud_mental_general_cont < 9 ] <- "Salud_mental_media"  
Total_data$Salud_mental_general_3cat [ Total_data$Salud_mental_general_cont >= 9] <- "Salud_mental_peor"  
Total_data$Salud_mental_general_3cat <- as.factor(Total_data$Salud_mental_general_3cat)

Total_data$Apoyo_social_general_3cat [ Total_data$Apoyo_social_general_cont >= 11 & Total_data$Apoyo_social_general_cont < 26 ] <- "Apoyo_social_peor"  
Total_data$Apoyo_social_general_3cat [ Total_data$Apoyo_social_general_cont >= 26 & Total_data$Apoyo_social_general_cont < 41 ] <- "Apoyo_social_medio"  
Total_data$Apoyo_social_general_3cat [ Total_data$Apoyo_social_general_cont >= 41] <- "Apoyo_social_mejor"  
Total_data$Apoyo_social_general_3cat <- as.factor(Total_data$Apoyo_social_general_3cat)


#### Filters - removing non-selected cases ####
Total_data <- Total_data[which(Total_data$Edad >= 18), ] # aged 18 and over


#### Descriptive statistics ####
# VD
table(Total_data$Salud_percibida_2cat);prop.table(table(Total_data$Salud_percibida_2cat))
table(Total_data$Grado_limitacion);prop.table(table(Total_data$Grado_limitacion))
table(Total_data$Tipo_problema_enf);prop.table(table(Total_data$Tipo_problema_enf))

table(Total_data$Salud_mental_general_3cat);prop.table(table(Total_data$Salud_mental_general_3cat))
table(Total_data$Apoyo_social_general_3cat);prop.table(table(Total_data$Apoyo_social_general_3cat))



# VI
table(Total_data$Alcohol_5cat);prop.table(table(Total_data$Alcohol_5cat))
table(Total_data$Consumo_alcohol_abusivo_dia);prop.table(table(Total_data$Consumo_alcohol_abusivo_dia))
table(Total_data$Alcohol_preferido2);prop.table(table(Total_data$Alcohol_preferido2))
table(Total_data$Alcohol_intensidad);prop.table(table(Total_data$Alcohol_intensidad))
table(Total_data$Alcohol_beer);prop.table(table(Total_data$Alcohol_beer))


# COVARIABLES
summary(Total_data$Edad); sd(Total_data$Edad)
table(Total_data$Edad_3grupos);prop.table(table(Total_data$Edad_3grupos))
table(Total_data$Genero);prop.table(table(Total_data$Genero))
table(Total_data$Clase_social_3cat);prop.table(table(Total_data$Clase_social_3cat))
table(Total_data$Educacion_3cat);prop.table(table(Total_data$Educacion_3cat))

table(Total_data$Año);prop.table(table(Total_data$Año))
table(Total_data$CCAA);prop.table(table(Total_data$CCAA))
table(Total_data$Lugar_residencia_3cat);prop.table(table(Total_data$Lugar_residencia_3cat))

table(Total_data$Frec_AF_libre);prop.table(table(Total_data$Frec_AF_libre))
table(Total_data$Frec_fruta);prop.table(table(Total_data$Frec_fruta))
table(Total_data$Frec_verdura);prop.table(table(Total_data$Frec_verdura))
table(Total_data$Frec_dulces);prop.table(table(Total_data$Frec_dulces))
table(Total_data$Frec_refrescos);prop.table(table(Total_data$Frec_refrescos))
table(Total_data$Frec_comida_rapida);prop.table(table(Total_data$Frec_comida_rapida))
table(Total_data$Frec_aperitivos);prop.table(table(Total_data$Frec_aperitivos))
table(Total_data$IMC_grupo);prop.table(table(Total_data$IMC_grupo))

table(Total_data$Tabaco);prop.table(table(Total_data$Tabaco))



# CROSS-TABLES BY ALCOHOL CONSUMPTION
library(descr)

crosstab(Total_data$Alcohol_beer, Total_data$Genero, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Edad_3grupos, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Clase_social_3cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Educacion_3cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Año, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Lugar_residencia_3cat, prop.r = T,digits = 2, chisq = T, plot = F)

crosstab(Total_data$Alcohol_beer, Total_data$Frec_AF_libre, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_fruta, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_verdura, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_dulces, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_refrescos, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_comida_rapida, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Frec_aperitivos, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$IMC_grupo, prop.r = T,digits = 2, chisq = T, plot = F)

crosstab(Total_data$Alcohol_beer, Total_data$Tabaco, prop.r = T,digits = 2, chisq = T, plot = F)


crosstab(Total_data$Alcohol_beer, Total_data$Consumo_alcohol_abusivo_dia, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Salud_percibida_2cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Grado_limitacion, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Tipo_problema_enf, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Salud_mental_general_3cat, prop.r = T,digits = 2, chisq = T, plot = F)
crosstab(Total_data$Alcohol_beer, Total_data$Apoyo_social_general_3cat, prop.r = T,digits = 2, chisq = T, plot = F)



Total_data %>%  group_by(Alcohol_beer) %>% 
    summarise(n = n(), 
              mean_age = mean(Edad, na.rm = T),
              sd_age = sd(Edad, na.rm = T))

summary(aov(Edad ~ Alcohol_beer, data = Total_data))
pairwise.t.test(Total_data$Edad, Total_data$Alcohol_beer, p.adjust.method = "BH")



# Supplementary cross-table with and without Alcohol missing values
Total_data$Alcohol_data [is.na(Total_data$Consumo_total_dia_gramos_alc)] <-"Missing alcohol data"
Total_data$Alcohol_data [Total_data$Consumo_total_dia_gramos_alc >= 0 ] <- "Complete alcohol data"
Total_data$Alcohol_data <- as.factor(Total_data$Alcohol_data)
table(Total_data$Alcohol_data);prop.table(table(Total_data$Alcohol_data))



#### Alpha Cronbach - Mental health and social support ####
library(ltm)

cronbach.alpha(Total_data[c("Mental_concentrarse", "Mental_preocupaciones", "Mental_util", 
                          "Mental_decisiones", "Mental_agobiado", "Mental_no_superacion", 
                          "Mental_disfrutar", "Mental_frente_problemas", "Mental_deprimido", 
                          "Mental_no_confianza", "Mental_no_valor", "Mental_feliz")])

cronbach.alpha(Total_data[c("Social_visitas", "Social_ayuda_hogar", "Social_trabajo", 
                            "Social_personas", "Social_afecto", "Social_hablar_trabajo", 
                            "Social_hablar_personal", "Social_hablar_economico", "Social_salir_fuera", 
                            "Social_consejos", "Social_ayuda_enferm")])


#### Logistic regressions ####
library("moments")
library("sjPlot")
set.seed(123456)

Total_data$Edad_2grupos [ Total_data$Edad >= 15 & Total_data$Edad < 40 ] <- "18-39"  
Total_data$Edad_2grupos [ Total_data$Edad >= 40] <- "40+"  
Total_data$Edad_2grupos <- as.factor(Total_data$Edad_2grupos)

Total_data$Genero <- relevel(Total_data$Genero, ref = "Mujer")
Total_data$Edad_3grupos <- relevel(Total_data$Edad_3grupos, ref = "18-34")
Total_data$Edad_2grupos <- relevel(Total_data$Edad_2grupos, ref = "18-39")
Total_data$Clase_social_3cat <- relevel(Total_data$Clase_social_3cat, ref = "Baja")
Total_data$Educacion_3cat <- relevel(Total_data$Educacion_3cat, ref = "Primarios o sin estudios")
Total_data$Año <- relevel(Total_data$Año, ref = "2012")
Total_data$Lugar_residencia_3cat <- relevel(Total_data$Lugar_residencia_3cat, ref = "Rural")
Total_data$Frec_AF_libre <- relevel(Total_data$Frec_AF_libre, ref = "No hago ejercicio. El tiempo libre lo ocupo de forma casi completamente sedentaria")
Total_data$Frec_fruta <- relevel(Total_data$Frec_fruta, ref = "Nunca o casi nunca")
Total_data$Frec_verdura <- relevel(Total_data$Frec_verdura, ref = "Nunca o casi nunca")
Total_data$Frec_dulces <- relevel(Total_data$Frec_dulces, ref = "Nunca o casi nunca")
Total_data$Frec_refrescos <- relevel(Total_data$Frec_refrescos, ref = "Nunca o casi nunca")
Total_data$Frec_comida_rapida <- relevel(Total_data$Frec_comida_rapida, ref = "Nunca o casi nunca")
Total_data$Frec_aperitivos <- relevel(Total_data$Frec_aperitivos, ref = "Nunca o casi nunca")
Total_data$IMC_grupo <- relevel(Total_data$IMC_grupo, ref = "Normopeso")
Total_data$Tabaco <- relevel(Total_data$Tabaco, ref = "Nunca")


Total_data$Alcohol_beer <- relevel(Total_data$Alcohol_beer, ref = "Abstemio")

Total_data$Salud_percibida_2cat <- relevel(Total_data$Salud_percibida_2cat, ref = "Malo")
Total_data$Grado_limitacion <- relevel(Total_data$Grado_limitacion, ref = "Nada limitado")
Total_data$Tipo_problema_enf <- relevel(Total_data$Tipo_problema_enf, ref = "ninguno")



## Salud percibida 

model1a <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                  Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                   data = Total_data, family = binomial, na.action = na.exclude)


model1b <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                  Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
              data=subset(Total_data, Enfermedad_cronica =="Sí"), family = binomial, na.action = na.exclude)

model1c <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                  Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
              data=subset(Total_data, Enfermedad_cronica =="No"), family = binomial, na.action = na.exclude)

Total_data_non_abusive_alcohol <- Total_data[which(Total_data$Consumo_alcohol_abusivo_dia == "Nunca en toda mi vida" | Total_data$Consumo_alcohol_abusivo_dia == "Ex_bebedor" | Total_data$Consumo_alcohol_abusivo_dia == "No en los últimos 12 meses"), ]

model1d <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                  Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
              data= Total_data_non_abusive_alcohol, family = binomial, na.action = na.exclude)


tab_model(model1a, model1b, model1c, model1d, digits.re = 3)




model1e <- glm(Salud_percibida_2cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Genero =="Mujer"), family = binomial, na.action = na.exclude)

model1f <- glm(Salud_percibida_2cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Genero =="Hombre"), family = binomial, na.action = na.exclude)


model1g <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Edad_2grupos =="18-39"), family = binomial, na.action = na.exclude)

model1h <- glm(Salud_percibida_2cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Edad_2grupos =="40+"), family = binomial, na.action = na.exclude)

tab_model(model1e, model1f, model1g, model1h, digits.re = 3)




## Grado limitacion
library("nnet")

Total_data$Grado_limitacion<- fct_drop(Total_data$Grado_limitacion, only = c("NS","NC"))
Total_data_non_abusive_alcohol$Grado_limitacion<- fct_drop(Total_data_non_abusive_alcohol$Grado_limitacion, only = c("NS","NC"))


model2a <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data = Total_data, family = binomial, na.action = na.exclude)
tab_model(model2a, digits.re = 3)


model2b <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Enfermedad_cronica =="Sí"), family = binomial, na.action = na.exclude)
tab_model(model2b, digits.re = 3)

model2c <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Enfermedad_cronica =="No"), family = binomial, na.action = na.exclude)
tab_model(model2c, digits.re = 3)


model2d <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data= Total_data_non_abusive_alcohol, family = binomial, na.action = na.exclude)
tab_model(model2d, digits.re = 3)




model2e <- multinom(Grado_limitacion ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Genero =="Mujer"), family = binomial, na.action = na.exclude)
tab_model(model2e, digits.re = 3)

model2f <- multinom(Grado_limitacion ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Genero =="Hombre"), family = binomial, na.action = na.exclude)
tab_model(model2f, digits.re = 3)


model2g <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Edad_2grupos =="18-39"), family = binomial, na.action = na.exclude)
tab_model(model2g, digits.re = 3)

model2h <- multinom(Grado_limitacion ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                   Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
               data=subset(Total_data, Edad_2grupos =="40+"), family = binomial, na.action = na.exclude)
tab_model(model2h, digits.re = 3)






## Tipo de enfermedad
Total_data$Tipo_problema_enf<- fct_drop(Total_data$Tipo_problema_enf, only = c("NS","NC"))
Total_data_non_abusive_alcohol$Tipo_problema_enf<- fct_drop(Total_data_non_abusive_alcohol$Tipo_problema_enf, only = c("NS","NC"))


model3a <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data = Total_data, family = binomial, na.action = na.exclude)
tab_model(model3a, digits.re = 3)


model3b <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="Sí"), family = binomial, na.action = na.exclude)
tab_model(model3b, digits.re = 3)

model3c <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="No"), family = binomial, na.action = na.exclude)
tab_model(model3c, digits.re = 3)


model3d <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data= Total_data_non_abusive_alcohol, family = binomial, na.action = na.exclude)
tab_model(model3d, digits.re = 3)




model3e <- multinom(Tipo_problema_enf ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Mujer"), family = binomial, na.action = na.exclude)
tab_model(model3e, digits.re = 3)

model3f <- multinom(Tipo_problema_enf ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Hombre"), family = binomial, na.action = na.exclude)
tab_model(model3f, digits.re = 3)


model3g <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="18-39"), family = binomial, na.action = na.exclude)
tab_model(model3g, digits.re = 3)

model3h <- multinom(Tipo_problema_enf ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="40+"), family = binomial, na.action = na.exclude)
tab_model(model3h, digits.re = 3)





## Salud mental 
Total_data_non_abusive_alcohol$Salud_mental_general_3cat [ Total_data_non_abusive_alcohol$Salud_mental_general_cont >= 0 & Total_data_non_abusive_alcohol$Salud_mental_general_cont < 4 ] <- "Salud_mental_mejor"  
Total_data_non_abusive_alcohol$Salud_mental_general_3cat [ Total_data_non_abusive_alcohol$Salud_mental_general_cont >= 4 & Total_data_non_abusive_alcohol$Salud_mental_general_cont < 9 ] <- "Salud_mental_media"  
Total_data_non_abusive_alcohol$Salud_mental_general_3cat [ Total_data_non_abusive_alcohol$Salud_mental_general_cont >= 9] <- "Salud_mental_peor"  
Total_data_non_abusive_alcohol$Salud_mental_general_3cat <- as.factor(Total_data_non_abusive_alcohol$Salud_mental_general_3cat)

Total_data$Salud_mental_general_3cat <- relevel(Total_data$Salud_mental_general_3cat, ref = "Salud_mental_peor")
Total_data_non_abusive_alcohol$Salud_mental_general_3cat <- relevel(Total_data_non_abusive_alcohol$Salud_mental_general_3cat, ref = "Salud_mental_peor")


model4a <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data = Total_data, family = binomial, na.action = na.exclude)
tab_model(model4a, digits.re = 3)


model4b <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="Sí"), family = binomial, na.action = na.exclude)
tab_model(model4b, digits.re = 3)

model4c <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="No"), family = binomial, na.action = na.exclude)
tab_model(model4c, digits.re = 3)


model4d <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data= Total_data_non_abusive_alcohol, family = binomial, na.action = na.exclude)
tab_model(model4d, digits.re = 3)





model4e <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Mujer"), family = binomial, na.action = na.exclude)
tab_model(model4e, digits.re = 3)

model4f <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Hombre"), family = binomial, na.action = na.exclude)
tab_model(model4f, digits.re = 3)


model4g <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="18-39"), family = binomial, na.action = na.exclude)
tab_model(model4g, digits.re = 3)

model4h <- multinom(Salud_mental_general_3cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="40+"), family = binomial, na.action = na.exclude)
tab_model(model4h, digits.re = 3)




## Apoyo social
Total_data_non_abusive_alcohol$Apoyo_social_general_3cat [ Total_data_non_abusive_alcohol$Apoyo_social_general_cont >= 11 & Total_data_non_abusive_alcohol$Apoyo_social_general_cont < 26 ] <- "Apoyo_social_peor"  
Total_data_non_abusive_alcohol$Apoyo_social_general_3cat [ Total_data_non_abusive_alcohol$Apoyo_social_general_cont >= 26 & Total_data_non_abusive_alcohol$Apoyo_social_general_cont < 41 ] <- "Apoyo_social_medio"  
Total_data_non_abusive_alcohol$Apoyo_social_general_3cat [ Total_data_non_abusive_alcohol$Apoyo_social_general_cont >= 41] <- "Apoyo_social_mejor"  
Total_data_non_abusive_alcohol$Apoyo_social_general_3cat <- as.factor(Total_data_non_abusive_alcohol$Apoyo_social_general_3cat)

Total_data$Apoyo_social_general_3cat <- relevel(Total_data$Apoyo_social_general_3cat, ref = "Apoyo_social_peor")
Total_data_non_abusive_alcohol$Apoyo_social_general_3cat <- relevel(Total_data_non_abusive_alcohol$Apoyo_social_general_3cat, ref = "Apoyo_social_peor")


model5a <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data = Total_data, family = binomial, na.action = na.exclude)
tab_model(model5a, digits.re = 3)


model5b <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="Sí"), family = binomial, na.action = na.exclude)
tab_model(model5b, digits.re = 3)

model5c <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Enfermedad_cronica =="No"), family = binomial, na.action = na.exclude)
tab_model(model5c, digits.re = 3)


model5d <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data= Total_data_non_abusive_alcohol, family = binomial, na.action = na.exclude)
tab_model(model5d, digits.re = 3)






model5e <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Mujer"), family = binomial, na.action = na.exclude)
tab_model(model5e, digits.re = 3)

model5f <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Edad_3grupos + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Genero =="Hombre"), family = binomial, na.action = na.exclude)
tab_model(model5f, digits.re = 3)


model5g <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="18-39"), family = binomial, na.action = na.exclude)
tab_model(model5g, digits.re = 3)

model5h <- multinom(Apoyo_social_general_3cat ~ Alcohol_beer + Genero + Clase_social_3cat + Educacion_3cat + Año + Lugar_residencia_3cat + 
                        Frec_AF_libre + Frec_fruta + Frec_verdura + Frec_dulces + Frec_refrescos + Frec_comida_rapida + Frec_aperitivos + Tabaco + IMC_grupo, 
                    data=subset(Total_data, Edad_2grupos =="40+"), family = binomial, na.action = na.exclude)
tab_model(model5h, digits.re = 3)


