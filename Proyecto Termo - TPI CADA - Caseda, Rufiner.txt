; Trabajo Practico Integrador (Proyecto Termo)

; Integrantes: Caseda, Leonardo - Rufiner, Santiago
; Video youtube: https://youtu.be/ZU7bFcycndg

(defun c:termo()

  (datos_termo)
  (configuracion_termo)
  (medidas_termo)
  (dibujar_cuerpo_termo)
  (dibujar_tapa_termo)
  (dibujar_manija_termo)
  (restauracion)
  
)

(defun datos_termo()

    (setq seleccion1 nil)
    (setq seleccion2 nil)
    (setq seleccion3 nil)

    ; Le pedimos al usuario el tamaño del termo
    (initget 1 "1LMedio 1Litro MedioLitro")
    (setq seleccion1 (getkword "\nTamaños disponibles: [1LMedio/1Litro/MedioLitro] "))
    (cond
      ((= seleccion1 "1LMedio")
       (setq tamaño 1500000))
      ((= seleccion1 "1Litro")
       (setq tamaño 1000000))
      ((= seleccion1 "MedioLitro")
       (setq tamaño 500000))
      (t
       (setq tamaño "Opción inválida. Intenta nuevamente."))
    )
    (initget 1 "Negro Blanco Verde Rojo Azul Gris")
    (setq seleccion2 (getkword "\nColor disponibles: [Negro/Blanco/Verde/Rojo/Azul/Gris] "))
    (cond
      ((= seleccion2 "Negro")
       (setq color 250))
      ((= seleccion2 "Blanco")
       (setq color 255))
      ((= seleccion2 "Verde")
       (setq color 75))
      ((= seleccion2 "Rojo")
       (setq color 10))
      ((= seleccion2 "Azul")
       (setq color 170))
      ((= seleccion2 "Gris")
       (setq color 253))
      (t
       (setq color 75))
    )
    (initget 1 "Rosca Pico")
    (setq seleccion3 (getkword "\nTapas disponibles: [Rosca/Pico] "))
    (cond
      ((= seleccion3 "Rosca")
       (setq tapa 0))
      ((= seleccion3 "Pico")
       (setq tapa 1))
      (t
       (setq tapa 0))
    )

    (setq punto_base(getpoint "Ingrese el punto de insersión: "))
  
)


(defun configuracion_termo()
  
  (setq var (getvar "osmode"))
  (setvar "osmode" 0)
  (command "UCS" "W" "")
  (setvar "isolines" 20)
  (setvar "SURFTAB1" 20)
  (setvar "SURFTAB2" 20)
  (command "-layer" "N" "Cuerpo" "" "")
  (command "-layer" "S" "Cuerpo" "" "")
  (command "-layer" "C" color "" "")
  (command "-layer" "N" "Tapa" "" "")
  (command "-layer" "S" "Tapa" "" "")
  (command "-layer" "C" color "" "")
  (command "-layer" "N" "aux" "" "")
  (command "-layer" "S" "aux" "" "")
  (command "-layer" "C" color "" "")
  (command "-layer" "N" "Manija" "" "")
  (command "-layer" "S" "Manija" "" "")
  (command "-layer" "C" color "" "")
  
)


(defun medidas_termo()

  ; Medidas cuerpo
  (setq radio_ext 45)
  (setq radio_int 40)
  (setq altura(/ tamaño (* pi radio_int radio_int)))
  (setq altura_base 20)
  (setq altura_cuerpo(+ altura altura_base))
  (setq r_centro_toroide 33)
  (setq r_toroide 12)
  (setq altura_cuello 45)
  (setq r_cuello 20)
  (setq r_tapa 20)
  (setq angulo(atan (/ altura_cuello 7)))
  (setq angulo(/(* angulo 180)pi))
  (setq angulo(- 90 angulo))

  ; Medidas tapa
  (setq altura_tapa 36)
  (setq altura_tapon 13)
  (setq r_tapon 28)
  (setq altura_helice 20)
  (setq r_ext_helice 1)
  (setq altura_cuña 9)
  (setq grosor_linea_tapon 0.1)
  (setq altura_cilindros 6)
  (setq r_cilindros 2)
  (setq altura_cono 1.5)
  (setq r_interior_pico 2.5)
  (setq r_exterior_pico 4.5)
  (setq d_afuera_pico 5)

  ; Medidas manija
  (setq altura_union1(/(* 40 altura_cuerpo)200))
  (setq altura_union2(/(* 140 altura)200))
  (setq largo_manija(/(* 30 altura) 200))
  (setq r_manija(/(* 5 altura) 200))
  ;(setq r_manija 5)
  
)
	 
  
(defun dibujar_cuerpo_termo()
  
  (command "-layer" "S" "Cuerpo" "" "")
  (command "CYLINDER" punto_base (- radio_ext 0.1) altura_base "")
  (setq cilindro_base(entlast))
  (command "CHPROP" cilindro_base "" "C" 255 "" "") ;; Cambio Color base
  
  (setq pto1(list(nth 0 punto_base)(nth 1 punto_base)(+(nth 2 punto_base) altura_base)))
  (command "CYLINDER" pto1 radio_ext altura "")
  (setq cilindro_externo(entlast))
  (command "CYLINDER" pto1 radio_int altura "")
  (setq cilindro_interno(entlast))
  (command "SUBTRACT" cilindro_externo "" cilindro_interno "")
  (setq cuerpo(entlast))
  (setq pto1(list(nth 0 pto1)(nth 1 pto1)(+(nth 2 pto1) altura)))
  (command "TORUS" pto1 r_centro_toroide r_toroide "")
  (setq toroide(entlast))
  (command "UNION" cuerpo toroide "")
  (setq cuerpo(entlast))
  (command "CHPROP" cuerpo "" "C" color "" "") ;; Cambio Color cuerpo
  
  (setq pto1(list(nth 0 pto1)(nth 1 pto1)(+(nth 2 pto1) r_toroide)))
  (command "CIRCLE" pto1 r_centro_toroide "")
  (setq circulo(entlast))
  (command "EXTRUDE" circulo "" "T" angulo altura_cuello "")
  (setq cuello(entlast))
  (command "CYLINDER" pto1 r_cuello altura_cuello "")
  (setq cilindro(entlast))
  (command "SUBTRACT" cuello "" cilindro "")
  (setq cuello(entlast))
  (command "CHPROP" cuello "" "C" 255 "" "") ;; Cambio Color cuello
  
  (command "UNION" cuerpo cuello cilindro_base "")
  (setq cuerpo(entlast))
  
)

(defun dibujar_tapa_termo()

  (command "-layer" "S" "Tapa" "" "")
  (setq p_tapa_abajo(list(nth 0 punto_base)(nth 1 punto_base)(+(nth 2 punto_base)(+ altura_cuerpo  r_toroide 9))))
  (command "CYLINDER" p_tapa_abajo r_tapa altura_tapa "")
  (setq tapa_abajo(entlast))

  (setq p_helice(list(nth 0 p_tapa_abajo)(nth 1 p_tapa_abajo)(+(nth 2 p_tapa_abajo)8)))
  (command "HELIX" p_helice r_tapa r_tapa "T" 4 altura_helice "")
  (setq helice(entlast))

  (setq p_punta_helice(list(+(nth 0 p_helice)r_tapa)(nth 1 p_helice)(nth 2 p_helice)))
  (command "UCS" "M" p_punta_helice  "")
  (command "UCS" "X" -90 "")

  (setq p_origen(list 0 0 0))
  (command "CIRCLE" p_origen r_ext_helice "")
  (setq circulo(entlast))
  
  (command "EXTRUDE" circulo "" "P" helice "" "")
  (setq helices(entlast))
  
  (command "UNION" tapa_abajo helices "")
  (setq tapa_con_helices(entlast))
  
  (command "UCS" "W" "")
  (setq p_tapa_arriba(list(nth 0 p_tapa_abajo)(nth 1 p_tapa_abajo)(+(nth 2 p_tapa_abajo)altura_tapa)))
  (command "CYLINDER" p_tapa_arriba r_tapon altura_tapon)
  (setq tapa_arriba(entlast))

  (command "UNION" tapa_con_helices tapa_arriba "")
  (setq tapa_entera(entlast))

  (setq p_tapa_final(list(nth 0 p_tapa_arriba)(nth 1 p_tapa_arriba)(+(nth 2 p_tapa_arriba)altura_tapon)))
  (command "TORUS" p_tapa_final (- r_tapon 5) grosor_linea_tapon "")
  (setq circulo_tapon(entlast))
     
  (command "SUBTRACT" tapa_con_helices "" circulo_tapon "")
  (setq tapa_entera(entlast))
  (command "CHPROP" tapa_entera "" "C" color "" "")
  
  (cond
    
   ((= tapa 0)
    
   (progn
     
     ; Dibujar tapa a rosca
    
    (setq p_punta_box(list(+(nth 0 p_tapa_abajo) r_tapa 1)(-(nth 1 p_tapa_abajo)10)(nth 2 p_tapa_abajo)))
    (setq p_punta_box2(list(-(nth 0 p_punta_box)5)(+(nth 1 p_punta_box)20)(+(nth 2 p_punta_box)altura_tapa)))
    (command "BOX" p_punta_box p_punta_box2 "")
    (setq caja1(entlast))

    (setq p_cuña1(list(nth 0 p_punta_box2)(nth 1 p_punta_box2)(nth 2 p_punta_box2)))
    (setq p_cuña2(list(+(nth 0 p_punta_box)9)(nth 1 p_punta_box)(+(nth 2 p_punta_box)altura_tapa)))
    (command "WEDGE" p_cuña2 p_cuña1 altura_cuña "")
    (setq cuña1(entlast))
    
    (command "UNION" caja1 cuña1 "")
    (setq caja_cuña(entlast))
    (command "CHPROP" caja_cuña "" "C" 255 "" "") ;; Cambio Color caja cuña 1

    (setq p_mirror1(list(nth 0 p_tapa_abajo)(+(nth 1 p_tapa_abajo)r_tapa)(nth 2 p_tapa_abajo)))
    (setq p_mirror2(list(nth 0 p_tapa_abajo)(-(nth 1 p_tapa_abajo)r_tapa)(nth 2 p_tapa_abajo)))
    (command "MIRROR" caja_cuña "" p_mirror1 p_mirror2 "N" "")
    (setq caja_cuña2(entlast))
    (command "CHPROP" caja_cuña2  "" "C" color "" "");; Cambio Color caja cuña 2
    
    (command "SUBTRACT" tapa_entera "" caja_cuña caja_cuña2 "")
    (setq tapa_entera(entlast))
    (command "CHPROP" tapa_entera "" "C" 255 "" "") ;; Cambio Color tapa
  
    (command "-layer" "S" "0" "" "")
    (setq p_cilindros_abajo(list(+(nth 0 p_tapa_arriba)r_tapon)(nth 1 p_tapa_arriba)(+(nth 2 p_tapa_arriba)2)))
    (command "CYLINDER" p_cilindros_abajo r_cilindros altura_cilindros)
    (setq cilindro_solo(entlast))
    (command "CHPROP" cilindro_cono "" "C" color "" "")
  
    (setq p_cilindros_arriba(list(nth 0 p_cilindros_abajo)(nth 1 p_cilindros_abajo)(+(nth 2 p_cilindros_abajo)altura_cilindros)))
    (command "CONE" p_cilindros_arriba r_cilindros altura_cono)
    (setq cono_solo(entlast))
    (command "CHPROP" cono_solo "" "C" color "" "")

    (command "UNION" cilindro_solo cono_solo "")
    (setq cilindro_cono(entlast))
    (command "CHPROP" cilindro_cono "" "C" color "" "")

    (command "ARRAYPOLAR" cilindro_cono "" p_tapa_final "I" 24 "" "")
    (setq cilindros(entlast))
    (command "CHPROP" cilindros "" "C" color "" "")
    
    (command "EXPLODE" cilindros "")
    (setq capa_cilindros(ssget "x" '((8 . "0"))))
    (command "CHPROP" capa_cilindros "" "C" color "" "")

    (command "SUBTRACT" tapa_entera "" capa_cilindros "")
    (setq tapa_entera(entlast))
    (command "CHPROP" tapa_entera "" "C" color "" "")
    
    ;(command "UNION" tapa_entera cuerpo "") ;;; Lo comenté pq sino no se puede unir la manija al cuerpo del termo
    ;(setq termo(entlast))
   ))

    ((= tapa 1)
    
      (progn
	
    	 ; Dibujar Tapa a pico
	(command "-layer" "S" "Tapa" "" "")
	(setq p_tapon_arriba_derecha(list(nth 0 p_tapa_arriba)(+(nth 1 p_tapa_arriba)r_tapon)(nth 2 p_tapa_arriba)))
	(setq p_tapon_arriba_izquierda(list(nth 0 p_tapa_arriba)(-(nth 1 p_tapa_arriba)r_tapon)(nth 2 p_tapa_arriba)))
	(command "ARC" "C" p_tapa_arriba p_tapon_arriba_izquierda p_tapon_arriba_derecha "")
	(setq arco_path(entlast))

	(setq p_tapa_arriba_medio(list(nth 0 p_tapa_arriba)(nth 1 p_tapa_arriba)(+(nth 2 p_tapa_arriba)(/ altura_tapon 2))))
	(command "MOVE" arco_path "" p_tapa_arriba p_tapa_arriba_medio "")

	(setq p_move_ucs(list(nth 0 p_tapa_arriba_medio)(-(nth 1 p_tapa_arriba_medio)r_tapon)(nth 2 p_tapa_arriba_medio)))

	(command "UCS" "M" p_move_ucs)
	(command "UCS" "Y" 90 "")

	(setq p_box1(list 0 0 0))
	(setq p_box2(list(-(nth 0 p_box1)(* altura_tapon 0.7))(-(nth 1 p_box1)(* altura_tapon 0.7))(nth 2 p_box1)))
	(command "RECTANG" p_box1 p_box2 "")
	(setq rectangulo_path(entlast))

	(setq p_box3(list(/(nth 0 p_box2)2)(/(nth 1 p_box2)2)(nth 2 p_box2)))
	(command "MOVE" rectangulo_path "" p_box3 p_box1 "")

	(command "EXTRUDE" rectangulo_path "" "P" arco_path "")
	(setq arco_subt(entlast))

	(command "SUBTRACT" tapa_entera "" arco_subt "")

	(setq p_centro_pico(list(nth 0 p_box1)(+(nth 1 p_box1)r_tapon)(+(nth 1 p_box1)r_tapon)))
	(command "UCS" "M" p_centro_pico)

	(setq p_centro_pico(list 0 0 0))
	(command "CIRCLE" p_centro_pico r_interior_pico "")
	(setq circulo_pico(entlast))

	(command "CIRCLE" p_centro_pico r_exterior_pico "")
	(setq poligono_pico(entlast))

	(setq p_aux1(list(nth 0 p_centro_pico)(nth 1 p_centro_pico)(-(nth 2 p_centro_pico)r_tapon)))
	(setq p_aux2(list(+(nth 0 p_aux1)(+ altura_tapa altura_tapon))(nth 1 p_aux1)(nth 2 p_aux1)))

	(command "LINE" p_centro_pico p_aux1 "")
	(setq linea1(entlast))
	(command "LINE" p_centro_pico p_aux1 "")
	(setq linea3(entlast))
	
	(command "LINE" p_aux1 p_aux2 "")
	(setq linea2(entlast))
	(command "LINE" p_aux1 p_aux2 "")
	(setq linea4(entlast))

	(command "PEDIT" linea1 "" "J" linea2 "" "")
	(setq pico_path1(entlast))

	(command "PEDIT" linea3 "" "J" linea4 "" "")
	(setq pico_path2(entlast))

	(command "EXTRUDE" circulo_pico "" "P" pico_path1 "")
	(setq extrude_circulo_pico(entlast))
	(command "EXTRUDE" poligono_pico "" "P" pico_path2 "")
	(setq extrude_poligono_pico(entlast))

	(setq p_mover_pico(list(nth 0 p_centro_pico)(nth 1 p_centro_pico)(+(nth 2 p_centro_pico)d_afuera_pico)))

	(command "MOVE" extrude_circulo_pico extrude_poligono_pico "" p_centro_pico p_mover_pico "")
	
	(command "SUBTRACT" tapa_entera extrude_poligono_pico "" extrude_circulo_pico "")
	(command "UCS" "W" "")

    	))

   )

)

(defun dibujar_manija_termo()

  (command "-layer" "S" "Manija" "" "")

  (setq p_union1(list(-(nth 0 punto_base)radio_ext)(nth 1 punto_base)(+(nth 2 punto_base)altura_union1)))
  (command "UCS" "M" p_union1 "")
  (command "UCS" "Y" -90 "")
  
  (setq p_union1(list 0 0 0))
  (setq p_union2(list(+(nth 0 p_union1)altura_union2)(nth 1 p_union1)(nth 2 p_union1)))
  (setq p_manija1(list(nth 0 p_union1)(nth 1 p_union1)(+(nth 2 p_union1)largo_manija)))
  (setq p_manija2(list(+(nth 0 p_union1)altura_union2)(nth 1 p_union1)(+(nth 2 p_union1)largo_manija)))
  
  (command "CIRCLE" p_union1 r_manija "")
  (setq circulo_path_manija(entlast))
  (command "CHPROP" circulo_path_manija "" "C" color "" "")
  
  (command "LINE" p_union1 p_manija1 "")
  (setq l1(entlast))
  (command "LINE" p_manija1 p_manija2 "")
  (setq l2(entlast))
  (command "LINE" p_manija2 p_union2 "")
  (setq l3(entlast))

  (command "PEDIT" l1 "" "J" l2 l3 "" "")
  (setq linea_path_manija(entlast))
  (command "CHPROP" linea_path_manija "" "C" color "" "")

  (command "EXTRUDE" circulo_path_manija "" "P" linea_path_manija "")
  (setq manija(entlast))
  (command "CHPROP" manija "" "C" color "" "")
  
  (command "UNION" cuerpo manija "")
  (setq termo(entlast))
  (command "CHPROP" termo "" "C" color "" "")
  
  (command "UCS" "W" "")
  
)

(defun restauracion()
  (setvar "osmode" var)
  (command "-visualstyles" "C" "C" "")
)