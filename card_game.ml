Random.self_init ();;


type couleur = Coeur|Carreau|Pique|Trefle ;;
type hauteur = Roi|Dame|Valet|Valeur of int ;;
type carte = couleur * hauteur ;;


type jeu = {mutable taille : int ;
mutable paquet : carte array} ;;


let nouveau_paquet () =
  let nouveau_jeu = {taille = 52 ; paquet = Array.make 52 (Carreau, Valeur(0))} in
    for i = 0 to 9 do
      nouveau_jeu.paquet.(i) <- (Carreau , Valeur(i+1)) ;
      nouveau_jeu.paquet.(i+13) <- (Coeur , Valeur(i+1)) ;
      nouveau_jeu.paquet.(i+26) <- (Pique , Valeur(i+1)) ;
      nouveau_jeu.paquet.(i+39) <- (Trefle , Valeur(i+1)) ;
    done;
    for i = 10 to 10 do
      nouveau_jeu.paquet.(i) <- (Carreau , Valet) ;
      nouveau_jeu.paquet.(i+13) <- (Coeur , Valet) ;
      nouveau_jeu.paquet.(i+26) <- (Pique , Valet) ;
      nouveau_jeu.paquet.(i+39) <- (Trefle , Valet) ;
    done;
    for i = 11 to 11 do
      nouveau_jeu.paquet.(i) <- (Carreau , Dame) ;
      nouveau_jeu.paquet.(i+13) <- (Coeur , Dame) ;
      nouveau_jeu.paquet.(i+26) <- (Pique , Dame) ;
      nouveau_jeu.paquet.(i+39) <- (Trefle , Dame) ;
    done;
    for i = 12 to 12 do
      nouveau_jeu.paquet.(i) <- (Carreau , Roi) ;
      nouveau_jeu.paquet.(i+13) <- (Coeur , Roi) ;
      nouveau_jeu.paquet.(i+26) <- (Pique , Roi) ;
      nouveau_jeu.paquet.(i+39) <- (Trefle , Roi) ;
    done;
    nouveau_jeu ;;


let tire jeu =
  let n = jeu.taille and v = jeu.paquet and c = ref (Carreau , Valeur(0)) in
    if v = [| |] then failwith "paquet vide" ;
  let i = Random.int(n) in
    c := v.(i) ;
      let v1 = Array.make (n-1) (Carreau , Valeur(0)) in
        for j = 0 to i-1 do
          v1.(j)<-v.(j)
        done ;
        for k = i+1 to n-1 do
          v1.(k-1)<-v.(k)
        done ;
    jeu.taille <- (n-1) ; jeu.paquet <- v1 ;
    !c;;


let valeur carte =
  match carte with
    |(_,Valeur(i)) -> if i=1 then 14 else i 
    |(_,Valet) -> 11
    |(_,Dame) -> 12 
    |(_,Roi) -> 13 ;;


let rec score_tour jeu1 jeu2 n =
  let jeu3 = nouveau_paquet () and jeu4 = nouveau_paquet () in
    if jeu1.paquet = [||] || jeu2.paquet = [||] then score_tour jeu3 jeu4 n
    else let a = valeur(tire jeu1) and b = valeur(tire jeu2) in
	 if a = b then score_tour jeu1 jeu2 (n+1)
	 else if a > b then (n,0)
	 else (0,n) ;;


let somme_score (x,y) (x1,y1) =
  (x+x1,y+y1) ;;


let partie n =
  let score = ref(0,0) and jeu1 = nouveau_paquet () and jeu2 = nouveau_paquet () in
     for i=1 to n do
       score := somme_score !score (score_tour jeu1 jeu2 1)
     done;
     !score ;;


let print_score (x,y) =
  let _ = Printf.printf "score joueur 1: %d, score joueur 2: %d\n" x y in
  if x > y then Printf.printf "%s\n\n" "joueur 1 a gagné"
  else 
    if x < y then Printf.printf "%s\n\n" "joueur 2 a gagné"
    else Printf.printf "%s\n\n" "égalité";;


print_score (partie 20);;


print_score (partie 1000000);;
