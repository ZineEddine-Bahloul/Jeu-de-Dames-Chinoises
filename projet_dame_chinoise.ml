type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;


type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)

type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

(*A REMPLIR*)
let est_dans_losange (c:case)(dim:dimension):bool=
  let (i,j,k) = c in
  if i >= (-2*dim) && j>= -dim && j<=dim && k>= -dim && k<=dim then true else false;;

est_dans_losange (-3,-1,4) 3;;

(*Ici on a donc considerer l'etoile comme la  reunion des deux grands tiangles comportants les camps nord et sud*)
let est_dans_etoile (c:case)(dim:dimension):bool=
  let (i,j,k) = c in
  if (est_case(i,j,k)=true) && ((i >= -dim && j>= -dim && k>= -dim) || (i<=dim && j<=dim && k<=dim))
  then
    true
  else
    false;;        
let rec associe a l defaut=
  match l with
  | [] -> defaut
  | (a2, b) :: suite -> if a = a2 then b else associe a suite defaut;;

(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(* La configuation initiale avec deux joueurs et un plateau de dimension 2*)
let conf_init : configuration =
  ([((3, -1, -2), Jaune); ((3, -2, -1), Jaune); ((4, -2, -2), Jaune);
    ((5, -3, -2), Jaune); ((-3, 1, 2), Vert); ((-3, 2, 1), Vert);
    ((-4, 2, 2), Vert); ((-5, 3, 2), Vert)],
   [Vert; Jaune], 2);;

affiche conf_init;;

(*A essayer apres avoir fait remplir_init
  affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)


(*Q4*)
let  rec tourner_case (m:int)(c:case):case =
  let (i1,j1,k1) = c in
  match m with 
  | 1 -> (-k1,-i1,-j1)
  | 2 -> (j1,k1,i1)
  | 3 -> (-i1,-j1,-k1)
  | 4 -> (k1,i1,j1)
  | 5 -> (-j1,-k1,-i1)
  | 6 -> (i1,j1,k1) 
  | _ -> tourner_case (m-6) (i1,j1,k1)
;;
tourner_case(10)(-6, 3, 3);;
(*Q5*)
let translate (c:case)(v:vecteur):case = 
  let (c1,c2,c3)=c and (v1,v2,v3)=v in 
  (c1+v1,c2+v2,c3+v3) ;;
translate (-4,1,3)(0,2,-2);;

(*Q6*)
let diff_case(c1:case)(c2:case):vecteur =
  let (x1,y1,z1)=c1 and (x2,y2,z2)=c2 in 
  (x2-x1,y2-y1,z2-z1) ;;
diff_case(2, -2, 0)(2, 3, -5);;

(*Q7*)
let sont_cases_voisines(c1:case)(c2:case):bool =
  let (x1,y1,z1)=c1 and (x2,y2,z2)=c2 in
  let x=x2-x1 and y=y2-y1 and z=z2-z1 in 
  if (abs(x)=1 && abs(y)=1 && abs(z)!=1) ||(abs(x)=1 && abs(y)!=1 && abs(z)=1) ||(abs(x)!=1 && abs(y)=1 && abs(z)=1) then
    true 
  else 
    false ;;

sont_cases_voisines (1,0,(-1))(0,1,-1);;

(*Q8*)
let distance((i1,j1,k1):case)((i2,j2,k2):case):int=
  let (i3,j3,k3) = diff_case(i1,j1,k1)(i2,j2,k2) in
  if (i1=i2) then 
    abs(j3) 
  else 
  if (j1=j2) then 
    abs(i3) 
  else 
  if (k1=k2) then 
    abs(j3) 
  else 
    0 ;;
let test_alignement ((i1,j1,k1):case) ((i2,j2,k2):case):bool =
  if (i1=i2) || (j1=j2) || (k1=k2) then 
    true 
  else 
    false ;;
let calcul_pivot ((i1,j1,k1):case)((i2,j2,k2):case): case option =
  let (i3,j3,k3) = diff_case(i1,j1,k1)(i2,j2,k2) in 
  if test_alignement(i1,j1,k1)(i2,j2,k2)=true &&  distance(i1,j1,k1)(i2,j2,k2) mod 2 = 0 then 
    Some(i1+(i3/2),j1+(j3/2),k1+(k3/2)) 
  else 
    None ;;

calcul_pivot(2, -1, 1)(-1, -1, 2);;

(*Q9*)
let vec_et_dist((i1,j1,k1):case)((i2,j2,k2):case):vecteur*int=
  if test_alignement(i1,j1,k1)(i2,j2,k2)=true then 
    let dis = distance(i1,j1,k1)(i2,j2,k2) and (i3,j3,k3) = diff_case(i1,j1,k1)(i2,j2,k2) in
    ((i3/dis,j3/dis,k3/dis),dis)
  else 
    ((0,0,0),0);;
vec_et_dist (6, -3, -3)(-1, 4, -3);;

(* 2-Deuxième partie *)
(*2.1-Configuration initiale et rotation du plateau *)

(*Q10*)
let rec tourner_liste (l:'a list):'a list =
  match l with 
  |[]->[]
  |[e1]->[e1]
  |e1::e2::fin -> e2::tourner_liste(e1::fin) ;;
tourner_liste([Vert]) ;;
let rec der_liste(l: 'a list):'a =
  match l with 
  |[]->failwith "erreur liste vide"
  |[e1]->e1
  |e1::fin -> der_liste(fin);;
der_liste([Vert;Jaune;Rouge;Noir]);;

(*Q11*)
let rec remplir_segment(m:int)((i,j,k):case):case list =
  match m with 
  |1->[(i,j,k)]
  |m->(i,j,k)::remplir_segment(m-1)(i,j+1,k-1);;
remplir_segment 3 (-4,1,3) ;;

(*Q12*)
let rec concatene(l1:'a list)(l2:'a list):'a list =
  match l1 with 
  |[]->l2
  |e1::fin ->e1::concatene(fin)(l2);;
concatene([(-4, 1, 3); (-4, 2, 2)]) ([(-4, 3, 1);(-4,2,4)]);;
let rec remplir_triangle_bas(m:int)((i,j,k):case):case list =
  match m with
  |1->[(i,j,k)]
  |m -> concatene (remplir_segment m (i,j,k))(remplir_triangle_bas (m-1)(i-1,j+1,k)) ;;
remplir_triangle_bas 2 (0, 2, -2) ;;

(*Q13*)
let rec remplir_triangle_haut(m:int)((i,j,k):case):case list =
  match m with
  |1->[(i,j,k)]
  |m -> concatene (remplir_segment m (i,j,k))(remplir_triangle_haut (m-1)(i+1,j,k-1)) ;;
remplir_triangle_haut 3 (-3,4,-1);;

(*Q14*)
let rec colorie (coul:couleur)(lc:case list):case_coloree list =
  match lc with 
  |[]->failwith "liste vide"
  |[e1]->[(e1,coul)]
  |e1::fin -> (e1,coul)::colorie(coul)(fin);;
colorie(Vert)([(-3, 5, -2);(-3, 6, -3);(-2, 4, -2); (-2, 5, -3)]);;

(*Q15*)
let rec longueur (l:'a list):int =
  match l with
  |[]->0
  |e1::fin -> 1+longueur(fin);;

let rec tourner_liste_case_coloree (m:int)(lc:case_coloree list):case_coloree list =
  match lc with
  |[]->failwith "liste vide"
  |[cas,coul] -> [(tourner_case m cas,coul)]
  |(cas,coul)::fin -> (tourner_case m cas,coul)::tourner_liste_case_coloree m fin;;
tourner_liste_case_coloree (2)([((3, -6, 3),Jaune); ((3, -5, 2),Jaune); ((3, -4, 1),Jaune); ((2, -5, 3),Jaune); ((2, -4, 2),Jaune); ((1, -4, 3),Jaune)]);;
let ccl = [((-4, 1, 3),Jaune); ((-2, 1, 1),Jaune); ((-4, 3, 1),Jaune); ((4, -3, -1), Vert); ((4, -2, -2), Vert); ((4, -1, -3), Vert)];;
let rec tourner_config((ccl,cl,dim):configuration):configuration =
  let m = 6/longueur(cl) and cl = tourner_liste cl in
  tourner_liste_case_coloree m ccl,cl,dim ;;

(* affiche (ccl,[Vert;Jaune],3) ;;
   let test = [((-6, 3, 3),Jaune); ((3, 3, -6),Vert); ((3, -6, 3),Rouge)] ;;
   affiche ([((-4, 1, 3),Jaune); ((-4, 2, 2),Jaune); ((-4, 3, 1),Jaune)],[Vert;Jaune;Rouge],3);;
   affiche(tourner_config([((-4, 1, 3),Jaune); ((-4, 2, 2),Jaune); ((-4, 3, 1),Jaune)],[Vert;Jaune;Rouge],3)) ;;
*)
(*Q16*)

(*fonctions auxiliare *)

let premier (l:'a list):'a=
  match l with 
  |[]-> failwith"liste vide"
  |e::fin -> e;;

let rec remplir_init_aux (liste_joueur:couleur list)(liste_coul:couleur list)(dim:dimension):configuration =
  match liste_joueur with 
  |[]->failwith "erreur liste vide"
  |[coul1] -> colorie(coul1)(remplir_triangle_bas dim (-(dim+1),1,dim)),liste_joueur,dim
  |coul1::fin -> let a1 = colorie(coul1)(remplir_triangle_bas dim (-(dim+1),1,dim)) and x,y,z= remplir_init_aux fin liste_coul dim in
    let x,y,z = tourner_config (x,liste_coul,dim) in
    concatene a1 x , liste_joueur,dim ;;

let remplir_init (liste_joueur:couleur list)(dim:dimension):configuration =
  let liste_coul = liste_joueur in remplir_init_aux (liste_joueur)(liste_coul)(dim) ;;

affiche (remplir_init [Vert;Jaune] 3) ;;

(*2.2 Recherche et suppression de case dans une configuration :*)

(*Q17*)
let quelle_couleur (cas:case)(config:configuration):couleur=
  let x,y,z = config in (associe cas x Libre);;

(*Q18*)
let rec supprime_dans_config_aux(l:(case*couleur) list)(c,coul:case*couleur):(case * couleur) list = 
  match l with 
  |[]->failwith "erreur"
  |[e1]-> if e1=(c,coul) then [] else [e1]
  |e1::fin -> if e1=(c,coul) then fin else e1::supprime_dans_config_aux fin (c,coul);;

supprime_dans_config_aux ccl ((-4, 3, 7), Marron) ;; 
let supprime_dans_config (config:configuration)(c:case):configuration =
  let coul = quelle_couleur c config and l,y,z = config in
  supprime_dans_config_aux l (c,coul),y,z ;;

supprime_dans_config (ccl,[Vert;Jaune],3) (-4, 2, 2) ;;

(*2.3 Jouer un coup unitaire*)
(*Q19*)
let est_coup_valide ((l_cas_coul,l_coul,dim):configuration)(Du(c1,c2):coup):bool=
  (sont_cases_voisines c1 c2) && ((quelle_couleur c1 (l_cas_coul,l_coul,dim))=premier l_coul)&&(quelle_couleur c2 (l_cas_coul,l_coul,dim) = Libre)&&(est_dans_losange c2 dim);;


let config_initial = remplir_init [Vert;Jaune] 3 ;;
(*Q20*)
let appliquer_coup((l_cas_coul,l_coul,dim):configuration)(Du(c1,c2):coup):configuration=
  let coul = quelle_couleur c1 (l_cas_coul,l_coul,dim) in
  supprime_dans_config (((c2,coul)::l_cas_coul,l_coul,dim):configuration)(c1);;

appliquer_coup config_initial (Du((-4, 2, 2), (-3, 2, 1)));;
(*Q21*)
let mettre_a_jour_configuration (conf:configuration)(Du(c1,c2):coup):configuration =
  match est_coup_valide conf (Du(c1,c2)) with 
  |true -> tourner_config(appliquer_coup conf (Du(c1,c2)))
  |false -> failwith  "Ce coup n’est pas valide, le joueur doit rejouer";;

mettre_a_jour_configuration config_initial (Du((-4, 2, 2), (-3, 2, 1)));;
affiche(mettre_a_jour_configuration config_initial (Du((-4, 2, 2), (-3, 2, 1))));;

(*2.4 Jouer un coup*)

(*Q22*)
let rec est_libre_seg_aux ((i1,j1,k1):case)((i2,j2,k2):case)(conf:configuration)((x,y,z):vecteur)(dist:int):bool =
  match dist with 
  |0->failwith"c'est la meme case"
  |1->false
  |2->quelle_couleur (i2-x,j2-y,k2-z) conf = Libre
  |_ -> (quelle_couleur (i1+x,j1+y,k1+z) conf = Libre) &&( est_libre_seg_aux (i1+x,j1+y,k1+z) (i2,j2,k2) conf (x,y,z) (dist-1) ) ;;

let est_libre_seg ((i1,j1,k1):case)((i2,j2,k2):case)(conf:configuration):bool=
  let (x,y,z),(dist) = vec_et_dist (i1,j1,k1)(i2,j2,k2) in
  est_libre_seg_aux (i1,j1,k1)(i2,j2,k2)(conf)((x,y,z))(dist) ;;

est_libre_seg (-2, 3, -1)(-2, -1, 3) (ccl,[Jaune;Vert],3) ;;

(*Q23*)
let est_saut((i1,j1,k1):case)((i2,j2,k2):case)(conf:configuration):bool=
  let (x,y,z),(dist) = vec_et_dist (i1,j1,k1)(i2,j2,k2)  in 
  (dist=2)&&(est_libre_seg (i1,j1,k1)(i2,j2,k2)conf=false)&&(quelle_couleur (i2,j2,k2) conf =Libre);;
let conf_test1:configuration = [(-2, 1, 1),Jaune; (-2, 3, -1),Vert],[Vert;Jaune],3;;
est_saut (-2, 1, 1)(-2, 2, 0)conf_test1;;

(*Q24*)
let rec est_saut_multiple(l:case list)(conf:configuration):bool =
  let lst_cs_cl,lst_cl,dim = conf in  
  match l with 
  |[]->false
  |[c1]->false
  |[c1;c2]->(est_saut c1 c2 conf) && (est_dans_losange c2 dim)
  |c1::c2::fin -> (est_saut c1 c2 conf) && (est_saut_multiple (c2::fin) conf);;

let lst_case_test1:case list = [(-6, 3, 3); (-4, 3, 1); (-2, 3, -1); (0, 3, -3)];;
let conf_test2:configuration = [(-6, 3, 3),Vert; (-5, 3, 2),Vert; (-3, 3, 0),Vert; (-1, 3, -2),Vert],[Vert;Jaune],3 ;;
est_saut_multiple lst_case_test1 conf_test2 ;;
est_saut   (-4, 3, 1)(-2, 3, -1) conf_test2 ;;
(*Q25*)

(*est_coup_valide avec sauts multiples *)
let est_coup_valide ((l_cas_coul,l_coul,dim):configuration)(cp:coup):bool=
  match cp with 
  |Du(c1,c2)-> (sont_cases_voisines c1 c2) && (*quelle_couleur c1 (l_cas_coul,l_coul,dim))=premier l_coul*)(quelle_couleur c2 (l_cas_coul,l_coul,dim) = Libre)&&(est_dans_losange c2 dim)
  |Sm(l) -> est_saut_multiple(l)(l_cas_coul,l_coul,dim) ;;
est_coup_valide conf_test2 (Sm(lst_case_test1));;
(*appliquer_coup avec sauts multiples*)

let rec appliquer_coup((l_cas_coul,l_coul,dim):configuration)(cp:coup):configuration=
  match cp with 
  |Du(c1,c2)->let coul = quelle_couleur c1 (l_cas_coul,l_coul,dim) in
    supprime_dans_config (((c2,coul)::l_cas_coul,l_coul,dim):configuration)(c1)
  |Sm(l)-> match l with 
    |[c1;c2]->appliquer_coup (l_cas_coul,l_coul,dim)(Du(c1,c2))
    |c1::c2::fin -> let a1,b1,c1 = appliquer_coup (l_cas_coul,l_coul,dim)(Du(c1,c2)) in 
      appliquer_coup(a1,b1,c1)(Sm(c2::fin));;

appliquer_coup  conf_test2 (Sm(lst_case_test1));;

(*mettre_a_jour_configuration avec sauts multiples*)

let mettre_a_jour_configuration (conf:configuration)(cp:coup):configuration =
  match est_coup_valide conf (cp) with 
  |true -> tourner_config(appliquer_coup conf (cp))
  |false -> failwith  "Ce coup n’est pas valide, le joueur doit rejouer";;

mettre_a_jour_configuration conf_test2 (Sm(lst_case_test1));;

(*3 Vérifier une partie*)

(*Q26*)
let rec score ((l_cas_coul,l_coul,dim):configuration):int =
  let coul1=premier l_coul in 
  match l_cas_coul with
  |[]->failwith"liste vide"
  |[(i,j,k),coul]-> if coul = coul1 then i else 0
  |((i,j,k),coul)::fin -> if coul = coul1 then i+score(fin ,l_coul,dim) else score(fin ,l_coul,dim);;

score conf_test2;;

let rec score_gagnant_aux (l:case list):int =
  match l with
  |[]->failwith"liste vide"
  |[(i,j,k)]-> i
  |((i,j,k))::fin ->  i+score_gagnant_aux(fin) ;;
let score_gagnant (dim:dimension):int =
  let l = remplir_triangle_haut(dim)((dim+1,-dim,-1)) in
  score_gagnant_aux l ;;

score_gagnant_aux[(4, -3, -1); (4, -2, -2); (4, -1, -3); (5, -3, -2); (5, -2, -3); (6, -3, -3)];;
score_gagnant 1;;

(*Q27*)
let gagne ((l_cas_coul,l_coul,dim):configuration):bool=
  score (l_cas_coul,l_coul,dim) = score_gagnant dim ;;

(*Q28*)

(*aplliquer chaque coup et mettre a jour la config *)
let rec est_partie_aux_1 ((l_cas_coul,l_coul,dim):configuration)(cp_lst:coup list):configuration=
  match cp_lst with 
  |[]->failwith"liste de coup vide"
  |[cp1]->mettre_a_jour_configuration (l_cas_coul,l_coul,dim) cp1 
  |cp1::fin -> let a,b,c = mettre_a_jour_configuration (l_cas_coul,l_coul,dim) cp1 in
    est_partie_aux_1 (a,b,c) fin ;;


(*trouver quelle est le gagnant et retourner la couleur *)
let rec est_partie_aux_2 (l_cas_coul,l_coul,dim:configuration)(cp_lst:coup list):couleur =
  match l_coul with 
  |[]->failwith"liste de coup vide"
  |[coul1]-> if gagne (l_cas_coul,l_coul,dim) then coul1 else Libre
  |coul1::fin -> if gagne (l_cas_coul,l_coul,dim) then coul1 else est_partie_aux_2 (l_cas_coul,fin,dim) cp_lst ;;

let est_partie (conf:configuration)(cp_lst:coup list):couleur =
  let l_cas_coul,l_coul,dim = est_partie_aux_1 (conf) (cp_lst) in
  est_partie_aux_2 (l_cas_coul,l_coul,dim:configuration)(cp_lst:coup list) ;;
affiche conf_init ;;




let list_cp_test = [Du((-4, 2, 2), (-3, 2, 1));
                    Du((-4, 2, 2), (-3, 2, 1));
                    Du((-4, 2, 2), (-3, 2, 1));
                    Sm([(-4, 3, 1); (-2, 1, 1)]);
                    Sm([(-4, 3, 1); (-2, 1, 1)]);
                    Sm([(-4, 3, 1); (-2, 1, 1)])] ;;

let cpnf_test_partie3 = remplir_init [Vert;Rouge;Jaune] 3 ;;

affiche (est_partie_aux_1 cpnf_test_partie3 list_cp_test);;
est_partie cpnf_test_partie3 list_cp_test;;

(* 4) Bonus : *)
(*4.1 Calcul des coups :*)

(*Q29*)

type 'a arbre = V | N of ('a arbre) * 'a * ('a arbre);;

let a_2=N(V,2,V);;
let a_3=N(V,3,V);;

let a_2_1_3=N(a_2,1,a_3);;

let rec taille a=
  match a with
  |V->0
  |N(g,_,d)-> (taille g)+(taille d)+1;;

taille a_2_1_3;;

let rec miroir a=
  match a with
  |V->V
  |N(g,r,d)->N(miroir d ,r,miroir g );; (*pas oublier le miroir*)
miroir a_2_1_3;;