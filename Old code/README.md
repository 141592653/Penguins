# dm-prog



Principe de l'algorithme : 
Dans l'absolu, on veut explorer toutes les branches de l'arbre des configurations.
Même si on ne le fait pas en pratique, il va falloir être sur qu'on n'oublie rien.
Pour cela, on va utiliser un système de marquage.

Chaque noeud est une configuration (ensemble + position) accompagnée du chemin (pré-chemin)
qui a permis d'arriver à cette configuration et du plus long chemin (post-chemin)
qu'on a trouvé jusqu'à présent (si on en a trouvé un) qui aboutit à la noyade 
du pingouin. 


On va maintenir les invariants suivants : 
-Un noeud est marqué si et seulement si on est sûr que sont post-chemin est maximum.
-Le pre-chemin (resp post) est maximum parmi ceux qu'on a vus.
-une configuration apparaît au plus une fois dans la file de priorité.
-le nombre de marquage d'un noeud est égal au nombre de fils restant à marquer pour
que le noeud puisse être marqué à son tour. Si on ne connaît pas encore ce nombre, 
il vaut 0.

L'algorithme : 
On met la configuration initiale dans file de priorité.
Une étape de l'algorithme va consister à extraire l'élément prioritaire et le traiter.
Traiter une configuration : 
-si la configuration est marquée, rien à faire
-regarder si le traitement de la configuration est pertinent (c'est-à-dire qu'on ait une chance 
d'obtenir un meilleur chemin que ceux obtenus jusqu'ici.
-pour chaque configuration fille 
   *mettre à jour le pré-chemin
   *si la configuration n'a jamais été rencontrée, l'ajouter à la file
   *sinon actualiser sa priorité
-Si on se trouve sur une feuille de l'arbre (le pingouin va se noyer), marquer la feuille
et mettre à jour les postes chemins et le marquage des parents.


Notes sur quelques détails : 
-les pré-chemins sont stockés dan l'ordre inverse
-on stocke en permanence la taille des chemins pour éviter à avoir à la recalculer.
-j'ai considéré que la première case ne faisait pas partie du chemin

A propos des performances :
-J'ai laissé les trois types de clés différents et ils peuvent se changer facilement.
Mais il faut prendre en compte que avec KeysSum et NoKeys, on n'aurait pas besoin de
calculer de manière systématique des disconnected ce qui accélèrerait le programme.
En revanche, au niveau du nombre d'appels dans la file, c'est KeysDis qui gagne.
En réalité, NoKeys n'est pas très loin derrière. Je crois que pour moi c'est la meilleure
performance en temps (quele ironie !)
