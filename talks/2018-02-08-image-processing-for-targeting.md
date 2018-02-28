---
title: Traitement d'image pour le ciblage
author: Simon Guillot
---

# Pertinence pour la SmartRemote

- Suppression des balises
- Pas d'estimation de la pose : pas de calibration entre deux sessions
- Pas de "localisation" pour des objets connus d'avance
- Les objets peuvent être déplacés : pointer son chat pour commander des croquettes

**La solution envisagée doit pouvoir** :

1. Reconnaitre des produits connus
2. Reconnaitre des objets génériques connus
3. Reconnaitre de nouveaux produits à partir de quelques photos (3 ou 4)

# Pertinence pour la SmartRemote

<table width="100%">
<caption>Catégories identifiées d'objets à reconnaitre</caption>
<tr>
<th width="33%">Objets abstraits</th>
<th width="33%">Objets concrets connus</th>
<th width="33%">Nouveaux objets concrets</th>
</tr>
<tr>
<td>
- TV
- Fenêtres
- Portes
- Animaux
</td>
<td>
- Sonos
- Amazon Echo
- Google Home
- Thermostat Nest
</td>
<td>
- Lampes
</td>
</tr>
</table>

**Difficultés identifiées** :

- Reconnaitre un écran lorsqu'il est allumé
- Différences de luminosité
- Occlusion des objets
- Plusieurs objets identiques à contrôler de manière indépendante

# Reconnaissance et détection d'objets

**Outils à disposition** :

- Des approches *deep learning* : reconnaissance d'objets génériques et de produits avec une grande résilience aux dégradations de l'image
- Le traitement du signal : des points d'intérêts identifiables, résistants aux changements de perspective



- Reconnaissance : classification de l'objet "principal" de l'image
- Détection : classification et localisation de l'ensemble des objets connus dans l'image. Différentes approches se basant sur des modèles de reconnaissance ou de détection directe.

![](/media/image-processing/localization_detection.png)

# Reconnaissance *versus* détection

<table style="margin-top: 50px">
<tr>
<th width="50%">Reconnaissance</th>
<th>Détection</th>
</tr>
<tr style='color: green'>
<td>
- Domaine foisonnant à l'état de l'art d'une très grande précision
- Simplicité des modèles
</td>
<td>
- Ordonnancement de plusieurs objets dans le champ de vision
</td>
</tr>
<tr style='color: red'>
<td>
- Ambiguïtés si plusieurs objets
- Compréhension difficile des prédictions
</td>
<td>
- Modèles plus complexes donc plus coûteux en calcul
</td>
</tr>
</table>

**Quelques exemples** :

- Reconnaissance : VGG, Inception v4, Resnet
- Détection : FasterRCNN, SSD, TinyYolo, Yolo v2

# Données de référence

- Les catégories : jeux de donnés accessibles tels ImageNet, COCO et PascalVOC
- Les objets connectés : modèles 3D, images marketing via Google Image
- A la configuration, l'utilisateur vise ses objets depuis plusieurs points de vues et 
ajuste les prises sur l'app

**Modèles 3D** :

- Apportent davantage d'informations que de simples images
- Ouvre l'accès à un champ de recherche spécifique (*3D model pose estimation*)
- Ne permet pas de gérer les objets "abstraits"
- Très peu courant pour les tâches de détection

**Intérêt des images** :

- Abondance de données pour tous les objets nécessaires
- Facilement étiquetables par l'utilisateur

# Supervision de l'apprentissage

<table width="100%" style="margin-top: 70px">
<tr>
<td style="color: green; vertical-align: middle">Supervisé</td>
<td style='text-align: center'>
Faiblement supervisé
<div style="font-size: smaller; margin-top: -10px">
Données étiquetées pour une tâche annexe. Par exemple : détection d'objets uniquement a partir des classes associées aux images.
</div>

<br/>

Semi-supervisé
<div style="font-size: smaller; margin-top: -10px">
Une part importante de l'ensemble d'entrainement n'est pas étiquetée.
</div>

<br/>

Rares exemples (*Few shots*)
<div style="font-size: smaller; margin-top: -10px">
Apprentissage de la tâche à partir d'un très petit nombre d'exemple étiquetés. Possiblement en ayant eu accès au préalable à un grand nombre d'exemples pour une tâche similaire.
</div>
</td>
<td style="color: red; vertical-align: middle">Non supervisé</td>
</tr>
</table>

# Conclusion

- Choix à faire : reconnaissance ou détection ?
- La solution se trouve probablement dans un ensemble de sous-solutions optimales :
	- Modèle avec apprentissage classique pour les objets abstraits
	- Algorithme utilisant des modèles 3D pour les produits connus d'avance
	- Pour les nouveaux objets concrêts : ajustement d'un modèle à partir de quelques prises de vues fournies par l'utilisateur

