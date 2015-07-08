cheminot.db
===========

L'[Open Data SNCF](https://data.sncf.com/) fournit des données relative aux horaires de train au format [GTFS](https://developers.google.com/transit/gtfs/).
Cet outil traite l'ensemble de ces données pour en créer une base données.
Le but étant d'être exploité par l'application mobile [cheminot.m](https://github.com/cheminotorg/cheminot.m).

La base de données produite est constituée d'un ensemble de fichiers:

* `cheminot.db`: une base de données sqlite contenant l'ensemble des trajets.
* `graph`: un fichier au format [protobuf](https://github.com/google/protobuf) contenant l'arbre des arrêts et des horaires.
* `calendardates`: un fichier au format [protobuf](https://github.com/google/protobuf) contenant le calendrier des trajets.
* `stops_ttree.json`: un fichier au format json contenant l'arbres des arrêts ([ternary tree](https://en.wikipedia.org/wiki/Ternary_tree)).

Tous ces fichiers sont donc livrés avec l'application [cheminot.m](https://github.com/cheminotorg/cheminot.m).
Les trois premiers sont utilisés par l'algorithme de planification de trajet (cf. le project [cheminot.c](https://github.com/cheminotorg/cheminot.c)).
Le dernier, `stops_ttree.json` est utilisé pour la suggestion des arrêts dans l'interface.

## Usage

`sbt run --help`

```
Usage: cheminotdb [options]

  --help
        prints this usage text
  -a | --autoupdate
        Auto-update mode
  --twitter-consumer-key <value>

  --twitter-consumer-secret <value>

  --twitter-access-key <value>

  --twitter-access-secret <value>

  --twitter-pseudo <value>
        Specify twitter pseudo to notify
  -s | --sqlite
        Build sqlite db
  -g | --graph
        Build graph file
  -c | --calendar
        Build calendar dates file
  -t | --ttstops
        Build TTreeStops
  -d <value> | --gtfs <value>
        Specify gtfs root directory
  -e <value> | --db <value>
        Specify db root directory
```

Les horaires de train au format GTFS doivent être présent dans le répertoire `gtfs`.
Il est possible de spécifier un autre emplacement avec l'option `-gtfs <path>`.
Il en est de même pour le répertoire de destination `db` en utilisant l'option `-db <path>`.

La SNCF renouvelle tous les mois ses horaires de train en republiant les fichiers gtfs.
Le mode démon vérifie périodiquement s'il y a eu une mise à jour ou non.
Si c'est effectivement le cas, les fichiers gtfs sont téléchargés et une nouvelle version de notre base de données est générée.
Il est possible d'envoyer un tweet une fois l'opération terminée.

## Cheat sheet

* `sbt run`: construit l'ensemble des fichiers.
* `sbt run -s`: construit `cheminot.db`.
* `sbt run -g`: construit `graph`.
* `sbt run -c`: construit `calendardates`.
* `sbt run -t`: construit `stops_ttree.json`
* `sbt run -a`: mode démon, met à jour automatiquement la base de données.
