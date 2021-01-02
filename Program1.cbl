       identification division.
       program-id. Program1.

       environment division.
       configuration section.

       special-names.
           currency sign is "€"
           decimal-point is comma.

       data division.
       working-storage section.
      
      * Variables temporelles
       01 DateSysteme.
         10 AAAA PIC 9(4).
         10 MM PIC 9(2).
         10 JJ PIC 9(2).

       01 DateValidite.
         10 AAAA PIC 9(4).
         10 MM PIC 9(2).
         10 JJ PIC 9(2).

       01 AgeClient.
         10 AAAA PIC S9(4).
         10 MM PIC S9(2).
         10 JJ PIC S9(2).
     
       77 totaljoursAAAA PIC 9(4).
       77 totaljoursMM PIC 9(4).
       77 totaljoursJJ PIC 9(4).         

       77 tmpDateCreaClient PIC X(10).
       77 tmpDateCreaContrat PIC X(10).
       77 tmpDateCreaSinistre PIC X(10).
       77 tmpDateCreaBeneficiaire PIC X(10).
       77 tmpDatemodClient pic X(10).

      * Definition des variables graphiques
       77 CouleurFondEcran pic 99 value 15.
       77 CouleurCaractere pic 99 value 0.

      * Variable d'option des ecrans
       77 OptionMenuPrincipal pic 9 value 0.

      * Variable pour les choix d'options
       77 OptionRechercheNom pic 9 value 0.
       77 OptionRechercheCodeClient pic 9 value 0.
       77 OptionRechercheContrat pic 9 value 0.
       77 OptionRechercheSinistre pic 9 value 0.
       77 OptionRechercheBeneficiaire pic 9 value 0.
       77 OptionCreationClient pic 9 value 0.
       77 OptionCreationContrat pic 9 value 0.
       77 OptionCreationSinistre pic 9 value 0.
       77 OptionCreationBeneficiaire pic 9 value 0.
       77 OptionValiditeAgeClient pic 9 value 1.
       77 OptionVisualisationClients pic 9 value 0.
       77 OptionMenuClient pic 9 value 0.
       77 OptionVisualisationContrats pic 9 value 0.
       77 OptionMenuContrat pic 9 value 0.
       77 OptionVisualisationSinistres pic 9 value 0.
       77 OptionMenuSinistre pic 9 value 0.
       77 OptionVisualisationBeneficiaires pic 9 value 0.
       77 OptionMenuBeneficiaires pic 9 value 0.
       77 OptionModClient pic 9 value 0.
       77 OptionModBeneficiaire pic 9 value 0.
       77 OptionModContrat pic 9 value 0.
       77 OptionModSinistre pic 9 value 0.         

      * Variables de fin de traitement
       77 Menu-trt-fin pic 9.
       77 Recherche-nom-trt-Fin pic 9.
       77 Recherche-Codeclient-trt-Fin pic 9.
       77 Recherche-Contrat-trt-Fin pic 9.
       77 Recherche-Sinistre-trt-Fin pic 9.
       77 Recherche-Beneficiaire-trt-Fin pic 9.       
       77 VisualisationlisteClients-Trt-fin pic 9.
       77 VisualisationContrats-Trt-fin pic 9.
       77 VisualisationSinistres-Trt-fin pic 9.
       77 VisualisationBeneficiaires-Trt-fin pic 9.
       77 VisualisationListeBeneficiaire-trt-Fin pic 9.
       77 CreationClient-Trt-fin pic 9.
       77 CreationContrat-Trt-fin pic 9.
       77 CreationSinistre-trt-Fin pic 9.
       77 CreationBeneficiaire-trt-Fin pic 9.
       77 Visualisation-Detail-Client-trt-fin pic 9.
       77 Visualisation-Detail-Contrat-Trt-fin pic 9.
       77 Visualisation-Detail-Sinistre-Trt-fin pic 9.
       77 Visualisation-Detail-beneficiaire-trt-fin pic 9.

      * sert a faire la requete sql avec les 5 1er chiffres
       77 FillerREQSQL PIC x(6).

      * Faire les autres variables intermediaires
      * j'ai changer la longueur des variables pour ajuster la taille d'affichage de la liste des clients
       01 variablesIntermediaireClientCourant.
         05 codeClient PIC x(5).
         05 nom PIC x(10).
         05 prenom PIC x(10).
         05 DateNaissance.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).
         05 adresse PIC x(15).
         05 codePostal PIC x(5).
         05 ville PIC x(10).

       01 variablesIntermediaireContratCourant.
         05 codeContrat PIC x(5).
         05 refCodeClient PIC x(5).
         05 sinistresCouverts.
           10 IT PIC 9.
           10 PE PIC 9.
           10 IA PIC 9.
           10 MT PIC 9.
           10 CHM PIC 9.
           10 DC pic 9.
         05 date-Contrat.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).       
         05 franchise.
           10 FRIT PIC 99.
           10 FRPE PIC 99.
           10 FRIA PIC 99.
           10 FRMT PIC 99.
           10 FRCH PIC 99.
         05 NombreBeneficiaires PIC 99.
         05 Validite PIC XXX.

       01 variablesIntermediaireSinistreCourant.
         05 codeSinistre PIC x(5).
         05 refCodeClient PIC x(5).
         05 refCodeContrat PIC x(5).
         05 typeSinistre PIC XX.
         05 date-Sinistre.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).

       01 variablesIntermediaireBeneficiaireCourant.
         05 CodeBeneficiaire pic X(5).
         05 Codecontrat PIC x(5).
         05 nom PIC x(10).
         05 prenom PIC x(10).
         05 DateNaissance.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).
         05 adresse PIC x(15).
         05 codePostal PIC x(5).
         05 ville PIC x(10).
         05 somme pic X(12).
           

      * Correspond a la description des tables de la base de donnees. Le nom des champs doit etre identique aux champs des tables
      * ASTUCE : Commencer le niveau 2 avec un 05 permet de faire des copier/coller pour les tableaux et variables intermediaires
       01 clientcourant.
         05 codeClient PIC x(36).
         05 nom PIC x(30).
         05 prenom PIC x(30).
         05 DateNaissance.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).
         05 adresse PIC x(50).
         05 codePostal PIC x(5).
         05 ville PIC x(30).

       01 contratCourant.
         05 codeContrat PIC x(36).
         05 refCodeClient PIC x(36).
         05 sinistresCouverts.
           10 IT PIC 9.
           10 PE PIC 9.
           10 IA PIC 9.
           10 MT PIC 9.
           10 CHM PIC 9.
           10 DC pic 9.
         05 date-Contrat.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).       
         05 franchise.
           10 FRIT PIC 99.
           10 FRPE PIC 99.
           10 FRIA PIC 99.
           10 FRMT PIC 99.
           10 FRCH PIC 99.
         05 NombreBeneficiaires PIC 99.
         05 Validite PIC XXX.

       01 sinistreCourant.
         05 codeSinistre PIC x(36).
         05 refCodeClient PIC x(36).
         05 refCodeContrat PIC x(36).
         05 typeSinistre PIC XX.
         05 date-Sinistre.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).

       01 BeneficiaireCourant.
         05 CodeBeneficiaire pic X(36).
         05 Codecontrat PIC x(36).
         05 nom PIC x(30).
         05 prenom PIC x(30).
         05 DateNaissance.
           10 AAAA PIC 9(4).
           10 pic x value "/".
           10 MM PIC 9(2).
           10 pic x value "/".
           10 JJ PIC 9(2).
         05 adresse PIC x(50).
         05 codePostal PIC x(5).
         05 ville PIC x(30).
         05 somme pic X(12).

      * reservation en memoire de 50 lignes pour les tableau
       01 TableauClient.
         02 indice OCCURS 50.
           03 clientTable.
             05 codeClient PIC x(36).
             05 nom PIC x(30).
             05 prenom PIC x(30).
             05 DateNaissance.
               10 AAAA PIC 9(4).
               10 pic x value "/".
               10 MM PIC 9(2).
               10 pic x value "/".
               10 JJ PIC 9(2).
             05 adresse PIC x(50).
             05 codePostal PIC x(5).
             05 ville PIC x(30).

       01 TableauContrat.
         02 indice OCCURS 50.
           03 contratTable.
             05 codeContrat PIC x(36).
             05 refCodeClient PIC x(36).
             05 sinistresCouverts.
               10 IT PIC 9.
               10 PE PIC 9.
               10 IA PIC 9.
               10 MT PIC 9.
               10 CHM PIC 9.
               10 DC pic 9.
             05 date-Contrat.
               10 AAAA PIC 9(4).
               10 pic x value "/".
               10 MM PIC 9(2).
               10 pic x value "/".
               10 JJ PIC 9(2).       
             05 franchise.
               10 FRIT PIC 99.
               10 FRPE PIC 99.
               10 FRIA PIC 99.
               10 FRMT PIC 99.
               10 FRCH PIC 99.
             05 NombreBeneficiaires PIC 99.
             05 Validite PIC XXX.

       01 TableauSinistre.
         02 indice OCCURS 50.
           03 sinistreTable.
             05 codeSinistre PIC x(36).
             05 refCodeClient PIC x(36).
             05 refCodeContrat PIC x(36).
             05 typeSinistre PIC XX.
             05 date-Sinistre.
               10 AAAA PIC 9(4).
               10 pic x value "/".
               10 MM PIC 9(2).
               10 pic x value "/".
               10 JJ PIC 9(2).

       01 TableauBeneficiaire.
         02 indice OCCURS 50.
           03 beneficiaireTable.
             05 CodeBeneficiaire pic X(36).
             05 Codecontrat PIC x(36).
             05 nom PIC x(30).
             05 prenom PIC x(30).
             05 DateNaissance.
               10 AAAA PIC 9(4).
               10 pic x value "/".
               10 MM PIC 9(2).
               10 pic x value "/".
               10 JJ PIC 9(2).
             05 adresse PIC x(50).
             05 codePostal PIC x(5).
             05 ville PIC x(30).
             05 somme pic X(12).

      * Numero d'indice de la table, taille de la table et numero de ligne.
       77 indiceTabClient PIC 99.
       77 tailleTabClient PIC 99.
       77 NoligneClient pic 99.

       77 indiceTabContrat PIC 99.
       77 tailleTabContrat PIC 99.
       77 NoligneContrat pic 99.

       77 indiceTabSinistre PIC 99.
       77 tailleTabSinistre PIC 99.
       77 NoligneSinistre pic 99.

       77 indiceTabBeneficiaire PIC 99.
       77 tailleTabBeneficiaire PIC 99.
       77 NoligneBeneficiaire pic 99.

       77 indiceTab PIC 99.
       77 tailleTab PIC 99.
       77 Noligne pic 99.

      * Definit le nombre de page de la liste et la page courante
       77 pagecourante pic 99.
       77 pagesTotales pic 99.

      * Variable qui donne la concatenation des infos du client pour faire les listes
       77 resultatclient PIC X(80).
       77 resultatcontrat pic X(80).
       77 resultatsinistre pic X(80).
       77 resultatBeneficiaire pic X(80).

      * Déclaration des variables de connection SQL Server
       77 CNXDB STRING.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
               INCLUDE SQLDA
           END-EXEC.

       Screen section.

      * Menu principal avec recherche par nom/code client/num contrat/num sinistre/Beneficiaire et creation Client
       01 menu-principal background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU PRINCIPAL ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 5 value "Recherche par : ".
         10 line 7 col 5 value "- 1 - Nom".
         10 line 8 col 5 value "- 2 - Code client".
         10 line 9 col 5 value "- 3 - Numero de contrat".
         10 line 10 col 5 value "- 4 - Numero de sinistre".
         10 line 11 col 5 value "- 5 - Beneficiaire".
         10 line 13 col 5 value "- 6 - Creation Client ".
         10 line 14 col 5 value "- 0 - Quitter ".
         10 line 18 col 5 value "Option : ".

      * Recherche par nom/prenom du client
       01 Menu-Recherche-nomPrenom background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE PAR NOM ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Recherche par nom : ".
         10 line 8 col 25 using Nom of clientcourant PIC X(30).
         10 line 9 col 5 value "Recherche par prenom : ".
         10 line 9 col 28 using Prenom of clientcourant PIC X(30).

      * recherche par code client
       01 Menu-Recherche-Codeclient background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE PAR NUMERO DE CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Recherche par Numero de client : ".
         10 line 8 col 38 using CodeClient of clientcourant PIC X(5).

      * recherche par contrat
       01 Menu-Recherche-Codecontrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE PAR NUMERO DE CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Recherche par Numero de contrat : ".
         10 line 8 col 39 using CodeContrat of contratcourant PIC X(5).

      * Recherche par sinistre
       01 Menu-Recherche-Codesinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE PAR NUMERO DE SINISTE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Recherche par Numero de sinistre : ".
         10 line 8 col 40 using Codesinistre of sinistrecourant PIC X(5).

      * recherche beneficiaires
       01 menu-recherche-Beneficiaires background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU RECHERCHE BENEFICIAIRES ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Nom : ".
         10 line 8 col 11 using Nom of beneficiairecourant PIC X(30).
         10 line 9 col 5 value "Prenom : ".
         10 line 9 col 14 using Prenom of beneficiairecourant PIC X(30).
         10 line 10 col 5 value "Code contrat : ".
         10 line 10 col 19 using codeContrat of Contratcourant PIC X(5).

      * Creation d'un client
       01 menu-Creation-Client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Nom            : ".
         10 line 8 col 22 using Nom of clientCourant PIC X(30).
         10 line 9 col 5 value "Prenom         : ".
         10 line 9 col 22 using Prenom of clientCourant PIC X(30).
         10 line 10 col 5 value "Adresse        : ".
         10 line 10 col 22 using Adresse of clientCourant PIC X(60).
         10 line 11 col 5 value "Code Postal    : ".
         10 line 11 col 22 using CodePostal of clientCourant PIC X(5).
         10 line 12 col 5 value "Ville          : ".
         10 line 12 col 22 using Ville of clientCourant PIC X(30).
         10 line 13 col 5 value "Date naissance : ".
         10 line 13 col 22 using JJ of ClientCourant PIC 9(2).
         10 line 13 col 24 value "/".
         10 line 13 col 25 using MM of ClientCourant PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 using AAAA of ClientCourant PIC 9(4).
         10 line 17 col 5 value "Option : ".
         10 line 17 col 14 from OptionMenuClient PIC 9.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- 0 - Quitter                                                       ".
         10 line 21 col 5 value "- 1 - Cree                                                          ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Creation d'un contrat
       01 menu-Creation-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.         
         10 line 5 col 2 from CodeClient of clientCourant PIC X(5).
         10 line 5 col 9 value "/".
         10 Line 5 Col 10 from Nom of clientCourant PIC X(10).
         10 line 5 col 22 value "/".
         10 Line 5 Col 23 from Prenom of clientCourant PIC X(10).
         10 line 10 col 5 value " Garantie couverte : ".
         10 line 12 col 5 value " IT - Incapacite Temporaire :                Franchise de :    jours ".
         10 line 12 col 35 using IT of contratCourant PIC 9.
         10 line 12 col 66 using FRIT of contratCourant PIC 99.
         10 line 13 col 5 value " PE - Perte d'emploi        :                Franchise de :    jours ".
         10 line 13 col 35 using PE of contratCourant PIC 9.
         10 line 13 col 66 using FRPE of contratCourant PIC 99.
         10 line 14 col 5 value " IA - Invalidite            :                Franchise de :    jours ".
         10 line 14 col 35 using IA of contratCourant PIC 9.
         10 line 14 col 66 using FRIA of contratCourant PIC 99.
         10 line 15 col 5 value " MT - Maternite             :                Franchise de :    jours ".
         10 line 15 col 35 using MT of contratCourant PIC 9.
         10 line 15 col 66 using FRMT of contratCourant PIC 99.
         10 line 16 col 5 value " CH - Chomage               :                Franchise de :    jours ".
         10 line 16 col 35 using CHM of contratCourant PIC 9.
         10 line 16 col 66 using FRCH of contratCourant PIC 99.
         10 line 17 col 5 value " DC - DECES                 :                Nb de beneficiaires :   ".
         10 line 17 col 35 using DC of contratCourant PIC 9.
         10 line 18 col 5 value " Option : ".
         10 line 18 col 15 from OptionMenuContrat PIC 9.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- 0 - Quitter                                                       ".
         10 line 21 col 5 value "- 1 - Cree                                                          ".
      *  10 line 22 col 5 value "- 2 - Calcul du prix du contrat                                     ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".
      *todo Si on as le temps, faire un autre menu choix avec le calcul du prix du contrat fx option et somme beneficiaire

      * Creation d'un sinistre
       01 menu-Creation-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION SINISTRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 5 from codecontrat of contratcourant PIC X(8).
         10 line 5 col 15 value "/".
         10 Line 5 Col 16 from Nom of clientCourant PIC X(15).
         10 line 5 col 35 value "/".
         10 Line 5 Col 36 from Prenom of clientCourant PIC X(15).
         10 line 7 col 5 value " Type : ".
         10 Line 7 Col 13 using typeSinistre of sinistrecourant PIC XX.
         10 line 8 col 5 value " Date du sinistre : ".
         10 Line 8 Col 24 using JJ of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 26 value "/".
         10 Line 8 Col 27 using MM of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 29 value "/".
         10 Line 8 Col 30 using AAAA of Date-sinistre of sinistrecourant PIC 9(4).
         10 line 18 col 5 value " Option : ".           
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Quitter                                                       ".
         10 line 22 col 5 value "- 1 - Cree                                                          ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".
      
      * Creation Date du sinistre
       01 Menu-Creation-Sinistre-Date background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 Line 8 Col 24 using JJ of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 26 value "/".
         10 Line 8 Col 27 using MM of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 29 value "/".
         10 Line 8 Col 30 using AAAA of Date-sinistre of sinistrecourant PIC 9(4).

      * Creation d'un beneficiaire
       01 menu-Creation-Beneficiaire background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU CREATION BENEFICIAIRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 8 col 5 value "Nom            : ".
         10 line 8 col 22 using Nom of beneficiaireCourant PIC X(30).
         10 line 9 col 5 value "Prenom         : ".
         10 line 9 col 22 using Prenom of beneficiaireCourant PIC X(30).
         10 line 10 col 5 value "Adresse        : ".
         10 line 10 col 22 using Adresse of beneficiaireCourant PIC X(60).
         10 line 11 col 5 value "Code Postal    : ".
         10 line 11 col 22 using CodePostal of beneficiaireCourant PIC X(5).
         10 line 12 col 5 value "Ville          : ".
         10 line 12 col 22 using Ville of beneficiaireCourant PIC X(30).
         10 line 13 col 5 value "Date naissance : ".
         10 line 13 col 22 using JJ of beneficiaireCourant PIC 9(2).
         10 line 13 col 24 value "/".
         10 line 13 col 25 using MM of beneficiaireCourant PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 using AAAA of beneficiaireCourant PIC 9(4).
         10 line 14 col 5 value "Somme du contrat d'assurance vie : ".
         10 line 14 col 40 using somme of beneficiaireCourant PIC X(12).
         10 line 14 col 53 value "Euros".
         10 line 18 col 5 value "Option : ".
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Quitter                                                       ".
         10 line 22 col 5 value "- 1 - Cree                                                          ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Affichage du menu visualisation liste clients
       01 menu-visualisation-liste-clients background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION LISTE CLIENTS ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 6 col 1 value "Nu  Client  Nom        Prenom     Adresse           CP    Ville      Naissance  ".
         10 line 7 col 1 value "--------------------------------------------------------------------------------".
         10 line 18 col 5 value " Num Client : ".        
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "-Num- Selection du client dans la liste                             ".
         10 line 22 col 5 value "- 0 - Menu Principal                                                ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Affichage menu Visualisation detail client
       01 menu-Visualisation-Detail-client background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION DETAIL CLIENT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 7 col 5 value "Code client    : ".
         10 line 7 col 22 from CodeClient of clientCourant PIC X(5).
         10 line 8 col 5 value "Nom            : ".
         10 line 8 col 22 using Nom of clientCourant PIC X(30).
         10 line 9 col 5 value "Prenom         : ".
         10 line 9 col 22 using Prenom of clientCourant PIC X(30).
         10 line 10 col 5 value "Adresse        : ".
         10 line 10 col 22 using Adresse of clientCourant PIC X(60).
         10 line 11 col 5 value "Code Postal    : ".
         10 line 11 col 22 using CodePostal of clientCourant PIC X(5).
         10 line 12 col 5 value "Ville          : ".
         10 line 12 col 22 using Ville of clientCourant PIC X(30).
         10 line 13 col 5 value "Date naissance : ".
         10 line 13 col 22 using JJ of DateNaissance of ClientCourant PIC 9(2).
         10 line 13 col 24 value "/".
         10 line 13 col 25 using MM of DateNaissance of ClientCourant PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 using AAAA of DateNaissance of ClientCourant PIC 9(4).
         10 line 17 col 5 value "Option : ".
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "- 0 - Quitter                       - 4 - Cree un contrat           ".
         10 line 21 col 5 value "- 1 - Modifier                      - 5 -                           ".
         10 line 22 col 5 value "- 2 - Voir les contrats             - 6 -                           ".
         10 line 23 col 5 value "- 3 - Voir les sinistres            - 7 -                           ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Cache certaines options du menu Detail Client
       01 menu-Visualisation-Detail-client-Choix background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 3 col 1 value " MENU MODIFICATION DETAIL CLIENT ".
         10 line 20 col 5 value "- 0 - Quitter                                                       ".
         10 line 21 col 5 value "- 1 - Valider                                                       ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".

      * Affichage du menu Visualisation liste contrats
       01 menu-visualisation-liste-contrats background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION LISTE CONTRATS ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 2 from refCodeClient of contratCourant PIC X(5).
         10 line 5 col 8 value "/".
         10 Line 5 Col 9 from Nom of clientCourant PIC X(10).
         10 line 5 col 20 value "/".
         10 Line 5 Col 21 from Prenom of clientCourant PIC X(10).
         10 line 5 col 32 value "/".
         10 Line 5 Col 33 from Ville of clientCourant PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
         10 line 7 col 1 value "Nu  contrat Client IT FR  PE FR  IA FR  MT FR  CH FR  DC Nb  Signature  Validite".
         10 line 18 col 5 value " Numero Contrat : ".
         10 line 18 col 67 value "Page ".
      *  10 line 18 col 72 from pageCourante.
      *  10 line 18 col 75 value "de ".
      *  10 line 18 col 78 from pagesTotales.
      * Prévoir une alerte si le client a plus de 65 ans.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "-Num- Selection contrat                                             ".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Affichage menu visualisation detail contrat
       01 menu-visualisation-Detail-contrat background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION DETAIL CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 2 from CodeClient of clientCourant PIC X(5).
         10 line 5 col 8 value "/".
         10 Line 5 Col 9 from Nom of clientCourant PIC X(10).
         10 line 5 col 20 value "/".
         10 Line 5 Col 21 from Prenom of clientCourant PIC X(10).
         10 line 5 col 32 value "/".
         10 Line 5 Col 33 from Ville of clientCourant PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Date du contrat : ".
         10 Line 7 Col 24 from JJ of contratcourant.
         10 line 7 col 26 value "/".
         10 Line 7 Col 27 from MM of contratcourant.
         10 line 7 col 29 value "/".
         10 Line 7 Col 30 from AAAA of contratcourant.
         10 line 8 col 5 value " Code Contrat : ".
         10 line 8 col 19 from codeContrat of contratCourant PIC X(36).
         10 line 10 col 5 value " Garantie couverte : ".
         10 line 12 col 5 value " IT - Incapacite Temporaire :                Franchise de :    jours ".
         10 line 12 col 35 using IT of contratCourant PIC 9.
         10 line 12 col 66 using FRIT of contratCourant PIC 99.
         10 line 13 col 5 value " PE - Perte d'emploi        :                Franchise de :    jours ".
         10 line 13 col 35 using PE of contratCourant PIC 9.
         10 line 13 col 66 using FRPE of contratCourant PIC 99.
         10 line 14 col 5 value " IA - Invalidite            :                Franchise de :    jours ".
         10 line 14 col 35 using IA of contratCourant PIC 9.
         10 line 14 col 66 using FRIA of contratCourant PIC 99.
         10 line 15 col 5 value " MT - Maternite             :                Franchise de :    jours ".
         10 line 15 col 35 using MT of contratCourant PIC 9.
         10 line 15 col 66 using FRMT of contratCourant PIC 99.
         10 line 16 col 5 value " CH - Chomage               :                Franchise de :    jours ".
         10 line 16 col 35 using CHM of contratCourant PIC 9.
         10 line 16 col 66 using FRCH of contratCourant PIC 99.
         10 line 17 col 5 value " DC - DECES                 :                Nb de beneficiaires :   ".
         10 line 17 col 35 using DC of contratCourant PIC 9.
         10 line 17 col 73 from NombreBeneficiaires of contratCourant PIC 9.
         10 line 18 col 5 value " Option : ".
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu Precedant               - 3 - Creation Sinistre          ".
         10 line 22 col 5 value "- 1 - Modification                 - 4 - Ajout Beneficiaire         ".
         10 line 23 col 5 value "- 2 - Visualisation Sinistre       - 5 - Modification Beneficiaire  ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Cache certaines options du menu Detail Contrat
       01 menu-Visualisation-Detail-contrat-Choix background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 3 col 1 value " MENU MODIFICATION DETAIL CONTRAT ".
         10 line 20 col 5 value "- 0 - Quitter                                                       ".
         10 line 21 col 5 value "- 1 - Valider                                                       ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".

      * Affichage du menu Visualisation liste sinistre par contrat
       01 menu-visualisation-liste-sinistres background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION LISTE SINISTRES PAR CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 2 from Codeclient of clientCourant PIC X(5).
         10 line 5 col 8 value "/".
         10 Line 5 Col 9 from Nom of clientCourant PIC X(10).
         10 line 5 col 20 value "/".
         10 Line 5 Col 21 from Prenom of clientCourant PIC X(10).
         10 line 5 col 32 value "/".
         10 Line 5 Col 33 from Ville of clientCourant PIC X(15).
         10 Line 5 Col 60 from JJ of dateNaissance of clientCourant.
         10 line 5 col 62 value "/".
         10 Line 5 Col 63 from MM of dateNaissance of clientCourant.
         10 line 5 col 65 value "/".
         10 Line 5 Col 66 from AAAA of dateNaissance of clientCourant.
         10 line 7 col 5 value " Num / Sinistre / Client / Contrat / type / date sinistre                                   ".
         10 line 18 col 5 value " Option : ".       
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- Num- Selection sinistre                                           ".
         10 line 22 col 5 value "- 0 - Menu Precedent                                                ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Affichage menu visualisation detail sinistre
       01 menu-visualisation-Detail-sinistre background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION DETAIL SINISTRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.
         10 line 5 col 5 from refcodeclient of Sinistrecourant PIC X(5).
         10 line 5 col 12 value "/".
         10 Line 5 Col 14 from refCodecontrat of SinistreCourant PIC X(5).
         10 line 5 col 21 value "/".
         10 Line 5 Col 23 from Nom of clientCourant PIC X(15).
         10 line 5 col 40 value "/".
         10 Line 5 Col 42 from Prenom of clientCourant PIC X(15).
         10 line 7 col 5 value " Type : ".
         10 Line 7 Col 13 from typeSinistre of sinistrecourant PIC XX.
         10 line 8 col 5 value "Date du sinistre : ".
         10 Line 8 Col 24 from JJ of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 26 value "/".
         10 Line 8 Col 27 from MM of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 29 value "/".
         10 Line 8 Col 30 from AAAA of Date-sinistre of sinistrecourant PIC 9999.
         10 line 18 col 5 value " Options : ".        
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu precedant                                                ".
         10 line 22 col 5 value "- 1 - Modification                                                  ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".
      
      * Cache certaines options du menu Detail Sinistre
       01 menu-Visualisation-Detail-sinistre-Choix background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 3 col 1 value " MENU MODIFICATION DETAIL SINISTRE ".
         10 line 7 col 5 value " Type : ".
         10 Line 7 Col 13 using typeSinistre of sinistrecourant PIC XX.
         10 line 8 col 5 value "Date du sinistre : ".
         10 Line 8 Col 24 using JJ of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 26 value "/".
         10 Line 8 Col 27 using MM of Date-sinistre of sinistrecourant PIC 99.
         10 line 8 col 29 value "/".
         10 Line 8 Col 30 using AAAA of Date-sinistre of sinistrecourant PIC 9999.
         10 line 18 col 5 value " Options : ".
         10 line 20 col 5 value "- 0 - Quitter                                                       ".
         10 line 21 col 5 value "- 1 - Valider                                                       ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".

      * Affichage du menu Visualisation liste beneficiaires par contrat
       01 menu-visualisation-liste-beneficiaires background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION LISTE BENEFICIAIRES PAR CONTRAT ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.       
         10 line 7 col 1 value " Nu  Contrat  Nom        Prenom        CP     Ville      Naissance      Somme ".
         10 line 18 col 5 value " Numero Beneficiaire : ".
         10 line 18 col 67 value "Page ".
         10 line 18 col 72 from pageCourante.
         10 line 18 col 75 value "de ".
         10 line 18 col 78 from pagesTotales.
         10 line 19 col 5 value "--------------------------------------------------------------------".
         10 line 20 col 5 value "-Num- Selection beneficiaire                                        ".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "                                                                    ".
         10 line 23 col 5 value "                                                                    ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Affichage menu visualisation detail beneficiaire
       01 menu-visualisation-Detail-beneficiaire background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 1 col 1 Blank Screen.
         10 line 3 col 1 value " MENU VISUALISATION DETAIL BENEFICIAIRE ".
         10 line 3 col 60 value " Date : ".
         10 line 3 col 68 from JJ of DateSysteme.
         10 line 3 col 70 value "/".
         10 line 3 col 71 from MM of DateSysteme.
         10 line 3 col 73 value "/".
         10 line 3 col 74 from AAAA of DateSysteme.       
         10 line 7 col 5 value "Code contrat   : ".
         10 line 7 col 22 from Codecontrat of beneficiaireCourant PIC X(36).
         10 line 8 col 5 value "Nom            : ".
         10 line 8 col 22 using Nom of beneficiaireCourant PIC X(30).
         10 line 9 col 5 value "Prenom         : ".
         10 line 9 col 22 using Prenom of beneficiaireCourant PIC X(30).
         10 line 10 col 5 value "Adresse        : ".
         10 line 10 col 22 using Adresse of beneficiaireCourant PIC X(30).
         10 line 11 col 5 value "Code Postal    : ".
         10 line 11 col 22 using CodePostal of beneficiaireCourant PIC X(5).
         10 line 12 col 5 value "Ville          : ".
         10 line 12 col 22 using Ville of beneficiaireCourant PIC X(30).
         10 line 13 col 5 value "Date naissance : ".
         10 line 13 col 22 using JJ of beneficiaireCourant PIC 9(2).
         10 line 13 col 24 value "/".
         10 line 13 col 25 using MM of beneficiaireCourant PIC 9(2).
         10 line 13 col 27 value "/".
         10 line 13 col 28 using AAAA of beneficiaireCourant PIC 9(4).
         10 line 14 col 5 value "Somme du contrat d'assurance vie : ".
         10 line 14 col 40 using somme of beneficiaireCourant PIC X(12).
         10 line 14 col 53 value "Euros".
         10 line 18 col 5 value "Option : ".        
         10 line 20 col 5 value "--------------------------------------------------------------------".
         10 line 21 col 5 value "- 0 - Menu Precedant                                                ".
         10 line 22 col 5 value "- 1 - Modification                                                  ".
         10 line 23 col 5 value "- 2 - Suppression Beneficiaires                                     ".
         10 line 24 col 5 value "--------------------------------------------------------------------".

      * Cache certaines options du menu Detail beneficiaire
       01 menu-Visualisation-Detail-beneficiaire-Choix background-color is CouleurFondEcran foreground-color is CouleurCaractere.
         10 line 3 col 1 value " MENU MODIFICATION DETAIL BENEFICIAIRE ".
         10 line 21 col 5 value "- 0 - Quitter                                                       ".
         10 line 22 col 5 value "- 1 - Valider                                                       ".

      
       procedure division.
      ***************************************************************
      ** Main Menu          OK si pas de modif a rajouter
      ***************************************************************
       Menu.
           perform Menu-Init.
           perform Menu-Trt until Menu-trt-fin = 1.
           perform Menu-Fin.

      ***************************************************************
      ** Menu Principal       Tout est OK
      ***************************************************************

       Menu-init.
           move 0 to Menu-trt-fin.
           Move FUNCTION CURRENT-DATE TO DateSysteme.           
           initialize clientcourant.
           initialize contratCourant.
           initialize sinistreCourant.
           initialize BeneficiaireCourant.

      ********** Connexion à la base de données ***********************
           
      ********************************************************************************************************************
      ***********   Si export du code, PENSER A MODIFIER LES REFERENCES DU SERVER SQL PAR LES VOTRES   *******************
      ********************************************************************************************************************
           MOVE "Trusted_Connection=yes;Database=stagePOECCobol;server=COMPNAME\SQLEXPRESS;factory=System.Data.SqlClient;"
      ********************************************************************************************************************
             to cnxDb.
           exec sql
               Connect using :CnxDb
           end-exec.

      ********** Choix de l'option Autocommit  *************************
           EXEC SQL
               SET AUTOCOMMIT ON
           End-EXEC.

       Menu-trt.
           move 1 to Menu-trt-fin.
           display menu-principal.
           Move "" to OptionMenuPrincipal.
           accept OptionMenuPrincipal line 18 col 14.

           evaluate OptionMenuPrincipal
               when 1
                   perform Recherche-nom
               when 2
                   perform Recherche-codeclient
               when 3
                   perform Recherche-contrat
               when 4
                   perform Recherche-sinistre
               when 5
                   perform Recherche-Beneficiaire
               when 6
                   perform CreationClient
               when 0
                   perform Menu-Fin
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   accept OptionMenuPrincipal line 18 col 14
                   perform Menu-Trt
           end-evaluate.

       Menu-Fin.
           stop run.

      ***************************************************************
      ** Recherche Nom/prenom     Modif OK    
      ***************************************************************
       Recherche-nom.
           perform Recherche-nom-Init.
           perform Recherche-nom-Trt until Recherche-nom-trt-Fin = 1.
           perform Recherche-nom-Fin.

       Recherche-nom-Init.
           Move 0 to Recherche-nom-trt-Fin.
           initialize indiceTabclient.
           initialize tailleTabclient.
           initialize clientCourant.
           initialize FillerREQSQL.
           Display Menu-Recherche-nomPrenom.
           accept Menu-Recherche-nomPrenom.

       Recherche-nom-Trt.
           Move 1 to Recherche-nom-trt-Fin.
           if nom of clientCourant = '' AND prenom of clientcourant = '' THEN
               perform Menu
           else
               perform SQLNom
               perform displayListeClientsNom
               perform OptionVisualisationClient.
     
       OptionVisualisationClient.
           accept optionVisualisationClients line 18 col 19
           evaluate optionVisualisationClients
               when 0
                   perform menu
               when 1 thru 9
                   if OptionVisualisationClients >= indiceTabClient then
                       display "Veillez entrer un choix valide. " line 19 col 5                        
                       perform OptionVisualisationClient
                   else
                       move optionVisualisationClients to indiceTab
                       move corresponding ClientTable(indiceTab) to clientCourant
                       perform Visualisation-Detail-Client
                   end-if
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5       
                   perform OptionVisualisationClient.        

       SQLNom.
      ************************************************************************************************
      * declare un curseur  CursorClient
      * selectionne les variables a mettre dans le curseur  de la table clients
      * ou le nom ou le prenom est egale :clientcourant.nom , :clientcourant.prenom
      * classer par nom
      *************************************************************************************************
           EXEC sql
              declare Cursor-nom-prenom cursor for
              select codeClient, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance), adresse, codePostal, ville
              from clients
              where nom = :clientcourant.nom  OR prenom = :clientcourant.prenom
              order by nom, prenom
           END-EXEC
      * ouvre le curseur
           EXEC sql
              open Cursor-nom-prenom
           END-EXEC
      * Met 1 dans l'indice pour initialiser le tableau a la ligne 1 de ClientTab et 0 dans la taille de la table.
           move 1 to indiceTabclient
           move 0 to tailleTabclient
      * boucle aussi longtemps qu'il y a des occurances
           perform until SQLCODE <> 0
      * Met le contenu du curseur dans les variables. Attention a la concordance
               EXEC sql
                          fetch Cursor-nom-prenom into
                          :clientcourant.codeClient,
                          :clientcourant.nom,
                          :clientcourant.prenom,
                          :clientcourant.DateNaissance.JJ,
                          :clientcourant.DateNaissance.MM,
                          :clientcourant.DateNaissance.AAAA,
                          :clientcourant.adresse,
                          :clientcourant.codePostal,
                          :clientcourant.ville
               END-EXEC
      * Move chaque variable dans le tableau ClientTable a l'indice en cours
      * et ensuite ajoute 1 a l'indice et a la taille de la table
               IF SQLCODE = 0 THEN
                   move corresponding clientcourant to clientTable(indiceTabclient)
                   add 1 to indiceTabclient
                   add 1 to tailleTabclient
               end-if

           END-PERFORM.
           EXEC sql
                 close Cursor-nom-prenom
           END-EXEC.

       DisplayListeClientsNom.
      
           Display menu-visualisation-liste-clients
           initialize indiceTab.
           perform until indiceTab = tailleTabclient
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabclient
                   add 1 to indiceTab
                   move corresponding clientTable(indiceTab) to variablesIntermediaireClientCourant
                   STRING indiceTab "  "
                     codeClient of variablesIntermediaireClientCourant "   "
                     nom of variablesIntermediaireClientCourant " "
                     prenom of variablesIntermediaireClientCourant " "
                     adresse of variablesIntermediaireClientCourant "   "
                     codePostal of variablesIntermediaireClientCourant " "
                     ville of variablesIntermediaireClientCourant " "
                     JJ of dateNaissance of variablesIntermediaireClientCourant "/"
                     MM of dateNaissance of variablesIntermediaireClientCourant "/"
                     AAAA of dateNaissance of variablesIntermediaireClientCourant INTO resultatclient
                   DISPLAY resultatclient line NoLigne col 1
                   ADD 1 TO NoLigne
               end-perform.

       Recherche-nom-Fin.
           EXEC sql
               close Cursor-nom-prenom
           END-EXEC
           perform Menu.

      ***************************************************************
      ** Recherche Code Client   Modif OK
      ***************************************************************
       Recherche-Codeclient.
           perform Recherche-Codeclient-Init.
           perform Recherche-Codeclient-Trt until Recherche-Codeclient-trt-Fin = 1.
           perform Recherche-Codeclient-Fin.

       Recherche-Codeclient-Init.
           Move 0 to Recherche-Codeclient-trt-Fin.
           initialize indiceTabclient.
           initialize tailleTabclient.
           initialize clientCourant.
           initialize FillerREQSQL.
           Display menu-Recherche-CodeClient.
           accept menu-Recherche-CodeClient.
       Recherche-Codeclient-Trt.
           Move 1 to Recherche-Codeclient-trt-Fin.
           if codeClient of clientCourant = '' THEN
               perform menu
           else
               STRING codeClient of clientcourant '%' DELIMITED ' ' INTO FillerREQSQL
               perform SQLClient
               perform DisplayClientCode
               perform OptionVisualisationCodeClient.

       OptionVisualisationCodeClient.
           accept optionVisualisationClients line 18 col 19
           evaluate optionVisualisationClients
               when 0
                   perform menu
               when 1 
                   move optionVisualisationClients to indiceTab
                   move corresponding ClientTable(indiceTab) to clientCourant
                   perform Visualisation-Detail-Client
              when other
                   display "Veillez entrer un choix valide. " line 19 col 5       
                   perform OptionVisualisationCodeClient.


       SQLClient.
      *************************************************************************************************
      * declare un curseur  CursorClient
      * selectionne les variables a mettre dans le curseur de la table clients
      * ou le nom ou le prenom ou le codeclient est egale :client.nom , :client.prenom ou :CodeClientFillerREQSQL
      * classer par nom et prenom
      *************************************************************************************************
           EXEC sql
              declare Cursor-CodeClient cursor for
              select codeClient, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance), adresse, codePostal, ville
              from clients
              where CodeClient like :FillerREQSQL
              order by nom, prenom
           END-EXEC

           EXEC sql
              open Cursor-CodeClient
           END-EXEC
      * Initialisation du tableau Client (clientTab) avec un indice de 1 et une taille de 0.
           move 1 to indiceTabclient
           move 0 to tailleTabclient
      * boucle aussi longtemps qu'il y a des occurances
           perform until SQLCODE <> 0
      * Met le contenu du curseur dans les variables. Attention a la concordance
               EXEC sql
                   fetch Cursor-CodeClient into
                   :clientcourant.codeClient,
                   :clientcourant.nom,
                   :clientcourant.prenom,
                   :clientcourant.DateNaissance.JJ,
                   :clientcourant.DateNaissance.MM,
                   :clientcourant.DateNaissance.AAAA,
                   :clientcourant.adresse,
                   :clientcourant.codePostal,
                   :clientcourant.ville
               END-EXEC
      * Move chaque variable dans le tableau ClientTable a l'indice en cours et ensuite ajoute 1 a l'indice et a la taille de la table
               IF SQLCODE = 0 THEN
                   move corresponding clientcourant to clientTable(indiceTabclient)
                   add 1 to indiceTabclient
                   add 1 to tailleTabclient
               end-if
           END-PERFORM.
           EXEC sql
                 close Cursor-CodeClient
           END-EXEC.

       DisplayClientCode.
           Display menu-visualisation-liste-clients
           initialize indiceTab.
           perform until indiceTab = tailleTabclient
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabclient
                   add 1 to indiceTab
                   move corresponding clientTable(indiceTab) to variablesIntermediaireClientCourant
                   STRING indiceTab " "
                     codeClient of variablesIntermediaireClientCourant " "
                     nom of variablesIntermediaireClientCourant " "
                     prenom of variablesIntermediaireClientCourant " "
                     adresse of variablesIntermediaireClientCourant " "
                     codePostal of variablesIntermediaireClientCourant " "
                     ville of variablesIntermediaireClientCourant " "
                     JJ of dateNaissance of variablesIntermediaireClientCourant "/"
                     MM of dateNaissance of variablesIntermediaireClientCourant "/"
                     AAAA of dateNaissance of variablesIntermediaireClientCourant INTO resultatclient
                   DISPLAY resultatclient line NoLigne col 1
                   ADD 1 TO NoLigne
               end-perform.

       Recherche-Codeclient-Fin.
           continue.

      ***************************************************************
      ** Recherche contrat    Modif OK
      ***************************************************************
       Recherche-Contrat.
           perform Recherche-Contrat-Init.
           perform Recherche-Contrat-Trt until Recherche-Contrat-trt-Fin = 1.
           perform Recherche-Contrat-Fin.
.
       Recherche-Contrat-Init.
           Move 0 to Recherche-Contrat-trt-Fin.
           initialize indiceTabcontrat.
           initialize tailleTabContrat.
           initialize contratCourant.
           initialize FillerREQSQL.
           Display Menu-Recherche-Codecontrat.
           accept Menu-Recherche-Codecontrat.
       Recherche-Contrat-Trt.
           Move 1 to Recherche-Contrat-trt-Fin.
           if codeContrat of contratCourant = '' THEN
               perform menu
           else
               STRING codecontrat of contratcourant '%' DELIMITED ' ' INTO FillerREQSQL
               perform SQLContrat
               perform DisplayContrat
               perform OptionVisualisationCodeContrat.

       OptionVisualisationCodeContrat.
           accept optionVisualisationContrats line 18 col 23
           evaluate optionVisualisationContrats
               when 0
                   perform menu
               when 1 
                   move optionVisualisationContrats to indiceTab
                   move corresponding ContratTable(indiceTab) to contratCourant
                   perform Visualisation-Detail-Contrat
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   perform OptionVisualisationCodeContrat.

       SQLContrat.
      ************************************************************************************************
      * declare un curseur  Cursorcontrat
      * selectionne les variables a mettre dans le curseur  de la table contrat
      * ou le codeContrat = :FillerREQSQL
      * classer par nom
      *************************************************************************************************
           EXEC sql
                   declare Cursor-CodeContrat Cursor for
                   select codeContrat, CodeClient, IT, PE, IA, MT, CH, DAY(dateSignature), MONTH(dateSignature), YEAR(dateSignature), FRIT,FRPE,FRIA,FRMT,FRCH,DC
                   from contrats
                   where CodeContrat like :FillerREQSQL                    
           END-EXEC

           EXEC sql
               open Cursor-CodeContrat
           END-EXEC
      * Met 1 dans l'indice de la table et 0 dans la taille de la table.
           move 1 to indiceTabcontrat
           move 0 to tailleTabContrat
      * boucle aussi longtemps qu'il y a des occurances
           perform until SQLCODE <> 0
      * Met le contenu du curseur dans les variables. Attention a la concordance
               EXEC sql
                   fetch Cursor-CodeContrat into
                   :contratcourant.codeContrat,
                   :contratcourant.refCodeClient,
                   :contratcourant.sinistresCouverts.IT,
                   :contratcourant.sinistresCouverts.PE,
                   :contratcourant.sinistresCouverts.IA,
                   :contratcourant.sinistresCouverts.MT,
                   :contratcourant.sinistresCouverts.CHM,
                   :contratcourant.Date-contrat.JJ,
                   :contratcourant.Date-contrat.MM,
                   :contratcourant.Date-contrat.AAAA,
                   :contratcourant.franchise.FRIT,
                   :contratcourant.franchise.FRPE,
                   :contratcourant.franchise.FRIA,
                   :contratcourant.franchise.FRMT,
                   :contratcourant.franchise.FRCH,
                   :contratcourant.sinistresCouverts.DC,
                   :contratcourant.NombreBeneficiaires
               END-EXEC
      * Move chaque variable dans le tableau ContratTable a l'indice en cours et ensuite ajoute 1 a l'indice et a la taille de la table
               IF SQLCODE = 0 THEN
                   move corresponding contratcourant to contratTable(indiceTabcontrat)
                   add 1 to indiceTabcontrat
                   add 1 to tailleTabContrat
               end-if
           END-PERFORM.
           EXEC sql
               close Cursor-CodeContrat
           END-EXEC.

       DisplayContrat.
           Display menu-visualisation-liste-contrats.
           initialize indiceTab.
           perform until indiceTab = tailleTabContrat
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabContrat
                   add 1 to indiceTab
                   move corresponding contratTable(indiceTab) to variablesIntermediaireContratCourant
                   STRING indiceTab "  "
                     codeContrat of variablesIntermediaireContratCourant "  "
                     refCodeClient of variablesIntermediaireContratCourant "   "
                     IT of variablesIntermediaireContratCourant "  "
                     FRIT of variablesIntermediaireContratCourant "  "
                     PE of variablesIntermediaireContratCourant "  "
                     FRPE of variablesIntermediaireContratCourant "  "
                     IA of variablesIntermediaireContratCourant "  "
                     FRIA of variablesIntermediaireContratCourant "  "
                     MT of variablesIntermediaireContratCourant "  "
                     FRMT of variablesIntermediaireContratCourant "  "
                     CHM of variablesIntermediaireContratCourant "  "
                     FRCH of variablesIntermediaireContratCourant "  "
                     DC of variablesIntermediaireContratCourant "  "
                     NombreBeneficiaires of variablesIntermediaireContratCourant "  "
                     JJ of date-Contrat of variablesIntermediaireContratCourant "/"
                     MM of date-Contrat of variablesIntermediaireContratCourant "/"
                     AAAA of date-Contrat of variablesIntermediaireContratCourant INTO resultatcontrat
                   DISPLAY resultatcontrat line NoLigne col 1
                   ADD 1 TO NoLigne
               end-perform
           end-perform.

       Recherche-Contrat-Fin.
           perform Menu.

      ***************************************************************
      ** Recherche sinistre    Modif OK
      ***************************************************************
       Recherche-Sinistre.
           perform Recherche-Sinistre-Init.
           perform Recherche-Sinistre-Trt until Recherche-Sinistre-trt-Fin = 1.
           perform Recherche-Sinistre-Fin.

       Recherche-Sinistre-Init.
           Move 0 to Recherche-Sinistre-trt-Fin.
           initialize indiceTabsinistre.
           initialize tailleTabSinistre.
           initialize sinistreCourant.
           initialize FillerREQSQL.
           Display Menu-Recherche-Codesinistre.
           accept Menu-Recherche-Codesinistre.
       Recherche-Sinistre-Trt.
           Move 1 to Recherche-Sinistre-trt-Fin.
           if codeSinistre of SinistreCourant = '' THEN
               perform menu
           else
               STRING codeSinistre of sinistrecourant '%' DELIMITED ' ' INTO FillerREQSQL
               perform SQLSinistre
               perform DisplaySinistre
               perform OptionVisualisationCodeSinistre.

       OptionVisualisationCodeSinistre.
           accept optionVisualisationSinistres line 18 col 14
           evaluate optionVisualisationSinistres
               when 0
                   perform menu
               when 1 
                   move optionVisualisationSinistres to indiceTab
                   move corresponding SinistreTable(indiceTab) to SinistreCourant
                   perform Visualisation-Detail-Sinistre
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   perform OptionVisualisationCodeSinistre.

       SQLSinistre.
      ************************************************************************************************
      * declare un curseur  CursorClient
      * selectionne les variables a mettre dans le curseur  de la table clients
      * ou le nom ou le prenom ou le codeclient est egale :client.nom , :client.prenom ou :CodeClientFillerREQSQL
      * classer par nom
      *************************************************************************************************

           EXEC sql
              declare Cursor-CodeSinistre cursor for
              select codeSinistre, codeClient, codeContrat, typeSinistre, DAY(dateDuSinistre), MONTH(dateDuSinistre), YEAR(dateDuSinistre)
              from sinistres
              where CodeSinistre like :FillerREQSQL               
           END-EXEC
      * ouvre le curseur
           EXEC sql
              open Cursor-CodeSinistre
           END-EXEC
      * Met 1 dans l'indice de la table et 0 dans la taille de la table ?
           move 1 to indiceTabsinistre
           move 0 to tailleTabsinistre
      * boucle aussi longtemps qu'il y a des occurances
           perform until SQLCODE <> 0
      * Met le contenu du curseur dans les variables. Attention a la concordance
               EXEC sql
                   fetch Cursor-CodeSinistre into
                   :Sinistrecourant.codeSinistre,
                   :Sinistrecourant.refCodeClient,
                   :Sinistrecourant.refCodeContrat,
                   :Sinistrecourant.TypeSinistre,
                   :Sinistrecourant.Date-sinistre.JJ,
                   :Sinistrecourant.Date-sinistre.MM,
                   :Sinistrecourant.Date-sinistre.AAAA
               END-EXEC
      * Move chaque variable dans le tableau ContratTable a l'indice en cours et ensuite ajoute 1 a l'indice et a la taille de la table
               IF SQLCODE = 0 THEN
                   move corresponding sinistrecourant to sinistreTable(indiceTabsinistre)
                   add 1 to indiceTabsinistre
                   add 1 to tailleTabsinistre
               end-if
           END-PERFORM.
           EXEC sql
                 close Cursor-CodeSinistre
           END-EXEC.

       DisplaySinistre.
           Display menu-visualisation-liste-sinistres.
           initialize indiceTab.
           perform until indiceTab = tailleTabsinistre
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabsinistre
                   add 1 to indiceTab
                   move corresponding sinistreTable(indiceTab) to variablesIntermediaireSinistreCourant
                   STRING "      " indiceTab "    "
                     codeSinistre of variablesIntermediaireSinistreCourant "     "
                     refCodeClient of variablesIntermediaireSinistreCourant "    "
                     refCodeContrat of variablesIntermediaireSinistreCourant "     "
                     typesinistre of variablesIntermediaireSinistreCourant "       "
                     JJ of date-Sinistre of variablesIntermediaireSinistreCourant "/"
                     MM of date-Sinistre of variablesIntermediaireSinistreCourant "/"
                     AAAA of date-Sinistre of variablesIntermediaireSinistreCourant INTO resultatsinistre
                   DISPLAY resultatsinistre line NoLigne col 1
                   ADD 1 TO NoLigne
               end-perform.

       Recherche-Sinistre-Fin.
           Perform Menu.

      ***************************************************************
      * Recherche beneficiaire    Modif OK     
      ***************************************************************
       Recherche-Beneficiaire.
           perform Recherche-Beneficiaire-Init.
           perform Recherche-Beneficiaire-Trt until Recherche-Beneficiaire-trt-Fin = 1.
           perform Recherche-Beneficiaire-Fin.

       Recherche-Beneficiaire-Init.
           Move 0 to Recherche-Beneficiaire-trt-Fin.
           initialize BeneficiaireCourant.
           initialize indiceTabbeneficiaire.
           initialize tailleTabbeneficiaire.
           initialize FillerREQSQL.
           Display Menu-Recherche-beneficiaires.
           accept Menu-Recherche-beneficiaires.

       Recherche-Beneficiaire-Trt.
           Move 1 to Recherche-Beneficiaire-trt-Fin.
           if nom of beneficiaireCourant = '' AND prenom of beneficiairecourant = '' and codeContrat of contratCourant = '' THEN
               perform Menu
           else
               STRING codecontrat of contratcourant '%' DELIMITED ' ' INTO FillerREQSQL
               perform SQLBeneficiaire
               perform DisplayBeneficiaire
               perform OptionVisualisationCodeBeneficiaire.

       OptionVisualisationCodeBeneficiaire.
           accept optionVisualisationbeneficiaires line 18 col 28
           evaluate optionVisualisationbeneficiaires
               when 0
                   perform menu
               when 1 thru 9
                   if OptionVisualisationBeneficiaires >= indiceTabBeneficiaire then
                       display "Veillez entrer un choix valide. " line 19 col 5
                       perform OptionVisualisationBeneficiaire
                   else
                       move optionVisualisationbeneficiaires to indiceTab
                       move corresponding beneficiaireTable(indiceTab) to beneficiaireCourant
                       perform Visualisation-Detail-beneficiaire
                   end-if
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   perform OptionVisualisationCodeBeneficiaire.

       SQLBeneficiaire.
      ************************************************************************************************
      * declare un curseur  Cursor-Beneficiaire
      * selectionne les variables a mettre dans le curseur  de la table beneficiaire
      * ou le nom ou le prenom est egale :beneficiairecourant.nom , :beneficiairecourant.prenom
      * classer par nom.
      *************************************************************************************************
           EXEC sql
              declare Cursor-Beneficiaire cursor for
              select codeBeneficiaire, codecontrat, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance), adresse, codePostal, ville, somme
              from Beneficiaires
              where nom = :beneficiairecourant.nom  OR prenom = :beneficiairecourant.prenom OR codecontrat like :FillerREQSQL  
              order by nom
           END-EXEC
           
           EXEC sql
              open Cursor-Beneficiaire
           END-EXEC
     
           move 1 to indiceTabbeneficiaire
           move 0 to tailleTabbeneficiaire
      
           perform until SQLCODE <> 0
      
               EXEC sql
                       fetch Cursor-Beneficiaire into
                       :beneficiairecourant.codeBeneficiaire,
                       :beneficiairecourant.codeContrat,
                       :beneficiairecourant.nom,
                       :beneficiairecourant.prenom,
                       :beneficiairecourant.DateNaissance.JJ,
                       :beneficiairecourant.DateNaissance.MM,
                       :beneficiairecourant.DateNaissance.AAAA,
                       :beneficiairecourant.adresse,
                       :beneficiairecourant.codePostal,
                       :beneficiairecourant.ville,
                       :beneficiairecourant.somme
               END-EXEC
      
               IF SQLCODE = 0 THEN
                   move corresponding beneficiairecourant to beneficiaireTable(indiceTabbeneficiaire)
                   add 1 to indiceTabbeneficiaire
                   add 1 to tailleTabbeneficiaire
               end-if
           END-PERFORM.
           EXEC sql
                 close Cursor-nom-prenom
           END-EXEC.

       DisplayBeneficiaire.
           Display menu-visualisation-liste-beneficiaires
           initialize indiceTab.
           perform until indiceTab = tailleTabbeneficiaire
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabbeneficiaire
                   add 1 to indiceTab
                   move corresponding beneficiaireTable(indiceTab) to variablesIntermediairebeneficiaireCourant
                   STRING " " indiceTab "  "       
                     codecontrat of variablesIntermediairebeneficiaireCourant "    "
                     nom of variablesIntermediairebeneficiaireCourant " "
                     prenom of variablesIntermediairebeneficiaireCourant "    "         
                     codePostal of variablesIntermediairebeneficiaireCourant "  "
                     ville of variablesIntermediairebeneficiaireCourant " "
                     JJ of dateNaissance of variablesIntermediairebeneficiaireCourant "/"
                     MM of dateNaissance of variablesIntermediairebeneficiaireCourant "/"
                     AAAA of dateNaissance of variablesIntermediairebeneficiaireCourant " "
                     somme of variablesIntermediairebeneficiaireCourant INTO resultatbeneficiaire
                   DISPLAY resultatbeneficiaire line NoLigne col 1       
                   ADD 1 TO NoLigne
               end-perform.
                                                           

       Recherche-Beneficiaire-Fin.
           Perform Menu.     

      ***************************************************************
      ** Visualisation liste Contrats     Modif OK
      ***************************************************************
       VisualisationContrats.
           perform VisualisationContrats-Init.
           perform VisualisationContrats-Trt until VisualisationContrats-Trt-fin = 1.
           perform VisualisationContrats-Fin.

       VisualisationContrats-Init.
           Move 0 to VisualisationContrats-Trt-fin.
           Display menu-visualisation-liste-contrats.
           initialize indiceTabcontrat.
           initialize tailleTabcontrat.

       VisualisationContrats-Trt.
           Move 1 to VisualisationContrats-Trt-fin.
           perform SQLContratListe.
           perform DisplayContratListe.
           perform OptionVisualisationContrat.
       OptionVisualisationContrat.
           accept optionVisualisationContrats line 18 col 23
           evaluate optionVisualisationContrats
               when 0
                   perform menu
               when 1 thru 9
                   if optionVisualisationContrats >= indiceTabContrat then
                       display "Veillez entrer un choix valide. " line 19 col 5
                       perform OptionVisualisationContrat
                   else
                       move optionVisualisationContrats to indiceTab
                       move corresponding ContratTable(indiceTab) to contratCourant
                       perform Visualisation-Detail-Contrat
                   end-if
              when other
           display "Veillez entrer un choix valide. " line 19 col 5
           perform OptionVisualisationContrat.

       SQLContratListe.
           EXEC sql
               declare Cursor-CodeContratliste Cursor for
               select codeContrat, CodeClient, IT, PE, IA, MT, CH, DAY(dateSignature), MONTH(dateSignature), YEAR(dateSignature), FRIT, FRPE, FRIA, FRMT, FRCH, DC, NombreBeneficiaires
               from contrats
               where Codeclient = :clientCourant.codeclient
               order by dateSignature DESC
           END-EXEC

           EXEC sql
               open Cursor-CodeContratliste
           END-EXEC

           move 1 to indiceTabContrat
           move 0 to tailleTabContrat

           perform until SQLCODE <> 0
               EXEC sql
                   fetch Cursor-CodeContratliste into
                   :contratcourant.codeContrat,
                   :contratcourant.refCodeClient,
                   :contratcourant.sinistresCouverts.IT,
                   :contratcourant.sinistresCouverts.PE,
                   :contratcourant.sinistresCouverts.IA,
                   :contratcourant.sinistresCouverts.MT,
                   :contratcourant.sinistresCouverts.CHM,
                   :contratcourant.Date-contrat.JJ,
                   :contratcourant.Date-contrat.MM,
                   :contratcourant.Date-contrat.AAAA,
                   :contratcourant.franchise.FRIT,
                   :contratcourant.franchise.FRPE,
                   :contratcourant.franchise.FRIA,
                   :contratcourant.franchise.FRMT,
                   :contratcourant.franchise.FRCH,
                   :contratcourant.sinistresCouverts.DC,
                   :contratcourant.NombreBeneficiaires
               END-EXEC

               IF SQLCODE = 0 THEN
                   move corresponding contratcourant to contratTable(indiceTabContrat)
                   add 1 to indiceTabContrat
                   add 1 to tailleTabContrat
               end-if
           END-PERFORM.

           EXEC sql
               close Cursor-CodeContratliste
           END-EXEC.

       DisplayContratListe.
           initialize indiceTab
           perform until indiceTab = tailleTabcontrat
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabcontrat
                   add 1 to indiceTab
                   move corresponding contratTable(indiceTab) to variablesIntermediaireContratCourant

                   perform ValiditeContrat      

                   STRING indiceTab "  "
                     codeContrat of variablesIntermediaireContratCourant "   "
                     refCodeClient of variablesIntermediaireContratCourant "  "
                     IT of variablesIntermediaireContratCourant "  "
                     FRIT of variablesIntermediaireContratCourant "  "
                     PE of variablesIntermediaireContratCourant "  "
                     FRPE of variablesIntermediaireContratCourant "  "
                     IA of variablesIntermediaireContratCourant "  "
                     FRIA of variablesIntermediaireContratCourant "  "
                     MT of variablesIntermediaireContratCourant "  "
                     FRMT of variablesIntermediaireContratCourant "  "
                     CHM of variablesIntermediaireContratCourant "  "
                     FRCH of variablesIntermediaireContratCourant "  "
                     DC of variablesIntermediaireContratCourant "  "
                     NombreBeneficiaires of variablesIntermediaireContratCourant "  "
                     JJ of date-Contrat of variablesIntermediaireContratCourant "/"
                     MM of date-Contrat of variablesIntermediaireContratCourant "/"
                     AAAA of date-Contrat of variablesIntermediaireContratCourant "   "
                     Validite of variablesIntermediaireContratCourant " " INTO resultatcontrat
                   DISPLAY resultatcontrat line NoLigne col 1       
                   ADD 1 TO NoLigne
               end-perform
           end-perform.            

       VisualisationContrats-Fin.
           Perform Menu.

      ***************************************************************
      ** Visualisation liste Sinistres     Modif OK
      ***************************************************************
       VisualisationSinistres.
           perform VisualisationSinistres-Init.
           perform VisualisationSinistres-Trt until VisualisationSinistres-Trt-fin = 1.
           perform VisualisationSinistres-Fin.

       VisualisationSinistres-Init.
           Move 0 to VisualisationSinistres-Trt-fin.
           Display menu-visualisation-liste-sinistres.
           initialize indiceTabsinistre.
           initialize tailleTabsinistre.       
                                  
       VisualisationSinistres-Trt.
           Move 1 to VisualisationSinistres-Trt-fin.
           perform SQLSinistreListe.
           perform DisplaySinistreListe.
           perform optionVisualisationSinistre.

       optionVisualisationSinistre.
           accept optionVisualisationSinistres line 18 col 15
           evaluate optionVisualisationSinistres
               when 0
                   perform menu
               when 1 thru 9
                   if optionVisualisationSinistres >= indiceTabSinistre then
                       display "Veillez entrer un choix valide. " line 19 col 5
                       perform OptionVisualisationSinistre
                   else
                       move optionVisualisationSinistres to indiceTab
                       move corresponding SinistreTable(indiceTab) to SinistreCourant
                       perform Visualisation-Detail-Sinistre
                   end-if
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   perform OptionVisualisationSinistre.
       SQLSinistreListe.
           EXEC sql
               declare Cursor-CodeSinistreliste cursor for
               select codeSinistre, codeClient, codeContrat, typeSinistre, DAY(dateDuSinistre), MONTH(dateDuSinistre), YEAR(dateDuSinistre)
               from sinistres      
               where codeclient = :clientcourant.codeclient
               order by DateDuSinistre DESC
           END-EXEC

           EXEC sql
               open Cursor-CodeSinistreliste
           END-EXEC

           move 1 to indiceTabsinistre
           move 0 to tailleTabsinistre

           perform until SQLCODE <> 0
               EXEC sql
                   fetch Cursor-CodeSinistreliste into
                   :Sinistrecourant.codeSinistre,
                   :Sinistrecourant.refCodeClient,
                   :Sinistrecourant.refCodeContrat,
                   :Sinistrecourant.TypeSinistre,
                   :Sinistrecourant.Date-sinistre.JJ,
                   :Sinistrecourant.Date-sinistre.MM,
                   :Sinistrecourant.Date-sinistre.AAAA
               END-EXEC

               IF SQLCODE = 0 THEN
                   move corresponding sinistrecourant to sinistreTable(indiceTabsinistre)
                   add 1 to indiceTabsinistre
                   add 1 to tailleTabsinistre
               end-if
           END-PERFORM.

           EXEC sql
                 close Cursor-CodeSinistreliste
           END-EXEC.

       DisplaySinistreListe.
           initialize indiceTab
           perform until indiceTab = tailleTabsinistre
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabsinistre
                   add 1 to indiceTab
                   move corresponding sinistreTable(indiceTab) to variablesIntermediaireSinistreCourant
                   STRING "     " indiceTab "    "
                     codeSinistre of variablesIntermediaireSinistreCourant "      "
                     refCodeClient of variablesIntermediaireSinistreCourant "    "
                     refCodeContrat of variablesIntermediaireSinistreCourant "     "
                     typeSinistre of variablesIntermediaireSinistreCourant "     "
                     JJ of date-Sinistre of variablesIntermediaireSinistreCourant "/"
                     MM of date-Sinistre of variablesIntermediaireSinistreCourant "/"
                     AAAA of date-Sinistre of variablesIntermediaireSinistreCourant INTO resultatsinistre
                   DISPLAY resultatsinistre line NoLigne col 1
                   ADD 1 TO NoLigne
               end-perform
           end-perform.

           

       VisualisationSinistres-Fin.

      ***************************************************************
      * Visualisation liste beneficiaires     Modif OK    
      ***************************************************************
       VisualisationBeneficiaires.
           perform VisualisationBeneficiaires-Init.
           perform VisualisationBeneficiaires-Trt until VisualisationBeneficiaires-Trt-fin = 1.
           perform VisualisationBeneficiaires-Fin.

       VisualisationBeneficiaires-Init.
           Move 0 to VisualisationBeneficiaires-Trt-fin.
           Display menu-visualisation-liste-beneficiaires.
           initialize indiceTabbeneficiaire.
           initialize tailleTabbeneficiaire.

       VisualisationBeneficiaires-Trt.
           Move 1 to VisualisationBeneficiaires-Trt-fin.
           perform SQLBeneficiaireListe.
           perform DisplayBeneficiaireListe.
           perform optionVisualisationbeneficiaire.

       optionVisualisationbeneficiaire.
           accept optionVisualisationbeneficiaires line 18 col 19
           evaluate optionVisualisationbeneficiaires
               when 0
                   perform menu
               when 1 thru 9
                   if optionVisualisationbeneficiaires >= indiceTabBeneficiaire then
                       display "Veillez entrer un choix valide. " line 19 col 5
                       perform OptionVisualisationBeneficiaire
                   else
                       move optionVisualisationbeneficiaires to indiceTab
                       move corresponding beneficiaireTable(indiceTab) to beneficiaireCourant
                       perform Visualisation-Detail-beneficiaire
                   end-if
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   perform OptionVisualisationBeneficiaire.

       SQLBeneficiaireListe.
           EXEC sql
              declare Cursor-Beneficiaireliste cursor for
              select codeBeneficiaire, codeContrat, nom, prenom, DAY(dateNaissance), MONTH(dateNaissance), YEAR(dateNaissance), adresse, codePostal, ville, somme
              from Beneficiaire
              where codeContrat = :Contratcourant.codeContrat
              order by nom, prenom
           END-EXEC

           EXEC sql
               open Cursor-Beneficiaireliste
           END-EXEC

           move 1 to indiceTabbeneficiaire
           move 0 to indiceTabbeneficiaire

           perform until SQLCODE <> 0

               EXEC sql
                   fetch Cursor-nom-prenom into
                   :beneficiairecourant.codeBeneficiaire,
                   :beneficiairecourant.nom,
                   :beneficiairecourant.prenom,
                   :beneficiairecourant.DateNaissance.JJ,
                   :beneficiairecourant.DateNaissance.MM,
                   :beneficiairecourant.DateNaissance.AAAA,
                   :beneficiairecourant.adresse,
                   :beneficiairecourant.codePostal,
                   :beneficiairecourant.ville,
                   :beneficiairecourant.somme
               END-EXEC

               IF SQLCODE = 0 THEN
                   move corresponding beneficiairecourant to beneficiaireTable(indiceTabbeneficiaire)
                   add 1 to indiceTabbeneficiaire
                   add 1 to tailleTabbeneficiaire
               end-if
           END-PERFORM.

           EXEC sql
                 close Cursor-nom-prenom
           END-EXEC.

       DisplayBeneficiaireListe.
           initialize indiceTab
           perform until indiceTab = tailleTabbeneficiaire
               move 8 to NoLigne
               perform until NoLigne = 18 OR indiceTab = tailleTabbeneficiaire
                   add 1 to indiceTab
                   move corresponding beneficiaireTable(indiceTab) to variablesIntermediairebeneficiaireCourant
                   STRING indiceTab " "
                     codebeneficiaire of variablesIntermediairebeneficiaireCourant " "
                     codecontrat of variablesIntermediairebeneficiaireCourant " "
                     nom of variablesIntermediairebeneficiaireCourant " "
                     prenom of variablesIntermediairebeneficiaireCourant " "
                     adresse of variablesIntermediairebeneficiaireCourant " "
                     codePostal of variablesIntermediairebeneficiaireCourant " "
                     ville of variablesIntermediairebeneficiaireCourant " "
                     JJ of dateNaissance of variablesIntermediairebeneficiaireCourant "/"
                     MM of dateNaissance of variablesIntermediairebeneficiaireCourant "/"
                     AAAA of dateNaissance of variablesIntermediairebeneficiaireCourant "   "
                     somme of variablesIntermediairebeneficiaireCourant INTO resultatbeneficiaire
                   DISPLAY resultatbeneficiaire line NoLigne col 1        
                   ADD 1 TO NoLigne
               end-perform.
           

       VisualisationBeneficiaires-Fin.

      ***************************************************************
      ** Visualisation detail Client        Normalement OK
      ***************************************************************              
       Visualisation-Detail-Client.
           perform Visualisation-Detail-Client-Init.
           perform Visualisation-Detail-Client-Trt until Visualisation-Detail-Client-trt-fin = 1.
           perform Visualisation-Detail-Client-Fin.

       Visualisation-Detail-Client-Init.
           Move 0 to Visualisation-Detail-Client-trt-fin.
           Display menu-Visualisation-Detail-client.
           accept OptionMenuClient line 17 col 14.
       Visualisation-Detail-Client-Trt.
           move 1 to Visualisation-Detail-Client-trt-fin.
           evaluate OptionMenuClient
               when 0
                   perform Menu
               when 1
                   perform Mod-Client
               when 2
                   perform VisualisationContrats
               when 3
                   perform VisualisationSinistres
               when 4
                   perform CreationContrat      
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   accept OptionMenuClient line 17 col 14
                   perform Visualisation-Detail-Client-Trt
           end-evaluate.

       Visualisation-Detail-Client-Fin.
           perform menu.

       Mod-Client.
           display menu-Visualisation-Detail-client.
           Display menu-Visualisation-Detail-client-Choix.
           accept menu-Visualisation-Detail-client.
           accept OptionModClient line 17 col 14.
           evaluate OptionModClient
               when 1
                   STRING JJ of dateNaissance of clientCourant "-" MM of dateNaissance of clientCourant "-" AAAA of dateNaissance of clientCourant INTO tmpDatemodClient
                   EXEC sql
                   UPDATE clients
                       set nom = :clientCourant.nom,
                           prenom = :clientCourant.prenom,
                           datenaissance = :tmpDatemodClient,
                           adresse = :clientCourant.adresse,
                           codePostal = :clientCourant.codePostal,
                           ville = :clientCourant.ville
                       where codeClient = :clientCourant.codeClient
                   END-EXEC

                   if SQLCODE = 0
                       Display "Modification du client reussie." line 18 col 5
                       accept OptionModClient
                       move 0 to OptionModClient
                   else
                       Display "Modification du client echouee." line 18 col 5
                       accept OptionModClient
                       move 1 to OptionModClient
                   end-if

               when 0
                   perform Menu
           end-evaluate.

      ***************************************************************
      ** Visualisation detail Contrat     Normalement OK
      ***************************************************************
       Visualisation-Detail-Contrat.
           perform Visualisation-Detail-Contrat-Init.
           perform Visualisation-Detail-Contrat-Trt until Visualisation-Detail-Contrat-Trt-fin = 1.
           perform Visualisation-Detail-Contrat-Fin.

       Visualisation-Detail-Contrat-Init.
           Move 0 to Visualisation-Detail-Contrat-Trt-fin.
           Display menu-visualisation-Detail-contrat.      
           accept OptionMenuContrat line 18 col 15.
       Visualisation-Detail-Contrat-Trt.
           move 1 to Visualisation-Detail-Contrat-Trt-fin.
           evaluate OptionMenuContrat
               when 0
                   perform menu
               when 1
                   perform Mod-Contrat
               when 2
                   perform VisualisationSinistres
               when 3
                   perform CreationSinistre
               when 4
                   if DC of contratCourant = 1 then perform CreationBeneficiaire
                   else
                       Display "le contrat n'a pas de clause Deces" line 19 col 5
                   accept OptionMenuContrat line 18 col 15
               when 5
                   perform Mod-beneficiaire
               when 6
                   perform Supp-Beneficiaire
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
           end-evaluate.

       Visualisation-Detail-Contrat-Fin.

       Mod-Contrat.
           display menu-Visualisation-Detail-contrat.
           Display menu-Visualisation-Detail-contrat-Choix.
           accept menu-Visualisation-Detail-contrat.
           accept OptionModContrat line 18 col 15.
           evaluate OptionModContrat
               when 1
                   EXEC sql
                   UPDATE contrats
                       set IT = :contratCourant.IT,
                           PE = :contratCourant.PE,
                           IA = :contratCourant.IA,
                           MT = :contratCourant.MT,
                           CH = :contratCourant.CHM,
                           FRIT = :contratCourant.FRIT,
                           FRPE = :contratCourant.FRPE,
                           FRIA = :contratCourant.FRIA,
                           FRMT = :contratCourant.FRMT,
                           FRCH = :contratCourant.FRCH,
                           DC = :contratCourant.DC
                       where codeContrat = :contratCourant.codeContrat
                   END-EXEC

                   if SQLCODE = 0
                       Display "Modification du contrat reussie." line 19 col 5
                       accept OptionModContrat
                       move 0 to OptionModContrat
                   else
                       Display "Modification du contrat echouee." line 19 col 5
                       accept OptionModContrat
                       move 1 to OptionModContrat
                   end-if
               when 0
                   perform Menu
           end-evaluate.

      ****************************************************************
      ** Visualisation detail Sinistre    Normalement OK
      ***************************************************************
       Visualisation-Detail-Sinistre.
           perform Visualisation-Detail-Sinistre-Init.
           perform Visualisation-Detail-Sinistre-Trt until Visualisation-Detail-Sinistre-Trt-fin = 1.
           perform Visualisation-Detail-Sinistre-Fin.

       Visualisation-Detail-Sinistre-Init.
           Move 0 to Visualisation-Detail-Sinistre-Trt-fin.
           Display menu-Visualisation-Detail-sinistre.       
           accept OptionMenuSinistre line 18 col 15.
       Visualisation-Detail-Sinistre-Trt.
           move 1 to Visualisation-Detail-Sinistre-Trt-fin.
           evaluate OptionMenuSinistre
               when 0
                   perform menu
               when 1
                   perform Mod-Sinistre               
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
           end-evaluate.

       Visualisation-Detail-Sinistre-Fin.

       Mod-Sinistre.
           display menu-Visualisation-Detail-Sinistre.
           Display menu-Visualisation-Detail-sinistre-Choix.
           accept menu-Visualisation-Detail-sinistre-choix.
           accept OptionModSinistre line 18 col 15.
           evaluate OptionModSinistre
               when 1
                   STRING AAAA of date-sinistre of sinistreCourant "-" MM of date-sinistre of sinistreCourant "-" JJ of date-sinistre of sinistreCourant INTO tmpDateCreaSinistre
                   EXEC sql
                       UPDATE sinistres
                           set typeSinistre = :sinistreCourant.typeSinistre,
                               dateDuSinistre = :tmpDateCreaSinistre
                           where codeSinistre = :sinistreCourant.codeSinistre
                   END-EXEC
                   if SQLCODE = 0
                       Display "Modification du sinistre reussie." line 18 col 5
                       accept OptionModSinistre
                       move 0 to OptionModSinistre
                   else
                       Display "Modification du sinistre echouee." line 19 col 5
                       accept OptionModSinistre
                       move 1 to OptionModSinistre
                   end-if

               when 0
                   perform Menu
           end-evaluate.

      ****************************************************************
      ** Visualisation detail Beneficiaire   Normalement OK
      ***************************************************************
       Visualisation-Detail-beneficiaire.
           perform Visualisation-Detail-beneficiaire-Init.
           perform Visualisation-Detail-beneficiaire-Trt until Visualisation-Detail-beneficiaire-trt-fin = 1.
           perform Visualisation-Detail-beneficiaire-Fin.

       Visualisation-Detail-beneficiaire-init.
           Move 0 to Visualisation-Detail-beneficiaire-trt-fin.
           Display menu-Visualisation-Detail-beneficiaire.       
           accept OptionMenuBeneficiaires line 18 col 14.
       Visualisation-Detail-beneficiaire-Trt.
           move 1 to Visualisation-Detail-beneficiaire-Trt-fin.
           evaluate OptionMenubeneficiaires
               when 0
                   perform menu
               when 1
                   perform Mod-beneficiaire
               when 2 
                   perform Supp-Beneficiaire
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
           end-evaluate.

       Visualisation-Detail-beneficiaire-Fin.
           perform Menu.
       Mod-Beneficiaire.
      *todo pb de sreen, pas le bon, faire beneficiaire-choix mais avec modif

           display menu-Visualisation-Detail-Beneficiaire.
           accept menu-Visualisation-Detail-Beneficiaire.
           accept OptionModBeneficiaire line 18 col 14.
           evaluate OptionModBeneficiaire
               when 1
                   STRING AAAA of dateNaissance of beneficiaireCourant "-" MM of dateNaissance of beneficiaireCourant "-" JJ of dateNaissance of beneficiaireCourant INTO tmpDateCreaBeneficiaire
                   inspect somme of BeneficiaireCourant replacing all "," by "."
                   EXEC sql
                       UPDATE beneficiaires
                           set nom = :beneficiaireCourant.nom,
                               prenom = :beneficiaireCourant.prenom,
                               dateNaissance = :tmpDateCreaBeneficiaire,
                               adresse = :beneficiaireCourant.adresse,
                               codePostal = :beneficiaireCourant.codePostal,
                               ville = :beneficiaireCourant.ville,
                               somme = :beneficiaireCourant.somme
                           where codeContrat = :BeneficiaireCourant.codecontrat
                   END-EXEC
                   if SQLCODE = 0
                       Display "Modification du beneficiaire reussie." line 19 col 5
                       accept OptionModBeneficiaire
                       move 0 to OptionModBeneficiaire
                   else
                       Display "Modification du beneficiaire echouee." line 19 col 5
                       accept OptionModBeneficiaire
                       move 1 to OptionModBeneficiaire
                   end-if

               when 0
                   perform Menu
           end-evaluate.

       Supp-Beneficiaire.
           exec sql
             delete from Beneficiaires where CodeBeneficiaire = :Beneficiairecourant.Codebeneficiaire
           end-exec
           if SQLCODE = 0
               Display "Suppression du beneficiaire reussie." line 19 col 5
               accept OptionModBeneficiaire
               move 0 to OptionModBeneficiaire
           else
               Display "Suppression du beneficiaire echouee." line 19 col 5
               accept OptionModBeneficiaire
               move 1 to OptionModBeneficiaire
           end-if.                  

      ***************************************************************
      ** Creation Client    Modif OK   
      ***************************************************************

       CreationClient.
           perform CreationClient-Init.
           perform CreationClient-Trt until CreationClient-Trt-fin = 1.
           perform CreationClient-Fin.

       CreationClient-Init.
           Move 0 to CreationClient-Trt-fin.
           initialize clientcourant.
           Display menu-Creation-Client.
           accept menu-Creation-Client.
           accept OptionCreationClient line 17 col 14.

       CreationClient-Trt.
      * Code de verification des conditions
           evaluate OptionCreationClient
               when 0
                   perform Menu
               when 1
                   perform ValiditeAgeClient
                   if optionCreationClient = 1 AND
                     OptionValiditeAgeClient = 1 AND
                     nom of clientCourant <> '' AND
                     prenom of clientCourant <> '' AND
                     adresse of clientCourant <> '' AND
                     codePostal of clientCourant <> '' AND
                     ville of clientCourant <> '' AND
                     (JJ of dateNaissance of clientCourant >= 1 AND JJ of dateNaissance of clientCourant <= 31) AND
                     (MM of dateNaissance of clientCourant >= 1 AND MM of dateNaissance of clientCourant <= 12) AND
                     (AAAA of dateNaissance of clientCourant >= 1900 AND AAAA of dateNaissance of clientCourant <= AAAA of DateSysteme) then
                       STRING JJ of dateNaissance of clientCourant "-" MM of dateNaissance of clientCourant "-" AAAA of dateNaissance of clientCourant INTO tmpDateCreaClient
                       perform SQLInsertClient
                   else
                       accept menu-Creation-Client.
                       accept OptionCreationClient line 17 col 14.
                       perform CreationClient-trt.       

       SQLInsertClient.
           EXEC SQL
               select newid() into :clientCourant.codeClient
           END-EXEC
           EXEC sql
               INSERT INTO Clients (CodeClient, nom, prenom, dateNaissance, adresse, codePostal, ville)
               VALUES (:clientCourant.codeClient, :clientCourant.nom, :clientCourant.prenom, :tmpDateCreaClient, :clientCourant.adresse, :clientCourant.codePostal, :clientCourant.ville)
           END-EXEC
           if SQLCODE = 0
               Display "Creation du client reussie." line 18 col 5
               accept optionCreationClient
               move 0 to optionCreationClient
           else
               Display "Creation du client echouee." line 18 col 5
               accept optionCreationClient
               move 1 to optionCreationClient
           end-if.

           

       CreationClient-Fin.
           perform Menu.
      
      ***************************************************************
      ** Creation Contrat        Faire option calcul cout du contrat si on a le temps
      ***************************************************************
       CreationContrat.
           perform CreationContrat-Init.
           perform CreationContrat-Trt until CreationContrat-Trt-fin = 1.
           perform CreationContrat-Fin.

       CreationContrat-Init.
           Move 0 to CreationContrat-Trt-fin.
           Display menu-Creation-contrat.
           initialize contratCourant.
           accept menu-creation-contrat.
           accept OptionCreationContrat line 18 col 15.
       CreationContrat-Trt.
           evaluate OptionCreationContrat
               when 0
                   perform Menu
               when 1
                   perform ValiditeAgeClient.
                   if optioncreationContrat = 1 AND
                       OptionValiditeAgeClient = 1 AND (
                       IT of contratCourant <> '' or
                       PE of contratCourant <> '' or
                       IA of contratCourant <> '' or
                       MT of contratCourant <> '' or
                       CHM of contratCourant <> '' or
                       FRIT of contratCourant <> '' or
                       FRPE of contratCourant <> '' or
                       FRIA of contratCourant <> '' or
                       FRMT of contratCourant <> '' or
                       CHM of contratCourant <> '' or
                       DC of contratCourant <> '') then
                       STRING JJ of datesysteme "-" MM of dateSysteme "-" AAAA of dateSysteme INTO tmpDateCreaContrat
                       perform SQLContratInsert
                   else
                       accept menu-creation-contrat.
                       accept OptionCreationContrat line 18 col 15.
                       perform CreationContrat-Trt.

       SQLContratInsert.
               move 0 to NombreBeneficiaires of contratCourant
               EXEC SQL
                   select newid() into :ContratCourant.codeContrat
               END-EXEC
               EXEC sql
                 INSERT INTO Contrats (codeContrat, CodeClient, IT, PE, IA, MT, CH, FRIT, FRPE, FRIA, FRMT, FRCH, DC, dateSignature, NombreBeneficiaires)
                 VALUES (:contratCourant.codeContrat, :clientCourant.CodeClient, :contratCourant.sinistresCouverts.IT, :contratCourant.sinistresCouverts.PE, :contratCourant.sinistresCouverts.IA, :contratCourant.sinistresCouverts.MT,
                         :contratCourant.sinistresCouverts.CHM, :contratCourant.franchise.FRIT, :contratCourant.franchise.FRPE, :contratCourant.franchise.FRIA, :contratCourant.franchise.FRMT, :contratCourant.franchise.FRCH,
                     :contratCourant.sinistresCouverts.DC, :tmpDateCreaContrat, :ContratCourant.NombreBeneficiaires)
           END-EXEC
               if SQLCODE = 0
                   Display "Creation du contrat reussie." line 19 col 5
                   accept OptionCreationContrat line 18 col 15
                   move 0 to OptionCreationContrat
               else
                   Display "Creation du contrat echouee." line 19 col 5
                   accept OptionCreationContrat line 18 col 15
                   move 1 to OptionCreationContrat
               end-if.

          

       CreationContrat-Fin.
           perform menu.

      ***************************************************************
      ** Creation Sinistre          Modif OK  
      ***************************************************************
       CreationSinistre.
           perform CreationSinistre-Init.
           perform CreationSinistre-Trt until CreationSinistre-Trt-fin = 1.
           perform CreationSinistre-Fin.

       CreationSinistre-Init.
           Move 0 to CreationSinistre-Trt-fin.
           Display menu-creation-sinistre.
           initialize sinistrecourant.
           accept menu-creation-sinistre.
           accept OptionCreationSinistre line 18 col 15.
           
       CreationSinistre-Trt.
           evaluate OptionCreationSinistre
               when 0
                   perform Menu
               when 1
                   if optioncreationSinistre = 1 AND (
                     typeSinistre of sinistreCourant <> '' or
                     date-Sinistre of sinistreCourant <> '') then
                       perform VerificationSinistreCouvert
                       perform ValiditeDateSinistre
                       STRING JJ of date-Sinistre of sinistreCourant "-" MM of date-Sinistre of sinistreCourant "-" AAAA of date-Sinistre of sinistreCourant INTO tmpDateCreasinistre
                       perform SQLSinistreInsert
               when other
                   display "Veillez entrer un choix valide. " line 19 col 5
                   accept OptionCreationSinistre line 18 col 15
                   perform CreationSinistre-Trt.

       SQLSinistreInsert.
           EXEC SQL
               select newid() into :sinistreCourant.codesinistre
           END-EXEC
           EXEC sql
               INSERT INTO sinistres (codeSinistre, codeClient, codeContrat, typeSinistre, dateDuSinistre)
               VALUES ( :sinistreCourant.codesinistre, :clientCourant.CodeClient, :contratCourant.codecontrat, :sinistreCourant.typesinistre, :tmpDateCreasinistre)
           END-EXEC
           if SQLCODE = 0
               Display "Creation du sinistre reussie.                      " line 19 col 5
               move 0 to OptionCreationSinistre
               accept OptionCreationSinistre line 18 col 15               
               perform CreationSinistre-Trt
           else
               Display "Creation du sinistre echouee.                      " line 19 col 5
               perform CreationSinistre-Trt
           end-if.

           

       CreationSinistre-Fin.
           perform menu.

      ***************************************************************
      ** Creation beneficiaires   Modif OK  
      ***************************************************************
       CreationBeneficiaire.
           perform CreationBeneficiaire-Init.
           perform CreationBeneficiaire-Trt until CreationBeneficiaire-Trt-fin = 1.
           perform CreationBeneficiaire-Fin.

       CreationBeneficiaire-Init.
           Move 0 to CreationBeneficiaire-Trt-fin.
           Display menu-creation-beneficiaire.
           initialize beneficiairecourant.
           accept menu-creation-Beneficiaire.
           accept OptionCreationBeneficiaire line 18 col 14.
       CreationBeneficiaire-Trt.

           if optioncreationBeneficiaire = 1 AND
             DC of contratCourant = 1 AND (
             nom of beneficiaireCourant <> '' or
             prenom of beneficiaireCourant <> '') then
               STRING JJ of datenaissance of BeneficiaireCourant "-" MM of datenaissance of BeneficiaireCourant "-" AAAA of datenaissance of BeneficiaireCourant INTO tmpDateCreaBeneficiaire
               perform SQLBeneficiaireInsert
           else
               if OptionCreationBeneficiaire = 0
                   move 0 to OptionCreationBeneficiaire
               else
                   move 1 to OptionCreationBeneficiaire
               end-if.

           move 1 to CreationBeneficiaire-Trt-fin.

       SQLBeneficiaireInsert.
           ADD 1 to NombreBeneficiaires of contratCourant
           inspect somme of BeneficiaireCourant replacing all "," by "."
           EXEC sql
               UPDATE contrats
                   set NombreBeneficiaires = :contratCourant.NombreBeneficiaires                             
                   where codeContrat = :contratCourant.codeContrat
           END-EXEC

           EXEC SQL
               select newid() into :beneficiaireCourant.codebeneficiaire
           END-EXEC

           EXEC sql
               INSERT INTO beneficiaires (codeBeneficiaire, codeContrat, nom, prenom, dateNaissance, adresse, codePostal, ville, somme)
               VALUES ( :beneficiaireCourant.codebeneficiaire, :contratCourant.codeContrat, :beneficiaireCourant.nom, :beneficiaireCourant.prenom, :tmpDateCreaBeneficiaire,
                       :beneficiaireCourant.adresse, :beneficiaireCourant.codePostal, :beneficiaireCourant.ville, :beneficiaireCourant.somme )
           END-EXEC

           if SQLCODE = 0
               Display "Creation du Beneficiaire reussie." line 19 col 5
               accept OptionCreationBeneficiaire line 18 col 14
               move 0 to OptionCreationBeneficiaire
           else
               Display "Creation du Beneficiaire echouee." line 19 col 5
               accept OptionCreationBeneficiaire line 18 col 14
               move 1 to OptionCreationBeneficiaire
           end-if.

           

       CreationBeneficiaire-Fin.
           perform menu.
      ***************************************************************
      ** Zone de controle
      ***************************************************************

       ValiditeContrat.
      * Verifie si le contrat a moins de 1 ans 
           initialize DateValidite
           subtract AAAA of DateSysteme from AAAA of contratTable(indiceTab) GIVING AAAA of DateValidite
           subtract MM of DateSysteme from MM of contratTable(indiceTab) GIVING MM of DateValidite
           subtract JJ of DateSysteme from JJ of contratTable(indiceTab) GIVING JJ of DateValidite

           multiply 365 by AAAA of DateValidite GIVING totalJoursAAAA
           multiply 30,58 by MM of DateValidite GIVING totalJoursMM
           add totaljoursAAAA to totalJoursMM
           add JJ of DateValidite to totaljoursMM

           IF totalJoursMM > 365
               move "non" to Validite of variablesIntermediaireContratCourant
           else
               move "Oui" to Validite of variablesIntermediaireContratCourant
           END-IF.

       

       ValiditeAgeClient.
      * Verifie si le client a plus de 18 ans et moins de 65 ans.
           initialize DateValidite
           subtract AAAA of clientcourant from AAAA of DateSysteme GIVING AAAA of AgeClient
           subtract MM of clientcourant from MM of DateSysteme GIVING MM of AgeClient
           subtract JJ of clientcourant from JJ of DateSysteme GIVING JJ of AgeClient

           If AAAA of AgeClient <= 18 and MM of ageClient >= 0 and JJ of ageClient > 0 then
               display "Le client est mineur" line 19 col 5
               Move 0 to OptionValiditeAgeClient
           end-if
           If AAAA of AgeClient >= 65 and MM of ageClient >= 0 and JJ of ageClient > 0 then
               display "Le client est trop age pour souscrire un contrat" line 19 col 5
               Move 0 to OptionValiditeAgeClient
           end-if.
     
      
       VerificationSinistreCouvert.
      * Verifie si le sinistre declarer est couvert par le contrat.
           IF DC of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "DC" OR
              IT of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "IT" OR
              PE of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "PE" OR
              IA of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "IA" OR
              MT of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "MT" OR
             CHM of sinistresCouverts of contratCourant = 1 AND TypeSinistre of sinistreCourant = "CH" THEN
               continue
           Else
               Display "Le sinistre n'est pas couvert par ce contrat        " line 19 col 5
               accept menu-creation-sinistre
               accept OptionCreationSinistre line 18 col 15
               perform CreationSinistre-Trt.              

      
       ValiditeDateSinistre.
      * Verifie si le sinistre date de moins de 7 jours. 
           initialize DateValidite
           subtract AAAA of DateSysteme from AAAA of Date-sinistre of sinistrecourant GIVING AAAA of DateValidite
           subtract MM of DateSysteme from MM of Date-sinistre of sinistrecourant GIVING MM of DateValidite
           subtract JJ of DateSysteme from JJ of Date-sinistre of sinistrecourant GIVING JJ of DateValidite

           multiply 365 by AAAA of DateValidite GIVING totalJoursAAAA
           multiply 30,58 by MM of DateValidite GIVING totalJoursMM
           add totaljoursAAAA to totalJoursMM
           add JJ of DateValidite to totaljoursMM
           if totalJoursMM > 7
               Display "La date du sinistre est anterieur a 7 jours        " line 19 col 5
               accept Menu-Creation-Sinistre-Date
               accept OptionCreationSinistre line 18 col 15
               perform CreationSinistre-Trt
           else
               Move 0 to OptionCreationSinistre
           END-IF.

       end program Program1.

