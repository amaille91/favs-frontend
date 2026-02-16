{
  /***************************************************************************
   * flake.nix — Environnement de dev reproductible pour favs-frontend
   *
   * Objectifs :
   *  - Avoir Node (version choisie) sans toucher à APT Debian
   *  - Avoir une toolchain minimale pour compiler des modules npm natifs si besoin
   *  - Avoir les librairies runtime courantes nécessaires à Chromium (Playwright)
   *  - Fonctionner sur Debian “host” et Debian en VM VirtualBox
   *  - Garder le système “propre” : tout va dans /nix/store + caches user
   *
   * Utilisation :
   *   nix develop
   *   npm ci
   *   npx playwright install chromium
   *   npm run dev / npm test
   ***************************************************************************/

  description = "favs-frontend devshell (Node + Playwright deps)";

  /***************************************************************************
   * inputs : les dépendances “Nix” du flake.
   *
   * - nixpkgs : la collection de paquets Nix (équivalent “repository” géant).
   * - flake-utils : aide pour générer les sorties pour plusieurs plateformes
   *   (x86_64-linux, aarch64-linux, etc.).
   *
   * IMPORTANT :
   *   Le fichier flake.lock va pinner (verrouiller) les versions exactes
   *   des inputs. C’est l’équivalent du package-lock.json mais pour Nix.
   ***************************************************************************/
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  /***************************************************************************
   * outputs : ce que ton flake “expose”.
   *
   * On veut surtout un devShell :
   *   devShells.default  => l’environnement chargé par `nix develop`
   *
   * flake-utils.eachDefaultSystem va générer ce devShell pour chaque
   * architecture/OS supporté par Nix (principalement Linux ici).
   ***************************************************************************/
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        /*********************************************************************
         * pkgs : import de nixpkgs pour l'architecture courante.
         * 'system' vaut souvent "x86_64-linux" sur Debian PC classique.
         *********************************************************************/
        pkgs = import nixpkgs { inherit system; };

        /*********************************************************************
         * Choix Node :
         * - nodejs_20 : bon compromis “moderne” (tu peux passer à nodejs_18
         *   si ton projet est strict sur 18).
         *
         * Conseil : aligne ça avec ton .nvmrc / engines dans package.json
         * (si vous en avez) et avec vos contraintes CI.
         *********************************************************************/
        node = pkgs.nodejs_20;

      in
      {
        /*********************************************************************
         * devShells.default : ce que `nix develop` va activer.
         *
         * pkgs.mkShell crée un environnement temporaire (un “shell dev”).
         * Il ne modifie pas /usr, il ajoute juste des variables d’environnement
         * (PATH, etc.) pour que les outils viennent de /nix/store.
         *********************************************************************/
        devShells.default = pkgs.mkShell {

          /*******************************************************************
           * packages : liste des outils disponibles dans le shell.
           *
           * À retenir :
           * - Ici on installe des outils “système de dev” (node, gcc, etc.).
           * - Les dépendances JS du projet restent gérées par npm/yarn/pnpm
           *   dans ./node_modules (local au repo).
           *******************************************************************/
          packages = with pkgs; [
            node
            git

            # Outils usuels (souvent nécessaires indirectement via npm)
            python3       # node-gyp peut en dépendre
            pkg-config    # pour détecter des libs natives
            gcc           # compilation C/C++
            gnumake       # build
          ];

          /*******************************************************************
           * “Runtime deps” pour Chromium (Playwright)
           *
           * Playwright utilise un navigateur (Chromium) qui dépend de
           * nombreuses libs graphiques et système.
           *
           * Sur une Debian minimaliste (ou VM), sinon tu te prends du genre :
           *   error while loading shared libraries: libX11.so.6: cannot open...
           *
           * Ici on ajoute une liste large, “pragmatique” :
           * - ça évite d’installer via apt
           * - mais ça augmente un peu la taille du /nix/store
           *
           * NOTE :
           * Playwright peut aussi installer des deps via apt (--with-deps),
           * mais toi tu veux justement éviter d’impacter l’OS -> on préfère Nix.
           *******************************************************************/
          packages = (with pkgs; [
            node
            git
            python3
            pkg-config
            gcc
            gnumake
          ]) ++ (with pkgs; [
            # Graphique / rendu / UI
            cairo
            pango
            atk
            at-spi2-atk
            gtk3
            gdk-pixbuf
            glib

            # SSL / réseau
            nss
            nspr

            # Audio / bus / bas niveau
            alsa-lib
            dbus
            expat

            # GPU / Mesa
            libdrm
            mesa

            # Clavier / input
            libxkbcommon

            # X11: bibliothèques nécessaires au navigateur (même en headless, souvent)
            xorg.libX11
            xorg.libXcomposite
            xorg.libXdamage
            xorg.libXext
            xorg.libXfixes
            xorg.libXrandr
            xorg.libXrender
            xorg.libXtst
            xorg.libxcb
            xorg.libXi
            xorg.libXScrnSaver
            xorg.libXcursor
            xorg.libXinerama
            xorg.libXxf86vm
            xorg.libXshmfence

            # Polices / texte (sinon rendu bizarre ou plantages)
            fontconfig
            freetype
          ]);

          /*******************************************************************
           * shellHook : script exécuté à l’entrée dans le dev shell.
           *
           * C’est utile pour :
           * - afficher des infos
           * - exporter des variables d’environnement
           * - définir des chemins de cache utilisateur
           *
           * IMPORTANT :
           * - On n’écrit PAS dans /nix/store ici.
           * - On évite d’installer “global” (npm -g) ici.
           *******************************************************************/
          shellHook = ''
            echo "✅ favs-frontend dev shell"
            echo "   node: $(node -v)"
            echo "   npm : $(npm -v)"

            # Où Playwright stocke ses navigateurs téléchargés.
            # Par défaut Playwright utilise ~/.cache/ms-playwright sur Linux.
            # On l’explicite pour que ce soit clair et stable.
            export PLAYWRIGHT_BROWSERS_PATH="$HOME/.cache/ms-playwright"

            # Optionnel : rendre npm un peu plus “propre” côté caches
            # (ça ne change pas node_modules, uniquement les caches user).
            export npm_config_cache="$HOME/.cache/npm"

            # Petit rappel UX
            echo "   Next steps:"
            echo "     npm ci"
            echo "     npx playwright install chromium"
          '';
        };

        /*********************************************************************
         * (Optionnel) Tu peux aussi exposer un “formatter” ou d’autres sorties
         * mais pour ton usage ce n’est pas nécessaire.
         *********************************************************************/
      }
    );
}
