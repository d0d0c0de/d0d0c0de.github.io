#! /bin/sh

BUILD_DIR="../master"

# sync static files
rsync -av --exclude '.git' --delete "static/" "$BUILD_DIR/"

# build elm app
elm-make elm/Main.elm --warn --output "$BUILD_DIR/main.js"

# build CSS
echo "Building main.css from SCSS files..."
sassc scss/main.scss "$BUILD_DIR/main.css"

