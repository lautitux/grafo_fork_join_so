gleam build --target=javascript
esbuild './build/dev/javascript/fork_join/fork_join.mjs' --bundle --minify --format='esm' --outfile='./site/gleam_bundle.mjs'