#!/usr/bin/env bash
# Lay the Maven-jar sources desargues' pure half needs as plain directories,
# because cljw's -cp takes directories only and skips Maven coordinates.
# Output: target/cljw-cp/<lib>/ ; prints a colon-joined classpath on stdout.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
out="$root/target/cljw-cp"
m2="$HOME/.m2/repository"
mkdir -p "$out"
jars=(
  "org/typedclojure/typed.clj.runtime/1.3.0/typed.clj.runtime-1.3.0.jar"
  "io/github/frenchy64/fully-satisfies/1.12.0/fully-satisfies-1.12.0.jar"
  "org/clojure/core.logic/1.1.1/core.logic-1.1.1.jar"
  "org/clojure/core.match/1.0.1/core.match-1.0.1.jar"
  "org/clojure/math.numeric-tower/0.0.4/math.numeric-tower-0.0.4.jar"
  "com/taoensso/timbre/6.0.4/timbre-6.0.4.jar"
  "com/taoensso/encore/3.43.0/encore-3.43.0.jar"
  "org/babashka/sci/0.8.40/sci-0.8.40.jar"
  "org/clojure/test.check/1.1.1/test.check-1.1.1.jar"
)
# shims first: they shadow typed.clojure (JVM-internal runtime) and stub cljs.analyzer.api
cp="$root/src:$root/dev/cljw/shim"
for j in "${jars[@]}"; do
  name=$(basename "$j" .jar)
  if [ -f "$m2/$j" ]; then
    [ -d "$out/$name" ] || (mkdir -p "$out/$name" && cd "$out/$name" && unzip -qo "$m2/$j" -x 'META-INF/*')
    cp="$cp:$out/$name"
  else
    echo "missing jar: $j" >&2
  fi
done
emmy=$(ls -d "$HOME"/.gitlibs/libs/io.github.mentat-collective/emmy/53fd990* | head -1)
# emmy.util/sci-macro expands to `(clojure.core/defmacro ...)`; cljw 1.14.11 only
# recognises the unqualified `defmacro`. A patched copy shadows emmy.util.
if [ -n "${CLJW_PATCH_EMMY:-}" ]; then
  mkdir -p "$out/emmy-patch/emmy"
  sed "s#~'clojure.core/defmacro#~'defmacro#" "$emmy/src/emmy/util.cljc" > "$out/emmy-patch/emmy/util.cljc"
  cp="$cp:$out/emmy-patch"
fi
cp="$cp:$emmy/src"
echo "$cp"
