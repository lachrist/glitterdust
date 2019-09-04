./cesk instrument-literal-abstraction-condition-application $1 > instrumented.scm
node merge.js meta/log.scm instrumented.scm > merged.scm
./cesk stored merged.scm