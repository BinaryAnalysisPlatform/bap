find /Volumes -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -print | parallel "bap superset-disasm --ground_truth_bin={} {}"

#$(find /Volumes -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -printa | tr "\n" ",")

#bap supersetd-graph-metrics "/Volumes/arm-binaries/coreutils/coreutils_O0_cp",
