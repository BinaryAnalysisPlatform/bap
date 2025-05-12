time find /Volumes -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -print | parallel "bap superset-disasm --ground_truth_bin={} {}"

find /Volumes -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -print > files.txt
bap supersetd-graph-metrics ./files.txt
