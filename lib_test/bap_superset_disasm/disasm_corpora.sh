find /Volumes -type f -executable -exec sh -c "file -i '{}' | grep -q 'x-executable; charset=binary'" \; -print > files.txt

time cat files.txt | parallel "bap superset-disasm --find-fn-culprit --analyses="StronglyConnectedComponentData","Grammar-convergent" --heuristics=Callsites3,FixpointGrammar,ImgEntry,InterpretationDepthOne,FixpointInterpDepthOne --ground-truth-bin={} {}"

bap supersetd-graph-metrics ./files.txt --print-fn-bins
