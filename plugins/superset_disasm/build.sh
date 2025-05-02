#pushd plugin/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg findlib.dynload -pkg str -pkg zmq -pkg bap-primus -pkg bap-knowledge -pkg superset_disassemblers superset_disassembler.plugin
#popd
