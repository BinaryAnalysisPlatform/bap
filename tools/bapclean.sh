core_library="BAP Core Library"

for package in `ocamlfind list | cut -d' ' -f1`
do
    desc=`ocamlfind query -format "%D" $package`
    if [ "x$desc" = "x$core_library" ]
    then
        echo "deleting package $package that is a part of $core_library"
        ocamlfind remove $package
    else
        case $package in
            bap-plugin-*)
                echo "deleting $package because it is a bap-plugin"
                ocamlfind remove $package
                ;;
            bap-*)
                echo "deleting $package because it is prefixed with bap"
                ocamlfind remove $package
                ;;
        esac
    fi
done
