import idautils
import tempfile


def taint_and_color(ptr_or_reg):

    args = {
        'input_file_path': idc.GetInputFilePath(),
        'taint_location': idc.ScreenEA(),
        'ida_script_location': tempfile.mkstemp(suffix='.py',
                                                prefix='ida-bap-')[1],
        'ptr_or_reg': ptr_or_reg
    }

    idc.Message('-------- STARTING TAINT ANALYSIS --------------\n')
    idc.SetStatus(IDA_STATUS_WAITING)

    idaapi.refresh_idaview_anyway()

    # TODO : Figure out how to get rid of the opam dependency here
    idc.Exec(
        "\
        eval `opam config env`; \
        bap \"{input_file_path}\" \
        --taint-{ptr_or_reg}=0x{taint_location:X} \
        --taint \
        --propagate-taint \
        --map-terms-with='((true) (color gray))' \
        --map-terms-with='((is-visited) (color white))' \
        --map-terms-with='((has-taints) (color red))' \
        --map-terms-with='((taints) (color yellow))' \
        --map-terms \
        --emit-ida-script-attr=color \
        --emit-ida-script-file={ida_script_location} \
        --emit-ida-script \
        ".format(**args)
    )

    idc.Message('-------- DONE WITH TAINT ANALYSIS -------------\n\n')
    idc.SetStatus(IDA_STATUS_READY)

    idaapi.IDAPython_ExecScript(args['ida_script_location'], globals())

    idc.Exec("rm -f {ida_script_location}".format(**args))  # Cleanup

    idc.Refresh()  # Force the color information to show up


def taint_reg_and_color():
    taint_and_color('reg')


def taint_ptr_and_color():
    taint_and_color('ptr')


def add_hotkey(hotkey, func):
    hotkey_ctx = idaapi.add_hotkey(hotkey, func)
    if hotkey_ctx is None:
        print("Failed to register {} for {}".format(hotkey, func))
    else:
        print("Registered {} for {}".format(hotkey, func))


add_hotkey("Shift-A", taint_reg_and_color)
add_hotkey("Ctrl-Shift-A", taint_ptr_and_color)
